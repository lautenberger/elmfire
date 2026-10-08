#!/usr/bin/env python3
import argparse
import os
import re
import shlex
import subprocess
import sys
from math import floor
import mimetypes

VALID_SUITES = ("all", "verification", "validation")
SLURM_HEADER_FILES = {
    "verification": "slurm_verification_head.txt",
    "validation": "slurm_validation_head.txt",
}

def usage():
    return """Usage: run_all.py [OPTIONS]

Run all discovered cases' run_case.sh, with optional sharding for Cloud Run Jobs.

Options:
  -l, --list              List the resolved case scripts for THIS SHARD and exit
  -n, --dry-run           Show the commands without executing them
  -s, --slurm             Submit jobs via Slurm (sbatch run_case_slurm.sh)
  --suite SCOPE           Select all, verification, or validation cases
                          (default: all)
  --verification-only     Alias for --suite verification
  --validation-only       Alias for --suite validation
  --shard-index N         Zero-based shard index for this worker (overrides env)
  --shard-count K         Total number of shards/workers (overrides env)
  -h, --help              Show this help message

Notes:
  - Sharding sources (highest priority first):
      1) --shard-index / --shard-count
      2) CLOUD_RUN_TASK_INDEX / CLOUD_RUN_TASK_COUNT
      3) TASK_COUNT (custom) with index=0
      4) default: index=0, count=1
  - Each case executes from its own directory so ELMFIRE sees inputs in CWD.
  - Suite selection is applied before sharding, so shard indices refer only to
    the selected verification or validation case set.
  - With --slurm, a run_case_slurm.sh wrapper is generated per case using the
    matching verification or validation header under common/.
"""

def discover_cases(cases_dir, suite="all"):
    """Find runnable cases in the requested suite, excluding templates/legacy."""
    if suite not in VALID_SUITES:
        raise ValueError(f"Unknown suite scope: {suite}")

    search_root = cases_dir
    if suite != "all":
        search_root = os.path.join(cases_dir, suite.capitalize())
    if not os.path.isdir(search_root):
        return []

    scripts = []
    for dirpath, dirnames, filenames in os.walk(search_root):
        # Prune non-runnable repository material before descending into it.
        dirnames[:] = [
            name for name in dirnames
            if name != "case_template" and not name.startswith("__legacy__")
        ]
        relative_parts = os.path.relpath(dirpath, cases_dir).split(os.sep)
        if "case_template" in relative_parts or "__legacy__" in relative_parts:
            continue
        if "run_case.sh" in filenames:
            scripts.append(os.path.join(dirpath, "run_case.sh"))
    scripts.sort()
    return scripts

def format_case(script, root_dir):
    """Pretty relative path like cases/Validation/tubbs_fire"""
    rel = os.path.relpath(script, root_dir)
    return rel.removesuffix("/run_case.sh")

def slurm_header_for_case(script, cases_dir, common_dir):
    """Return the suite-specific Slurm header for a discovered case script."""
    relative_parts = os.path.relpath(script, cases_dir).split(os.sep)
    if not relative_parts:
        raise ValueError(f"Cannot determine suite for case script: {script}")

    suite = relative_parts[0].casefold()
    header_name = SLURM_HEADER_FILES.get(suite)
    if header_name is None:
        raise ValueError(f"No Slurm header is defined for suite path: {script}")
    return os.path.join(common_dir, header_name)

def slurm_job_name(case_dir):
    """Create a stable Slurm job name from the canonical case directory ID."""
    case_id = os.path.basename(os.path.normpath(case_dir))
    safe_case_id = re.sub(r"[^A-Za-z0-9_.-]+", "-", case_id).strip("-._")
    if not safe_case_id:
        safe_case_id = "case"
    return f"elmfire-{safe_case_id}"[:128]

def render_slurm_header(header_text, job_name):
    """Replace a generic header job name with the case-specific job name."""
    directive = re.compile(r"^#SBATCH\s+--job-name(?:=|\s+).*$", re.MULTILINE)
    replacement = f"#SBATCH --job-name={job_name}"
    if directive.search(header_text):
        return directive.sub(replacement, header_text, count=1)
    return replacement + "\n" + header_text

def make_slurm_wrapper(case_dir, header_path):
    """Create a Slurm wrapper that executes the original case runner.

    Slurm copies submitted scripts into its spool directory.  Embedding the
    contents of run_case.sh here would therefore make BASH_SOURCE[0] point at
    /var/spool rather than the case, breaking case-root discovery.  Execute the
    original script as a separate Bash program so it retains its real path.
    """
    wrapper = os.path.join(case_dir, "run_case_slurm.sh")
    run_case = os.path.join(case_dir, "run_case.sh")

    if not os.path.isfile(header_path):
        print(f"[ERROR] Missing Slurm header file: {header_path}", file=sys.stderr)
        sys.exit(1)
    if not os.path.isfile(run_case):
        print(f"[ERROR] Missing run_case.sh in {case_dir}", file=sys.stderr)
        sys.exit(1)

    with open(wrapper, "w") as f:
        f.write("#!/usr/bin/env bash\n\n")
        with open(header_path, "r") as hdr:
            header = render_slurm_header(hdr.read(), slurm_job_name(case_dir))
            f.write(header.rstrip() + "\n\n")
        f.write(f"cd -- {shlex.quote(case_dir)}\n")
        f.write(f"exec bash {shlex.quote(run_case)}\n")

    os.chmod(wrapper, 0o755)
    return wrapper

def resolve_shard_args(cli_index, cli_count):
    """
    Determine (shard_index, shard_count) using CLI > env > defaults.
    Recognized envs:
      - CLOUD_RUN_TASK_INDEX (0-based), CLOUD_RUN_TASK_COUNT
      - TASK_COUNT (fallback; index assumed 0)
    """
    # CLI wins
    if cli_index is not None and cli_count is not None:
        return cli_index, cli_count
    if (cli_index is None) ^ (cli_count is None):
        print("[ERROR] --shard-index and --shard-count must be provided together", file=sys.stderr)
        sys.exit(2)

    # Cloud Run Jobs envs
    env_idx = os.getenv("CLOUD_RUN_TASK_INDEX")
    env_cnt = os.getenv("CLOUD_RUN_TASK_COUNT")
    if env_idx is not None and env_cnt is not None:
        try:
            idx = int(env_idx)
            cnt = int(env_cnt)
            return idx, cnt
        except ValueError:
            print("[WARN] Invalid CLOUD_RUN_TASK_* values; falling back", file=sys.stderr)

    # Google Batch
    b_idx = os.getenv("BATCH_TASK_INDEX")
    b_cnt = os.getenv("BATCH_TASK_COUNT")  # not always set; fall back to TASK_COUNT
    if b_idx is not None:
        try:
            idx = int(b_idx)
            cnt = int(b_cnt) if b_cnt is not None else int(os.getenv("TASK_COUNT", "1"))
            return idx, cnt
        except ValueError:
            print("[WARN] Invalid BATCH_TASK_*; falling back", file=sys.stderr)

    # Fallback custom
    env_cnt2 = os.getenv("TASK_COUNT")
    if env_cnt2 is not None:
        try:
            cnt = int(env_cnt2)
            return 0, cnt
        except ValueError:
            print("[WARN] Invalid TASK_COUNT; falling back", file=sys.stderr)

    # Default: single shard
    return 0, 1

def shard_slice(n_items, k_shards, i_index):
    """Contiguous sharding: [floor(i*n/k), floor((i+1)*n/k))"""
    if k_shards <= 0:
        return 0, n_items
    if i_index < 0 or i_index >= k_shards:
        return 0, 0
    start = floor(i_index * n_items / k_shards)
    end = floor((i_index + 1) * n_items / k_shards)
    return start, end

def upload_tree_to_gcs(local_root: str, bucket_url: str, prefix: str = ""):
    """
    Recursively upload local_root to a GCS bucket/prefix.
    bucket_url: like 'gs://elmfire-vnv-reports'
    prefix: destination prefix, no leading slash (e.g., 'runs/sha123/task_0')
    """
    # Cloud storage is optional for local listing, dry runs, and execution.
    # Import it only when an upload has actually been requested.
    from google.cloud import storage

    if not bucket_url.startswith("gs://"):
        print(f"[WARN] RESULTS_BUCKET must start with gs:// (got {bucket_url}); skipping upload.")
        return

    bucket_name = bucket_url[5:] if "/" not in bucket_url[5:] else bucket_url[5:].split("/", 1)[0]
    base_prefix = "" if "/" not in bucket_url[5:] else bucket_url[5+len(bucket_name)+1:]
    if base_prefix:
        # allow bucket URL like gs://bucket/some/base/path
        base_prefix = base_prefix.strip("/")

    client = storage.Client()
    bucket = client.bucket(bucket_name)

    # Normalize prefix
    full_prefix = "/".join([p for p in [base_prefix, prefix] if p]).strip("/")

    uploaded = 0
    for root, _, files in os.walk(local_root):
        for fname in files:
            local_path = os.path.join(root, fname)
            rel = os.path.relpath(local_path, local_root)
            rel = rel.replace("\\", "/")
            blob_path = "/".join([p for p in [full_prefix, rel] if p])

            blob = bucket.blob(blob_path)
            content_type, _ = mimetypes.guess_type(fname)
            if content_type:
                blob.content_type = content_type

            blob.upload_from_filename(local_path)
            uploaded += 1
            if uploaded % 100 == 0:
                print(f"[INFO] Uploaded {uploaded} files...")

    print(f"[OK] Uploaded {uploaded} file(s) from {local_root} to gs://{bucket_name}/{full_prefix}")

def main():
    parser = argparse.ArgumentParser(add_help=False, usage=usage())
    parser.add_argument("-l", "--list", action="store_true")
    parser.add_argument("-n", "--dry-run", action="store_true")
    parser.add_argument("-s", "--slurm", action="store_true")
    scope = parser.add_mutually_exclusive_group()
    scope.add_argument("--suite", choices=VALID_SUITES, default="all")
    scope.add_argument(
        "--verification-only", dest="suite", action="store_const", const="verification"
    )
    scope.add_argument(
        "--validation-only", dest="suite", action="store_const", const="validation"
    )
    parser.add_argument("--shard-index", type=int, default=None)
    parser.add_argument("--shard-count", type=int, default=None)
    parser.add_argument("-h", "--help", action="store_true")
    args, extra = parser.parse_known_args()

    if args.help:
        print(usage())
        sys.exit(0)

    if extra:
        print(f"[ERROR] Unexpected argument: {extra[0]}", file=sys.stderr)
        print(usage(), file=sys.stderr)
        sys.exit(2)

    # Basic paths
    root_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
    cases_dir = os.path.join(root_dir, "cases")
    common_dir = os.path.join(root_dir, "common")

    # Log context for Cloud Run logs
    commit_sha = os.getenv("COMMIT_SHA", "")
    results_bucket = os.getenv("RESULTS_BUCKET", "")
    if commit_sha:
        print(f"[INFO] COMMIT_SHA={commit_sha}")
    if results_bucket:
        print(f"[INFO] RESULTS_BUCKET={results_bucket}")

    if not os.path.isdir(cases_dir):
        print(f"[ERROR] Cases directory not found: {cases_dir}", file=sys.stderr)
        sys.exit(1)

    all_scripts = discover_cases(cases_dir, args.suite)
    total_all = len(all_scripts)

    if total_all == 0:
        print(
            f"[WARN] No {args.suite} case scripts were discovered under {cases_dir}",
            file=sys.stderr,
        )
        sys.exit(0)

    # Shard resolution
    shard_idx, shard_cnt = resolve_shard_args(args.shard_index, args.shard_count)
    s, e = shard_slice(total_all, shard_cnt, shard_idx)
    shard_scripts = all_scripts[s:e]
    total_shard = len(shard_scripts)

    print(f"[INFO] Selection: suite={args.suite}, total_selected_cases={total_all}")
    print(f"[INFO] Sharding: total_cases={total_all}, shard_index={shard_idx}, shard_count={shard_cnt}, "
          f"assigned_range=[{s}:{e}) => shard_cases={total_shard}")

    if total_shard == 0:
        print("[OK] This shard has no assigned cases. Exiting.")
        sys.exit(0)

    # Listing / dry-run only shows THIS SHARD's plan
    if args.list:
        print(f"Discovered {total_shard} case(s) in this shard:")
        for script in shard_scripts:
            print(f"  - {format_case(script, root_dir)}")
        sys.exit(0)

    if args.dry_run:
        mode = "Slurm submission" if args.slurm else "local execution"
        print(f"[DRY-RUN] {total_shard} case(s) would be run with {mode}:")
        for script in shard_scripts:
            rel = format_case(script, root_dir)
            case_dir = os.path.dirname(script)
            if args.slurm:
                print(
                    f"  - {rel} (job={slurm_job_name(case_dir)}; "
                    f"cd {case_dir} && sbatch run_case_slurm.sh)"
                )
            else:
                print(f"  - {rel} (cd {case_dir} && bash ./run_case.sh)")
        sys.exit(0)

    # Actual execution
    if args.slurm:
        conda_env = os.getenv("ELMFIRE_VNV_CONDA_ENV", "").strip()
        if not conda_env:
            print(
                "[ERROR] ELMFIRE_VNV_CONDA_ENV must be exported before Slurm submission.",
                file=sys.stderr,
            )
            print(
                "[ERROR] Set it to the Conda environment name or absolute path used by the suite.",
                file=sys.stderr,
            )
            sys.exit(2)
        print(f"[INFO] Slurm Conda environment: {conda_env}")
        print(f"[INFO] Preparing & submitting {total_shard} case(s) via Slurm ...")
    else:
        print(f"[INFO] Running {total_shard} case(s) sequentially in this shard ...")

    for idx, script in enumerate(shard_scripts, start=1):
        rel = format_case(script, root_dir)
        case_dir = os.path.dirname(script)
        print(f"\n[INFO] [{idx}/{total_shard}] {rel}")

        if not os.path.isdir(case_dir):
            print(f"[ERROR] Cannot cd into {case_dir}", file=sys.stderr)
            sys.exit(1)

        if args.slurm:
            header_path = slurm_header_for_case(script, cases_dir, common_dir)
            wrapper = make_slurm_wrapper(case_dir, header_path)
            try:
                subprocess.run(["sbatch", wrapper], cwd=case_dir, check=True)
                print(f"[OK] Submitted {rel} as {slurm_job_name(case_dir)}")
            except subprocess.CalledProcessError as e:
                print(f"[ERROR] sbatch failed in {case_dir}", file=sys.stderr)
                sys.exit(e.returncode)
        else:
            try:
                # Prefer bash explicitly to avoid executable bit issues
                subprocess.run(["bash", "./run_case.sh"], cwd=case_dir, check=True)
                print(f"[OK] Completed {rel}")
            except subprocess.CalledProcessError as e:
                print(f"[ERROR] run_case.sh failed in {case_dir}", file=sys.stderr)
                sys.exit(e.returncode)

    if args.slurm:
        print(f"\n[OK] Submitted {total_shard} job(s) from this shard.")
    else:
        print(f"\n[OK] All {total_shard} case(s) in this shard completed successfully.")

    # Upload results if configured (preserve case folder structure on GCS)
    results_bucket = os.getenv("RESULTS_BUCKET", "").strip()
    dump_dir = os.getenv("DUMP_DIR", "/elmfire/elmfire/vnv_suite").strip()  # still available if you want a full suite dump
    results_prefix = os.getenv("RESULTS_PREFIX", "").strip()
    task_idx = os.getenv("CLOUD_RUN_TASK_INDEX", "")
    if results_bucket:
        results_prefix = results_prefix.strip("/")

        try:
            # 1) Upload each case directory to a matching path in GCS:
            #    gs://<bucket>/<results_prefix>/<relative path under cases/>
            # uploaded_cases = 0
            # for script in shard_scripts:
            #     case_dir = os.path.dirname(script)
            #     # relative path like: Validation/landscape_scale/tubbs_fire
            #     rel_case = os.path.relpath(case_dir, cases_dir).replace(os.sep, "/")
            #     dest_prefix = "/".join([p for p in [results_prefix, rel_case] if p])

            #     print(f"[INFO] Uploading case '{rel_case}' from {case_dir} -> {results_bucket}/{dest_prefix}")
            #     upload_tree_to_gcs(case_dir, results_bucket, dest_prefix)
            #     uploaded_cases += 1

            # print(f"[OK] Uploaded {uploaded_cases} case folder(s) to {results_bucket}/{results_prefix}")

            # 2) Also upload suite-level artifacts if you want:
            #    Uncomment if you still want a full VnV suite snapshot at the shard root
            print(f"[INFO] Uploading suite snapshot {dump_dir} -> {results_bucket}/{results_prefix}/vnv_suite")
            upload_tree_to_gcs(dump_dir, results_bucket, f"{results_prefix}/vnv_suite")

        except Exception as e:
            # Do not hide job success just because upload failed; surface clearly.
            print(f"[ERROR] Upload to GCS failed: {e}", file=sys.stderr)
            # If you prefer to fail the job on upload issues, uncomment:
            # sys.exit(2)

if __name__ == "__main__":
    main()
