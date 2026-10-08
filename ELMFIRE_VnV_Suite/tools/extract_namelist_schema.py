#!/usr/bin/env python3
"""Extract an auditable ELMFIRE namelist schema from its Fortran reader."""

from __future__ import annotations

import argparse
import hashlib
import json
import re
import subprocess
from pathlib import Path

from namelist_lib import strip_fortran_comment


NAMELIST_START = re.compile(r"\bNAMELIST\s*/\s*([A-Za-z0-9_]+)\s*/\s*(.*)", re.IGNORECASE)
DEFAULT_ASSIGN = re.compile(r"^\s*([A-Za-z][A-Za-z0-9_]*)(?:\([^=]*\))?\s*=\s*(.+?)\s*$")


def git_value(root: Path, *args: str) -> str | None:
    result = subprocess.run(
        ["git", "-C", str(root), *args], text=True, capture_output=True, check=False
    )
    return result.stdout.strip() if result.returncode == 0 else None


def extract(source_file: Path, source_root: Path, source_label: str | None = None) -> dict:
    lines = source_file.read_text(encoding="utf-8").splitlines()
    groups: dict[str, dict] = {}
    index = 0
    while index < len(lines):
        clean = strip_fortran_comment(lines[index])
        match = NAMELIST_START.search(clean)
        if not match:
            index += 1
            continue
        group = match.group(1).upper()
        fragments = [match.group(2)]
        while strip_fortran_comment(lines[index]).rstrip().endswith("&"):
            index += 1
            fragments.append(strip_fortran_comment(lines[index]))
        joined = " ".join(fragment.replace("&", " ") for fragment in fragments)
        variables = [item.strip().upper() for item in joined.split(",") if item.strip()]
        defaults: dict[str, str] = {}
        scan = index + 1
        while scan < len(lines) and not re.search(rf"READ\s*\([^)]*NML\s*=\s*{group}\b", lines[scan], re.I):
            candidate = strip_fortran_comment(lines[scan]).strip()
            default = DEFAULT_ASSIGN.match(candidate)
            if default and default.group(1).upper() in variables:
                defaults.setdefault(default.group(1).upper(), default.group(2).strip())
            scan += 1
        groups[group] = {
            "variables": {name: {"default": defaults.get(name)} for name in variables},
            "source_line": index + 1,
        }
        index += 1

    relative_source = str(source_file.resolve().relative_to(source_root.resolve()))
    digest = hashlib.sha256(source_file.read_bytes()).hexdigest()
    commit = git_value(source_root, "rev-parse", "HEAD")
    dirty = bool(git_value(source_root, "status", "--porcelain"))
    return {
        "format_version": 1,
        "source": {
            "root_label": source_label or source_root.name,
            "file": relative_source,
            "sha256": digest,
            "git_commit": commit,
            "git_dirty": dirty,
        },
        "groups": groups,
    }


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--source-root", type=Path, required=True)
    parser.add_argument("--namelist-source", type=Path)
    parser.add_argument("--source-label", help="portable repository/release label stored in the schema")
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    source = args.namelist_source or args.source_root / "build/source/elmfire_namelists.f90"
    schema = extract(source.resolve(), args.source_root.resolve(), args.source_label)
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(schema, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    print(f"Wrote {sum(len(g['variables']) for g in schema['groups'].values())} variables to {args.output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
