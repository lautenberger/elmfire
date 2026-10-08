#!/usr/bin/env python3
"""Content fingerprints for generated CASE46 inputs and oracle artifacts."""
from __future__ import annotations

import hashlib
import json
from pathlib import Path


def _sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def runtime_files(variant_root: Path) -> list[Path]:
    files = [variant_root / "elmfire.data"]
    files.extend(sorted((variant_root / "inputs").glob("*.tif")))
    files.extend(sorted((variant_root / "misc").glob("*.csv")))
    if any(not path.is_file() for path in files) or len(files) < 4:
        raise FileNotFoundError(f"runtime input set is incomplete under {variant_root}")
    return sorted(files, key=lambda path: path.relative_to(variant_root).as_posix())


def variant_fingerprint(variant_root: Path) -> dict[str, object]:
    digest = hashlib.sha256()
    entries: list[dict[str, object]] = []
    for path in runtime_files(variant_root):
        relative = path.relative_to(variant_root).as_posix()
        file_digest = _sha256(path)
        size = path.stat().st_size
        digest.update(relative.encode("utf-8") + b"\0")
        digest.update(file_digest.encode("ascii") + b"\0")
        digest.update(str(size).encode("ascii") + b"\n")
        entries.append({"path": relative, "sha256": file_digest, "size_bytes": size})
    return {"sha256": digest.hexdigest(), "file_count": len(entries), "files": entries}


def aggregate_fingerprint(variants: dict[str, dict[str, object]]) -> str:
    compact = {key: value["sha256"] for key, value in sorted(variants.items())}
    encoded = json.dumps(compact, sort_keys=True, separators=(",", ":")).encode("utf-8")
    return hashlib.sha256(encoded).hexdigest()


def _manifest(paths: list[Path], base: Path) -> dict[str, object]:
    digest = hashlib.sha256()
    entries: list[dict[str, object]] = []
    for path in sorted(paths, key=lambda item: item.relative_to(base).as_posix()):
        if not path.is_file():
            raise FileNotFoundError(f"required fingerprint artifact is absent: {path}")
        relative = path.relative_to(base).as_posix()
        file_digest = _sha256(path)
        size = path.stat().st_size
        digest.update(relative.encode("utf-8") + b"\0")
        digest.update(file_digest.encode("ascii") + b"\0")
        digest.update(str(size).encode("ascii") + b"\n")
        entries.append({"path": relative, "sha256": file_digest, "size_bytes": size})
    return {"sha256": digest.hexdigest(), "file_count": len(entries), "files": entries}


def oracle_artifact_fingerprint(case_dir: Path) -> dict[str, object]:
    """Bind the exact expected matrix, per-variant metadata, and preflight."""
    variants_dir = case_dir / "variants"
    paths = [
        variants_dir / "expected.json",
        variants_dir / "variant_ids.txt",
        case_dir / "outputs/source_selector_preflight.json",
    ]
    paths.extend(sorted(variants_dir.glob("*/variant.json")))
    if len(paths) != 10:
        raise FileNotFoundError(
            "CASE46 requires expected.json, variant_ids.txt, preflight, and seven variant.json files"
        )
    return _manifest(paths, case_dir)


def sha256_file(path: Path) -> str:
    """Public helper used by the completion-receipt validator."""
    return _sha256(path)
