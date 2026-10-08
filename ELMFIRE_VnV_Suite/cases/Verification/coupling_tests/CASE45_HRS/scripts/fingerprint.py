#!/usr/bin/env python3
"""Content fingerprints for generated inputs and completed output snapshots."""
from __future__ import annotations

import hashlib
import json
from pathlib import Path


def sha256_file(path: Path) -> str:
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
        file_digest = sha256_file(path)
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


def file_snapshot(paths: list[Path], base: Path) -> dict[str, object]:
    """Hash an explicit file set, including names and sizes, relative to *base*."""
    entries: list[dict[str, object]] = []
    for path in sorted(paths, key=lambda item: item.relative_to(base).as_posix()):
        if not path.is_file():
            raise FileNotFoundError(path)
        entries.append(
            {
                "path": path.relative_to(base).as_posix(),
                "sha256": sha256_file(path),
                "size_bytes": path.stat().st_size,
            }
        )
    encoded = json.dumps(entries, sort_keys=True, separators=(",", ":")).encode("utf-8")
    return {
        "sha256": hashlib.sha256(encoded).hexdigest(),
        "file_count": len(entries),
        "files": entries,
    }


def output_snapshot(variant_root: Path) -> dict[str, object]:
    """Fingerprint every regular output except the completion marker itself."""
    output_dir = variant_root / "outputs"
    paths = [
        path
        for path in output_dir.rglob("*")
        if path.is_file() and path.name != "run_complete.json"
    ]
    if not paths:
        raise FileNotFoundError(f"no ELMFIRE outputs exist under {output_dir}")
    return file_snapshot(paths, variant_root)
