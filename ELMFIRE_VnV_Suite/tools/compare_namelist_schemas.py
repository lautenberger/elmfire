#!/usr/bin/env python3
"""Compare two extracted ELMFIRE namelist schemas."""

from __future__ import annotations

import argparse
import json
from pathlib import Path

from namelist_lib import load_json, schema_paths


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("old", type=Path)
    parser.add_argument("new", type=Path)
    parser.add_argument("--json", action="store_true")
    args = parser.parse_args()
    old, new = load_json(args.old), load_json(args.new)
    old_paths, new_paths = schema_paths(old), schema_paths(new)
    old_candidates: dict[str, list[str]] = {}
    new_candidates: dict[str, list[str]] = {}
    for path in old_paths:
        old_candidates.setdefault(path.split(".", 1)[1], []).append(path)
    for path in new_paths:
        new_candidates.setdefault(path.split(".", 1)[1], []).append(path)
    old_by_key = {key: paths[0] for key, paths in old_candidates.items() if len(paths) == 1}
    new_by_key = {key: paths[0] for key, paths in new_candidates.items() if len(paths) == 1}
    moved = sorted(
        {key: {"from": old_by_key[key], "to": new_by_key[key]} for key in old_by_key.keys() & new_by_key.keys()
         if old_by_key[key] != new_by_key[key]}.values(), key=lambda item: item["from"]
    )
    moved_from = {item["from"] for item in moved}
    moved_to = {item["to"] for item in moved}
    defaults = []
    for path in sorted(old_paths & new_paths):
        group, key = path.split(".", 1)
        before = old["groups"][group]["variables"][key].get("default")
        after = new["groups"][group]["variables"][key].get("default")
        if before != after:
            defaults.append({"path": path, "from": before, "to": after})
    result = {
        "added": sorted(new_paths - old_paths - moved_to),
        "removed": sorted(old_paths - new_paths - moved_from),
        "moved_same_name": moved,
        "default_changes": defaults,
    }
    if args.json:
        print(json.dumps(result, indent=2))
    else:
        for label, items in result.items():
            print(f"{label}: {len(items)}")
            for item in items:
                print(f"  {item}")
    return 1 if result["removed"] else 0


if __name__ == "__main__":
    raise SystemExit(main())
