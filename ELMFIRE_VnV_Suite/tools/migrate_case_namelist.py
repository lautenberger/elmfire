#!/usr/bin/env python3
"""Create review-only namelist candidates for a target ELMFIRE schema."""

from __future__ import annotations

import argparse
import json
from pathlib import Path

from namelist_lib import (
    Assignment, NamelistError, assignment_map, contract_files, load_json,
    parse_namelist, parse_scalar, render_namelist, schema_paths, values_equal,
)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("case", type=Path)
    parser.add_argument("--target-schema", type=Path, required=True)
    parser.add_argument("--rules", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    parser.add_argument("--namelist", action="append", default=[],
                        help="relative path for an immutable case without a namelist_contract")
    args = parser.parse_args()
    case_yaml = args.case if args.case.name == "case.yaml" else args.case / "case.yaml"
    case_root = case_yaml.parent.resolve()
    output_root = args.output_dir.resolve()
    if output_root == case_root or case_root in output_root.parents:
        raise SystemExit("Refusing to write migration candidates inside the case directory")
    schema, catalog = load_json(args.target_schema), load_json(args.rules)
    valid = schema_paths(schema)
    rules = {item["from"].upper(): item for item in catalog.get("rules", [])}
    unresolved: list[dict] = []
    report: dict = {"case": str(case_root), "target_schema": str(args.target_schema), "files": []}
    try:
        files = ([{"path": item, "invariants": {}} for item in args.namelist]
                 if args.namelist else contract_files(case_yaml))
        for file_entry in files:
            relative = file_entry["path"]
            source = case_root / relative
            migrated: list[Assignment] = []
            changes: list[dict] = []
            for assignment in parse_namelist(source):
                if assignment.base_path in valid:
                    migrated.append(assignment)
                    continue
                rule = rules.get(assignment.base_path)
                if rule is None:
                    unresolved.append({"file": relative, "path": assignment.base_path, "value": assignment.value})
                    migrated.append(assignment)
                    continue
                action = rule.get("action")
                if action == "drop":
                    changes.append({"from": assignment.base_path, "action": "drop", "reason": rule.get("reason")})
                    continue
                if action not in {"move", "rename"}:
                    unresolved.append({"file": relative, "path": assignment.base_path,
                                       "reason": f"unsupported migration action: {action!r}"})
                    migrated.append(assignment)
                    continue
                target = rule.get("to", "").upper()
                if target not in valid:
                    unresolved.append({"file": relative, "path": assignment.base_path,
                                       "reason": f"rule target is absent: {target}"})
                    migrated.append(assignment)
                    continue
                target_group, target_key = target.split(".", 1)
                if "(" in assignment.key:
                    target_key += assignment.key[assignment.key.index("("):]
                value = assignment.value
                value_map = {str(k).upper(): str(v) for k, v in rule.get("value_map", {}).items()}
                value = value_map.get(value.upper(), value)
                migrated.append(Assignment(target_group, target_key, value, assignment.line))
                changes.append({"from": assignment.base_path, "to": target, "value": value,
                                "reason": rule.get("reason")})
            try:
                migrated_by_path = assignment_map(migrated)
            except NamelistError as exc:
                unresolved.append({"file": relative, "reason": str(exc)})
                migrated_by_path = {}
            for invariant, expected in file_entry.get("invariants", {}).items():
                migrated_assignment = migrated_by_path.get(invariant.upper())
                if migrated_assignment is None:
                    unresolved.append({"file": relative, "invariant": invariant,
                                       "reason": "missing after migration"})
                elif not values_equal(parse_scalar(migrated_assignment.value), expected):
                    unresolved.append({"file": relative, "invariant": invariant,
                                       "expected": expected, "actual": migrated_assignment.value,
                                       "reason": "changed by migration"})
            destination = output_root / case_root.name / relative
            destination.parent.mkdir(parents=True, exist_ok=True)
            destination.write_text(render_namelist(migrated, f"source: {source}"), encoding="utf-8")
            report["files"].append({"source": relative, "candidate": str(destination), "changes": changes})
    except (NamelistError, OSError, KeyError, ValueError) as exc:
        unresolved.append({"error": str(exc)})
    report["unresolved"] = unresolved
    report["status"] = "READY FOR REVIEW" if not unresolved else "BLOCKED"
    output_root.mkdir(parents=True, exist_ok=True)
    report_path = output_root / f"{case_root.name}_migration_report.json"
    report_path.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
    print(f"{report['status']}: {report_path}")
    return 0 if not unresolved else 2


if __name__ == "__main__":
    raise SystemExit(main())
