#!/usr/bin/env python3
"""Validate case namelists against a source schema and case-critical invariants."""

from __future__ import annotations

import argparse
import json
from pathlib import Path

from namelist_lib import (
    NamelistError, assignment_map, base_key, contract_applicability,
    contract_files, load_json,
    parse_namelist, parse_scalar, schema_paths, values_equal,
)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("case", type=Path, help="case directory or case.yaml")
    parser.add_argument("--schema", type=Path, required=True)
    parser.add_argument("--namelist", action="append", default=[],
                        help="relative namelist path for schema-only review when case.yaml is immutable")
    parser.add_argument("--json", action="store_true")
    args = parser.parse_args()
    case_yaml = args.case if args.case.name == "case.yaml" else args.case / "case.yaml"
    case_root = case_yaml.parent
    schema = load_json(args.schema)
    valid_paths = schema_paths(schema)
    errors: list[str] = []
    checked = 0
    applicability = "required"
    try:
        if args.namelist:
            files = [{"path": item, "invariants": {}} for item in args.namelist]
        else:
            applicability = contract_applicability(case_yaml)
            files = contract_files(case_yaml)
        for entry in files:
            relative = entry.get("path")
            if not isinstance(relative, str):
                errors.append("contract file entry has no string path")
                continue
            path = case_root / relative
            if not path.is_file():
                errors.append(f"missing namelist: {relative}")
                continue
            template = entry.get("template", False)
            if not isinstance(template, bool):
                errors.append(f"{relative}: template must be true or false")
                continue
            assignments = parse_namelist(
                path, allow_template_placeholders=template
            )
            actual = assignment_map(assignments)
            for assignment in assignments:
                if assignment.base_path not in valid_paths:
                    errors.append(f"{relative}:{assignment.line}: unsupported {assignment.base_path}")
            invariants = entry.get("invariants", {})
            if not isinstance(invariants, dict):
                errors.append(f"{relative}: invariants must be a mapping")
                continue
            for invariant, expected in invariants.items():
                key = invariant.upper()
                checked += 1
                assignment = actual.get(key)
                if assignment is None:
                    errors.append(f"{relative}: missing invariant {key}")
                    continue
                actual_value = parse_scalar(assignment.value)
                if not values_equal(actual_value, expected):
                    errors.append(f"{relative}: {key} is {actual_value!r}, expected {expected!r}")
    except (NamelistError, OSError, ValueError) as exc:
        errors.append(str(exc))
    if applicability == "not_applicable" and not errors:
        passing_status = "NOT APPLICABLE"
    elif args.namelist and not errors:
        passing_status = "SCHEMA ONLY"
    else:
        passing_status = "PASS"
    result = {"case": str(case_root), "schema": str(args.schema), "checked_invariants": checked,
              "status": passing_status if not errors else "FAIL", "errors": errors}
    if args.json:
        print(json.dumps(result, indent=2))
    else:
        print(f"{result['status']}: {case_root} ({checked} invariants)")
        for error in errors:
            print(f"  - {error}")
    return 0 if not errors else 1


if __name__ == "__main__":
    raise SystemExit(main())
