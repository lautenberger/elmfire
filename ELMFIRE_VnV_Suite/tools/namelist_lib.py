#!/usr/bin/env python3
"""Shared, dependency-light helpers for ELMFIRE namelist version checks."""

from __future__ import annotations

import json
import math
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Iterable


GROUP_RE = re.compile(r"^\s*&([A-Za-z][A-Za-z0-9_]*)\s*$")
TEMPLATE_DIRECTIVE_RE = re.compile(r"^\s*@[A-Za-z][A-Za-z0-9_]*@\s*$")
ASSIGN_START_RE = re.compile(
    r"(?:^|,)\s*([A-Za-z][A-Za-z0-9_]*(?:\([^=]*\))?)\s*=\s*"
)


class NamelistError(ValueError):
    """Raised when a namelist or contract cannot be interpreted safely."""


@dataclass(frozen=True)
class Assignment:
    group: str
    key: str
    value: str
    line: int

    @property
    def path(self) -> str:
        return f"{self.group}.{self.key}"

    @property
    def base_path(self) -> str:
        return f"{self.group}.{base_key(self.key)}"


def strip_fortran_comment(line: str) -> str:
    """Remove an unquoted Fortran comment from one line."""
    quote: str | None = None
    result: list[str] = []
    index = 0
    while index < len(line):
        char = line[index]
        if char in "'\"":
            if quote == char and index + 1 < len(line) and line[index + 1] == char:
                result.extend((char, char))
                index += 2
                continue
            quote = None if quote == char else (char if quote is None else quote)
        if char == "!" and quote is None:
            break
        result.append(char)
        index += 1
    return "".join(result)


def base_key(key: str) -> str:
    return key.split("(", 1)[0].strip().upper()


def parse_namelist(
    path: Path, *, allow_template_placeholders: bool = False
) -> list[Assignment]:
    """Parse an ELMFIRE namelist without evaluating assignment values.

    A canonical case template may contain a standalone ``@TOKEN@`` that a
    deterministic preprocessor replaces with one or more assignments. Such a
    directive is ignored only when the caller explicitly enables template
    placeholders; ordinary namelists continue to reject it.
    """
    assignments: list[Assignment] = []
    group: str | None = None
    for line_number, original in enumerate(path.read_text(encoding="utf-8").splitlines(), 1):
        line = strip_fortran_comment(original).strip()
        if not line:
            continue
        match = GROUP_RE.match(line)
        if match:
            if group is not None:
                raise NamelistError(f"{path}:{line_number}: nested namelist group")
            group = match.group(1).upper()
            continue
        if line in {"/", "&END", "&end"}:
            group = None
            continue
        if group is None:
            continue
        if allow_template_placeholders and TEMPLATE_DIRECTIVE_RE.match(line):
            continue
        matches = list(ASSIGN_START_RE.finditer(line))
        if not matches or matches[0].start() != 0:
            raise NamelistError(
                f"{path}:{line_number}: unsupported namelist assignment syntax"
            )
        for match_index, match in enumerate(matches):
            value_end = matches[match_index + 1].start() if match_index + 1 < len(matches) else len(line)
            value = line[match.end():value_end].strip().rstrip(",").strip()
            if not value:
                raise NamelistError(f"{path}:{line_number}: empty value for {match.group(1)}")
            assignments.append(
                Assignment(group, match.group(1).strip().upper(), value, line_number)
            )
    if group is not None:
        raise NamelistError(f"{path}: unterminated &{group} group")
    return assignments


def parse_scalar(raw: str) -> Any:
    """Convert a scalar Fortran literal to a comparable Python value."""
    value = raw.strip().rstrip(",").strip()
    upper = value.upper()
    if upper in {".TRUE.", "TRUE"}:
        return True
    if upper in {".FALSE.", "FALSE"}:
        return False
    if len(value) >= 2 and value[0] == value[-1] and value[0] in "'\"":
        return value[1:-1].replace(value[0] * 2, value[0])
    numeric = re.sub(r"(?<=\d)[dD](?=[+-]?\d)", "e", value)
    try:
        return int(numeric)
    except ValueError:
        try:
            return float(numeric)
        except ValueError:
            return value


def values_equal(actual: Any, expected: Any, *, atol: float = 0.0, rtol: float = 1e-12) -> bool:
    if isinstance(actual, bool) or isinstance(expected, bool):
        return actual is expected
    if isinstance(actual, (int, float)) and isinstance(expected, (int, float)):
        return math.isclose(float(actual), float(expected), abs_tol=atol, rel_tol=rtol)
    return str(actual).strip().upper() == str(expected).strip().upper()


def load_yaml(path: Path) -> dict[str, Any]:
    try:
        import yaml
    except ImportError as exc:  # pragma: no cover - environment-dependent
        raise NamelistError("PyYAML is required: python3 -m pip install PyYAML") from exc
    data = yaml.safe_load(path.read_text(encoding="utf-8"))
    if not isinstance(data, dict):
        raise NamelistError(f"{path}: expected a YAML mapping")
    return data


def load_json(path: Path) -> dict[str, Any]:
    data = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(data, dict):
        raise NamelistError(f"{path}: expected a JSON object")
    return data


def schema_paths(schema: dict[str, Any]) -> set[str]:
    result: set[str] = set()
    for group, details in schema.get("groups", {}).items():
        for variable in details.get("variables", {}):
            result.add(f"{group.upper()}.{variable.upper()}")
    return result


def namelist_contract(case_yaml: Path) -> dict[str, Any]:
    """Load and validate the common namelist-contract envelope."""
    metadata = load_yaml(case_yaml)
    contract = metadata.get("namelist_contract")
    if not isinstance(contract, dict):
        raise NamelistError(f"{case_yaml}: missing namelist_contract")
    if contract.get("schema_version") != 1:
        raise NamelistError(f"{case_yaml}: namelist_contract.schema_version must be 1")
    applicability = contract.get("applicability", "required")
    if applicability not in {"required", "not_applicable"}:
        raise NamelistError(
            f"{case_yaml}: namelist_contract.applicability must be required or not_applicable"
        )
    if applicability == "not_applicable":
        reason = contract.get("reason")
        if not isinstance(reason, str) or not reason.strip():
            raise NamelistError(
                f"{case_yaml}: a not_applicable namelist contract requires a reason"
            )
    return contract


def contract_applicability(case_yaml: Path) -> str:
    """Return whether the case executes an ELMFIRE namelist."""
    return str(namelist_contract(case_yaml).get("applicability", "required"))


def contract_files(case_yaml: Path) -> list[dict[str, Any]]:
    contract = namelist_contract(case_yaml)
    files = contract.get("files")
    if contract.get("applicability", "required") == "not_applicable":
        if files not in (None, []):
            raise NamelistError(
                f"{case_yaml}: a not_applicable namelist contract cannot list files"
            )
        return []
    if not isinstance(files, list) or not files:
        raise NamelistError(f"{case_yaml}: namelist_contract.files must be a non-empty list")
    return files


def assignment_map(assignments: Iterable[Assignment]) -> dict[str, Assignment]:
    result: dict[str, Assignment] = {}
    for assignment in assignments:
        if assignment.path in result:
            raise NamelistError(f"duplicate assignment: {assignment.path}")
        result[assignment.path] = assignment
    return result


def render_namelist(assignments: Iterable[Assignment], provenance: str) -> str:
    """Render a review candidate; comments intentionally identify its generated status."""
    grouped: dict[str, list[Assignment]] = {}
    for assignment in assignments:
        grouped.setdefault(assignment.group, []).append(assignment)
    lines = ["! REVIEW CANDIDATE - do not adopt without contract and source review", f"! {provenance}", ""]
    for group, entries in grouped.items():
        lines.append(f"&{group}")
        for entry in entries:
            lines.append(f"  {entry.key} = {entry.value}")
        lines.extend(("/", ""))
    return "\n".join(lines)
