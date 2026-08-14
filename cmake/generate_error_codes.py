#!/usr/bin/env python3
"""Generate MAPL Fortran error-code constants and catalog data from YAML."""

import argparse
import re
from pathlib import Path

import yaml


NAME_RE = re.compile(r"^[A-Z][A-Z0-9_]*$")
MAX_NAME_LENGTH = 64
MAX_TEMPLATE_LENGTH = 512
MAX_FIELDS_LENGTH = 256


def fail(message):
    raise ValueError(message)


def fortran_string(value):
    return "'" + value.replace("'", "''") + "'"


def load_catalog(path):
    with path.open(encoding="utf-8") as stream:
        catalog = yaml.safe_load(stream)
    if not isinstance(catalog, dict) or str(catalog.get("version")) != "1":
        fail("catalog version must be 1")
    errors = catalog.get("errors")
    if not isinstance(errors, dict) or not errors:
        fail("catalog errors must be non-empty mapping")

    entries = []
    codes = set()
    for name, entry in errors.items():
        if not isinstance(name, str) or not NAME_RE.fullmatch(name):
            fail(f"invalid error name: {name!r}")
        if len(name) > MAX_NAME_LENGTH:
            fail(f"error name too long: {name}")
        if not isinstance(entry, dict):
            fail(f"entry {name} must be mapping")
        code = entry.get("code")
        template = entry.get("template")
        category = entry.get("category")
        status = entry.get("status")
        fields = entry.get("fields", [])
        if not isinstance(code, int) or isinstance(code, bool) or code < 0:
            fail(f"entry {name} has invalid code")
        if code in codes:
            fail(f"duplicate error code: {code}")
        if not isinstance(template, str) or not template.strip():
            fail(f"entry {name} has empty template")
        if len(template) > MAX_TEMPLATE_LENGTH:
            fail(f"template too long: {name}")
        if not isinstance(category, str) or not category.strip():
            fail(f"entry {name} has empty category")
        if status not in ("active", "deprecated"):
            fail(f"entry {name} has invalid status")
        if not isinstance(fields, list) or any(not isinstance(field, str) for field in fields):
            fail(f"entry {name} has invalid fields")
        if len(" ".join(fields)) > MAX_FIELDS_LENGTH:
            fail(f"fields too long: {name}")
        placeholders = set(re.findall(r"\{([A-Za-z_][A-Za-z0-9_]*)\}", template))
        if not placeholders.issubset(fields):
            fail(f"entry {name} template fields are not declared")
        codes.add(code)
        entries.append((name, code, template, " ".join(fields)))

    entries.sort(key=lambda item: item[1])
    return entries


def emit(output, entries):
    max_name = max(MAX_NAME_LENGTH, *(len(name) for name, *_ in entries))
    max_template = max(MAX_TEMPLATE_LENGTH, *(len(template) for _, _, template, _ in entries))
    max_fields = max(MAX_FIELDS_LENGTH, *(len(fields) for *_, fields in entries))
    lines = [
        "module mapl_ErrorCodes_generated_mod",
        "   implicit none",
        "   private",
        "",
        "   public :: MAPL_ERROR_CODE_COUNT",
    ]
    lines.extend(f"   public :: {name}" for name, *_ in entries)
    lines.extend([
        "   public :: MAPL_ERROR_CODES",
        "   public :: MAPL_ERROR_NAMES",
        "   public :: MAPL_ERROR_TEMPLATES",
        "   public :: MAPL_ERROR_FIELDS",
        "",
        f"   integer, parameter :: MAPL_ERROR_CODE_COUNT = {len(entries)}",
    ])
    lines.extend(f"   integer, parameter :: {name} = {code}" for name, code, *_ in entries)
    lines.extend([
        "",
        f"   integer, parameter :: MAPL_ERROR_CODES(MAPL_ERROR_CODE_COUNT) = [{', '.join(str(code) for _, code, *_ in entries)}]",
        f"   character(len={max_name}), parameter :: MAPL_ERROR_NAMES(MAPL_ERROR_CODE_COUNT) = &",
        "      [character(len=" + str(max_name) + ") :: &",
        "         " + ", &\n         ".join(fortran_string(name) for name, *_ in entries) + "]",
        f"   character(len={max_template}), parameter :: MAPL_ERROR_TEMPLATES(MAPL_ERROR_CODE_COUNT) = &",
        "      [character(len=" + str(max_template) + ") :: &",
        "         " + ", &\n         ".join(fortran_string(template) for _, _, template, _ in entries) + "]",
        f"   character(len={max_fields}), parameter :: MAPL_ERROR_FIELDS(MAPL_ERROR_CODE_COUNT) = &",
        "      [character(len=" + str(max_fields) + ") :: &",
        "         " + ", &\n         ".join(fortran_string(fields) for *_, fields in entries) + "]",
        "",
        "end module mapl_ErrorCodes_generated_mod",
        "",
    ])
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text("\n".join(lines), encoding="utf-8")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("source", type=Path)
    parser.add_argument("output", type=Path)
    args = parser.parse_args()
    try:
        emit(args.output, load_catalog(args.source))
    except (OSError, ValueError, yaml.YAMLError) as error:
        parser.error(str(error))


if __name__ == "__main__":
    main()
