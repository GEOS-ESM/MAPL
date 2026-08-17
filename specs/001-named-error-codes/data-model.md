# Data Model: Named Error Code Catalog

## Error Code

Represents stable identity for an error condition.

| Field | Type | Rules |
|---|---|---|
| `name` | string | Required, non-empty, unique; matches MAPL symbolic constant |
| `code` | integer | Required, stable, unique; nonzero for failures |
| `category` | string | Required for catalog entries; identifies assertion, failure, propagation, or catalog handling |
| `status` | string | `active` or `deprecated`; deprecated codes cannot be reused |

## Error Message

Human-readable diagnostic associated with one error code.

| Field | Type | Rules |
|---|---|---|
| `template` | string | Required, non-empty; safe fallback text when context is unavailable |
| `context` | string | Optional diagnostic context supplied by reporting path |

## Error Catalog

Versioned YAML source collection compiled into generated Fortran data.

| Field | Type | Rules |
|---|---|---|
| `version` | string | Required catalog format version |
| `errors` | mapping | Required; each key is a unique symbolic name and each entry has a unique `code` |
| `template` | string | Required human-readable message template |
| `fields` | sequence | Optional context names referenced by template |

Build-time validation rejects missing required fields, duplicate names or codes,
malformed values, empty templates, unsupported versions, and entries whose symbolic
name has no corresponding MAPL constant or reserved hardwired definition. Generated
Fortran contains constants, templates, and lookup data; runtime code does not retain a
YAML path.

## Hardwired Entries

These entries are emitted into generated Fortran first and remain available as runtime
fallbacks when catalog lookup fails:

| Entry | Purpose |
|---|---|
| `MAPL_UNKNOWN_ERROR` (code 19) | Undefined runtime lookup or unavailable message |
| `MAPL_FILE_NOT_FOUND` (code 1) | Existing file-not-found return code |
| `MAPL_ERROR_CATALOG_MISSING` (code 10) | Catalog file absent or unreadable |
| `MAPL_ERROR_CATALOG_INVALID` (code 11) | Catalog malformed, partial, duplicate, or otherwise invalid |
| `MAPL_ERROR_VERIFY` (code 12) | `_VERIFY` propagation and preserved status context |
| `MAPL_SUCCESS` | Successful return status |

Existing MAPL codes 0 through 9 remain unchanged, including `MAPL_FILE_NOT_FOUND = 1`.
`MAPL_UNKNOWN_ERROR = 19` resolves prior duplicate numbering. New numeric assignments
must be reserved in generated public constants and never reused after publication.

## Error Path Lifecycle

1. CMake validates YAML source.
2. CMake generates Fortran constants, templates, and lookup data.
3. Hardwired entries initialize before generated catalog data is used.
4. Migrated macro sites report named codes; legacy sites retain existing behavior and
   use generic legacy diagnostics until migration.
5. Context values are formatted into templates; absent optional values use safe
   fallback text.

## Error Merge Decision

Records why multiple source messages share or do not share a canonical error code.

| Field | Type | Rules |
|---|---|---|
| `source_sites` | sequence | Required; every merged source location is listed |
| `canonical_code` | identifier | Required; one stable code for semantically equivalent sites |
| `condition_summary` | string | Required; describes shared failure meaning |
| `rationale` | string | Required; explains equivalence or separation decision |
| `context_fields` | sequence | Required when site-specific details remain useful |
| `rejected_near_matches` | sequence | Required for reviewed similar-but-distinct messages |

Merge decisions compare semantic condition, error category, recovery behavior, and
operator action. Text resemblance alone is insufficient.

## Generated Catalog Module

Generated module exports one named integer constant per catalog entry and lookup data
for templates and required context fields. `MAPL_Constants` re-exports generated codes
to top-level `MAPL`; `utils/API.F90` does not maintain a second code list. Generated
output is build-local and must be reproducible from identical YAML input.
