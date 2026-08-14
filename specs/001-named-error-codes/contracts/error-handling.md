# Error Handling Contract

## Catalog Generation

The CMake build validates YAML source and generates Fortran constants, templates, and
lookup data in `MAPL.constants` before compiling consumers. `MAPL_Constants` re-exports
generated codes through top-level `MAPL`. Generation failure stops build and reports
source location and validation reason. Runtime code does not parse or locate YAML.

## Reporting

Supported error output MUST contain:

- Stable error code and symbolic name
- Human-readable generated-catalog message, or hardwired fallback message
- Existing source filename and line number
- Existing status or stack-capture context when available

`_VERIFY` uses `MAPL_ERROR_VERIFY`. A lookup for an undefined code uses
`MAPL_UNKNOWN_ERROR` and includes the unresolved code rather than substituting another
catalog message.

Catalog-backed and hardwired diagnostics use Fortran `ERROR_UNIT` in serial and MPI
paths.

## Macro Forms

Existing `_ASSERT(condition,message)` and `_FAIL(message)` forms remain valid legacy
forms. Migrated sites use code-bearing forms:

- `_ASSERT_CODE(condition,error_code)`
- `_FAIL_CODE(error_code)`
- `_ASSERT_CODE_CTX(condition,error_code,context)`
- `_FAIL_CODE_CTX(error_code,context)`

Code-bearing forms preserve existing return-code propagation. `_CTX` values supply
site-specific data; catalog templates supply prose. `_VERIFY(status)` reports the
propagation code while preserving original status in `rc`.

## Catalog Validation

Unsupported catalog versions are invalid input. Generation reports
`MAPL_ERROR_CATALOG_INVALID` and stops before consumers compile. Missing context values
use template fallback behavior and do not cause recursive error handling.

## Compatibility

Existing macro call forms remain valid during staged migration. Migrated `_ASSERT` and
`_FAIL` paths provide named codes; `_VERIFY` paths use the hardwired propagation code.
Unmigrated paths retain current behavior and are tracked in the migration inventory.
