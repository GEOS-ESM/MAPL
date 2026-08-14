# Quickstart: Named Error Code Catalog

## Prerequisites

- MAPL dependencies installed, including ESMF and pFUnit.
- A configured MAPL build directory.
- A Fortran test or MAPL application that initializes error handling.

## Validate Catalog Generation

1. Build MAPL with tests enabled.
2. Change `utils/Constants/mapl_error_codes.yaml` and confirm CMake regenerates the
   Fortran catalog module exposed through `MAPL_Constants`.
3. Trigger one migrated `_ASSERT_CODE` or `_FAIL_CODE` path.
4. Confirm serial and MPI `ERROR_UNIT` include symbolic name, integer code, generated
   catalog message, source location, and existing context.

Expected result: the named entry is generated at build time and status propagation
remains unchanged without runtime YAML access.

## Validate Hardwired Fallbacks

Run the same scenario with each input:

- Missing required YAML source
- Malformed YAML
- YAML with duplicate or incomplete entry
- YAML with unsupported catalog version
- YAML template with missing context value

Expected results:

- Missing or malformed source stops generation with a clear build diagnostic.
- Generated hardwired entries report `MAPL_ERROR_CATALOG_INVALID` when generation rejects input.
- `_VERIFY` reports `MAPL_ERROR_VERIFY` and preserves stack/status context.
- Undefined runtime lookup reports `MAPL_UNKNOWN_ERROR` and includes unresolved code.
- Missing optional context produces safe fallback text.
- Existing stack capture and return behavior remain available in every case.

## Run Tests

```text
ctest --test-dir build-dir -L ESSENTIAL -R MAPL.utils.tests
```

The test suite must cover generation validation, fallback diagnostics, code-bearing and
context macros, and representative `ERROR_UNIT` records.

## Validation Record

With `gfortran-stack` loaded, `ctest --test-dir gfortran -L ESSENTIAL
--output-on-failure` passed all 65 tests on 2026-08-14.
