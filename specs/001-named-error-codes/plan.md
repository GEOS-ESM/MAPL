# Implementation Plan: Named Error Code Catalog

**Branch**: `001-named-error-codes` | **Date**: 2026-08-14 | **Spec**: [spec.md](spec.md)

**GitHub Issue**: [#5324](https://github.com/GEOS-ESM/MAPL/issues/5324)

**Input**: Feature specification from `/specs/001-named-error-codes/spec.md`

## Summary

Extend MAPL's existing MPI-free error handling with stable named integer codes,
YAML-to-Fortran catalog generation at CMake time, hardwired fallback diagnostics, and
a reviewed semantic merge process for duplicate error meanings. Preserve current stack
capture and macro compatibility while allowing context values at migrated sites.

## Technical Context

**Language/Version**: Fortran 2003 or newer; C preprocessor macros

**Primary Dependencies**: Existing GFTL shared map support, pFUnit for tests, CMake
3.24+, Python 3 with generator dependencies available to the build

**Storage**: Versioned YAML source under `utils/Constants/`; generated Fortran module
owned by `MAPL.constants`; no runtime YAML file dependency

**Testing**: Generator validation plus pFUnit tests under `utils/tests`, CTest ESSENTIAL
label, Essential integration coverage for `ERROR_UNIT` routing; no direct pFUnit I/O
capture

**Target Platform**: MAPL-supported serial and MPI environments on GNU, Intel, and NAG
Fortran toolchains

**Project Type**: MPI-capable scientific Fortran library with serial foundational
utilities

**Performance Goals**: Generated catalog lookup performs no runtime file I/O and remains
bounded by in-memory map lookup.

**Constraints**: YAML generation and validation must finish before catalog consumers
compile; hardwired diagnostics must preserve stack capture; legacy macro call forms
must remain valid while code-bearing and `_CTX` forms support migration; serial and MPI
reporting must use `ERROR_UNIT`; catalog codes must never be silently reused

**Scale/Scope**: One process-local generated catalog; representative MAPL-owned macro migration
with inventory of all remaining MAPL-owned `_ASSERT`, `_FAIL`, and `_VERIFY` sites;
semantic grouping of existing assertion/failure messages before canonical assignment

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

- Stable component contracts: PASS. Existing macro forms remain compatible; generated
  catalog integration is additive.
- Standards-based Fortran and portability: PASS. Design uses existing Fortran, ESMF,
  and compiler-supported interfaces; no compiler-specific behavior is required.
- Verification before integration: PASS. pFUnit and CTest scenarios cover generated,
  invalid, fallback, macro, context, and `ERROR_UNIT` behavior.
- Reproducible builds and dependencies: PASS. YAML generation, catalog schema, and
  generator dependencies are documented and build-time validated.
- Scientific integrity and observable failure: PASS. Error codes/messages are stable,
  fallback diagnostics preserve context, and output remains observable.
- Technical constraints: PASS. Design remains within CMake, ESMF, MPI, and supported
  compiler constraints.
- Development workflow: PASS. This plan is tied to the feature specification and must
  be implemented through its linked GitHub issue before code modification.

## Project Structure

### Documentation (this feature)

```text
specs/001-named-error-codes/
├── plan.md
├── research.md
├── data-model.md
├── quickstart.md
├── error-code-inventory.md
├── contracts/
│   ├── error-catalog.yaml
│   └── error-handling.md
└── checklists/requirements.md
```

### Source Code (repository root)

```text
utils/
├── ErrorHandling.F90              # Catalog state, initialization, lookup, fallbacks
├── Constants/
│   ├── Constants.F90                # Re-exports generated codes through MAPL_Constants
│   └── mapl_error_codes.yaml         # YAML source of truth
└── tests/
    ├── CMakeLists.txt              # Register error-handling pFUnit sources
    └── test_ErrorHandling.pf       # Catalog, fallback, and reporting tests

cmake/
└── generate_error_codes.py         # Validate YAML and emit generated Fortran module

mp_utils/
└── MAPL_MpiErrorHandling.F90       # Preserve MPI initialization integration

include/
└── MAPL_ErrLog.h                   # Legacy and code-bearing macro forms

base/SimpleBundleMod.F90             # Representative _ASSERT/_FAIL/_VERIFY sites
mp_utils/Partition.F90               # Representative assertion sites
superstructure/generic/UserSetServices.F90
superstructure/generic/vertical/FixedLevelsVerticalGrid.F90
include/MAPL_private_state.h         # Representative preprocessor macro sites
```

**Structure Decision**: Keep YAML source and generated module ownership in
`MAPL.constants`. Generate a Fortran module in the build tree through
`cmake/generate_error_codes.py`; `MAPL_Constants` re-exports it and top-level `MAPL`
provides public access. `MAPL.utils` consumes constants-layer data and does not maintain
manual code exports. No YAML file is required at runtime. Keep MPI-specific throw/abort
integration in `MAPL.mp_utils` and route both serial and MPI diagnostics to `ERROR_UNIT`.
Add code-bearing and `_CTX` macro forms in `include/MAPL_ErrLog.h`, keep legacy forms,
and maintain an error-code inventory for semantic merge decisions.

## Phase 0: Research Complete

Research decisions are recorded in [research.md](research.md). No unresolved
technical-context questions remain.

## Phase 1: Design Complete

Data entities and lifecycle are recorded in [data-model.md](data-model.md). Public
behavior and YAML shape are recorded in [contracts/](contracts/). Runnable validation
steps are recorded in [quickstart.md](quickstart.md). Semantic grouping and source
coverage are recorded in [error-code-inventory.md](error-code-inventory.md).

## Constitution Check: Post-Design

- Contract compatibility remains preserved through additive initialization and
  fallback behavior.
- Hardwired entries guarantee observable failure and stack capture when generated
  catalog data is unavailable.
- Generated Fortran is the deployed catalog representation; runtime YAML parsing and
  path discovery are excluded.
- Code-bearing macro forms preserve legacy source compatibility while making migrated
  code-to-catalog correspondence explicit; `_CTX` variants carry values only.
- Complete baseline YAML includes existing codes 0-9, hardwired codes 10-12,
  `MAPL_UNKNOWN_ERROR = 19`, and representative migrated entries.
- Existing numeric codes are defined once in YAML and exposed through `MAPL_Constants`
  and top-level `MAPL`; duplicate source declarations are removed.
- Serial and MPI reporting use `ERROR_UNIT` consistently.
- Test scope covers all clarified failure modes and representative migration paths.

**Result**: PASS. No complexity exception required.

## Complexity Tracking

No constitution violations.
