---

description: "Task list for named error code catalog"
---

# Tasks: Named Error Code Catalog

**Input**: Design documents from `/specs/001-named-error-codes/`

**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/,
quickstart.md, error-code-inventory.md

**Tests**: Included because specification requires generator validation, fallback,
macro/context behavior, and Essential CTest verification.

**Organization**: Tasks grouped by user story for independent implementation and
validation.

**Implementation Gate**: Governing issue is
[#5324](https://github.com/GEOS-ESM/MAPL/issues/5324). Do not begin source tasks until
work is tracked against this issue.

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Freeze contracts and source-of-truth inputs before generator work.

- [X] T001 [P] Reconcile YAML field names, template placeholders, context fields, and reserved codes in `specs/001-named-error-codes/contracts/error-catalog.yaml`
- [X] T002 [P] Reconcile generated-module, macro, context, `ERROR_UNIT`, and return-code behavior in `specs/001-named-error-codes/contracts/error-handling.md`
- [X] T003 [P] Update catalog lifecycle, generated-module entities, and context validation rules in `specs/001-named-error-codes/data-model.md`
- [X] T004 [P] Freeze representative source-site scope and semantic merge review fields in `specs/001-named-error-codes/error-code-inventory.md`

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Build generated catalog and shared reporting/macro foundations.

**CRITICAL**: Complete Phase 2 before any user story phase.

- [X] T005 Implement YAML schema validation, code/name uniqueness checks, template-field validation, and deterministic Fortran emission in `cmake/generate_error_codes.py`
- [X] T006 Add CMake custom command, dependency, and generated-source target for `cmake/generate_error_codes.py` in `utils/Constants/CMakeLists.txt`
- [X] T007 Add generated module include/output paths, `MAPL_Constants` re-export, and `MAPL.constants` linkage in `utils/Constants/CMakeLists.txt` and `utils/Constants/Constants.F90`
- [X] T008 Refactor hardwired diagnostics and lookup to consume `MAPL_Constants` generated data without runtime YAML state in `utils/ErrorHandling.F90`
- [X] T009 Define `_ASSERT_CODE`, `_FAIL_CODE`, `_ASSERT_CODE_CTX`, and `_FAIL_CODE_CTX` while preserving legacy forms and hidden `rc` behavior in `include/MAPL_ErrLog.h`
- [X] T010 Route serial and MPI diagnostics to `ERROR_UNIT` while preserving source, line, status, and stack context in `utils/MAPL_Throw.F90` and `mp_utils/MAPL_MpiErrorHandling.F90`
- [X] T011 Register generator fixtures and error-handling pFUnit sources in `utils/tests/CMakeLists.txt`

**Checkpoint**: YAML generation, generated lookup, hardwired fallback, macro forms,
context plumbing, and `ERROR_UNIT` reporting are available.

---

## Phase 3: User Story 1 - Diagnose a Failing Operation (Priority: P1) MVP

**Goal**: Users receive stable code, generated message, source context, and optional
site-specific data on `ERROR_UNIT` without losing return/status behavior.

**Independent Test**: Generate catalog, trigger code-bearing assertion/failure and
`_VERIFY` paths with and without context, and inspect captured `ERROR_UNIT` records.

### Tests for User Story 1

- [X] T012 [US1] Add pFUnit assertions for generated code/name/template output from `_ASSERT_CODE` and `_FAIL_CODE` in `utils/tests/test_ErrorHandling.pf`
- [X] T013 [US1] Add pFUnit assertions for `_ASSERT_CODE_CTX` and `_FAIL_CODE_CTX` rendering of file-path context in `utils/tests/test_ErrorHandling.pf`
- [X] T014 [US1] Validate `_VERIFY` propagation code, preserved original status, and `ERROR_UNIT` routing through Essential integration tests; avoid direct I/O capture in `utils/tests/test_ErrorHandling.pf`

### Implementation for User Story 1

- [X] T015 [US1] Implement generated template lookup, required/optional context formatting, and unknown-code fallback in `utils/ErrorHandling.F90`
- [X] T016 [US1] Implement `_ASSERT_CODE_CTX` and `_FAIL_CODE_CTX` calls that preserve hidden `rc`, file, line, and early-return behavior in `include/MAPL_ErrLog.h`
- [X] T017 [US1] Preserve legacy `_ASSERT`/`_FAIL` output and add code/context fields to new reporting paths in `utils/ErrorHandling.F90`

**Checkpoint**: User Story 1 independently reports named diagnostics with optional
context and unchanged return/status semantics.

---

## Phase 4: User Story 2 - Maintain Error Definitions (Priority: P1)

**Goal**: Maintainers edit one YAML source and receive reproducible, validated
Fortran catalog output.

**Independent Test**: Generate valid YAML and reject malformed, duplicate, incomplete,
unsupported-version, and missing-context-field inputs before consumer compilation.

### Tests for User Story 2

- [X] T018 [US2] Add generator fixtures for valid, malformed, partial, duplicate-code, empty-template, unsupported-version, and invalid-field YAML in `utils/tests/mapl_error_codes_*.yaml`
- [X] T019 [US2] Add generator tests for deterministic output and changed-source regeneration in `utils/tests/test_error_code_generator.py`

### Implementation for User Story 2

- [X] T020 [US2] Add all existing codes 0-9, hardwired codes, and representative context-template entries to YAML source in `utils/Constants/mapl_error_codes.yaml`
- [X] T021 [US2] Emit public named constants, numeric codes, templates, required fields, and hardwired fallbacks in `cmake/generate_error_codes.py`
- [X] T022 [US2] Fail CMake generation before compiling consumers when schema, version, uniqueness, or template validation fails in `utils/CMakeLists.txt`
- [X] T023 [US2] Document YAML source ownership, generated-module lifecycle, generator prerequisites, and no-runtime-YAML behavior in `specs/001-named-error-codes/quickstart.md`

**Checkpoint**: User Story 2 independently validates and regenerates catalog data
without maintaining duplicate Fortran constants or runtime YAML paths.

---

## Phase 5: User Story 3 - Adopt Codes Across Error Macros (Priority: P2)

**Goal**: Representative MAPL macro sites receive reviewed canonical codes and pass
site-specific values without duplicating diagnostic prose.

**Independent Test**: Review inventory groups, migrate representative sites, and confirm
equivalent conditions share codes while distinct conditions retain context and codes.

### Tests for User Story 3

- [X] T024 [US3] Add inventory review fixtures for canonical groups, rejected near-matches, and required context fields in `specs/001-named-error-codes/error-code-inventory.md`
- [X] T025 [US3] Add pFUnit coverage for representative migrated `_ASSERT_CODE`, `_FAIL_CODE`, `_CTX`, and legacy compatibility paths in `utils/tests/test_ErrorHandling.pf`

### Implementation for User Story 3

- [X] T026 [US3] Scan all MAPL-owned `_ASSERT`, `_FAIL`, and `_VERIFY` sites and record conditions, candidate groups, migration status, and context fields in `specs/001-named-error-codes/error-code-inventory.md`
- [X] T027 [P] [US3] Migrate bundle lookup sites to code-bearing/context forms in `base/SimpleBundleMod.F90`
- [X] T028 [P] [US3] Migrate partition validation sites to code-bearing/context forms in `mp_utils/Partition.F90`
- [X] T029 [P] [US3] Migrate DSO validation sites to code-bearing/context forms in `superstructure/generic/UserSetServices.F90`
- [X] T030 [P] [US3] Migrate vertical-grid validation sites to code-bearing/context forms in `superstructure/generic/vertical/FixedLevelsVerticalGrid.F90`
- [X] T031 [P] [US3] Migrate private-state assertion sites to code-bearing/context forms in `include/MAPL_private_state.h`
- [X] T032 [US3] Obtain maintainer review, assign canonical codes, and document merge rationale/rejected near-matches in `utils/Constants/mapl_error_codes.yaml` and `specs/001-named-error-codes/error-code-inventory.md`
- [X] T033 [US3] Document legacy, code-bearing, and `_CTX` macro guidance in `include/README.md`

**Checkpoint**: User Story 3 independently demonstrates reviewed semantic consolidation
without changing distinct failure meaning or losing site context.

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Reconcile documentation, run required validation, and close review gates.

- [X] T034 [P] Reconcile spec, plan, research, data model, contracts, inventory, quickstart, and YAML references in `specs/001-named-error-codes/`
- [X] T035 [P] Document generated-module, `ERROR_UNIT`, and `_CTX` behavior in `utils/ErrorHandling.F90` and `include/README.md`
- [X] T036 Run `ctest --test-dir build-dir -L ESSENTIAL --output-on-failure` with configured compiler stack and record results in `specs/001-named-error-codes/quickstart.md`
- [X] T037 Review portability, contract compatibility, scientific diagnostic behavior, maintainer merge approvals, and remaining inventory before pull request in `specs/001-named-error-codes/error-code-inventory.md`

---

## Dependencies & Execution Order

### Phase Dependencies

- **Phase 1 Setup**: No dependencies; T001-T004 can run in parallel.
- **Phase 2 Foundational**: Depends on Phase 1; blocks all user stories.
- **Phase 3 User Story 1**: Depends on Phase 2; MVP diagnostic behavior.
- **Phase 4 User Story 2**: Depends on Phase 2 and supports US1 generated data.
- **Phase 5 User Story 3**: Depends on US1 and US2; migration requires stable macros,
  generated catalog, and context contract.
- **Phase 6 Polish**: Depends on all selected story phases.

### User Story Dependencies

- **US1 (P1)**: Depends only on Foundational; MVP.
- **US2 (P1)**: Depends on Foundational; independently validates source generation.
- **US3 (P2)**: Depends on US1 and US2; migration uses generated codes and context rules.

### Parallel Opportunities

- T001-T004 can run in parallel because they touch separate design artifacts.
- T027-T031 can run in parallel after T026 and maintainer semantic grouping review.
- T034-T035 can run in parallel after implementation.

## Parallel Example: User Story 3

```text
Task: "Migrate bundle lookup sites in base/SimpleBundleMod.F90"
Task: "Migrate partition validation sites in mp_utils/Partition.F90"
Task: "Migrate DSO validation sites in superstructure/generic/UserSetServices.F90"
Task: "Migrate vertical-grid validation sites in superstructure/generic/vertical/FixedLevelsVerticalGrid.F90"
Task: "Migrate private-state assertion sites in include/MAPL_private_state.h"
```

Run only after inventory grouping and canonical-code review complete.

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1 setup.
2. Complete Phase 2 generator, macro, fallback, context, and `ERROR_UNIT` foundations.
3. Complete Phase 3 User Story 1.
4. Stop and validate generated named diagnostics, context values, fallback behavior,
   and `_VERIFY` status preservation with `ctest -L ESSENTIAL`.

### Incremental Delivery

1. Add User Story 2 generator validation and reproducible output.
2. Build and review complete macro inventory and semantic merge groups.
3. Add User Story 3 representative migrations.
4. Complete cross-cutting documentation and validation gates.

## Notes

- `[P]` means different files and no dependency on incomplete work.
- Every task has a sequential ID, required story label where applicable, and exact file path.
- Tests are requirements-driven and use Essential CTest, not an alias that may skip feature tests.
- Commit after each logical task group; link implementation changes to issue #5324.
