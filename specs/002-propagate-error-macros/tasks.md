---
description: "Task list for propagating generated MAPL error macros"
---

# Tasks: Propagate Error Macros

**Input**: Design documents from `/specs/002-propagate-error-macros/`

**Prerequisites**: `plan.md`, `spec.md`, `research.md`, `data-model.md`, `contracts/migration-contract.md`, `quickstart.md`, and `error-code-inventory.md`

**Tests**: Required by the feature specification. Use pFUnit for typed-context behavior and `ctest -L ESSENTIAL --output-on-failure` at every migration-batch gate. Do not capture `ERROR_UNIT` directly in pFUnit.

**Organization**: Tasks are grouped by user story. User Story 1 and User Story 2 are both P1 and can proceed in parallel after foundational work; User Story 3 supplies review records for proposed groups and merges.

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Establish issue-linked branch/worktree and repository build prerequisites before source migration.

- [X] T001 Verify issue #5328 and parent issue #5324 traceability in `specs/002-propagate-error-macros/plan.md`
- [X] T002 [P] Verify feature worktree contains the external `ESMA_cmake` symlink required by `CMakeLists.txt`
- [X] T003 [P] Record configured GNU, Intel, or NAG compiler/dependency stack and build directory in `specs/002-propagate-error-macros/quickstart.md`
- [X] T004 [P] Inspect existing generated-constant and error-macro build targets in `CMakeLists.txt`, `include/MAPL_ErrLog.h`, and `utils/Constants/CMakeLists.txt`

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Complete inventory, generated-code baseline, and validation workflow before any user-story migration.

**CRITICAL**: No site migration can begin until this phase is complete.

- [ ] T005 Enumerate all MAPL-owned `_ASSERT`, `_FAIL`, and applicable `_VERIFY` references and populate source, macro, condition, current code/message, subsystem, and status fields in `specs/002-propagate-error-macros/error-code-inventory.md`
- [X] T006 [P] Verify generated named-code and context macro signatures against `include/MAPL_ErrLog.h` and `utils/ErrorHandling.F90`
- [X] T007 [P] Verify existing numeric codes and symbolic names against `utils/Constants/mapl_error_codes.yaml` and generated `MAPL_Constants`
- [ ] T008 [P] Define subsystem batch boundaries and stop-on-failure Essential validation records in `specs/002-propagate-error-macros/error-code-inventory.md`
- [X] T009 [P] Add inventory review fields for category, recovery action, return behavior, context fields, canonical group, rejected near-matches, and approval status in `specs/002-propagate-error-macros/data-model.md`
- [X] T010 Build current MAPL targets and establish a passing baseline with `ctest -L ESSENTIAL --output-on-failure` using the configured build directory recorded in `specs/002-propagate-error-macros/quickstart.md`

**Checkpoint**: Complete inventory, verified macro contract, generated-code baseline, and passing Essential test baseline exist.

---

## Phase 3: User Story 1 - Migrate Existing Error Sites (Priority: P1) MVP

**Goal**: Migrate approved MAPL-owned assertion/failure sites to generated code-bearing forms while preserving numeric codes, meanings, hidden `rc`, source location, and early-return behavior.

**Independent Test**: Select one approved subsystem batch, confirm every selected site has a canonical generated code in `error-code-inventory.md`, inspect the diff for unchanged return semantics, and pass `ctest -L ESSENTIAL --output-on-failure` before opening the next batch.

### Tests for User Story 1

- [X] T011 [P] [US1] Add source-inventory validation that every migrated site uses `_ASSERT_CODE`, `_FAIL_CODE`, `_ASSERT_CODE_CTX`, or `_FAIL_CODE_CTX` in `specs/002-propagate-error-macros/error-code-inventory.md`
- [X] T012 [P] [US1] Add batch regression cases for unchanged numeric codes and `rc` behavior in the affected subsystem test files under `tests/`

### Implementation for User Story 1

- [X] T013 [US1] Classify the `base/FileMetadataUtilities.F90` variable-lookup group and assign its canonical code in `specs/002-propagate-error-macros/error-code-inventory.md`
- [X] T014 [US1] Migrate 67 approved base lookup, metadata-type, partition-argument, unsupported-type, and lifecycle sites to generated generic macros
- [X] T015 [US1] Run `ctest -L ESSENTIAL --output-on-failure` after the coordinated base batch and record result in `specs/002-propagate-error-macros/error-code-inventory.md`
- [ ] T016 [US1] Classify and migrate approved `mp_utils/` sites in MAPL-owned `mp_utils/` source files, preserving existing codes and return behavior
- [ ] T017 [US1] Run `ctest -L ESSENTIAL --output-on-failure` after the `mp_utils/` batch and record result in `specs/002-propagate-error-macros/error-code-inventory.md`
- [ ] T018 [US1] Classify and migrate approved `infrastructure/` and `superstructure/` sites in their MAPL-owned source files, preserving `_VERIFY` status propagation into `rc`
- [ ] T019 [US1] Run `ctest -L ESSENTIAL --output-on-failure` after the `infrastructure/` and `superstructure/` batches and record results in `specs/002-propagate-error-macros/error-code-inventory.md`
- [ ] T020 [US1] Classify and migrate approved `gridcomps/` and MAPL-owned `tests/` sites in their source files, leaving unapproved legacy sites explicitly inventoried
- [ ] T021 [US1] Run `ctest -L ESSENTIAL --output-on-failure` after the `gridcomps/` and `tests/` batches and record results in `specs/002-propagate-error-macros/error-code-inventory.md`

**Checkpoint**: Every migrated approved site has generated code-bearing form and preserved behavior; remaining legacy sites have explicit status and rationale.

---

## Phase 4: User Story 2 - Report Useful Typed Context (Priority: P1)

**Goal**: Render string and integer `_CTX` values through catalog-owned prose without caller-authored duplicate diagnostics, while preserving safe fallback behavior.

**Independent Test**: Exercise string and integer context cases, then inspect `ERROR_UNIT` output for code, symbolic name, catalog prose, source location, status, and rendered value; verify unsupported context types remain blocked for human review.

### Tests for User Story 2

- [X] T022 [P] [US2] Add pFUnit string-context coverage for catalog template rendering and source/status metadata in the appropriate `tests/` error-handling test file
- [X] T023 [P] [US2] Add pFUnit integer-context coverage for consistent conversion, range/error fallback, and source/status metadata in the appropriate `tests/` error-handling test file
- [ ] T024 [P] [US2] Add regression coverage proving array, floating-point, and mixed context sites are rejected or remain legacy in `tests/`

### Implementation for User Story 2

- [X] T025 [US2] Implement string context extraction and catalog-template rendering for `_ASSERT_CODE_CTX` and `_FAIL_CODE_CTX` in `utils/ErrorHandling.F90`
- [X] T026 [US2] Implement integer context conversion, supported-range handling, and safe fallback formatting in `utils/ErrorHandling.F90`
- [X] T027 [US2] Update catalog context declarations and approved string/integer templates in `utils/Constants/mapl_error_codes.yaml`
- [ ] T028 [US2] Update macro forwarding and typed context forms while preserving hidden `rc`, file, and line behavior in `include/MAPL_ErrLog.h`
- [ ] T029 [US2] Migrate approved string and integer context call sites without duplicate local prose in MAPL-owned subsystem files recorded in `specs/002-propagate-error-macros/error-code-inventory.md`
- [X] T030 [US2] Build affected targets and run `ctest -L ESSENTIAL --output-on-failure`, recording typed-context evidence in `specs/002-propagate-error-macros/error-code-inventory.md`

**Checkpoint**: String and integer context diagnostics are catalog-owned and human-readable; unsupported context types have no implementation without approval.

---

## Phase 5: User Story 3 - Review Semantic Merges (Priority: P2)

**Goal**: Document and approve canonical error groups only when failure condition, category, recovery/action, return behavior, and required context are equivalent.

**Independent Test**: Review every inventory group, its rejected near-matches, retained context, and recovery behavior; confirm wording similarity alone never causes a merge.

### Tests for User Story 3

- [ ] T031 [P] [US3] Add a merge-review checklist covering condition, category, recovery/action, return behavior, context, source sites, rationale, and rejected near-matches in `specs/002-propagate-error-macros/error-code-inventory.md`
- [ ] T032 [P] [US3] Add review examples for distinct same-wording failures and equivalent semantic failures in `specs/002-propagate-error-macros/contracts/migration-contract.md`

### Implementation for User Story 3

- [ ] T033 [US3] Review each subsystem inventory group and mark canonical-code assignment, merge, rejection, or human-review status in `specs/002-propagate-error-macros/error-code-inventory.md`
- [ ] T034 [US3] Add only approved new codes and catalog templates to `utils/Constants/mapl_error_codes.yaml`, then regenerate `MAPL_Constants` through the repository CMake generation path
- [ ] T035 [US3] Record scientific or operational interpretation changes and validation evidence in `specs/002-propagate-error-macros/error-code-inventory.md`
- [ ] T036 [US3] Verify no source call site defines local numeric error constants and all approved groups resolve to generated constants in `include/MAPL_ErrLog.h` and MAPL-owned source files

**Checkpoint**: All approved merges have traceable rationale and rejected near-matches; unresolved semantic ambiguity remains legacy or explicitly human-review gated.

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Complete coverage, documentation, portability checks, and final validation.

- [ ] T037 [P] Re-scan MAPL-owned source and test files and reconcile 100% macro-site coverage in `specs/002-propagate-error-macros/error-code-inventory.md`
- [ ] T038 [P] Run configured GNU, Intel, and NAG compile/test paths where available and record compiler-specific results in `specs/002-propagate-error-macros/quickstart.md`
- [ ] T039 [P] Update migration workflow, unsupported-context review gate, and expected outcomes in `specs/002-propagate-error-macros/quickstart.md`
- [X] T040 Run final `ctest -L ESSENTIAL --output-on-failure` and record complete batch evidence in `specs/002-propagate-error-macros/error-code-inventory.md`
- [X] T041 Confirm all migrated diagnostics include code, symbolic name, catalog prose, source file/line, status, and typed values through `ERROR_UNIT` as documented in `specs/002-propagate-error-macros/contracts/migration-contract.md`

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No feature dependencies; establish worktree, build stack, and generated-code targets.
- **Foundational (Phase 2)**: Depends on Setup; blocks all story implementation until inventory and baseline validation are complete.
- **User Story 1 (Phase 3)**: Depends on Foundational; migration batches depend on their own classification and approval records.
- **User Story 2 (Phase 4)**: Depends on Foundational macro/API baseline; can proceed in parallel with User Story 1 and must precede context-site migration.
- **User Story 3 (Phase 5)**: Depends on Foundational inventory; review tasks can run alongside P1 work, but final code assignment and merge records gate affected migrations.
- **Polish (Phase 6)**: Depends on all selected story work and every migration batch passing its Essential gate.

### User Story Dependencies

- **US1 (P1)**: Starts after Phase 2. No dependency on completed US2 or US3 for inventory/classification; each site migration requires its own approved group.
- **US2 (P1)**: Starts after Phase 2. Independent of US1 except for shared generated macro/API files; typed-context tests can run separately.
- **US3 (P2)**: Starts after Phase 2. Reviews inventory groups created by foundational work and supplies approval evidence consumed by US1 and new-code work.

### Within Each User Story

- Tests and review checklists precede implementation where practical.
- Catalog/YAML and generated interface changes precede source call-site migration.
- Each subsystem batch requires Essential validation before next batch.
- Legacy sites remain unchanged until classification, assignment, context mapping, and verification complete.

### Parallel Opportunities

- T002-T004 can run in parallel after feature worktree confirmation.
- T006-T009 can run in parallel against separate interfaces/documentation files.
- US1 inventory batch classification and US2 typed-context test/API work can proceed in parallel after Phase 2.
- Within US1, different subsystem batches can be prepared in parallel but source edits and shared inventory merges must be serialized or coordinated.
- US2 string and integer pFUnit tests can run in parallel; catalog and reporting implementation must converge before context-site migration.
- US3 review checklist/examples can run in parallel with US1/US2 implementation.
- T037-T039 can run in parallel; T040-T041 are final validation tasks.

## Parallel Examples

### User Story 1

```text
Developer A: classify base/ sites in specs/002-propagate-error-macros/error-code-inventory.md
Developer B: classify mp_utils/ sites in specs/002-propagate-error-macros/error-code-inventory.md
Developer C: prepare subsystem regression cases in tests/
```

### User Story 2

```text
Developer A: string context pFUnit coverage in tests/
Developer B: integer context pFUnit coverage in tests/
Developer C: typed reporting implementation in utils/ErrorHandling.F90
```

### User Story 3

```text
Developer A: review base/ and mp_utils/ groups in specs/002-propagate-error-macros/error-code-inventory.md
Developer B: review infrastructure/ and superstructure/ groups in specs/002-propagate-error-macros/error-code-inventory.md
Developer C: review gridcomps/ and tests/ groups in specs/002-propagate-error-macros/error-code-inventory.md
```

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1 setup and Phase 2 inventory/baseline.
2. Complete one approved subsystem batch from Phase 3.
3. Run its Essential test gate and verify unchanged codes/return behavior.
4. Stop at the US1 checkpoint for independent review and delivery.

### Incremental Delivery

1. Deliver approved US1 migration batches one at a time with Essential gates.
2. Deliver US2 typed string/integer reporting with pFUnit coverage.
3. Complete US3 semantic merge records and generated-code reconciliation.
4. Finish coverage, portability, documentation, and final Essential validation.

### Parallel Team Strategy

1. Team completes Setup and Foundational phases together.
2. Assign separate developers to US1 subsystem batches, US2 typed reporting/tests, and US3 merge review while coordinating shared inventory changes.
3. Integrate each batch only after its recorded Essential result passes.

## Completion Criteria

- 100% of MAPL-owned macro sites appear in the inventory with migration status.
- Migrated sites use generated code-bearing forms and preserve established codes/return behavior.
- String and integer context diagnostics render catalog-owned prose without duplicate local messages.
- Every approved merge records rationale, retained context, source sites, and rejected near-matches.
- No unsupported context representation is added without human approval.
- Every task uses the required `- [ ] T### [P?] [US?]` checklist format and names concrete repository paths.
