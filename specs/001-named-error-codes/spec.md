# Feature Specification: Named Error Code Catalog

**Feature Branch**: `001-named-error-codes`

**Created**: 2026-08-14

**Status**: Draft

**Input**: User description: "Introduce named errorcodes consistently throughout the
code base. Items involving _ASSERT and _FAIL macros should correspond to specific
error codes. _VERIFY is used for propagating errors up the chain and should have a
unique error code. Maintain correspondence between error codes and human-readable
strings in a YAML source file. Generate a Fortran module from that source during the
CMake build so error handling can report messages without runtime catalog-file
discovery."

## Clarifications

### Session 2026-08-14

- Q: What should happen if the YAML source is missing or invalid during generation? →
  A: Provide hardwired diagnostics for missing source, malformed or partial source,
  and `_VERIFY` propagation so existing stack capture continues to work when
  generation or lookup fails.
- Q: When a migrated error references a code absent from an otherwise valid catalog,
  what should reporting do? → A: Emit the unresolved code with a generic hardwired
  message.
- Q: Should the initial implementation migrate every existing MAPL-owned error-macro
  site? → A: Establish the mechanism and migrate representative sites first, with an
  explicit inventory of remaining sites.
- Q: How should remaining legacy error paths behave until they receive named codes? →
  A: Preserve existing behavior and emit a generic hardwired legacy diagnostic while
  tracking each path in the migration inventory.
- Q: What is the authoritative catalog representation? → A: YAML is the single source
  of truth; CMake generates a Fortran module containing constants and messages.
- Q: How should site-specific diagnostic data be supplied? → A: Use `_CTX` macro
  variants that pass data values while catalog templates provide diagnostic prose.
- Q: Which output stream should diagnostics use? → A: Use Fortran `ERROR_UNIT` for
  serial and MPI diagnostics.
- Q: Where should generated error-code constants be exposed? → A: Existing codes and
  generated codes belong in `MAPL_Constants`, re-exported through top-level `MAPL`.
- Q: How should the existing numeric-code collision be resolved? → A: Preserve
  `MAPL_FILE_NOT_FOUND = 1`; assign `MAPL_UNKNOWN_ERROR` new code 19 and remove
  duplicate source enum declarations.

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Diagnose a Failing Operation (Priority: P1)

As a MAPL user or operator, I want an error report to include a stable named code and
human-readable message so that I can identify and respond to failures consistently.

**Why this priority**: Consistent diagnostics are the primary value and support
debugging across components and releases.

**Independent Test**: Trigger representative assertion, failure, and propagated-error
paths and verify each emits its assigned code and corresponding human-readable text.

**Acceptance Scenarios**:

1. **Given** an assertion or failure path with an assigned error code, **When** the
   path reports an error, **Then** `ERROR_UNIT` contains the stable code and its
   generated catalog message.
2. **Given** a propagated error handled through `_VERIFY`, **When** the error reaches
   its reporting boundary, **Then** `ERROR_UNIT` identifies the propagation error code and
   preserves the originating diagnostic context.

---

### User Story 2 - Maintain Error Definitions (Priority: P1)

As a maintainer, I want one versioned catalog mapping error codes to human-readable
strings so that definitions remain discoverable and consistent across the code base.

**Why this priority**: A single source of correspondence prevents duplicate or
conflicting diagnostics as more components adopt named errors.

**Independent Test**: Add, review, and generate representative catalog entries, then
verify every entry has a unique code and non-empty template.

**Acceptance Scenarios**:

1. **Given** a valid YAML source, **When** CMake generates the catalog module, **Then**
   all defined code-to-template mappings are available to error handling.
2. **Given** duplicate, missing, or malformed YAML data, **When** generation validates
   the source, **Then** the build reports the hardwired invalid-catalog condition and
   does not emit a misleading generated module.

---

### User Story 3 - Adopt Codes Across Error Macros (Priority: P2)

As a developer, I want `_ASSERT` and `_FAIL` sites to use specific codes and `_VERIFY`
sites to use a distinct propagation code so that error categories are consistent
throughout MAPL.

**Why this priority**: Broad adoption makes the catalog useful beyond isolated new
code and establishes predictable macro semantics.

**Independent Test**: Inspect and exercise a representative set of existing macro
sites from each category, confirming code assignment and output behavior.

**Acceptance Scenarios**:

1. **Given** an `_ASSERT` or `_FAIL` site, **When** it is migrated, **Then** it has a
   specific named error code linked to a catalog message.
2. **Given** a `_VERIFY` site, **When** it propagates an error, **Then** it uses the
   designated propagation code rather than being confused with an originating
   assertion or failure code.

### Edge Cases

- When the YAML source is absent or unreadable, generation MUST report a hardwired
  missing-catalog condition and preserve the last valid generated module until the
  build fails safely.
- When the YAML source is malformed, partial, or contains duplicate codes or empty
  templates, generation MUST report a hardwired invalid-catalog condition and MUST NOT
  emit a misleading generated module.
- `_VERIFY` MUST always have a hardwired propagation diagnostic available, including
  when catalog loading fails.
- When an error path references a code missing from a valid catalog, reporting MUST
  identify the unresolved code with a generic hardwired message and MUST NOT substitute
  an unrelated catalog message.
- When a human-readable message requires unavailable formatting data, reporting MUST
  emit the stable code and available message context without failing recursively.
- When YAML validation or Fortran-module generation fails, the build MUST fail before
  compiling consumers and MUST identify the catalog source error.
- When a context value is absent, reporting MUST emit the catalog message and stable
  code without inventing a replacement value.
- Legacy error paths MUST remain reportable during staged migration, using the
  hardwired diagnostics or an explicitly documented compatibility path.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The system MUST define stable, named error codes for supported MAPL
  error categories.
- **FR-002**: Each migrated assertion or failure path MUST use a code-bearing macro
  form that references a specific error code; existing `_ASSERT(A,msg)` and
  `_FAIL(msg)` forms MUST remain valid as legacy forms during staged migration.
- **FR-003**: `_VERIFY` error propagation MUST use a distinct code that identifies
  propagation separately from the originating error.
- **FR-004**: The project MUST maintain one versioned YAML source catalog mapping every
  supported error code to exactly one non-empty human-readable template.
- **FR-005**: The CMake build MUST validate the YAML catalog and generate a Fortran
  module containing named constants, numeric codes, templates, and lookup data before
  compiling catalog consumers.
- **FR-006**: Error reporting MUST emit the assigned code and generated catalog message
  to Fortran `ERROR_UNIT` for supported error paths in serial and MPI execution.
- **FR-007**: Catalog validation MUST detect duplicate codes, missing templates,
  malformed entries, partial data, and references to undefined codes.
- **FR-008**: Catalog lookup failures MUST produce an explicit diagnostic containing
  the unresolved code and a generic hardwired message, and MUST NOT silently report
  an unrelated message.
- **FR-009**: Error handling MUST provide hardwired diagnostics for catalog-generation
  failures and `_VERIFY` propagation so stack capture remains available when generated
  catalog data is unavailable.
- **FR-010**: The migration MUST document code assignment rules and identify any
  existing error paths intentionally outside the initial migration scope, including
  an inventory of remaining MAPL-owned macro sites.
- **FR-011**: Error code and message changes MUST preserve traceability in tests or
  other automated verification so later edits cannot silently break correspondence.
- **FR-012**: Legacy error paths outside the initial migration MUST preserve existing
  behavior, emit a generic hardwired legacy diagnostic, and remain in the migration
  inventory until assigned named codes.
- **FR-013**: The migration MUST classify existing assertion and failure messages by
  semantic error condition before assigning canonical codes; wording similarity alone
  MUST NOT cause a merge.
- **FR-014**: Error paths representing the same semantic condition MUST share one
  canonical error code and catalog message, while distinct conditions MUST retain
  distinct codes even when their wording is similar.
- **FR-015**: Each merge decision MUST record source sites, canonical code, rationale,
  preserved contextual fields, and rejected near-matches for reviewer traceability.
- **FR-016**: The project MUST define `_ASSERT_CODE(condition, code)` and
  `_FAIL_CODE(code)` macro forms; legacy `_ASSERT(condition, message)` and
  `_FAIL(message)` forms MUST remain valid during migration.
- **FR-017**: The project MUST define `_ASSERT_CODE_CTX(condition, code, context)` and
  `_FAIL_CODE_CTX(code, context)` forms that pass site-specific data without requiring
  site-authored diagnostic prose.
- **FR-018**: The baseline catalog MUST include every existing supported MAPL code and
  every hardwired code before representative migration entries are added.
- **FR-019**: Catalog generation MUST reject unsupported catalog versions with the
  hardwired invalid-catalog diagnostic and MUST NOT emit a generated module.
- **FR-020**: Repeated generation from unchanged YAML MUST be deterministic, and
  changed YAML MUST replace generated output only after successful validation.
- **FR-021**: Semantic merge decisions MUST receive maintainer review before canonical
  codes are assigned to source sites.
- **FR-022**: Catalog templates MUST declare required context fields, and reporting
  MUST render supplied context values without failing when optional context is absent.
- **FR-023**: Generated catalog output MUST be reproducible from YAML and MUST NOT
  require runtime access to the YAML source file.
- **FR-024**: The YAML source MUST include all existing MAPL error codes, preserving
  their published numeric values, before new canonical migration codes are added.
- **FR-025**: Generated error-code constants MUST be owned by `MAPL_Constants` and
  available through top-level `MAPL` without manual error-code lists in `utils/API.F90`.
- **FR-026**: Error handling MUST consume error-code constants and generated lookup data
  through the constants layer without redefining numeric codes.
- **FR-027**: The catalog MUST preserve `MAPL_FILE_NOT_FOUND = 1`, assign
  `MAPL_UNKNOWN_ERROR = 19`, and remove duplicate declarations that make constants
  ambiguous through `MAPL_Constants`.

### Key Entities *(include if feature involves data)*

- **Error Code**: Stable named identifier representing an error category or reporting
  condition; includes category, identifier, and lifecycle status.
- **Error Message**: Human-readable text associated with exactly one error code and
  suitable for diagnostic output.
- **Error Catalog**: Versioned YAML source and generated code/template mappings used by
  error handling at runtime.
- **Error Path**: A macro-backed assertion, failure, or propagation site that emits or
  forwards an error code.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: 100% of migrated `_ASSERT` and `_FAIL` paths emit a code present in the
  catalog during automated verification.
- **SC-002**: 100% of migrated `_VERIFY` paths use the designated propagation code and
  preserve originating error context in automated verification.
- **SC-003**: 100% of catalog entries have unique codes, non-empty templates, and pass
  validation before generated consumers compile.
- **SC-004**: In representative diagnostics, users can identify both error category
  and human-readable cause from one `ERROR_UNIT` record without consulting source code.
- **SC-005**: Adding a new catalog entry and corresponding error path requires no
  duplicate message definitions in unrelated components.
- **SC-006**: 100% of merged error groups have documented semantic equivalence and
  retain source-site context needed to distinguish individual failures at runtime.
- **SC-007**: 100% of migrated assertion and failure sites use code-bearing macro forms
  while preserving existing return-code and contextual-message behavior.
- **SC-008**: 100% of representative missing-resource diagnostics include supplied
  site-specific context, such as the missing file name, without duplicated prose.
- **SC-009**: Existing codes `0-9` retain identical numeric values and are available
  through `MAPL_Constants` and top-level `MAPL` after generation.
- **SC-010**: `MAPL_FILE_NOT_FOUND` remains code `1`, `MAPL_UNKNOWN_ERROR` is code `19`,
  and no public `MAPL_Constants` name is ambiguous after generation.

## Assumptions

- The initial migration covers representative MAPL-owned `_ASSERT`, `_FAIL`, and
  `_VERIFY` paths and inventories remaining MAPL-owned sites; external dependencies
  are outside scope unless explicitly adopted later.
- Similar-sounding errors are not assumed equivalent; consolidation requires
  maintainer-reviewed semantic equivalence and preserves useful site-specific context.
- “MAPL-owned” means error-macro sites in MAPL repository source; ESMF, GFE, MPI,
  pFUnit, and other external dependency sources are excluded.
- Error codes remain stable after publication; retiring a code requires an explicit
  deprecation record rather than silent reuse.
- Human-readable templates target operational diagnosis; context values are supplied
  by error sites and are not duplicated as local message strings.
- `ERROR_UNIT` is the required destination for catalog-backed and hardwired diagnostics;
  existing data output on standard output is not redirected.
- Runtime error handling does not discover or parse YAML; generated Fortran data is the
  deployed catalog representation.
- The GitHub issue linked to this specification tracks implementation scope before
  code modification, as required by the MAPL constitution.
- The implementation issue must be linked in the plan before any source task begins;
  this specification does not invent or assume an issue identifier.
