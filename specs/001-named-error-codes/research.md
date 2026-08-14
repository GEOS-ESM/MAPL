# Research: Named Error Code Catalog

## Decision: Make YAML source of truth and generate Fortran at build time

**Rationale**: Numeric constants and messages cannot safely live in independent
Fortran and YAML authorities. CMake validates one YAML source and generates a Fortran
module containing constants, templates, context metadata, and lookup data. Runtime
error handling then has no file-location or parser dependency.

**Alternatives considered**: Keeping everything in Fortran is simplest at runtime but
reduces maintainability. Runtime YAML parsing avoids generation but creates deployment
and initialization failure modes. Maintaining both independently is rejected because
drift is unavoidable.

## Decision: Use a CMake generator, not runtime ESMF HConfig

**Rationale**: The requested YAML source remains human-maintainable while CMake turns
it into compiler-checked data. Build failure catches duplicate codes, missing fields,
unsupported versions, and invalid templates before consumers compile.

**Alternatives considered**: ESMF HConfig remains appropriate for application YAML,
but runtime catalog parsing is unnecessary once generated Fortran is authoritative at
execution time. yaFyaml would add a separate parser dependency to foundational error
handling.

## Decision: Keep error-handling ownership in `MAPL.utils`

**Rationale**: `utils/ErrorHandling.F90` already owns `MAPL_Assert`, `MAPL_Verify`,
`MAPL_Return`, and message lookup. It consumes generated constants from
`MAPL_Constants`. MPI throw/abort behavior remains in
`mp_utils/MAPL_MpiErrorHandling.F90`.

**Alternatives considered**: Higher-layer ownership would make foundational macros
depend on MPI or generic component libraries.

## Decision: Export codes through `MAPL_Constants`

**Rationale**: Existing numeric codes are constants, not error-handling state. Placing
YAML and generated output under `utils/Constants` lets `MAPL_Constants` re-export all
codes through top-level `MAPL` without maintaining a fragile `utils/API.F90` public
list. `MAPL.utils` consumes the constants layer.

**Alternatives considered**: Keeping exports in `utils/API.F90` requires manual updates
for every new code and is easy to miss because the module defaults to `private`.

## Decision: Preserve file-not-found code and renumber unknown fallback

**Rationale**: Existing source defines `MAPL_FILE_NOT_FOUND = 1`, while prior error
handling also used `1` for `MAPL_UNKNOWN_ERROR`. Preserve the operational file status
ABI and assign unknown fallback code `19`; remove duplicate declarations before
re-exporting through `MAPL_Constants`.

**Alternatives considered**: Merging meanings would make missing files indistinguishable
from unknown failures. Renumbering file-not-found risks existing callers that inspect
its return code.

## Decision: Keep FPP macros as thin return-code wrappers

**Rationale**: FPP cannot provide Fortran generic overloading or infer runtime context.
Macros remain responsible for hidden `rc`, source file, line, and early return. New
forms `_ASSERT_CODE`, `_FAIL_CODE`, `_ASSERT_CODE_CTX`, and `_FAIL_CODE_CTX` call typed
Fortran procedures; legacy forms remain valid during migration.

**Alternatives considered**: Variadic FPP macros are less portable and obscure error
signatures. Direct Fortran calls expose `rc` but cannot preserve current early-return
syntax without repetitive boilerplate.

## Decision: Pass context values, never duplicate diagnostic prose

**Rationale**: Catalog templates own prose and declare fields such as `path`. Call
sites pass values through `_CTX` forms. Reporter adds source location, code, status,
and available context. Missing optional context uses safe fallback text.

**Alternatives considered**: Global pending-context state is unsafe with OpenMP and
MPI. Requiring a complete local message recreates current duplication problem.

## Decision: Emit diagnostics through `ERROR_UNIT`

**Rationale**: Error output must not contaminate scientific data on standard output.
`ERROR_UNIT` preserves existing MAPL behavior and supports normal shell redirection.
Serial and MPI reporters use the same destination.

**Alternatives considered**: `OUTPUT_UNIT` is appropriate for data/results, not
diagnostics.

## Decision: Merge errors by semantic equivalence, not wording similarity

**Rationale**: Existing `_ASSERT` and `_FAIL` messages may differ textually while
describing one condition, or resemble each other while requiring different operator
responses. Consolidation compares condition, category, recoverability, operator action,
and context. Each canonical group records source sites, rationale, retained context,
and rejected near-matches for maintainer review.

**Alternatives considered**: Exact string deduplication misses paraphrases. Automatic
lexical or embedding-based merging risks collapsing distinct scientific or operational
failures and is advisory at most.

## Decision: Test generator, fallback, macro, context, and integration behavior

**Rationale**: pFUnit tests cover generated catalog validation, hardwired fallback,
code-bearing macros, and context formatting. Essential integration tests validate
`ERROR_UNIT` routing without brittle direct stream capture. Broader application tests
may require external regression data.

**Alternatives considered**: Broad application tests alone cannot isolate catalog
generation or fallback failures.
