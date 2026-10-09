## Why

`GraphBuilder.F90`'s two resolution-report output parameters
(`unresolved_imports`, `unsupported_characteristics`, both
`type(StringVector)`) are populated by colon-concatenating
`component_name // ':' // short_name` (and, for `unsupported`, one more
`':' // reason` segment) into a single string per entry. This is an ad
hoc string encoding of what is really a 2-3 field record: callers that
need the identity (not just a log-friendly string) must split on `:`,
the field count differs between the two collections and even between
different call sites within `unsupported` (plain `component:short_name`
for mismatched-payload/no-owner cases vs. `component:short_name:reason`
elsewhere), and nothing enforces that `short_name`/`reason` themselves
never contain a literal `:` character. The project has already
established the fix for exactly this shape of problem elsewhere in the
same file: `QualifiedExportEntry` (a real record type, `GraphBuilder.F90`
~line 297) replaced an earlier string-keyed representation for the
qualified-export namespace. This was flagged in review discussion as the
same treatment the two resolution-report collections should eventually
get, but no roadmap entry or change was ever filed to track it, so the
gap was effectively lost. This change files it and lands it.

## What Changes

- Introduce `GraphResolutionEntry`, a small public record type
  (`component_name`, `short_name`, `reason` — all `character(:),
  allocatable`, `reason` set to `''` for entries where no reason beyond
  "no matching export" applies) plus a generated `GraphResolutionEntryVector`/
  `GraphResolutionEntryVectorIterator` pair (gFTL2 vector template
  instantiation, mirroring `mapl_VariableSpecVector_mod`'s own
  `VariableSpec`/`VariableSpecVector.F90` split — a type-definition module
  plus a sibling module that instantiates `vector/template.inc` against
  it), in new files `superstructure/generic/GraphResolutionEntry.F90` and
  `superstructure/generic/GraphResolutionEntryVector.F90`.
- **BREAKING** (internal-only, no external production caller exists today
  — see Impact): change `graphbuilder_check_unsatisfied_imports`'s
  `unresolved_imports` and `graphbuilder_resolve_connections`'s
  `unresolved_imports`/`unsupported_characteristics` optional output
  arguments from `type(StringVector)` to `type(GraphResolutionEntryVector)`.
  Every internal `%push_back(component_name // ':' // short_name [//
  ':' // reason])` call site becomes
  `%push_back(GraphResolutionEntry(component_name, short_name, reason))`.
- Update `graphbuilder_run_activate_hook`/`graphbuilder_run_connect_hook`'s
  own warning-log loops (the only real production consumers) to read the
  three fields off each `GraphResolutionEntry` and format the log message
  from them, rather than logging the opaque concatenated string directly.
- Update `Test_GraphBuilder.pf`'s ~15 assertions that currently compare
  `unresolved%of(i)`/`unsupported%of(i)` against a hand-written
  colon-joined literal (e.g. `'child_dst:Q'`,
  `'child_dst:T:unsupported_item_class:VECTOR'`) to compare the
  corresponding `GraphResolutionEntry` fields instead.
- Add this gap to `docs/graph/spec/20-implementation-roadmap.md` as a new
  discovered-gap phase entry (mirroring the existing Phase 7/Phase 8
  "discovered gap, not previously tracked by any phase above" treatment),
  so the next similar review-discussion item does not get silently lost
  the same way this one did.

### Explicitly out of scope
- Changing the two collections' field **content** (what gets reported) —
  this is a pure representation change; every scenario already covered by
  `openspec/specs/graph/graph-builder/spec.md` ("unresolved import is
  reported", "unsupported characteristic is reported") continues to be
  satisfied with the same reported identities and reasons, just no longer
  string-encoded.
- Adding a real production call site that consumes `unresolved_imports`/
  `unsupported_characteristics` beyond the existing log-and-continue
  hooks — no such caller exists today (see Impact) and none is proposed.
- Reusing `GraphResolutionEntry` for any other existing report shape in
  `GraphBuilder.F90` (e.g. `QualifiedExportEntry` itself, or
  `resolve_inout_destination`'s own rejection path) — out of scope unless
  a future change identifies a genuine duplication to consolidate.

## Capabilities

### New Capabilities
(none)

### Modified Capabilities
(none — pure internal representation change; no requirement or scenario
in `openspec/specs/graph/graph-builder/spec.md` changes, since every
scenario is phrased in terms of "reported," not in terms of the
reporting collection's Fortran type. `.openspec.yaml` sets
`skip_specs: true`.)

## Impact

- **Code**: `superstructure/generic/GraphBuilder.F90` (primary — both
  resolution-report output parameters and every push-back call site,
  plus the two logging hooks), two new files
  `superstructure/generic/GraphResolutionEntry.F90`,
  `superstructure/generic/GraphResolutionEntryVector.F90`,
  `superstructure/generic/CMakeLists.txt` (register the two new sources).
- **Tests**: `superstructure/generic/tests/Test_GraphBuilder.pf` — ~15
  assertion updates (field comparisons instead of string-literal
  comparisons), no new test scenarios (behavior is unchanged).
- **Docs**: `docs/graph/spec/20-implementation-roadmap.md` — new
  discovered-gap phase entry recording this item (closing the gap the
  "Why" section above describes), per this document's own "append,
  don't silently drop a review-discussion follow-up" discipline already
  established for Phase 7/8.
- **No production callers today**: a repo-wide search confirms
  `unresolved_imports`/`unsupported_characteristics` are requested only
  from `Test_GraphBuilder.pf` and from `GraphBuilder.F90`'s own two
  logging hooks (`graphbuilder_run_activate_hook`,
  `graphbuilder_run_connect_hook`) — no `OuterMetaComponent`/
  `initialize_*.F90` call site outside `GraphBuilder.F90` consumes either
  collection, so there is no wider blast radius to audit.
