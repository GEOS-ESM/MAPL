## Context

See proposal.md - Why. Two `GraphBuilder.F90` procedures,
`graphbuilder_check_unsatisfied_imports` and
`graphbuilder_resolve_connections`, each take optional
`type(StringVector), intent(out)` arguments (`unresolved_imports`, and
`unresolved_imports`/`unsupported_characteristics` respectively) that are
populated, at every one of their ~10 `push_back` call sites inside the
same file, by concatenating `component_name // ':' // short_name` (plus,
for `unsupported`, one more `':' // reason` segment) into a single
string. The file already has an established, working precedent for
replacing exactly this shape of ad hoc string key with a real record
type: `QualifiedExportEntry` (`component_name`-equivalent `comp_path` +
`qualified_name` + the full `VariableSpec` payload), grown into a plain
`allocatable` array by `build_qualified_export_namespace`/
`collect_qualified_exports`. That hand-rolled growth pattern exists
specifically to work around real compiler defects (documented inline,
`GraphBuilder.F90` ~lines 1779-1799) triggered by
`QualifiedExportEntry`'s *nested, polymorphic* `VariableSpec` component
(classic ifort ICE on self-referencing array-constructor growth; NAG bus
error on zero-extent array-section defined-assignment) - neither applies
to the flat, three-`character`-field record this change introduces.

`StringVector`/`push_back`/`size`/`of` is the call shape every existing
caller (both `GraphBuilder.F90`'s own two logging hooks and
`Test_GraphBuilder.pf`) already uses for these two collections today.

## Goals / Non-Goals

**Goals:**
- Replace the colon-encoded `character` payload with a real record
  (`GraphResolutionEntry`) carrying `component_name`, `short_name`, and
  `reason` as distinct fields, for both `unresolved_imports` and
  `unsupported_characteristics`.
- Keep the call-site shape (`push_back`, `size`, `of`/`at`) identical to
  today's `StringVector` usage, so the diff at each of the ~10 production
  push-back sites and the ~15 test assertion sites is a type/constructor
  change only, not a restructuring of control flow or iteration style.
- Record the gap's existence in `docs/graph/spec/20-implementation-roadmap.md`
  so a future review-discussion item of this shape does not get silently
  dropped again (proposal.md "What Changes").

**Non-Goals:**
- Changing what is reported (field content/semantics) - see proposal.md
  "Explicitly out of scope".
- Giving `GraphResolutionEntry` or its vector a wider role than these two
  existing collections (e.g. generalizing `QualifiedExportEntry` itself
  onto the same type, or using it for `resolve_inout_destination`'s own
  rejection path) - no such consolidation is attempted here.
- Adding a new real production consumer of either collection beyond the
  existing log-and-continue hooks.

## Decisions

**1. One shared record type for both collections, not two.**
`GraphResolutionEntry` has three fields: `component_name`, `short_name`
(both always meaningful - every call site already supplies both), and
`reason` (`character(:), allocatable`, always allocated, set to `''`
when no reason beyond "no matching export" applies - i.e. every
`unresolved_imports` entry, by construction). *Alternative considered:*
two distinct types, e.g. `UnresolvedImportEntry` (no `reason` field at
all) and `UnsupportedCharacteristicEntry` (`reason` required, non-empty).
Rejected: every one of the 10 existing call sites already shares the
identical `component_name`/`short_name` identity shape regardless of
which collection it feeds; a second near-identical type/vector pair
would duplicate the type, the vector-template instantiation, and every
field-level test assertion helper for no behavioral gain, and would
block any future caller that wants to treat "resolution problems" (both
kinds) uniformly (e.g. a single combined diagnostics report).

**2. Storage is a generated gFTL2 vector (`GraphResolutionEntryVector`),
not a hand-rolled `allocatable` array.**
Mirrors `mapl_VariableSpecVector_mod`'s own split
(`VariableSpec.F90` + `VariableSpecVector.F90`, `#include
"vector/template.inc"`), itself already a production-proven example of a
gFTL2 vector instantiated over a record type with its own nested
allocatable components, growing via ordinary `push_back` with no special
workaround needed. *Alternative considered:* follow
`QualifiedExportEntry`'s own hand-rolled growth pattern (plain
`allocatable` array, explicit `grown_entries`/`move_alloc` on every
append). Rejected: that pattern exists specifically to route around the
ifort-ICE/NAG-bus-error pair triggered by a *nested, polymorphic*
`VariableSpec` component (Context, above) - `GraphResolutionEntry` has no
such component (three flat `character(:), allocatable` fields only), so
there is no comparable defect to avoid, and a gFTL2 vector is a strictly
smaller diff at every existing `push_back`/`size`/`of` call site (both
collections are already `StringVector`, itself a gFTL2 vector - swapping
the element type changes nothing about the surrounding call shape).

**3. `GraphResolutionEntry`/`GraphResolutionEntryVector` get their own
two files, not an inline definition inside `GraphBuilder.F90` (unlike
`QualifiedExportEntry`).**
New files: `superstructure/generic/GraphResolutionEntry.F90` (the record
type, `public`) and `superstructure/generic/GraphResolutionEntryVector.F90`
(the `#include "vector/template.inc"` instantiation against it,
`public :: GraphResolutionEntry, GraphResolutionEntryVector,
GraphResolutionEntryVectorIterator` re-exported), both `use`d from
`GraphBuilder.F90` exactly as `mapl_VariableSpecVector_mod` already is.
*Alternative considered:* define `GraphResolutionEntry` inline inside
`GraphBuilder.F90`'s own module body, the way `QualifiedExportEntry`
already is. Rejected: `vector/template.inc`'s gFTL2 instantiation must be
`#include`d at the top level of the module that hosts it (the
`VariableSpec`/`VariableSpecVector` precedent always puts type and vector
in separate files for exactly this reason) - hosting both the record
type and the template instantiation inside `GraphBuilder.F90`'s own
already-large module body would require restructuring that module's
existing `use`/`contains` layout for a type with no other reason to live
there; two small, focused files following the established
`specs/VariableSpec*.F90` split keep the change mechanically simple and
match this subtree's own existing file-organization convention.

**4. Logging hooks are updated to format from fields, not to carry the
old message text verbatim by re-joining them.**
`graphbuilder_run_activate_hook`/`graphbuilder_run_connect_hook`'s
existing `lgr%warning(...)` calls currently interpolate the single
pre-joined string (`%a`, one placeholder). They become two/three
placeholders (`entry%component_name`, `entry%short_name`, and -
`unsupported` only - `entry%reason`), composed with the same `:`
separator the old string used, so the emitted log line stays
byte-identical to today's output. This is a formatting-site decision,
not a reporting-content change (Goals, above) - the log text a real
operator would see does not change.

## Risks / Trade-offs

- **[Risk]** Changing a public optional-argument type
  (`type(StringVector)` -> `type(GraphResolutionEntryVector)`) is
  binary/source breaking for any caller outside this file. **Mitigation**:
  proposal.md "Impact" documents a repo-wide search confirming the only
  callers are `GraphBuilder.F90`'s own two logging hooks and
  `Test_GraphBuilder.pf` - no `OuterMetaComponent`/`initialize_*.F90` call
  site, and no other module, requests either collection today.
- **[Risk]** Two new source files (plus one more gFTL2 vector template
  expansion) for what is, by entry count, a small type. **Mitigation**:
  this is the same per-type cost already paid for every other gFTL2
  vector in this subtree (`StringVector`, `VariableSpecVector`,
  `ConnectionVector`, ...) - no new build-system capability or pattern is
  introduced, only one more instance of an existing one.
- **[Risk]** The ~15 updated test assertions become more verbose (3
  field-level comparisons instead of 1 string-equality comparison) at
  each of the sites listed in proposal.md "What Changes". **Mitigation**:
  accepted trade-off - the verbosity makes explicit exactly what each
  assertion is actually checking (identity vs. reason, previously
  conflated inside one opaque string), which is the same legibility
  improvement motivating the type change itself.

## Migration Plan

Pure internal refactor, single change, no runtime migration or rollback
concerns (Risks, above - no external caller to coordinate with). Land in
one pass: add the two new files and register them in
`superstructure/generic/CMakeLists.txt`; swap `GraphBuilder.F90`'s two
output-parameter types and every `push_back` call site; update the two
logging hooks; update `Test_GraphBuilder.pf`'s assertions; confirm
`Test_GraphBuilder.pf` and the full `superstructure/generic` test suite
still pass. See tasks.md for the ordered breakdown.
