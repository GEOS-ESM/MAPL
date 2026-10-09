## 1. New record type + vector modules

- [x] 1.1 Create `superstructure/generic/GraphResolutionEntry.F90`
      (module `mapl_GraphResolutionEntry_mod`) defining `public, type ::
      GraphResolutionEntry` with `character(:), allocatable ::
      component_name`, `short_name`, `reason` (design.md Decision 1).
      Add a convenience constructor/structure-constructor usage so
      callers can write
      `GraphResolutionEntry(component_name, short_name, reason)` with
      `reason` defaulting to `''` when omitted.
- [x] 1.2 Create `superstructure/generic/GraphResolutionEntryVector.F90`
      (module `mapl_GraphResolutionEntryVector_mod`), instantiating
      `vector/template.inc` against `GraphResolutionEntry`
      (`#define T GraphResolutionEntry`, `#define Vector
      GraphResolutionEntryVector`, `#define VectorIterator
      GraphResolutionEntryVectorIterator`), mirroring
      `superstructure/generic/specs/VariableSpecVector.F90` exactly
      (design.md Decision 2/3).
- [x] 1.3 Register both new files in
      `superstructure/generic/CMakeLists.txt` (alongside the existing
      `GraphBuilder.F90` entry).
- [x] 1.4 Build just these two new modules (or the smallest target that
      includes them) to confirm the gFTL2 template instantiation
      compiles cleanly before touching `GraphBuilder.F90`.

## 2. Rewire `GraphBuilder.F90`'s resolution-report plumbing

- [x] 2.1 Add `use mapl_GraphResolutionEntry_mod` / `use
      mapl_GraphResolutionEntryVector_mod` to `GraphBuilder.F90`
      (alongside the existing `gFTL2_StringVector` use).
- [x] 2.2 Change `graphbuilder_check_unsatisfied_imports`'s
      `unresolved_imports` argument and local `unresolved` variable from
      `type(StringVector)` to `type(GraphResolutionEntryVector)`.
- [x] 2.3 Change `check_match_connection_unsatisfied`'s `unresolved`
      dummy argument the same way; update both its `push_back` call
      sites (`component_name // ':' // short_name`) to
      `push_back(GraphResolutionEntry(dst_pt%component_name,
      var_spec%short_name))` (reason defaults to `''`).
- [x] 2.4 Change `graphbuilder_resolve_connections`'s `unresolved_imports`/
      `unsupported_characteristics` arguments and local `unresolved`/
      `unsupported` variables from `type(StringVector)` to
      `type(GraphResolutionEntryVector)`.
- [x] 2.5 Change `resolve_match_connection`'s `unresolved`/`unsupported`
      dummy arguments the same way; update every `push_back` call site
      inside it and its internal `resolve_one` (callback+inout mutual-
      exclusion rejection, callback-rejected loop, plain unresolved-import
      case) to construct a `GraphResolutionEntry` with the reason text as
      the third field instead of a third colon-joined string segment.
- [x] 2.6 Update `resolve_inout_destination`'s `unsupported` dummy
      argument and its three `push_back` call sites the same way.
- [x] 2.7 Grep `GraphBuilder.F90` for any remaining
      `type(StringVector)` declarations tied to resolution reporting (not
      `QualifiedExportEntry`'s own unrelated `callback_rejected`
      `StringVector`, which stays as-is - out of scope, proposal.md) to
      confirm none were missed.

## 3. Update the two logging hooks

- [x] 3.1 `graphbuilder_run_activate_hook`: change the `unresolved`
      local from `type(StringVector)` to
      `type(GraphResolutionEntryVector)`; update the loop to read
      `entry => unresolved%of(i)` and format the warning from
      `entry%component_name`/`entry%short_name` (design.md Decision 4 -
      byte-identical log text to today's output).
- [x] 3.2 `graphbuilder_run_connect_hook`: same change for its
      `unsupported`/`unresolved` locals, including the
      `unsupported`-loop warning (now also interpolating `entry%reason`)
      and the `assert_converged` call (its `unresolved` dummy argument
      type changes too - only `%size()` is used there, no field access
      needed).

## 4. Update tests

- [x] 4.1 In `superstructure/generic/tests/Test_GraphBuilder.pf`, change
      every `type(StringVector) :: unresolved` / `unsupported` local
      declaration to `type(GraphResolutionEntryVector)`, and add the
      corresponding `use mapl_GraphResolutionEntryVector_mod` (and
      `mapl_GraphResolutionEntry_mod` if field access needs it directly).
- [x] 4.2 Replace each of the ~15 string-literal assertions (e.g.
      `@assert_that(unresolved%at(1, status), is(equal_to('child_dst:Q')))`,
      `@assert_that(unsupported%at(1, status), is(equal_to(
      'child_dst:T:unsupported_item_class:VECTOR')))`) with field-level
      assertions against the returned `GraphResolutionEntry`
      (`entry%component_name`, `entry%short_name`, `entry%reason`),
      preserving each test's original intent (same component/short_name/
      reason values, just compared per-field instead of as one joined
      string).
- [x] 4.3 Run `superstructure/generic/tests/Test_GraphBuilder.pf` (and
      the full `superstructure/generic` pFUnit suite) and confirm all
      pass with no behavior change.

## 5. Roadmap documentation

- [x] 5.1 Add a new discovered-gap phase entry to
      `docs/graph/spec/20-implementation-roadmap.md` (mirroring the
      existing Phase 7/Phase 8 "discovered gap, not previously tracked by
      any phase above" treatment, §20.4.5/§20.4.6) recording: the
      `GraphResolutionEntry` gap was surfaced in review discussion before
      this change, no roadmap entry was filed for it at the time, and
      this change closes it. Add the corresponding one-line bullet to the
      Phase 1-8 summary list (roadmap ~lines 120-164) alongside the
      existing Phase 7/8 bullets.

## 6. Final verification

- [x] 6.1 Full build of `superstructure/generic` (and any dependent
      target) with the compiler(s) normally used for this subtree.
- [x] 6.2 Confirm `openspec validate graphbuilder-resolution-entry`
      passes (`skip_specs: true`, zero spec deltas expected).
