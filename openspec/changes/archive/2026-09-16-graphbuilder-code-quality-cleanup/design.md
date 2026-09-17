## Context

See proposal.md for motivation. Relevant current state:

- `GraphBuilder.F90` (725 lines) is the only file under
  `superstructure/generic/` (checked all of `graph/`, `graph/containers/`,
  and `GraphBuilder.F90` itself) using raw gFTL `%begin()/%end()`
  iteration; every other module (`OuterMetaComponent/recurse.F90`,
  `OuterMetaComponent/SetServices.F90`, `specs/StateItemSpec.F90`,
  `specs/*ClassAspect.F90`, `GriddedComponentDriver/run_*_couplers.F90`)
  uses `%ftn_begin()/%ftn_end()` with `call iter%next()` as the *first*
  statement in the loop body.
- `resolve_match_connection` and `check_match_connection_unsatisfied` are
  near-duplicates of each other (the module's own header comment says so:
  "same destination-import filtering and same-name export existence
  check") and share the same 4-level `do / if / if / if-else` shape.
- `get_child_meta` (GraphBuilder.F90:523-538) reaches a named child's
  `OuterMetaComponent` via `this%get_child(name) -> %get_gridcomp() ->
  get_outer_meta(child_gc)`, duplicating exactly the reach pattern
  `Test_ComponentHierarchyGraph.pf` uses for test setup and that
  `propagate_geom_to_children.F90`/`apply_to_children_custom.F90` use for
  the framework's own child-mutation needs — but GraphBuilder currently
  owns this 3-hop chain itself rather than getting it from
  `OuterMetaComponent`.
- `mapl_GraphBuilder_mod` currently has 10 public entities: 7 procedures
  operating on an explicit `OuterMetaComponent` argument (no GraphBuilder
  state), plus `item_key`/`proxy_key` (unaffected by this change).
- Fortran has no "package-private"/"friend module" visibility modifier —
  module-level `private`/`public` is binary and applies to every
  consumer, submodules aside. This matters for the child-accessor
  visibility decision below.

## Goals / Non-Goals

**Goals:**
- Bring `GraphBuilder.F90`'s gFTL loop idiom in line with the rest of
  `superstructure/generic/`.
- Reduce the nesting depth of the two ordinary-connection-resolution
  procedures without changing their externally observable results.
- Remove `GraphBuilder.F90`'s direct dependency on
  `GriddedComponentDriver`/`get_gridcomp`/`get_outer_meta` by moving the
  child-reach convenience onto `OuterMetaComponent`.
- Shrink `mapl_GraphBuilder_mod`'s public procedure surface to a single
  type.

**Non-Goals:**
- No change to any requirement/scenario in
  `openspec/specs/graph/graph-builder/spec.md` — see proposal.md.
- No change to `item_key`/`proxy_key` or the string-keyed
  `ComponentGraph%resource_index` API they feed (explicitly skipped per
  discussion).
- No change to how `graphbuilder_run_*_hook`/`report_if_failed` handle
  (or don't trap) errors (deferred per discussion).
- No compiler-enforced access control for the new child accessors (see
  Decision 3) — Fortran doesn't offer that; this is a documented
  convention, same class of trust the codebase already extends to
  `get_child_meta`'s existing reach (REQ-GB-002).

## Decisions

### 1. Loop idiom: mechanical `ftn_begin/ftn_end` + top-of-loop `next()`
Straight conversion of the 5 affected loops (see proposal.md) to the
pattern already used everywhere else, e.g.:

```fortran
! before
associate (e => comp_spec%var_specs%end())
   iter = comp_spec%var_specs%begin()
   do while (iter /= e)
      var_spec => iter%of()
      call advertise_one(graph, var_spec, _RC)
      call iter%next()
   end do
end associate

! after
associate (e => comp_spec%var_specs%ftn_end())
   iter = comp_spec%var_specs%ftn_begin()
   do while (iter /= e)
      call iter%next()
      var_spec => iter%of()
      call advertise_one(graph, var_spec, _RC)
   end do
end associate
```
No alternative considered — this is a straight convention match, not a
design choice.

### 2. Flatten nesting via guard clauses + shared match-filter helper
`resolve_match_connection` and `check_match_connection_unsatisfied` both
iterate `dst_spec%var_specs`, keep only `IMPORT`s whose short name matches
`dst_pt%v_pt`, and then diverge (one adds a dependency edge or records
unresolved; the other only checks export existence and records
unresolved). Plan:
- Extract a private helper, e.g. `for_each_matching_import(dst_spec,
  dst_pt, callback)` (or an iterator-returning function), that owns the
  do-loop + two guard clauses (`if (state_intent /= IMPORT) cycle`,
  `if (.not. v_pt%matches(...)) cycle`) and leaves each caller with a
  flat body operating on one `var_spec` at a time.
- Both call sites then reduce to 1-2 levels of nesting (the
  has-export/else split, or none at all for the unsatisfied-check case).

**Alternatives considered:**
- *Guard clauses only, no extraction*: flattens each procedure
  individually (do/cycle/cycle/if-else, ~2 levels) but leaves the
  duplication between the two procedures untouched. Simpler, smaller
  diff; loses the DRY benefit.
- *Full extraction into a shared function returning a list of matches*:
  more upfront restructuring, more testable in isolation, but a bigger
  diff for a first cleanup pass.

**Decision**: do both in one pass — guard clauses to flatten each
procedure's control flow, *and* extract the shared match-filter helper
so the duplication between the two procedures is removed at the same
time, rather than deferring the DRY cleanup to a later change.

### 3. Child accessors move to `OuterMetaComponent`, visibility is convention-only
Add `get_child_component_spec(child_name, rc)` and
`get_child_component_graph(child_name, rc)` to `OuterMetaComponent`
(implemented as submodule procedures under `OuterMetaComponent/`,
following the existing one-file-per-procedure pattern e.g.
`get_component_graph.F90`), each internally doing the same
`get_child()->get_gridcomp()->get_outer_meta()->get_component_spec()/
get_component_graph()` chain `get_child_meta` does today.
`GraphBuilder.F90` calls these instead of owning the chain itself, and
its `use mapl_GriddedComponentDriver_mod` import goes away along with
`get_child_meta`.

**Visibility ("only accessible within `mapl_generic`")**: Fortran has no
friend-module or package-private mechanism — `public`/`private` at
module scope applies uniformly to every consumer, and `GraphBuilder.F90`
is a sibling module of `mapl_OuterMetaComponent_mod`, not one of its
submodules, so it cannot call a genuinely `private` type-bound
procedure. The two new accessors must therefore be Fortran-`public` on
`OuterMetaComponent` for `GraphBuilder.F90` to reach them at all. "Only
within `mapl_generic`" is enforced the same way `get_child_meta`'s
existing reach already is: by convention/documentation (a comment on
each accessor stating it's for the same REQ-GB-002-style
framework-internal carve-out, not general API) plus **not re-exporting
them from any user-facing aggregator module** (`MAPL_Generic.F90` at the
`superstructure/generic/` level already imports `OuterMetaComponent`
directly rather than through a curated re-export list, so no action
needed there beyond not adding these two names to any such list in the
future).

**Alternative considered**: make `GraphBuilder.F90`'s relevant
procedures a `submodule (mapl_OuterMetaComponent_mod)` instead of an
independent module, which would let the new accessors be genuinely
`private` and compiler-enforced. Rejected for this change: it would
mean folding `GraphBuilder`'s ~700 lines and its own dependency set
(`ComponentSpec`, `Connection`, `MatchConnection`, ...) into
`OuterMetaComponent`'s submodule tree, a much larger structural change
than this cleanup is scoped for, and it would undo the module boundary
the original design deliberately drew (GraphBuilder as an "integration
layer," not part of `OuterMetaComponent` itself).

### 4. `GraphBuilder` as an empty type with type-bound procedures
```fortran
type :: GraphBuilder
end type GraphBuilder

! usage
type(GraphBuilder) :: gb
call gb%run_advertise_hook(this)
```
Every procedure keeps its current signature (`this` is still the
explicit `OuterMetaComponent` argument) — only the calling convention and
public-symbol count change. `item_key`/`proxy_key` stay free functions
(they don't operate on an `OuterMetaComponent` and aren't part of this
type's natural interface).

**Alternative considered**: leave the free-function API and just reduce
the public list by making some of today's public procedures private
module-internals. Rejected because `graphbuilder_advertise`,
`_check_unsatisfied_imports`, `_resolve_connections`, and `_freeze` are
each directly unit-tested (`Test_GraphBuilder.pf`) as well as called
from the three hook wrappers — they need to stay reachable from outside
the module either way, so the type-bound approach achieves the same
surface reduction without sacrificing testability.

## Risks / Trade-offs

- [Loop conversion touches 5 procedures with existing passing tests] →
  Low risk (mechanical, semantics-preserving by established convention);
  full `Test_GraphBuilder.pf` / `Test_GraphBuilderEquivalence.pf` run
  after the change is the verification, not new tests.
- [Nesting refactor changes control flow shape in two procedures whose
  correctness the equivalence test (`Test_GraphBuilderEquivalence.pf`)
  specifically guards] → Mitigation: same test suite must pass unchanged
  before/after; no new edge cases are being added, only restructured.
- [New `OuterMetaComponent` accessors have no compiler-enforced
  "internal use only" boundary] → Accepted trade-off, same class of
  trust the codebase already places in `get_child_meta`'s reach and in
  REQ-GB-002 generally; mitigated by clear doc comments and by not
  re-exporting the names from any user-facing aggregator.
- [Type-bound conversion is a breaking call-site change] → Contained:
  only 3 lifecycle-hook call sites
  (`initialize_advertise.F90` x2, `initialize_accept_transfer.F90` x1)
  and the two test files call these procedures; all are in this repo
  and updated as part of this change.
