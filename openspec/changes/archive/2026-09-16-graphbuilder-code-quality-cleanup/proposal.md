## Why

The Phase 3b `GraphBuilder` module (`superstructure/generic/GraphBuilder.F90`)
landed with several implementation-quality issues that don't match
conventions used elsewhere in `superstructure/generic/graph/` and its
neighbors: it's the only module in that subtree still using raw gFTL
`%begin()/%end()` iteration instead of the `ftn_begin()/ftn_end()` idiom
used everywhere else, several of its procedures have 4-level-deep nested
control flow, it reaches into a child component's internals through a
3-hop accessor chain it owns itself rather than a narrower framework-level
one, and its 8 free public procedures could be a single type with
type-bound procedures to shrink the module's public surface. None of this
changes observable behavior — it's a targeted internal cleanup identified
during a post-implementation review.

## What Changes

- Convert all gFTL loops in `GraphBuilder.F90` (`graphbuilder_advertise`,
  `check_match_connection_unsatisfied`,
  `graphbuilder_check_unsatisfied_imports`, `resolve_match_connection`,
  `graphbuilder_resolve_connections`) from `%begin()/%end()` with
  `next()` at the bottom of the loop body to `%ftn_begin()/%ftn_end()`
  with `call iter%next()` at the top, matching the idiom used in
  `OuterMetaComponent/recurse.F90`, `specs/StateItemSpec.F90`,
  `OuterMetaComponent/SetServices.F90`, and the
  `GriddedComponentDriver` coupler runners.
- Reduce nesting depth in `resolve_match_connection` and
  `check_match_connection_unsatisfied` (currently 4-deep
  do/if/if/if-else arrow code), most likely via guard-clause style
  (`cycle` on non-match) and/or extracting their shared
  filter-matching-imports logic, which today is duplicated almost
  verbatim between the two procedures.
- Replace `get_child_meta`'s `get_child() -> get_gridcomp() ->
  get_outer_meta()` reach-around with direct
  `get_child_component_spec(child_name)` /
  `get_child_component_graph(child_name)` accessors added to
  `OuterMetaComponent` itself, removing `GraphBuilder.F90`'s need to
  know about `GriddedComponentDriver`/`get_gridcomp`/`get_outer_meta`.
  **These two new accessors are visible only within `mapl_generic`**
  (not part of `OuterMetaComponent`'s general public API) — they exist
  to serve this same "framework may reach into a child's own state"
  carve-out (REQ-GB-002/REQ-HIER-005) that `get_child_meta` already
  relied on, not to become ordinary user-facing API.
- Replace `GraphBuilder.F90`'s 8 free public procedures
  (`graphbuilder_advertise`, `graphbuilder_check_unsatisfied_imports`,
  `graphbuilder_resolve_connections`, `graphbuilder_freeze`,
  `graphbuilder_run_advertise_hook`, `graphbuilder_run_activate_hook`,
  `graphbuilder_run_connect_hook`) with a single empty `GraphBuilder`
  derived type carrying no state (consistent with design.md's existing
  "stateless-per-call" decision) and type-bound procedures taking the
  same `OuterMetaComponent` argument they do today, e.g.
  `gb = GraphBuilder(); call gb%run_advertise_hook(this)`. `item_key`
  and `proxy_key` remain free functions (out of scope — see below).
- **BREAKING** (internal-only): call sites in
  `initialize_advertise.F90`, `initialize_accept_transfer.F90`, and
  both `Test_GraphBuilder.pf` / `Test_GraphBuilderEquivalence.pf` update
  to the new type-bound and accessor call shapes.

### Explicitly out of scope for this change
- **Encapsulating `item_key()`/`proxy_key()` in a derived type**
  (skip entirely, per discussion — not pursued now).
- **Trapping errors in `graphbuilder_run_*_hook`/`report_if_failed`**
  through the standard `_RC`/`_VERIFY` macros (deferred — those macros
  propagate/return on failure, which is the opposite of this
  boundary's intentional "catch and log, never break real MAPL init"
  design; a "log and continue" idiom that doesn't hardwire a specific
  logger still needs to be worked out before touching this).

## Capabilities

### New Capabilities
(none)

### Modified Capabilities
(none — this is a pure internal refactor of the `graph-builder`
capability's implementation; no requirement or scenario in
`openspec/specs/graph/graph-builder/spec.md` changes. `.openspec.yaml`
sets `skip_specs: true`.)

## Impact

- **Code**: `superstructure/generic/GraphBuilder.F90` (primary),
  `superstructure/generic/OuterMetaComponent.F90` +
  `superstructure/generic/OuterMetaComponent/*.F90` (two new
  `mapl_generic`-only accessors), `initialize_advertise.F90`,
  `initialize_accept_transfer.F90`.
- **Tests**: `superstructure/generic/tests/Test_GraphBuilder.pf`,
  `superstructure/generic/tests/Test_GraphBuilderEquivalence.pf` —
  call-site updates only, no new test scenarios (behavior is
  unchanged).
- **Public API**: `mapl_GraphBuilder_mod`'s public surface shrinks from
  8 procedures to 1 type (`item_key`/`proxy_key` unchanged). The two
  new `OuterMetaComponent` accessors are `mapl_generic`-internal, not
  part of its general public API.
- **No behavior change**: every scenario in
  `openspec/specs/graph/graph-builder/spec.md` continues to hold
  exactly as today.
