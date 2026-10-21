## Why

`docs/graph/spec/20-implementation-roadmap.md` §20.4.5 (Phase 7c) identifies
the third and last of the legacy `ClassAspect` shape-dispatch mechanisms
with no graph-native analog: `WildcardClassAspect` — a connection-topology
discovery mechanism, not a value-conversion one, that fans a single
pattern-declared import out into N real producers, each kept individually
addressable. Phase 7a (`Bracket`/`VectorBracket` 1:1 shape conversion) and
Phase 7b (`Expression` N:1 value conversion) are both landed; `WILDCARD`
remains, along with `SERVICE`, in `is_ordinary_resolvable_import`'s
unconditional "no graph-native resolution path exists" exclusion list
(`GraphBuilder_ConnectionResolution_smod.F90`). Per the scope narrowing
already recorded in roadmap §20.4.5/§20.4.11, `SERVICE` is being retired
outright (replaced by callbacks) and is explicitly not this change's
concern — only `WILDCARD` is in scope here.

**Why the roadmap's own stated blockers are substantially resolved
already.** §20.4.5 flagged two open risks for this phase: (1) "not yet
validated against the real sharing/identity mechanics
`18-state-item-characteristics.md` §18.7 already uses," and (2) cross-level
export visibility — `Test_GraphBuilderEquivalence.pf`'s own note that
`history_wildcard` needs `StateRegistry`'s `propagate_exports`/
`propagate_unsatisfied_imports` machinery, which `GraphBuilder` did not
consult beyond 3b2's single-import bubbling at the time that note was
written. Both have since landed, for reasons unrelated to Wildcard
specifically: `hierarchy-wide-component-graph` Groups 6/6.8 added
symmetric import-mirroring (`graphbuilder_run_propagate_hook`) and
export-qualification (`graphbuilder_run_export_propagate_hook`,
`qualify_exported_name` — producing exactly the `child_name/short_name`
shape legacy's own `propagate_exports_virtual_pt` produces) all the way up
the hierarchy. A cross-level wildcard scan's own source namespace
(`build_qualified_export_namespace`, already built and already used by
`callback-wiring`'s own cross-level resolution) is therefore already
populated with real, correctly-qualified `NodeId`s at every level —
resolved as a byproduct of already-landed, Wildcard-unrelated work, not
something this change must build.

**Why the destination-side pattern mechanism is not a gap at all.**
`for_each_matching_import` — the shared filter every connection-resolution
path (`SimpleConnection`, `MatchConnection`, including the `all_unsatisfied`
catch-all) already runs through — matches every declared import name via
`VirtualConnectionPt%matches()` (regex, unconditionally): an ordinary
declared name is simply a regex that happens to match only itself. Finding
`collection_1`'s own `WILDCARD`-declared import (named `A/E_A.*`) under an
`all_unsatisfied` connection already works today, with zero new code —
confirmed by reading `for_each_matching_import`/`VirtualConnectionPt`
directly. The one genuine asymmetry is on the *source* side: every
existing source-resolution call site (`find_export_node_id`,
`resolve_one`'s exact-match branch, the Phase 7b expression hook) does
exact-name equality, never pattern expansion. `WildcardClassAspect`'s own
real job, past what `MatchConnection`-style resolution already does, is:
re-run the destination's own matched pattern against the *source's*
namespace too (instead of an exact-name lookup) and accumulate every
resulting hit, rather than resolving to a single producer.

**Why this is a composite-member fan-in problem, not a multi-producer
`DependencyNetwork` problem.** Legacy's own `WildcardClassAspect` never
holds one `Field` written by N producers — `get_payload()` is a no-op,
and `add_to_state` inserts each matched export into the import's
`MultiState` individually, under its own matched name (confirmed by
reading `WildcardClassAspect.F90` directly: `matched_items`, an
`ActualPt -> FieldClassAspect` map, flushed one entry at a time). Legacy
itself already models a wildcard import as "one logical slot holding N
independently-named sub-items," not "one item, N writers" — the same
shape `composite-state-spec`'s `declare_member`/member-`NodeId` mechanism
already gives graph-native code, three times over
(`vertical-grid-graph-state-item`, `callback-wiring`'s
`materialize_callback_collection`, `griddedcomponentdriver-integration-
lifecycle`'s composite-State handling generally). `DependencyNetwork`'s
existing single-producer-per-item invariant (`validate_producer_counts`)
is therefore not a conflict to resolve or relax — composite membership is
pure reference binding (`add_state_member`/`get_member`), with zero
`add_dependency` edges, exactly reproducing legacy's own non-conflicting
shape rather than inventing a new one.

## What Changes

- Add a new `resolve_one` branch (`GraphBuilder_ConnectionResolution_
  smod.F90`), dispatched by `dst_node%get_declared_item_type() ==
  MAPL_STATEITEM_WILDCARD`, parallel to the existing callback-interface and
  inout-borrower branches there: instead of resolving to a single producer
  by exact name, re-run the already-matched destination pattern
  (`dst_pt%v_pt`/the wildcard item's own declared short name) against the
  source side's own namespace (`build_qualified_export_namespace`,
  reused as-is — already cross-level-aware per Group 6.8 above), via
  `VirtualConnectionPt%matches()` (the same primitive `for_each_matching_
  import` already uses, applied source-side instead of destination-side).
- On one or more matches: materialize the wildcard import's own
  already-advertised `NodeId` as a composite (real `ESMF_State` payload),
  with one member per match, keyed by that match's own qualified name
  (`A/E_A1`, `B/E_B2`, ...) and bound to that match's own resolved
  `NodeId` — reusing `materialize_callback_collection`'s own construction
  pattern, but targeting the wildcard item's own existing node rather than
  synthesizing a fresh collection node (design.md D1).
- On zero matches: report, matching legacy's own hard-failure intent
  (design.md to decide the exact graph-native reporting shape — `_FAIL`
  vs. `GraphResolutionEntry`/`unsupported`, consistent with how other
  `resolve_one` branches already report a failure for this item).
- Narrow `is_ordinary_resolvable_import`'s exclusion
  (`GraphBuilder_ConnectionResolution_smod.F90`) so a `WILDCARD`-typed
  item is no longer unconditionally treated as "no graph-native resolution
  path" — it is resolvable once a real `MatchConnection`/`all_unsatisfied`
  connection has driven this change's own new branch for it. `SERVICE`
  remains excluded, unaffected (its own retirement is Phase 6b, not this
  change).
- Add `Test_Scenarios` coverage reusing the existing `history_wildcard`
  fixture under `GRAPH_MODE_BOTH` (equivalence-harness style, matching
  Phase 7b's own `expression*` coverage pattern), plus direct unit
  coverage for the new `resolve_one` branch's matching/accumulation/
  zero-match-reporting behavior.

## Capabilities

### New Capabilities
(none — this extends the existing graph-native `GraphBuilder`/
`composite-state-spec` architecture with one new `resolve_one` dispatch
branch; no new capability directory is warranted)

### Modified Capabilities
- `graph/graph-builder`: adds a new resolution branch for `WILDCARD`-typed
  imports (pattern-expanding source-side fan-in into a composite, instead
  of exact-name single-producer resolution), and narrows the "an import
  with no graph-native resolution path is excluded from propagation"
  requirement to no longer include `WILDCARD`. `SERVICE` remains excluded.

No other capability's own requirements change. `graph/composite-state-spec`
already specifies member declaration/binding generically — this change is
a new production consumer of that existing mechanism (materializing
members from a pattern-match scan instead of a declared `VariableSpec`
tree), not a requirement change to that capability itself.

## Impact

- `superstructure/generic/GraphBuilder_ConnectionResolution_smod.F90`: one
  new `resolve_one` branch (dispatched on `MAPL_STATEITEM_WILDCARD`), one
  new source-side pattern-matching helper (reusing
  `build_qualified_export_namespace`/`VirtualConnectionPt%matches`), one
  narrowed gate (`is_ordinary_resolvable_import`).
- `superstructure/generic/graph/GraphStateItem.F90` or
  `CompositeStateMaterialization.F90` (decided in design.md, if any change
  is needed beyond reusing existing composite-member accessors as-is):
  possible small addition if the wildcard item's own existing `NodeId`
  needs a materialize-in-place path distinct from
  `materialize_callback_collection`'s "always synthesize a fresh node"
  shape.
- `superstructure/generic/tests/scenarios/history_wildcard/`: existing
  fixture gains graph-native (`GRAPH_MODE_BOTH`) coverage; no new fixture
  files expected unless a gap surfaces during implementation.
- `superstructure/generic/graph/tests/` and/or
  `superstructure/generic/tests/`: new unit tests for the new `resolve_one`
  branch's matching/accumulation/zero-match behavior.
- No change to legacy `WildcardClassAspect`/`MatchConnection` (those remain
  the `GRAPH_MODE_REGISTRY`/legacy-coupler path, unaffected).
- No change to `Service` resolution (tracked separately as a Phase 6b
  audit-and-delete item, not migrated) or to Phase 7a/7b's own mechanisms
  (unaffected, already landed).
