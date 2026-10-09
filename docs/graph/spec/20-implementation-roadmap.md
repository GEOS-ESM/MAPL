# 20. Implementation Roadmap

Status: planning record, not a design spec. Captures the phased
implementation approach and repo-strategy decision discussed when scoping
an OpenSpec-driven ("SDD") implementation effort. Update this document as
the plan changes; do not let it silently drift out of sync with what is
actually being built — treat divergence the same way the rest of this
specification treats a stale requirement.

## 20.1 Readiness assessment

The specification is mature enough to plan a phased implementation, on
the following basis:

- **Core graph model is settled**: `03` (GraphNode hierarchy), `05`
  (identities), `06` (DependencyNetwork), `07` (ComponentGraph), `10`
  (transforms/ports), `11` (NodeRevision + update algorithm) are all
  `[SETTLED]`, including the two items formalized in this same pass
  (CHANGELOG 1.3): Q3 (port-binding storage) and Q11 (GraphStateItem/GraphValue
  reconciliation, fully resolved).
- **Genuinely deferred items do not block core work**: `16` (ordinary
  inout) is `[DEFERRED]`; `18` (StateItemCharacteristic) is
  `[SPECULATIVE]` with cross-graph mechanics still open (Q12/Q13); Q9
  (compiled execution) is explicitly a later phase; `19` (visualization
  export) is additive and not on the critical path.
- **Remaining opens are scoped to later phases**, not the core: RouteHandle
  time-dependent renewal detail, geometry shallow-copy-across-grid-
  recreate ESMF verification (`07-component-graph.md` §7.4.1), callback
  term naming (Q6), StateIntent-vs-placement (Q7) — none of these block
  `ComponentGraph`/`DependencyNetwork`/identity work.

**Conclusion:** proceed with Phase 1 (below) now. Do not wait on Q1/Q2/
Q6–Q10/Q12–Q18 — they are either already resolved or are correctly scoped
to phases that come later.

## 20.2 Repo strategy

**Decision:** develop the graph-neutral core (Phase 1–2, §20.3) in a
separate repository — this one, promoted from a specification-only repo
to a real library repo — rather than inside MAPL from the start.

**Rationale**, not merely an LLM-cost convenience, though that is real
too:

1. The specification already mandates the separation architecturally.
   `07-component-graph.md` REQ-CG-002 and `08-graph-builder.md` REQ-GB-001
   are hard rules: `ComponentGraph`/`DependencyNetwork`/`GraphNode*`/
   `GraphValue*` MUST NOT depend on `OuterComponent`, `StateRegistry`, or
   component-hierarchy implementation. `08-graph-builder.md` §8.3 states
   the reason directly — it is what keeps the graph core unit-testable in
   isolation, with synthetic nodes/values, with no ESMF component
   hierarchy present at all.
2. The core's actual external dependencies are narrow: gFTL containers
   (`05-identities.md` REQ-ID-006) and bare ESMF handle types
   (`ESMF_Field`/`FieldBundle`/`State`/`RouteHandle` as `GraphStateItem`
   components, `04-graph-value-hierarchy.md` REQ-SI-002). It does not need
   MAPL, `GriddedComponentDriver`, or the component hierarchy at all for
   Phase 1–2's scope.
3. **LLM context cost**: developing Phase 1–2 inside a full MAPL checkout
   would put the entire MAPL source tree (component hierarchy,
   `StateRegistry`, ESMF bindings, the existing extension/coupler
   machinery, generated code, build system) in scope for every
   agent-assisted change, even though none of it is relevant to
   `ComponentGraph`-internal work. A separate repo bounds the working set
   to what REQ-CG-002/REQ-GB-001 already say is the correct scope.
4. **This saving is real but scoped to Phase 1–2 only.** Phase 3+
   (§20.4) integrates with `StateRegistry`/`OuterComponent`/
   `GriddedComponentDriver` by construction — that work requires MAPL
   source in context regardless of which repo it is edited from. Repo
   separation does not avoid that cost; it just keeps it confined to the
   phases that actually need it.

**Tooling note:** `openspec` (installed locally) has a `store` concept for
standalone repos registered independently of a project's own OpenSpec
root, which fits "core library repo, consumed later by MAPL as a
dependency" directly — prefer that shape over forcing the core repo to
pretend to be a subdirectory of MAPL from day one.

## 20.3 Phase 1–2: graph-neutral core (this repo, or its successor library repo)

Buildable and testable now, no MAPL dependency:

- **Phase 1** — `05` Identities (NodeId + sibling ID types + template),
  `03` GraphNode hierarchy (`GraphNode`/`BaseGraphNode`/`StateItemNode`/
  `OperationGraphNode` stub), `04` §4.6 `GraphStateItem` (incl. REQ-SI-006
  membership maps), `06` DependencyNetwork (adjacency, cycle rejection,
  validation), `07` ComponentGraph (ownership, lifecycle, freeze).
- **Phase 2** — `10` TransformGraphNode + named ports + REQ-XFORM-005
  external port-binding table, `11` NodeRevision + demand-driven update
  algorithm (runtime-interpreted form; compiled form is Q9, later).
- **Phase 2 (additive)** — `19` §19.2's graph-neutral exporter layer
  (REQ-VIZ-003): depends only on the public `ComponentGraph`/
  `DependencyNetwork` query API and `NodeId%to_string()`, so it belongs
  here rather than waiting for Phase 3.

Exit criterion for Phase 1–2: synthetic-graph test suite exercising
construction, wiring, cycle rejection, freeze, demand-driven update, and
graph-neutral export, with no ESMF component/GridComp/StateRegistry
involved anywhere in the tests.

## 20.4 Phase 3+: MAPL integration (inside MAPL, or a MAPL checkout consuming Phase 1–2 as a dependency)

Requires real `StateRegistry`/`OuterComponent`/`GriddedComponentDriver`
context by construction — no repo-separation saving available here:

- **Phase 3** — `08` GraphBuilder (integration layer), `02` component
  hierarchy wiring (OuterComponent ownership, child proxy nodes,
  encapsulation boundary), `09` extension reuse / backward-compatible
  automatic-coupler behavior. `19` §19.2's enrichment layer (REQ-VIZ-004,
  name resolution via `StateRegistry`) ships here. See §20.4.1 for the
  sub-sequencing this phase needs before implementation starts.
- **Phase 4** — `12` MethodGraphNode + invocation adapters +
  `GriddedComponentDriver` + SetServices lifecycle, `15` callbacks
  (CallbackInterface/registry/Handler/Invoker), `13` geometry and
  vertical grids, `14` route handles (RouteHandleValue/Key, sharing).
  Time-dependent geometry/RouteHandle renewal (§13.4/§14.4) and
  exchange-component geometry (REQ-GEO-002a) are explicitly deferred
  out of this phase's initial scope, not silently assumed solved. See
  §20.4.3 for the sub-sequencing this phase needs before implementation
  starts.
- **Phase 5 (speculative/deferred, do not block on these)** — `18`
  StateItemCharacteristic hierarchy, `16` ordinary inout items, Q9
  compiled-execution optimization. See §20.4.4 for the sub-sequencing
  this phase needs before implementation starts.
- **Phase 6 (cleanup, growable — see §20.4.2)** — retire legacy
  `StateRegistry`/`ExtensionFamily`/aspect-based coupling once the
  graph-native paths above are the actual default, plus a running list
  of small, otherwise-easy-to-lose follow-ups that are blocked on that
  retirement (e.g. 3c's own `UnitsConverterTransform` ->
  `ConvertUnitsTransform` rename).
- **Phase 7 (shape-changing connection dispatch — discovered gap, not
  previously tracked by any phase above)** — legacy `ClassAspect`'s own
  shape-dispatch role (`09-extension-reuse.md`'s existing
  `Characteristic`/`CharacteristicMap` family has no analog of it yet):
  Bracket/VectorBracket -> Field/Vector time-interpolation conversion,
  Expression -> Field N-ary evaluation, and Wildcard/Service
  pattern-based multi-source fan-in. Directly relevant to eventually
  running `Test_Scenarios` against the graph-native path instead of
  `StateRegistry`, which is the intended strong end-to-end signal that
  all necessary capabilities have been migrated. See §20.4.5 for the
  sub-sequencing this phase needs before implementation starts, and why
  none of it belongs in Phase 5's `StateItemCharacteristic` family
  despite the naming similarity.
- **Phase 8 (add support for `VariableSpec` validation — discovered
  gap, not previously tracked by any phase above)** — `verify_variable_spec`
  (`superstructure/generic/specs/VariableSpec.F90`) already exists but is
  dead code: module-private, not re-exported, and called from nowhere,
  not even `make_VariableSpec` itself. Three already-landed graph-native
  "mark the item" fields (`callback_interface_id` - callback-wiring,
  `state_item_variant` - vertical-grid-graph-state-item,
  `is_inout_borrower` - ordinary-inout-direct-alias) are consequently
  reachable only from synthetic test code: neither real production
  construction path (`MAPL_GridCompAddSpec`/`gridcomp_add_spec`,
  `MAPL_Generic.F90`, or YAML parsing,
  `ComponentSpecParser/parse_var_specs.F90`) exposes any of the three as
  a keyword argument, and both bypass `ComponentSpec%add_var_spec()`
  entirely (pushing directly into `var_specs`), so there is also no
  single already-existing call-through point every construction path
  shares. `ordinary-inout-direct-alias`'s own mutual-exclusion guard
  (`callback_interface_id%is_valid() .and. is_inout_borrower`) was
  necessarily placed in `GraphBuilder.F90`'s `resolve_one` instead - it
  only fires when a connection happens to resolve against the item, not
  at declaration time, and is reachable from production code even less
  than the fields it guards. See §20.4.6 for scope.

### 20.4.1 Phase 3 sub-sequencing

Phase 3, taken as one unit, does not fit a single spec-driven change
proposal without an unreasonable context/cost footprint (four
genuinely separable concerns, at least one — 3b below — requiring
real-configuration validation, not just unit tests). Split into four
ordered sub-changes instead. `08-graph-builder.md` §8.2's own
responsibility table includes wildcard/callback connection resolution
and ESMF-relationship compilation rows that belong to Phase 4/5 above,
not Phase 3 — the sub-changes below exclude them accordingly.

- **3a. Component-hierarchy foundation** (`02`, REQ-HIER-001..006) —
  `OuterComponent`/`OuterMetaComponent` ownership shape (own +
  per-child `GriddedComponentDriver`, one local `ComponentGraph`,
  framework-managed states, public ports), the parent-may/child-may-not
  encapsulation boundary (REQ-HIER-003), and the proxy-node *storage*
  mechanism (REQ-HIER-006) only — not yet the logic that populates it
  (that is GraphBuilder's job, 3b). First point requiring a real
  `ESMF_GridComp`; does not yet need `StateRegistry`.
- **3b. GraphBuilder: advertising + ordinary connection resolution**
  (`08` §8.2: advertising, creating `StateItemNode`s, resolving
  ordinary connections, default dependency network, public ports/child
  proxies, validate/freeze) — the slice `17-open-questions.md` Q10
  flags as "the first point where existing MAPL coupler behavior must
  be reproduced exactly," recommending side-by-side comparison against
  the existing imperative coupler on real configurations before
  removing that path. Kept deliberately narrow: REQ-EXT-003's exact-
  match/no-op case only, no mismatch/extension handling yet, and (see
  3b2 immediately below) *single-level* only — a connection is resolved
  only when both endpoints declare the item directly; no cross-level
  propagation.
- **3b2. Cross-component unresolved-import propagation** — REQUIRED
  completion of 3b's own job, not optional polish: discovered missing
  during 3b's implementation and review (`openspec/changes/
  graphbuilder-advertising-connections`). An import left unresolved by
  3b's single-level matching is exactly legacy's own
  `propagate_unsatisfied_imports()` case — the point where an
  unsatisfied import must be re-advertised as needed one level up the
  hierarchy (mirroring `StateRegistry_Propagation_smod.F90`'s
  `childname/itemname` bubbling) so an ancestor's own connections get a
  chance to resolve it, all the way up if necessary. Checking for
  unsatisfied imports with no mechanism to ever satisfy them across
  component boundaries is not useful on its own; this sub-change is
  what makes 3b's activate-time check (`graphbuilder_check_unsatisfied_imports`)
  connect to anything beyond a log message. Depends on 3b's proxy-node
  and resource-index mechanisms; does not require 3c's mismatch/
  extension machinery. Sub-sequencing (3c, 3d below) numbering is
  intentionally left alone rather than renumbered — this slots in
  functionally between 3b and 3c, before 3c's mismatch-driven work
  needs to run on whatever is still unresolved after propagation.
- **3c. Extension reuse** (`09`, REQ-EXT-001/002/003/005) —
  `CharacteristicId`/`Characteristic`/`CharacteristicMap` (graph-native
  analog of legacy `AspectId`/`StateItemAspect`/`AspectMap`, mirroring
  those patterns' shape — type-safe id, a gFTL2 polymorphic map, and a
  deferred `build_transform` method exactly like
  `StateItemAspect%make_transform` — while deliberately independent of
  their implementation), Transform-chain creation for mismatched export/
  import pairs (real execution ships for `units` only, via
  `UnitsCharacteristic%build_transform` and a new `UnitsConverterTransform`
  — named to avoid colliding with legacy's `ConvertUnitsTransform`
  module; rename once that legacy module is removed), and the
  extension-family reuse search restated as a graph-native lookup via
  `ComponentGraph`'s existing resource index. Depends on 3b's wiring and
  Phase 2's `TransformGraphNode`/`Transform`. REQ-EXT-002a stays
  `[SPECULATIVE]`/`[OPEN]`, not resolved by this sub-change.
- **3c2. StateRegistry/OuterComponent visibility for extension items**
  (`09` REQ-EXT-004) — discovered missing during 3c's implementation:
  `StateItemSpec` has no construction path independent of its full
  `AspectMap`, so satisfying REQ-EXT-004 without reusing `StateRegistry`'s
  existing methods (a hard constraint by this point in the effort —
  `StateRegistry` is intended to be retired once graph development is far
  enough along, not extended) means a second, independent aspect-map-
  equivalent construction pipeline — deliberately not attempted inside
  3c itself. Extension items created by 3c are correct graph structure
  (real `NodeId`s, dependency edges, reuse semantics) but are not yet
  visible to `StateRegistry` or any ESMF state; 3c2 is what makes them
  visible. Depends on 3c's chain-creation machinery; does not need real
  providers for characteristics beyond `units` to be useful on its own.
  Numbering intentionally mirrors 3b/3b2's own precedent (a sub-change
  whose own implementation surfaced a required, committed follow-up) —
  slots functionally after 3c, before 3d.
- **3d. Visualization enrichment layer** (`19` REQ-VIZ-004 only) —
  wraps the Phase 1–2 graph-neutral exporter with a `NodeId -> label`
  resolver backed by `StateRegistry`. Independent of 3b/3c's hard
  parts (only needs 3a's `OuterComponent` shape + name lookup); MAY be
  reordered directly after 3a if a lower-risk early win is preferred.
  REQ-VIZ-005 (hierarchy-wide export) stays `[OPEN]` (Q18) and is out
  of scope for this sub-change.

**Repo/tooling note (extends §20.2).** Phase 3+ code (this section)
lives in the MAPL repo/checkout, not the Phase 1–2 core repo — per
§20.2, the LLM-context saving from repo separation does not apply once
`StateRegistry`/`OuterComponent` context is required regardless. The
Phase 1–2 core repo remains a distinct git repository (not merged into
MAPL) and, if referenced as a git submodule during this transition,
that MUST be treated as a temporary bootstrap mechanism only — not a
standing precedent for one subrepo per major MAPL design element. The
  This MAPL checkout now contains full architecture documents under
  `docs/graph/spec/` and executable Phase 1-2 requirement subsets under
  `openspec/specs/graph/`; Phase 3 agents should use these local copies
  rather than requiring access to the former standalone core repository.

### 20.4.2 Phase 6: legacy retirement (cleanup phase)

**Purpose:** every sub-change from 3c onward has deliberately kept the
graph-native path *additive* — real production behavior still runs
through legacy `StateRegistry`/`ExtensionFamily`/`ClassAspect`/
`AspectMap` (`superstructure/generic/registry/`,
`superstructure/generic/specs/*Aspect*.F90`), with the graph-native
equivalent gated off by default (e.g. `extension-registry-visibility`'s
`materialize_extensions` flag) or simply not yet wired into the real
init sequence at all. That additive posture is correct while the graph
path is still being built out, but it also means a small, growing set
of otherwise-easy-to-lose follow-ups keeps accumulating — each one
individually too small to be its own roadmap phase, but genuinely
blocked until legacy is gone, not merely deferred by choice. Phase 6 is
where that debt gets paid: (a) retire `StateRegistry`/`ExtensionFamily`/
the aspect system once the graph-native paths are the *actual* default
(gates removed, not just flippable) and validated at production scale
— not attempted piecemeal inside any earlier sub-change; and (b) work
through the list below once (a) has landed.

**Entry criterion:** do not start (a) until every real production code
path that currently depends on `StateRegistry`/`StateItemSpec`/
`make_StateItemSpec`/`make_aspects`/any `ClassAspect` subclass has a
graph-native replacement that has been validated (not merely built) as
the default behavior — matching each earlier sub-change's own explicit
"read-only background reference, never called into" boundary around
that legacy machinery, now finally made moot by having nothing left
that needs it.

**Growable list — append here, do not let a future sub-change silently
drop its own "blocked until legacy is retired" follow-up elsewhere:**

- Rename `UnitsConverterTransform` -> `ConvertUnitsTransform`
  (`superstructure/generic/graph/UnitsConverterTransform.F90`) once
  legacy `mapl_ConvertUnitsTransform_mod`
  (`superstructure/generic/transforms/ConvertUnitsTransform.F90`) is
  removed — module names share one global namespace, so the two cannot
  coexist under the same name. Originates from 3c (design.md); tracked
  as task 7.1 of `extension-registry-visibility`, confirmed still
  blocked 2026-09-17: legacy `UnitsAspect%make_transform`
  (`superstructure/generic/specs/UnitsAspect.F90:128`) still
  instantiates the legacy type directly from the live
  `StateRegistry_Extensions_smod.F90` dispatch path — load-bearing, not
  dead code.
- Consolidate `VariableSpec%itemType` (`ESMF_StateItem_Flag`,
  `superstructure/generic/specs/VariableSpec.F90`) onto a single,
  graph-native `MAPL_StateItem_Flag`-typed field (per 4f's
  `state_item_variant`, `mapl_StateItemFlag_mod`), with `itemType`
  becoming a derived/truncated view (native-kind parent of whatever
  refined value is stored) rather than a separately-stored field.
  Originates from 4f (`openspec/changes/vertical-grid-graph-state-item`,
  design.md discussion): every graph-native refined value (`GEOM`,
  `VECTOR`/`BRACKET`/`VECTORBRACKET`, `VERTICALGRID`, `ROUTEHANDLE`)
  already implies exactly one native `itemType` parent and never
  contradicts it — the two fields hold no genuinely independent
  information, only `VariableSpec`'s historical split between the
  legacy `mapl_StateItem_mod`/`ClassAspect`-dispatch vocabulary and the
  newer graph-only vocabulary keeps them as two stored fields today
  (unlike `GraphStateItem%itemType()`, which is already a pure derived
  query, not stored, at the node-payload tier). **Blocked**:
  `itemType`'s current `ESMF_StateItem_Flag` values include
  `WILDCARD`/`SERVICE`/`SERVICE_PROVIDER`/`SERVICE_SUBSCRIBER`/
  `EXPRESSION` (`mapl_StateItem_mod`, codes 201-208) with no graph-native
  equivalent yet, dispatched live from `make_ClassAspect`
  (`VariableSpec.F90:759`) and parsed live from YAML
  (`ComponentSpecParser/to_itemtype.F90`) — real, in-use legacy vocabulary,
  not dead code. `SERVICE` is expected to be retired outright (replaced
  by callbacks, `CallbackInterfaceId`), but `WILDCARD`/`EXPRESSION` are
  not yet represented in graph's vocabulary at all. Do not attempt this
  consolidation before `ClassAspect`'s own dispatch is itself retired or
  graph-natively replaced.
- Reconcile overlapping regrid-store settings in
  `superstructure/generic/graph/RouteHandleKey.F90` and
  `infrastructure/regridder_mgr/RoutehandleParam.F90`: evaluate extracting
  shared value representation/defaults or another reuse mechanism to prevent
  field drift, while preserving `RouteHandleKey`'s semantic-key rendering and
  `RouteHandleParam`'s execution role. A graph dependency on infrastructure is
  acceptable; graph code must not depend on `superstructure/generic/registry/`
  or otherwise use registry-layer APIs. Follow-up to Phase 4g, not a reason to
  block current implementation.

### 20.4.3 Phase 4 sub-sequencing

Phase 4, like Phase 3 (§20.4.1), does not fit a single spec-driven
change proposal without an unreasonable context/cost footprint. Unlike
Phase 3, none of Phase 4 is built yet — no `MethodGraphNode`, no
`CallbackInterface`/registry beyond an empty `CallbackInterfaceId`
identity stub, no `RouteHandleValue`/`RouteHandleKey`, no geometry
`GraphStateItem` handling — so this is greenfield work on top of the
completed Phase 1–3 foundation, not an extension of partially-built
Phase 4 code. `17-open-questions.md` Q10 already gives this phase's
internal ordering rationale (methods/lifecycle first since "phases
start flowing through the graph" here; callbacks next as new capability
with no legacy behavior to match; geometry/route-handles last since
they touch the most existing special-cased code and should wait until
the graph plumbing around them is well-exercised). Split into seven
ordered sub-changes:

**Landed prerequisite, discovered ahead of this list (not one of the
original seven, inserted before it):** `composite-state-spec`
(`openspec/changes/archive/2026-09-18-composite-state-spec`,
`openspec/specs/graph/composite-state-spec/spec.md`). `15-callbacks.md`'s
`CallbackStateBinding` (REQ-CB-007, "argument name → member `NodeId`")
requires a callback State's members to be individually graph-visible —
nothing in `VariableSpec`/`GraphBuilder` supported that before this
change landed (a `MAPL_STATEITEM_STATE` declaration produced an empty,
opaque `ESMF_State`, per proposal.md - Why). Resolved by letting
`VariableSpec` declare its own composite member structure directly
(`declare_member`/`get_member`/`get_member_names`, a member being any
ordinary `VariableSpec`, leaf or further-nested) and by
`GraphBuilder`'s `advertise_one` recursing into declared members to
build a real `StateItemNode` tree. Does not block 4a/4b (neither touches
composite structure); is a real prerequisite for 4c/4d below.

- **4a. MethodGraphNode + invocation adapters** (`12` REQ-MTH-001/002/
  004/005/006) — the node type covering both GridComp phase invocation
  and attached-State-method invocation through one invocation-adapter
  abstraction (`GridCompMethodInvocation`/`StateMethodInvocation`,
  `17-open-questions.md` Q2). Synthetic-driver testable, same posture as
  3a: no real `GriddedComponentDriver` wiring yet, no ESMF component
  required.
- **4b. GriddedComponentDriver integration + init lifecycle** (`12`
  REQ-MTH-007..013) — the stable driver-lookup mechanism (REQ-MTH-009),
  the trigger/advance discipline around invocation (REQ-MTH-003a), and
  the full `advertise → modify_advertised → cycle(realize_provided,
  accept_transfer, realize_accepted) → read_restart → user_specific`
  ordering (REQ-MTH-011). REQ-MTH-011 step (c)'s convergence algorithm
  (how progress is detected, iteration-limit behavior, non-convergence
  handling) is explicitly `[OPEN]` in the spec — this sub-change's
  design.md MUST resolve it as a planned, up-front design decision
  before implementation starts, not discover it mid-implementation the
  way 3b's own real-configuration validation surfaced 3b2 unplanned.
  First sub-change requiring real `GriddedComponentDriver`/ESMF context;
  depends on 4a.
- **4b2. Unify `OuterMetaComponent`'s own driver into its child-driver
  map** — discovered during 4b's own code review
  (`openspec/changes/griddedcomponentdriver-integration-lifecycle`), not
  a required completion of 4b's job the way 3b2 was for 3b: 4b's own
  `DriverResolver` already works correctly as shipped, and this
  sub-change is a pure simplification, not a correctness gap. Does not
  block 4c/4d. REQ-MTH-008's "own driver + one driver per child"
  ownership shape currently lives as two separate representations on
  `OuterMetaComponent` — a single `user_gc_driver` field plus a
  `children: GriddedComponentDriverMap` — which forces every
  driver-key-resolution call site (4b's own
  `OuterMetaComponentDriverResolver`, and any future one) to branch on
  "is this the `<self>` sentinel or a real child name" instead of doing
  one uniform lookup. Store the component's own driver in the *same*
  map, under the already-established `<self>` sentinel
  (`GraphBuilder.F90`'s own `SELF_COMPONENT_NAME`, matching
  `StateRegistry_Hierarchy_smod`'s own precedent for treating "the
  component itself" as a reserved name among its children) — unifying
  "the user gridcomp" and "a child gridcomp" as the same underlying
  representation, differing only in which name resolves to which entry.
  Real work, not a one-line rename: `user_gc_driver` is currently
  accessed as a bare field (not only through the existing
  `get_user_gc_driver()` accessor) from several of `OuterMetaComponent`'s
  own submodules (e.g. `initialize_accept_transfer.F90`'s
  `this%user_gc_driver%get_states()`), every one of which would need to
  move to a uniform accessor once the separate field is gone; must also
  confirm no existing code path can ever declare a child literally
  named `<self>` (almost certainly already excluded by the same
  sentinel convention elsewhere, but worth confirming explicitly rather
  than assuming). Scoped to `OuterMetaComponent`'s own driver storage
  only — does not attempt the broader "is a component itself just a
  specially-named child everywhere" unification `GraphBuilder.F90`'s own
  `is_self()`/`SELF_COMPONENT_NAME` checks hint at for connection
  resolution; that is a separate, larger question outside driver
  storage, not assumed solved by this sub-change.

  **Evaluated during planning (2026-09-20), declined — not
  implemented.** Change proposal drafted
  (`openspec/changes/outermetacomponent-driver-map-unification`,
  removed after this decision, never merged) and taken through design
  before implementation began. Two findings killed it:
  1. `this%children` (`OuterMetaComponent.F90:57`) is not merely
     driver storage — it is the live "list of real children" that
     `get_num_children.F90`, `get_child_name.F90`, `recurse.F90`
     (`initialize`/`write_restart`), `run_children_.F90`,
     `run_clock_advance.F90`, `finalize.F90`'s `recurse_finalize_`, and
     `apply_to_children_custom.F90` all iterate directly. Inserting the
     self driver into that same map under `<self>` would have made
     every one of those sites additionally process the self entry as a
     "child," double-invoking `initialize`/`run`/`finalize`/
     `clock_advance` on the user's own driver (on top of the existing
     explicit `user_gc_driver`/`get_user_gc_driver()` calls already
     present in `run_user.F90`, `run_custom.F90`, `finalize.F90`,
     `run_clock_advance.F90`) and off-by-one-ing `get_num_children`/
     `get_child_name`. Filtering `<self>` out of all seven sites was
     considered and rejected: it adds guard logic in seven lifecycle-
     critical loops, the opposite of this sub-change's own stated
     purpose (measurably *reducing* duplicated logic).
  2. With that option off the table, the fallback — collapse
     `OuterMetaComponentDriverResolver.F90`'s self-vs-child `if/else`
     into a new `OuterMetaComponent` accessor without merging the
     underlying storage — was checked against the actual codebase
     first rather than assumed worthwhile: a repo-wide search found
     that branch already exists in exactly one place
     (`OuterMetaComponentDriverResolver.F90:65-82`); every other
     `get_user_gc_driver()` caller already knows it wants the self
     driver and calls it directly, with no `driver_key`-style branch to
     share. There is no second copy of the logic anywhere to
     consolidate, so relocating those four lines to a new method would
     move code sideways for zero measurable reduction in duplication —
     the premise this sub-change was filed under.

  Net: both mechanically-available paths are net-negative or net-zero
  against the sub-change's own justification. 4b's `DriverResolver`
  remains as shipped (two representations, one already-centralized
  branch) — correct, if not maximally elegant. Revisit only if a
  second real consumer of self-or-child driver-key resolution appears;
  until then there is nothing to unify.
- **4c. Callback data model + registry** (`15` §15.2–15.7) —
  `CallbackInterface`/`CallbackArgumentSpec`/`CallbackMethodSpec`/
  `CallbackStateBinding`/`CallbackInterfaceRegistry`. Static and
  unit-testable, no `GraphBuilder` wiring yet. Depends on
  `composite-state-spec` (landed, above) for `CallbackStateBinding`'s
  member-`NodeId` addressability; otherwise no new dependency beyond
  Phase 1–3 — MAY proceed in parallel with 4a/4b if desired, though Q10's
  stated order (methods before callbacks) is the default assumption.
- **4d. Callback wiring** (`15` §15.9–15.10) — `GraphBuilder`
  wildcard/regex expansion against the flattened qualified-export
  namespace (REQ-CB-016, pattern syntax settled as regex per Q5),
  per-method `DependencyNetwork`s for get/put argument flow
  (REQ-CB-018), and the invoke-once-after-all-args-ready discipline
  (REQ-CB-020). Depends on 4a (binds to a `MethodGraphNode`, REQ-CB-019),
  4c, and `composite-state-spec` (landed, above).
- **4e. Horizontal geometry as GraphStateItem** (`13` §13.1–13.2) —
  **landed** (`openspec/changes/horizontal-geometry-graph-state-item`):
  geometry carried as an incomplete `esmf_field` proxy
  (`ESMF_FIELDSTATUS_GRIDSET`), given real graph structure for
  REQ-GEO-001..003's three single-source cases. **Deviation from this
  entry's original wording, discovered during implementation:** "resolved
  through ordinary advertise/connect/transform-if-needed rules" turned out
  to be the wrong framing — `GeometrySpec`/`initialize_geom_a.F90`/
  `initialize_geom_b.F90` already resolve own/from-parent/from-child,
  entirely before `GraphBuilder.F90` ever runs (lifecycle phases 3-4,
  hierarchy-wide, before phase 5's `GENERIC_INIT_ADVERTISE`). The landed
  design is a dedicated `GraphBuilder.F90` hook
  (`run_geometry_hook`/`graphbuilder_advertise_geometry`/
  `graphbuilder_resolve_geometry`) that represents that already-resolved
  outcome as real graph structure (one `StateItemNode` per component, plus
  a cross-`ComponentGraph` dependency edge for the ancestor/child cases,
  reusing `09-extension-reuse.md`'s existing `Characteristic`/mismatch/
  extension-chain machinery) rather than an ordinary `VariableSpec`-based
  connection the way `08-graph-builder.md`'s existing `resolve_one` path
  handles other items - a reserved-name `VariableSpec` for this purpose
  was tried first and abandoned, since `ComponentSpec%var_specs` also
  feeds legacy `StateRegistry%add_to_states`, which would have leaked the
  geometry proxy into real user-facing states (REQ-GEO-003 violation).
  Entirely gated behind a new global toggle, `mapl_GraphMode_mod`'s
  `graph_native_enabled()` (added mid-implementation, general
  infrastructure beyond this sub-change's own scope - see §20.4.3's own
  discussion below and that change's design.md Decision D6), default off.
  **Explicit deferral, as stated in this sub-change's own proposal.md:**
  REQ-GEO-002a (exchange-component geometry, e.g. `SURF`-style
  multi-source `XGrid`) and all of §13.4 (time-dependent geometry renewal
  under freeze) are out of scope — static geometry only. Depended on
  Phase 1–3 only, as planned.
- **4f. VerticalGrid model** (`13` §13.3) —
  **landed** (`openspec/changes/vertical-grid-graph-state-item`):
  `VerticalGrid` as an `esmf_state`-kind `GraphStateItem` with
  `variant() == MAPL_STATEITEM_VERTICALGRID` (REQ-GEO-009),
  physical-dimension-keyed coordinate sets (REQ-GEO-004/004a), and the
  dimension-adaptability check for mismatched vertical grids
  (REQ-GEO-007a). **Significant deviation from this entry's original
  wording, discovered during implementation:** the original plan (wrap
  `OuterMetaComponent%get_vertical_grid()`/legacy `ModelVerticalGrid%
  get_coordinate_field()`, mirroring 4e's `run_geometry_hook` shape) was
  abandoned after tracing `get_coordinate_field()` into
  `StateRegistry%extend()`, which can mutate the registry (build a real
  `ESMF_GridComp` coupler) as a side effect — disallowed by explicit
  project direction: **the graph solution must not use `StateRegistry`**;
  legacy's aspect/extension machinery is at most suggestive of needed
  capability, never something the graph code calls into. The landed
  design instead reuses the already-shipped, zero-`StateRegistry`
  `composite-state-spec` mechanism directly: a component declares its
  vertical grid as an ordinary composite `VariableSpec` (`declare_member`,
  one member per physical dimension), tagged via a new
  `VariableSpec%state_item_variant` field (mirrors the existing
  `callback_interface_id` field's "mark the item, not a new itemType"
  precedent); `CompositeStateMaterialization` gained one line to apply
  the tag via the existing `set_variant`. REQ-GEO-007a's classification
  lives inside the **existing** `VerticalGridCharacteristic` (not a new
  characteristic kind — a first implementation added a separate
  `VerticalGridMembershipCharacteristic`, reverted after review: two
  "vertical grid" characteristics was confusing, and matching by
  dimension-name overlap with no identity-token fallback at all was a
  real false-positive risk, not merely a documented limitation).
  `VerticalGridCharacteristic` instead gained an optional declared
  physical-dimension set alongside its existing opaque identity token —
  identity-token comparison remains authoritative when both sides have
  one (unchanged ordinary-Field behavior); dimension-set equality is the
  fallback, used only when no identity token exists (always true for the
  composite case). Because `get_supported_physical_dimensions()` is a
  pure accessor, dimensions are populated for the ordinary-Field path
  too, so the three-way diagnostic applies uniformly — one pre-existing
  test's expected failure message was updated accordingly. Exercised
  through the *existing*, unmodified `resolve_one`/`build_characteristics`
  path — **no new `GraphBuilder.F90` hook was needed**, unlike 4e's own
  `run_geometry_hook`; no new toggle was needed either, since the new
  field is inert for every declaration that does not opt in. REQ-GEO-007's
  superseded-text `ReferenceCharacteristic`
  link back to horizontal geometry is **not implemented as such** —
  `GraphStateItem` has no persisted characteristics map
  (`18-state-item-characteristics.md` REQ-CHAR-007, Phase 5) — the
  association is instead a structural fact (a coordinate-set item and its
  component's own 4e geometry item always share one `ComponentGraph`).
  Depended on 4e only, as planned; REQ-GEO-005's reserved
  `MAPL_VerticalGrids` materialization, REQ-GEO-007b's general
  multi-candidate-import case, real vertical-regrid execution, and §13.4
  remain explicit deferrals.
- **4g. RouteHandleValue/Key** (`14`) — **landed:** `RouteHandleKey`
  structure (REQ-RH-002/003) and reuse through the existing generic semantic
  index (REQ-RH-004/005), rather than a dedicated gFTL map. No
  `RegridTransform` or `GraphBuilder.F90` wiring was added; real regrid
  execution remains future work. **Explicit deferral:** §14.4 time-dependent
  renewal remains out of scope — reuse-of-existing-handle case only. Depends
  on 4e (needs geometry identity to populate `RouteHandleKey`).

**Repo/tooling note (extends §20.4.1's own note).** Phase 4 code lives
in the MAPL repo/checkout, same as Phase 3, for the same reason: real
`GriddedComponentDriver`/`OuterComponent`/ESMF context is required from
4b onward regardless of repo layout.

### 20.4.4 Phase 5 sub-sequencing

Phase 5, like Phase 3 (§20.4.1) and Phase 4 (§20.4.3), does not fit a
single spec-driven change proposal without an unreasonable context/cost
footprint — it bundles three genuinely independent, speculative/deferred
items (`18` StateItemCharacteristic hierarchy, `16` ordinary inout items,
Q9 compiled-execution optimization), each carrying its own explicit
"do not implement without further design" caveat. Split into sub-changes
as follows, rather than filing one proposal spanning all three.

- **5a. StateItemCharacteristic hierarchy** (`18` §18.2–§18.8) — ready to
  scope now. `18`'s header caveat was tied to Q11, which
  `17-open-questions.md` already records as resolved; what remains are
  ordinary `[OPEN]` naming/mechanism points, not a precondition on the
  order of `16`'s REQ-INOUT-002. This sub-change's own design.md MUST
  resolve, as a planned, up-front design decision before implementation
  starts (same discipline Phase 4's 4b followed for REQ-MTH-011 step (c)):
  `CharacteristicStatus` naming (§18.3), `CharacteristicType` naming
  (§18.4), the ordering-delegation mechanism for chaining mismatch
  Transforms (§18.6, Q13), and the absent-key-vs-`INVALID`
  `characteristics`-map representation (§18.5). Depends only on Phases
  1–3 (`GraphStateItem`, `09-extension-reuse.md`'s existing
  chain-building machinery) — no dependency on `16` or Q9.
- **5a2. Remaining StateItemCharacteristic subclasses** — discovered
  during 5a's own code review (`openspec/changes/
  state-item-characteristics`), not scoped by 5a itself: REQ-CHAR-001's
  table lists only three subclasses and explicitly says so ("almost
  certainly incomplete... treat the list as a starting point, not a
  closed set"), and 5a implemented exactly those three
  (`PhysicalUnitsCharacteristic`, `TypeKindCharacteristic`,
  `GeometryCharacteristic`). But legacy's own `StateItemAspect` hierarchy
  (`superstructure/generic/specs/*Aspect.F90`, `AspectId.F90`) already
  has ten concrete mismatch-detectable axes, not three — `GeomAspect`,
  `UnitsAspect`, `TypekindAspect` (5a's three analogs) plus seven more:
  `VerticalGridAspect`, `AttributesAspect`, `UngriddedDimsAspect`,
  `QuantityTypeAspect`, `ConservationAspect`, `NormalizationAspect`,
  `StandardNameAspect`. Per this document's own "roughly 1-to-1
  correspondence, with exceptions" expectation (confirmed during 5a's
  review), each of those seven needs a `StateItemCharacteristic` analog
  before the graph-native path can claim parity with legacy's own
  mismatch-detection surface — not attempted by 5a, which deliberately
  scoped to the three REQ-CHAR-001 names only. Numbering mirrors this
  project's own established precedent for a review/implementation-
  discovered, required-completion follow-up (3b/3b2, 4b/4b2) — note the
  same distinction already drawn for `5b2`: this is a *parity gap*
  discovered against an existing legacy surface, not a brand-new
  requirement.

  **Scope, one new type per legacy `*Aspect` (name chosen to avoid
  colliding with `graph/extension-reuse`'s own, deliberately independent
  `Characteristic` family — the `GeometryCharacteristic`-not-
  `GeomCharacteristic` precedent, 5a design.md D3 — applies again here
  wherever a name would otherwise collide):**

  | Legacy `AspectId` | Legacy `*Aspect` | New `StateItemCharacteristic` |
  |---|---|---|
  | `VERTICAL_GRID_ASPECT_ID` | `VerticalGridAspect` | a vertical-grid `ReferenceCharacteristic` (name TBD at design time; `graph/extension-reuse` already has an unrelated `VerticalGridCharacteristic` — collision, needs a different name) — REQ-CHAR-012's own text already anticipates this one as "expected to be common," alongside `GeomCharacteristic` |
  | `ATTRIBUTES_ASPECT_ID` | `AttributesAspect` | `AttributesCharacteristic` (`ValueCharacteristic`) |
  | `UNGRIDDED_DIMS_ASPECT_ID` | `UngriddedDimsAspect` | `UngriddedDimsCharacteristic` (`ValueCharacteristic`) |
  | `QUANTITY_TYPE_ASPECT_ID` | `QuantityTypeAspect` | `QuantityTypeCharacteristic` (`ValueCharacteristic`) |
  | `CONSERVATION_ASPECT_ID` | `ConservationAspect` | `ConservationCharacteristic` (`ValueCharacteristic`) — legacy's own `make_transform` is an unconditional `_FAIL("should not be called")`, i.e. this axis is detected but never itself adapted; the graph-native analog's `build_transform` MAY do the same |
  | `NORMALIZATION_ASPECT_ID` | `NormalizationAspect` | `NormalizationCharacteristic` (`ValueCharacteristic`) |
  | `STANDARD_NAME_ASPECT_ID` | `StandardNameAspect` | `StandardNameCharacteristic` (`ValueCharacteristic`) |

  Explicitly out of scope for 5a2, same as 5a: no `GraphBuilder` rewire
  (these remain additive, standalone types exercised by synthetic-node
  pFUnit coverage only, mirroring 5a's own design.md D6); real
  `build_transform` implementations beyond whatever each legacy
  `*Aspect%make_transform` already does unconditionally (most either
  fail explicitly or have no real adaptation logic today — only
  `UnitsAspect`/`GeomAspect`/`TypekindAspect`, 5a's own three, have any
  real executing transform in legacy either, via
  `superstructure/generic/transforms/`). Legacy's `AspectStatus` has a
  sixth value, `FROM_COMP`, that `CharacteristicStatus` (5a, REQ-CHAR-003)
  does not — 5a2's own design.md MUST decide, as a planned, up-front
  decision, whether that is a real gap to close or a legacy-only
  distinction with no graph-native equivalent needed, rather than
  discovering it mid-implementation.

  Depends on 5a (the `StateItemCharacteristic`/`ValueCharacteristic`/
  `ReferenceCharacteristic` hierarchy, `CharacteristicStatus`,
  `StateItemCharacteristicKind`, the sparse map on `GraphStateItem`) —
  not on `16` or Q9. MAY be filed any time after 5a lands.
- **5b. Ordinary inout, direct-alias case** (`16` REQ-INOUT-001 only) —
  **landed** (`openspec/changes/archive/ordinary-inout-direct-alias`): a
  destination item declared an ordinary inout borrower
  (`VariableSpec%is_inout_borrower`) is wired by `GraphBuilder` through
  the same REQ-EXT-003 no-op comparison the ordinary (non-inout) path
  already uses for the forward edge (owner -> borrower), plus a return
  edge (borrower -> owner) in a fresh per-pairing `DependencyNetwork` -
  no new Transform in either direction, as REQ-INOUT-001 requires.
  Mismatched payload, a borrower with no identifiable owner, and
  chained/recursive borrowing are all rejected explicitly (reported, not
  silently extension-chained) rather than attempting anything
  REQ-INOUT-002 reserves. **Explicit deferral, as stated in this
  sub-change's own design.md:** the runtime return-edge propagation
  trigger (§16.1's "after borrower execution" step) is not implemented -
  `ComponentGraph`'s demand-driven `update()` only does active work for a
  `TransformGraphNode` frame (confirmed by reading
  `ComponentGraph_DemandDrivenUpdate.F90` directly), and the real trigger
  event (the borrower's own GridComp run / `MethodGraphNode` invocation
  completing) has no invocation-completion hook in the codebase yet for
  ordinary (non-callback) components - building one is Phase 4
  invocation-lifecycle work, surfaced but not attempted by this narrow
  sub-change. This change lands the forward/return graph structure only,
  mirroring 4e/4g's own "structure now, real execution later" precedent.
  Depended on Phases 1-3 only, as planned; independent of 5a/5c.
- **5b2. Ordinary inout, general case** — **BLOCKED**, not ready to
  scope. REQ-INOUT-002 explicitly requires an explicit design addendum
  resolving four named open points before any non-trivial inout support
  may be implemented: revision authority under two producers (an inout
  owner value is both a forward-network source and a return-network
  target — which write wins is unresolved), lazy direction selection,
  recursion when a borrower is itself an owner, and authority rules in
  general for who may initiate a forward/return cycle. Numbering mirrors
  the project's existing precedent for a closely related follow-up
  sub-change (3b/3b2, 4b/4b2) — but note the distinction explicitly:
  those were follow-ups *discovered* during their parent's own
  implementation, whereas `5b2` is a *pre-existing*, spec-declared
  blocker, known before `5b` is even filed. Do not file `5b2`'s own
  `openspec` proposal until the REQ-INOUT-002 addendum exists.
- **5c. Compiled-execution optimization** (Q9) — ready to scope now,
  following Q9's own recommended approach: walk a *frozen*
  `ComponentGraph`, emit a direct call sequence per `DependencyNetwork`
  equivalent to the interpreted `update()` traversal (REQ-REV-006),
  exploiting the stated low fan-out (0–3) to avoid dynamic
  dispatch/lookup at runtime, and keep the interpreted path alive as a
  reference oracle (REQ-REV-009) rather than deleting it once compilation
  exists. Q9's own gate — "the reference (interpreted) implementation
  being correct and validated first" — is already satisfied: the
  interpreted implementation Q9 would compile (frozen `ComponentGraph`,
  `DependencyNetwork` walk, demand-driven `update()` from Phase 1–2;
  `MethodGraphNode` invocation, callback wiring, route handles from Phase
  3–4) is landed and exercised by each sub-change's own pFUnit suite.
  REQ-REV-009's "validated" does not require the stronger Phase 6 entry
  bar (legacy `StateRegistry` retired at production scale) — Q9 only
  needs the interpreted path to be the trusted oracle, which this
  document's own Phase 6 framing (§20.4.2) already assumes stays alive
  indefinitely, not something gated on legacy removal. Independent of
  `16`/`18` in principle — it compiles whatever `DependencyNetwork`/
  `TransformGraphNode` structure exists at freeze time, generically — but
  sequence it **after 5a**: if `18` lands first, its characteristic-driven
  `TransformGraphNode`s (`ConvertUnitsTransform`, the precision-conversion
  transform, `RegridTransform`) are already part of what 5c needs to
  compile correctly, avoiding a revisit once 5a's new Transform chains
  exist. `5b`'s direct-alias case adds no new Transform, so 5c has no
  ordering dependency on `5b`.

**Resulting order and independence:**

```
Phase 1-4 (landed)
   |
   +--> 5a  StateItemCharacteristic hierarchy        (ready now)
   |       |
   |       +--> 5a2  Remaining Characteristic subclasses  (ready now, after 5a)
   |       |
   |       v
   +--> 5c  Compiled-execution optimization          (ready now, after 5a)
   |
   +--> 5b  Ordinary inout, direct-alias case         (landed, independent)
             |
             v
         5b2 Ordinary inout, general case             (BLOCKED: needs REQ-INOUT-002 addendum)
```

`5a` and `5b` have no dependency on each other and MAY be filed in either
order or in parallel. `5a2` depends only on `5a` (not on `5c`/`5b`/`5b2`)
and MAY be filed any time after `5a` lands, independently of `5c`. `5c`
should follow `5a` (not `5a2` — `5c` compiles whatever Transform chains
exist at freeze time, generically, so it has no ordering dependency on
`5a2`'s own, mostly-non-executing `build_transform`s either way).
`5b2` is not filed until its design addendum exists.

**Repo/tooling note (extends §20.4.1's/§20.4.3's own notes).** Phase 5
code, for whichever sub-change is filed, lives in the MAPL repo/checkout,
same as Phase 3 and Phase 4: `5a`/`5b`/`5c` all build on landed Phase 1–4
MAPL-repo code (`GraphStateItem`, `ComponentGraph`, `DependencyNetwork`,
`MethodGraphNode`, callback wiring, route handles), so no
repo-separation saving (§20.2) applies here either.

### 20.4.5 Phase 7 sub-sequencing

**Discovered gap, not previously tracked by any phase above.** Surfaced
2026-10-08 during review of `5a2`
(`openspec/changes/state-item-characteristic-subclasses`): legacy
`ClassAspect` (`superstructure/generic/specs/ClassAspect.F90`) is the
*shape*-dispatch slot in `VariableSpec`'s `AspectMap` — orthogonal to
every per-characteristic aspect (units, geometry, vertical grid, ...)
`5a`/`5a2` already have graph-native analogs for. `ClassAspect` decides
which other aspects are even relevant (`get_aspect_order`), owns the
actual ESMF payload (`create`/`activate`/`allocate`/`destroy`), and — for
three concrete subclasses — performs a real *shape-changing* conversion
via `make_transform`, something no other legacy aspect does and nothing
in the graph architecture has any analog of today:

| Legacy pair | Transform | Cardinality |
|---|---|---|
| `BracketClassAspect` -> `FieldClassAspect` | `TimeInterpolateTransform` | 1:1 |
| `VectorBracketClassAspect` -> `VectorClassAspect` | `TimeInterpolateTransform` | 1:1 |
| `ExpressionClassAspect` -> `FieldClassAspect` | `EvalTransform` | N:1 (named refs) |

A fourth and fifth legacy mechanism — `WildcardClassAspect` and
`ServiceClassAspect`'s own accumulation role — are *not* shape-conversion
at all and do not belong in that table: both have dead/unreachable
`make_transform` (`matches`/`supports_conversion_*` hard-`.false.`) and
are resolved through a `connect_to_export` side-channel fed by
`MatchConnection` regex-expanding one import pattern against the
flattened sibling export namespace *before* any `AspectMap`-level
dispatch runs — legacy itself does not treat these two uniformly with
the rest of `StateItemAspect`, so graph needing separate machinery for
them is not a new asymmetry; it is the same one legacy already has.

`GraphBuilder.F90` already has an explicit, pre-existing scope boundary
for the pattern-based case: "Wildcard/callback/reexport/simple
connections: out of scope for this slice — deliberately skipped." This
was never promoted to a tracked roadmap gap; this phase is that
promotion, done accurately (§20.4.2's own Phase 6 cleanup list names
`WILDCARD`/`EXPRESSION`/`SERVICE` as missing graph-native *itemType
values* but says nothing about missing *dispatch behavior*, and does not
mention `BRACKET`/`VECTOR`/`VECTORBRACKET` at all — that list entry is
itself incomplete, left as-is rather than rewritten, per this document's
own "append, don't silently drop" discipline for growable lists).

**Does not belong in Phase 5's `StateItemCharacteristic` family
(`5a`/`5a2`)**, despite the naming proximity — two reasons: (1) `5a`
design.md D6 deliberately keeps that whole hierarchy independent of
`GraphBuilder.F90`'s real connection-resolution path, exercised only by
synthetic-node pFUnit tests; shape-dispatch, to be useful, MUST be wired
into real resolution. (2) The architecturally correct home is the
*other*, already-live graph-native characteristic family —
`09-extension-reuse.md`'s `Characteristic`/`CharacteristicMap`/
`CharacteristicId` (3c), already consulted by `GraphBuilder.F90`'s real
`resolve_one`/`ExtensionResolution.F90` chain-building
(`find_mismatched_characteristics`/`build_chain`) for Units and
VerticalGrid today. A new `CharacteristicId` whose `needs_extension_for`
checks `GraphStateItem%variant()` compatibility and whose
`build_transform` returns the Bracket/VectorBracket-equivalent
`TransformGraphNode` would be walked by the *same* `build_chain` loop,
uniformly with Units — preserving exactly the uniformity property
legacy's own `can_connect_to`/`make_extension` loop has for `ClassAspect`.
`variant()` is already a first-class `GraphStateItem` property
(REQ-SI-002b), not a sparse-map characteristic entry, which if anything
mirrors `ClassAspect`'s own privileged (decides-aspect-order,
owns-the-payload) role in legacy more faithfully than modeling shape as
an ordinary characteristic would.

**Three genuinely distinct mechanisms, not one** — do not file a single
change spanning all three:

- **7a. Shape-changing 1:1 dispatch** (`Bracket`->`Field`,
  `VectorBracket`->`Vector`) — mechanically the simplest: a new
  `CharacteristicId`/`Characteristic` subclass in the live
  `graph/extension-reuse` family, triggered by `variant()` mismatch
  instead of a value comparison, dispatched by the existing `build_chain`
  loop exactly like `UnitsCharacteristic` is today. **Lowest urgency**:
  zero `Test_Scenarios` fixtures exercise `BRACKET`/`VECTORBRACKET` end
  to end (`Test_BracketClassAspect.pf`/`Test_VectorBracketClassAspect.pf`
  are unit-level only, `MAPL.generic.aspects`) — nothing currently forces
  this before the other two.
- **7b. Expression -> Field, N:1** — needs `EXPRESSION` added to the
  graph-native `MAPL_StateItem_Flag` vocabulary (currently absent, unlike
  `BRACKET`/`VECTOR`/`VECTORBRACKET` which already have flag values) and
  a `GraphBuilder`-level hook resolving N named references —
  `build_transform`'s two-item `(src, dst)` signature cannot do this
  alone, mirroring legacy's own exception for
  `ExpressionClassAspect%make_transform` (it already reaches outside
  normal scope via `registry%extend()` per referenced variable). Likely
  shaped as a dedicated hook analogous to `4e`'s own `run_geometry_hook`
  precedent (graph-wide resolution, not an ordinary two-item
  `Characteristic`) rather than an extension of 7a's mechanism.
  **Working hypothesis, not yet designed**: an Expression item is likely
  best modeled as a composite `StateItemNode` (reusing the already-shipped
  `composite-state-spec` `declare_member` mechanism — the same mechanism
  `4c`'s `CallbackStateBinding` and `4f`'s `VerticalGrid` already reuse
  for "a state item needs independently-addressable sub-structure" — one
  member per referenced variable), with `TransformGraphNode`'s existing
  N-input port support (already generic since Phase 2,
  `10-transforms-and-ports.md`) doing the N:1 fan-in from those members.
  Needs real design work before implementation, not assumed solved here.
  `Test_Scenarios` evidence: 3 fixtures (`expression`, `expression_match`,
  `expression_defer_geom`) — a real forcing function.
- **7c. Wildcard/Service pattern-based multi-source fan-in** — hardest
  and least understood. Not a `Characteristic`/shape-dispatch problem at
  all: legacy's own mechanism is connection-*topology* discovery (regex
  against the flattened sibling export namespace,
  `VirtualConnectionPt%matches`/`MatchConnection`) happening *before* any
  per-item dispatch, followed by N-way accumulation into one persistent
  destination instance (`WildcardClassAspect%matched_items`).
  `DependencyNetwork%validate()` already explicitly rejects multiple
  producers writing one state item in the same network — that invariant
  is almost certainly correct and should not be weakened to accommodate
  this. Working hypothesis: model each pattern match as its own
  independently-produced composite *member* (reusing
  `composite-state-spec` again) rather than true multi-producer
  semantics — not yet validated against the real sharing/identity
  mechanics `18-state-item-characteristics.md` §18.7 already uses for
  Geom/VerticalGrid. Further complicated by cross-level export
  visibility: `openspec/changes/archive/
  2026-09-16-graphbuilder-advertising-connections`'s own
  `Test_GraphBuilderEquivalence.pf` already tried and rejected porting
  `statistics`/`history_1`/`history_wildcard`/`extdata_1` as equivalence
  targets, specifically because those scenarios need `StateRegistry`'s
  cross-level `propagate_exports`/`propagate_unsatisfied_imports`
  machinery that `GraphBuilder` does not consult beyond `3b2`'s single-
  import upward bubbling — a second, only-partially-solved prerequisite,
  not something 7c can assume away. `Test_Scenarios` evidence: 1 fixture
  for Wildcard (`history_wildcard`), 2 (+1 unused) for Service — real but
  entangled forcing functions.

**Recommended pilot, if/when this phase is picked up**: `vector_1`
(`VectorClassAspect`, same-shape, no conversion needed at all —
`matches` with per-component `standard_name` comparison only) as a smoke
test of whether `GraphBuilder`'s existing extension-chain model already
generalizes to a second characteristic cleanly, *before* attempting
`7a`'s genuine shape-conversion case or `7c`'s harder topology problem.
`Vector` itself needs no new mechanism beyond what `3c`/`5a2` already
provide — it is scoping/validation work, not new architecture, and tests
the ground floor before building the harder floors on top of it.

**Repo/tooling note (extends §20.4.1's/§20.4.3's/§20.4.4's own notes).**
Phase 7 lives in the MAPL repo/checkout, same as Phases 3-5: all three
sub-mechanisms build on landed `GraphBuilder.F90`/`graph/extension-reuse`/
`composite-state-spec` MAPL-repo code.

### 20.4.6 Phase 8 scope: `VariableSpec` validation

**Discovered gap, not previously tracked by any phase above.** Surfaced
2026-10-09 during post-archive review of `ordinary-inout-direct-alias`
(`openspec/changes/archive/2026-10-09-ordinary-inout-direct-alias`): a
reviewer asked where the new callback+inout mutual-exclusion guard
"should" live, and tracing the actual call graph showed every
`VariableSpec` consistency concern — not just this one — has nowhere
principled to live today.

**What exists:**
- `verify_variable_spec` (`VariableSpec.F90`) already checks
  `state_intent`/`short_name`/`regrid` consistency via
  `VariableSpec_private.F90`'s `verify_state_intent`/`verify_short_name`/
  `verify_regrid` - a real, if incomplete, validation scaffold.
- It is module-`private`, absent from `mapl_VariableSpec_mod`'s own
  `public ::` list, and has zero call sites anywhere in the repo,
  including from `make_VariableSpec` itself. Dead code since it was
  written.

**What this means for the three already-landed marker fields:**
`callback_interface_id`, `state_item_variant`, and `is_inout_borrower`
each followed the same precedent ("mark the item, not a new itemType,"
set via plain post-construction field assignment, no `make_VariableSpec`
keyword, explicitly deferring YAML/SetServices exposure as out of scope
in each one's own design.md). Confirmed by direct search: every
assignment of any of the three, anywhere in the repo, is in a pFUnit test
file (`Test_VariableSpecCallback.pf`, `Test_GraphGeometryHook.pf`,
`Test_GraphBuilder.pf`, `Test_VariableSpecInout.pf`). Neither real
production construction path exposes them:
- `MAPL_GridCompAddSpec`/`gridcomp_add_spec` (`MAPL_Generic.F90`) - the
  public macro real GEOS components call (`gridcomps/statistics/*.F90`
  and others) - builds a `VariableSpec` from its own keyword-argument
  list, which does not include any of the three, then pushes directly
  into `component_spec%var_specs` (bypassing `ComponentSpec%add_var_spec`).
- YAML parsing (`ComponentSpecParser/parse_var_specs.F90`) - same shape:
  builds via `make_VariableSpec(...)` with its own keyword list (also
  missing all three), pushes directly into the vector.

Each deferral was individually deliberate and documented at the time -
this phase is the first point anyone asked whether the accumulation of
three such deferrals, with no shared validation or exposure mechanism
among them, is itself a gap. It is: `ordinary-inout-direct-alias`'s own
mutual-exclusion guard is reachable only from `GraphBuilder.F90`'s
connection-resolution path, meaning a component that declares the
nonsensical combination but never gets a matching connection resolved
against it sails through completely unchecked today.

**Scope for this phase:**
1. Make `verify_variable_spec` real: export it (or an equivalent single
   choke point), and have it also check the cross-field consistency
   rules current code only checks piecemeal or not at all (starting with
   `ordinary-inout-direct-alias`'s callback+inout mutual exclusion -
   `GraphBuilder.F90`'s own copy should be removed once this lands, not
   kept as a redundant second check).
2. Decide, as a planned up-front design decision (same discipline this
   document already asks of other open points, e.g. Phase 4's 4b for
   REQ-MTH-011(c)): does validation run at `make_VariableSpec`'s own
   construction time (requires migrating `callback_interface_id`/
   `state_item_variant`/`is_inout_borrower` from post-construction plain
   assignment to real constructor keyword arguments - the more invasive,
   more principled option), or at both real production choke points
   (`gridcomp_add_spec` and `parse_var_specs.F90`'s per-item construction
   loop - less invasive, but two call sites to keep in sync, and still
   does nothing for construction via direct field assignment in test or
   future code that does not go through either)?
3. Whichever mechanism is chosen, decide whether `callback_interface_id`/
   `state_item_variant`/`is_inout_borrower` should finally get real
   production exposure (keyword args on `MAPL_GridCompAddSpec` and/or new
   YAML fields) as part of this phase, or whether validation should ship
   first while production exposure for each field remains its own,
   separately-scoped follow-up per capability.

**Not blocking anything above or already landed** - this is independent
cleanup/hardening work on construction-time correctness, not new graph
capability; it can be picked up at any time.

## 20.5 Cross-reference

This roadmap does not restate or supersede any requirement in `01`–`19`;
it only sequences them. If a phase boundary above turns out to be wrong
once implementation starts (e.g. a Phase 4 item turns out to be needed
earlier), update this document rather than letting the plan silently
diverge from practice — same discipline as the rest of this
specification.
