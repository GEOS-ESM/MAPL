## Why

`docs/graph/spec/20-implementation-roadmap.md` §20.4.3 identifies Phase
4a as the first Phase 4 sub-change: `MethodGraphNode` + invocation
adapters (`12-methods-and-drivers.md` REQ-MTH-001/002/004/005/006), the
node type covering both GridComp phase invocation and attached-State
callback invocation through one invocation-adapter abstraction
(`17-open-questions.md` Q2). Phase 1-3 give the graph a value side
(`StateItemNode`/`GraphStateItem`) and one operation-node kind
(`TransformGraphNode`, demand-driven). There is still no node type
representing "an explicitly invoked method" at all — `03-graph-node-
hierarchy.md` REQ-NODE-006 names `MethodGraphNode` but nothing
implements it (`superstructure/generic/graph/OperationGraphNode.F90`'s
own header states this explicitly: "`TransformGraphNode`/
`MethodGraphNode` concrete subclasses are out of scope here"). Without
it, neither a GridComp phase nor a callback method has any graph-visible
representation, which blocks every later Phase 4 sub-change (4b's
`GriddedComponentDriver` integration, 4c/4d's callback data model and
wiring all depend on this node type existing first).

This is greenfield work (no existing `MethodGraphNode` code to extend)
on top of the completed, settled Phase 1-3 foundation. Per the roadmap,
it is scoped narrowly and kept synthetic-driver testable, the same
posture Phase 3a used for `OuterComponent`: no real
`GriddedComponentDriver` wiring, no real ESMF component, no
`GraphBuilder`/`ComponentGraph` wiring of argument bindings into a real
`DependencyNetwork`. Those are explicitly later sub-changes' jobs (4b,
4c, 4d), not this one's.

## What Changes

- **New `AccessSpec` concept** (`15-callbacks.md` REQ-CB-005: "a general
  MAPL concept, not callback-specific", values analogous to `IN`/`OUT`/
  `INOUT`/`UNSPECIFIED"). Introduced now, ahead of the callback data
  model (Phase 4c), because `MethodGraphNode`'s own argument
  declarations need it today (REQ-MTH-002 restates `15-callbacks.md`
  §15.4's `AccessSpec` at the graph level). Phase 4c's own
  `CallbackMethodSpec` (REQ-CB-004, "argument name -> `AccessSpec`")
  reuses this same type rather than duplicating it. Follows the existing
  `MAPL_StateItem_Flag` pattern (`MAPLStateItemFlag.F90`): a small
  derived type wrapping an integer code, compared via `operator(==)`/
  `operator(/=)`, not a plain integer.
- **New `ArgumentSpec`/`ArgumentSpecMap`**: a named argument declaration
  (name + `AccessSpec` + optional expected `MAPL_StateItem_Flag` kind
  constraint), mirroring `PortSpec`/`PortSpecMap`'s existing shape
  (`10-transforms-and-ports.md` REQ-XFORM-002/003) but keyed by
  `AccessSpec` instead of a separate input/output port direction — this
  is the graph-level "named argument bindings and `AccessSpec`s"
  REQ-MTH-002 describes as common to both call shapes, at the
  declaration level (mirrors `CallbackMethodSpec`'s own "argument name
  -> `AccessSpec`" shape, REQ-CB-004, ahead of that type's own Phase 4c
  introduction).
- **New `MethodGraphNode`** (`superstructure/generic/graph/
  MethodGraphNode.F90`), a concrete type extending `OperationGraphNode`
  (REQ-NODE-004/006), covering both call shapes through one held
  invocation adapter (REQ-MTH-001, REQ-MTH-002):
  - Declares named arguments via `declare_argument(name, access, rc,
    expected_kind)`/`is_argument`/`get_argument`/`get_arguments` —
    self-contained on the node, exactly like `TransformGraphNode`'s own
    port *declarations* are self-contained via its held `Transform`.
  - Holds argument *bindings* (name -> bound `NodeId`) on-node, reusing
    the existing `StateItemMemberMap` (character -> `NodeId`) type
    already used for `GraphStateItem`'s membership maps
    (REQ-SI-006) and `PortBindingTable`'s per-node port maps — no new
    map type needed for this. Kept on-node rather than routed through
    the existing external `PortBindingTable`/`ComponentGraph.bind_port()`
    mechanism (`10-transforms-and-ports.md` REQ-XFORM-005): unlike
    `TransformGraphNode`, `MethodGraphNode` is explicitly excluded from
    `ComponentGraph`'s demand-driven dispatch (REQ-NODE-007,
    already implemented today as a `class default` no-op branch in
    `mapl_ComponentGraph_DemandDrivenUpdate_smod`) and never needs that
    dispatch code to treat it uniformly with `TransformGraphNode`,
    removing the forcing function behind Q3's external-table
    generalization for this node kind. Wiring these bindings into a real
    `ComponentGraph`'s `DependencyNetwork` (actual predecessor/successor
    edges for a real component's phases) is explicit Phase 4b work, not
    this change's (see Impact/deferrals below).
  - `invoke(rc, clock)`: asserts an adapter is attached, gathers the
    current argument declarations/bindings, and calls the adapter's own
    `invoke()` — the node itself never touches ESMF calling conventions
    (REQ-MTH-003's constraint, satisfied by construction through this
    delegation boundary, even though `REQ-MTH-003a`'s actual trigger/
    advance-revision discipline around this call is Phase 4b's job, per
    `12-methods-and-drivers.md` §12.2's own attribution to
    `OuterMetaComponent`). `clock` is an plain, optional invocation-time
    argument, never a `NodeId`/graph dependency (REQ-MTH-006).
- **New `MethodInvocationAdapter`** abstract type (one deferred
  `invoke(this, arguments, bindings, clock, rc)`), plus two concrete
  adapters realizing REQ-MTH-001's two call shapes:
  - `GridCompMethodInvocation` — models a GridComp phase. Carries a
    driver identifier (`character`, honoring REQ-MTH-009's "stable
    local identifier, never a raw pointer to the driver" shape from the
    start, even though the actual identifier -> driver resolution
    mechanism is Phase 4b's job) and a phase name. Delegates to an
    injected abstract `GridCompPhaseInvoker` (one deferred
    `invoke_phase` method) rather than calling anything ESMF-specific
    itself — satisfies REQ-MTH-003 ("must not duplicate invocation logic
    already in `GriddedComponentDriver`") by construction: this change
    supplies only a synthetic test-double implementation; Phase 4b
    supplies the real one backed by the existing `GriddedComponentDriver`
    (`superstructure/component/GriddedComponentDriver.F90`).
  - `StateMethodInvocation` — models an attached-State callback method.
    Carries the callback State's `NodeId` and a method name. Delegates to
    an injected abstract `StateMethodInvoker` (one deferred
    `invoke_method` method); this change supplies only a synthetic
    test-double implementation, Phase 4c/4d supply the real
    `ESMF_MethodExecute`-backed one once `CallbackInterfaceRegistry`/
    `CallbackStateBinding` exist.
- **Restated, not implemented:** REQ-MTH-004 (import/export as one
  conceptual method-argument state — trivially true here, since argument
  bindings are a flat name -> `NodeId` map with no import/export
  sub-structure, and no new `ESMF_State` is ever created by declaring or
  binding an argument) and REQ-MTH-005/REQ-NODE-008 (no separate
  "component" graph node — nothing new is introduced that could serve as
  one; captured as a spec requirement so future code stays accountable
  to it, not as new code).

**Explicit deferrals** (later Phase 4 sub-changes' jobs, per
`20-implementation-roadmap.md` §20.4.3):

- Real `GriddedComponentDriver` wiring, the REQ-MTH-009 driver-identifier
  -> real-driver resolution mechanism, and the REQ-MTH-003a trigger/
  advance-revision discipline around `invoke()` (owned by
  `OuterMetaComponent`) — all Phase 4b ("`GriddedComponentDriver`
  integration + init lifecycle").
- `SetServices`/the full init-phase-list lifecycle (REQ-MTH-007..013) —
  Phase 4b.
- `CallbackInterface`/`CallbackArgumentSpec`/`CallbackMethodSpec`/
  `CallbackStateBinding`/`CallbackInterfaceRegistry` (`15-callbacks.md`
  §15.2-15.7) — Phase 4c. This change's `AccessSpec`/`ArgumentSpec` are
  deliberately generic so 4c can reuse them without rework, but 4c's own
  types are not built here.
- Wildcard/regex callback expansion, per-method get/put dependency
  networks, invoke-once-after-all-args-ready discipline (`15-callbacks.md`
  §15.9-15.10) — Phase 4d.
- Wiring `MethodGraphNode` argument bindings into a real `ComponentGraph`
  `DependencyNetwork` (actual predecessor/successor edges,
  `GraphBuilder` advertise-time creation of `MethodGraphNode`s for real
  component phases) — Phase 4b, since it requires the real driver/
  lifecycle context this change deliberately stays independent of.

## Capabilities

### New Capabilities
- `graph/method-graph-node`: `MethodGraphNode`'s node-level contract
  (one node type for both call shapes, named argument declarations with
  `AccessSpec`, on-node argument bindings, invocation-adapter
  delegation, Clock-as-context, no separate component node) plus the
  new, general-purpose `AccessSpec` concept it depends on.

### Modified Capabilities
(none — no existing capability's requirements change. `ComponentGraph`
is untouched: `MethodGraphNode` is registered and used exactly like any
other `GraphNode` subtype via existing, unmodified `ComponentGraph`
node-registration methods, and remains outside its demand-driven
dispatch, which already `class default`-skips unrecognized operation-node
kinds today with no code change needed.)

## Impact

- **Affected code (all new files, `superstructure/generic/graph/`)**:
  - `AccessSpec.F90` (`mapl_AccessSpec_mod`)
  - `ArgumentSpec.F90` (`mapl_ArgumentSpec_mod`)
  - `containers/ArgumentSpecMap.F90` (`mapl_ArgumentSpecMap_mod`, gFTL
    map, mirrors `containers/PortSpecMap.F90`)
  - `MethodInvocationAdapter.F90` (`mapl_MethodInvocationAdapter_mod`,
    abstract type + `GridCompPhaseInvoker`/`StateMethodInvoker` abstract
    injection interfaces)
  - `GridCompMethodInvocation.F90` (`mapl_GridCompMethodInvocation_mod`)
  - `StateMethodInvocation.F90` (`mapl_StateMethodInvocation_mod`)
  - `MethodGraphNode.F90` (`mapl_MethodGraphNode_mod`)
  - Corresponding `CMakeLists.txt` entries
    (`superstructure/generic/graph/CMakeLists.txt` and its `containers/`
    subdirectory listing).
- **No changes** to `ComponentGraph.F90`,
  `ComponentGraph_DemandDrivenUpdate.F90`, `GraphBuilder.F90`,
  `OuterMetaComponent.F90`, or `GriddedComponentDriver.F90` — this
  change is additive, new-module-only, exactly like Phase 3a
  (`component-hierarchy-foundation`) and Phase 2's `TransformGraphNode`
  introduction were.
- **Dependencies**: `MethodGraphNode`'s module depends on
  `OperationGraphNode`, `NodeId`, the new `AccessSpec`/`ArgumentSpec`/
  `ArgumentSpecMap` modules, `StateItemMemberMap` (reused, not
  duplicated), and `MethodInvocationAdapter`. `MethodInvocationAdapter`
  and its two concrete adapters do not depend on `MethodGraphNode`
  (decoupled, matching `Transform`/`TransformGraphNode`'s own
  one-directional dependency).
- **Tests**: new unit tests exercising `MethodGraphNode` construction
  with each adapter kind, argument declaration (including duplicate-name
  rejection and kind-constraint checks), argument binding, `invoke()`
  delegating to a synthetic `GridCompPhaseInvoker`/`StateMethodInvoker`
  test double with no other side effect, Clock passed through without
  being stored as a `NodeId`, and a regression scenario confirming
  `ComponentGraph.update()` never schedules a registered
  `MethodGraphNode` (REQ-NODE-007, exercising already-existing dispatch
  code against this new node kind).
- **Out of scope**: everything under "Explicit deferrals" above.
