## Why

`docs/graph/spec/20-implementation-roadmap.md` §20.4.3 identifies Phase
4b as the second Phase 4 sub-change: `GriddedComponentDriver`
integration + init lifecycle (`12-methods-and-drivers.md`
REQ-MTH-007..013). Phase 4a (`method-graph-node`, archived) delivered
`MethodGraphNode` and its two invocation adapters
(`GridCompMethodInvocation`/`StateMethodInvocation`), but deliberately
stopped at synthetic test-double `GridCompPhaseInvoker`/
`StateMethodInvoker` implementations — both adapter modules' own header
comments name the three things Phase 4b must supply: a real
`GriddedComponentDriver`-backed `GridCompPhaseInvoker`, the real
`driver_key -> GriddedComponentDriver` resolution mechanism
(REQ-MTH-009), and the REQ-MTH-003a trigger/advance-revision discipline
around invocation (owned by `OuterMetaComponent`, REQ-REV-011a). Without
this, no `MethodGraphNode` can ever invoke a real GridComp phase — Phase
4a's node type has nothing to invoke through.

This is also the first Phase 4 sub-change requiring real
`GriddedComponentDriver`/ESMF context (`GriddedComponentDriver.F90`
already exists as pre-graph, legacy-shaped code — REQ-MTH-007's driver
already wraps GridComp/Import/Export/Internal/Clock exactly as
specified; `OuterMetaComponent` already owns one driver for its own
user component (`user_gc_driver`) and one per child
(`children: GriddedComponentDriverMap`), satisfying REQ-MTH-008's
ownership shape structurally, before any graph-native code existed).
The job here is to make `MethodGraphNode` invocation reach that
existing ownership through a stable identifier rather than a pointer,
and to fold the REQ-MTH-011 initialization phase ordering (already
implemented today as `OuterMetaComponent`'s
`initialize_advertise`/`initialize_modify_advertised`/
`initialize_realize_provided`/`initialize_accept_transfer`/
`initialize_realize_accepted`/`initialize_read_restart`/
`initialize_user` sequence, entirely `StateRegistry`-driven and
graph-unaware today) around the graph-native trigger/advance discipline
without displacing that existing sequence.

REQ-MTH-011 step (c)'s convergence algorithm for the
`realize_provided`/`accept_transfer`/`realize_accepted` cycle is
explicitly `[OPEN]` in the spec. This sub-change's design.md MUST
resolve it as a planned, up-front decision before implementation
starts — the spec explicitly calls out not repeating 3b's experience,
where real-configuration validation surfaced 3b2's cross-component
propagation requirement mid-implementation, unplanned.

## What Changes

- **Real `GridCompPhaseInvoker` implementation**
  (`superstructure/generic/graph/`, new module) backed by the existing
  `GriddedComponentDriver` (`superstructure/component/
  GriddedComponentDriver.F90`)'s own `initialize`/`run`/`finalize` entry
  points — satisfies REQ-MTH-003 ("must not duplicate invocation logic
  already in `GriddedComponentDriver`") by delegating, never
  reimplementing ESMF calling conventions.
- **`driver_key -> GriddedComponentDriver` resolution mechanism**
  (REQ-MTH-009): a stable local identifier resolvable within the owning
  `OuterMetaComponent` — resolves to `user_gc_driver` for the
  component's own driver, or a lookup into the existing
  `GriddedComponentDriverMap` (`children`) keyed by child name for a
  child's driver. `MethodGraphNode`/`GridCompMethodInvocation` continue
  to hold only the `character` key (already true since Phase 4a); no
  raw pointer or driver copy is ever held by graph-side code.
- **REQ-MTH-003a trigger/advance discipline**, implemented in
  `OuterMetaComponent` (REQ-REV-011a: this layer owns the discipline,
  not the invocation adapter) around each real phase invocation on the
  default `DependencyNetwork`: `ComponentGraph%update()` triggered over
  the method's bound `IN`/`INOUT` arguments immediately before
  invocation; `NodeRevision%advance()` called for the method's bound
  `OUT`/`INOUT` arguments immediately after invocation succeeds — both
  unconditional/all-or-nothing per REQ-REV-011's default-network
  assumption.
- **REQ-MTH-011 convergence algorithm — bounded to this sub-change's own
  scope, not closed for good.** For the
  `realize_provided`/`accept_transfer`/`realize_accepted` cycle,
  design.md makes a concrete, explicit decision: match the fixed
  two-pass schedule the real MAPL cap driver already hard-codes
  (`enums/GenericPhases.F90`'s `GENERIC_INIT_PHASE_SEQUENCE`), and add a
  hard-error check for a configuration that still has unresolved
  required imports once that fixed schedule ends, rather than only
  logging a warning as today. This is a deliberate, named limitation,
  not a general solution: a fixed two-pass schedule is known-inadequate
  for hybrid NUOPC coupling configurations that genuinely need more
  passes to converge. Real, general progress-based convergence
  detection remains open and is recorded in design.md's Open Questions
  with an explicit trigger for when it must be revisited — this
  sub-change does not claim to close REQ-MTH-011's `[OPEN]` item for
  good, only to give it a safe, honest, hard-failing interim behavior.
- **No change to the existing REQ-MTH-011 phase sequence's call sites**
  (`OuterMetaComponent`'s `initialize_advertise` through
  `initialize_user`, already implemented as the real MAPL init
  lifecycle): this change wires the graph-native trigger/advance
  discipline and driver resolution *into* the invocation path used when
  a `MethodGraphNode` fires, without renaming, reordering, or removing
  any existing phase entry point.

**Explicit deferrals** (later Phase 4 sub-changes' jobs, per
`20-implementation-roadmap.md` §20.4.3):

- `CallbackInterface`/`CallbackArgumentSpec`/`CallbackMethodSpec`/
  `CallbackStateBinding`/`CallbackInterfaceRegistry` and the real
  `StateMethodInvoker` (`ESMF_MethodExecute`-backed) — Phase 4c/4d.
  This change's `driver_key`/trigger-advance work is independent of
  those and does not block or get blocked by them.
- Horizontal geometry, vertical grids, `RouteHandleValue`/`Key` — Phase
  4e/4f/4g.
- Actually constructing real `MethodGraphNode`s for real component
  phases at advertise time (i.e., `GraphBuilder` wiring a
  `MethodGraphNode` per registered phase into a real
  `OuterMetaComponent`'s `local_graph`) is **out of scope** unless
  required to exercise this change's driver-resolution/trigger-advance
  code against something real — see design.md for the specific decision
  on how this change is tested without requiring that wiring to already
  exist. If real `MethodGraphNode` construction turns out to be a hard
  prerequisite for testing driver resolution meaningfully, design.md
  states that explicitly and scopes the minimal slice needed, still
  short of full `GraphBuilder` phase-registration wiring.
- The future annotation mechanism letting a method declare a narrower
  set of actually-touched imports/exports (noted in REQ-REV-011,
  REQ-MTH-003a) — not designed anywhere yet, out of scope here too.

## Capabilities

### New Capabilities
(none as a new top-level capability directory — this change extends the
existing `graph/method-graph-node` capability's requirements rather
than introducing an unrelated one.)

### Modified Capabilities
- `graph/method-graph-node`: adds requirements for real driver
  resolution (REQ-MTH-009), the real `GriddedComponentDriver`-backed
  `GridCompPhaseInvoker`, the REQ-MTH-003a trigger/advance discipline
  around invocation, and the REQ-MTH-011 lifecycle-ordering/convergence
  behavior — all previously explicit deferrals of Phase 4a's own
  delta spec.

## Impact

- **Affected code**:
  - New: a real `GridCompPhaseInvoker` implementation in
    `superstructure/generic/graph/` (backed by
    `GriddedComponentDriver`), plus its `CMakeLists.txt` entry.
  - Modified: `superstructure/generic/OuterMetaComponent.F90` and/or its
    submodule files (`initialize_advertise.F90`,
    `initialize_realize_provided.F90`, etc., and/or a new submodule) to
    own the trigger/advance discipline and driver-key resolution around
    `MethodGraphNode` invocation; `get_user_gc_driver.F90` and the
    existing `children: GriddedComponentDriverMap` accessor path are the
    likely resolution entry points (exact shape decided in design.md).
  - Not modified: `GriddedComponentDriver.F90` itself (REQ-MTH-007's
    driver already has the needed shape), `MethodGraphNode.F90`,
    `MethodInvocationAdapter.F90`, `GridCompMethodInvocation.F90`,
    `StateMethodInvocation.F90` (Phase 4a's node/adapter contracts are
    unchanged — only a concrete `GridCompPhaseInvoker` is added
    alongside them).
- **Dependencies**: depends on Phase 4a (`method-graph-node`, archived)
  for `MethodGraphNode`/`GridCompMethodInvocation`/
  `MethodInvocationAdapter`; depends on Phase 3a
  (`component-hierarchy-foundation`, archived) for `OuterMetaComponent`'s
  existing driver-ownership shape; depends on Phase 2's `NodeRevision`
  (`advance`) and `ComponentGraph%update()` for the trigger/advance
  discipline.
- **Tests**: unit tests exercising driver-key resolution (own driver vs.
  child driver, unresolvable key fails loudly), the real
  `GriddedComponentDriver`-backed `GridCompPhaseInvoker` delegating to
  `initialize`/`run`/`finalize` with no reimplemented ESMF logic, the
  trigger/advance discipline firing exactly once before/after a real
  invocation on bound `IN`/`INOUT`/`OUT` arguments, and the convergence
  algorithm's progress detection, iteration cap, and non-convergence
  error path (each exercised with synthetic graph data, per design.md).
- **Out of scope**: everything under "Explicit deferrals" above.
