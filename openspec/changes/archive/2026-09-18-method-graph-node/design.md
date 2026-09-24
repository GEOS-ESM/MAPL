## Context

Phase 1-3 delivered `BaseGraphNode`/`OperationGraphNode` (abstract,
`superstructure/generic/graph/OperationGraphNode.F90`, whose own header
comment defers `TransformGraphNode`/`MethodGraphNode` concrete
subclasses to later phases) and one concrete `OperationGraphNode`
descendant, `TransformGraphNode.F90` — demand-driven, holding one
`Transform` (`Transform.F90`) that declares named input/output ports via
`PortSpec`/`PortSpecMap` (`PortSpec.F90`/`containers/PortSpecMap.F90`).
Actual port *bindings* (which `NodeId` fills a declared port, for a
given `DependencyNetworkId`) live in a separate external table,
`PortBindingTable`/`ComponentGraph%bind_port()`
(`PortBindingTable.F90`, `ComponentGraph.F90:456-512`) — settled by
`17-open-questions.md` Q3 specifically to give every `DependencyNetwork`-
participating operation node one uniform binding-storage strategy,
avoiding an on-node/external branch selected per node kind.
`ComponentGraph`'s own demand-driven update
(`ComponentGraph_DemandDrivenUpdate.F90`) dispatches on node kind via
`select type`; its `class default` branch already documents "any other
`GraphNode` kind (e.g. a future `MethodGraphNode`): not demand-driven
per REQ-NODE-007 - intentionally not scheduled here" — i.e. today's code
already anticipates this node kind never appearing in that dispatch, and
requires no change to keep excluding it.

`docs/graph/spec/12-methods-and-drivers.md` is `[SETTLED]` on the
concept (`MethodGraphNode` covers both call shapes via an
invocation-adapter abstraction) and `[OPEN]` only on the adapter's exact
shape (§12.2, `17-open-questions.md` Q2). Q2's own recommendation gives
a concrete starting shape: the node holds (a) which adapter it uses, (b)
a stable driver identifier, (c) named argument bindings; the adapter's
`invoke()` does nothing but gather bound arguments and call one existing
driver/attachment entry point. REQ-MTH-009 (officially cited under
Phase 4b, `12-methods-and-drivers.md` §12.6) is explicit that this
identifier MUST be a stable key, never a raw pointer to the driver
object — a constraint cheap to honor structurally now even though the
actual identifier -> driver resolution registry does not exist until
Phase 4b.

## Goals / Non-Goals

**Goals:**

- Give `MethodGraphNode` a concrete shape that satisfies REQ-MTH-001,
  002, 004, 005, 006 and is unit-testable in complete isolation, with no
  `ComponentGraph`, no `GraphBuilder`, no ESMF component, and no real
  `GriddedComponentDriver` involved — matching Phase 3a's own posture
  for `OuterComponent`.
- Make the invocation-adapter boundary (REQ-MTH-003, Q2) real enough
  that Phase 4b/4c/4d can supply their respective real adapter payloads
  (`GriddedComponentDriver`-backed, `ESMF_MethodExecute`-backed) by
  implementing one small abstract interface each, without touching
  `MethodGraphNode` itself.
- Introduce `AccessSpec` as genuinely general (REQ-CB-005), so Phase 4c's
  `CallbackMethodSpec` can reuse it unchanged rather than this change
  accidentally producing a callback-specific type under a generic name.

**Non-Goals:**

- Wiring a `MethodGraphNode`'s argument bindings into a real
  `ComponentGraph`'s `DependencyNetwork` (actual `add_dependency` edges)
  or into the external `PortBindingTable`/`ComponentGraph%bind_port()`
  mechanism. See Decisions below for why this stays on-node for now.
- Resolving REQ-MTH-009's stable driver identifier against any real
  registry, or supplying a real `GriddedComponentDriver`-backed
  `GridCompPhaseInvoker`/`ESMF_MethodExecute`-backed `StateMethodInvoker`
  implementation. This change supplies the abstract interfaces and
  synthetic test doubles only.
- `SetServices`, the init-phase-list lifecycle (REQ-MTH-011), or the
  REQ-MTH-003a trigger/advance-revision discipline around `invoke()`
  (owned by `OuterMetaComponent`, per §12.2).
- `CallbackInterface`/`CallbackMethodSpec`/`CallbackStateBinding`/
  registry (Phase 4c) and wildcard/regex callback wiring (Phase 4d).
  `AccessSpec`/`ArgumentSpec` are built generically enough for 4c to
  reuse, but 4c's own types are not built here.

## Decisions

**`AccessSpec` follows the existing `MAPL_StateItem_Flag` pattern
exactly** (`MAPLStateItemFlag.F90`): a derived type wrapping a private
integer code, named `parameter` constants (`MAPL_ACCESS_IN`,
`MAPL_ACCESS_OUT`, `MAPL_ACCESS_INOUT`, `MAPL_ACCESS_UNSPECIFIED`),
`operator(==)`/`operator(/=)`, and a `to_string()` method. Alternative
considered: a plain `integer, parameter` enumeration. Rejected for the
same reason `MAPL_StateItem_Flag` itself rejected it — client code
should compare against named constants, not raw integers, so new access
modes (if ever needed) can be added without disturbing comparison call
sites.

**`ArgumentSpec`/`ArgumentSpecMap` mirror `PortSpec`/`PortSpecMap`
exactly, keyed by `AccessSpec` instead of input/output direction.**
`PortSpec` already has the right shape (name + optional expected-kind
constraint, `containers/PortSpecMap.F90`'s gFTL-map-in-its-own-module
pattern for the implicit-none/private conflict reason documented
there); `ArgumentSpec` adds exactly one field (`AccessSpec`) on top of
that same shape. Alternative considered: two separate maps ("input
arguments" / "output arguments"), mirroring `Transform`'s own
`input_ports`/`output_ports` split. Rejected: `AccessSpec` already
encodes direction (including the `INOUT`/`UNSPECIFIED` cases a two-map
split cannot represent without an arbitrary side-assignment), so a
single map keyed by name, each entry itself carrying its own
`AccessSpec`, is both sufficient and matches `CallbackMethodSpec`'s own
"argument name -> `AccessSpec`" shape (REQ-CB-004) that Phase 4c will
introduce — building `ArgumentSpecMap` as a two-map split now would mean
either 4c reworks it or duplicates a second, differently-shaped type.

**Argument bindings reuse the existing `StateItemMemberMap` (character
-> `NodeId`) unchanged, stored on `MethodGraphNode` itself, not routed
through `PortBindingTable`/`ComponentGraph%bind_port()`.** Two
alternatives considered:

1. *Extend `PortBindingTable`/`bind_port()`'s existing `select type` to
   also accept `MethodGraphNode`*, mirroring `TransformGraphNode`
   exactly and continuing Q3's "one uniform binding-storage strategy"
   precedent. Rejected for this change specifically because it requires
   touching `ComponentGraph.F90` (a Phase 1, settled file) and because
   the forcing function behind Q3's generalization — code that walks
   bindings needing to treat every operation-node kind uniformly during
   demand-driven dispatch — does not apply here: `MethodGraphNode` is
   permanently excluded from that dispatch (REQ-NODE-007), so there is
   no shared traversal code that would otherwise need an "is this
   on-node or external" branch. Revisit this decision in Phase 4b if
   real wiring turns out to need `ComponentGraph`-level binding storage
   after all (e.g. for visualization export parity with
   `TransformGraphNode`'s port bindings) — not precluded by anything
   here, just not built now.
2. *A new, `MethodGraphNode`-specific binding-map type.* Rejected: no
   new shape is needed — `StateItemMemberMap` is already exactly
   "character name -> `NodeId`", already used for two other member-map
   purposes (`GraphStateItem`'s membership maps, REQ-SI-006, and
   `PortBindingTable`'s own per-node port maps internally). Reusing it
   is a zero-cost, already-tested type.

**`MethodInvocationAdapter` is a decoupled abstract type — its module
does not depend on `mapl_MethodGraphNode_mod`.** Mirrors
`Transform`/`TransformGraphNode`'s existing one-directional dependency
(`Transform.F90` has zero knowledge of `TransformGraphNode`). The
adapter's `invoke(this, arguments, bindings, clock, rc)` interface takes
the argument declarations and bindings as plain values/maps, not a
reference back to the owning `MethodGraphNode` — an adapter never needs
anything else about the node (no `NodeId`, no lifecycle state), keeping
it as independently testable as `Transform` already is.

**Two concrete adapters, each delegating to its own small injected
abstract interface rather than calling anything ESMF-specific
directly**, satisfying REQ-MTH-003 by construction:

- `GridCompMethodInvocation` holds `driver_key` (`character(:),
  allocatable`, REQ-MTH-009's stable-identifier shape) and `phase_name`,
  and delegates to an injected `class(GridCompPhaseInvoker), allocatable`
  (one deferred `invoke_phase(this, driver_key, phase_name, arguments,
  bindings, clock, rc)`). This change's own tests supply a synthetic
  test-double `GridCompPhaseInvoker` that records its call and returns
  success; Phase 4b's job is to supply the real one, backed by the
  existing `GriddedComponentDriver`
  (`superstructure/component/GriddedComponentDriver.F90`)'s own
  `initialize`/`run`/`finalize` entry points, plus the real
  `driver_key` -> `GriddedComponentDriver` resolution mechanism
  (REQ-MTH-009's "resolvable within the owning `ComponentGraph`/
  `GraphBuilder` context").
- `StateMethodInvocation` holds the callback State's `NodeId` and
  `method_name`, and delegates to an injected `class(StateMethodInvoker),
  allocatable` (one deferred `invoke_method(this, state_node_id,
  method_name, arguments, bindings, rc)`, no clock — REQ-CB-001's
  callback State methods have no clock parameter). This change's tests
  supply a synthetic test double; Phase 4c/4d supply the real
  `ESMF_MethodExecute`-backed implementation once
  `CallbackInterfaceRegistry`/`CallbackStateBinding` exist.

Alternative considered for both: have the adapter call a bare procedure
pointer instead of an abstract-type-with-deferred-method. Rejected:
every other injectable-strategy point in this codebase (`Transform`,
now `MethodInvocationAdapter` itself) uses the abstract-type pattern,
which also lets the real Phase 4b/4c implementation carry its own state
(e.g. a reference to the driver-lookup structure) without inventing a
separate closure-context mechanism Fortran does not have natively.

**`MethodGraphNode%invoke(rc, clock)` gathers `this%arguments`/
`this%bindings` and calls `this%adapter%invoke(...)` — no branching on
adapter kind.** Directly satisfies REQ-MTH-002's "the graph model MUST
NOT need to know or care which ESMF call signature underlies a given
`MethodGraphNode`": the one call-site is adapter-kind-agnostic by
construction, since both concrete adapters implement the same deferred
interface.

**REQ-MTH-004/005 are satisfied by absence, not new code.** No
import/export sub-structure is added to the argument map (a flat
`character -> ArgumentSpec` map, a flat `character -> NodeId` binding
map); no new `ESMF_State` is created anywhere in this change; no
component-level node type is introduced. The spec.md requirements for
these restate the constraint so future code changes remain accountable
to it, matching how `12-methods-and-drivers.md` REQ-MTH-005 itself
restates `03-graph-node-hierarchy.md` REQ-NODE-008 "for locality."

## Risks / Trade-offs

- **[Risk]** Keeping argument bindings on-node (Decision above) instead
  of reusing `PortBindingTable` could mean Phase 4b or a later
  visualization-export sub-change has to add a second binding-storage
  code path if it ever needs to walk `MethodGraphNode` bindings the same
  way it walks `TransformGraphNode` bindings today. **Mitigation:**
  `MethodGraphNode` exposes its bindings through the same
  `get_argument_bindings()`-shaped accessor `TransformGraphNode` exposes
  its ports through, so a future caller can treat the two node kinds
  uniformly at the call-site level even though the underlying storage
  differs; only `ComponentGraph`-internal dispatch code would need a
  per-kind branch, and today's demand-driven dispatch already has one
  (the existing `class default` branch) for the opposite reason (to
  exclude `MethodGraphNode`, not include it).
- **[Risk]** Two small abstract injection interfaces
  (`GridCompPhaseInvoker`/`StateMethodInvoker`) are introduced with only
  synthetic implementations in this change, and `GridCompPhaseInvoker`'s
  deferred signature does not, in fact, fit the real translation path it
  is meant to stand in for. Found after the fact (not caught during this
  change's own review) by comparing against
  `superstructure/generic/OuterMetaComponent/run_child_by_name.F90`, the
  existing precedent for exactly this "invoke a named phase by name"
  operation: it resolves a driver via `this%get_child(child_name)` (an
  ordinary lookup on the *parent `OuterMetaComponent`'s* own child map,
  not anything at the `ComponentGraph`/`GraphBuilder` layer REQ-MTH-009's
  wording points at), translates `phase_name` to an integer `phase_idx`
  via `get_phase_index(child_meta%get_phases(ESMF_METHOD_RUN),
  phase_name=phase_name, found=found)`
  (`superstructure/generic/MethodPhasesMap.F90` - a `phase_name ->
  phase_idx` table already populated per component at `SetServices` time,
  not something Phase 4b needs to invent), and then calls
  `child%run(phase_idx=phase_idx, ...)` - never a `phase_name` argument.
  `GridCompMethodInvocation`'s own `phase_name : character` field does
  not match `GriddedComponentDriver%run()`'s actual `phase_idx : integer`
  parameter, and `run_child_by_name`'s own body uses neither `arguments`
  nor `bindings` anywhere - REQ-MTH-003a's trigger/advance-revision work
  around the call is `OuterMetaComponent`'s job using
  `MethodGraphNode%get_argument_bindings()` directly, not something
  `invoke_phase()` itself is likely to consume. `run_child_by_name` is
  also a *parent-invokes-child* shape specifically; a component invoking
  its own phase (the REQ-MTH-011 init-lifecycle case) needs a second,
  simpler shape with no `get_child` lookup at all. **Not mitigated by
  this change** - recorded here, not fixed, since fixing it would mean
  designing Phase 4b's actual resolution mechanism now, which is
  explicitly out of this change's scope (Goals/Non-Goals above). Phase
  4b's own design.md MUST resolve, before implementation: (1) whether
  `GridCompMethodInvocation` carries `phase_idx : integer` directly or a
  `phase_name` translated via `get_phase_index`/`get_phases()` at
  invocation time; (2) whether `driver_key` resolves through
  `OuterMetaComponent%get_child()`-style lookup (own-component vs.
  named-child, two cases) rather than any new `ComponentGraph`-level
  registry; (3) whether `GridCompPhaseInvoker%invoke_phase()` needs the
  `arguments`/`bindings` parameters at all, or whether they should be
  dropped from that specific interface (keeping them on
  `StateMethodInvoker%invoke_method()`, where a callback method's bound
  members plausibly are consumed directly, is not in question here).
- **[Trade-off]** REQ-MTH-009's driver-identifier field exists on
  `GridCompMethodInvocation` now but resolves to nothing (no lookup
  mechanism until Phase 4b) — the field is inert in this change,
  carried only for shape continuity. Accepted: cheaper to add the field
  now (avoiding a Phase 4b signature change to add it later) than to
  defer it entirely. Per the Risk immediately above, `run_child_by_name`
  suggests this field's real resolution is an `OuterMetaComponent`-level
  child-name lookup, not a `ComponentGraph`-level registry — the field
  itself (an opaque `character` key) is compatible with either shape, so
  this trade-off's acceptance still holds; only the *resolution*
  mechanism, not the field, is affected.

## Migration Plan

Purely additive: seven new modules, zero changes to any existing file's
behavior. No existing test, build target, or public API is touched.
Rollback is a plain revert of this change's new files and their
`CMakeLists.txt` entries.
