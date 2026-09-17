# 3. GraphNode Hierarchy

Status: `[SETTLED]` node hierarchy shape, including `BaseGraphNode` storing
its own `NodeId` (§3.1, REQ-NODE-002a), leaving adjacency out (kept as-is
on reconsideration), and the payload type held by `StateItemNode`
(renamed from `ValueGraphNode`; see §3.1a, `04-graph-value-hierarchy.md`
§4.6, `17-open-questions.md` Q11 — fully resolved).

## 3.1 Hierarchy

```
GraphNode                          (abstract, data-free interface)
  BaseGraphNode                    (common properties only)
    StateItemNode                  (concrete: one payload + one NodeRevision)
    OperationGraphNode             (abstract)
      TransformGraphNode           (demand-driven MAPL Transform)
      MethodGraphNode              (explicitly invoked method)
```

**REQ-NODE-001.** `GraphNode` MUST be an abstract, data-free interface. It
defines identity/dispatch contracts only; it MUST NOT carry state.

**REQ-NODE-002.** `BaseGraphNode` MUST contain only properties common to
*all* graph nodes — e.g. lifecycle status, and (REQ-NODE-002a) its own
`NodeId`. `BaseGraphNode` MUST NOT store adjacency (predecessors/
successors); adjacency lives exclusively in `DependencyNetwork` (see
`06-dependency-network.md`).

**Reconsidered and kept as-is (adjacency exclusion):** whether
`BaseGraphNode` should also cache its own adjacency was reconsidered
alongside the `NodeId` decision below (REQ-NODE-002a) and deliberately left
unchanged. A node's adjacency is not well-defined without specifying
*which* `DependencyNetwork` — REQ-DEP-004 permits more than one per
`ComponentGraph`, and REQ-DEP-008's own note allows a node to have
different producers in different networks (get/put, forward/return). A
flat, un-scoped adjacency cache on `BaseGraphNode` would be ambiguous or
wrong for any node participating in more than one network. If a cache is
wanted for performance, it should be scoped per-network rather than added
as a blanket field — most naturally as part of the eventual
compiled-execution work (`11-revision-and-update.md` REQ-REV-010,
`17-open-questions.md` Q9), which already anticipates a frozen-graph
optimization of this kind. Recorded here so this isn't re-litigated
without the context.

**REQ-NODE-002a.** `BaseGraphNode` MUST store its own `NodeId`, set once at
node creation (by the same operation that inserts the node into
`ComponentGraph`'s `NodeId → GraphNode` map) and never mutated afterward.
Access MUST be through an accessor method (e.g. `get_node_id()`), not raw
field access, consistent with the encapsulation style already used for
`NodeRevision` (REQ-REV-003). This reverses the original position
(`05-identities.md` REQ-ID-005, amended alongside this) after weighing the
practical cost of *not* having it: code that reaches a node via traversal
(port bindings, successor walks, the structural-reset walk in
`18-state-item-characteristics.md` §18.8) would otherwise have to carry the
`NodeId` alongside every node reference or perform a reverse lookup, purely
to avoid a single cached scalar. Unlike adjacency (above), a `NodeId` does
not vary per-network, so there is no analogous ambiguity.

**Invariant.** For every `(id, node)` entry in `ComponentGraph`'s map,
`node%get_node_id() == id` MUST hold. The map remains authoritative for
lookup and membership (`05-identities.md` REQ-ID-005); the node's own
stored copy is a convenience cache of the same fact, assigned once at
insertion, and MUST NOT be allowed to diverge from it.

## 3.1a Rename: ValueGraphNode → StateItemNode `[SETTLED]`

**REQ-NODE-003.** The node formerly named `ValueGraphNode` is renamed
`StateItemNode` throughout this specification. The rename is purely
nominal — every requirement previously stated for `ValueGraphNode` applies
unchanged to `StateItemNode` unless explicitly amended below.

**REQ-NODE-003a.** `StateItemNode` is concrete and MUST contain exactly:

- one payload
- one `NodeRevision`

in addition to the `NodeId` and lifecycle-status properties it inherits
from `BaseGraphNode` (REQ-NODE-002/002a). It MUST NOT contain adjacency
(REQ-NODE-002).

**Resolved (was `[OPEN, narrowed]`).** The original design (REQ-VAL-001)
specified the payload as "one polymorphic `GraphValue`." The adopted
amendment (`04-graph-value-hierarchy.md` §4.6) instead specifies the
payload as one concrete, non-polymorphic `GraphStateItem` with allocatable
components for `ESMF_Field`, `ESMF_FieldBundle`, and `ESMF_State` (three,
not four — a RouteHandle is represented via `esmf_state` in its
RouteHandle-wrapper role, distinguished by a two-tier `esmf_kind()`/
`variant()` classification rather than a dedicated fourth component; see
REQ-SI-002/002b), of which at most one is allocated — this fully
supersedes REQ-VAL-001 for `StateItemNode`'s payload (REQ-SI-005). The
geometry sub-case (`GeomValue`, §4.2a) is resolved (§4.6.4 — geometry is
carried as an incomplete `esmf_field`, `variant() ==
MAPL_STATEITEM_GEOM`, no separate node kind needed). The
`FieldBundleValue`/`StateValue` membership-map placement is resolved as
`GraphStateItem` components (`04-graph-value-hierarchy.md` REQ-SI-006).
`VerticalGridValue` does not collapse the same way — resolved separately
as `esmf_state`-kind `GraphStateItem`, `variant() ==
MAPL_STATEITEM_VERTICALGRID` (Q14,
`13-geometry-and-vertical-grids.md` REQ-GEO-009). See
`17-open-questions.md` Q11 (fully resolved) and Q14.

**REQ-NODE-004.** `OperationGraphNode` is abstract. It represents "something
that runs," as opposed to `StateItemNode`, which represents "something that
holds a value."

**REQ-NODE-005.** `TransformGraphNode` represents a demand-driven MAPL
Transform: a computation with named inputs and outputs, executed lazily
based on revision comparison (see `10-transforms-and-ports.md`,
`11-revision-and-update.md`).

**REQ-NODE-006.** `MethodGraphNode` represents an explicitly invoked method:
either a GridComp initialize/run phase, or an attached method on an ESMF
State (a callback). See `12-methods-and-drivers.md`.

## 3.2 Why OperationGraphNode splits into two subclasses

Transforms and Methods are both "something happens here," but differ in
*trigger discipline*:

- `TransformGraphNode` execution is **demand-driven**: the graph decides
  when to run it, based on revision comparison against declared outputs
  being requested.
- `MethodGraphNode` execution is **externally invoked**: something outside
  the graph (a parent's run method, a component's own run method calling a
  callback) decides when to call it. The graph records the dependency
  structure of its arguments but does not decide *when* it runs.

**REQ-NODE-007.** The graph MUST NOT attempt to schedule `MethodGraphNode`
execution the way it schedules `TransformGraphNode` execution. Method
invocation timing is owned by the invoking code (GridComp run phase,
callback call site), not by the dependency network's demand-driven update
logic.

## 3.3 No separate node type for "the component"

**REQ-NODE-008.** There is no `GraphNode` subclass representing a component
itself. A component is represented indirectly through its `MethodGraphNode`s
(its phases) and the `StateItemNode`s that are its state items. This is a
deliberate simplification, not an oversight — see `12-methods-and-drivers.md`
§12.4.
