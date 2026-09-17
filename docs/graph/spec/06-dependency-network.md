# 6. DependencyNetwork

Status: `[SETTLED]` core model, including port-binding storage location
(§6.6, `17-open-questions.md` Q3).

## 6.1 No first-class edges

**REQ-DEP-001.** The design MUST NOT use persistent `GraphEdge` or `EdgeId`
objects. A dependency is nothing more than a `(source NodeId, target
NodeId)` pair; it MUST NOT be reified as an addressable object with its own
identity in v1.0.

**REQ-DEP-002.** `DependencyNetwork` MUST store direct adjacency as two maps:

- `NodeId → predecessor NodeIdSet`
- `NodeId → successor NodeIdSet`

This is conventional adjacency-list storage.

**Note (revisit trigger):** if edges later need independently meaningful
properties (e.g., a per-edge transform-role annotation beyond what
`TransformGraphNode` ports already express), REQ-DEP-001 should be revisited
explicitly rather than worked around informally.

## 6.2 Ownership and acyclicity

**REQ-DEP-003.** Each `ComponentGraph` MUST own one or more
`DependencyNetwork`s.

**REQ-DEP-004.** Each individual `DependencyNetwork` MUST be acyclic. The
*union* of all of a `ComponentGraph`'s networks MAY be cyclic.

**Rationale (callbacks):** a get-network may contain `provider value → get
transform → callback value`; a put-network may contain `callback value →
put transform → provider value`. Their union is cyclic; each is
individually acyclic. This is why per-method networks exist instead of one
network per graph — see `15-callbacks.md` §15.7.

## 6.3 Structure

`DependencyNetwork` conceptually contains:

- `predecessors_by_node`
- `successors_by_node`
- `frozen` status

**REQ-DEP-005.** `DependencyNetwork` MUST expose at least the following
operations:

| Operation | Contract |
|---|---|
| `add_dependency(source, target)` | Adds `source → target`. MUST reject if it would create a cycle (§6.4) or fail validation. MUST fail if `frozen`. |
| `remove_dependency(source, target)` | Removes the pair if present. MUST fail if `frozen`. **No confirmed use case as of this writing** — checked against the concrete initialization phase cycle (`12-methods-and-drivers.md` REQ-MTH-011c: `realize_provided` → `accept_transfer` → `realize_accepted`, iterated to convergence), which appears strictly monotonic (each iteration only processes items newly reaching `SPECIFIED` status; nothing observed that retracts an already-made connection). Kept in the API for completeness and symmetry with `add_dependency`, and in case `modify_advertised` (currently a no-op, REQ-MTH-011b) later needs to undo a not-yet-realized connection — not because a current requirement demands it. |
| `contains_dependency(source, target)` | Query, no mutation. |
| `get_predecessors(node)` | Returns predecessor `NodeIdSet`. |
| `get_successors(node)` | Returns successor `NodeIdSet`. |
| `has_predecessors(node)` | Boolean convenience query. |
| `has_successors(node)` | Boolean convenience query. |
| `would_create_cycle(source, target)` | Pure query, no mutation; see §6.4. |
| `validate()` | Full structural validation, see §6.5. |
| `freeze()` | Irreversibly marks the network frozen; disables mutating operations. |
| `clear()` | Resets to empty. MUST fail if `frozen`. |

## 6.4 Cycle rejection algorithm

**REQ-DEP-006.** Before adding `source → target`, the network MUST search
downstream from `target` (i.e., traverse successors transitively). If
`source` is reachable from `target`, adding `source → target` would create a
cycle, and the operation MUST be rejected (no partial mutation).

## 6.5 Full validation

**REQ-DEP-007.** `validate()` MUST check:

1. Predecessor/successor symmetry (if `A` lists `B` as successor, `B` MUST
   list `A` as predecessor, and vice versa)
2. All referenced `NodeId`s are valid (exist in the owning `ComponentGraph`)
3. No self-dependencies (`source == target` is never valid)
4. Acyclicity of the whole network
5. Any graph-level producer constraints (§6.6)

**REQ-DEP-008.** Within one `DependencyNetwork`, each `StateItemNode` MUST
have at most one producer `TransformGraphNode`. Multiple logical
contributors to one value MUST be represented through an explicit merge or
accumulation `TransformGraphNode`, not by allowing multiple direct
producers.

Note: this constraint is scoped to a single `DependencyNetwork`. A
`StateItemNode` MAY have different producers in different networks it
participates in (e.g., forward vs. return network for an inout item — see
`16-inout-items.md`, `[DEFERRED]`).

**REQ-DEP-008a `[settled, sharpens REQ-DEP-008]`.** Across all networks a
`ComponentGraph` owns, there MUST NEVER be a case where, within a single
update pass, the same `StateItemNode` is written to more than once.
`GraphBuilder`/`ComponentGraph` construction-time validation MUST reject
any wiring that could result in this. REQ-DEP-008 alone does not prevent
this (it only limits producer count *per network*); this requirement adds
the cross-network check for the same-update-pass case. The callback
get/put pattern (`15-callbacks.md` §15.10) remains permitted because
REQ-CB-020 already guarantees the get-network write and the put-network
write are never part of the same pass (single invocation, after all
arguments are prepared — the two writes are temporally distinct events,
not concurrent producers within one pass). The general ordinary-inout case
(`16-inout-items.md`) remains `[DEFERRED]` and is not otherwise addressed
by this requirement beyond ruling out the same-pass-double-write hazard
specifically. See `17-open-questions.md` Q4.

## 6.6 Port bindings — location `[SETTLED]`

Named transform-port bindings (which `NodeId` fills which named argument of
a `TransformGraphNode`) are semantically distinct from adjacency (which
represents traversal topology only). These bindings MUST be stored
externally, keyed by `(DependencyNetworkId, node NodeId)`, per
`10-transforms-and-ports.md` REQ-XFORM-005 (resolves `17-open-questions.md`
Q3) — never as a field on `TransformGraphNode` itself.
