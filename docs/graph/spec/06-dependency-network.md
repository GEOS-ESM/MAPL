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

## 6.2 Ownership and acyclicity `[RE-EVALUATED]`

**REQ-DEP-003 `[re-evaluated]`.** Each hierarchy has exactly one
*forward* `DependencyNetwork`, held in the shared `HierarchyGraph`. Every
edge that carries data toward a consumer before the consumer runs
(ordinary connections, extension chains, expression evaluations,
collection edges, geometry edges, and the get side of a callback
argument) goes into it, whichever component's declaration produced the
edge. A `ComponentGraph` has no default network of its own; it owns only
the *write-back* networks it creates (the put side of a callback method,
the return edge of an inout borrower). A pull never follows a write-back
network.

**REQ-DEP-004.** Each individual `DependencyNetwork` MUST be acyclic. The
forward network is acyclic across the whole hierarchy. The union of the
forward network and a write-back network MAY be cyclic.

**Rationale (callbacks, re-evaluated):** the get side `provider value →
get transform → callback value` lives in the forward network; the put side
`callback value → put transform → provider value` lives in the method's
own write-back network. Their union is cyclic; each is individually
acyclic. Per-component forward networks were dropped because they split
one data path across networks (an edge declared by a grandparent was
invisible to a pull through the parent's network) and duplicated reusable
chains per declaring component — see `15-callbacks.md` §15.10.

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

Note `[re-evaluated]`: in the forward network this is checked when the
edge is added, across the whole hierarchy: a second, different producer
into a `StateItemNode` fails naming the item; re-adding the identical
edge is a no-op (two-pass initialization may resolve a connection twice).
Edges into a `TransformGraphNode` are not limited. A `StateItemNode` MAY
also be the target of a write-back edge (callback put, inout return; see
`16-inout-items.md`, `[DEFERRED]`).

**REQ-DEP-008a `[re-evaluated]`.** There MUST NEVER be a case where,
within a single update pass, the same `StateItemNode` is written to more
than once. With one forward network per hierarchy, REQ-DEP-008's add-time
check already covers every forward producer. A write-back edge into an
item that also has a forward producer is NOT rejected: a pull follows only
the forward network, and the write-back fires only after the method runs
(pull before, write-back after; REQ-CB-020), so the two writes are never
part of the same pass. The separate cross-network validation step is
therefore removed. The general ordinary-inout case (`16-inout-items.md`)
remains `[DEFERRED]`. See `17-open-questions.md` Q4.

## 6.6 Port bindings — location `[SETTLED]`

Named transform-port bindings (which `NodeId` fills which named argument of
a `TransformGraphNode`) are semantically distinct from adjacency (which
represents traversal topology only). These bindings MUST be stored
externally, keyed by `(DependencyNetworkId, node NodeId)`, per
`10-transforms-and-ports.md` REQ-XFORM-005 (resolves `17-open-questions.md`
Q3) — never as a field on `TransformGraphNode` itself.
