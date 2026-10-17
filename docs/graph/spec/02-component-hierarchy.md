# 2. Component Hierarchy

Status: `[SETTLED]` core structure; §2.2/§2.3 `[RE-EVALUATED]` per the
landed `hierarchy-wide-component-graph` change (node/network storage is
now hierarchy-wide, not one-per-component — see that change's own
`design.md` D1/D2 and `openspec/specs/graph/component-hierarchy/spec.md`
for the authoritative current requirements); `[SPECULATIVE]` future
global graph view (§2.4), now substantially already true in practice
(see §2.4's own note).

## 2.1 OuterComponent

**REQ-HIER-001.** Every user component MUST be wrapped by an `OuterComponent`.

**REQ-HIER-002.** An `OuterComponent` MUST contain:

- A `GriddedComponentDriver` for its own user component
- One `GriddedComponentDriver` per child `OuterComponent`
- One local `ComponentGraph`
- Framework-managed import, export, and internal states
- Public ports visible to its parent

**REQ-HIER-003.** A parent `OuterComponent` MAY access its children's public
ports and drivers. Children MUST NOT access their parent. This is a hard
encapsulation boundary, not just a convention.

## 2.2 Node/network storage is hierarchy-wide, not one-per-component `[RE-EVALUATED]`

**REQ-HIER-004 `[re-evaluated]`.** The original position — "each
`OuterComponent` owns exactly one local `ComponentGraph`, and there is
no single global graph instance" — is superseded by the landed
`hierarchy-wide-component-graph` change
(`openspec/specs/graph/component-hierarchy/spec.md`, "One shared
hierarchy-wide graph store, with per-component local state"). The
`NodeId → GraphNode` map, the `DependencyNetworkId → DependencyNetwork`
map, and the `NodeIdGenerator`/`DependencyNetworkIdGenerator` pair that
issue fresh ids are now shared across the entire hierarchy under one
root (`HierarchyGraph`/`HierarchyResources`,
`superstructure/generic/graph/HierarchyGraph.F90`), keyed by a
`HierarchyId` each `OuterComponent` holds as a plain value, not a
pointer. Each `OuterComponent` still owns, exactly as component-local
as before: its own local resource index, its own public import/export
ports, its own child-port bindings, and its own lifecycle state — this
change moved only node/network *storage* ownership, not any of
`ComponentGraph`'s other responsibilities (REQ-CG-001).

**Why this changed:** every cross-component feature landed under the
original one-graph-per-component design (ordinary connections,
geometry push/pull, callback-wiring's flattened export namespace)
invented its own ad hoc mechanism for referencing a node belonging to
a different `ComponentGraph` instance, because a `NodeId` was only
valid within the instance that issued it. A fifth need
(cross-component unresolved-import propagation, originally scoped as
"3b2") was attempted and abandoned under the per-component model
specifically because neither existing crossing mechanism fit its
shape. See `openspec/changes/hierarchy-wide-component-graph/proposal.md`
("Why") for the full motivation.

**Rationale (unaffected by this change):** encapsulation of a child's
*local* concerns (resource index, ports, child-port bindings,
lifecycle state) is preserved exactly as before — only the shared
node/network maps and id generators moved. ESMF-level component
encapsulation (REQ-HIER-003: a child cannot reach its parent) is now
enforced by an algorithmic invariant (connection-name resolution never
walks upward past its declaring component,
`openspec/specs/graph/component-hierarchy/spec.md` "Connection
endpoint resolution never resolves upward past its declaring
component") rather than by separate per-component graph storage.

## 2.3 Cross-component wiring uses real node identities directly, no proxy `[RE-EVALUATED]`

**REQ-HIER-005/006 `[re-evaluated, retired]`.** The original
boundary/proxy-node mechanism this section described — a parent
referring to a child's graph only through cached, `GraphBuilder`-hidden
proxy nodes standing in for the child's real nodes — has been fully
removed. With node/network storage hierarchy-wide (§2.2), a `NodeId`
issued anywhere in the hierarchy is already a valid reference
everywhere in the hierarchy; a cross-component dependency edge
connects two real `NodeId`s directly, with no intermediate proxy or
boundary node on either side
(`openspec/specs/graph/component-hierarchy/spec.md` "Cross-component
dependency wiring uses real node identities directly"). Every
proxy-creation mechanism that previously existed to work around
per-component graph storage has been retired:

- `proxy_key`/`get_or_make_local_node_id`'s cross-graph branch
  (ordinary connections)
- `parent_geometry_proxy_key`/`push_geometry_to_child` (geometry
  push/pull)
- `resolve_qualified_export_node_id`'s proxy-creation half (callback
  wiring)

A parent still only ever *names* a child's item through that child's
own published port (REQ-HIER-002's "public ports visible to its
parent" is unaffected); what changed is that resolving that name now
yields the child's own real `NodeId` directly, not a parent-local
stand-in for it.

## 2.4 Future global graph view `[SPECULATIVE]`

A future global graph view MAY be built that inspects frozen local graphs
across the hierarchy for diagnostics or optimization purposes. It:

- MUST NOT own nodes (ownership stays with each local `ComponentGraph`)
- MUST operate only on frozen graphs (never mutate)
- Is out of scope for v1.0

**Note (§2.2's shared storage substantially pre-empts this):** since
node/network storage is now hierarchy-wide (§2.2), the data this
speculative view would need is already in one place
(`HierarchyGraph`); what remains speculative/out-of-scope is only a
dedicated *read-only query API* over that shared storage for
diagnostics/optimization purposes, not the storage unification itself.
`openspec/changes/hierarchy-wide-component-graph` Task Group 7 added
one narrow instance of such a query
(`GraphBuilder.F90:query_component_exports` — a qualified-path lookup
of one component's own local resource index, with no push/propagation
step) for History's deep-hierarchy export-visibility need
specifically; a general-purpose diagnostic/optimization view remains
unbuilt.

See `17-open-questions.md` Q1 for the open question of whether this remains
sufficient once `MethodGraphNode` (parent-invokes-child-method) is fully
accounted for.
