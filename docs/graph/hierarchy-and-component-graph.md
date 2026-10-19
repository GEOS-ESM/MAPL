# Hierarchy and ComponentGraph

Source: `superstructure/generic/graph/{HierarchyGraph,HierarchyResources,HierarchyRegistry,ComponentGraph,ComponentGraph_DemandDrivenUpdate}.F90`.

## Two-tier ownership

A component hierarchy (one `OuterMetaComponent` tree, or one standalone
test graph) has:

- **exactly one `HierarchyGraph`** — the shared, hierarchy-wide owner of
  the polymorphic `NodeId -> GraphNode` map, the
  `DependencyNetworkId -> DependencyNetwork` map, and the one
  `NodeIdGenerator`/`DependencyNetworkIdGenerator` pair for the whole
  hierarchy (`HierarchyGraph.F90:1`). Held inside a `HierarchyResources`
  value, itself held in the private `HierarchyRegistry`
  (`HierarchyRegistry.F90:1`), keyed by `HierarchyId`.
- **one `ComponentGraph` per `OuterComponent`** — holds only its own
  `HierarchyId` (a plain value, never a cached pointer) and resolves
  the shared `HierarchyGraph` fresh via
  `get_hierarchy_resources(hierarchy_id)` on every access
  (`ComponentGraph.F90:9-19`). Each `ComponentGraph` additionally keeps
  its own private `NodeIdSet`/`DependencyNetworkIdSet` recording which
  ids *it itself* registered (`own_node_ids`/`own_network_ids`,
  `ComponentGraph.F90:111-120`) — "owned" means "recorded in this
  instance's own set," since every component's nodes/networks live
  together in the one shared `HierarchyGraph` map.

This split exists so that cross-component wiring (parent-local proxy
nodes standing in for a child's items, see `graph-builder.md`) can
share one id space across the hierarchy, while `ComponentGraph` still
enforces per-component ownership/encapsulation on top.

### Why `HierarchyRegistry` is keyed, not a bare singleton

Mirrors `CallbackInterfaceRegistry`'s "narrower wrapper procedures
hiding a singleton" convention, but keyed by `HierarchyId` so multiple
independent hierarchies (e.g. separate pFUnit test fixtures in one
process) each get isolated storage with no cross-hierarchy bleed.
Three entry points (`HierarchyRegistry.F90:47-49`):

- `request_new_hierarchy()` — mints a fresh `HierarchyId`, allocates a
  default `HierarchyResources` entry. Called once by a hierarchy's root
  `OuterMetaComponent`, and once by `ComponentGraph`'s own no-argument
  constructor (`graph = ComponentGraph()`, the ~40 existing standalone
  test call sites), making that standalone graph the root of its own
  one-component hierarchy.
- `get_hierarchy_resources(id)` — looked up fresh every call, **never
  cached across statements** by any caller; returns a pointer valid
  until the registry's backing map next structurally changes (ordinary
  gFTL `%at()` pointer-validity contract).
- `release_hierarchy(id)` — erases one entry; safe while other
  hierarchies' entries remain live (deep-copy-on-move backing map).

`HierarchyResources` (`HierarchyResources.F90:1`) also carries
`component_names` (a `StringSet`) — every component name added anywhere
in the hierarchy, so a name collision anywhere (not just among a new
child's siblings) is caught at add-child time.

## ComponentGraph responsibilities (per-component, local)

`ComponentGraph.F90:1`. **MUST NOT depend on** `OuterComponent`,
`StateRegistry`, `GriddedComponentDriver`, `GraphBuilder`, or any
component-hierarchy type (REQ-CG-002) — only `NodeId`/
`DependencyNetworkId`/`PortId`/`HierarchyId`, the `GraphNode` hierarchy,
and `DependencyNetwork`.

Owns, locally:

- **Public ports**: three `PortId -> NodeId` tables — import ports,
  export ports, child-port bindings (`GraphPortRegistry`,
  `ComponentGraph.F90:104`). `PortId` generation is per-component (not
  hierarchy-wide, unlike `NodeId`).
- **Transform port-binding table** (`PortBindingTable`) — external
  storage, keyed `(DependencyNetworkId, NodeId) -> port name -> NodeId`
  (REQ-XFORM-005). See `transforms-and-update.md`.
- **Semantic resource index** (`StateItemMemberMap`) — name-keyed
  `NodeId` lookup, reused by `GraphBuilder` for advertised-item and
  cross-graph-proxy identity lookup; no separate lookup table was
  invented for that purpose.
- **Lifecycle status**: `MUTABLE -> FROZEN -> FINALIZED`
  (`GRAPH_LIFECYCLE_*` parameters, `:97-99`). One-way.

### Lifecycle

```
MUTABLE  -- freeze() --> FROZEN  -- finalize() --> FINALIZED
```

`freeze()` validates first (REQ-CG-006 et al.); only a validated graph
may freeze. Structural topology is fixed after freeze — later queries
(including `GraphExport`) rely on this. See `graph-builder.md` for
*when* in the real init sequence freeze happens
(`graphbuilder_freeze()`, after real connection resolution, not at
advertise time).

### `add_dependency` and foreign ids

A dependency edge can only be added to a network **this `ComponentGraph`
owns**, between two `NodeId`s this call is allowed to reference. "Allowed"
is strictly narrower than "anything in the hierarchy": either ids this
instance itself registered, or ids `allowed_foreign_node()`
(`ComponentGraph.F90:139`) explicitly permits — "registered somewhere in
this hierarchy" is not sufficient on its own; see that procedure's own
header comment and REQ-HIER-005/006 (a parent may not reach into a
child's internal topology — cross-component wiring always goes through
a parent-local proxy node instead, never a direct edge to a child's real
node).

## Demand-driven `update()`

Implemented in the submodule `ComponentGraph_DemandDrivenUpdate.F90`
(split out purely for file size — `ComponentGraph.F90` is the largest
file in the directory). `update(network_id, node_id, rc)`: "ensure this
node's value is current, within this network." Dispatches on dynamic
node kind:

- **`StateItemNode`**: find its sole predecessor (producer) in this
  network. Zero predecessors ⇒ externally supplied, nothing to do.
  More than one is a defensive assert (validation should already
  forbid it). If the predecessor is a `TransformGraphNode`, recurse
  into it.
- **`TransformGraphNode`**: recursively update every predecessor first,
  then assemble the current declared-input revision snapshot via the
  port-binding table, ask the transform whether it needs to execute,
  execute if so, advance every declared output's revision together,
  record the new baseline.
- **any other kind** (notably `MethodGraphNode`): explicit `class
  default` no-op — not demand-driven, see `node-hierarchy.md`.

No cycle/visited-set bookkeeping — acyclicity is already a graph
invariant (`DependencyNetwork` rejects cycles at construction time), and
re-visiting an already-current node through a diamond dependency is
redundant but not incorrect. Traversal uses an explicit-stack frame
(`UpdateFrame`, declared in `ComponentGraph.F90` itself rather than the
submodule — an ifx shared-library symbol-versioning quirk forced this;
see the comment at `ComponentGraph.F90:188` before "fixing" it).

Optimizing repeated-visit cost on a frozen graph (ahead-of-time
compilation to direct wrapper calls) is a distinct, later concern — see
`open-questions.md` (compilation).
