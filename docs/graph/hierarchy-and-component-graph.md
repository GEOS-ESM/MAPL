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

This split exists so that cross-component wiring (an edge between two
real `NodeId`s issued by different components, see `graph-builder.md`) can
share one id space across the hierarchy, while `ComponentGraph` still
tracks which ids and write-back networks each component created. The one
forward network (every edge that carries data toward a consumer, whichever
component declared it) belongs to the `HierarchyGraph`, not to any
`ComponentGraph` (`HierarchyGraph.F90:14-20`;
`ComponentGraph.F90:96-100`).

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
  (REQ-XFORM-005). See `transforms-and-update.md`. The network id is
  the forward network or a write-back network this component created
  (`graph_bind_port`, `ComponentGraph.F90:647`).
- **Semantic resource index** (`StateItemMemberMap`) — name-keyed
  `NodeId` lookup, reused by `GraphBuilder` for advertised-item identity
  lookup; no separate lookup table was invented for that purpose.
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

A dependency edge can only be added to the hierarchy's forward network
or to a write-back network **this `ComponentGraph` created**
(`graph_add_dependency`, `ComponentGraph.F90:379-380`). Both endpoint
`NodeId`s must be registered somewhere in the hierarchy
(`allowed_foreign_node()`, `ComponentGraph.F90:386-387,439-447`): an id
issued by another component is accepted, so a cross-component edge
connects two real `NodeId`s directly, with no proxy node
(`spec/02-component-hierarchy.md` §2.3). The check is presence in the
shared `HierarchyGraph`; it does not answer "is this mine" (`owns_node()`)
and does not enforce the descendant-only rule, which
`resolve_qualified_component` does by walking the component tree
downward. In the forward network a state item has at most one producer
(`forward_producer_conflict`, `ComponentGraph.F90:393-396`); an identical
re-add is a no-op (`:390-392`).

## Demand-driven `update()`

Implemented in the submodule `ComponentGraph_DemandDrivenUpdate.F90`
(split out purely for file size — `ComponentGraph.F90` is the largest
file in the directory). `update(node_id, rc)`: "ensure this
node's value is current." It always walks the hierarchy's forward network
(`graph_update_item` calls `get_forward_network_id`,
`ComponentGraph_DemandDrivenUpdate.F90:75-82`); the internal helpers take
the network id as an argument, the public call does not. Dispatches on
dynamic node kind:

- **`StateItemNode`**: find its sole predecessor (producer) in the
  forward network. Zero predecessors ⇒ externally supplied, nothing to do.
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
