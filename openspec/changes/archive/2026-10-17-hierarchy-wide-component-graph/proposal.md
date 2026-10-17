## Why

A component-local `ComponentGraph` per `OuterComponent` (REQ-HIER-004)
makes every cross-component need — proxy nodes for an ordinary
connection (3a/3b), geometry push/pull (Phase 4e), the flattened
qualified-export namespace callback wiring resolves against (Phase
4c/4d), and the still-unfiled cross-component unresolved-import
propagation (3b2) — invent its own ad hoc mechanism for referencing a
node that belongs to a different graph instance, because `NodeId` is
only meaningful within the `ComponentGraph` that issued it. Four
features, four incompatible crossing primitives (`proxy_key`,
`parent_geometry_proxy_key`, `build_qualified_export_namespace`'s
recompute-every-call walk, and 3b2's own abandoned, never-settled
attempt to decide between `item_key` and `proxy_key`), none reusable by
the next one, and `GraphBuilder.F90` has grown to 1517 lines plus 11
submodules (4585 total) substantially because of this repeated
reinvention, not because the underlying connection logic is actually
that large.

Legacy's own two-sided propagation (imports bubble up only when
unresolved; exports are unconditionally pushed into a nested
per-component `ESMF_State` substate tree so History can see arbitrarily
deep into the hierarchy) has no faithful graph-native analog today —
the landed callback-wiring namespace only pulls on demand, from
scratch, with no maintained structure standing in for the nested-state
tree. Honestly replicating legacy's export side under the current
one-graph-per-component design would require building and maintaining
a second, parallel push mechanism of roughly the same shape and cost as
not having separate graphs at all.

Component names are already unique across the entire hierarchy (an
existing informal convention, relied on by History's own
component-name-based diagnostic lookup) — this makes a single,
hierarchy-wide graph both unambiguous and strictly simpler than the
per-component design it replaces: cross-component wiring becomes an
ordinary dependency edge between two already-valid `NodeId`s, with no
proxy, no push, and no recompute-on-demand walk required.

## What Changes

- **BREAKING** (internal API, no external-facing change): `NodeId` and
  `DependencyNetworkId` become valid across the entire component
  hierarchy, not scoped to one `ComponentGraph` instance. `GraphNode`
  storage (`NodeId → GraphNode`) and dependency-network storage
  (`DependencyNetworkId → DependencyNetwork`) move from
  per-`OuterComponent` ownership to one shared, hierarchy-wide store.
  Each `OuterComponent` continues to own its own local resource index,
  public ports, child-port bindings, and lifecycle state unchanged —
  only the two map ownerships above move.
- **BREAKING**: `freeze()` becomes a single hierarchy-wide event (one
  call, at the point all init phases complete across the whole
  hierarchy) rather than a per-`ComponentGraph` operation each
  component invokes independently.
- Retires, as a direct consequence (not independently re-scoped):
  proxy-node storage and the parent-local proxy-node mechanism
  (REQ-HIER-006), `proxy_key`/`get_or_make_local_node_id`,
  `parent_geometry_proxy_key`/`push_geometry_to_child`, and the
  cross-graph-proxy half of `resolve_qualified_export_node_id`. A
  cross-component dependency edge is added directly between the two
  real `NodeId`s involved; nothing stands in for either endpoint.
- Retires 3b2 (cross-component unresolved-import propagation) as a
  concept, not merely as an unfiled change: "propagate an unresolved
  import to the parent" is replaced by "search the named destination
  component's entire subtree for an unresolved import of that name" —
  no materialized proxy, no bubbled copy, nothing to keep in sync.
- Adds the un-replicated legacy export-visibility behavior
  (History's deep-hierarchy diagnostic access) as an ordinary query
  against the shared hierarchy-wide resource index — no push mechanism
  needed, so this is additive capability, not merely parity.
- Establishes component-name uniqueness across the entire hierarchy as
  an enforced invariant (checked when a child is added), not merely a
  convention — required because the shared store depends on it for
  correctness, where before it was coincidentally harmless if violated.
- Clarifies (no code change, newly explicit requirement): a composite
  declaration's internal members (`composite-state-spec`) remain
  unreachable as independent connection endpoints — only a composite's
  own top-level advertised name is nameable in an ordinary connection,
  exactly as `GraphBuilder`'s existing port-table population already
  enforces structurally today.
- Clarifies (no code change, newly explicit requirement): connection
  endpoint name resolution starting at a declaring component MUST only
  resolve to that component itself or one of its descendants, never to
  an ancestor or sibling of the declaring component — the exact
  property that let legacy's own component config format omit any way
  to spell a parent's name, now made an explicit requirement rather
  than an accident of per-component graph storage.

## Capabilities

### New Capabilities
(none — this change restructures existing capabilities' own ownership
model rather than introducing a new capability domain)

### Modified Capabilities
- `graph/component-hierarchy`: reverses "one local graph per component,
  no global graph" (and the proxy-node storage requirement that exists
  only to work around that choice) in favor of one hierarchy-wide
  node/network store with per-component local resource indexes, ports,
  and lifecycle state; adds the connection-name-resolution-direction and
  composite-member-non-addressability requirements above.
- `graph/component-graph`: splits `ComponentGraph` ownership — node map
  and dependency-network map move to shared hierarchy-wide storage;
  resource indexes, ports, child-port bindings, and lifecycle state
  remain per-instance; freeze becomes a hierarchy-wide operation.
- `graph/identities`: `NodeId`/`DependencyNetworkId` lifetime and
  uniqueness scope widens from "within one `ComponentGraph` instance's
  lifetime" to "within the hierarchy's lifetime."
- `graph/graph-builder`: removes the proxy-node creation responsibility
  from ordinary connection resolution (a dependency edge is now added
  directly between real `NodeId`s on either side of a former component
  boundary); the "Ordinary resolution matches existing coupler
  behavior" requirement's qualified multi-level path-walking becomes an
  ordinary single-step component-name lookup followed by a local
  resource-index lookup, with no proxy materialization step.
- `graph/callback-wiring`: `resolve_qualified_export_node_id`'s
  proxy-creation half is removed — a matched descendant export's
  already-real `NodeId` is used directly; the flattened qualified-export
  namespace itself (REQ-CB-013/014) is unaffected in shape, only in how
  a match is turned into something the Invoker's own network can wire
  against.

## Impact

- `superstructure/generic/graph/ComponentGraph.F90` (and its container
  types): node/network maps extracted into shared storage; resource
  index, ports, bindings, lifecycle state untouched in shape.
- `superstructure/generic/GraphBuilder.F90` and submodules
  (`GraphBuilder_ConnectionResolution_smod.F90`,
  `GraphBuilder_PortsAndProxies_smod.F90`,
  `GraphBuilder_Geometry_smod.F90`,
  `GraphBuilder_CallbackWiring_smod.F90`): every proxy-node /
  qualified-path-walk code path is deleted outright rather than
  modified — this is the bulk of the expected line-count reduction.
  `GraphBuilder_Freeze_smod.F90`: freeze becomes a single hierarchy-wide
  call site instead of one per `OuterComponent`.
- `superstructure/generic/OuterMetaComponent*.F90`: gains the
  component-name-uniqueness check at child-add time; loses any
  proxy-node storage accessors.
- `docs/graph/spec/02-component-hierarchy.md`,
  `07-component-graph.md`, `05-identities.md`, `08-graph-builder.md`:
  amended in place (`[RE-EVALUATED]`, following `07`'s own REQ-CG-003
  precedent) rather than superseded by a new document.
- `docs/graph/spec/20-implementation-roadmap.md`: gains a new phase
  (next free number) describing this as an architecture revision that
  retires specific landed mechanisms (listed above), not an additive
  capability phase.
- No test fixture (`superstructure/generic/tests/scenarios/`,
  `graph-scenarios/`) requires behavioral changes; the still-unfiled
  `graphbuilder_propagation` fixture becomes directly implementable
  (subtree search) without ever having needed its own abandoned
  proxy/propagation design.
