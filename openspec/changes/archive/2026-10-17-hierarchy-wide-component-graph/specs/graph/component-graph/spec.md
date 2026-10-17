## MODIFIED Requirements

### Requirement: ComponentGraph owns graph structure and identity generation
`ComponentGraph` MUST own: a generator for port identities (port
identities remain scoped to the single component that declares them);
a per-instance default network identity (its own valid
`DependencyNetworkId`, drawn from the shared generator below, but
distinctly its own); public import/export ports; child-port bindings;
a `TransformGraphNode` port-binding table keyed by
`(DependencyNetworkId, NodeId)` mapping port name to `NodeId`; and
semantic resource indexes. The polymorphic node map keyed by `NodeId`,
the map of dependency networks keyed by `DependencyNetworkId`, and the
generators that issue fresh `NodeId`/`DependencyNetworkId` values are
NOT owned per-instance — they are shared across every `ComponentGraph`
instance within one hierarchy (see `graph/component-hierarchy`'s "One
shared hierarchy-wide graph store" requirement), so that two different
components can never be issued the same `NodeId` or
`DependencyNetworkId`; each `ComponentGraph` instance references that
shared storage (maps and generators alike) rather than holding its own
separate copies.

#### Scenario: Graph owns distinct default network identity
- **WHEN** two component graphs are created
- **THEN** each has its own valid default dependency-network identity

#### Scenario: Node registration preserves identity
- **WHEN** a valid graph node is registered
- **THEN** graph can retrieve it by its assigned `NodeId` and no second
  node silently replaces it

#### Scenario: Port binding is retrievable by network and node
- **WHEN** a port on a `TransformGraphNode` owned by the graph is bound
  to a `NodeId`, for a given `DependencyNetworkId`
- **THEN** the graph can retrieve that binding by the same
  `(DependencyNetworkId, NodeId, port name)` combination

#### Scenario: A node registered through one component's graph is visible through another's
- **WHEN** a node is registered through component A's `ComponentGraph`
  instance
- **THEN** component B's `ComponentGraph` instance, querying the same
  shared node store by that node's `NodeId`, retrieves the identical
  node — because node storage is shared, not duplicated per instance

### Requirement: Freeze transitions graph from mutable to runtime-updateable
Freezing MUST irreversibly freeze graph structure for the entire
hierarchy and all owned dependency networks, applied as a single
hierarchy-wide operation rather than once per `ComponentGraph` instance
(see `graph/component-hierarchy`'s "Freeze is a single hierarchy-wide
operation" requirement). After freezing, nodes, dependencies, ports,
transform port bindings, members, networks, and structural indexes MUST
NOT be added or changed anywhere in the hierarchy, while runtime payload
values and revisions MAY be updated in place.

#### Scenario: Graph freeze propagates to networks
- **WHEN** the hierarchy-wide freeze operation is invoked once
- **THEN** every dependency network owned by every component in the
  hierarchy reports frozen state afterward

#### Scenario: Frozen structure rejects mutation
- **WHEN** client attempts to add a node, dependency, port, member, or
  network belonging to any component in an already-frozen hierarchy
- **THEN** operation fails and existing graph structure remains
  unchanged, regardless of which component's local graph the attempted
  mutation targets

#### Scenario: Frozen structure rejects new transform port bindings
- **WHEN** client attempts to add a transform port binding after
  hierarchy-wide freeze
- **THEN** operation fails and existing bindings remain unchanged

#### Scenario: Runtime value update remains allowed
- **WHEN** existing state-item payload or revision changes after
  hierarchy-wide freeze
- **THEN** update succeeds without changing node identity or graph
  topology

### Requirement: ComponentGraph validates ownership and cross-network writes
Graph validation MUST verify that owned nodes and networks are valid,
network references are consistent, each network is valid, and no single
update pass writes the same state item more than once across owned
networks. Because node and network storage is now shared across the
hierarchy (not per-`ComponentGraph`), "owned" here means tracked by an
explicit, per-instance record of which ids this particular
`ComponentGraph` registered (populated at registration time), not
"stored in this instance's own private map" — the record is forward
only ("which ids are mine"); no hierarchy-wide reverse lookup ("which
component owns an arbitrary id") is introduced or required. A graph
MUST reject wiring that violates the cross-network same-pass write
constraint.

#### Scenario: Invalid foreign node is rejected
- **WHEN** client attempts to wire a node identity not owned by any
  graph reachable from this operation's own component or its
  descendants, per the connection-endpoint-resolution requirement in
  `graph/component-hierarchy`
- **THEN** operation fails without partial graph mutation

#### Scenario: Same-pass cross-network write is rejected
- **WHEN** wiring would write one state item more than once during one
  update pass across graph-owned networks
- **THEN** graph validation rejects the wiring

#### Scenario: Ownership is explicit, not inferred from shared-map membership
- **WHEN** a `ComponentGraph` instance checks whether a given `NodeId`
  is one of its own
- **THEN** the answer comes from that instance's own explicit
  registration record, not from whether the shared node store happens
  to contain that key — because the shared store now contains every
  component's nodes together, so mere presence in it no longer
  distinguishes one component's nodes from another's
