# Component Graph Specification

## Purpose

Provides graph-neutral ownership and lifecycle for nodes, dependency networks,
identity generators, ports, bindings, and resource indexes without MAPL
component-hierarchy dependencies.

## Requirements

### Requirement: ComponentGraph owns graph structure and identity generation
`ComponentGraph` MUST own: a generator for port identities (port
identities remain scoped to the single component that declares them);
public import/export ports; and the resource index
of its own advertised items. The polymorphic node map keyed by `NodeId`,
the map of dependency networks keyed by `DependencyNetworkId`, the
generators that issue fresh `NodeId`/`DependencyNetworkId` values, the
one forward dependency network, the `TransformGraphNode` port-binding
table keyed by `(DependencyNetworkId, NodeId)`, and the reuse index are
NOT owned per-instance — they are shared across every `ComponentGraph`
instance within one hierarchy (see `graph/component-hierarchy`'s "One
shared hierarchy-wide graph store" requirement). Each `ComponentGraph`
instance references that shared storage rather than holding its own
separate copies. A `ComponentGraph` has no default network of its own.

#### Scenario: Graphs in one hierarchy share the forward network identity
- **WHEN** two component graphs are created in the same hierarchy
- **THEN** neither has a default network of its own: both report the
  same forward dependency-network identity

#### Scenario: Node registration preserves identity
- **WHEN** a valid graph node is registered
- **THEN** graph can retrieve it by its assigned `NodeId` and no second node
  silently replaces it

#### Scenario: Port binding is retrievable by network and node
- **WHEN** a port on a `TransformGraphNode` is bound to a `NodeId`, for a
  given `DependencyNetworkId`, through one component's graph
- **THEN** any component graph in the same hierarchy can retrieve that
  binding by the same `(DependencyNetworkId, NodeId, port name)`
  combination

#### Scenario: A node registered through one component's graph is visible through another's
- **WHEN** a node is registered through component A's `ComponentGraph`
  instance
- **THEN** component B's `ComponentGraph` instance, querying the same
  shared node store by that node's `NodeId`, retrieves the identical
  node — because node storage is shared, not duplicated per instance

### Requirement: ComponentGraph remains graph-neutral
`ComponentGraph` MUST NOT depend on `OuterComponent`, `StateRegistry`,
`GriddedComponentDriver`, `GraphBuilder`, or any component-hierarchy type.

#### Scenario: Synthetic graph works without MAPL hierarchy
- **WHEN** client constructs and wires a graph using synthetic graph nodes and
  values only
- **THEN** graph operations complete without a MAPL component-hierarchy object

### Requirement: ComponentGraph supports mutable initialization
Before freezing or finalization, the graph MUST allow valid node and network
registration, dependency wiring, port creation,
transform port-binding table updates, and resource-index updates, subject to
network and graph validation rules.

#### Scenario: Initialization builds graph structure
- **WHEN** client registers nodes, obtains the forward network, and adds valid
  dependencies
- **THEN** graph exposes the resulting structure through its query API

#### Scenario: Transform port binding requires graph-owned identities
- **WHEN** client attempts to bind a port using a `DependencyNetworkId` or
  `NodeId` not owned by the graph
- **THEN** the binding operation fails without partial mutation

### Requirement: ComponentGraph validates ownership and cross-network writes
Graph validation MUST verify that every node referenced by a network is
registered in the hierarchy and that each network is valid (acyclic, at
most one producer per state item). Validation of the forward network
covers the whole hierarchy. A write-back network is validated on its
own. Validation SHALL NOT reject an edge because one of its endpoints
was registered through a different component's graph; endpoint scope is
enforced by connection-name resolution (see `graph/component-hierarchy`,
"Connection endpoint resolution never resolves upward past its declaring
component").

#### Scenario: Invalid foreign node is rejected
- **WHEN** client attempts to wire a node identity that is not
  registered anywhere in the hierarchy
- **THEN** operation fails without partial graph mutation

#### Scenario: Same-pass cross-network write is rejected
- **WHEN** wiring would add a second, different forward producer for one
  state item, from any component's declaration
- **THEN** the wiring fails when the edge is added; a write-back edge
  into an item that also has a forward producer is not rejected, because
  the write-back fires only after the method runs, never in the same pull

#### Scenario: Node registered by another component is accepted
- **WHEN** an edge joins a node registered through component A's graph
  to a node registered through component B's graph in the same hierarchy
- **THEN** the edge is added to the forward network without error

#### Scenario: Ownership is explicit, not inferred from shared-map membership
- **WHEN** a `ComponentGraph` instance checks whether a given `NodeId`
  is one of its own
- **THEN** the answer comes from that instance's own explicit
  registration record, not from whether the shared node store happens
  to contain that key

### Requirement: Freeze transitions graph from mutable to runtime-updateable
Freezing MUST irreversibly freeze graph structure for the entire
hierarchy, including the forward network and every write-back network,
applied as a single hierarchy-wide operation rather than once per
`ComponentGraph` instance (see `graph/component-hierarchy`'s "Freeze is a
single hierarchy-wide operation" requirement). After freezing, nodes,
dependencies, ports, transform port bindings, members, networks, and
structural indexes MUST NOT be added or changed anywhere in the
hierarchy, while runtime payload values and revisions MAY be updated in
place.

#### Scenario: Graph freeze propagates to networks
- **WHEN** the hierarchy-wide freeze operation is invoked once
- **THEN** the forward network and every write-back network in the
  hierarchy report frozen state afterward

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

### Requirement: Finalization is explicit and lifecycle-checked
`finalize()` MUST remain an explicit fallible operation rather than an implicit
destructor action. After finalization, graph use MUST be rejected.

#### Scenario: Finalized graph rejects use
- **WHEN** client calls graph queries or mutation after successful finalization
- **THEN** operation fails with lifecycle error

### Requirement: ComponentGraph exposes node and network identity enumeration
`ComponentGraph` MUST expose public read-only operations returning the
complete set of `NodeId`s it registered and the complete set of
`DependencyNetworkId`s it uses: the forward network and every write-back
network it created. This is sufficient for a caller to walk the graph
using only public query operations.

#### Scenario: All owned node identities are enumerable
- **WHEN** client registers several nodes and then requests the graph's full
  set of node identities
- **THEN** every registered `NodeId` is present in the returned set and no
  unregistered identity is present

#### Scenario: All owned network identities are enumerable
- **WHEN** client creates a write-back network and then requests the
  graph's full set of network identities
- **THEN** both the forward network's identity and the write-back
  network's identity are present in the returned set

#### Scenario: Enumeration is unavailable after finalization
- **WHEN** node or network identity enumeration is requested after the graph
  has been finalized
- **THEN** the operation reports no identities rather than exposing
  finalized internal state
