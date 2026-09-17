# Component Graph Specification

## Purpose

Provides graph-neutral ownership and lifecycle for nodes, dependency networks,
identity generators, ports, bindings, and resource indexes without MAPL
component-hierarchy dependencies.

## Requirements

### Requirement: ComponentGraph owns graph structure and identity generation
`ComponentGraph` MUST own a polymorphic node map keyed by `NodeId`, one or
more dependency networks keyed by `DependencyNetworkId`, generators for node,
network, and port identities, a per-instance default network identity, public
import/export ports, child-port bindings, a `TransformGraphNode` port-binding
table keyed by `(DependencyNetworkId, NodeId)` mapping port name to `NodeId`,
semantic resource indexes, and initialized/finalized lifecycle state.

#### Scenario: Graph owns distinct default network identity
- **WHEN** two component graphs are created
- **THEN** each has its own valid default dependency-network identity

#### Scenario: Node registration preserves identity
- **WHEN** a valid graph node is registered
- **THEN** graph can retrieve it by its assigned `NodeId` and no second node
  silently replaces it

#### Scenario: Port binding is retrievable by network and node
- **WHEN** a port on a `TransformGraphNode` owned by the graph is bound to a
  `NodeId`, for a given `DependencyNetworkId`
- **THEN** the graph can retrieve that binding by the same
  `(DependencyNetworkId, NodeId, port name)` combination

### Requirement: ComponentGraph remains graph-neutral
`ComponentGraph` MUST NOT depend on `OuterComponent`, `StateRegistry`,
`GriddedComponentDriver`, `GraphBuilder`, or any component-hierarchy type.

#### Scenario: Synthetic graph works without MAPL hierarchy
- **WHEN** client constructs and wires a graph using synthetic graph nodes and
  values only
- **THEN** graph operations complete without a MAPL component-hierarchy object

### Requirement: ComponentGraph supports mutable initialization
Before freezing or finalization, the graph MUST allow valid node and network
registration, dependency wiring, port creation, child-port binding,
transform port-binding table updates, and resource-index updates, subject to
network and graph validation rules.

#### Scenario: Initialization builds graph structure
- **WHEN** client registers nodes, obtains the default network, and adds valid
  dependencies
- **THEN** graph exposes the resulting structure through its query API

#### Scenario: Transform port binding requires graph-owned identities
- **WHEN** client attempts to bind a port using a `DependencyNetworkId` or
  `NodeId` not owned by the graph
- **THEN** the binding operation fails without partial mutation

### Requirement: ComponentGraph validates ownership and cross-network writes
Graph validation MUST verify that owned nodes and networks are valid, network
references are consistent, each network is valid, and no single update pass
writes the same state item more than once across owned networks. A graph MUST
reject wiring that violates the cross-network same-pass write constraint.

#### Scenario: Invalid foreign node is rejected
- **WHEN** client attempts to wire a node identity not owned by graph
- **THEN** operation fails without partial graph mutation

#### Scenario: Same-pass cross-network write is rejected
- **WHEN** wiring would write one state item more than once during one update
  pass across graph-owned networks
- **THEN** graph validation rejects the wiring

### Requirement: Freeze transitions graph from mutable to runtime-updateable
`freeze()` MUST irreversibly freeze graph structure and all owned dependency
networks. After freezing, nodes, dependencies, ports, transform port
bindings, members, networks, and structural indexes MUST NOT be added or
changed, while runtime payload values and revisions MAY be updated in place.

#### Scenario: Graph freeze propagates to networks
- **WHEN** client freezes a valid initialized graph
- **THEN** graph and every owned dependency network report frozen state

#### Scenario: Frozen structure rejects mutation
- **WHEN** client attempts to add a node, dependency, port, member, or network
  after freeze
- **THEN** operation fails and existing graph structure remains unchanged

#### Scenario: Frozen structure rejects new transform port bindings
- **WHEN** client attempts to add a transform port binding after freeze
- **THEN** operation fails and existing bindings remain unchanged

#### Scenario: Runtime value update remains allowed
- **WHEN** existing state-item payload or revision changes after freeze
- **THEN** update succeeds without changing node identity or graph topology

### Requirement: Finalization is explicit and lifecycle-checked
`finalize()` MUST remain an explicit fallible operation rather than an implicit
destructor action. After finalization, graph use MUST be rejected.

#### Scenario: Finalized graph rejects use
- **WHEN** client calls graph queries or mutation after successful finalization
- **THEN** operation fails with lifecycle error

### Requirement: ComponentGraph exposes node and network identity enumeration
`ComponentGraph` MUST expose public read-only operations returning the
complete set of currently-owned `NodeId`s and the complete set of
currently-owned `DependencyNetworkId`s, sufficient for a caller to walk the
entire graph using only public query operations.

#### Scenario: All owned node identities are enumerable
- **WHEN** client registers several nodes and then requests the graph's full
  set of node identities
- **THEN** every registered `NodeId` is present in the returned set and no
  unregistered identity is present

#### Scenario: All owned network identities are enumerable
- **WHEN** client creates an additional dependency network beyond the default
  and then requests the graph's full set of network identities
- **THEN** both the default network's identity and the additional network's
  identity are present in the returned set

#### Scenario: Enumeration is unavailable after finalization
- **WHEN** node or network identity enumeration is requested after the graph
  has been finalized
- **THEN** the operation reports no identities rather than exposing
  finalized internal state
