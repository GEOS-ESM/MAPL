## MODIFIED Requirements

### Requirement: One shared hierarchy-wide graph store, with per-component local state
There SHALL be exactly one shared store of graph nodes, keyed by
`NodeId`, and one shared store of dependency networks, keyed by
`DependencyNetworkId`, spanning the entire component hierarchy under one
root. The shared store SHALL also hold, once per hierarchy: the one
forward dependency network (see "One forward network carries all
ordinary data flow"), the transform port bindings, and the reuse index
used to find an already-built extension chain, expression evaluation or
collection. Each `OuterComponent` MUST continue to own its own local
resource index of its advertised items (short name and state intent to
`NodeId`), its own public
import/export ports, its own child-port bindings, and its own lifecycle
state; everything else about a component's local graph view remains
component-local.

#### Scenario: Two components' nodes live in one shared store
- **WHEN** two different `OuterComponent`s each register a graph node
- **THEN** both nodes are retrievable, by their own `NodeId`, from the
  same shared node store — no second, independent node store exists for
  either component

#### Scenario: A component's local resource index remains its own
- **WHEN** two sibling `OuterComponent`s each advertise an item under
  the same short name and state intent
- **THEN** each component's own local resource index resolves that short
  name to its own component's node, with no collision or
  cross-contamination between the two components' local resource indexes

#### Scenario: A component's public ports and child-port bindings remain its own
- **WHEN** a component publishes a port or records a child-port binding
- **THEN** that port or binding is recorded only in that component's own
  per-component storage, retrievable through that component alone

#### Scenario: Two components see the same forward network
- **WHEN** two different `OuterComponent`s in one hierarchy each ask for
  the forward dependency network
- **THEN** both receive the same network identity

## ADDED Requirements

### Requirement: One forward network carries all ordinary data flow
Every dependency edge that carries data toward a consumer before the
consumer runs SHALL be added to the hierarchy's one forward dependency
network, whichever component's declaration produced the edge. This
covers ordinary connections, extension chains, expression evaluations,
collection membership edges, geometry edges and the get side of a
callback argument. The forward network SHALL be acyclic as a whole, and
every state item SHALL have at most one producer in it, across the
whole hierarchy.

#### Scenario: Edges declared by different components share one network
- **WHEN** a parent declares a connection between two of its children
  and a grandparent declares a connection from one of those children's
  exports to a component elsewhere in the hierarchy
- **THEN** both connections' edges are in the same forward network

#### Scenario: A second forward producer is rejected
- **WHEN** a state item already has a producer in the forward network
  and wiring would add another edge into that item from a different
  node in the forward network
- **THEN** the wiring fails with an error naming the item, and the
  forward network is unchanged

#### Scenario: A cycle across declaring components is rejected
- **WHEN** one component's declaration would add `X -> Y` and another
  component's declaration would add `Y -> X` to the forward network
- **THEN** the second edge is rejected and the forward network is
  unchanged

#### Scenario: A transform may have many inputs
- **WHEN** a transform node has several input items (for example an
  expression over two fields)
- **THEN** each input has an edge into the transform, and the
  transform's output item still has exactly one producer

### Requirement: Write-back edges live in separate networks that a pull never follows
An edge that carries data back to an item after a method has modified
it (the put side of a callback argument, and the return edge of an
inout borrower) SHALL be added to a write-back network of its own, never
to the forward network. A pull SHALL NOT follow a write-back network. A
write-back into an item and that item's forward producer SHALL NOT both
fire within the same pull.

#### Scenario: Callback put edge is not in the forward network
- **WHEN** a callback argument with input-and-output access is wired
- **THEN** its member-to-provider edge is in a write-back network, and
  the forward network has no edge from the member to the provider

#### Scenario: A pull ignores write-back edges
- **WHEN** an item has a forward producer and is also the target of a
  write-back edge, and the item is pulled
- **THEN** the pull follows only the forward producer

#### Scenario: Inout borrower keeps its return network
- **WHEN** an inout borrower is wired in the direct-alias case
- **THEN** its owner-to-borrower edge is in the forward network and its
  borrower-to-owner edge is in a write-back network of its own,
  unchanged from the existing ordinary-inout behavior
