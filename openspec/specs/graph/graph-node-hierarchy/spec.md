# graph-node-hierarchy Specification

## Purpose

Defines the `GraphNode` type hierarchy — the abstract node interface and
its concrete/abstract descendants — that every graph-neutral node kind
(state-holding or operation) is built on, including identity storage and
lifecycle status, but deliberately excluding adjacency.

## Requirements

### Requirement: GraphNode is abstract and data-free
`GraphNode` SHALL be an abstract type defining identity/dispatch
contracts only. It SHALL NOT declare any stored data component.

#### Scenario: No concrete instance of GraphNode
- **WHEN** code attempts to declare a variable of dynamic type
  `GraphNode` directly (not one of its concrete descendants)
- **THEN** compilation fails, because `GraphNode` is abstract

#### Scenario: No stored state on GraphNode
- **WHEN** the `GraphNode` type definition is inspected
- **THEN** it declares no data components (only deferred/abstract
  bindings)

### Requirement: BaseGraphNode holds only common properties
`BaseGraphNode` SHALL contain exactly the properties common to every
graph node: a lifecycle-status property and its own `NodeId`. It SHALL
NOT contain adjacency information (predecessors/successors).

#### Scenario: BaseGraphNode exposes its own NodeId
- **WHEN** a `BaseGraphNode` (or any descendant) is queried via an
  accessor method for its node id
- **THEN** it returns the `NodeId` that was assigned to it at creation,
  with no public path to construct or overwrite that id afterward

#### Scenario: NodeId set once at creation
- **WHEN** a `BaseGraphNode` has had its `NodeId` assigned during
  creation
- **THEN** no public operation on the node subsequently changes the
  value returned by its node-id accessor

#### Scenario: No adjacency storage on BaseGraphNode
- **WHEN** the `BaseGraphNode` type definition is inspected
- **THEN** it declares no predecessor/successor or other adjacency-list
  component; adjacency is represented exclusively outside the node type

### Requirement: StateItemNode holds exactly one payload and one revision
`StateItemNode` SHALL be a concrete type, extending `BaseGraphNode`,
containing exactly one payload value and one revision-tracking value in
addition to the properties it inherits from `BaseGraphNode`. It SHALL
NOT contain adjacency information.

#### Scenario: StateItemNode is instantiable
- **WHEN** code declares and creates a `StateItemNode` instance
- **THEN** the instance is constructed successfully (concrete type, not
  abstract)

#### Scenario: StateItemNode exposes exactly one payload
- **WHEN** a `StateItemNode` instance is queried for its payload
- **THEN** exactly one payload value is returned, consistent for the
  lifetime of the node until explicitly replaced

#### Scenario: StateItemNode exposes exactly one revision
- **WHEN** a `StateItemNode` instance is queried for its revision-tracking
  value
- **THEN** exactly one such value is returned

### Requirement: OperationGraphNode is an abstract "something that runs"
`OperationGraphNode` SHALL be an abstract type, extending
`BaseGraphNode`, distinct from `StateItemNode` in representing an
executable operation rather than a held value.

#### Scenario: No concrete instance of OperationGraphNode
- **WHEN** code attempts to declare a variable of dynamic type
  `OperationGraphNode` directly
- **THEN** compilation fails, because `OperationGraphNode` is abstract

#### Scenario: OperationGraphNode inherits BaseGraphNode identity
- **WHEN** a concrete descendant of `OperationGraphNode` is queried for
  its node id or lifecycle status
- **THEN** it answers via the properties inherited from `BaseGraphNode`,
  with no separate/duplicate identity mechanism

### Requirement: No separate node type represents a component
The `GraphNode` hierarchy SHALL NOT include a subclass representing "the
component" as a distinct graph-node kind. A component is represented
only indirectly, through its operation nodes and its state-item nodes.

#### Scenario: No component-kind node exists
- **WHEN** the set of concrete/abstract types in the `GraphNode`
  hierarchy is enumerated
- **THEN** no type among them purports to represent "a component" itself
  as opposed to one of its state items or operations
