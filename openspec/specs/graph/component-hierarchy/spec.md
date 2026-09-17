## Purpose

Defines the ownership shape and encapsulation boundary of the MAPL
component hierarchy (`OuterComponent`) with respect to the graph-neutral
core: what each component owns, what a parent may see of a child, and
what storage exists for cross-graph boundary bookkeeping.

## Requirements

### Requirement: OuterComponent ownership shape
Every wrapped user component MUST be represented by an `OuterComponent`
that owns: a driver for its own user component, one driver per child
component, exactly one local dependency graph, framework-managed import,
export, and internal states, and a set of public ports visible to its
parent.

#### Scenario: New component owns its required parts
- **WHEN** a new `OuterComponent` is constructed for a user component
- **THEN** it exposes its own user-component driver, an empty child-driver
  collection, exactly one local dependency graph, and framework-managed
  import/export/internal states, all accessible through read-only queries

#### Scenario: Adding a child does not add a second local graph
- **WHEN** a child component is added to an `OuterComponent`
- **THEN** the parent still owns exactly one local dependency graph, and
  the child owns its own separate local dependency graph

### Requirement: One local graph per component, no global graph
Each `OuterComponent` MUST own exactly one local dependency graph. There
MUST NOT be a single global graph instance shared across the hierarchy.

#### Scenario: Sibling components have distinct graphs
- **WHEN** two sibling `OuterComponent`s exist under the same parent
- **THEN** each has its own local dependency graph, and no operation on
  one component's graph is visible through the other's graph query API

#### Scenario: No global graph accessor exists
- **WHEN** a caller queries the public API of an `OuterComponent` or the
  hierarchy as a whole
- **THEN** no operation returns a graph spanning more than one
  component's local graph

### Requirement: Parent-child encapsulation boundary
A parent `OuterComponent` MAY query a child's public ports and its
driver. A child `OuterComponent` MUST NOT be able to query its parent's
graph, ports, or driver through any public API.

#### Scenario: Parent reads child's public ports
- **WHEN** a parent `OuterComponent` queries a named child's public ports
- **THEN** the parent receives the child's published ports without error

#### Scenario: Child has no path to its parent
- **WHEN** a component's public API is inspected for any operation that
  accepts or returns a reference to its parent
- **THEN** no such operation exists

#### Scenario: Parent cannot see child's internal graph
- **WHEN** a parent `OuterComponent` queries a child's local dependency
  graph directly, bypassing the child's public ports
- **THEN** no public API accepts a child reference and returns the
  child's internal graph, node identities, or dependency-network
  identities

### Requirement: Proxy-node storage for cross-graph boundaries
An `OuterComponent` MUST make available, through its local dependency
graph, storage capable of holding, per child, boundary or proxy graph
nodes that stand in for that child's published ports in the parent's
local graph. This storage MUST be reachable only through the framework's
internal graph-construction path, never through a user-facing or
component-author-facing API.

#### Scenario: New component has empty proxy-node storage
- **WHEN** a new `OuterComponent` is constructed
- **THEN** its local graph's proxy-node storage exists and initially
  holds no entries

#### Scenario: Proxy-node storage is not user-visible
- **WHEN** the user-facing/component-author-facing API of an
  `OuterComponent` is inspected
- **THEN** no operation in that surface exposes proxy-node identities or
  contents; only the framework's internal graph-construction path can
  reach the storage

#### Scenario: Proxy-node storage remains empty absent population logic
- **WHEN** a child is added to an `OuterComponent` but no
  graph-construction step has run to populate proxy nodes for it
- **THEN** the parent's proxy-node storage still holds no entries for
  that child
