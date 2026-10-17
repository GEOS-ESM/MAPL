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

### Requirement: One shared hierarchy-wide graph store, with per-component local state
There SHALL be exactly one shared store of graph nodes, keyed by
`NodeId`, and one shared store of dependency networks, keyed by
`DependencyNetworkId`, spanning the entire component hierarchy under one
root. Each `OuterComponent` MUST continue to own its own local resource
index, its own public import/export ports, its own child-port bindings,
and its own lifecycle state, unaffected by this requirement — only node
and dependency-network storage are shared; everything else about a
component's local graph view remains exactly as component-local as
before.

#### Scenario: Two components' nodes live in one shared store
- **WHEN** two different `OuterComponent`s each register a graph node
- **THEN** both nodes are retrievable, by their own `NodeId`, from the
  same shared node store — no second, independent node store exists for
  either component

#### Scenario: A component's local resource index remains its own
- **WHEN** two sibling `OuterComponent`s each advertise an item under
  the same short name and state intent
- **THEN** each component's own local resource index resolves that
  short name to its own component's node, with no collision or
  cross-contamination between the two components' local indexes

#### Scenario: A component's public ports and child-port bindings remain its own
- **WHEN** a component publishes a port or records a child-port binding
- **THEN** that port or binding is recorded only in that component's own
  per-component storage, retrievable through that component alone

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

### Requirement: Cross-component dependency wiring uses real node identities directly
A dependency edge connecting a node owned by one component to a node
owned by a different component SHALL be added directly, using each
node's own real `NodeId`, with no intermediate proxy or boundary node
standing in for either endpoint.

#### Scenario: A cross-component edge references both real endpoints directly
- **WHEN** a dependency edge is added between a node owned by component
  A and a node owned by component B
- **THEN** the edge's recorded source and target identities are A's and
  B's own real `NodeId`s, not identities of any intermediate node
  created to stand in for either one

#### Scenario: No proxy node is created for a cross-component edge
- **WHEN** a cross-component dependency edge is added
- **THEN** no new graph node is created solely to represent one side of
  that edge in the other component's own local storage

### Requirement: Freeze is a single hierarchy-wide operation
Freezing the graph SHALL be one operation applied once to the entire
hierarchy under a given root, rather than an operation each
`OuterComponent` invokes independently for its own local graph state.
After this hierarchy-wide freeze, no node, dependency, port, member, or
network anywhere in the hierarchy MAY be added or structurally changed,
while runtime payload values and revisions MAY still be updated in
place, unchanged from the per-component freeze semantics this
requirement replaces.

#### Scenario: One freeze call covers the whole hierarchy
- **WHEN** the hierarchy-wide freeze operation is invoked once, at the
  root of a fully-advertised and fully-wired hierarchy
- **THEN** every component's nodes, dependency networks, and ports
  across the entire hierarchy report themselves frozen afterward

#### Scenario: A component cannot be frozen independently of its hierarchy
- **WHEN** a hierarchy-wide freeze has not yet been invoked
- **THEN** no individual component's own graph state reports itself
  frozen, even if that component's own advertising and wiring work is
  already complete

### Requirement: Connection endpoint resolution never resolves upward past its declaring component
Resolving a connection's declared source or destination component name,
starting from the component on which the connection is declared, SHALL
only ever resolve to that declaring component itself or to one of its
descendants. It SHALL NOT resolve to an ancestor of the declaring
component, nor to any component that is neither the declaring component
nor one of its descendants.

#### Scenario: A connection resolves to the declaring component or a descendant
- **WHEN** a connection declared on component P names a source or
  destination component
- **THEN** that name resolves only to P itself or to some descendant of
  P, never to anything outside that set

#### Scenario: A connection cannot name an ancestor
- **WHEN** a connection declared on component P names a component that
  is an ancestor of P, or a component that is neither P nor a descendant
  of P
- **THEN** resolution fails to find that name, rather than resolving to
  the ancestor or unrelated component

### Requirement: Component names are unique across the entire hierarchy
Every component's name SHALL be unique across the entire hierarchy
rooted at a given top-level component, not merely unique among its own
siblings. This uniqueness SHALL be checked and enforced at the point a
child component is added, rather than left as an unenforced convention.

#### Scenario: Adding a child with a name already used elsewhere in the hierarchy fails
- **WHEN** a child component is added whose name already belongs to some
  other component anywhere in the hierarchy, not only among the new
  child's own siblings
- **THEN** the add operation fails, reporting the name collision, rather
  than succeeding and leaving two components sharing one name

#### Scenario: Sibling-unique but hierarchy-duplicate names are rejected
- **WHEN** a child component's name does not collide with any of its own
  siblings' names, but does collide with a component's name elsewhere in
  the hierarchy (for example, a cousin several levels removed)
- **THEN** the add operation still fails — sibling-level uniqueness alone
  is not sufficient

### Requirement: Composite member items are not independently nameable in a connection
A member of a composite declaration (`composite-state-spec`) SHALL NOT
be reachable as an independent connection endpoint by name. Only a
composite declaration's own top-level advertised name MAY appear as a
connection's source or destination; none of its internal members, at any
nesting depth, is a separately resolvable connection endpoint.

#### Scenario: A composite's top-level name is a valid connection endpoint
- **WHEN** a connection names a component's advertised composite item by
  its own top-level short name
- **THEN** that name resolves to the composite's own top-level node,
  exactly as an ordinary (non-composite) item would

#### Scenario: A composite's internal member name is not a valid connection endpoint
- **WHEN** a connection attempts to name one of a composite's internal
  members directly (rather than the composite's own top-level name)
- **THEN** that name does not resolve as a connection endpoint — the
  member remains reachable only through the callback-wiring capability's
  own member-resolution path, not through ordinary connection
  declaration
</content>
