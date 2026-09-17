# Transform Graph Node Specification

## Purpose

Represents demand-driven MAPL Transforms as graph nodes with named,
declared input/output ports, and defines where the concrete port
bindings for a given wiring live, independent of node/dependency
storage.

## Requirements

### Requirement: TransformGraphNode is a concrete operation node
`TransformGraphNode` SHALL be a concrete type extending
`OperationGraphNode`, representing a computation with an injectable
execution strategy, executed only when explicitly triggered by the
update algorithm rather than eagerly at construction or wiring time.

#### Scenario: TransformGraphNode is instantiable
- **WHEN** code constructs a `TransformGraphNode` with an execution
  strategy attached
- **THEN** the instance is created successfully and inherits its node
  id and lifecycle status from `OperationGraphNode`/`BaseGraphNode`
  with no separate identity mechanism

#### Scenario: Execution does not happen implicitly
- **WHEN** a `TransformGraphNode` is constructed and wired into a graph
  but never explicitly asked to execute or update
- **THEN** its attached execution strategy is never invoked

### Requirement: Named, multi-input/multi-output port declarations
A `TransformGraphNode` SHALL support declaring any number of named
input ports and any number of named output ports, each identified by a
name distinct within its own direction (input or output), independent
of any ESMF State import/export naming convention. A port declaration
MAY optionally constrain its expected `GraphStateItem` kind.

#### Scenario: Multiple named inputs and outputs
- **WHEN** a `TransformGraphNode` declares input ports
  `source_field`, `source_vertical_grid`, `destination_vertical_grid`
  and an output port `destination_field`
- **THEN** all four declared ports are individually retrievable by name
  and by direction (input vs. output)

#### Scenario: Duplicate port name in the same direction is rejected
- **WHEN** a second input port is declared with a name already used by
  an existing input port declaration on the same node
- **THEN** the declaration fails and the original declaration is
  unchanged

#### Scenario: Declared port kind is queryable
- **WHEN** a port is declared with an expected `GraphStateItem` kind
  constraint
- **THEN** the constraint is retrievable from the port's declaration
  without inspecting any bound value

### Requirement: Port bindings are stored externally, keyed by network and node
Port *bindings* (which concrete `NodeId` fills a named port, for a
specific `DependencyNetworkId`) SHALL be stored external to
`TransformGraphNode`, keyed by the pair `(DependencyNetworkId,
NodeId)` and mapping each bound port name to a `NodeId`. Port
*declarations* (the named-argument specification itself) remain
on-node metadata (see previous requirement) and are not affected by
this requirement.

#### Scenario: Same transform node bound differently in two networks
- **WHEN** the same `TransformGraphNode` participates in two different
  `DependencyNetwork`s and is bound to a different `NodeId` for the
  same declared port name in each
- **THEN** each network's binding is independently retrievable and
  neither overwrites the other

#### Scenario: Binding requires a matching port declaration
- **WHEN** a binding is attempted for a port name the target
  `TransformGraphNode` has not declared as an input or output
- **THEN** the binding is rejected

#### Scenario: Binding a kind-constrained port to a mismatched value is rejected
- **WHEN** a port declared with an expected `GraphStateItem` kind is bound
  to a `NodeId` whose `StateItemNode` payload reports a different kind
- **THEN** the binding is rejected

#### Scenario: Rebinding an already-bound port name is rejected
- **WHEN** a second binding is attempted for a port name already bound
  for the same `(DependencyNetworkId, NodeId)` pair
- **THEN** the binding is rejected and the original binding is
  unchanged

#### Scenario: Missing binding degrades gracefully
- **WHEN** a caller queries the binding for a declared port name that
  has not yet been bound in a given network
- **THEN** the query reports "no binding" rather than failing
