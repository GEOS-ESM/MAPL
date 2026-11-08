## MODIFIED Requirements

### Requirement: Demand-driven update executes a transform only when stale
Requesting an up-to-date value for a `StateItemNode` SHALL name only
the item. It SHALL recursively ensure every required predecessor value
is current first, following the hierarchy's forward network, whichever
component's declaration added each edge and whichever component makes
the request. Write-back networks SHALL NOT be followed. A
`TransformGraphNode` reached this way SHALL execute only if it has never
executed, or if at least one of its declared inputs' current revisions
differs from the revision recorded after its previous successful
execution.

#### Scenario: First request executes the transform
- **WHEN** an output of a `TransformGraphNode` that has never executed
  is requested
- **THEN** the transform's execution strategy is invoked exactly once

#### Scenario: Repeated request with unchanged inputs does not re-execute
- **WHEN** an output of a `TransformGraphNode` is requested a second
  time and none of its declared inputs' revisions have changed since
  its last successful execution
- **THEN** the transform's execution strategy is not invoked again

#### Scenario: Changed input triggers re-execution
- **WHEN** a declared input's revision has advanced since a
  `TransformGraphNode`'s last successful execution, and one of the
  transform's outputs is subsequently requested
- **THEN** the transform's execution strategy is invoked again

#### Scenario: Upstream chain updates before a downstream transform runs
- **WHEN** a requested `TransformGraphNode`'s declared input is itself
  produced by another `TransformGraphNode` whose own inputs have
  changed
- **THEN** the upstream transform executes and advances its output
  before the downstream transform's staleness is evaluated

#### Scenario: Result does not depend on who asks
- **WHEN** the same item is requested once through the graph of the
  component that declared its producing edge and once through the graph
  of the component that owns the item
- **THEN** both requests reach the same producer and give the same
  result
