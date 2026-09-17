# Node Revision And Update Specification

## Purpose

Gives `NodeRevision` real advance/comparison semantics and defines the
runtime-interpreted, demand-driven algorithm that decides when a
`TransformGraphNode` must re-execute and which revisions it advances.

## Requirements

### Requirement: NodeRevision has a distinct invalid state and monotonic advance
`NodeRevision` SHALL have an initial state distinct from any value it
can reach by advancing, and SHALL support `advance()`, which
transitions the invalid state to the first valid value and otherwise
increments a valid value to the next valid value. Overflow SHALL be
detected before wrapping rather than silently producing an
earlier-looking value.

#### Scenario: Newly constructed revision is invalid
- **WHEN** a `NodeRevision` is default-constructed
- **THEN** it reports itself as invalid, distinct from every value
  reachable by calling `advance()`

#### Scenario: First advance produces a valid revision
- **WHEN** `advance()` is called on an invalid `NodeRevision`
- **THEN** the result is valid and distinct from the invalid state

#### Scenario: Repeated advance strictly changes the revision
- **WHEN** `advance()` is called on an already-valid `NodeRevision`
- **THEN** the result differs from the value before the call

#### Scenario: Overflow is detected, not silently wrapped
- **WHEN** `advance()` is called on a `NodeRevision` already at its
  maximum representable valid value
- **THEN** the call reports failure and the revision's observable value
  is unchanged

### Requirement: Revision mutation is exposed only through accessor methods
Only operations that establish or modify a node's logical value SHALL
advance its revision; read-only access, aliasing, or graph traversal
SHALL NOT. `StateItemNode` SHALL expose revision advancement as a
method rather than allowing arbitrary direct assignment of a revision
value to represent "the value changed."

#### Scenario: Advancing a node's revision through its own method
- **WHEN** `StateItemNode%advance_revision()` is called
- **THEN** a subsequent `get_revision()` call returns a revision that
  differs from the value observed before the call

#### Scenario: Querying a revision never advances it
- **WHEN** `StateItemNode%get_revision()` is called any number of times
  with no intervening mutation
- **THEN** every call returns an identical revision value

### Requirement: Demand-driven update executes a transform only when stale
Requesting an up-to-date value for a `StateItemNode` SHALL recursively
ensure every required predecessor value is current first, following
`DependencyNetwork` adjacency for the network the request is made
against. A `TransformGraphNode` reached this way SHALL execute only if
it has never executed, or if at least one of its declared inputs'
current revisions differs from the revision recorded after its
previous successful execution.

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

### Requirement: Successful execution advances all declared outputs
On successful execution, a `TransformGraphNode` SHALL advance the
revision of every declared output, and SHALL record the input
revisions used for that execution as the new baseline for future
staleness comparisons. Partial-output execution SHALL NOT be assumed.

#### Scenario: All outputs advance together
- **WHEN** a `TransformGraphNode` with more than one declared output
  executes successfully
- **THEN** every declared output's revision advances, not only the one
  originally requested

#### Scenario: Baseline reflects the most recent successful execution
- **WHEN** a `TransformGraphNode` executes successfully with a given
  set of input revisions
- **THEN** a subsequent staleness check compares against exactly that
  set of input revisions, not an earlier one

### Requirement: Update traversal is runtime-interpreted
The update algorithm SHALL be implemented by directly walking
`ComponentGraph`/`DependencyNetwork` at runtime (interpreting the graph
structure), with no compiled or pre-resolved execution path required
for correctness.

#### Scenario: Update works against an unfrozen or frozen graph
- **WHEN** demand-driven update is requested against a graph's node,
  independent of whether the graph has been frozen
- **THEN** the update completes using only `ComponentGraph`'s and
  `DependencyNetwork`'s existing public operations
