## Purpose

Gives an ordinary component's own run phase a real graph-native
construction counterpart - exactly one method node per component,
keyed to that component's own advertised ports - so a future change
can wire real invocation on top of it once invocation-safety
prerequisites (a legacy-run-call skip mechanism; working
`GRAPH_MODE_GRAPH`-alone population) are separately resolved. This
capability covers construction only; invocation is explicitly out of
scope (see proposal.md Non-Goals).

## ADDED Requirements

### Requirement: Exactly one run-phase method node per component
For each component the graph builder processes under a non-default
graph mode, the system SHALL construct exactly one method node
representing that component's own run phase. The node's argument
declarations and bindings SHALL be derived from that component's own
advertised import and export items only; the node SHALL NOT be
constructed from, or require, any callback-interface declaration.

#### Scenario: One node constructed per component
- **WHEN** the graph builder processes a component that advertises at
  least one import or export item, under a non-default graph mode
- **THEN** exactly one run-phase method node is constructed for that
  component, with no second node constructed for the same component's
  run phase

#### Scenario: Node arguments mirror the component's own advertised items
- **WHEN** the run-phase method node is constructed for a component
  advertising a given set of import and export items
- **THEN** the node's declared arguments correspond to that same set of
  items, with no argument sourced from a callback-interface declaration

#### Scenario: Re-processing a component does not duplicate its node
- **WHEN** the graph builder processes the same component a second
  time after its run-phase method node already exists
- **THEN** no second node is constructed, and the existing node is
  left unchanged

### Requirement: Construction is gated by the existing graph-mode selector
Constructing a component's run-phase method node SHALL happen only
when that component's graph-mode selector reports a value other than
the default (`registry`). When the selector reports the default
value, no run-phase method node SHALL be constructed for that
component.

#### Scenario: No construction under the default graph mode
- **WHEN** a component's graph-mode selector reports its default value
- **THEN** no run-phase method node is constructed for that component

#### Scenario: Construction occurs under a non-default graph mode
- **WHEN** a component's graph-mode selector reports a value other than
  the default
- **THEN** a run-phase method node is constructed for that component

### Requirement: Construction does not invoke the underlying run phase
Constructing a component's run-phase method node SHALL NOT, by itself,
cause that component's underlying GridComp run phase to execute, and
SHALL NOT alter any effect the component's own legacy run call would
otherwise have.

#### Scenario: Construction has no run-time side effect
- **WHEN** a component's run-phase method node is constructed
- **THEN** the component's underlying GridComp run phase does not
  execute as a result, and no change is observable in any state the
  component's own legacy run call would otherwise produce
