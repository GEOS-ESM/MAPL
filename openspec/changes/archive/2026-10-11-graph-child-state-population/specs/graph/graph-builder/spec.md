## MODIFIED Requirements

### Requirement: Graph-native state population populates a real ESMF state from resolved graph structure
The system SHALL provide a graph-native equivalent of the legacy
imperative coupler's own state-population step: given a component's
resolved graph structure, it SHALL be able to insert each resolved,
field-typed advertised item's real materialized field into a real
`ESMF_State`, under that item's own short name, resolving an import's
real field by following its dependency edge back to whichever node
actually holds the materialized payload (the matching export directly,
or an extension chain's final node). This population step SHALL only be
invoked as a component's real state producer when the graph mode is
`graph`; in `registry` or `both` mode, it SHALL NOT insert anything into
the real `ESMF_State` the legacy coupler or any `check()`-style assertion
observes.

When the resolved connection is between two of the invoking component's
own children (a sibling-to-sibling connection the invoking component
itself declared, but did not itself advertise either endpoint of), the
system SHALL insert each child's own resolved field into that same
child's own real `ESMF_State` - the state its own GridComp actually
observes - never into the invoking (parent) component's own state. A
child component SHALL NOT be required to have, or exercise, any
awareness of its parent for this insertion to occur; the parent alone
is responsible for reaching into each child's own state.

#### Scenario: An exact-match import's real field is found via its export
- **WHEN** graph-native state population runs for a component whose
  import is wired by a direct dependency edge to a matching export's
  materialized node
- **THEN** the real field inserted for that import under its own short
  name is the same field object materialized on the export's node

#### Scenario: Graph mode governs whether population touches the real state
- **WHEN** graph-native state population is invoked for a component
  whose graph mode is `registry` or `both`
- **THEN** no item is inserted into the real `ESMF_State` as a result of
  that invocation

#### Scenario: Unresolved import is not populated
- **WHEN** graph-native state population runs for a component with an
  import that has no resolved dependency edge
- **THEN** no entry is inserted into the real `ESMF_State` for that
  import, and no error is raised solely because of its absence

#### Scenario: A sibling connection's resolved field reaches each child's own state
- **WHEN** graph-native state population runs for a parent component
  that declared an ordinary connection between two of its own children,
  and that connection is resolved
- **THEN** the export child's own real `ESMF_State` contains the
  resolved export field under its own short name, and the import
  child's own real `ESMF_State` contains the resolved import field
  under its own short name - neither field is inserted into the
  parent's own real `ESMF_State`

#### Scenario: A child needs no awareness of its parent to be populated
- **WHEN** a sibling connection's resolved field is inserted into a
  child's own real `ESMF_State` by graph-native state population
- **THEN** that insertion requires no accessor, lookup, or other
  reference from the child's own graph or component state back to its
  parent

## ADDED Requirements

### Requirement: A sibling connection's extension chain is re-evaluated after a run phase
After a component's own run phase completes, for each of that
component's own declared children, the system SHALL re-evaluate every
dependency that child's own advertised import items have on an
extension-chain transform, using the same demand-driven update
mechanism already defined for transform re-evaluation. This
re-evaluation SHALL happen only when the graph mode is `graph`; it
SHALL NOT invoke, construct, or otherwise schedule any method node, and
SHALL NOT cause any GridComp phase to execute a second time.

This requirement covers the re-evaluation mechanism itself, not
whether a run phase's own newly-written value is connected to that
mechanism's own input - whether a changed value actually reaches a
sibling's own extension-chain transform depends on that transform's
own input node already holding the component's real, run-phase-written
field, which is a separate concern from re-evaluation timing.

#### Scenario: Re-evaluation runs after a run phase completes
- **WHEN** a component's own run phase completes while its graph mode
  is `graph`
- **THEN** the system re-evaluates every one of that component's own
  declared children's advertised import items for staleness, using the
  existing demand-driven update mechanism

#### Scenario: Re-propagation never invokes a method node or a GridComp phase
- **WHEN** a component's own run phase completes and re-evaluation
  runs for its own declared children
- **THEN** no method node is invoked, and no GridComp phase executes,
  as a result of that re-evaluation

#### Scenario: Re-propagation is a no-op outside graph mode
- **WHEN** a component's own run phase completes while its graph mode
  is `registry` or `both`
- **THEN** no re-evaluation occurs, and no sibling's own real
  `ESMF_State` is touched as a result
