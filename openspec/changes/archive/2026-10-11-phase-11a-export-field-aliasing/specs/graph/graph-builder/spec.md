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

A sibling connection's export-side resolved payload SHALL be the same
real `ESMF_Field` the export child's own run phase writes a changed
value into, rather than an independent copy - established by aliasing,
never by duplicating, the export child's own real materialized field.
A run-phase value change on the export side SHALL therefore be
observable, through this same resolved structure, on the import side -
whether the import is wired directly to the export or through one or
more extension-chain transform nodes.

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

#### Scenario: A sibling connection's export-side payload is a real alias, not a copy
- **WHEN** a sibling-to-sibling connection's export-side proxy is
  resolved and the export child's own real field has already been
  realized
- **THEN** the proxy's own payload is a named alias of that same real
  `ESMF_Field` object - not a separately allocated field with equal
  attributes

#### Scenario: A run-phase value change on the export side reaches the import side
- **WHEN** the export child's own run phase writes a new value into its
  real export field, and the resolved connection's import side is then
  read (directly, or through an extension-chain transform's
  re-evaluation)
- **THEN** the value observed on the import side reflects the export
  child's own newly written value, converted by any transform the
  connection's extension chain applies
