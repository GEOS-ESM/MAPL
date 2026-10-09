## MODIFIED Requirements

### Requirement: Ordinary connections are resolved into dependency edges
For each ordinary (exact short-name match, non-wildcard, non-callback)
import/export connection declared between two components, the system
SHALL determine whether the export's payload matches what the import
requires. If it matches, the system SHALL add a dependency edge, in the
consuming component's default dependency network, directly from the
export's node to the matching import's node, and SHALL materialize a
real field payload on the export's node when the graph mode (see the
graph-mode capability) is `graph` or `both`. If it does not match, the
system SHALL delegate to the extension-reuse capability to interpose a
transform chain rather than wiring the mismatched pair directly. This is
the real-wiring step and SHALL occur only once graph mutation is
appropriate (see design.md for the exact lifecycle point) - not merely
once activity/need has been determined.

#### Scenario: Matching export and import are wired
- **WHEN** a destination component's import has the same short name as
  a source component's export, an ordinary connection between the two
  components is declared, and the export's payload already matches what
  the import requires
- **THEN** the destination's default dependency network contains a
  dependency edge directly from the export's node to the import's node,
  with no extension chain interposed

#### Scenario: Matching export is materialized with a real field
- **WHEN** a destination component's import has the same short name as
  a source component's export, an ordinary connection between the two
  components is declared, the export's payload already matches what the
  import requires, and the graph mode is `graph` or `both`
- **THEN** the export's node reports a real, allocated field payload,
  rather than the unallocated placeholder it reported before resolution

#### Scenario: Matching export is not materialized in registry mode
- **WHEN** a destination component's import has the same short name as
  a source component's export, an ordinary connection between the two
  components is declared, the export's payload already matches what the
  import requires, and the graph mode is `registry`
- **THEN** the export's node still reports the unallocated placeholder
  payload it reported before resolution, unchanged from today's behavior

#### Scenario: Mismatched export and import are wired through an extension chain
- **WHEN** a destination component's import has the same short name as
  a source component's export, an ordinary connection between the two
  components is declared, and the export's payload does not match what
  the import requires
- **THEN** this capability delegates to the extension-reuse capability
  rather than adding a dependency edge directly from the export's node
  to the import's node

#### Scenario: Import with no matching export is left unresolved
- **WHEN** a destination component's import has no export of the same
  short name available from the declared source
- **THEN** no dependency edge is created for that import, and the
  unresolved import is reported rather than silently ignored

#### Scenario: Non-exact-match cases are not resolved by this capability
- **WHEN** a connection requires wildcard expansion or callback-style
  binding between export and import
- **THEN** this capability does not create a dependency edge for that
  connection and does not report it as an ordinary-connection failure —
  such connections remain the responsibility of a later resolution step

## ADDED Requirements

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
