## Purpose

Defines the single, component-wide selector that decides which producer
— the legacy imperative coupler, the graph-native path, or both running
side by side with no shared-state collision — populates a component's
real `ESMF_State` entries, giving every later Phase 10 validation
sub-change a safe way to compare graph-native behavior against legacy
without risking a duplicate-insertion conflict in a real `ESMF_State`.

## ADDED Requirements

### Requirement: A single selector governs which producer populates real state
The system SHALL expose exactly one, component-wide selector with three
mutually exclusive values — `registry`, `graph`, and `both` — governing
which mechanism is treated as the real producer of a component's
`ESMF_State` entries. The default value SHALL be `registry`, so that no
real production run observes any behavior change unless something
explicitly selects a different value.

#### Scenario: Default mode preserves today's behavior
- **WHEN** no caller has ever selected a graph mode for a component
- **THEN** the selector reports `registry`, and only the legacy
  imperative coupler populates that component's real `ESMF_State`

#### Scenario: Selecting a mode is observable
- **WHEN** a caller selects `graph` or `both` for the selector
- **THEN** a subsequent query of the selector reports that same value

### Requirement: `registry` mode is unchanged legacy behavior
In `registry` mode, the system SHALL NOT perform graph-native
materialization of ordinary state items or graph-native state
population; only the legacy imperative coupler populates the real
`ESMF_State`.

#### Scenario: No graph-native materialization in registry mode
- **WHEN** a component resolves its ordinary connections while the
  selector reports `registry`
- **THEN** no ordinary `StateItemNode` is given a real field as a result
  of that resolution

### Requirement: `both` mode runs both producers without a shared-state collision
In `both` mode, the system SHALL run graph-native materialization
alongside the legacy imperative coupler, but SHALL NOT insert any
graph-materialized data into the shared `ESMF_State` that the legacy
coupler populates and that `check()`-style assertions observe. A
graph-materialized item's value SHALL instead be reachable only by
reading its owning node's payload directly, out of band from the shared
`ESMF_State`.

#### Scenario: Both producers can be compared without a name collision
- **WHEN** a component resolves its ordinary connections while the
  selector reports `both`
- **THEN** the legacy imperative coupler's real `ESMF_State` contains
  exactly the entries it would contain in `registry` mode, with no
  additional or duplicate entry inserted by graph-native materialization

#### Scenario: Graph-materialized value is readable out of band
- **WHEN** a component resolves its ordinary connections while the
  selector reports `both`
- **THEN** the graph-materialized value for a resolved item is readable
  directly from its owning node's payload, independent of the shared
  `ESMF_State`

### Requirement: `graph` mode makes graph-native population the real producer
In `graph` mode, the system SHALL treat graph-native state population as
the sole real producer of a component's `ESMF_State` entries for that
component; the legacy imperative coupler's own population step SHALL be
skipped entirely for that component. Only one producer SHALL ever insert
a given name into one `ESMF_State`, by construction, regardless of which
mode is selected.

#### Scenario: Legacy population is skipped in graph mode
- **WHEN** a component's selector reports `graph`
- **THEN** the legacy imperative coupler's own state-population step
  does not run for that component, and graph-native population is the
  only producer of its real `ESMF_State` entries

#### Scenario: No mode ever produces a double insertion
- **WHEN** a component's real `ESMF_State` entries are populated under
  any selector value
- **THEN** no state entry name is ever inserted by more than one
  producer
