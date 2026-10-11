## MODIFIED Requirements

### Requirement: `graph` mode makes graph-native population the real producer
In `graph` mode, the system SHALL treat graph-native state population as
the sole real producer of a component's `ESMF_State` entries for that
component; the legacy imperative coupler's own population step SHALL be
skipped entirely for that component. Only one producer SHALL ever insert
a given name into one `ESMF_State`, by construction, regardless of which
mode is selected. This holds equally whether the item being populated
is advertised and connected entirely within one component, or is one
side of a connection between two sibling children that some ancestor
component declared: in either case, exactly one producer - graph-native
population, reaching into whichever component's own `ESMF_State`
actually owns the item - populates it, and the legacy imperative
coupler never does, for any component whose own graph mode is `graph`.

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

#### Scenario: Sibling children are each the sole producer of their own state
- **WHEN** two sibling children, each with its own graph mode reporting
  `graph`, are connected by an ordinary connection their parent
  declared
- **THEN** graph-native population is the only producer that inserts
  into either child's own real `ESMF_State` for that connection's
  resolved item, and the legacy imperative coupler does not populate
  either child's own state for that item
