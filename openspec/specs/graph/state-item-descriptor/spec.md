# state-item-descriptor Specification

## Purpose

Defines `StateItemDescriptor`, the per-node holder of a state item's
description (its characteristics map), separate from the `GraphStateItem`
ESMF handle. It also covers generic, table-driven status resolution,
the static allowlist for mirrored exports, the freeze-time check of
deferred characteristics, the MAPL-setter-only Info facade,
`VariableSpec` translation into characteristics, and the rule that
`GraphBuilder` is characteristic-agnostic.

## Requirements

### Requirement: StateItemNode owns a descriptor separate from its ESMF handle
Each `StateItemNode` SHALL own a `StateItemDescriptor` in addition to its
`GraphStateItem` payload. The descriptor SHALL contain a
`StateItemCharacteristicMap` and nothing else, and SHALL be the single
source of truth for the item's description (shape, geometry, vertical
coordinate and stagger, units, typekind, ungridded dims, attributes,
quantity type, conservation, normalization, standard name, restart mode,
expression). `GraphStateItem` SHALL hold only the ESMF handle and member
maps.

#### Scenario: Description is read only from the descriptor
- **WHEN** any graph-layer or GraphBuilder code needs a description value
  of a state item (for example its shape, ungridded dims, or restart mode)
- **THEN** it obtains it from the node's descriptor, and `GraphStateItem`
  has no component or accessor carrying that value

#### Scenario: A node without a payload still has a description
- **WHEN** a `StateItemNode` is registered at ADVERTISE before any ESMF
  object exists
- **THEN** its descriptor is populated and queryable, and its handle has
  no allocated ESMF component

### Requirement: Status resolution is generic and table-driven
The descriptor SHALL resolve each characteristic kind present on both an
export and an import using only the two statuses, per this table, before
any subclass method is called:

| export \ import | SPECIFIED | MIRRORED | UNCHECKED | DEFERRED |
|---|---|---|---|---|
| SPECIFIED | subclass comparison | copy export value to import; match | match | error |
| MIRRORED | error unless allowlisted | error | match | error |
| UNCHECKED | match | copy export value to import (import becomes UNCHECKED); match | match | error |
| DEFERRED | error | error | error | error |

Subclass comparison, Transform-building, and Info-writing methods SHALL
be invoked only when both sides are SPECIFIED. An INVALID status
encountered by the descriptor SHALL be reported as an error.

#### Scenario: Mirrored import adopts the export's value
- **WHEN** an export's TYPE_KIND is SPECIFIED as R8 and the import's
  TYPE_KIND is MIRRORED
- **THEN** the import's characteristic becomes a SPECIFIED copy of R8,
  no Transform is built for that kind, and no subclass method was called
  with a MIRRORED argument

#### Scenario: Unchecked on either side never extends
- **WHEN** either side's characteristic of a kind is UNCHECKED
- **THEN** no Transform is built for that kind and no error is raised

#### Scenario: Double mirror fails
- **WHEN** both export and import characteristics of a kind are MIRRORED
- **THEN** connection resolution fails with a diagnostic naming the kind

#### Scenario: Non-allowlisted mirrored export fails
- **WHEN** a FIELD export's GEOMETRY is MIRRORED and is connected to an
  import with SPECIFIED GEOMETRY
- **THEN** connection resolution fails with a diagnostic naming the item
  and the kind

### Requirement: Mirrored exports are admitted only through a static allowlist
A MIRRORED export characteristic SHALL be accepted only when the pair
(export shape, kind) appears in a static allowlist owned by the
descriptor module. The initial allowlist SHALL be: (EXPRESSION,
GEOMETRY), (EXPRESSION, VERTICAL_GRID), (EXPRESSION,
VERTICAL_STAGGER), (EXPRESSION, TYPE_KIND). For an allowlisted pair the
extension chain tip for that consumer SHALL take the import's value
without a Transform step, and the export's own descriptor SHALL NOT be
modified.

#### Scenario: Expression acquires the consumer's geometry
- **WHEN** an EXPRESSION export with MIRRORED GEOMETRY is connected to an
  import with SPECIFIED GEOMETRY G
- **THEN** the chain tip for that consumer has GEOMETRY G, no geometry
  Transform is inserted, and the export's GEOMETRY remains MIRRORED

### Requirement: Deferred characteristics are resolved before connection and checked at freeze
A DEFERRED characteristic SHALL become SPECIFIED through
`StateItemDescriptor%resolve_deferred` before connection resolution. At
graph freeze, any characteristic still DEFERRED SHALL cause an error
naming the item and the kind.

#### Scenario: Deferred geometry resolved at GEOM_B
- **WHEN** an item's GEOMETRY is DEFERRED at ADVERTISE and its component
  sets a geometry in GEOM_B
- **THEN** after GEOM_B the item's GEOMETRY is SPECIFIED with that
  geometry

#### Scenario: Deferred characteristic at freeze fails
- **WHEN** graph freeze runs and some item still has a DEFERRED
  characteristic
- **THEN** freeze fails with a diagnostic naming that item and kind

### Requirement: Description reaches ESMF Info only through the MAPL setters
`StateItemDescriptor%sync_to_info(handle, phase)` SHALL write each
SPECIFIED or UNCHECKED characteristic through that characteristic's own
write method, which SHALL call only `MAPL_FieldSet`,
`MAPL_FieldBundleSet`, or `MAPL_StateSet`. Structural characteristics
(geometry, vertical grid, vertical stagger, typekind, ungridded
dims, units) SHALL be written only in the STRUCTURAL phase, before the
field is completed. UNCHECKED units and standard_name SHALL be written as
`'<unknown>'`; other UNCHECKED characteristics SHALL write nothing.
MIRRORED and DEFERRED characteristics SHALL write nothing.

#### Scenario: Descriptive sync on a complete field does not reallocate
- **WHEN** `sync_to_info` runs in the DESCRIPTIVE phase on a COMPLETE
  field
- **THEN** no structural keyword (and no `units=`) is passed to
  `MAPL_FieldSet`, so its `FieldDelta` path is not entered

#### Scenario: Unchecked units are visible to Info readers
- **WHEN** an item with UNCHECKED PHYSICAL_UNITS is synced
- **THEN** reading the field's units through `MAPL_FieldGet` returns
  `'<unknown>'`

### Requirement: VariableSpec translates itself into characteristics
`VariableSpec%make_characteristics(descriptor)` SHALL populate
the descriptor by looping over a fixed list of per-characteristic factory
procedures, each of which decides applicability to the item's shape and
the characteristic's initial status. GraphBuilder SHALL call it once per
advertised item and SHALL NOT construct characteristics itself.

#### Scenario: Unspecified units become UNCHECKED, not absent
- **WHEN** a FIELD `VariableSpec` has no units (or `'<unknown>'`)
- **THEN** the descriptor holds a PHYSICAL_UNITS entry with status
  UNCHECKED

#### Scenario: Empty attributes are still present
- **WHEN** a FIELD `VariableSpec` declares no attributes
- **THEN** the descriptor holds a SPECIFIED ATTRIBUTES entry with an
  empty set

#### Scenario: Typekind mirror becomes MIRRORED, not absent
- **WHEN** a `VariableSpec` declares `MAPL_TYPEKIND_MIRROR`
- **THEN** the descriptor holds a TYPE_KIND entry with status MIRRORED

#### Scenario: Undeclared geometry is deferred to the component
- **WHEN** a FIELD `VariableSpec` declares no geometry
- **THEN** the descriptor holds a GEOMETRY entry with status DEFERRED,
  not MIRRORED

#### Scenario: Default typekind is specified
- **WHEN** a FIELD `VariableSpec` does not set a typekind
- **THEN** the descriptor holds a SPECIFIED TYPE_KIND entry with value R4

### Requirement: GraphBuilder is characteristic-agnostic
`GraphBuilder.F90` and its submodules SHALL NOT name any
`*_CHARACTERISTIC_KIND` constant or any concrete `*Characteristic` type.

#### Scenario: Grep finds no kind names in GraphBuilder
- **WHEN** `GraphBuilder*.F90` is searched for `_CHARACTERISTIC_KIND` and
  for concrete characteristic type names outside comments
- **THEN** no match is found
