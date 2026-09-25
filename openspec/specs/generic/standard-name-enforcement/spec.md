# generic/standard-name-enforcement Specification

## Purpose

Enforces agreement of the `standard_name` descriptive metadata between a
connected Import and Export, and uses that agreed `standard_name` to fill in
`long_name`/`units` from the `FieldDictionary`, with a runtime switch to relax
enforcement while dictionary coverage is incomplete.

## Requirements

### Requirement: Connected Import and Export standard_name values must agree
When both an Import and its connected Export declare a (non-wildcard)
`standard_name`, the two values SHALL be equal. Whether a disagreement is
reported as a fatal error or a warning is governed by the active
`ValidationMode` (see the runtime switch requirement below).

#### Scenario: Matching standard_name connects without a diagnostic
- **WHEN** an Export declares `standard_name: "surface_temperature"` and a
  connected Import declares `standard_name: "surface_temperature"`
- **THEN** the connection succeeds and no mismatch diagnostic is produced

#### Scenario: Disagreeing standard_name is a fatal error in strict mode
- **WHEN** an Export declares `standard_name: "surface_temperature"`, a
  connected Import declares `standard_name: "air_temperature"`, and the
  active `ValidationMode` is `STRICT`
- **THEN** the connection fails with an error identifying both declared
  values and the components/fields involved

#### Scenario: Disagreeing standard_name is a warning in permissive mode
- **WHEN** an Export declares `standard_name: "surface_temperature"`, a
  connected Import declares `standard_name: "air_temperature"`, and the
  active `ValidationMode` is `PERMISSIVE`
- **THEN** the connection succeeds and a warning identifying both declared
  values is logged

### Requirement: Vector standard_name agreement is checked per component
A Vector `VarSpec`'s `standard_name` encodes two independent component names
(for example `"(eastward_wind,northward_wind)"`). A connected Vector Import
and Vector Export SHALL have their `standard_name` agreement checked
component-by-component (component 1 against component 1, component 2 against
component 2), using the same agreement/wildcard/warning rules as a scalar
Field, rather than comparing the two encoded strings as a single opaque
value. Since a Vector can only connect to another Vector, this is a
refinement of - not an exception to - the scalar agreement requirement above.

This requirement does not address how `standard_name` should change when a
vector is rotated between basis kinds (for example CF's `eastward_wind`/
`northward_wind` vs. `x_wind`/`y_wind` for a grid-relative basis) - this is
deferred; see the follow-up tracked against basis-aware canonicalization.

#### Scenario: Matching per-component standard_name connects without a diagnostic
- **WHEN** a Vector Export declares `standard_name: "(eastward_wind,northward_wind)"`
  and a connected Vector Import declares the identical `standard_name`
- **THEN** the connection succeeds and no mismatch diagnostic is produced

#### Scenario: A single component's disagreement is a fatal error in strict mode
- **WHEN** a Vector Export declares `standard_name: "(eastward_wind,northward_wind)"`,
  a connected Vector Import declares `standard_name: "(eastward_wind,upward_air_velocity)"`
  (component 1 agrees, component 2 disagrees), and the active `ValidationMode`
  is `STRICT`
- **THEN** the connection fails with an error identifying the disagreeing
  component and both of its declared values

### Requirement: An Import may declare a wildcard standard_name
An Import SHALL be able to declare a wildcard `standard_name` that is
considered to agree with any Export `standard_name` (including an Export
that declares none), regardless of the active `ValidationMode`.

#### Scenario: Wildcard Import accepts any Export standard_name
- **WHEN** an Import declares a wildcard `standard_name` and a connected
  Export declares `standard_name: "surface_temperature"`
- **THEN** the connection succeeds and no mismatch diagnostic is produced,
  in either `ValidationMode`

### Requirement: An Export without a declared standard_name triggers a warning when its Import declares one
When an Import declares a `standard_name` (non-wildcard) but the connected
Export declares none, the system SHALL log a warning identifying the Import's
declared `standard_name` and the unnamed Export, independent of the active
`ValidationMode`. This is a warning, never a fatal error, since a data-source
Export may legitimately have no YAML-declared metadata (extending this to
extdata-yaml-supplied Exports is explicitly deferred).

#### Scenario: Import declares standard_name, Export declares none
- **WHEN** an Import declares `standard_name: "surface_temperature"` and a
  connected Export declares no `standard_name`
- **THEN** the connection succeeds and a warning identifying the Import's
  declared `standard_name` is logged

#### Scenario: Neither side declares a standard_name
- **WHEN** neither an Import nor its connected Export declares a
  `standard_name`
- **THEN** the connection succeeds and no standard-name-related diagnostic is
  produced

### Requirement: A VarSpec that declares a plain (non-compound) standard_name defaults long_name and units from the FieldDictionary
When a `VarSpec` of a required item type (Field, Vector, Bracket, or
VectorBracket) declares a plain `standard_name` (not the Vector compound
`"(name1,name2)"` encoding - see below) that has an entry in the
`FieldDictionary`, any `long_name`/`units` not explicitly declared on that
`VarSpec` SHALL default to the `FieldDictionary` entry's values. An
explicitly-declared `long_name`/`units` on the `VarSpec` always overrides the
dictionary value. FieldBundle, State, Wildcard, Expression, and Service item
types are exempt from this requirement (they carry no `standard_name` of
their own to look up).

A Vector `VarSpec`'s compound-encoded `standard_name` is exempt from this
defaulting: `long_name`/`units` are single values shared by both vector
components, and the two components generally require different dictionary
entries (see the per-component agreement requirement above), so there is no
single well-defined dictionary lookup to default a shared `long_name`/`units`
from.

#### Scenario: long_name and units filled in from the dictionary
- **WHEN** a Field `VarSpec` declares `standard_name: "surface_temperature"`
  and no `long_name` or `units`, and the `FieldDictionary` has an entry for
  `"surface_temperature"` with `long_name: "Surface Temperature"` and
  `canonical_units: "K"`
- **THEN** the resulting Field's `long_name` is `"Surface Temperature"` and
  its `units` is `"K"`

#### Scenario: Explicit long_name and units override the dictionary
- **WHEN** a Field `VarSpec` declares `standard_name: "surface_temperature"`,
  `long_name: "Skin Temp"`, and `units: "degC"`, and the `FieldDictionary`
  entry for `"surface_temperature"` specifies different values
- **THEN** the resulting Field's `long_name` is `"Skin Temp"` and its `units`
  is `"degC"`

#### Scenario: Vector's compound standard_name is not looked up for defaulting
- **WHEN** a Vector `VarSpec` declares `standard_name: "(eastward_wind,northward_wind)"`
  and does not declare `long_name`/`units`
- **THEN** no `FieldDictionary` lookup is attempted for `long_name`/`units`
  defaulting, and the resulting Vector's Fields have no `long_name`/`units`
  defaulted by this mechanism (unaffected by this requirement)

### Requirement: A standard_name absent from the FieldDictionary is governed by ValidationMode
When a `VarSpec` of a required item type (Field, Vector, Bracket, or
VectorBracket) declares a plain (non-compound) `standard_name` that has no
entry in the `FieldDictionary`, the system SHALL treat this as a
standard-name-convention violation whose severity is governed by the active
`ValidationMode`: a fatal error in `STRICT` mode, a warning in `PERMISSIVE`
mode. In either case, no `long_name`/`units` defaulting occurs for that
`VarSpec` (there is no dictionary entry to default from). A Vector's
compound-encoded `standard_name` is never looked up in the `FieldDictionary`
(see the defaulting requirement above) and so is exempt from this
unknown-standard_name check as well.

#### Scenario: Unknown standard_name is a fatal error in strict mode
- **WHEN** a `VarSpec` declares a `standard_name` not present in the
  `FieldDictionary` and the active `ValidationMode` is `STRICT`
- **THEN** spec construction/connection fails with an error naming the
  undeclared `standard_name`

#### Scenario: Unknown standard_name is a warning in permissive mode
- **WHEN** a `VarSpec` declares a `standard_name` not present in the
  `FieldDictionary` and the active `ValidationMode` is `PERMISSIVE`
- **THEN** spec construction/connection succeeds, a warning naming the
  undeclared `standard_name` is logged, and no `long_name`/`units`
  defaulting occurs for that `VarSpec`

### Requirement: A runtime ValidationMode switch controls enforcement severity
The system SHALL provide a runtime-configurable `ValidationMode`
(`STRICT` or `PERMISSIVE`) that governs whether standard-name-convention
violations (disagreement between a connected Import/Export, or a
`standard_name` absent from the `FieldDictionary`) are reported as fatal
errors or as warnings. Item types that are exempt from `FieldDictionary`
validation (FieldBundle, State, Wildcard, Expression, Service) are not
required to declare or validate a `standard_name` in either mode.

#### Scenario: Strict mode configured for a run
- **WHEN** a run's configuration selects `ValidationMode: STRICT`
- **THEN** every standard-name-convention violation in that run is reported
  as a fatal error

#### Scenario: Permissive mode configured for a run
- **WHEN** a run's configuration selects `ValidationMode: PERMISSIVE` (or
  declares no `ValidationMode`, since permissive is the default)
- **THEN** standard-name-convention violations in that run are logged as
  warnings and execution continues

#### Scenario: Exempt item types are unaffected by either mode
- **WHEN** a FieldBundle, State, Wildcard, Expression, or Service state item
  declares no `standard_name`
- **THEN** no standard-name-convention diagnostic is produced for that item,
  regardless of the active `ValidationMode`
