## Purpose

Defines what the framework checks when a Handler's realized callback
State is bound to a registered callback interface (structural
conformance), the order of the checks, how violations are reported, and
how attribute values and call scalars are hosted and read under the
interface's namespace.

## ADDED Requirements

### Requirement: Implementations conform structurally
A Handler's callback State SHALL conform to an interface when it
provides at least what the interface requires. Members not named by the
interface, attributes not named by it, and Info keys under another
interface's namespace SHALL be ignored. One Handler procedure MAY be
attached to callback States of several interfaces; each callback State
is checked only against its own interface.

#### Scenario: Extra member and foreign attribute are accepted
- **WHEN** a `toy_scaling` callback State has the required members plus
  an extra member `extra`, and an Info key `other_iface/water`
- **THEN** it conforms

#### Scenario: One procedure serves two interfaces
- **WHEN** one procedure is attached to State S1 for `needs_field` and
  State S2 for `needs_water_field`, and S2 lacks the required attribute
  `needs_water_field/water`
- **THEN** S1 conforms and only S2 fails

### Requirement: Contract checks run on the realized callback State
For each Handler callback State bound to an interface, the framework
SHALL check, against the realized ESMF State and the Info of its
members: every required argument is present as a member; every present
member has the declared item kind and gridded dimensions; every
required attribute is present on its host (the Info of the member bound
to the attribute's declared host argument for scope `member`, the
callback State's Info for scope `state`) under
`<interface>/<attribute>`; every present attribute's stored value type
is compatible with the declared type, read without conversion (integer
and real distinct; 4-byte and 8-byte real both "real"; a list type
needs an array of the element type); every present string or integer
attribute with an allowed set has a value in it; every key under
`<interface>/` on any host is a declared attribute or call scalar; for
every rule whose attribute has the rule's value, every argument it
requires is present; for every argument `w` with `weight_of: a`, if `a`
is present then `w` is present, and `w`'s geometry equals `a`'s.

#### Scenario: Required argument missing (F1)
- **WHEN** a `toy_scaling` Handler has no member `quantity`
- **THEN** wiring fails with a missing-argument error naming
  `quantity`

#### Scenario: Kind mismatch (F2)
- **WHEN** `quantity` is bound to a bundle
- **THEN** wiring fails with a kind-mismatch error naming `quantity`,
  the found kind and the expected kind

#### Scenario: Required attribute missing (F3, MF6)
- **WHEN** `toy_scaling/operation` is absent from the callback State's
  Info, or `turbulent_mixing/mix_like` is absent from a member's Info
- **THEN** wiring fails with a missing-attribute error naming the
  attribute and its host

#### Scenario: Attribute type (F4)
- **WHEN** `toy_scaling/factor` is stored as the string `'2.0'`
- **THEN** wiring fails with an attribute-type error naming the stored
  type and the declared type

#### Scenario: R4 and R8 both satisfy real
- **WHEN** `toy_scaling/factor` is stored once as 4-byte and once as
  8-byte real in two Handlers
- **THEN** both conform

#### Scenario: Value not allowed (F5, MF5)
- **WHEN** `operation` is `ofset`, or `mix_like` is `moistre`
- **THEN** wiring fails with an attribute-value error listing the
  allowed values (`scale, offset`; `heat, moisture, momentum`)

#### Scenario: Rule not met (F6, MF7)
- **WHEN** `operation` is `offset` and no `offset_value` member is
  bound, or `bottom_boundary` is `robin` and no
  `dry_deposition_velocity` is bound
- **THEN** wiring fails with a rule error stating the rule and the
  missing argument

#### Scenario: Unknown key in the interface namespace (F8, MF8)
- **WHEN** the callback State's Info has `toy_scaling/facter`, or a
  member's Info has `turbulent_mixing/bottom_bondary`
- **THEN** wiring fails with an unknown-key error listing the legal
  names

#### Scenario: Required weight missing (MF1)
- **WHEN** a `turbulent_mixing` owner binds no `air_mass`
- **THEN** wiring fails with a missing-argument error naming
  `air_mass` as the weight of `mixed_quantity`

#### Scenario: Optional weighted pair half bound (MF1b)
- **WHEN** an interface has optional `mixed_quantity` and optional
  weight `air_mass`, and an owner binds only `mixed_quantity`
- **THEN** wiring fails with a weight-missing error

#### Scenario: Weight on another geometry (MF4)
- **WHEN** `air_mass` is bound on a geometry other than
  `mixed_quantity`'s
- **THEN** wiring fails with a weight-geometry error; no regrid of the
  weight is inserted

### Requirement: Attribute checks precede rule checks
An attribute's presence, type and allowed-value checks SHALL run before
any rule that uses the attribute. A rule whose attribute failed its own
checks SHALL NOT be evaluated or reported.

#### Scenario: Misspelt value hides the rule (F7)
- **WHEN** `operation` is `ofset` and no `offset_value` is bound
- **THEN** only the allowed-value error is reported, not a rule error

### Requirement: All violations of one callback State are reported together
The framework SHALL collect every violation found for one callback State
and report them in one message, then fail wiring. Each violation SHALL
name the interface and its version, the Handler component, the callback
State, the member if any, what was found, and what is legal.

#### Scenario: Two violations, one report
- **WHEN** a `toy_scaling` Handler lacks `quantity` and has
  `operation: ofset`
- **THEN** one failure reports both violations, each naming
  `toy_scaling` (version 1) and the Handler

#### Scenario: Message wording
- **WHEN** `operation` is `ofset` on Handler `scaler_b`
- **THEN** the message contains `interface 'toy_scaling' (version 1)`,
  `Handler 'scaler_b'`, `'toy_scaling/operation' = 'ofset' is not
  allowed`, and `allowed values: scale, offset`

### Requirement: Defaults are reported and rule-required arguments are never defaulted
When an optional attribute is absent, its declared default SHALL be the
value seen through the attribute accessor, and the framework SHALL log
once per callback State and attribute that the default was used. A
missing rule-required argument SHALL always be an error; the framework
SHALL NOT supply a value for it.

#### Scenario: Default used and reported (P4)
- **WHEN** a `toy_scaling` Handler does not set `factor`
- **THEN** the accessor returns 2.0 and the log states that `factor`
  used its default 2.0 for that Handler

### Requirement: Conformance is checked before the first method execution
The full check SHALL run before any method on a callback State is
executed, and after every component has finished its user
initialization (the last point at which a Handler may set its attribute
values). A Handler SHALL NOT set attribute values during `SetServices`:
the callback State and its members do not exist as realized `ESMF_State`
objects, with a real `ESMF_Info`, until after `SetServices` completes
for every component; the earliest a Handler can set an attribute value
is its own user initialization. The first method execution on a
callback State not yet checked SHALL run the check first. An Invoker
SHALL also be able to request the check for its whole collection
explicitly at any point after initialization, for example at the start
of its first run. A State SHALL be checked at most once.

#### Scenario: Invoker requests the check explicitly
- **WHEN** an Invoker requests the conformance check of its collection
  at the start of its first run and one member does not conform
- **THEN** the request fails with that member's report

#### Scenario: Check runs implicitly before first execution
- **WHEN** an Invoker executes a method on a callback State that was not
  checked yet
- **THEN** the check runs first, and a violation fails the execution
  before the method runs

### Requirement: Declaration-level checks run at connect
At connect time, a matched export that declares the expected interface
SHALL be checked for the items its declaration shows (member presence,
kind and dimensions). A failure SHALL be a wiring error with the same
report form. A matched export that declares no interface SHALL keep
today's behavior: it is skipped from the collection, and the skip is
reported.

#### Scenario: Declared Handler with a missing member
- **WHEN** a matched export declares `toy_scaling` and has no
  `quantity` member
- **THEN** connect fails with the missing-argument report

#### Scenario: Undeclared export that does not fit
- **WHEN** a matched export declares no interface and has no
  `quantity` member
- **THEN** it is not a collection member and the skip is reported, as
  today

### Requirement: Attributes and call scalars are hosted under the interface namespace
A per-member attribute SHALL be stored in the Info of the member bound
to the attribute's declared host argument, under
`<interface>/<attribute>`. Other members (for example a shared air-mass
field bound by many callback States) SHALL NOT carry it. A per-State
attribute and every call scalar SHALL be stored in the callback State's
own Info under `<interface>/<name>`. The prefix `mapl_mock/` SHALL be
reserved for the test tools.

#### Scenario: Member and State hosts
- **WHEN** a Handler sets `mix_like` on member `DU001` and `operation` on
  its `toy_scaling` callback State
- **THEN** the keys are `turbulent_mixing/mix_like` on `DU001`'s Info
  and `toy_scaling/operation` on the State's Info

### Requirement: The attribute accessor takes the namespace from the invoked callback State
MAPL SHALL provide an accessor that reads an attribute by its bare name
for a member or for the callback State the current method was invoked
under. It SHALL take the interface namespace from that callback State,
so one implementation serves several interfaces. It SHALL return the
default for an absent optional attribute. A name the interface does not
declare SHALL fail with a message listing the declared names.

#### Scenario: Same procedure, two namespaces
- **WHEN** one procedure is invoked on a `needs_field` State and on a
  `needs_water_field` State and reads `water` through the accessor
- **THEN** the read on the second uses `needs_water_field/water`, and on
  the first fails as undeclared

#### Scenario: Misspelt name
- **WHEN** the accessor is asked for `quantiy` on a `toy_scaling` State
- **THEN** it fails with a message listing `quantity, offset_value`

### Requirement: Call scalars are written through a checked setter
MAPL SHALL provide a setter for call scalars that writes into the
callback State's own Info under the interface namespace. It SHALL reject
a name the interface does not declare as a call scalar, a value of the
wrong type, and a write by a party other than the declared writer.

#### Scenario: Invoker writes a declared scalar
- **WHEN** an Invoker sets `time_step` = `PT900S` on a callback State
  whose interface declares it written by the invoker
- **THEN** the Handler reads `PT900S`

#### Scenario: Wrong writer
- **WHEN** a Handler sets a call scalar declared `written_by: invoker`
- **THEN** the setter fails

### Requirement: The framework does not check procedures or argument use
The framework SHALL NOT check what the procedure attached to a method
does, nor whether an Invoker reads each bound argument. An Invoker that
rejects unused arguments SHALL do so in its own code.

#### Scenario: Invoker rejects an unused argument itself (MF9)
- **WHEN** a `turbulent_mixing` member has `bottom_boundary:
  neumann_homogeneous` and binds `surface_upward_flux`
- **THEN** the framework accepts the State, and the test Invoker fails
  before its first method execution with its own message
