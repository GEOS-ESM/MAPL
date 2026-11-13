## ADDED Requirements

### Requirement: A callback interface carries a version and a summary
A callback interface SHALL carry an integer version of at least 1 and a
one-line summary. It MAY carry a longer description. These SHALL be
retrievable from the interface value.

#### Scenario: Version and summary are retrievable
- **WHEN** an interface is built with version 1 and a summary line
- **THEN** the interface reports version 1 and that summary

#### Scenario: Version below 1 is rejected
- **WHEN** an interface is built with version 0
- **THEN** construction is rejected

### Requirement: A callback argument declares its gridded dimensions, optionality, extensivity and weight
Besides its name and expected item kind, a callback argument SHALL
declare: its gridded dimensions (`xyz` or `xy`), required for a field
argument; whether it is optional (default not optional); whether it is
extensive, meaning a per-area quantity (default not extensive); and,
optionally, the name of another argument whose mass weight it is
(`weight_of`). Defaults SHALL keep the meaning of an argument declared
with name and kind only: required, not extensive, no weight.

#### Scenario: Defaults match a name-and-kind-only declaration
- **WHEN** an argument is declared with a name and a field kind and
  dims `xyz` only
- **THEN** it reports required, not extensive, and no `weight_of`

#### Scenario: A field argument without dims is rejected
- **WHEN** a field argument is declared without gridded dimensions
- **THEN** the declaration is rejected

#### Scenario: Weight relation is retrievable
- **WHEN** argument `air_mass` is declared extensive with
  `weight_of: mixed_quantity`
- **THEN** the interface reports `air_mass` as the weight of
  `mixed_quantity`

### Requirement: A callback interface declares an attribute schema
A callback interface SHALL support declaring named attributes. Each
attribute SHALL declare a value type (integer, real, logical, string, or
a list of one of these), a scope (`member`: hosted on the Info of the
argument it names as its host; `state`: hosted on the callback State's
own Info; default `member`), whether it is required (default not
required), an optional default value, and, for string and integer types
only, an optional set of allowed values. A required attribute SHALL NOT
have a default. A default SHALL have the declared type and, when an
allowed set exists, lie in it. Real SHALL accept either 4-byte or 8-byte
storage.

#### Scenario: Attribute with allowed values is retrievable
- **WHEN** attribute `operation` is declared as a required string with
  scope `state` and allowed values `scale, offset`
- **THEN** the interface reports its type, scope, required flag and
  allowed set as declared

#### Scenario: Default on a required attribute is rejected
- **WHEN** an attribute is declared required and with a default
- **THEN** the declaration is rejected

#### Scenario: Default outside the allowed set is rejected
- **WHEN** a string attribute with allowed values `a, b` is declared
  with default `c`
- **THEN** the declaration is rejected

#### Scenario: Allowed set on a real attribute is rejected
- **WHEN** a real attribute is declared with an allowed set
- **THEN** the declaration is rejected

### Requirement: A member-scoped attribute names the argument that hosts it
An attribute declared with scope `member` SHALL name the argument whose
Info hosts it (`host_argument`). The named argument SHALL be a declared,
required argument that is not the weight of another (a weight argument
can be shared across many unrelated Handlers' bindings, so it cannot
host a value specific to one of them). An attribute declared with scope
`state` SHALL NOT name a host argument.

#### Scenario: Host argument is retrievable
- **WHEN** `turbulent_mixing` declares the member-scoped attribute
  `mix_like` with `host_argument: mixed_quantity`
- **THEN** the interface reports `mixed_quantity` as `mix_like`'s host
  argument

#### Scenario: Member-scoped attribute without a host argument is rejected
- **WHEN** an attribute is declared with scope `member` and no
  `host_argument`
- **THEN** the declaration is rejected

#### Scenario: State-scoped attribute with a host argument is rejected
- **WHEN** an attribute is declared with scope `state` and a
  `host_argument`
- **THEN** the declaration is rejected

#### Scenario: Host argument naming an unknown argument is rejected
- **WHEN** an attribute names `host_argument: quantiy` and no such
  argument is declared
- **THEN** the declaration is rejected, naming the unknown argument and
  the declared argument names

#### Scenario: Host argument that is a weight is rejected
- **WHEN** an attribute names `host_argument: air_mass`, and `air_mass`
  is declared `weight_of: mixed_quantity`
- **THEN** the declaration is rejected

### Requirement: A callback interface declares call scalars
A callback interface SHALL support declaring call scalars: named
scalars written into the callback State's own Info just before a method
call. Each SHALL declare a value type (as for attributes; time intervals
are strings holding an ISO 8601 duration) and its writer (`invoker` or
`handler`).

#### Scenario: Call scalar is retrievable
- **WHEN** a call scalar `time_step` of type string, written by the
  invoker, is declared
- **THEN** the interface reports its type and writer as declared

### Requirement: A callback interface declares rules tying attribute values to required arguments
A callback interface SHALL support rules of the form "when attribute `a`
has value `v`, arguments `x, ...` are required". A rule SHALL name a
declared attribute, a value legal for it, and only declared optional
arguments.

#### Scenario: Rule is retrievable
- **WHEN** a rule "when `operation` = `offset`, `offset_value` is
  required" is declared
- **THEN** the interface reports that rule

#### Scenario: Rule naming a required argument is rejected
- **WHEN** a rule names an argument that is not optional
- **THEN** the declaration is rejected

#### Scenario: Rule naming an unknown attribute or argument is rejected
- **WHEN** a rule names an attribute or argument the interface does not
  declare
- **THEN** the declaration is rejected

### Requirement: Names in a callback interface contain no slash
Interface, argument, method, attribute and call-scalar names SHALL NOT
contain `/`, because `/` separates the interface namespace from the key
in Info.

#### Scenario: Slash in a name is rejected
- **WHEN** an attribute named `a/b` is declared
- **THEN** the declaration is rejected

### Requirement: Weight declarations are consistent
An argument carrying `weight_of` SHALL name another declared argument
that is not extensive. The weight argument itself SHALL be extensive,
and SHALL be required whenever the weighted argument is required.

#### Scenario: Weight of an unknown argument is rejected
- **WHEN** `air_mass` declares `weight_of: mixed_quantiy` and no such
  argument exists
- **THEN** the declaration is rejected, naming the unknown argument and
  the declared argument names

#### Scenario: Non-extensive weight is rejected
- **WHEN** `air_mass` declares `weight_of: mixed_quantity` without being
  extensive
- **THEN** the declaration is rejected

#### Scenario: Optional weight of a required argument is rejected
- **WHEN** `mixed_quantity` is required and its weight `air_mass` is
  optional
- **THEN** the declaration is rejected

### Requirement: A method's argument access may carry per-argument options
A method's access declaration for one argument MAY carry named options
besides the access mode. The option `put_flavour` with value `value` or
`delta` SHALL be recognized. An unrecognized option name SHALL be
rejected.

#### Scenario: Put flavour is retrievable
- **WHEN** method `put` declares `mixed_quantity` with access `in` and
  `put_flavour: value`
- **THEN** the method reports access `in` and put flavour `value` for
  that argument

#### Scenario: Unknown method option is rejected
- **WHEN** a method declares an argument with option `put_flavor`
- **THEN** the declaration is rejected

### Requirement: The interface registry can be frozen
The callback interface registry SHALL support being frozen. After
freezing, any registration SHALL fail with a message stating that the
registry is frozen and naming the interface whose registration was
attempted. Lookups SHALL keep working after the freeze. Freezing an
already frozen registry SHALL have no further effect.

#### Scenario: Registration after freeze fails
- **WHEN** the registry is frozen and an interface `extra` is then
  registered
- **THEN** the registration fails with a message naming `extra` and
  stating the registry is frozen

#### Scenario: Lookup after freeze succeeds
- **WHEN** an interface was registered before the freeze
- **THEN** it is still retrievable by name after the freeze

### Requirement: The interface registry can enumerate its interfaces
The registry SHALL list every registered interface name, in sorted
order, so that a report can print each interface with its version and
summary.

#### Scenario: Enumeration lists every registered name
- **WHEN** interfaces `b_iface` and `a_iface` are registered
- **THEN** the enumeration returns `a_iface, b_iface`
