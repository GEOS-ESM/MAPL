## Purpose

Defines the per-experiment callback registration file: a YAML file that
holds every callback interface of one experiment, how MAPL finds, loads,
validates and reports it, and how the registry is frozen so production
code can never register interfaces.

## ADDED Requirements

### Requirement: Callback interfaces are defined in one registration file per experiment
Every experiment SHALL define its callback interfaces in one YAML file
named `callback_interfaces.yaml`, read from the current working
directory. There SHALL be no configuration key giving another path. The
file's top-level keys SHALL be interface names; each value SHALL be one
interface entry.

#### Scenario: File in the current directory is loaded
- **WHEN** `callback_interfaces.yaml` in the current directory defines
  `toy_scaling`
- **THEN** after MAPL initialization `toy_scaling` is registered

### Requirement: The registration file format
An interface entry SHALL accept these keys and no others: `version`
(integer at least 1, required), `summary` (string, required),
`description` (string, optional), `subject` (argument name, required
when any attribute has scope `member`), `arguments` (mapping, required,
may be empty), `methods` (mapping, required, at least one method),
`attributes` (mapping, optional), `call_scalars` (mapping, optional),
`rules` (list, optional). An argument entry SHALL accept `kind`
(`field`, `bundle` or `state`, default `field`), `dims` (`xyz` or `xy`,
required for fields), `optional`, `extensive`, `weight_of`,
`description`. A method entry SHALL map each argument name either to an
access (`in`, `out`, `inout`) or to a mapping with `access` and
per-argument options (`put_flavour`). An attribute entry SHALL accept
`type` (`integer`, `real`, `logical`, `string`, or `list of <type>`),
`scope` (`member` or `state`, default `member`), `required`, `default`,
`allowed`, `description`. A call-scalar entry SHALL accept `type`,
`written_by` (`invoker` or `handler`), `description`. A rule entry SHALL
have `when` (a one-entry mapping attribute: value) and `requires` (a
list of argument names). An unknown key at any level SHALL be a load
error.

#### Scenario: The toy_scaling entry loads
- **WHEN** the file holds the `toy_scaling` entry: version 1; arguments
  `quantity` (field, xyz) and `offset_value` (field, xy, optional);
  methods `get` {quantity: out, offset_value: out} and `put`
  {quantity: in}; attributes `operation` (string, state, required,
  allowed scale, offset) and `factor` (real, state, default 2.0); rule
  when operation = offset requires offset_value
- **THEN** the registered `toy_scaling` interface reports exactly these
  declarations

#### Scenario: Unknown key in an entry is a load error
- **WHEN** an argument entry has the key `optinal: true`
- **THEN** loading fails with a message naming the file, the interface,
  the argument and the unknown key

### Requirement: The loader validates every entry before registering it
Before registering an entry, the loader SHALL check that: every
argument named in a method exists; every rule names an existing
attribute, a value legal for it, and existing optional arguments; every
default has the declared type and lies in the allowed set; every
`weight_of` names an existing non-extensive argument, and the argument
carrying it is extensive and is required whenever the weighted argument
is; no name contains `/`. A failure SHALL name the file and the
interface, and SHALL list the legal names where a name was wrong.
Loading SHALL stop at the first invalid entry and register nothing from
the file.

#### Scenario: Rule naming an unknown argument
- **WHEN** a `toy_scaling` rule requires `offset_valu`
- **THEN** loading fails with a message naming the file, `toy_scaling`,
  `offset_valu`, and the legal argument names

#### Scenario: Weight of an unknown argument
- **WHEN** `turbulent_mixing` declares `air_mass` with
  `weight_of: mixed_quantiy`
- **THEN** loading fails with a message naming the file,
  `turbulent_mixing`, `mixed_quantiy`, and the legal argument names

#### Scenario: Weight not extensive or not required
- **WHEN** `air_mass` lacks `extensive: true`, or is optional while
  `mixed_quantity` is required
- **THEN** loading fails with a message saying the weight must be
  extensive, or must be required

### Requirement: The file is loaded in MAPL initialization
The registration file SHALL be loaded during `mapl_initialize`, before
any component is created, in every MAPL program, unit-test executables
included. When the file is absent, initialization SHALL succeed with an
empty registry; the absence SHALL become an error only when a component
first looks up a callback interface, and that error SHALL say that no
`callback_interfaces.yaml` was found in the current directory.

#### Scenario: Missing file with no callback use
- **WHEN** no registration file exists and no component uses a callback
- **THEN** the program runs normally

#### Scenario: Missing file with callback use
- **WHEN** no registration file exists and a component declares an
  import expecting `toy_scaling`
- **THEN** wiring fails with a message stating that no
  `callback_interfaces.yaml` was found in the current directory

### Requirement: The Cap freezes the registry before any SetServices
In a program driven by the MAPL Cap, the registry SHALL be frozen after
loading and before the root component's `SetServices`. After that, a
registration from any code SHALL fail with a message stating that the
registry is frozen. A program without the Cap, such as a unit test,
SHALL be able to register further interfaces in code after loading.

#### Scenario: Registration from component code under the Cap
- **WHEN** a component's `SetServices` registers an interface in a
  Cap-driven run
- **THEN** the run fails with a message that the callback registry is
  frozen

#### Scenario: Unit test adds an interface after loading
- **WHEN** a unit test without a Cap registers an interface in code
- **THEN** the registration succeeds

### Requirement: MAPL reports the loaded registration once at startup
After loading, MAPL SHALL log once: the path of the file, its checksum,
and each registered interface in sorted name order with its version and
summary. When no file was found, it SHALL log that instead.

#### Scenario: Startup report contents
- **WHEN** the file defines `toy_scaling` (version 1)
- **THEN** the log contains the file path, its checksum, and a line with
  `toy_scaling`, version 1, and its summary

### Requirement: Duplicate top-level keys follow documented behavior
MAPL SHALL NOT parse the registration file with any parser other than
ESMF HConfig. The behavior of a file with two entries under the same
interface name (which entry wins, and whether any error is reported)
SHALL be established by a test and documented in the header comment of
each shipped file and in the user guide.

#### Scenario: Two toy_scaling entries
- **WHEN** a file holds two `toy_scaling:` entries with versions 1 and 2
- **THEN** the registered version is the one the documentation states
  wins, and a test asserts it

### Requirement: MAPL ships and installs two registration files
MAPL SHALL ship a test registration file holding the interfaces used by
its own tests, and a GEOS registration file holding the interfaces for
most GEOS experiments. Both SHALL be installed to `etc/` by the CMake
install step. Each unit test that uses callbacks SHALL have a copy of the
file it needs placed in its working directory by the build.

#### Scenario: Files are installed
- **WHEN** MAPL is installed
- **THEN** `etc/` contains the test file and the GEOS file

#### Scenario: Unit test finds its file
- **WHEN** a callback unit test runs under ctest
- **THEN** its working directory contains the registration file it uses

### Requirement: Shipped interfaces have optional hand-written name constants
For the interfaces in its shipped files, MAPL SHALL provide hand-written
modules of name constants (interface, method, argument, attribute and
value names). Names SHALL remain run-time strings checked against the
registry; MAPL SHALL NOT generate code from the registration file.

#### Scenario: Constant matches the registered name
- **WHEN** a test compares each constant in the `toy_scaling` constants
  module with the loaded test file
- **THEN** every constant names an existing interface, method,
  argument, attribute or allowed value
