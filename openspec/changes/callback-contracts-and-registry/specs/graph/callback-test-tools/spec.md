## Purpose

Provides MAPL test components that stand in for a Handler or an Invoker
of any registered callback interface, configured in YAML, so contracts
and real components can be tested without writing a matching partner.

## ADDED Requirements

### Requirement: A configurable Handler impersonates a Handler of any registered interface
MAPL SHALL provide a test gridcomp that, from its YAML configuration,
builds one callback State per configured interface, from that
interface's registry entry, so it conforms by construction. Its
configuration SHALL set per-State attributes (`state_attributes`) and
per-member attributes (`member_attributes`) under the interface
namespace, and MAY name its own geometry (`geometry`, default the
Invoker's).

#### Scenario: Conforming mock
- **WHEN** a configurable Handler is configured for `toy_scaling` with
  `operation: offset` and `factor: 3.0`
- **THEN** its callback State passes the contract checks

### Requirement: The configurable Handler supplies configured values
For each `out` or `inout` argument of a method, the configurable Handler
SHALL fill the member from its `supply` configuration: `constant: v`
(every point), `profile: [v1, ..., vK]` (one value per level,
horizontally uniform), or `function: "<expression>"` evaluated per cell
centre in `lon`, `lat` (degrees) and `level`. A supply form that does
not fit the argument (for example a profile of the wrong length) SHALL
fail at initialization.

#### Scenario: Constant and profile
- **WHEN** `quantity` is supplied `constant: 1.0` and
  `mixed_quantity` `profile: [1.0e-6, 2.0e-6, 3.0e-6]`
- **THEN** after `get` the Invoker sees 1.0 everywhere in `quantity`
  and the three level values in `mixed_quantity`

### Requirement: The configurable Handler records every call
After each method call the configurable Handler SHALL record, in its
callback State's own Info under `mapl_mock/`: `call_count` (integer),
`calls` (list of method names in order), and `mapl_mock/<method>/<name>`
for each call scalar seen. For each argument listed under `record` with
access `in` or `inout` in the method, it SHALL keep a copy of the
received values as a field named `mapl_mock_<method>_<argument>`. It
SHALL never write into member Info or under an interface's own
namespace.

#### Scenario: One Invoker step (P1)
- **WHEN** a real Invoker executes `get` then `put` once on a mock
  configured to record `quantity`
- **THEN** `mapl_mock/call_count` is 2, `mapl_mock/calls` is
  `[get, put]`, and field `mapl_mock_put_quantity` equals the Invoker's
  result regridded to the mock's grid

### Requirement: The configurable Handler may declare a non-negative owner field
The configurable Handler SHALL accept `non_negative: {fill: column}` for
a mock owner field, so the framework inserts conservative column
filling after a put, and SHALL record the owner field after `put` and
the fill diagnostic when listed under `record`.

#### Scenario: Fill recorded
- **WHEN** a mock owner declares `non_negative: {fill: column}` and
  records `mixed_quantity`
- **THEN** after `put` the recorded owner field is non-negative and the
  fill diagnostic is available

### Requirement: A configurable Invoker calls a collection on a schedule
MAPL SHALL provide a test gridcomp that imports a collection of a
configured interface, matched by a regular expression as for callback
collections, and, at each run, executes the configured schedule of
method calls on every member in turn. For each argument listed under
`record`, it SHALL keep the values received after each `get` as a field.

#### Scenario: Real Handler driven by the configurable Invoker (P2)
- **WHEN** the configurable Invoker runs `[{call: get}, {call: put}]`
  on a real `toy_scaling` Handler and records `quantity`
- **THEN** its recorded `quantity` equals the Handler's export
  regridded to the Invoker's grid, and the Handler's export after `put`
  holds the value the Invoker delivered
