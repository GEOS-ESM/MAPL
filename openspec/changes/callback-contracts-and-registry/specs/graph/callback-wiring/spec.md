## ADDED Requirements

### Requirement: Imports and exports declare an expected callback interface by name
A component SHALL be able to declare, in its YAML state specification
(key `callback_interface: <name>`) or in its Fortran spec call, that an
import expects, or a STATE export implements, a registered callback
interface. The name SHALL be resolved through the registry when the
specification is read; an unregistered name SHALL fail with a message
naming the interface and the registered names. An export that declares
no interface SHALL behave as today.

#### Scenario: Import declared in YAML
- **WHEN** an Invoker's YAML import `scalers` has
  `callback_interface: toy_scaling` and `toy_scaling` is registered
- **THEN** the import is resolved as a callback collection for
  `toy_scaling`

#### Scenario: Unregistered name
- **WHEN** an import declares `callback_interface: toy_scalin`
- **THEN** reading the specification fails, naming `toy_scalin` and the
  registered interface names

#### Scenario: Export declared in YAML
- **WHEN** a Handler's STATE export declares
  `callback_interface: toy_scaling`
- **THEN** a pattern match of that export against a `toy_scaling`
  import is checked as a declared Handler

### Requirement: An Invoker executes a named method on a callback State of its collection
MAPL SHALL let an Invoker, inside its own run code, execute a method by
name on one callback State of a collection it imported. The execution
SHALL run, in order: the conformance check if that State was not yet
checked; the method's pre-call network (bringing every `in` and `inout`
argument up to date on the Invoker side and taking any snapshot a put
needs); any call scalars already written by the Invoker staying in
place; the procedure the Handler attached under that method name, with
the callback State as its argument; the method's post-call network
(bringing every `out` and `inout` argument to its destination). An
unknown method name SHALL fail with a message listing the interface's
method names. A failure of the Handler's procedure SHALL be returned to
the Invoker, and the post-call network SHALL NOT run.

#### Scenario: Get then put on a same-grid Handler
- **WHEN** an Invoker executes `get` then `put` on a `toy_scaling`
  callback State whose Handler shares its grid
- **THEN** after `get` the Invoker sees the Handler's `quantity`, and
  after `put` the Handler's export holds the value the Invoker wrote

#### Scenario: Unknown method name
- **WHEN** an Invoker executes `gte` on a `toy_scaling` callback State
- **THEN** execution fails with a message listing `get, put`

#### Scenario: Handler failure stops the post-call network
- **WHEN** the Handler's `get` procedure returns a failure
- **THEN** the Invoker receives the failure and no post-call transform
  runs

### Requirement: An Invoker iterates its collection by member
MAPL SHALL let an Invoker query the number of callback States in an
imported collection, retrieve each by position and by qualified name,
and execute methods on each in turn. The order SHALL be stable from run
to run for the same configuration.

#### Scenario: Two Handlers in one collection
- **WHEN** an Invoker's `toy_scaling` collection matches `scaler_a` and
  `scaler_b`
- **THEN** the Invoker sees two callback States, in the same order on
  every run

### Requirement: Callback arguments carry characteristics and get transforms
A callback argument's Handler-side member and Invoker-side copy SHALL
be compared by the same characteristics as an ordinary connection, and
a mismatch SHALL be repaired by an inserted transform where one exists,
in the pre-call or post-call network that carries the argument. A
mismatch with no transform SHALL be reported as for an ordinary
connection.

#### Scenario: Handler on another grid
- **WHEN** a `toy_scaling` Handler runs on a 2°×2.5° grid and the
  Invoker on a 4°×5° grid
- **THEN** a horizontal regrid is inserted on the `get` return of
  `quantity` and on the `put` delivery of `quantity`

### Requirement: Callback arguments are regridded first-order conservatively by default
Unless an interface rule says otherwise, a callback argument SHALL be
regridded with the first-order conservative method in both directions.
A higher-order method SHALL NOT be used unless an argument asks for it
explicitly. On a return path, the mass part of a delta and every
extensive output SHALL use the same horizontal map.

#### Scenario: Default regrid method
- **WHEN** a `turbulent_mixing` member is on another grid than the
  Invoker
- **THEN** every inserted horizontal regrid on its arguments uses the
  first-order conservative method

### Requirement: Value-flavour puts return a weighted delta
For a `put` argument with `put_flavour: value` whose Handler side is on
another grid, the framework SHALL take a snapshot `c0` of the
Invoker-side copy when `get` delivers it, and at `put` SHALL form
`ΔT = c − c0` on the Invoker grid. When the argument has a weight `w`
(`weight_for`), it SHALL return `ΔO = R†(mT ΔT) / mO`, where `mT` is the
weight delivered to the Invoker at `get`, `mO` the owner's bound weight
read at `put`, and `R†` the first-order conservative map back to the
owner grid; then add `ΔO` to the owner field. Without a weight (an
extensive argument), it SHALL return `R†(ΔT)`. When both sides share a
grid, the Invoker SHALL write the owner field directly, with no
snapshot and no delta.

#### Scenario: Stand-alone weighted delta-back (M2)
- **WHEN** two Invoker cells of area a have `mT = (10, 30)` and
  `ΔT = (4, 0)`, under one owner cell of area 2a with `mO = 20`
- **THEN** the weighted transform gives `ΔO = 1`, a gain of 40a, while
  a plain area average would give `ΔO = 2`, a gain of 80a

#### Scenario: Same grid, no delta (M1)
- **WHEN** a `turbulent_mixing` owner shares the Invoker's grid
- **THEN** the transform report shows no regrid and no snapshot, and
  the owner field equals the Invoker's result

#### Scenario: Coarser non-nested owner closes its budget (M3)
- **WHEN** an owner on a non-nested 6°×7.5° grid with non-uniform mass
  receives a `neumann` mixing result
- **THEN** every owner column satisfies `Σk mO,k ΔcO,k = Δt F`
  within 10⁻¹² of the column scale, while the plain regrid of `ΔT`
  leaves a residual of about 1.2% of `Δt F`

### Requirement: Conservative column filling follows delta-back for non-negative owners
When an owner declares its field `non_negative` with column fill, the
framework SHALL, after applying a delta, move tracer within each column
so that no value is negative and the column total is unchanged. The
fill amount SHALL be available as a diagnostic, separate from the
delta, and nonzero only in columns that went negative.

#### Scenario: Finer nested owner with fill (M4)
- **WHEN** four owner cells under one Invoker cell have bottom-layer
  `c = (0, 2, 0, 2)` and `mO = (10, 30, 10, 30)`, and the Invoker's
  bottom change is −1
- **THEN** the pre-fill change is `(−2, −2/3, −2, −2/3)`, every owner
  column changes by −20, after the fill every value is non-negative,
  column totals are unchanged, and the fill diagnostic is nonzero only
  in the two columns that went negative
