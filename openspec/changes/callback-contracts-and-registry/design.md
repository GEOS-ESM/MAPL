## Context

See proposal.md (Why) for motivation. This change builds on two landed
changes: `callback-data-model-registry` (the `CallbackInterface`,
`CallbackArgumentSpec`, `CallbackMethodSpec`, `CallbackStateBinding` and
`CallbackInterfaceRegistry` types) and `callback-wiring` (the flattened
qualified-export namespace, `resolve_callback_import`,
`materialize_callback_collection`, `CallbackMethodBinding`,
`invoke_callback_method`). Their design decisions stay in force: the
interface carries no service name of its own; the registry is a
module-level singleton behind wrapper procedures; callback-ness is
declared on the item (`VariableSpec%callback_interface_id`), not on a
new `Connection` subtype; get edges join the hierarchy's forward network
and each method attachment owns a put (write-back) network.

The source design note is
`geos-mapl3-design/mapl3-callback-contracts-and-registry.md` (2026-11-07),
with binding decisions 62, 63, 64 and 66 of that repository's `PLAN.md`.
It is not in this repository; this design restates what the
implementation needs.

The current tree (verified for this change) has these gaps:

- `CallbackArgumentSpec` holds a name and an item kind only;
  `CallbackMethodSpec` maps argument name to access only.
- `export_conforms_to_interface`
  (`GraphBuilder_CallbackWiring_smod.F90`) checks member names and kinds
  on the export's `VariableSpec`, stops at the first failure, and a
  rejected match is reported through the connect hook's
  "no registered extension provider for mismatched characteristic"
  warning, not as a contract error.
- Nothing outside tests calls `register_callback_interface`,
  `build_callback_state_binding`, `build_callback_method_binding` or
  `invoke_callback_method`.
- No `ESMF_MethodExecute`-backed `StateMethodInvoker` exists
  (`MethodInvocationAdapter.F90`, `StateMethodInvocation.F90`). The
  Handler side can attach procedures (`MAPL_MethodAdd` in
  `MAPL_Generic.F90`, `StateAddMethod` in `superstructure/state/`).
- The collection State built by `materialize_callback_collection` is a
  structurally empty `ESMF_State`; its members exist only as graph
  `NodeId`s. Graph state population skips STATE items.
- `MAPL_InfoGetShared` (`InfoUtilities.F90`) reads typed values only from
  a named item inside a State (plus a string from a State's own Info).
  It has no `r8_1d`, no string list, no non-string getter for a State's
  own Info, and no stored-type query. All shared keys sit under
  `/MAPL/shared/` (`INFO_SHARED_NAMESPACE`, `MAPL_ESMF_InfoKeys.F90`).
- `MAPL_initialize` (`mapl/MaplFramework.F90`) is called by every
  `mapl_pFUnit_Initialize_mod`-based test executable, but not by the
  `MAPL.generic.graph` executable (`GraphCoreTestInit.F90`).
- `mapl_cap_create` (`mapl/CapDriver.F90`) calls `make_driver`, which
  calls the cap gridcomp's `SetServices`.

## Goals / Non-Goals

**Goals:**
- Full contract data in `CallbackInterface`, loaded from one YAML file,
  validated at load, frozen by the Cap.
- Conformance checks on the realized callback State, collected and
  worded for people.
- Invoker-side execution with transforms on callback arguments,
  weighted delta-back and conservative column filling.
- Test tools and the two slices (`toy_scaling`, `turbulent_mixing`).

**Non-Goals:**
- Overloads for R4 and R8, units checks, geometry wildcards,
  Handler-declared widths, a per-mode "consumed" table, a debug report of
  requested arguments, and registration from component code (all later,
  compatible additions).
- Enforcing compatibility by version (the version is recorded and
  reported only).
- A per-argument `regrid:` key (second-order on request).
- Ungridded `split | batch`, filters, families, generated Handlers,
  internal-State nesting (other parts of the GEOS design).
- The GEOS setup-script copy and modified-file warning: that is
  GEOSgcm fixture work. This change ships the files and records the
  expectation.
- `implicit_flux_solve`, `convective_transport`, `tracer_advection`
  entries beyond placeholders in the GEOS file.

## Decisions

**1. Additive data model with defaults that keep today's meaning.**
`CallbackArgumentSpec` gains `dims`, `optional`, `extensive`,
`weight_of` as optional constructor arguments. New small value types
`CallbackAttributeSpec`, `CallbackCallScalarSpec` and `CallbackRule`
live in `graph/callback/` beside the existing ones, with gFTL map or
vector containers in `graph/containers/` following
`CallbackArgumentSpecMap`. `CallbackInterface` gains `version`,
`summary`, `description`, `subject`, and the three new collections.
`CallbackMethodSpec` keeps its `AccessSpecMap` and adds a parallel
per-argument options map (only `put_flavour` recognized). Every check the
loader does is also enforced by the declaring procedures, so a test
that builds an interface in code gets the same errors.
*Alternative:* a new `CallbackContract` type wrapping the old interface.
Rejected: two types for one concept, and every existing caller would have
to choose.

**2. A `subject` argument hosts member-scoped attributes.**
The note puts member-scoped attributes "on each member's Info" and states
that a callback State has one member (one tracer). `turbulent_mixing`
has several field arguments (`mixed_quantity`, `air_mass`, fluxes), and
`air_mass` (for example `DELP`) is shared by many owners' callback
States. Requiring `turbulent_mixing/mix_like` on every member would
force the same key, with possibly different values, onto one shared
Info. So an interface with member-scoped attributes names one argument
as its subject, and only that member hosts them. *Alternative:* host on
every member and require equal values. Rejected: shared Info would make
two owners with different `mix_like` conflict. This is an addition to
the note; see Owner questions in the plan.

**3. Keys sit under MAPL's shared namespace.** `<interface>/<name>` is
relative to `INFO_SHARED_NAMESPACE`, so the stored key is
`/MAPL/shared/toy_scaling/operation`. This matches how
`MAPL_InfoSetShared` already writes and keeps graph code off raw
`ESMF_Info` (CONVENTIONS §5a). The unknown-key check enumerates the
children of `/MAPL/shared/<interface>/` on each host. The reserved
`mapl_mock/` prefix sits there too.

**4. Info wrappers grow as needed, in `InfoUtilities.F90`.** New
specifics: a stored-type query (wraps `ESMF_InfoInquire`, returns a MAPL
enum: integer, real, logical, string, list of each, absent, other);
`r8_1d`; string list; and State-own-Info getters and setters for
logical, `i4`, `r4`, `r8` (scope `state` needs them). A key enumeration
under a prefix supports the unknown-key check (via `ESMF_InfoInquire`'s
key count and indexed key retrieval; the probe confirms it works on
nested objects). A small probe test first
records whether `ESMF_InfoGet` converts silently and what
`ESMF_InfoInquire` reports for R4 and R8 values (both are unverified in
the note).

**5. The loader is a graph-layer module that only uses HConfig.**
`mapl_CallbackRegistrationLoader_mod` (in `graph/callback/`) reads
`./callback_interfaces.yaml` with `ESMF_HConfigCreate(filename=...)`,
walks the keys, builds each `CallbackInterface` through the public
declaring API, validates it, and registers all entries only after every
entry passed (all-or-nothing, so a bad file leaves an empty registry
and a clear error). Errors name the file, the interface, the offending
key or name, and the legal names. No second YAML parser (owner).

**6. Load in `mapl_initialize`, freeze in `mapl_cap_create`.** The load
call goes at the end of `MaplFramework%initialize`, after the field
dictionary, so every MAPL program shares one path. The freeze and the
startup report go in `mapl_cap_create`, before `make_driver` (which
runs the root `SetServices`). A missing file is recorded, not an error;
`lookup_callback_interface_id` and the new YAML key resolution fail with
"no callback_interfaces.yaml found in <cwd>" when the registry is empty
because no file was found (author decision honored). The
`MAPL.generic.graph` executable does not call `mapl_initialize`; its
tests keep registering in code, which the files-only rule allows for
tests.

**7. Checksum: CRC-32, implemented in MAPL.** MAPL has no checksum
routine today. A table-driven CRC-32 over the file bytes is about 40
lines of Fortran, needs no new dependency, and is enough to spot an
edited file in a log. SHA-256 is stronger but larger to carry and adds
nothing for an accidental-edit warning. Logged as 8 hex digits.

**8. Handlers may declare their interface; that makes conformance an
error.** `VariableSpec%callback_interface_id` today marks an import as a
callback consumer. The same field on a STATE export now marks it as a
Handler of that interface. A YAML key `callback_interface: <name>` and a
`make_VariableSpec` keyword set it by name through the registry. At
connect, a matched export that declares the expected interface and fails
the declaration-level check (Decision 9) is a wiring error with the full
report; a matched export that declares nothing keeps today's
skip-and-report path, so existing tests and wildcard behavior stay as
they are. *Alternative:* make every non-conforming STATE match an error.
Rejected: a regex such as `.*/tracers` may legitimately match unrelated
State exports, and today's specs describe a rejection as a report.

**9. Two check stages (open point 2 resolved).**
- *Connect, declaration level:* `export_conforms_to_interface` is
  extended, not replaced: it checks required arguments, kinds and dims
  against the export's `VariableSpec` members, treats optional arguments
  as optional, and returns all violations instead of the first. It runs
  where it runs today (`resolve_callback_import`).
- *Before first execution, realized level:* a new pure checker,
  `check_callback_state_conformance(state, iface, handler_name,
  violations, rc)`, takes the realized `ESMF_State` and the interface and
  returns a violation list (attributes, types, allowed values, unknown
  keys, rules after attributes, weights, geometry of weights). It has no
  graph dependency, so it is testable on hand-built States. It runs
  lazily before the first `execute` on each callback State, or when an
  Invoker asks for its collection. It cannot run earlier because a
  Handler cannot set attribute values during `SetServices` at all (the
  callback State's members have no realized `ESMF_Info` to write to
  until after every component's `SetServices` has run), and in practice
  sets them in its own user initialization, which may run after an
  Invoker's. A per-State "checked" flag (in the callback State's Info,
  under `/MAPL/callback/`) keeps it to one run.

**10. Message format.** One failure per callback State, header
`callback contract violation: interface '<name>' (version <v>), Handler
'<component>', callback State '<state>':` and one line per violation,
using the wording of the spec scenarios. The short reason codes of
today (`missing_member:<name>`) stay as internal codes.

**11. The attribute accessor finds the namespace on the State.** When a
callback State is bound, the framework writes the interface name into
the State's own Info (`/MAPL/callback/interface`). The accessor
`MAPL_CallbackGetAttribute(state, name, value, unusable, member, rc)`
(generic over the value types) reads that name, checks `name` against
the interface's schema, reads from the subject member or the State, and
returns the default when absent. It also records "default used" for the
report. The call-scalar setter `MAPL_CallbackSetScalar(state, name,
value, rc)` checks name, type and writer; the writer is known from the
calling context (`invoker` from the execute API, `handler` inside the
attached procedure, tracked by a flag the execute API sets around the
call).

**12. Invoker-side execution is owner work and the critical path.**
None of execution exists today: realizing each Handler's callback State
as a real `ESMF_State` holding its members, filling the Invoker's
collection State, an `ESMF_MethodExecute`-backed `StateMethodInvoker`,
production calls to `build_callback_state_binding` and
`build_callback_method_binding`, and an Invoker API
(`MAPL_CallbackCollectionGet(collection, count, ...)`,
`MAPL_CallbackExecute(state, method, rc)`). The owner implements it
directly. Every task that runs a method depends on it. The
note's pre-call / post-call networks are the existing forward network
(pull) and the per-method put network; this change does not rename
them.

**13. Callback arguments get characteristics from their members.**
`wire_callback_argument` today compares two empty characteristic maps.
It changes to compare the Handler member's descriptor with the
Invoker-side copy's descriptor (geometry from the Invoker's component),
so `find_or_build_extension_chain` inserts the ordinary transforms. The
horizontal regrid param for callback arguments defaults to first-order
conservative (Decision 66), set when the Invoker-side copy is created.

**14. Weighted delta-back as a multi-input graph Transform.**
`WeightedDeltaBackTransform` (in `graph/transforms/`) has input ports
`value` (c on T), `snapshot` (c0 on T), `weight_invoker` (mT on T),
`weight_owner` (mO on O) and `owner` (X on O), and output `owner`. It
forms `mT (c − c0)`, regrids it with the same first-order conservative
regridder the argument uses, divides by `mO`, and adds to X. Without a
weight it regrids `c − c0` directly. The snapshot is a value copy taken
by a `SnapshotTransform` on the `get` return path, only when a regrid
lies between owner and Invoker. `ExpressionTransform` is the precedent
for several input ports. Legacy `NormalizationTransform` is not used
(duplicate, do not delegate; it is also not usable in production). The
Transform is tested stand-alone on the M2 numbers first.

**15. Conservative column fill as a separate Transform.**
`ConservativeColumnFillTransform` runs after the delta is applied when
the owner declares `non_negative: {fill: column}`. Per column with
negative values it scales `max(c, 0)` by the non-negative ratio that
restores the column's mass total (with the owner's weight), and writes
the change as a diagnostic field. A column with a negative total cannot
be filled; that is an error naming the column. `non_negative` is read
from the owner's declaration as a never-compared characteristic
(design-note Group C precedent: `RestartMode`).

**16. Recorded values are held by the mock (open point 4 resolved).**
The configurable Handler keeps its scalar records in its callback State's
Info under `mapl_mock/` (as the note requires) and its full-value records
as its own export fields named `mapl_mock_<method>_<argument>`. A test
reads them from the mock's export State, and History can write them.
Nesting them in the callback State was rejected: they would become extra
members visible to the Invoker and to collection matching.

**17. Weight on another geometry is rejected (open point 5 honored).**
`mO` must be the mass of the owner's own cells, or the owner's budget
does not close. A repair by inserted regrid is a later, compatible
relaxation.

**18. Test layout.** Pure data-model and loader tests go in the
`MAPL.generic.graph` executable. Info wrapper tests go with
`infrastructure/esmf` tests. Conformance-checker tests use hand-built
ESMF States in the graph executable. Slice tests (P, F, M, MF) use small
test gridcomps under `gridcomps/callback_test/` and the scenario
machinery under `superstructure/generic/tests/`, each test directory
carrying its `callback_interfaces.yaml`, copied by CMake into the test
working directory. Test grids: `toy_scaling` Invoker 4°×5°, Handlers
2°×2.5°; `turbulent_mixing` Invoker 4°×5° with three levels, owners on
the same grid, 6°×7.5° (non-nested) and 2°×2.5° (nested), all
edge-aligned (`dateline: DE`, `pole: PE`). Tolerance: R8 fields,
relative 10⁻¹² of the column scale `Σk mk |ck|`, fallback 10⁻¹⁰ if
ESMF's conservative weights prove less accurate.

## Risks / Trade-offs

- [Execution is unbuilt and large] → It is split out as the owner track.
  Everything that can be built and tested without running a method
  (data model, loader, freeze, Info wrappers, checker on hand-built
  States, the two Transforms stand-alone, the column solver) is ordered
  first and can proceed in parallel.
- [HConfig may silently keep one of two duplicate keys] → A probe test
  records the behavior; the shipped files' headers and the user guide
  document it. MAPL does not add its own YAML parser.
- [`ESMF_InfoInquire` may not distinguish what the checks need] → The
  Info probe runs first; if R4/R8 or integer/real cannot be told apart,
  the type check is weakened to what ESMF reports and the gap is
  documented, before the checker is written.
- [ESMF conservative weights may not reach 10⁻¹²] → Fallback tolerance
  10⁻¹⁰, decided by the stand-alone Transform test.
- [Lazy checks fail at the first run, not at initialization] → Invokers
  can request the check explicitly at the start of the first run; the
  report is the same.
- [Shared namespace prefix] → Keys are long (`/MAPL/shared/<iface>/...`)
  but consistent with every other MAPL attribute; user-facing messages
  print the short form `<iface>/<name>`.

## Migration Plan

Additive. No production code registers interfaces or declares Handler
interfaces today. Existing callback tests that register interfaces in
code keep working (tests may register; the graph executable has no
Cap). Rollback is reverting the change's commits.

## Open Questions

- Which entry wins for duplicate keys in HConfig (settled by the probe
  task; changes only documentation).
- Whether `ESMF_InfoGet` converts silently (settled by the Info probe;
  affects only which ESMF call the type check uses).
