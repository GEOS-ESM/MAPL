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
- Overloads for R4 and R8, geometry wildcards, Handler-declared widths, a
  per-mode "consumed" table, a debug report of requested arguments, and
  registration from component code (all later, compatible additions).
- Unit *conversion* or compatibility checking beyond placeholder binding
  (Decision 21): a placeholder's declared unit is recorded and shared
  across arguments that name it, and every placeholder must be bound,
  but nothing in this change converts between units or validates one
  against UDUNITS; Invoker-side unit inference and unit expressions
  (`$U s-1`) are later, compatible additions (decision notes, topic
  A-units).
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
`weight_for` as optional constructor arguments. New small value types
`CallbackAttributeSpec`, `CallbackCallScalarSpec` and `CallbackRule`
live in `graph/callback/` beside the existing ones, with gFTL map or
vector containers in `graph/containers/` following
`CallbackArgumentSpecMap`. `CallbackInterface` gains `version`,
`summary`, `description`, and the three new collections.
`CallbackMethodSpec` keeps its `AccessSpecMap` and adds a parallel
per-argument options map (only `put_flavour` recognized). Every check the
loader does is also enforced by the declaring procedures, so a test
that builds an interface in code gets the same errors.
*Alternative:* a new `CallbackContract` type wrapping the old interface.
Rejected: two types for one concept, and every existing caller would have
to choose.

**2. Each member-scoped attribute names its own `host_argument`.**
The note puts member-scoped attributes "on each member's Info" and states
that a callback State has one member (one tracer). `turbulent_mixing`
has several field arguments (`mixed_quantity`, `air_mass`, fluxes), and
`air_mass` (for example `DELP`) is shared by many owners' callback
States. Requiring `turbulent_mixing/mix_like` on every member would
force the same key, with possibly different values, onto one shared
Info. A single interface-wide `subject` does not fix this either when an
interface has several member-scoped attributes that belong to different
arguments (for example `mix_like` belonging to `mixed_quantity` and a
second attribute belonging to a flux argument): one subject can only
name one of them. So each attribute declares its own `host_argument`,
and only that argument's bound member hosts it. *Alternative:* a single
interface-level `subject` naming one host for every member-scoped
attribute. Rejected: does not generalize past interfaces with exactly
one member-scoped attribute. *Alternative:* host on every member and
require equal values. Rejected: shared Info would make two owners with
different `mix_like` conflict. This is an addition to the note; see
Owner questions in the plan.

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
nested objects). A probe test recorded the following, stored directly
in a raw `ESMF_Info` (no production code):
`ESMF_InfoInquire` (via the generic `ESMF_InfoGet` with `typekind=`,
`isArray=`, `size=`) correctly reports the stored kind for every one of
i4, i8, r4, r8, logical, and string, both scalar and array; there is no
kind it cannot distinguish. `ESMF_InfoGet` does not convert silently
across kind: reading a stored r8 value as i4, or a stored string (even
a numeric-looking one such as `'2.5'`) as r8, both return a
non-success `rc` rather than a converted value. Keys nested under a
prefix can be enumerated from the `ESMF_Info` object itself: `size=` at
the parent key gives the child count, then `idx=`/`ikey=` (1-based)
for each index returns that child's own unqualified key name.

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
the interface's schema, and reads from the given `member`'s Info (scope
`member`; the caller passes the member bound to the attribute's declared
`host_argument`) or the State's own Info (scope `state`), returning the
default when absent. It also records "default used" for the
report. The call-scalar setter `MAPL_CallbackSetScalar(state, name,
value, rc)` checks only name and declared type, the same cheap checks
every other accessor in this change makes. A call scalar's declared
writer (Handler or Invoker, decision 9) is not checked at the call site
-- it is a documented convention, same trust level as decision 14's
"every `select case` ends in an error default": the framework guards
against what is cheap to tell apart from correct use (a typo'd name, a
wrong type) and trusts implementors for the rest. No sourced example in
this change has a production Handler and Invoker actually contesting a
call scalar; the only user today is the Mock/Configurable Handler's own
test tooling (task 7.4).

**12. Invoker-side execution is the critical path; the owner designs it,
subagents author it (amended 2026-11-22).** None of execution exists
today: realizing each Handler's callback State as a real `ESMF_State`
holding its members, filling the Invoker's collection State, an
`ESMF_MethodExecute`-backed `StateMethodInvoker`, production calls to
`build_callback_state_binding` and `build_callback_method_binding`, and
an Invoker API (`MAPL_CallbackCollectionGet(collection, count, ...)`,
`MAPL_CallbackExecute(state, method, rc)`). Every task that runs a
method depends on it. The note's pre-call / post-call networks are the
existing forward network (pull) and the per-method put network; this
change does not rename them.

Originally scoped as owner-implemented code (no volunteer/subagent
author). Amended: the owner stays directly involved in *design*
(working through the open questions with meta, as for every other task
here), but once a design is settled it is handed to the usual
author/reviewer subagent pipeline, same as any other task. Several
design questions are already closed this way: execution mechanics for
pulling get-arguments, advancing put-arguments and finding a method
binding (Decision 19), and the writer-identity question (Decision 11).
The four items this paragraph used to list as open (member key
namespace, unit-placeholder YAML, alias/template YAML, the Invoker API
shape for 5.3) are resolved; see Decision 21. 5.1-5.3 can now be handed
to the usual author/reviewer pipeline.

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

The characteristic landed with the Transform, not deferred:
`NonNegativeCharacteristic` (kind `NON_NEGATIVE`), YAML
`non_negative: {fill: column}` and `make_VariableSpec(non_negative='column')`,
FIELD only, with `column` as the only fill. It writes no Info key; 6.5
reads it from the owner's descriptor to place the Transform.

Round-off tolerance (author decision, for owner confirmation): a column
whose total is below zero by at most a small fraction of its column
scale `Σk mO,k |ck|` is treated as a zero total and filled to all zeros,
so a column an Invoker fully drains does not fail on regrid round-off.
The fraction is 10⁻¹² for R8 fields and 4 ε(R4) (about 5·10⁻⁷) for R4
fields, whose regrid round-off summing in R8 does not remove. These are
runtime judgment calls: 10⁻¹² matches the Decision 18 test tolerance, but
Decision 18's 10⁻¹⁰ fallback is test-only and has no runtime equivalent.
A column total of exactly zero is filled to zeros too. A total below that
bound, or a column holding a NaN or infinite value or weight, is the
error naming the column; the error names the PET and the PET-local
column index.

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

**19. Execution mechanics reuse the existing demand-driven update, not
new machinery (decision 12's reserved work, mechanics settled in
advance).** `pull_callback_get_arguments` is a loop over `get_bindings`
calling the existing `ComponentGraph%update(node_id)` on each
get-binding's bound member NodeId -- the same "ensure this node's value
is current, following the hierarchy's forward network" call every
ordinary connection already uses; no new pull mechanism.
`advance_callback_put_arguments` bumps the Invoker-side copy's node
revision the same way `ComponentGraph_DemandDrivenUpdate`'s
`finish_transform` already marks an ordinary transform's output fresh;
propagation into the Handler's real internal item (through
`SnapshotTransform`/`WeightedDeltaBackTransform`/etc., task 6.x) happens
the next time something calls `update()` on that downstream item, not as
a separate forced step inside `MAPL_CallbackExecute`.
*Amended 2026-11-23:* finding a method's binding from a bare
`ESMF_State` by an Info key recorded at REALIZE_ACCEPTED, as a free
function `MAPL_CallbackExecute(state, method, rc)` (mirroring legacy
`StateAddMethod.F90`'s `MAPL_CALLBACK_MAP` Info key), is superseded by
decision 21's amended Invoker API shape below. "No separate global
NodeId-keyed registry" still holds -- it turns out to be unneeded
outright, not merely avoided, once the iterator hands back a handle
that already carries what the Info key would have had to recover.

**20. `wire_callback_argument` requires both sides already resolved; a
DEFERRED characteristic is a caller-ordering bug, not something it
retries.** Ordinary connection resolution tolerates a DEFERRED
characteristic (e.g. an `EXPORT` with no `quantity_type:` yet) by
reporting `waiting` and retrying on a later connect pass
(`graphbuilder_resolve_connections`/`note_connection_waiting`).
`wire_callback_argument` has no such retry loop, because
`build_callback_method_binding` has no production caller yet (decision
12); it compares the real descriptors with `resolve_characteristics`
directly (passing `waiting`, not through `find_mismatched_characteristics`,
which always fails hard on any conflict) and `_FAIL`s with a clear
message if `waiting` comes back true, rather than silently skipping the
characteristic or guessing at a retry mechanism that does not exist.
This is deliberate: decision C1 places callback binding's Invoker-side
realization at ACCEPT_TRANSFER, the same phase ordinary connections
resolve in -- if a real caller ever invokes `build_callback_method_binding`
while something is still DEFERRED, that is a caller-ordering bug in the
code that invokes it (part of decision 12's reserved work), to be fixed
there, not papered over here.

**21. Member key namespace, unit-placeholder and alias/template YAML
syntax, and the 5.3 Invoker API shape (closes the four open items under
Decision 12).**

- **Member key namespace: no separator needed, no joined string built.**
  `CompositeStateMaterialization%materialize_member` already keeps a
  member out of ordinary connection/wildcard/History matching: it
  registers the member's `NodeId` but never adds it to the
  `ComponentGraph`'s flat `resource_index` map those paths use, so a
  member is reachable only by walking its owning composite node's own
  `state_members` map. There is nothing left to isolate with a
  separator. Every accessor (`MAPL_CallbackGetAttribute`,
  `MAPL_CallbackSetScalar`, decision 11) and every future message or API
  that names "this member of this callback State" keeps the State name
  and the member name as two separate strings; nothing ever joins them
  into one compound key for lookup. A message that wants a
  human-readable joined form for display only (never parsed back apart)
  may use `::` (unused elsewhere in component/item naming); it must not
  reuse `/`, which already means component path in a qualified export
  name.
- **Unit placeholder YAML: one `units:` key, scalar or one-entry
  mapping.** On the interface, `units: $U` declares a placeholder named
  `U`; every argument across the interface sharing `$U` must agree on
  units. `units: 'kg'` (any value not starting with `$`) is a concrete,
  fixed unit. A Handler binds a placeholder with a one-entry mapping at
  the same key, `units: {U: 'kg kg-1'}`; every placeholder the interface
  declares must be bound this way before the Handler's callback State
  can be realized. The scalar/mapping shapes are told apart with
  `ESMF_HConfigIsScalar`/`ESMF_HConfigIsMap`, the same calls
  `parse_var_specs.F90` already uses for other polymorphic keys.
- **Alias/template YAML: explicit opt-in keys on `declare_member`, new
  field default unchanged.** A member declaration gains two optional
  keys: `alias: <name>` binds the member as a zero-copy view of one
  already-existing Handler item (geometry/typekind/units come from the
  aliased item, per Decision B above); `template: true` on the same
  declaration expands it, via the existing wildcard-matching regex code
  already used for import names, into **one callback State per matching
  internal item, each containing one member** (e.g. `alias: 'TR.*'` with
  `template: true` on an interface's `tracer` argument yields three
  sibling callback States, one per `TR001`/`TR002`/`TR003`, each with its
  own single `tracer` member) -- owner decision 2026-11-23, amending this
  bullet's earlier "one member per matching internal item" wording, which
  conflicted with the working notes (topic B) and with
  `build_callback_state_binding`'s one `members%at(name)` lookup per
  fixed argument name inside one State. Whether a callback interface
  should be able to opt into the other shape (one State, many members) is
  left open for later, per-interface refinement; this is the only shape
  built now. Neither `alias:` nor `template:` changes `declare_member`'s
  existing meaning when absent: a plain declaration is still always a
  new, framework-allocated field. **Split from 5.1 into its own task 5.1b
  (owner 2026-11-23, following cumulative review): plain `declare_member`
  and single-item `alias:` are done and tested; `template:` expansion is
  not implemented at all.** Landing 5.1 without it is additive -- nothing
  downstream (5.2/5.3) needs template expansion, since the Invoker-side
  execution path operates per already-realized callback State regardless
  of how many sibling States a template produced it from.
- **Minimal YAML syntax for declaring a Handler's callback-State members
  (owner decision 2026-11-23): build just enough now, refine later
  alongside the GEOS registration-entry work (9.x).** Before this, a
  Handler's callback State and its members could only be built by
  calling `declare_member` from Fortran; no YAML key reached it. A
  callback-State EXPORT gains a `members:` map (member name -> an
  optional `{alias:, units:}`, the same two keys `declare_member` already
  takes) that `parse_var_specs`'s own `parse_callback_members` turns into
  one `declare_member` call per entry. This covers a Handler whose
  callback-State members are plain
  new fields or single-item aliases declared from YAML; template
  expansion and nested composite members stay Fortran-API-only for now
  (no concrete YAML shape designed yet for either). `alias:`/map-shaped
  `units:` remain a hard parse error anywhere outside a `members:` entry.
  **A `members:` entry only answers a FIELD-kind interface argument
  (owner 2026-11-23): reject it loudly at parse/advertise time when the
  interface argument is a FieldBundle, rather than letting the mismatch
  surface later at conformance-check time.** Real FieldBundle-via-YAML
  support is an undesigned follow-up, not part of this change.
  **5.1a (3D members, resolved):** a `members:` entry accepts
  `dims:`/`vertical_dim_spec:`/`vertical_grid:` (the top-level item's own
  key names; `vertical_grid:` is a direct sibling key on the member, not
  nested under `geometry:`, since a member has no `esmf_geom:` of its
  own), parsed with the same helpers a top-level item uses. `mirror` is
  rejected on `dims:`/`vertical_dim_spec:` since a member is never part
  of the IMPORT/EXPORT connection-resolution graph and has no partner to
  mirror from; `vertical_grid_mirror` is rejected by the generic
  unsupported-key error rather than a dedicated check, since it is not in
  a member's known-key list. A member has no realized field of its own
  for a harvest pass to read a stagger back from, so an omitted
  `vertical_dim_spec:` cannot use the DEFERRED status a top-level item
  would get here; it defaults explicitly to NONE, or to CENTER when the
  entry names its own `vertical_grid:`. `vertical_dim_spec: none`
  combined with an explicit `vertical_grid:` on the same entry is a
  parse-time error (a 2D field cannot also carry a vertical grid). See
  tasks.md 5.1a. A member's own `dims:` is invisible to the
  conformance-check dims helper, tracked as tasks.md 5.1c.
- **Invoker API shape for 5.3: an iterator, not count-and-index.**
  `MAPL_CallbackCollectionGet` and friends present an iterator over the
  bound Handlers for one collection (canonical pattern: advance, check
  done, fetch current), not a `count` plus `GetAt(i)` pair -- this also
  makes an out-of-range access harder to construct by accident. Access
  by qualified name (a lookup by a specific Handler's identity, as
  opposed to iterating all of them) and detailed error behavior (a loud
  `_FAIL` vs. a `found` out-argument) are deliberately left open past
  5.1-5.3: this is the first version of a new API with no real caller
  yet to learn from, legacy's closest analogs (the old "service" and
  "friendly" mechanisms) had essentially no error handling either, and
  over-specifying now risks guessing wrong ahead of real usage. 5.1-5.3's
  author should pick the smallest iterator shape that satisfies the
  same-grid get/put test in task 5.3 and leave qualified-name access and
  stricter error handling as later, compatible additions.

  *Amended 2026-11-23: `current()` returns a handle, not a bare
  `ESMF_State`.* Executing a bound Handler's method needs the Invoker's
  own `ComponentGraph` and the target method's `MethodGraphNode` NodeId
  (`invoke_callback_method`'s two arguments); a bare `ESMF_State` carries
  neither, which is exactly why decision 19's original design had
  `MAPL_CallbackExecute` recover them from an Info key. The iterator
  already resolves both, once, for each collection member while
  advancing -- it was built from the same `ComponentGraph` and the same
  matched `NodeId`s `materialize_callback_collection`/
  `build_callback_method_binding` produced. Handing that resolution back
  as part of `current()`'s return value is free; recovering it a second
  time from the State's own Info on every call is not, and needs
  machinery (the Info key, decision C1's REALIZE_ACCEPTED write) that
  buys nothing once the handle exists. So `current()` returns a small
  value (exact type name left to the implementer) wrapping the member's
  `ESMF_State` plus its already-resolved graph pointer and method
  `NodeId` lookup, with two operations: `%execute(method, rc)`
  (replaces the free-function `MAPL_CallbackExecute`, runs
  `invoke_callback_method`'s three phases) and `%get_state()` (returns
  the bare `ESMF_State`, unchanged, for `MAPL_CallbackGetAttribute`/
  `MAPL_CallbackSetScalar`, which stay free functions -- they only ever
  touch the State's own Info, confirmed to have no graph dependency in
  `CallbackAttributeAccess.F90`, so they have nothing to gain from the
  handle). No Info key is written for method-binding lookup at
  REALIZE_ACCEPTED; `record_callback_interface_name`'s Info key (5.1,
  `MAPL_InfoSetCallbackInterface`) is a separate, still-needed mechanism
  for a different purpose (resolving the attribute/call-scalar schema),
  unaffected by this amendment.

- **How an Invoker names its own argument providers (owner decision
  2026-11-23, task 5.2b): YAML keys on the callback IMPORT entry.**
  `build_callback_method_binding` needs, per callback argument, which of
  the Invoker's own items supplies its value before a Handler runs
  ("get", `provider_get_ids`) and which receives it afterwards ("put",
  `provider_put_ids`). The Invoker's callback IMPORT entry gains two
  optional maps from argument name to the Invoker's own item short name:
  `provides: {<argument>: <item>}` for get and
  `receives: {<argument>: <item>}` for put. This keeps the whole contract
  declarative in YAML, matching the Handler side's `members:`. The
  rejected alternatives were matching an argument to a same-named Invoker
  item by convention (a rename silently breaks the wiring) and a
  Fortran-only registration call (new API plus a must-run-before-advertise
  ordering rule). The owner expects a Fortran API binding for most or all
  user-configured callbacks eventually, as ordinary state items have
  (tracked as a later task). The priority now is minimal working code.

- **Put-direction (`receives:`) data movement is a known gap, accepted
  for now (owner decision 2026-11-24, task 5.2b).** Production binding
  (above) wires `get`/`provides:` argument data movement end to end: get
  edges join the hierarchy's one forward network, so
  `ComponentGraph%update()` already pulls them before a bound method
  runs. `put`/`receives:` arguments are not: `advance_callback_put_arguments`
  only advances a put argument's `NodeRevision`, and `update()` never
  walks a put network, so an OUT/INOUT argument's `receives:` item never
  receives the value a bound method wrote. This is a general property of
  write-back `DependencyNetwork`s (the ordinary INOUT-borrower return
  network has the same gap), not specific to callbacks. Fixing it needs
  either a put-network execution path or redesigning `receives:` items to
  share `NodeId`s with the Handler member instead of an edge (`provides:`
  already works and needs no such change); deferred as out of scope for
  5.2b. The fix belongs to task 6.5 (`CB6-WireDeltaBack`), which already
  plans to reuse the existing pull mechanics rather than add a new
  executor (Decision 19): wiring `SnapshotTransform`/
  `WeightedDeltaBackTransform`/`ConservativeColumnFillTransform` into the
  put path should make a downstream `update()` find them as the
  forward-network producer, closing the gap without new network-walking
  code. 6.5 depends on all of CB5-Execute (5.1b, 5.1e, 5.2b, 5.2c, 5.3,
  5.4) plus the already-done transform tasks 6.1-6.4.

- **Same-named Handlers merge through re-export (owner decisions
  2026-11-24, tasks 5.1b and 5.1d; tentative until there is real-world
  experience).** Callbacks are treated as collections of nearly
  identical callback States on both the Handler and the Invoker side.
  Re-export stays the one control over what is visible at each level of
  the hierarchy (owner): a bare name in a connection always means an
  export of `src_comp` itself, as in legacy. Descendants' exports are
  reached only path-qualified or through a re-export. There is no
  subtree search by bare name. This replaces an earlier same-day version
  of this bullet that matched a bare name at any depth under `src_comp`,
  which the owner rejected because it breaks legacy name lookup.
  - *Re-export merge.* A `ReexportConnection` whose destination name is
    already taken by a different node is an error today ("Re-export
    destination already exists"). If both the existing and the new
    source are callback exports that declare the same
    `callback_interface:`, the re-export instead adds the new source
    under the same name, so the name stands for several Handlers.
    Otherwise it is still the existing error. Re-exporting an
    already-merged name one level further carries every source along.
    Example, in GOCART.yaml:
    `{src_comp: DU, src_name: MIXING_CB, src_intent: export, dst_comp: <self>, dst_name: MIXING_CB, dst_intent: export}`,
    plus the same with `src_comp: SS`. PHYS.yaml then connects with
    `{src_comp: GOCART, src_name: MIXING_CB, dst_comp: TURB, dst_name: MIXING}`
    and TURB's collection holds the States of both DU and SS.
  - *Plain-name callback match.* For a callback-interface import and a
    `src_name` with no regex metacharacters, the matches are every
    export of `src_comp` itself under that name. That includes every
    re-exported source merged under it, and every template expansion
    declared under it (below). A regex `src_name` keeps the existing
    full-path match over the descendant namespace. The Invoker
    collection fill (5.2a, members keyed by qualified name) does not
    change.
  - *Declared name.* For an ordinary callback export it is its own
    short name. For a `template: true` expansion (5.1b) it is the name
    the template was declared under. Each expansion gets a generated,
    unique short name, `<declared>_<matched item>` (for example
    `MIXING_CB_DU001`), because one `ESMF_State` cannot hold two items of
    the same name. Users never write generated names. A connection or a
    re-export naming `MIXING_CB` on DU takes all of DU's expansions.
  - *Opt-in by declaration.* Merging several sources under one name
    requires each to declare the same `callback_interface:`. An
    undeclared export keeps today's exact-name behavior.
  - *Empty is allowed.* A plain-name callback connection that matches no
    Handler resolves to an empty collection (the Invoker loops zero
    times), not an unresolved import. This is consistent with the
    owner's answer for an empty template (2026-11-23: a thinned GEOS may
    run with no tracers). A later option on the callback import requires
    a match count (`>= 0` default, `>= 1`, `== 1`): task 5.1e. A
    candidate that exists but is rejected (declares a different
    interface, fails conformance, or an explicit path that resolves to
    nothing) is not "no Handler"; it stays unresolved with its reason,
    the same as any other failed connection.
  - *Known limitation.* An ordinary (non-callback) connection to a name
    that re-export has merged under several sources is not itself a
    merge target: today's resolution picks one of the merged nodes.
    Acceptable because merging only triggers when both sources declare
    the same `callback_interface_id`, so a non-callback import
    connecting to such a name is not an intended use; revisit if one
    turns up.
  - Rejected: a bare-name search at any depth under `src_comp` (breaks
    legacy lookup and bypasses re-export as the visibility control).
    Also rejected: automatic merging of same-named callback exports on
    propagation (service-like, but exports would no longer need a
    re-export). Also rejected: one collection-valued export item per
    templated declaration (a new item class, and 5.2a's fill rewritten).
    Also rejected: generated suffix names plus a regex at the ancestor
    (`'.*/MIXING_CB_.*'`), which puts the naming convention into every
    ancestor's YAML.

**22. `turbulent_mixing`'s `bottom_boundary` carries all four conditions
the column solver implements (owner decision 2026-11-24, found by a 9.2
preflight, amending the 9.1 artifact).** The column solver
(`ImplicitColumnDiffusion`) implements `neumann_homogeneous`, `neumann`,
`robin` and `dirichlet`; the GEOS registration file shipped under 9.1
declared only `allowed: [neumann_homogeneous, robin]`, which cannot
satisfy the already-landed spec scenario M3 (requires a `neumann`
result). `allowed:` widens to all four. Neither `neumann`'s flux value
nor `dirichlet`'s pin value becomes a new interface argument, attribute
or call_scalar: both are supplied as TestInvoker-internal constants, one
set per mixing class, the same way M3/M4/M9's own test scenarios already
pick fixed numbers (flux, `robin`'s r, dt, kappa) rather than sourcing
them from a Handler. `robin`'s coefficient keeps coming from the
existing `dry_deposition_velocity` argument (`rules:` unchanged);
converting it to Robin's linear-term coefficient needs a reference air
density no argument supplies, so the TestInvoker uses a fixed test
constant (ρ = 1.2 kg/m³, M9's own test value), not a physical input.
*Rejected:* keeping the 2-value list and rewriting M3 (would discard an
already-landed scenario); widening to three and dropping `dirichlet`
from production (matches the plan note's weakest reading but leaves no
path from a real registered member to the solver's fourth, already-built
and already-unit-tested condition).

## Risks / Trade-offs

- [Execution is unbuilt and large] → It is split out as the owner track.
  Everything that can be built and tested without running a method
  (data model, loader, freeze, Info wrappers, checker on hand-built
  States, the two Transforms stand-alone, the column solver) is ordered
  first and can proceed in parallel.
- [HConfig may silently keep one of two duplicate keys] → Resolved: a
  probe test (`Test_HConfigDuplicateKeys.pf`) found `ESMF_HConfigGetSize`
  and `ESMF_HConfigIter` both see two entries, in file order, but the
  `ESMF_HConfigCreateAt(keyString=...)` lookup every existing parser
  uses silently returns only the first occurrence's value; the shipped
  files' headers and the user guide document this. MAPL does not add
  its own YAML parser.
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

- Resolved: duplicate top-level keys in HConfig are not collapsed or
  flagged. `ESMF_HConfigGetSize` reports 2 for a map with a duplicate
  top-level key, and `ESMF_HConfigIter` visits both entries in file
  order under the same key name; but `ESMF_HConfigCreateAt(hconfig,
  keyString=...)` — the lookup pattern production parsers use — silently
  returns only the first occurrence's value, never the last, never an
  error (`Test_HConfigDuplicateKeys.pf`).
- Whether `ESMF_InfoGet` converts silently (settled by the Info probe;
  affects only which ESMF call the type check uses).
- A rule's trigger value is normalized from its YAML source text only
  for the logical case (several bool spellings all become the literal
  `true`/`false` `CallbackInterface` expects); no canonical textual
  form is defined yet for comparing numeric rule values at
  rule-matching time, since rule matching itself is not built. Left
  for whoever builds rule matching.
