# Plan: Callback contracts and the interface registry

> Canonical copy: `docs/graph/plans/callback-contracts-and-registry.md`
> (in repo). The openspec change
> `openspec/changes/callback-contracts-and-registry/` (proposal, delta
> specs, design.md, tasks.md) **takes precedence over this plan's
> summaries**. Every task records its Status in the same change as the
> work: tick its `tasks.md` items and update its Status line here. If
> the plan and the source disagree, stop and report rather than guess.

Status: reviewed by the owner (2026-11-13); see "Owner questions"
below for the resolution of each item. Drafted from the design note
`mapl3-callback-contracts-and-registry.md` (2026-11-07) of the
`geos-mapl3-design` planning repository, binding decisions 62, 63, 64
and 66 of its `PLAN.md`. That repository is not part of this checkout;
design.md restates everything implementation needs.

**Integration branch:** `feature/callback-contracts-and-registry` (own
branch, Q-W1 resolved: does not share `feature/#5286-graph-phase-6`).
Tracking issue: GEOS-ESM/MAPL#5513. Proposal PR (artifacts only, no
code): GEOS-ESM/MAPL#5514. The 14 ready tasks below are open as
volunteer issues (Q-W2 resolved: go): GEOS-ESM/MAPL#5515-#5528.

**Verified against:** `callback/openspec-conversion` @ `e9da20e1d`
(2026-11-13, on top of `feature/#5286-graph-phase-6`). The design note
was written against `feature/#5286-graph-phase-5` @ `e471ba5a9`; see
"Observations" for what changed and what the note got wrong.

**Workflow.** Same as `docs/graph/plans/separation-of-concerns.md`
("Workflow", "Working with the owner", "Review and commit", "Rules for
every agent prompt"), unless the owner decides otherwise (Owner
question Q-W1). In short: one task = one commit (or a short series);
agents do not commit, they leave the work in the tree and a proposed
message in `$(git rev-parse --git-dir)/PLAN_TASK_MSG`; every commit
ends with a trailer naming the task:

```
Plan-Task: CB1-AttributeSpec
```

Every task leaves `ctest -L ESSENTIAL` green on NAG, then gfortran.
Docs-only tasks write "docs only; ctest not run".

## Context every task prompt should include

- Read root `AGENTS.md`, `docs/graph/ARCHITECTURE.md`,
  `docs/graph/CONVENTIONS.md`, `docs/graph/callbacks.md`, then the
  change's `proposal.md`, `design.md`, the delta spec(s) the task
  implements, and its `tasks.md` items.
- Code: `superstructure/generic/graph/callback/`,
  `superstructure/generic/GraphBuilder_CallbackWiring_smod.F90`, the
  callback branch of `GraphBuilder_ConnectionResolution_smod.F90`
  (`resolve_callback_destination`, `source_satisfies_import`),
  `GraphBuilder.F90` (`resolve_callback_import`).
- Landed specs this change extends:
  `openspec/specs/graph/callback-data-model/spec.md`,
  `openspec/specs/graph/callback-wiring/spec.md`. Their archived designs:
  `git show graph-phase-5-archive:openspec/changes/archive/2026-09-20-callback-data-model-registry/design.md`
  and `.../2026-09-21-callback-wiring/design.md`.
- Graph code reaches Info only through `MAPL_Info*` / `MAPL_Field*` /
  `MAPL_State*` wrappers (CONVENTIONS §5a); extend `InfoUtilities.F90`
  rather than calling `ESMF_Info*` directly.
- Duplicate, do not delegate: no calls into legacy `transforms/`
  (for example `NormalizationTransform`), `specs/*Aspect`, `registry/`.

## Target design (summary; design.md is authoritative)

- **Exact contract, structural conformance.** An interface states
  version, summary, arguments (kind, dims, optional, extensive,
  `weight_of`), per-method access (+ `put_flavour`), attributes (type,
  scope, `host_argument` for scope `member`, required/default, allowed),
  call scalars, and rules. A Handler may provide more, never less.
- **Files only.** `./callback_interfaces.yaml`, loaded in
  `mapl_initialize`, all-or-nothing validation, frozen by
  `mapl_cap_create` before the root `SetServices`, CRC-32 startup
  report. Tests may still register in code.
- **Two check stages.** Declaration level at connect
  (`export_conforms_to_interface`, extended); realized level on the
  `ESMF_State` before first method execution
  (`check_callback_state_conformance`). All violations of a State in
  one message.
- **Execution** (owner track): realized callback States, a real
  `ESMF_MethodExecute` invoker, `MAPL_CallbackExecute`.
- **Transforms:** characteristics on callback arguments, first-order
  conservative regrid by default, `SnapshotTransform`,
  `WeightedDeltaBackTransform`, `ConservativeColumnFillTransform`.
- **Test tools** and the `toy_scaling` then `turbulent_mixing` slices.

## Critical path: Invoker-side execution (owner track)

Invoker-side method execution **does not exist** in MAPL3 today, and
it is more than an "execute" call. Missing, all of it:

1. A Handler's callback State is never realized as an `ESMF_State`
   holding its members. Graph state population skips STATE items, and
   the Invoker's collection (`materialize_callback_collection`) is a
   structurally empty `ESMF_State` whose members exist only as graph
   `NodeId`s.
2. No `ESMF_MethodExecute`-backed `StateMethodInvoker`
   (`MethodInvocationAdapter.F90`, `StateMethodInvocation.F90` say so).
   `MAPL_ExecuteMethod`, cited by the design work, does not exist on
   this branch. The Handler side (`MAPL_MethodAdd`,
   `StateAddMethod`) does exist.
3. `build_callback_state_binding`, `build_callback_method_binding` and
   `invoke_callback_method` have no production caller; only tests call
   them.
4. No Invoker API to walk a collection and execute a method by name.

The owner takes this on directly (`CB5-Execute`). It is **not** offered
to volunteers. Every task that runs a callback method end to end waits
for it: CB5-LazyCheck, CB6-ArgCharacteristics, CB6-WireDeltaBack, the
recording half of CB7-MockHandler, CB7-MockInvoker, all of CB8 and the
end-to-end half of CB9.

## Tasks

Labels are `CB<group>-<Name>`; the group number matches the `tasks.md`
section. "Ready" means it can become a `ready` GitHub issue for a
volunteer agent today: no unlanded dependency, a self-contained brief,
no owner decision pending. Size: S (under a day, one or two files),
M (a few files, one subsystem), L (several subsystems or new numerics).
Risk: what is most likely to go wrong.

| Task | tasks.md | Author | Reviewer | Ready | Size / risk | Depends on |
|---|---|---|---|---|---|---|
| CB0-HConfigDup | 0.1 | sonnet | sonnet | **yes** | S / low; probe only | - |
| CB0-InfoTypes | 0.2 | sonnet | sonnet | **yes** | S / low; probe only, result shapes CB2 | - |
| CB1-ArgSpec | 1.1 | sonnet | sonnet | **yes** | S / low; defaults must keep today's meaning | - |
| CB1-AttributeSpec | 1.2 | sonnet | sonnet | **yes** | S / low; list-typed defaults | - |
| CB1-CallScalarSpec | 1.3 | haiku | sonnet | **yes** | S / low | - |
| CB1-Rule | 1.4 | haiku | sonnet | **yes** | S / low | - |
| CB1-MethodOptions | 1.5 | sonnet | sonnet | **yes** | S / low; second map beside `AccessSpecMap` | - |
| CB1-Interface | 1.6 | sonnet | opus | no | M / medium; cross-checks (rules, weights, host arguments) | CB1-ArgSpec, -AttributeSpec, -CallScalarSpec, -Rule, -MethodOptions |
| CB1-RegistryFreeze | 1.7 | haiku | sonnet | **yes** | S / low; singleton state in tests (unfreeze for tests?) | - |
| CB2-InfoWrappers | 2.1 | sonnet | opus | no | M / medium; touches infrastructure API, overload ambiguity | CB0-InfoTypes |
| CB3-CRC32 | 3.1 | haiku | sonnet | **yes** | S / low | - |
| CB3-Loader | 3.2 | sonnet | opus | no | M / medium; error wording, all-or-nothing | CB1-Interface |
| CB3-InitLoad | 3.3 | sonnet | sonnet | no | S / medium; every pFUnit executable now loads | CB3-Loader |
| CB3-CapFreeze | 3.4 | sonnet | sonnet | no | S / low | CB3-InitLoad, CB1-RegistryFreeze, CB3-CRC32 |
| CB3-ShippedFiles | 3.5 | sonnet | sonnet | no | S / low; CMake install and test copies | CB3-Loader, CB0-HConfigDup |
| CB3-Constants | 3.6 | haiku | sonnet | no | S / low | CB3-ShippedFiles |
| CB3-YamlKey | 3.7 | sonnet | sonnet | no | S / medium; parser, `make_VariableSpec` keyword | CB3-InitLoad |
| CB4-Messages | 4.1 | sonnet | sonnet | **yes** | S / low; wording fixed by specs | - |
| CB4-DeclCheck | 4.2 | sonnet | opus | no | M / medium; changes existing wiring tests | CB1-ArgSpec, CB3-YamlKey, CB4-Messages |
| CB4-StateCheck | 4.3 | opus | sonnet | no | M / medium; check order, unknown keys, weight geometry | CB1-Interface, CB2-InfoWrappers, CB4-Messages |
| CB4-Accessors | 4.4 | sonnet | opus | no | M / medium; writer tracking | CB2-InfoWrappers, CB4-StateCheck |
| **CB5-Execute** | 5.1-5.3 | **owner** | sonnet / opus | **no: owner track** | L / high; critical path | CB1-Interface |
| CB5-LazyCheck | 5.4 | sonnet | sonnet | no | S / low | CB5-Execute, CB4-StateCheck |
| CB6-ArgCharacteristics | 6.1 | opus | sonnet | no | M / high; first real transforms on callback edges | CB5-Execute |
| CB6-Snapshot | 6.2 | sonnet | sonnet | **yes** | S / low; stand-alone Transform | - |
| CB6-WeightedDeltaBack | 6.3 | opus | gpt-sol | **yes** | M / high; numerics, multi-port, regrid accuracy (M2) | - |
| CB6-ColumnFill | 6.4 | opus | sonnet | **yes** | M / medium; stand-alone Transform + diagnostic (M4) | - |
| CB6-WireDeltaBack | 6.5 | opus | sonnet | no | M / high | CB5-Execute, CB6-ArgCharacteristics, CB6-Snapshot, CB6-WeightedDeltaBack, CB6-ColumnFill |
| CB7-MockHandler | 7.1 | sonnet | opus | no | M / medium | CB1-Interface, CB4-Accessors (construction); CB5-Execute (recording) |
| CB7-MockFunction | 7.2 | sonnet | sonnet | no | S / medium; parser may not fit (stop and report) | CB7-MockHandler |
| CB7-MockInvoker | 7.3 | sonnet | sonnet | no | M / medium | CB5-Execute, CB7-MockHandler |
| CB7-Expectations | 7.4 | sonnet | sonnet | no | M / low; later scope | CB8-Matrix |
| CB8-ToyComponents | 8.1 | sonnet | sonnet | no | S / low | CB5-Execute, CB3-YamlKey |
| CB8-Matrix | 8.2-8.3 | sonnet | opus | no | M / medium | CB8-ToyComponents, CB7-MockHandler, CB7-MockInvoker, CB6-ArgCharacteristics, CB4-DeclCheck, CB5-LazyCheck, CB3-CapFreeze |
| CB9-GeosEntry | 9.1 | haiku | sonnet | no | S / low | CB3-ShippedFiles |
| CB9-ColumnSolver | 9.2 (solver part) | opus | gpt-sol | **yes** | M / medium; M-matrix positivity (M9), stand-alone | - |
| CB9-TestInvoker | 9.2 (component) | sonnet | opus | no | M / medium | CB9-ColumnSolver, CB5-Execute |
| CB9-MockOwners | 9.3 | sonnet | sonnet | no | S / low | CB7-MockHandler |
| CB9-Matrix | 9.4 | opus | gpt-sol | no | L / high; budget identities at 10⁻¹² | CB9-TestInvoker, CB9-MockOwners, CB6-WireDeltaBack, CB9-GeosEntry |
| CB10-Docs | 10.1-10.2 | sonnet | sonnet | no | S / low | CB8-Matrix (10.2 may start after CB3-CapFreeze) |
| GEOS-SetupScripts | (outside MAPL) | GEOS team | owner | no | S / low; tracking only | CB3-ShippedFiles |

`(10.3)` ARCHITECTURE.md callback rows: meta batch, not a task.

### Ready for volunteers today (dependency-free)

CB0-HConfigDup, CB0-InfoTypes, CB1-ArgSpec, CB1-AttributeSpec,
CB1-CallScalarSpec, CB1-Rule, CB1-MethodOptions, CB1-RegistryFreeze,
CB3-CRC32, CB4-Messages, CB6-Snapshot, CB6-WeightedDeltaBack,
CB6-ColumnFill, CB9-ColumnSolver.

Why these: each touches new files or one existing type, needs no
unlanded API, and has a stand-alone test (probes, value types, a pure
utility, message text, or a Transform/solver tested on fixed numbers).
CB6-WeightedDeltaBack, CB6-ColumnFill and CB9-ColumnSolver are numerics
and risky; give them an opus-tier author or a strong volunteer and the
exact M2/M4/M9 numbers. The CB1 tasks touch neighbouring files
(`graph/callback/`, `graph/containers/`, their `CMakeLists.txt`); run
at most two at once to keep merges trivial. Q-W2 asks whether the owner
wants them outsourced at all.

Not ready, and why: CB1-Interface and everything after it waits on the
CB1 value types; CB2-InfoWrappers waits for CB0-InfoTypes' facts;
anything that executes a method waits for **CB5-Execute (owner
track)**.

### Per-task notes

**CB0-HConfigDup.** Fact-finding test for duplicate top-level keys
(design note §3.3, F11): which entry wins, whether any HConfig option
reports it. No production code; MAPL must not add a second YAML parser.
- **Status:** not started

**CB0-InfoTypes.** Probe what `ESMF_InfoInquire` reports for each stored
type (i4/i8/r4/r8/logical/string, arrays), whether `ESMF_InfoGet`
converts silently, and whether keys under a nested prefix can be
enumerated. Result goes into design.md Decision 4.
- **Status:** not started

**CB1-ArgSpec / -AttributeSpec / -CallScalarSpec / -Rule /
-MethodOptions / -Interface / -RegistryFreeze.** Data model per the
`graph/callback-data-model` delta. Existing tests must pass unchanged
(defaults). CB1-RegistryFreeze must keep tests isolated: if a test
freezes the singleton, a test-only reset is acceptable (precedent:
`set_materialize_extensions`).
- **Status:** not started (each)

**CB2-InfoWrappers.** New `MAPL_InfoGetShared` / `MAPL_InfoSetShared`
specifics (r8_1d, string list, State-own-Info for logical/i4/r4/r8),
stored-type query, key enumeration. Overload, do not invent new generic
names, if the compilers accept it; stop and report on ambiguity.
- **Status:** not started

**CB3-CRC32 / -Loader / -InitLoad / -CapFreeze / -ShippedFiles /
-Constants / -YamlKey.** Per the `graph/callback-registration-file`
delta. CB3-InitLoad changes every pFUnit executable that uses
`mapl_pFUnit_Initialize_mod`: with no file in the working directory the
registry stays empty and nothing changes; check ESSENTIAL on both
compilers. CB3-YamlKey adds the YAML key the landed `callback-wiring`
change deferred.
- **Status:** not started (each)

**CB4-Messages / -DeclCheck / -StateCheck / -Accessors.** Per the
`graph/callback-contract-conformance` delta. CB4-DeclCheck changes how
a non-conforming match is reported only for a Handler that declares
its interface; undeclared matches keep today's skip-and-report.
- **Status:** not started (each)

**CB5-Execute (owner track; critical path).** See "Critical path"
above. Reserved for the owner; not offered to volunteers.
- **Status:** not started

**CB5-LazyCheck.** Hook the realized-State check into execution and
add the explicit collection check.
- **Status:** not started

**CB6-ArgCharacteristics / -Snapshot / -WeightedDeltaBack / -ColumnFill
/ -WireDeltaBack.** Per the `graph/callback-wiring` delta. The three
Transforms are tested stand-alone first (decision 14 of the design
track: each transform tested once, alone).
- **Status:** not started (each)

**CB7-MockHandler / -MockFunction / -MockInvoker / -Expectations.**
Per the `graph/callback-test-tools` delta. Configurable components
follow `gridcomps/configurable/ConfigurableGridComp.F90`.
- **Status:** not started (each)

**CB8-ToyComponents / -Matrix.** `toy_scaling` slice, P1-P5 and
F1-F12.
- **Status:** not started (each)

**CB9-GeosEntry / -ColumnSolver / -TestInvoker / -MockOwners /
-Matrix.** `turbulent_mixing` slice, M1-M9 and MF1-MF9.
- **Status:** not started (each)

**CB10-Docs.** `docs/graph/callbacks.md` and the user-guide section.
- **Status:** not started

**GEOS-SetupScripts (tracking only).** GEOS setup scripts copy the
installed GEOS registration file into the run directory as
`callback_interfaces.yaml`, and warn at copy time and on re-setup when
the run-directory copy differs from the installed one. GEOSgcm fixture
work, not MAPL.
- **Status:** not started

## Dependency summary

CB1 value types -> CB1-Interface -> {CB3-Loader -> CB3-InitLoad ->
{CB3-CapFreeze, CB3-YamlKey}, CB4-StateCheck, CB7-MockHandler};
CB0-InfoTypes -> CB2-InfoWrappers -> {CB4-StateCheck, CB4-Accessors};
CB0-HConfigDup -> CB3-ShippedFiles.
**CB5-Execute (owner)** -> {CB5-LazyCheck, CB6-ArgCharacteristics,
CB7-MockInvoker, CB8-ToyComponents, CB9-TestInvoker}.
{CB6-Snapshot, CB6-WeightedDeltaBack, CB6-ColumnFill,
CB6-ArgCharacteristics} -> CB6-WireDeltaBack -> CB9-Matrix.
CB8-Matrix closes the first slice; CB9-Matrix closes the GEOS slice.

**Suggested run order:** volunteers take the ready set while the owner
builds CB5-Execute; the main line runs CB1-Interface -> CB2 -> CB3 ->
CB4, which is everything the first slice needs except execution. When
CB5-Execute lands, CB6-ArgCharacteristics, CB7 and CB8 follow; CB9
after.

## Owner questions

Each records the call made in the artifacts, so work can proceed. The
owner confirms or overrides. Items 1-7 are the design note's §7 open
points; the rest are author decisions in the note that are flagged for
review, or new ambiguities found while converting it.

**From the design note's open points (§7):**

1. **Duplicate keys in HConfig** (§7.1). Settled by the probe
   CB0-HConfigDup; the result is documented in the shipped files and
   the user guide. No owner decision needed unless the result is "one
   wins silently and no option reports it": then confirm that silent
   behavior is acceptable (no second parser), or say what you want.
2. **When the attribute checks run** (§7.2). Call made: two stages.
   (a) At connect, the existing `export_conforms_to_interface` is
   extended (optional arguments, dims, all violations) and checks only
   what the `VariableSpec` declares. (b) The attribute, rule,
   unknown-key and weight checks run on the realized `ESMF_State`, once
   per State, lazily before its first method execution, or when the
   Invoker asks. Reason: Handlers set attribute values in their own user
   initialization, which can run after the Invoker's. Consequence: these
   errors appear at the first run, not during initialization. Confirm,
   or name an initialization point you want (for example a new
   post-`INIT_USER` pass).
3. **Installed-copy warning** (§7.3, author decision). Call made: the
   GEOS setup scripts compare and warn; MAPL knows nothing of the
   install layout and only logs path and checksum. Confirm. Tracked as
   GEOS-SetupScripts, outside MAPL.
4. **Where the configurable Handler keeps full recorded values** (§7.4).
   Call made: as the mock's own export fields
   (`mapl_mock_<method>_<argument>`), not nested in the callback State
   (nested fields would become extra members the Invoker and collection
   matching can see). Scalar records stay in the callback State's Info
   under `mapl_mock/`. Confirm.
5. **Weight on another geometry** (§7.5, author decision). Call made:
   rejected at wiring (MF4), no regrid inserted. Confirm.
6. **Mixing operator uses the delivered `air_mass`** (§7.6). Settled by
   you (option a); implemented as stated. No question.
7. **Demonstrating the weighting with M2/M3/M4** (§7.7). Settled by you
   (option a). No question.

**Author decisions in the note, honored, for review:**

8. **Missing registration file.** Call made: initialization succeeds;
   the first callback-interface lookup fails with "no
   callback_interfaces.yaml found in <cwd>". Confirm.
9. **Checksum algorithm.** The note said "what MAPL uses elsewhere, else
   CRC-32 or SHA-256". MAPL has no checksum routine. Call made: CRC-32
   in Fortran (small, no dependency; enough to spot an edit). Confirm,
   or ask for SHA-256.
10. **Startup report includes each interface's summary** (addition to
    decision 63). Kept. Confirm.
11. **Weight checks** (§2.3): the loader requires the weight whenever
    the weighted argument is required (so a missing `air_mass` is
    MISSING_ARGUMENT, MF1); WEIGHT_MISSING covers only an optional pair
    (MF1b). Kept. Confirm.
12. **`toy_scaling` attributes scoped `state`**, `turbulent_mixing`'s
    scoped `member`. Kept. Confirm.
13. **Attribute accessor takes its namespace from the invoked callback
    State** (decision 62 author decision). Kept; the framework writes the
    interface name into the State's Info at binding. Confirm.
14. **Invoker coding rule:** every `select case` on an attribute value
    ends in an error default (F12). Kept as a documented rule (user
    guide), not something the framework can enforce. Confirm.
15. **Put side uses first-order conservative too** (decision 66, "owner's
    silence, not an explicit answer"). Kept: the delta's mass part and
    the realized flux share one first-order map. Please confirm
    explicitly.
16. **`surface_upward_flux` is `extensive`**, and `m_T` is the copy
    delivered at `get`, `m_O` the owner's bound field read at `put`.
    Kept. Confirm.
17. **Test geometries and tolerance** (§5.3, §5.7, §5.8): 4°×5° Invoker,
    2°×2.5° / 6°×7.5° owners, edge-aligned (`DE`, `PE`), three levels;
    R8 fields, 10⁻¹² of the column scale, 10⁻¹⁰ fallback. Kept. Confirm.
18. **Column fill algorithm** (M4): scale `max(c, 0)` by a non-negative
    ratio per column. Kept. Confirm.
19. **Configurable Handler keys** (`geometry`, `supply` forms, `function`
    via the state arithmetic parser with `lon`, `lat`, `level`). Kept;
    whether the parser fits is checked in CB7-MockFunction, which stops
    and reports if not. Confirm.

**New ambiguities found in conversion:**

20. **Which member hosts member-scoped attributes.** The note says
    "each member's Info" and "one member (one tracer) per callback
    State", but `turbulent_mixing` has several field arguments, and
    `air_mass` (for example `DELP`) is shared by many owners' callback
    States. Putting `turbulent_mixing/mix_like` on every member would
    make owners with different `mix_like` conflict on the shared
    `DELP` Info. **Resolved (2026-11-13):** the owner rejected the
    drafted interface-wide `subject` indirection as unnecessary framing
    — `mix_like` is simply an attribute of the `mixed_quantity`
    argument, so it is declared that way directly. Each member-scoped
    attribute now names its own `host_argument` (the argument whose
    bound member hosts it); there is no `subject` concept. Applied to
    the data-model, registration-file and conformance specs and to
    `design.md` Decision 2.
21. **Making conformance a wiring error without breaking today's
    behavior.** Today a pattern match that does not conform is skipped
    and reported as a warning ("no registered extension provider for
    mismatched characteristic"), and the landed spec requires that.
    The note wants violations to fail wiring. Call made: a Handler's
    STATE export may declare `callback_interface: <name>`; a declared
    Handler that does not conform fails wiring with the full report; an
    undeclared match keeps today's skip-and-report. Options: (a) as
    drafted; (b) every non-conforming STATE match fails (simpler, but a
    broad regex may match unrelated State exports). Recommendation: (a).
22. **YAML key and `make_VariableSpec` keyword for callback
    interfaces.** The landed `callback-wiring` change deferred them;
    the test gridcomps need them. Call made: `callback_interface:
    <name>` (resolved through the registry when the spec is read) and a
    keyword of the same name. Confirm the name.
23. **Names of the shipped files.** MAPL looks for the fixed name
    `callback_interfaces.yaml`, but ships two files to `etc/`. Call
    made: `etc/callback_interfaces_test.yaml` and
    `etc/callback_interfaces_geos.yaml`; setup scripts and test CMake
    copy the right one in as `callback_interfaces.yaml`. Confirm.
24. **MF9 timing.** The note says the test Invoker rejects an unused
    argument "at initialization". Its members' attributes may not be set
    until the Handlers' user initialization, which can run later. Call
    made: the test Invoker checks before its first method execution.
    Confirm.
25. **Column with a negative total.** Column fill cannot fix a column
    whose total is negative. Call made: an error naming the column.
    Options: (a) error; (b) clip to zero and report the lost mass in the
    diagnostic. Recommendation: (a) for the test slice.
26. **Where real owners declare `non_negative`.** The mock takes
    `non_negative: {fill: column}`; for real components this is an
    owner characteristic (design track Group C) with no `VariableSpec`
    key yet. Call made: a never-compared characteristic; its YAML key
    and `make_VariableSpec` keyword are added by CB6-ColumnFill with the
    same spelling. Confirm the spelling, and whether ACG needs a column
    now or later.
27. **Info key prefix.** MAPL writes shared attributes under
    `/MAPL/shared/`. Call made: interface keys are relative to it
    (stored as `/MAPL/shared/toy_scaling/operation`; messages show
    `toy_scaling/operation`). Confirm, or ask for top-level
    `toy_scaling/...` keys (which would bypass the MAPL wrappers).

**Workflow:**

- **Q-W1.** Does this plan follow `separation-of-concerns.md`'s
  workflow (integration branch, `Plan-Task:` trailers, agents do not
  commit, meta lands) on its own integration branch, or does it share
  `feature/#5286-graph-phase-6`?
- **Q-W2.** May the "ready" tasks be outsourced as GitHub issues now,
  or should they wait until you have reviewed this plan and the
  change?

## Observations (drift between the design note and current source)

Checked at `e9da20e1d` against the note's claims (written at
`e471ba5a9`). None changes the shape of the proposal; items 1 and 2 make
the execution track larger than the note suggests.

1. **"No Invoker-side execute API" understates the gap.** Besides no
   `ESMF_MethodExecute`-backed invoker (confirmed), the Handler's
   callback State is never realized as an `ESMF_State` with its members
   (state population skips STATE items), the Invoker's collection State
   is structurally empty (members are `NodeId`s only), and
   `build_callback_state_binding`, `build_callback_method_binding`,
   `invoke_callback_method` have no production caller. See "Critical
   path".
2. **`MAPL_ExecuteMethod` does not exist** (note marked it
   Unverified). Handler-side `MAPL_MethodAdd` (`MAPL_Generic.F90`) and
   `StateAddMethod` (`superstructure/state/StateAddMethod.F90`) exist;
   the Python bridge has the only `ESMF_MethodExecute` call.
3. **Rejected matches are a warning with a misleading text.**
   `export_conforms_to_interface` returns a reason; the caller pushes
   it into `unsupported`, and the connect hook logs "no registered
   extension provider for mismatched characteristic ...", which is not
   about characteristics. Noted, not fixed.
4. **Get and put networks.** Since `e471ba5a9`, `CallbackMethodBinding`
   stores only the put network; get edges join the hierarchy's forward
   network (`docs/graph/callbacks.md`). The note's "pre-call network"
   is therefore the forward-network pull, its "post-call network" the
   per-method put network. design.md uses that mapping and does not
   rename anything.
5. **Info wrappers.** The note's list of `MAPL_InfoGetShared` specifics
   is right. It did not mention two things the contract checks need:
   there are no non-string getters or setters for a State's **own**
   Info (scope `state`, such as `factor`), and all shared keys live
   under `/MAPL/shared/` (`INFO_SHARED_NAMESPACE`), so the note's
   `toy_scaling/operation` is a relative key.
6. **No YAML key or keyword for `callback_interface_id`.** The landed
   wiring change deferred it; the note's test components need it. Added
   as CB3-YamlKey.
7. **`MAPL.generic.graph` does not call `mapl_initialize`**
   (`GraphCoreTestInit.F90`); its callback tests register in code,
   which the files-only rule allows for tests.
8. **No checksum routine in MAPL** (the note assumed one might exist).
9. **Lat-lon citations hold:** `case ('DE')` in
   `infrastructure/geom/LatLon/LonAxis/get_lon_range.F90`, `case ('PE')`
   in `.../LatAxis/get_lat_range.F90`.
10. **`docs/graph/callbacks.md` source paths are stale:** its first
    line lists the callback modules under `superstructure/generic/graph/`,
    but they are in `superstructure/generic/graph/callback/`. Fix in
    CB10-Docs.
