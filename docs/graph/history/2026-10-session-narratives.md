# Graph-layer session narratives (historical)

Moved verbatim from `docs/graph/ARCHITECTURE.md` (as of 2026-10-21) when
that file was reduced to current state only. Nothing here describes the
tree as it is now: "this session", "uncommitted", and "active change"
refer to the moment each passage was written (mid-October 2026). The
`hierarchy-wide-component-graph` change is now archived at
`openspec/changes/archive/2026-10-17-hierarchy-wide-component-graph/`.
Kept for the debugging-methodology lessons (notably the "pre-existing
failure" mistake in section 3 and the two surprises in section 7).

---

## 3. Task Group 6 narrative (historical — now fully committed)

**[UPDATED, this session]** This section originally described
*uncommitted* working-tree state. Group 6 (including task 6.8) has
since been committed as `9fdea0fd`; Groups 7, 8, and 9 followed as
`bc51fc9c`, `1ea0fdc1`, and `15a50ec5` respectively — all landed on
this branch, confirmed clean by `git status` at the start of this
session. The narrative below (the regression-discovery story for task
6.8, in particular) is preserved as-is for its debugging-methodology
value; treat every "uncommitted"/"this working tree" reference in it
as historical, not current. Current branch state as of this session:
Task Groups 1-9 complete and committed; Task Group 10 (full regression
pass + spec-doc updates) is the one this session performed — see §2.2
above (updated spec docs) and §6 below (current phase-status summary)
for its outcome.

**[UPDATED, `openspec/changes/simpleconnection-graph-resolution`]**
§3.2's own table entry for `GraphBuilder_ConnectionResolution_smod.F90`
below describes `connection_resolves_import`'s `class default` branch
as "recognizing `SimpleConnection`/`ReexportConnection` as 'spoken
for'" (task 6.8's own fix for a propagation false-negative). That
branch is **removed** by `simpleconnection-graph-resolution`:
`SimpleConnection` now has its own `class is (SimpleConnection)` arm
calling the same shared `connection_pt_pair_resolves` function
`MatchConnection` uses (a real structural check — does the source
actually resolve — not a name-match heuristic); only
`ReexportConnection` (and other genuinely out-of-scope subtypes) still
falls to `class default`. The §3.2 table entry below is left as-is
(historical record of task 6.8's own state at the time), per this
file's own stated practice above of preserving prior narrative
verbatim rather than silently rewriting it.

**Verified via `git status`/`git diff` and a real NAG build+test run on
2026-10-16 (this update, revised after discovering and fixing a real
regression — see §3.4).** This section describes code that is
*modified but not yet committed*, implementing Task Group 6 of
`openspec/changes/hierarchy-wide-component-graph/tasks.md` ("Replace
3b2 with recursive import mirroring") — tasks 6.1-6.6 and 6.8 (new,
discovered mid-task) are now done; only 6.7 (explicit follow-up, out
of scope) remains.

**[CORRECTION, important]** An earlier revision of this file (and this
same session's own first-pass completion claim for task 6.5) asserted
`ctest -R MAPL.generic.scenarios`'s own 40-48 failing cases were
"pre-existing and unrelated to this change." **That was wrong.** It
was checked by reverting only the new *test file*, not the production
`GraphBuilder*.F90`/`initialize_advertise.F90` diff tasks 6.1/6.2 had
already landed — which proved nothing about whether that diff itself
was the regression's source. Rebuilding against true `HEAD`
(`f629872a`) in an isolated `git worktree` (plus only the two
unrelated, trivial NAG-portability fixes already listed in §3.2 below)
showed `MAPL.generic.scenarios` passing 100%, zero failures. The
failures were a real regression in task 6.1/6.2's own import
propagation, root-caused and fixed as task 6.8 (§3.2/§3.4 below). If
you are verifying a claim like this yourself: always diff against a
real, independently-buildable commit (a `git worktree` of `HEAD`, not
a stash-juggled working tree), never just "with my own latest addition
reverted."

### 3.1 What Group 6 replaces

Legacy's `StateRegistry%propagate_unsatisfied_imports()` bubbles an
unresolved import up one level, unconditionally, independent of any
connection naming the propagating component. The graph-native path had
no analog (`3b2`, named but never implemented — see roadmap §20.4.1).
An earlier design draft ("search the subtree") was tried and
**rejected during design review before code was written against it**
(two flaws: requires a connection to trigger; never materializes
anything at the intermediate level) — see design.md D3 in the active
change for the full postmortem. The corrected design mirrors one level
at a time, reusing existing single-level machinery unchanged.

Task 6.8 (new, discovered mid-task — see §3.4) adds the symmetric
EXPORT-side propagation legacy's own `propagate_exports_subregistry`
already does and graph-native resolution did not: a propagated
export's own name is qualified with its immediate child's own name
(`child_name/short_name`), and left UNCHANGED at every further
ancestor level — never re-qualified a second time.

### 3.2 What is implemented (modified files, not committed)

| File | Change |
|---|---|
| `GraphBuilder.F90` | New public interface block for `graphbuilder_unresolved_import_node_ids`, `graphbuilder_run_propagate_hook`, `graphbuilder_run_export_propagate_hook` (task 6.8), new `UnresolvedImportEntry` type; new `run_propagate_hook`/`run_export_propagate_hook` type-bound procedures; new `is_ordinary_resolvable_import` filter (task 6.8, excludes `MAPL_STATEITEM_WILDCARD`/`SERVICE`/`EXPRESSION` from import propagation); unrelated NAG-portability fix (long `_ASSERT` string split into a local variable at two call sites) |
| `GraphBuilder_ConnectionResolution_smod.F90` | New `graphbuilder_unresolved_import_node_ids` (structural scan, now gated by `is_ordinary_resolvable_import`), new `connection_resolves_import` helper (now with a `class default` branch recognizing `SimpleConnection`/`ReexportConnection` as "spoken for," task 6.8), new `graphbuilder_run_propagate_hook`/`graphbuilder_run_export_propagate_hook` (task 6.8: for each direct child, propagate its export-port entries into `this`'s own export-port table under a qualified name — `qualify_exported_name`/`already_exported` helpers), new `already_mirrored` idempotency guard, the `resolve_one` fix noted in §2.3, and (task 6.8) `find_export_node`/`find_export_node_id` fixed to match/resolve against each export port's own EFFECTIVE qualified name instead of the node's own real `short_name` directly (`find_export_node_id`'s own fix is the same class of bug task 6.3 already fixed on the import side: use the already-found node's own real `NodeId` directly, never re-derive via a resource-index lookup keyed by the wrong name) |
| `GraphBuilder_Geometry_smod.F90` | Unrelated NAG-portability fix (same long-`_ASSERT`-string pattern as above) |
| `GraphBuilder_LifecycleHooks_smod.F90` | Changed two `lgr%warning(...)` calls from pflogger's variadic `%a:%a` substitution to pre-concatenated plain strings — **found via a real nondeterministic gfortran hang/corruption during Group 6 testing**; pflogger's `%a` substitution is unreliable under gfortran for allocatable-character actuals (a gfortran/pflogger interaction, not specific to this change) |
| `OuterMetaComponent/initialize_advertise.F90` | Inserts `call gb%run_propagate_hook(this)` immediately after `run_activate_hook`, before the legacy `registry%propagate_unsatisfied_imports` call; (task 6.8) `call gb%run_export_propagate_hook(this)` next to legacy's own `registry%propagate_exports` call |
| `graph/ComponentGraph.F90` | `graph_add_import_port` relaxed to accept a value `NodeId` that is either owned by `this` or `allowed_foreign_node` (previously required `owns_node` only) — needed because a mirrored import's value is, by construction, a different component's own real `NodeId`; (task 6.8) `graph_add_export_port` relaxed identically, plus a new sparse `PortId -> qualified_name` side-map (`GraphPortRegistry%export_port_names`, `PortIdStringMap`) and `get_export_port_name()` accessor — a propagated export's own real `StateItemNode%short_name` is NEVER rewritten; only this side-table carries the ancestor-qualified name |
| `graph/containers/PortIdStringMap.F90` (new) | gFTL `PortId -> character(:)` map instantiation (`T __CHARACTER_DEFERRED`, following `StateItemSpecMap.F90`'s own precedent for a deferred-character gFTL parameter), backing the side-map above |
| `superstructure/generic/tests/CMakeLists.txt` | Registers new `Test_GraphBuilderPropagation.pf` |
| `superstructure/generic/tests/Test_GraphBuilderPropagation.pf` (new, untracked) | pFUnit suite covering tasks 6.1/6.3/6.4/6.8 (unit-level, direct `gb%advertise`/`gb%run_propagate_hook`/`gb%run_export_propagate_hook` calls, no real lifecycle) |
| `superstructure/generic/tests/Test_GraphModeDemonstration.pf` | New test `test_graph_mode_mirrors_grandchild_import_recursively` (task 6.5) — real `GENERIC_INIT_PHASE_SEQUENCE` lifecycle run against the resurrected fixture below |
| `superstructure/generic/tests/graph-scenarios/graphbuilder_propagation/` (new, untracked: `cap.yaml`, `comp_src.yaml`, `comp_mid.yaml`, `comp_dst.yaml`, `description.md`) | Resurrected cap→comp_mid→comp_dst fixture (task 6.5) — cap's own connection names `comp_mid`, not `comp_dst`, as destination; comp_mid declares no connection of its own |

### 3.3 Task-list status (`openspec/changes/hierarchy-wide-component-graph/tasks.md`)

Verified by reading the checklist directly:

- **Groups 1–5: `[x]` all checked** — shared storage foundation,
  name-uniqueness enforcement, proxy removal (ordinary connections,
  geometry, callback wiring). Corresponds to commits `9ce2ecd5` and
  `f629872a`.
- **Group 6 (import mirroring + task 6.8 export propagation): `[x]`
  all checked except 6.7.**
  - `[x]` 6.1, 6.2 — implemented, as in §3.2 above.
  - `[x]` 6.3 — confirmed by
    `Test_GraphBuilderPropagation.pf::test_mirrored_import_resolves_via_unmodified_resolution`
    (three-level GRANDPARENT→MID→comp_dst, MID declares no connection;
    GRANDPARENT's own connection names MID and resolves against the
    mirrored port with zero changes to
    `for_each_matching_import`/`resolve_one`).
  - `[x]` 6.4 — confirmed by
    `Test_GraphBuilderPropagation.pf::test_multiple_descendants_bubble_same_name`
    (two distinct children, same short name 'T', both independently
    resolved via `for_each_matching_import`'s existing multi-match
    iteration).
  - `[x]` 6.5 — resurrected as
    `superstructure/generic/tests/graph-scenarios/graphbuilder_propagation/`
    + `Test_GraphModeDemonstration.pf::test_graph_mode_mirrors_grandchild_import_recursively`,
    exercised through the real init-phase lifecycle (not the direct,
    synthetic `gb%` calls `Test_GraphBuilderPropagation.pf` uses). This
    task is also where the real regression (task 6.8) was found,
    while verifying it — see the correction note above.
  - `[x]` 6.6 — **confirmed as a no-op, not a removal**: there is no
    `## REMOVED Requirements` section in this change's own
    `graph-builder` delta spec (checked directly — only `MODIFIED`/
    `ADDED` exist), and the two-phase check/resolve split is not
    superseded — `graphbuilder_check_unsatisfied_imports` is a direct,
    load-bearing dependency of `graphbuilder_run_propagate_hook`
    (task 6.1) and of `graphbuilder_run_connect_hook`'s own post-freeze
    convergence check, in addition to `run_activate_hook`'s own
    pre-existing use. tasks.md's own note (quoted below) correctly
    anticipated this outcome; no code or spec text was removed. See
    tasks.md task 6.6 itself for the full three-point justification.
  - `[ ]` 6.7 — explicitly out-of-scope follow-up (bubbled-import
    insertion into the real `ESMF_State` under a child-qualified name)
    — tracked, not attempted.
  - `[x]` 6.8 — **new task, not in the original tasks.md** — export
    propagation (symmetric to 6.1's import propagation) plus three
    independently-discovered, required bug fixes. See tasks.md task
    6.8 itself for the full five-point breakdown (export propagation
    + qualification; `find_export_node`'s own read-path fix;
    `find_export_node_id`'s own 6.3-style re-derivation bug;
    `is_ordinary_resolvable_import` filter for WILDCARD/SERVICE/
    EXPRESSION; `SimpleConnection`/`ReexportConnection` recognition in
    `connection_resolves_import`) and §3.4 immediately below for the
    debugging narrative.
- **Groups 7–8: `[x]` complete** (commit `bc51fc9c`, 7.1/7.2 — note:
  this commit's own message says "Group 8" but its content is tasks
  7.1/7.2, i.e. Group 7; fixed post-hoc, see history — and Group 8's
  own 8.1 test addition, committed as `1ea0fdc1`). **[UPDATED, this
  session] Group 9: `[x]` complete** (commit `15a50ec5`,
  hierarchy-wide freeze). **Group 10: `[x]` complete** (this session —
  full regression pass under both NAG and gfortran, zero regression;
  spec-doc updates to `02`/`05`/`07`/`08` done, all four now carry
  `[RE-EVALUATED]` sections matching the landed design; new Phase 16
  entry added to `20-implementation-roadmap.md`; task 10.4's line-count
  check performed — found an *increase*, not the predicted decrease,
  recorded honestly rather than silently closed — see
  `tasks.md` task 10.4 and roadmap §20.4.16 for the full accounting).
  - Group 7 (`query_component_exports`, D4): confirmed landed,
    `Test_GraphBuilder.pf::test_query_component_exports_scoped_to_
    resolved_component` /
    `test_query_component_exports_deep_hierarchy_no_propagation`.
  - Group 8 (8.1, composite-member non-addressability): confirmed, with
    zero code change (D6's own prediction) — `advertise_one` only ever
    populates the resource index / import-port / export-port tables for
    a composite's own top-level identity; `materialize_composite`/
    `materialize_member` never call any of those three for a member.
    New test, `Test_GraphBuilder.pf::
    test_composite_member_not_independently_addressable` (this
    session, uncommitted): asserts the member's own `NodeId` is absent
    from both the resource index and the import/export-port tables of
    either component, and that an ordinary `MatchConnection` naming the
    member directly by its own short_name resolves nothing (zero
    `unresolved`/`unsupported` entries — `for_each_matching_import`
    never finds a port-table entry to match against). Passes under NAG
    and gfortran; full `ctest -L ESSENTIAL` (64 tests) 100% on both,
    verified this session.

### 3.4 Debugging narrative for task 6.8 (how the regression was found and root-caused)

Kept here, not just in tasks.md, because the METHOD matters as much as
the fix for anyone resuming this work:

1. A plausible-looking "pre-existing and unrelated" claim was made
   without actually comparing against a real, independently-buildable
   baseline — only against "my own latest addition reverted," which
   does not isolate anything about the rest of the diff.
2. Challenged, the correct check was to build true `HEAD` in an
   isolated `git worktree` (`git worktree add /tmp/... HEAD`) — this
   immediately revealed `HEAD` itself does not compile under NAG at
   all (`GraphBuilder.F90:1329`, an `_ASSERT` macro call split across
   two physical lines with no `&` continuation — introduced by an
   earlier, NAG-untested gfortran portability fix the user described
   separately). Adding back only that one two-line fix (already
   present in the uncommitted diff, unrelated to Group 6's own logic)
   made true `HEAD` buildable and `MAPL.generic.scenarios` passed
   100%.
3. Re-adding the uncommitted Group 6 files one at a time to that same
   clean worktree isolated the regression to
   `OuterMetaComponent/initialize_advertise.F90`'s own wiring (i.e.
   `run_propagate_hook` actually running in the real lifecycle, not
   merely existing as unreachable code) combined with
   `ComponentGraph.F90`'s own `add_import_port` relaxation.
4. Tracing the actual failing assertions (not just the logged
   `report_if_failed` warning text, which is swallowed and does NOT
   itself fail the test — the REAL failure is `MAPL_throw_exception`
   firing deep inside `_ASSERT`/`_FAIL`, registered with pFUnit's own
   exception mechanism regardless of whether the caller checks `rc`)
   led through `A/E_A1`-shaped import names to the ESMF sub-state-
   nesting semantics correction, then to legacy's own
   `propagate_exports_virtual_pt` qualification rule, then to the
   `WILDCARD`-item-type gap (`history_wildcard`), then to the
   `SimpleConnection` gap (`statistics_real`) — three independent,
   compounding causes, found one at a time by fixing the first,
   re-running, and reading the NEXT distinct failure message rather
   than assuming one fix would resolve everything.

**[ASSUMPTION]** The untracked `Test_GraphBuilderPropagation.pf` file
and the `CMakeLists.txt` registration were almost certainly authored
together as part of the same in-progress Group 6 work; not independently
re-verified beyond reading their content.


---

## Verification logs (originally ARCHITECTURE.md section 4.1, 4.2, 4.5)

### 4.1 NAG (verified clean run exists)

```bash
module load nag-stack/7.2.41 && cmake --build nag -j 8 --target build-tests
module load nag-stack/7.2.41 && ctest --test-dir nag --output-on-failure
```

(`module load nag mpi baselibs` from an earlier revision of this file
does not resolve on this host's Lmod setup — `nag-stack/7.2.41` is the
correct meta-module matching this checkout's actual configured
toolchain, clang-17.0/nag-7.2.41/openmpi-5.0, confirmed by reading
`nag/CMakeCache.txt`'s own `BASEDIR` value.)

- `nag/ctest-task6-final.log` (full top-level ctest, this working
  tree's Group-6 state, predating 6.3-6.6): **91% passed, 7 failed out
  of 78** — the **same 7 failures, same count**, as
  `nag/ctest-baseline.log` (a run predating this branch's Group 6
  changes). The 7 failures are unrelated to the graph work:
  `ll-ll`/`cs-cs`/`cs-ll`/`ll-cs` (app regression tests) and
  `MAPL3G_Comp_Test_case02`/`case11`/`case23` (an EXTDATA dry-run check
  failing on `--narrow requires the netCDF4 Python package, which
  could not be imported` — an environment/Python dependency issue, not
  a graph defect).
- `MAPL.generic.components` (the ctest target containing
  `Test_GraphBuilderPropagation`, tasks 6.1-6.4/6.8) — **Passed**
  (verified this session, `ctest --test-dir nag -R MAPL.generic.components`),
  all 11 propagation tests (7 import-side + 4 new export-side, task
  6.8) individually confirmed passing via `-v`.
- `MAPL.generic.scenarios` — **Passed 100%, 0 failures** (verified
  this session, 3 consecutive runs via
  `ctest --test-dir nag -R MAPL.generic.scenarios`), including
  `history_1`/`history_wildcard`/`statistics_real`, which an earlier
  revision of this file wrongly reported as "pre-existing and
  unrelated" — see §3's own correction note for the full story. That
  claim was never actually checked against a real `HEAD` build; it was
  checked by reverting only this session's own new test file, which
  does not isolate the production diff at all. Root-caused and fixed
  as task 6.8 (export propagation + three independently-discovered
  bugs).
- Full top-level `ctest --test-dir nag` (75 tests, confirmed 2x this
  session): **71/75 pass (95%)**. The 4 failures —
  `ll-ll`/`cs-cs`/`cs-ll`/`ll-cs` — are `apps/tests/Regrid_Util/` app-
  regression tests, confirmed pre-existing by building true `HEAD` in
  an isolated `git worktree` (plus only the two trivial, unrelated NAG-
  portability `_ASSERT`-split fixes already in §3.2) and reproducing
  the same 4 failures there, with none of Group 6's own code present
  at all.
- `nag/build-tests-task6.log` confirms
  `Test_GraphBuilderPropagation.F90.o` compiled and linked into
  `MAPL.generic.components` with no errors (only pre-existing unused-
  variable warnings in unrelated files).

**Conclusion: on NAG, Group 6's current code (tasks 6.1–6.6, 6.8)
builds and passes, with ZERO regression anywhere in the suite relative
to true `HEAD` (confirmed via an isolated `git worktree` comparison,
not just "my own addition reverted"). The only 4 remaining ctest
failures are pre-existing, unrelated app-regression tests, confirmed
by reproducing them on `HEAD` alone.**

### 4.2 gfortran (verified clean, this session)

```bash
module load gfortran-stack && cmake --build gfortran -j 8 --target build-tests
module load gfortran-stack && ctest --test-dir gfortran --output-on-failure
```

**Build initially failed** on this session's own task 6.8 code:
`is_ordinary_resolvable_import` (new helper, `GraphBuilder.F90`) was
declared without `public`. NAG tolerates this (the only caller,
`GraphBuilder_ConnectionResolution_smod.F90`, is a submodule of the
same parent module — NAG resolves it via host association regardless
of visibility); gfortran's linker requires the symbol to be exported
from the compiled `.o` for a separate-file submodule to link against
it: `Undefined symbols ... is_ordinary_resolvable_import`. Fixed by
adding `public :: is_ordinary_resolvable_import` alongside this
module's existing `resolve_local_node_id`/`is_self`/`itemtype_name`
public declarations — same pattern every other submodule-shared
helper in this file already follows; this one new helper had simply
been missed. **Lesson for future work in this file:** any new plain
(non-`module`-interface) procedure added to `GraphBuilder.F90` that a
submodule calls needs an explicit `public ::` line — NAG will not
catch a missing one; only gfortran's separate-TU linking will.

**After that one-line fix:** `build-tests` succeeds cleanly, zero
errors. `ctest --test-dir gfortran -R "MAPL.generic.components|MAPL.generic.scenarios"`
— both pass. Full `ctest --test-dir gfortran` (75 tests): **71/75
pass (95%)** — the identical 4 failures as NAG
(`ll-ll`/`cs-cs`/`cs-ll`/`ll-cs`, `apps/tests/Regrid_Util/`, confirmed
pre-existing and unrelated to graph work in §3's own correction note),
no gfortran-specific regression beyond the one `public` fix above.
**Group 6 (tasks 6.1-6.6, 6.8) is confirmed portable across both
compilers**, superseding the earlier "INCONCLUSIVE" status this
section previously carried.

### 4.5 Task Group 10 full regression pass (this session, 2026-10-17)

Working tree was clean (Groups 1-9 fully committed through `15a50ec5`)
at the start of this pass — no production code was changed; this is a
pure verification run against already-landed code, plus the doc-only
changes recorded in §2.2/§6.

```bash
module load nag-stack/7.2.41
cmake --build nag --target build-tests -j 8
ctest --test-dir nag -L ESSENTIAL --output-on-failure -j 4   # 64/64 pass
ctest --test-dir nag --output-on-failure -j 4                # 71/75 pass

module load gfortran-stack
cmake --build gfortran --target build-tests -j 8
ctest --test-dir gfortran -L ESSENTIAL --output-on-failure -j 4   # 64/64 pass
ctest --test-dir gfortran --output-on-failure -j 4                # 71/75 pass
```

**Result, both compilers, identical:** `ESSENTIAL` label 100%
(64/64); full suite 71/75, the only 4 failures being the long-standing
`ll-ll`/`cs-cs`/`cs-ll`/`ll-cs` `apps/tests/Regrid_Util/`
regression-data tests (`CMake Error ... regression data directory not
found ... Is LOCAL_REGRESSION_DATA_DIR set correctly?`) — a missing
local-data-directory environment issue, not a graph defect, matching
every prior session's documentation of exactly these same 4 tests
(§4.1 above). No build warnings or errors beyond the pre-existing
`ld: warning: ignoring duplicate libraries`/macOS-SDK-version linker
noise already present before this session. **Zero regression found in
`superstructure/generic/tests/scenarios/` or `graph-scenarios/` under
any of the three `graph_mode()` values** (`MAPL.generic.scenarios`,
`MAPL.generic.components`, and `MAPL.generic.graph` all passed on both
compilers — these three targets together exercise `GRAPH_MODE_REGISTRY`,
`GRAPH_MODE_GRAPH`, and `GRAPH_MODE_BOTH`, per `Test_GraphMode.pf`/
`Test_GraphModeDemonstration.pf`/`Test_GraphBuilderMaterializeGate.pf`/
`Test_GraphBuilderEquivalence.pf`/`Test_GraphEquivalenceHarness.pf`).

---

## 7. Phase 12 narrative — six `StateItemCharacteristic` axes wired into real dispatch

**Verified this session (2026-10-19) by implementing the change
directly**, not merely read from a prior pass — kept as a narrative
section (like §3's Task-Group-6 account) because the two surprises
below cost real build/test cycles and are exactly the kind of thing a
future pass through this axis family should know up front.

**Scope.** `openspec/changes/phase-12-characteristic-dispatch-wiring`
wires six of 5a2's seven standalone `StateItemCharacteristic`
subclasses — `AttributesCharacteristic`, `UngriddedDimsCharacteristic`,
`StandardNameCharacteristic`, `QuantityTypeCharacteristic`,
`ConservationCharacteristic`, `NormalizationCharacteristic` — into
`GraphBuilder_Advertising_smod.F90`'s `populate_characteristics`
insertion point, mirroring the existing `PhysicalUnitsCharacteristic`/
`TypeKindCharacteristic`/`VerticalCoordinateCharacteristic` precedent
(`VerticalCoordinateCharacteristic` was already wired before this
phase, hence six axes here, not seven as the roadmap's original
`5a2`-subclass count implied).

**What changed (uncommitted at the time of this writing — see below):**
- `superstructure/generic/specs/VariableSpec.F90`: three new fields —
  `quantity_type`, `conservation`, `normalization` — plain field
  assignment only (no `make_VariableSpec` keyword, no YAML/parser
  wiring; `quantity_type` in particular is YAML-unreachable today,
  same as `ConservationCharacteristic`/`NormalizationCharacteristic`'s
  own metadata types).
- `GraphBuilder_Advertising_smod.F90`'s `populate_characteristics`:
  six insertion branches, each gated per design.md D1 to match its own
  field's "unset" representation — `allocated()` gate for
  `standard_name` (same shape as `units`'s existing gate);
  `%size() > 0` gate for `attributes` (skips the empty/common case as
  an optimization, not a correctness requirement); unconditional
  insert for the other four (same shape as `typekind`'s existing
  unconditional insert — each field's own default is itself a
  meaningful, comparable value).
- `GraphBuilder.F90`'s host `use mapl_StateItemCharacteristicKind_mod`
  line extended with the six new kind constants (no separate
  submodule `use` needed — submodules reach them via host
  association).
- `GraphBuilder_ConnectionResolution_smod.F90`'s
  `merged_item_for_link`: one combined, comment-only `else if` branch
  added for all six kinds, documenting that none can actually reach
  this point in practice (see surprise #1 below) — enumerated
  explicitly so a genuine mismatch fails with ITS OWN message, not
  this helper's unrelated "unrecognized StateItemCharacteristicKind"
  fallback.

**Surprise #1 — none of the six axes has a real `build_transform`,
confirmed empirically rather than planned around up front.** This
document's own §1.1 (now corrected above) already stated this as a
known fact before this phase started; it was re-discovered the hard
way instead of being read first (see the self-assessment this session
also produced, referenced below). Every mismatch on these six axes
surfaces as `'<Name>Characteristic: build_transform - no real
provider exists for this axis'`, via the generic `_FAIL` default
every `*Characteristic%build_transform` stub shares — not a silent
pass, not a converted value. Both the synthetic unit test
(`test_standard_name_mismatch_fails_explicitly`,
`Test_GraphBuilder_ConnectionResolution.pf`) and the real-pipeline
scenario test
(`test_graph_mode_leaves_newly_wired_axis_mismatch_unwired`,
`Test_GraphModeDemonstration.pf`) need
`@assertExceptionRaised('StandardNameCharacteristic: build_transform
- no real provider exists for this axis')` to consume that exception
before their own assertions run. **Lesson for future work on any
newly-wired-but-not-yet-converted axis:** write the
`@assertExceptionRaised` expectation first, before running the test
once to "discover" it — reading §1.1/§7 here would have made this
the first draft, not the second.

**Surprise #2 — legacy's `UngriddedDimsAspect%can_connect_to` rejects
any ungridded-dims disagreement unconditionally, independent of
`ValidationMode`, unlike `StandardNameAspect%matches()` which is only
strict under strict `ValidationMode`.** This asymmetry killed the
fixture's first draft (`standard_name_and_quantity_type_mismatch`,
renamed to `standard_name_and_ungridded_dims_mismatch` after
`quantity_type` turned out to be YAML-unreachable) when it also tried
to mismatch `ungridded_dims`: legacy's own `process_connections()`
hard-failed before `GraphBuilder`'s own resolution ever ran, regardless
of mode. The final fixture
(`superstructure/generic/tests/graph-scenarios/
standard_name_and_ungridded_dims_mismatch/`) mismatches only
`standard_name` (`air_temperature` vs. `surface_temperature`) and
deliberately MATCHES `ungridded_dims` (`foo1`/extent 3 both sides),
and its scenario test overrides `FieldDictionaryConfig` to permissive
for its own duration (save/restore, mirroring `Test_Aspects.pf`'s own
convention) — `MAPL.generic.scenarios` otherwise forces strict mode
process-wide via `Initialize_strict`, and legacy's
`StandardNameAspect%matches()` runs unconditionally alongside
`GraphBuilder`'s own resolution even under `GRAPH_MODE_GRAPH`.
**Lesson:** when picking which axis pairs to co-mismatch in a new
fixture, check each legacy `*Aspect%can_connect_to`/`matches()` for a
`ValidationMode` gate (or its absence) before writing the YAML, not
after a hard failure in `process_connections()` explains it.

**Verification (NAG only this session; gfortran not re-run for this
phase):** all six Task Groups' own tests pass
(`Test_GraphBuilder_Advertising.pf` new cases,
`Test_GraphBuilder_ConnectionResolution.pf`'s
`test_standard_name_mismatch_fails_explicitly`,
`Test_GraphModeDemonstration.pf`'s
`test_graph_mode_leaves_newly_wired_axis_mismatch_unwired`); full
`ctest -R "^MAPL.generic"` 8/8 suites passing; `openspec validate
phase-12-characteristic-dispatch-wiring --strict` valid.

**State as of this writing:** all source changes above are
**uncommitted** in the working tree; the OpenSpec change itself is
mid-archive (artifact/task completeness confirmed, delta-spec sync
decision pending). A separate source commit is still needed once
archiving completes. A companion self-assessment this session
concluded `ARCHITECTURE.md` (this file) was NOT actually consulted
during implementation despite §1.1 already containing the exact fact
that would have preempted surprise #1 — recorded here as the fix for
that gap, and as a concrete argument for reading this file before,
not after, writing characteristic-mismatch tests on any future axis.


