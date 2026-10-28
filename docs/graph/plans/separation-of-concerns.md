# Plan: Graph layer separation-of-concerns cleanup

> Canonical copy: `docs/graph/plans/separation-of-concerns.md` (in repo).
> Every task MUST update its own **Status** line (done/partial, and any
> deviations from this plan) as part of the same change as the work. Artifacts produced by earlier tasks
> (`docs/graph/CONVENTIONS.md`, `docs/graph/scenario-parity.md`, the B0
> openspec design, `docs/graph/adding-a-characteristic.md`, ...)
> **take precedence over this plan's summaries** once they exist. If the
> plan and the source disagree, stop and report rather than guess.

Status: agreed in discussion with tclune, 2026-10-21.

**Workflow: no PRs.** Work lands directly on the owner's integration
branch (currently `feature/#5286-graph-phase-5`); one task = one commit,
or a short series of commits. Agents **do not commit**: they leave the
task as uncommitted changes plus a proposed message in
`.git/PLAN_TASK_MSG`, and the owner reviews the working tree before
committing (see "Review and commit" below). The integration branch goes to `develop` as
one final PR, followed by a walkthrough with the senior developers.
Every task commit ends with a git trailer naming its task:

```
Plan-Task: B3-Units
```

Labels are the task ids below (`A0`, `A1`, `A1-diagnosis`, `A2a`, `B3-Units`, `B3-TypeKind`,
`C2-<short-name>`, ...). Find a task's work with
`git log --grep='^Plan-Task: B3-Units' -p`. Do not record commit hashes
in this plan (they change if the branch is rebased); the trailer is the
reference. Never push; never rebase or amend existing commits.
**Every task must leave all ctests passing** (NAG first, then gfortran;
`ctest -L ESSENTIAL`). Pending/expected-fail work is gated by explicit
toggles, never by red tests.

## Working with the owner (for planning/meta sessions)

Preferences stated in discussion, not obvious from the rest of the plan:
- Ask about legacy MAPL, gFTL, pFUnit and ESMF design intent rather than
  guessing; the owner knows the history.
- No make-work: no benchmarks, scaffolding or process steps without a
  concrete payoff. Prefer trying things on real tasks.
- No permanent tests for things code must *not* contain (grep guards,
  lint-style ctests) unless the risk is subtle or dangerous. Check such
  rules once in a task's Done condition; document the rule once the
  source satisfies it; review enforces it afterwards.
- One task at a time: no parallel task agents or worktrees, and no plan
  edits while a task has uncommitted work that touches the plan (wait
  for its commit, then edit).
- Fix-as-needed beats speculative completeness (e.g. `MAPL_StateSet` is
  extended when a task needs it); no speculative compiler workarounds.
- Prefer overloading existing MAPL generic names (`MAPL_FieldGet`,
  `MAPL_StateGet`, ...) to inventing new procedure names, when the
  compilers accept the overload.
- Meta sessions do not author tasks. Edit only this file
  (`~/.opencode/plan/graph-separation-of-concerns.md` is a symlink to
  it, not a copy) and commit only the files changed, with trailer `Plan-Task: plan-maintenance`. Never
  commit or delete the owner's untracked files
  (`.opencode/skills/mapl-implementation/`, `ordering.md`).
- After a task lands: check its commit (trailer, both ctest lines,
  Status line, scope), delete a stale `.git/PLAN_TASK_MSG`, then fold
  its findings into later tasks.
- Tooling lives outside the plan: `.opencode/README.md` (agents,
  commands, `with-*` wrappers, `model-for` tiers).

## Context every task prompt should include

- Read root `AGENTS.md`, `docs/graph/ARCHITECTURE.md`,
  `docs/generic/ARCHITECTURE.md` (after A0 lands: also
  `docs/graph/CONVENTIONS.md`).
- Graph layer: `superstructure/generic/graph/` + `superstructure/generic/GraphBuilder*.F90`.
  Legacy layer: `superstructure/generic/registry/`, `specs/*Aspect.F90`,
  `transforms/`.
- **End state:** Aspects are replaced by Characteristics; legacy
  `ExtensionTransform` subclasses are replaced by graph `Transform`
  subclasses; `registry/` is deleted. Until then **duplicate, do not
  delegate**: graph code must not call into `specs/*Aspect`, `registry/`,
  or legacy `transforms/`. Near-duplicates are acceptable.
- Geometry is intentionally a graph node (import/export of geometry as
  non-completed `ESMF_Field`s; motivated by SURF deriving its geom from
  ATM/Land/Glacier/Ocean). Keep that.
- gFTL containers: prefer pointer access over copying populated
  containers (deep defined-assignment can break compilers). Problems
  should be exceptional - do not add workarounds speculatively.
- Build: `with-nag cmake --build nag -j 10 --target build-tests`;
  `with-nag ctest --test-dir nag -L ESSENTIAL`. Then same with `with-gfortran` / `gfortran/`.
  (`module load` does not persist between agent shell calls; always use the wrappers.)
- Any plain helper in `GraphBuilder.F90` called from a submodule needs
  `public ::` (NAG won't catch it; gfortran link will).

## Target design (decided)

### D1. Handle vs. descriptor split
- `GraphStateItem` = **ESMF handle only**: one of Field/FieldBundle/State
  (+ member maps, route-handle-wrapper role). Its original purpose:
  a common type over ESMF objects lacking a common base class.
- New `StateItemDescriptor` = **single source of truth** for description:
  a `StateItemCharacteristicMap` and nothing else. All current
  `declared_*` / `ungridded_dims` / `vertical_stagger` components on
  `GraphStateItem` move into the map as characteristics (new ones as
  needed: restart mode, expression, vertical stagger, ...).
- `StateItemNode` owns both.

### D2. Generic status (base class, never subclass logic)
Statuses: `SPECIFIED`, `MIRROR`, `UNCHECKED`, `DEFERRED` (transient).
`FROM_COMP` collapses into `SPECIFIED`. `INVALID` = programming error.

| export \ import | SPECIFIED | MIRROR | UNCHECKED |
|---|---|---|---|
| SPECIFIED | subclass `needs_extension_for`/`build_transform` | copy export value -> import, becomes SPECIFIED | match |
| MIRROR | **error** except allowlisted shapes (EXPRESSION) | **error** | match |
| UNCHECKED | match | copy export -> import, becomes UNCHECKED | match |

- UNCHECKED = "always match, user beware" (bootstrapping MAPL2-era items
  that never quantified the axis; expression outputs).
- DEFERRED = owner supplies later (e.g. component-default geom at
  GEOM_B); must be SPECIFIED or UNCHECKED before freeze, else freeze fails.
- MIRROR resolution is generic: `allocate(c, source=partner_c)` + set
  SPECIFIED. No per-subclass MIRROR sentinels anywhere in the map.
- Subclass methods (`needs_extension_for`, `build_transform`,
  the D3 write method, ...) are only ever called with both sides SPECIFIED; the
  base/descriptor enforces this.
- "Absent from map" means only "not applicable to this item's shape".

### D3. ESMF Info facade
- Every characteristic implements a deferred write method (reference
  characteristics write a node/registry id).
- **Write through the existing MAPL setters, not raw `ESMF_Info`:**
  `MAPL_FieldSet` / `MAPL_FieldGet`, `MAPL_FieldBundleSet` /
  `MAPL_FieldBundleGet`, `MAPL_StateSet` / `MAPL_StateGet`. They already
  own the Info keys and layout (`mapl_FieldInfo_mod`,
  `mapl_FieldBundleInfo_mod`), so keys match legacy automatically.
  Bypass them only for a stated, very good reason (record it in the B0
  design), e.g. data no setter takes yet; then prefer adding the
  argument to the setter over raw `ESMF_InfoSet`. Consequences for B0:
  - The write method takes the item's ESMF handle (Field, FieldBundle,
    or State item), not a bare `ESMF_Info`; B0 decides how a
    characteristic dispatches over handle kinds (e.g. each provides the
    keyword value and the descriptor makes one `MAPL_*Set` call per
    handle kind).
  - `MAPL_FieldSet` on a COMPLETE field is not Info-only: `geom`,
    `typekind`, `units`, `num_levels` go through `FieldDelta` and can
    modify the field. `FieldDelta` is slated to go away (owner): graph
    code must not depend on that path. Changes to a field's structure
    are graph `Transform`s/materialization, not Info writes. B0 decides
    how `sync_to_info` avoids the `FieldDelta` path (e.g. only Info-only
    keywords after materialization).
  - Existing direct Info access in the graph layer is removed by A5.
  - State items carry essentially no characteristics of their own (no
    geometry, typekind, units, ...): those belong to the members of the
    composite. A State's own Info holds only framework data such as its
    variant tag (ItemShape) and, later, the callback map. B0 should not
    give State-shaped items characteristic slots they cannot have.
  - `MAPL_StateSet` is incomplete and probably broken (`intent(out)`
    dummies, three keywords). Rare today; CallbackState is the main
    motivating State use. Fix and extend it as tasks need it, rather
    than bypassing it.
- `StateItemDescriptor%sync_to_info(handle, rc)`: one loop, called at
  materialization and whenever a DEFERRED value becomes SPECIFIED.
- Graph code may read Info back, but only through `MAPL_*Get` (never raw
  `ESMF_Info*`; CONVENTIONS §5a). Harvest of a DEFERRED value does this.
- Info keys must match legacy (`mapl_FieldInfo_mod`) so History/user
  code is unaffected.
- UNCHECKED: open question (no entry vs. explicit marker) - decide in B0
  by surveying legacy Info consumers.
- Fallback if this proves significantly complex: Info only for
  user-visible characteristics (owner agreed this is negotiable).

### D4. Extension ordering is owned by the export's shape
- Mirrors legacy `ClassAspect%get_aspect_order(goal_aspects)`
  (see `specs/FieldClassAspect.F90`, `specs/BracketClassAspect.F90`,
  called from `registry/ExtensionFamily.F90`).
- `ItemShapeCharacteristic` (or a shape strategy it holds) gets
  `get_extension_order(goal_descriptor, rc)` returning an explicit,
  complete list of kinds. Example: Bracket regrids before time-interp
  unless the goal geom is time-dependent.
- Mismatched kinds not listed by the shape are appended in fixed
  registered-name order (deterministic). A golden test records the
  order for each shape.
- Delete `GraphStateItem%ordering` / `characteristic_ordering_table`
  (variant-keyed, no legacy basis).

### D5. Translation from VariableSpec
- `VariableSpec` remains the Fortran representation of user-level item
  specs (YAML is mostly for synthetic tests; most user specs come from
  Fortran calls).
- New `VariableSpec%make_characteristics(descriptor, rc)`, implemented as
  a loop over a per-characteristic factory registry, living beside
  `make_aspects`. Each factory owns translating its VariableSpec
  field(s) and sentinels (`MAPL_TYPEKIND_MIRROR`, unallocated, ...) into
  value + generic status.

### D6. GraphBuilder is characteristic-agnostic
GraphBuilder and its submodules must not name any `*_CHARACTERISTIC_KIND`
constant or concrete `*Characteristic` type. Rationale: this is the
separation of concerns the plan exists for. If GraphBuilder names
kinds, every new characteristic means editing GraphBuilder's dispatch
chains (`merged_item_for_link`, `get_item_units`, ...), and behavior for
one characteristic is split between its class and GraphBuilder. With
the rule, adding a characteristic touches only its class, its factory,
and `ItemShapeCharacteristic`'s ordering. Checked by B4's Done condition
(a one-off grep) and by review, not a permanent test (owner decision).

## Tasks

### Phase A - safety net and hygiene

**A0. Docs/source reconciliation + conventions.**
- Diff `AGENTS.md`, `docs/graph/ARCHITECTURE.md`,
  `docs/generic/ARCHITECTURE.md`, `superstructure/generic/AGENTS.md`
  against current source/git log. Known drift: claims an active
  `hierarchy-wide-component-graph` change (`openspec/changes/` now only
  has `archive/`); transform-coverage table predates phases 7a/7b/7c;
  line counts.
- Move session narrative (§3, §3.4, §7 and similar) to
  `docs/graph/history/`; ARCHITECTURE.md describes current state only.
- Create `docs/graph/CONVENTIONS.md`: comment policy (no
  `openspec/changes/...` history in source; comments explain why, not
  when; stable `openspec/specs` requirement IDs allowed; workaround
  comments one line), gFTL pointer-not-copy, `public ::` submodule rule,
  D6 rule, duplicate-don't-delegate rule.
- Create `docs/graph/GLOSSARY.md`: legacy -> graph mapping
  (Aspect->Characteristic, AspectStatus->CharacteristicStatus,
  ExtensionTransform->Transform, StateRegistry->ComponentGraph/HierarchyGraph,
  VirtualConnectionPt->NodeId/PortId, ExtensionFamily->ExtensionResolution,
  ClassAspect->ItemShapeCharacteristic, ...).
- Docs only. No code.
- **Status:** done (commit trailer `Plan-Task: A0`). Deviations:
  `docs/graph/ARCHITECTURE.md` rewritten to current state only (old
  §3/§4.x/§7 narrative moved verbatim to
  `docs/graph/history/2026-10-session-narratives.md`; its section
  numbers changed, cross-references in `docs/generic/` updated).
  `docs/generic/ARCHITECTURE.md`/`README.md` got targeted fixes (7b/7c
  landed, counts, dead cross-references), not a rewrite. Found and not
  fixed (source change, out of scope): stray copy not in the build
  `superstructure/generic/to_itemtype.F90` differs from the compiled
  `ComponentSpecParser/to_itemtype.F90` (the former parses
  `'vector_bracket'`, the latter does not). Deletion is listed under
  D3-cleanup.

**A1. Legacy scenarios under graph mode, gated.**
- Parameterize `tests/Test_Scenarios.pf` by graph mode
  (registry/graph/both). Add a `graph_ready` flag per scenario; only
  ready scenarios get graph/both parameters. ESSENTIAL stays green.
- Optional non-ESSENTIAL ctest target running pending scenarios.
- Deliverable: `docs/graph/scenario-parity.md` table: scenario,
  ready?, one-line root cause of failure. Expected causes include
  missing geometry transform (`regrid*`, `propagate_geom`). Flipping a
  flag is how later tasks show progress.
- **Diagnosis** (separate task, label `A1-diagnosis`, opus): confirm or
  correct each first-pass root cause in `scenario-parity.md`, so C2.x
  tasks start from verified causes. Doc changes only; any source edits
  for experiments are reverted.
- **Status (harness, `Plan-Task: A1`):** done. Deviations:
  `graph_ready` lives in a `scenario_table()` in `Test_Scenarios.pf`;
  pending scenarios run via ctest `MAPL.generic.scenarios.pending`
  (label `PENDING`, not ESSENTIAL, env `MAPL_SCENARIO_PENDING`).
  Measured: 23 of 31 scenarios pass under graph/both, 8 fail (see
  `docs/graph/scenario-parity.md`). Contrary to this plan, `regrid*`,
  `propagate_geom` and `vertical_*` pass in graph mode; judged masked
  (legacy `add_to_states` still runs in `initialize_realize_accepted`;
  no item-level geometry characteristic), so they are held at
  `graph_ready = .false.` with the 8 real failures (owner decision).
  Root causes in the table are first-pass; A1 diagnosis confirms them.
  Follow-up: parity doc now records which ready scenarios depend on
  legacy `add_to_states` (experiment with the calls skipped in graph mode;
  not committed).
- **Status (diagnosis, `Plan-Task: A1-diagnosis`):** done. Every cause
  confirmed by a reverted source experiment (NAG; failure signatures
  identical on gfortran); see "Verified root causes" in
  `docs/graph/scenario-parity.md`. Corrections to first-pass: the
  "no import units" failure is a symptom of export aliasing looking in
  the wrong component for propagated exports (R1); SERVICE items are not
  excluded from graph resolution, aliasing assumes a Field (R2). The
  masked passes are confirmed: the graph never sees a geometry mismatch
  (M1, no item-level characteristic) or a vertical-grid mismatch (M2,
  component `vertical_grid` is not copied to items, so C1 does not
  cover `vertical_*`). Proposed C2 split recorded in the parity doc;
  `C2-runphase-args` (P1): method-node arguments must be keyed by
  (intent, short name), not short name alone.

**A2a..A2n. Graph-native scenario ports (~5 scenarios/task).**
- Port legacy scenarios to `tests/graph-scenarios/` with improved
  import/export/standard names and full `expectations.yaml`. First task (A2a):
  the existing 7 graph-scenarios (add `expectations.yaml`) + write
  `docs/graph/scenario-naming.md`. Later tasks follow it.
- Each scenario gets `description.md` (what it exercises).
- **Status:** A2a done (commit trailer `Plan-Task: A2a`); A2b.. not
  started. Deviations: the 7 existing fixtures were not renamed (tests
  refer to their directory and item names); they gained
  `expectations.yaml`, and `plain_field`, `vertical_field` and
  `ungridded_dims` gained `description.md`. Naming rules are in
  `docs/graph/scenario-naming.md`. Nothing reads the new
  `expectations.yaml` files until A3; they were checked by temporarily
  adding the fixtures to `Test_Scenarios.pf`'s `scenario_table()`
  (reverted): six of seven pass in registry/graph/both modes, the
  seventh (`standard_name_and_ungridded_dims_mismatch`) passes its state
  checks but needs a permissive field-dictionary override and leaves the
  expected `StandardNameCharacteristic` exception unconsumed. A3 must
  handle both.

**A3. Shared expectations driver for graph scenarios.**
- Graph-scenario tests (`Test_GraphModeDemonstration.pf`,
  `Test_GraphEquivalenceHarness.pf`) consume `expectations.yaml` via the
  same checkers as `Test_Scenarios.pf`; add graph-specific checks
  (node count, transform chain kinds/order). Remove hand-written
  assertions now covered.
- **Status:** done (commit trailer `Plan-Task: A3`). Deviations: no new
  driver module; `Test_Scenarios.pf` itself runs the 7 graph-scenarios
  (added to `scenario_table()`) and gained a `units` checker and a
  `check_graph` step driven by a `graph:` key on the `<root>` entry of
  `expectations.yaml` (format in `docs/graph/scenario-naming.md` §8).
  `ScenarioEntry` gained `permissive` and `expected_exception`, which
  handle the `standard_name_and_ungridded_dims_mismatch` issues from A2a.
  `Test_GraphModeDemonstration.pf` lost
  `test_graph_mode_leaves_newly_wired_axis_mismatch_unwired` and the
  structure/typekind/units/value assertions in the two units tests and
  the grandchild-mirroring test (edge check only). Still hand-written:
  item_count, the run phase itself, import-port mirroring, restart
  metadata. `Test_GraphEquivalenceHarness.pf` is unchanged: its
  legacy-vs-graph parity checks have no `expectations.yaml` equivalent.
  `graph:` also takes an optional `nodes:` count (nodes owned by the
  root's graph). Chain step kinds are inferred from units/typekind either side of each
  transform; only `units` and `typekind` are recognized (others report
  `unknown`).

**A4. Comment hygiene (before Phase B).** One task per file group, each
in a fresh session (one session for all groups would compact and lose
detail). Groups, in order (paths under `superstructure/generic/`;
reference counts as of A3):
- `A4a`: `GraphBuilder_ConnectionResolution_smod.F90` (2532 lines, 66 refs)
- `A4b`: the other `GraphBuilder*.F90` (11 files, 87 refs)
- `A4c`: `graph/*.F90` (34 files, 44 refs)
- `A4d`: `graph/characteristics/`, `graph/transforms/`,
  `graph/containers/`, `graph/id_types/`, `graph/callback/` (48 refs)
- `A4e`: everything else with references: remaining top-level `*.F90`,
  `OuterMetaComponent/`, `specs/` (about 41 refs)
 Rationale: ~33% of graph-layer lines
are comments (40-52% in `GraphBuilder*`), 277 dead `openspec/changes/...`
paths (now under `archive/`), many describing removed code. Every
Phase B/D task reads these files; stale narrative misleads agents and
consumes context.
- Delete change history, task/decision citations, and descriptions of
  removed or prior code. Git log + `openspec/changes/archive/` keep it.
- Keep non-obvious "why" constraints (ordering, must-not-mutate,
  compiler workarounds), cut to 1-2 lines.
- A spec link is allowed only as a stable requirement ID that exists in
  `openspec/specs/` (e.g. `REQ-INOUT-001`), never a change path.
- Comment-only: no code, no whitespace reflow of code lines. Verify
  with `.opencode/scripts/check-comment-only.py <paths...>` (strips `!`
  comments and blank lines from HEAD and working tree, diffs them, and
  counts remaining `openspec/changes` references). It must exit 0 and
  report 0 references for the group; put its output in the commit
  message body.
- Must land before B1 and D1 (both rewrite these files).
- **Status:** A4a done (`GraphBuilder_ConnectionResolution_smod.F90`: 2532 -> 1880
  lines, 0 `openspec/changes` references, comment-only verified; no
  deviations). A4b done (the other 11 `GraphBuilder*.F90` files: 0
  `openspec/changes` references, comment-only verified; the stale
  `GraphBuilder.F90` module header was rewritten from the current call
  sites, since it described removed proxy nodes and a nonexistent
  `get_or_make_local_node_id`). A4c done (`graph/*.F90`, 33 files: 0
  `openspec/changes` references, comment-only verified; `spec/NN-*.md` paths
  dropped, REQ IDs kept; stale ComponentGraph/MethodInvocationAdapter/
  GraphExport headers rewritten from current code; no deviations). A4d done
  (`graph/characteristics/`, `graph/transforms/`, `graph/containers/`,
  `graph/id_types/`, `graph/callback/`; 27 files: 0 `openspec/changes`
  references, comment-only verified; all history paths deleted, "why"
  constraints preserved as 1-2 lines per CONVENTIONS.md §1). A4e done
  (17 files: `GenericGridComp.F90`, `GraphResolutionEntry.F90`,
  `OuterMetaComponent.F90`, 13 files under `OuterMetaComponent/`, 4 files
  under `specs/`; 0 `openspec/changes` references, comment-only verified;
  stale history compressed, REQ-IDs (REQ-HIER-002/003/004/005) retained,
  "why" constraints 1-2 lines; no deviations).

**A5. Centralize graph-layer Info access.** Code change. After A4d
(A4c/A4d edit comments in the same files). Before B3 (B3 write methods
rely on it). Graph code (`superstructure/generic/graph/`,
`GraphBuilder*.F90`) must reach framework-managed Info only through the
`MAPL_Field*`, `MAPL_FieldBundle*`, `MAPL_State*` setters/getters; key
strings live only in the infrastructure Info modules
(`mapl_FieldInfo_mod`, `mapl_FieldBundleInfo_mod`, state equivalents).
Known direct access to convert:
- `GraphBuilder_StatePopulation_smod.F90`: `ESMF_InfoGetFromHost` +
  `FieldInfoSetInternal(info, alias_id, restart_mode)`. Add a
  `restart_mode` keyword to `MAPL_FieldSet` (and `MAPL_FieldGet`), which
  already resolve the named-alias id; legacy callers of the internal
  routine may stay for now.
- `graph/StateItemVariantInfo.F90` and the `*_set_info`/`*_get_info`
  procedures in `graph/MAPLStateItemFlag.F90`: the variant tag
  (`/MAPL/GraphStateItem/variant`) on a Field, FieldBundle, or State's
  own Info. Move the key into the infrastructure Info modules and expose
  it through the setters/getters. There is no setter today for a
  State's own Info (`MAPL_StateSet` takes an item name); propose the
  interface and ask the owner before adding one.
- Out of scope: Info on non-item objects in `OuterMetaComponent/`
  (memory-checkpoint flag, `MAPL/GRIDCOMP_IS_GENERIC`).
- **Status:** done (commit trailer `Plan-Task: A5`). Deviations:
  `restart_mode` and `state_item_variant` are new keywords of
  `MAPL_FieldSet`/`MAPL_FieldGet`; `state_item_variant` is also on
  `MAPL_FieldBundleSet`/`Get`. `MAPL_FieldSet` skips the `FieldDelta` path
  when none of geom/vgrid/typekind/num_levels/units is given, so the new
  keywords are Info-only on a complete field. Owner decisions: a State's
  own Info is reached by new separate routines `MAPL_StateSetVariant` /
  `MAPL_StateGetVariant` (`MAPL_StateSet`/`Get` left alone), and
  `MAPL_StateItem_Flag` moved to `enums/StateItemFlag.F90` so the
  setters take the typed flag (not re-exported by `mapl_enums_api`: its
  `MAPL_STATEITEM_*` names clash with `mapl_StateItem_mod`). Untagged
  items read back `MAPL_STATEITEM_INVALID` (replaces `found`); a tag
  that is present but not a known name fails. The Info
  key (`/MAPL/GraphStateItem/variant`) and stored value are unchanged,
  now `KEY_STATE_ITEM_VARIANT` in `MAPL_ESMF_InfoKeys.F90`;
  `StateItemVariantInfo.F90` moved to `infrastructure/field/` with new
  names. No grep ctest was kept (owner decision: such rules are fixed
  when noticed, or belong in a future general linter).
  `MAPL_StateSet`'s `intent(out)` bug is untouched (not needed here).

**A5b. Overload `MAPL_StateGet`/`MAPL_StateSet` for a State's own
properties.** Owner preference: overloads, if the compilers accept
them. Replace `MAPL_StateGetVariant`/`MAPL_StateSetVariant` with
specifics `MAPL_StateGet(state, unusable, state_item_variant, rc)` /
`MAPL_StateSet(...)` in the existing generic interfaces. Keep the
existing item specifics (`state_get(state, itemName, ...)`, which has a
required `character` second argument). The new specifics must have no
`itemName` dummy and must put `class(KeywordEnforcer) unusable` second,
so they are distinguishable (F2018 15.4.3.4.5). Note `StateGet` already
has `state_get_bundle(state, bundle, rc)`: check the new specific is
distinguishable from it too. If NAG or gfortran reports ambiguity, stop
and report; do not rename. Also:
- Fix `state_set`'s `intent(out)` value dummies (should be `intent(in)`).
- Update callers (`GraphStateItem.F90`, `CompositeStateMaterialization.F90`,
  `Test_StateVariant.pf`) and the docs A5 touched.
- Out of scope: the callback map in `StateAddMethod.F90` (wait for
  CallbackState work).
- **Status:** done (commit trailer `Plan-Task: A5b`). Deviations: none;
  NAG and gfortran both accept the overloads (`state_get_own`,
  `state_set_own`). `StateVariant.F90` / `mapl_StateVariant_mod` deleted.
  `state_set`'s dummies are now `intent(in)` (`units` is
  `character(len=*)`), and its `intenT` typo is fixed.

### Phase B - characteristic architecture

B1 through B5 each implement their section of
`openspec/changes/characteristic-separation-of-concerns/tasks.md`, using
its `design.md`. These take precedence over the summaries below. Each
task ticks the checkboxes it completes.

**B0. Openspec design change (no code).** Write
`openspec/changes/characteristic-separation-of-concerns/` covering
D1-D6. Must specifically study and document:
- `specs/ExpressionClassAspect.F90`: how expressions convert to a field
  then use field rules -> defines the EXPRESSION export-MIRROR allowlist
  entry and the expression shape's extension order.
- Legacy `get_aspect_order` for every ClassAspect -> table of required
  shape orders.
- Legacy Info consumers -> UNCHECKED Info policy.
- D3 setter mapping: for each characteristic, the `MAPL_FieldSet` /
  `MAPL_FieldBundleSet` / `MAPL_StateSet` keyword it writes through, or
  the reason it cannot (and which setter argument to add instead).
- Inventory of every current `GraphStateItem` component and every
  `populate_characteristics` block -> target characteristic for each.
Review with owner before B1.
- **Status:** done. Deviations from D1-D6 are listed in the change's
  design.md §8 (component-supplied values start DEFERRED, TypeKind R4
  default SPECIFIED; ordering re-queried per chain tip; phase argument
  on `sync_to_info`; Geometry writes `geom=` not an id; provisional
  `LongName`). Owner answered Q1, Q8, Q10, Q12 (design.md "Owner
  decisions"). The remaining questions are deferred to the B3 task named
  beside each.

**B1. Extract `StateItemDescriptor`.** Mechanical move of the
characteristics map and all `declared_*`/`ungridded_dims`/
`vertical_stagger` into a new type owned by `StateItemNode`. No
behavior change. `GraphStateItem` left as handle only.
- **Status:** done (commit trailer `Plan-Task: B1`). `StateItemDescriptor` added
  (`graph/StateItemDescriptor.F90`); `StateItemNode%get_descriptor()`
  returns a pointer. Deviations: `GraphStateItem%ordering` stays on
  `GraphStateItem` (design.md §4 and tasks.md 4.3 delete it in B4);
  `set_shared_characteristic` now installs only the ESMF handle and no
  longer replaces the node's characteristics or `declared_*` values
  (design.md §7.3 retargets it in B3-SetShared; no production caller);
  the characteristic/description tests moved to `Test_StateItemDescriptor.pf`;
  `find_mismatched_state_item_characteristics`,
  `resolve_export_geom/vgrid` and the materialization helpers now take
  `StateItemDescriptor`.

**B2. Generic status resolution.** Implement D2 in base class +
descriptor (including freeze check for DEFERRED and the EXPRESSION
allowlist). Convert TypeKind first (only real MIRROR user today:
StatisticsGridComp). Unit tests for every cell of the D2 table.
- **Status:** done (commit trailer `Plan-Task: B2`). Deviations: value constructors set SPECIFIED
  (default-initialized stays INVALID; owner decision), so
  `test_newly_constructed_characteristic_is_invalid_status` now tests a
  default-initialized variable. Resolution lives in
  `StateItemDescriptor.F90` (`resolve_pair`/`resolve_characteristics`/
  `resolve_descriptors`); the map-only `find_mismatched_characteristics`
  fails if a MIRRORED adoption would be needed. The freeze check is a
  descriptor method reported through `report_if_failed` (like the
  convergence check). `StateItemCharacteristicDetection` was deleted
  (test-only; its one-sided-is-mismatch rule contradicted §2.6).
  StatisticsGridComp scenarios remain in PENDING with the same failures
  as before B2; ESSENTIAL `history_1` (`typekind: mirror`) passes.
  Owner-approved gaps: the allowlist had no (EXPRESSION,
  VERTICAL_STAGGER) entry until that kind existed (added by B3-VerticalStagger);
  an allowlisted MIRRORED export only resolves as a legal match, and
  giving each consumer its own precision/geometry stays with the
  existing expression hook until B3-Expression.

**B3.x. One task per characteristic.** For each: move to generic status,
implement its D3 write method (through `MAPL_*Set`), add VariableSpec factory, remove its block from
`populate_characteristics`, remove any GraphBuilder references to it.
Label `B3-<Name>` with the name as listed (exception: PhysicalUnits is
`B3-Units`). List: PhysicalUnits, TypeKind (finish), UngriddedDims, VerticalCoordinate,
VerticalStagger (new), Geometry, ItemShape (incl. D4 ordering),
StandardName, Attributes, QuantityType, Conservation, Normalization,
RestartMode (new), Expression (new), LongName (new),
SetShared (kept: runtime geometry change is a goal). Added by owner
decisions (design.md A13-A18): RenameVerticalGrid (renames the landed
VerticalCoordinate to VerticalGrid; old label `B3-VerticalCoordinate`
stays in history), VerticalOrientation (new, split from VerticalStagger),
HorizontalDimsSpec (new, split from Geometry), StringMirror (shared
mirror sentinel for StandardName/LongName), MirrorFlagGuard (A21: reject
mirror flags on exports in `make_VariableSpec`), MirrorGeneric (A21 as one
generic rule: no mirror of any kind on a non-EXPRESSION export), DimsZLegacy (A22: legacy
`GeomAspect` skips the geom comparison for two `dims: z` items),
IncompatiblePair (A23: `BUILD_TRANSFORM_INCOMPATIBLE`, reported as a
connection conflict). Run order: tasks.md §3 header.
- Later cleanup, not a current task (owner): one "Declared by" keyword per
  characteristic, one mirror mechanism (sentinels instead of
  `mirror_geom`/`mirror_vgrid`), and renaming ItemShape and
  VerticalOrientation (design.md §5.1 note). Rejecting `mirror_geom`/`mirror_vgrid`
  on a non-EXPRESSION export is task B3-MirrorFlagGuard (A21), not part of
  this cleanup.
Run one at a time after B2 (direct commits; no parallel agents). Produce `docs/graph/adding-a-characteristic.md`
in the first one; later ones refine it.
- Each task whose characteristic has a Transform teaches
  `check_graph` in `Test_Scenarios.pf` to recognize that step kind
  (today only `units` and `typekind`; others report `unknown`).
- B3-VerticalCoordinate also covers parity finding M2: the component's
  `geometry: vertical_grid:` must reach each item's characteristic
  (today only an item-level `geometry:` block sets it), so the graph can
  see a vertical-grid mismatch. Do not flip `vertical_*` to ready here;
  that needs C1's vertical regrid Transform.
- **Status:** B3-Units, B3-TypeKind, B3-UngriddedDims, B3-Geometry,
  B3-VerticalCoordinate, B3-VerticalStagger, B3-VerticalOrientation, B3-HorizontalDimsSpec, B3-StandardName, B3-Attributes, B3-StringMirror and B3-MirrorFlagGuard done (commit trailers
  `Plan-Task: B3-Units`, `Plan-Task: B3-TypeKind`,
  `Plan-Task: B3-UngriddedDims`, `Plan-Task: B3-Geometry`,
  `Plan-Task: B3-VerticalCoordinate`, `Plan-Task: B3-StandardName`,
  `Plan-Task: B3-Attributes`, `Plan-Task: B3-RenameVerticalGrid`,
  `Plan-Task: B3-VerticalStagger`, `Plan-Task: B3-VerticalOrientation`,
  `Plan-Task: B3-HorizontalDimsSpec`, `Plan-Task: B3-StringMirror`,
  `Plan-Task: B3-MirrorFlagGuard`); others not started.
  B3-StringMirror: `VariableSpec` exports `MAPL_STRING_MIRROR` (`'<mirror>'`;
  YAML `standard_name: "<mirror>"`), shared with the later LongName task.
  `make_VariableSpec` does not store it: it sets `standard_name_mirror` and
  leaves `standard_name` unallocated, so the FieldDictionary lookup (alias
  fallback), `StandardNameAspect` and `split_name` never see the string. The
  StandardName factory maps the flag to MIRRORED and asserts on an export;
  only FIELD carries the entry, so the flag is ignored on other shapes
  (their member fields carry their own names). Owner decision, generic
  for all kinds: an UNCHECKED export against a MIRRORED import now adopts
  (the import becomes UNCHECKED) instead of conflicting; design.md §2.2, the
  descriptor spec table and `Test_StatusResolution` are amended. The factory
  assertion runs only in the graph path, so an export typo is not caught in
  legacy-only mode. No scenario changed. Not reproduced on a clean HEAD
  baseline: `MAPL.generic.scenarios.pending` on NAG fails 114 checks in the
  tree (the count recorded for B3-VerticalStagger and B3-VerticalOrientation),
  but the HEAD worktree's pending run aborted on a NAG gFTL runtime error
  (`Dangling pointer A%TREE`, `set/iterator_procedures.inc`) before running
  scenarios, so the set could not be compared. That worktree was configured
  `Debug` while the main `nag/` tree is `Release`, so the abort may be a
  Debug-only NAG runtime check rather than a HEAD defect (not investigated).
  One parallel gfortran ESSENTIAL
  run timed out `MAPL.generic.scenarios` (1876 s) because the host laptop
  was asleep during the run; it passed alone in 45 s and in a clean full
  rerun.
  B3-MirrorGeneric (tasks.md 3.19b, done): A21 is one rule.
  `VariableSpec%requests_mirror()` lists each explicit mirror spelling;
  `make_VariableSpec` asserts `not requests_mirror()` on a non-EXPRESSION
  export, and `make_characteristics` runs a post-loop check that fails on any
  MIRRORED entry on such an export, naming the kind. The per-factory export
  asserts are removed. `requests_mirror()` checks only explicit requests;
  an EXPRESSION's default MIRRORED entries are not requests (consistent
  with A21, not a deviation). Owner-approved fixture changes: `typekind` and `ungridded_dims`
  mirror tests now use import intent. `test_shapes_without_standard_name_get_no_entry`,
  `..._ungridded_dims_...`, `..._attributes_...` reset the descriptor per shape
  (they leaked a BRACKET-family MIRRORED geometry into later shapes).
  Factory-path tests expect the full message including ` (<KIND>)` plus
  `status=1`; `_ASSERT` in the check records a message as well as setting rc.
  Sonnet review (after landing): accept with follow-ups. Owner accepted the
  map copy in `check_no_mirrored_entry` as is. The rest is folded into
  tasks.md 3.20 (owner): stale
  per-kind "error on an export" text in `make_characteristics.F90` header
  comments and `adding-a-characteristic.md` (VerticalStagger,
  VerticalOrientation, StringMirror, HorizontalDimsSpec sections), and
  `requests_mirror()` constructor tests for typekind, horizontal dims,
  ungridded dims, stagger and alignment, plus post-loop tests for typekind
  and ungridded dims. Non-ESSENTIAL tests and non-test configs: E1-intel.
  B3-MirrorFlagGuard (done, owner-approved fixture change): `make_VariableSpec`
  rejects `MAPL_STRING_MIRROR` on any export and `mirror_geom`/`mirror_vgrid`
  on a non-EXPRESSION export (A21); the Geometry and VerticalGrid factories
  assert the flags too (A19). Fixtures: `Test_GraphGeometryHook`
  `advertise_and_hook` makes `M` an import (`check_item` takes an intent),
  `advertise_undeclared_stagger` uses `mirror_geom` for imports only and a
  `geom_id` for exports; seven `Test_VariableSpecCharacteristics` tests set
  `ESMF_STATEINTENT_IMPORT` (owner-approved). Constructor tests check the
  message via `@assertExceptionRaised`. gfortran rejects `&` continuations
  inside `_ASSERT` and identifiers over 63 characters; both avoided.
  This is the first, per-flag version of A21; the owner then made A21 one
  generic rule, which B3-MirrorGeneric (tasks.md 3.19b) implements.
  B3-Attributes: `AttributesCharacteristic` gained `write_info`, a
  successful no-op (no Info key; design §3.4), `is_structural()` stays
  false. The factory (`make_attributes_characteristic`) always gives
  SPECIFIED, including an empty set (design §5 parity fix), and inserts
  only for FIELD/BRACKET/VECTOR/VECTORBRACKET/FIELDBUNDLE (design §4.3:
  not EXPRESSION, which previously got an entry when it declared
  attributes; STATE, SERVICE and WILDCARD likewise). Its
  `populate_characteristics` block, kind constant and type are gone from
  `GraphBuilder_Advertising_smod.F90`. No legacy absent-means-mirror
  case. Remaining GraphBuilder references, for B4: the ATTRIBUTES kind
  in `merged_item_for_link` and the `GraphBuilder.F90` use list. No
  `Transform` exists (an unmet requirement is `BUILD_TRANSFORM_UNSUPPORTED`),
  so `check_graph` needs no new step kind. Scenario report (design Risks):
  no scenario changed. ESSENTIAL scenarios pass; `MAPL.generic.scenarios.pending`
  on NAG fails the same 114 checks (`history_1`, `service_service`,
  `service_with_options`, `statistics`, `statistics_real`) in a HEAD
  worktree and in this tree (Rule 1). Tests: factory status per input and
  shape set (empty default is SPECIFIED, EXPRESSION has no entry);
  `sync_to_info` on Field and FieldBundle succeeds in
  both phases; the status table with attributes (empty export vs required
  import is `PAIR_EXTEND`, superset matches, asymmetric); advertise default
  now has an empty ATTRIBUTES entry. ESSENTIAL is green on NAG and
  gfortran (65/65).
  B3-StandardName: `StandardNameCharacteristic` no longer reads status
  (the UNCHECKED escape in `needs_extension_for` is deleted; the
  descriptor's `resolve_pair` decides) and gained `write_info`
  (`standard_name=` through `MAPL_FieldSet`/`MAPL_FieldBundleSet`, both
  phases, `is_structural()` stays false; UNCHECKED writes `'<unknown>'`).
  The factory (`make_standard_name_characteristic`) gives SPECIFIED for a
  declared name, UNCHECKED for an unallocated or `'<unknown>'` one, and
  inserts only for FIELD (design §4.3). Its `populate_characteristics`
  block is gone. Behavior changes: a FIELD with no standard_name now has
  an UNCHECKED entry (was absent; same matching), and BRACKET, VECTOR,
  VECTORBRACKET, FIELDBUNDLE and EXPRESSION no longer get an entry even
  when one is declared (was inserted). No legacy absent-means-mirror
  case. The only GraphBuilder reference left is the
  `STANDARD_NAME_CHARACTERISTIC_KIND` enumeration in
  `merged_item_for_link` and the `GraphBuilder.F90` use list, for B4. No
  `Transform` exists, so `check_graph` needs no new step kind.
  Tests: the subclass's UNCHECKED-status test moved to
  `Test_StatusResolution.pf` (the table does it now); `sync_to_info` on
  Field and FieldBundle, UNCHECKED and INVALID, and the factory's status
  and shape set are new. ESSENTIAL is green on NAG and gfortran (65/65);
  PENDING `MAPL.generic.scenarios.pending` on NAG fails `history_1`,
  `service_service`, `service_with_options`, `statistics`,
  `statistics_real`; this matches the set recorded for B3-Geometry, but
  was not re-run on a HEAD baseline here.
  B3-VerticalCoordinate: see design.md §5 and adding-a-characteristic.md
  "VerticalGrid as implemented".
  B3-VerticalOrientation (done): see adding-a-characteristic.md
  "VerticalOrientation as implemented". New kind
  `VERTICAL_ORIENTATION_CHARACTERISTIC_KIND` and
  `VerticalOrientationCharacteristic` (structural; `vert_alignment=`
  through `MAPL_FieldSet`/`MAPL_FieldBundleSet`). The (EXPRESSION,
  VERTICAL_ORIENTATION) allowlist entry is added. Deviations: no new
  `CharacteristicDefaults` field (it reads the existing vgrid's
  `get_coordinate_direction`); the resolved direction is written instead of
  `with_grid` (owner-accepted). Owner decision: `parse_var_specs` now reads
  `vertical_alignment`, so the `vertical_alignment_*` scenarios carry real
  orientations; legacy copy-on-connect of the export's alignment is
  treated as a legacy defect, and the graph compares the values. No
  `GraphBuilder*.F90` reference to the kind exists. ESSENTIAL is green on
  NAG and gfortran (65/65), so no legacy test needed repair. PENDING
  `MAPL.generic.scenarios.pending` on NAG fails 114 checks in the same five
  scenarios (`history_1`, `service_service`, `service_with_options`,
  `statistics`, `statistics_real`) and the same count as recorded for
  B3-VerticalStagger; not re-run on a HEAD baseline here. The orientation
  mismatch in `vertical_alignment_same_grid` is masked by the earlier
  `VERTICAL_GRID` mismatch (warning only), so no scenario exercises the
  kind; a reversal `Transform` is a C1 item.
  B3-HorizontalDimsSpec (done): see adding-a-characteristic.md
  "HorizontalDimsSpec as implemented". New kind
  `HORIZONTAL_DIMS_SPEC_CHARACTERISTIC_KIND` (14) and
  `HorizontalDimsSpecCharacteristic` (structural; `horizontal_dims_spec=`
  through `MAPL_FieldSet`, nothing for bundles); `horizontal_dims_spec` is
  gone from `GeometryCharacteristic`. Decision gate: field creation does
  need the geom of a NONE item (`make_grid_to_field_map`), so the owner
  approved Option A: a NONE item's Geometry is UNCHECKED and may carry a
  geom (`GeometryCharacteristic(compared=.false.)`); `write_info` writes
  it when it holds a geom, and `resolve_deferred` is now offered to
  UNCHECKED entries (design.md §3.3 amended). The mirror value is a new
  enum member `HORIZONTAL_DIMS_MIRROR` (YAML `dims: mirror`, imports only;
  `StatisticsGridComp` uses it); the factory turns it into MIRRORED, and
  legacy `GeomAspect` treats it as agreeing with anything and adopts the
  export's spec at connection. Deviations: (1) GEOM vs NONE is
  `BUILD_TRANSFORM_UNSUPPORTED`, like every structural kind, not a hard
  error (owner later decided it is a hard error, A23: B3-IncompatiblePair); (2) "ordered before GEOMETRY" is
  not implemented: it belongs with B3-ItemShape's `get_extension_order`
  (the new kind is 14, after GEOMETRY), which is harmless while a NONE
  item's Geometry never mismatches. New scenario
  `graph-scenarios/dims_z_field` (ready). Mismatch tests are unit tests
  (`Test_StatusResolution`, `Test_StateItemCharacteristicSubclasses`), not
  scenarios, since a GEOM-vs-NONE connection is unsupported. No
  `GraphBuilder*.F90` reference to the kind exists.
  B3-VerticalStagger (done): see adding-a-characteristic.md "VerticalStagger
  as implemented". New kind `VERTICAL_STAGGER_CHARACTERISTIC_KIND` and
  `VerticalStaggerCharacteristic` (structural; `vert_staggerloc=` through
  `MAPL_FieldSet`/`MAPL_FieldBundleSet`). The (EXPRESSION, VERTICAL_STAGGER)
  allowlist entry is added. `StateItemDescriptor%vertical_stagger` and its
  get/set are gone; `resolve_export_vgrid` now also returns the stagger it
  read from the characteristic. `CharacteristicDefaults` gained
  `component_scope` and `vert_stagger`; GEOM_B sets the first, and
  `fill_deferred_from_handle` harvests the second from a completed Field
  (or a bundle's first member) that recorded a stagger. Behavior change: an
  item with no `vertical_dim_spec` is DEFERRED, and becomes SPECIFIED NONE
  at GEOM_B only in a component with no vertical grid. Test fixtures that
  build a `VariableSpec` by hand now set `vertical_stagger` where the item
  must settle. No scenario changed: ESSENTIAL is green on NAG and gfortran
  (65/65); PENDING `MAPL.generic.scenarios.pending` on NAG still fails 114
  checks, the same 114 scenarios with identical failure messages as a
  clean HEAD worktree. No `Transform` exists (`BUILD_TRANSFORM_UNSUPPORTED`).
  Remaining GraphBuilder references, for B4: `get_item_vertical_stagger`
  (names the kind and the concrete type) and `harvest_stagger` in
  `GraphBuilder_PortsAndProxies_smod.F90` / `_Geometry_smod.F90`; the
  `GraphBuilder.F90` use list and `merged_item_for_link` are unchanged.
  B3-RenameVerticalGrid (done, no behavior change): `VerticalCoordinateCharacteristic`
  is now `VerticalGridCharacteristic` (file `graph/characteristics/VerticalGridCharacteristic.F90`,
  kind `VERTICAL_GRID_CHARACTERISTIC_KIND`, display `"VERTICAL_GRID"`), with the factory
  `make_vertical_grid_characteristic`. Renamed in the graph sources, tests, CMake, the
  `VariableSpec` factory, and the docs and delta spec that name the kind. Deviations: the
  renamed file's header dropped its "named X, NOT Y" collision note, which would otherwise
  read as self-contradictory, and its change-history comments about the retired
  module were removed (AGENTS.md), shortening several comments. Historical `B3-VerticalCoordinate` labels,
  archives, and the unrelated `VerticalCoordinateDirection` and legacy `VerticalCoordinate`
  (infrastructure) types are untouched. ESSENTIAL 65/65 on NAG and gfortran.
  B3-Geometry (first DEFERRED kind): `GeometryCharacteristic` gained
  `write_info`, `is_structural() = .true.` and `resolve_deferred`; the
  factory (`make_geometry_characteristic`) gives SPECIFIED for a declared
  geom or geom id, MIRRORED for an EXPRESSION or `mirror_geom`, DEFERRED
  otherwise. `mirror_geom` is a new `AddSpec`/`VariableSpec` keyword (YAML:
  `geometry_mirror: true`); Statistics imports and History collection
  imports without a `geom:` use it. No implicit absent-means-mirror. Its
  `populate_characteristics` had no GEOMETRY insert (only
  `set_declared_geom`), so this task is the first to give FIELD, BUNDLE,
  EXPRESSION and SERVICE items a GEOMETRY entry (parity finding M1);
  `StateItemDescriptor%declared_geom` is gone. New: `CharacteristicDefaults` (the component's geom and id),
  `StateItemCharacteristic%resolve_deferred` (no-op default) and
  `StateItemDescriptor%resolve_deferred`, called at GEOM_B (and, for a
  History collection declaring `geom:`, that resolves its imports from the
  collection's own geom: covered by
  `test_geometry_hook_resolves_deferred_imports_from_own_geom`).
  Owner-approved deviations, recorded in design.md §2.2/§2.3/§8: a
  DEFERRED side at connect time is `PAIR_WAIT` (not a conflict); the
  import stays unsettled and `resolve_connections` retries it on a later
  pass; a component with connections still waiting at freeze fails freeze
  (`has_waiting_connections`). When two connections name one import and
  one waits, the import stays unsettled (`waiting_imports`) so the waiting
  connection is retried. A component with no default geom fills its
  DEFERRED items in its own init phase; `graphbuilder_harvest_deferred`
  reads the geom off the component's field or bundle of that name in the
  state, read-only (it does not bind the node's handle; imports are
  skipped), at the start of the connect hook. Owner-approved scope
  change: Statistics advertises only the chosen variance algorithm's
  internal fields (`TimeVariance.F90`), and `realize_provided` shares
  `get_variance_algorithm`. `push_geometry_to_child` is skipped for a
  component with no geom of its own. Every stub
  `build_transform` (Geometry, Attributes, UngriddedDims, StandardName,
  QuantityType, Conservation, Normalization, ItemShape's other pairings,
  the VerticalCoordinate single-overlap case) now calls
  `report_build_transform_unsupported(rc)`: `rc = BUILD_TRANSFORM_UNSUPPORTED`,
  nothing raised, documented in CONVENTIONS.md; so `regrid`, `regrid_r8`
  and `vector_1` are reported as unsupported, not errors. Tests added: harvest (fills from a realized
  field; skips imports), the geometry hook resolving DEFERRED imports from
  the component geom, a MIRRORED import wiring through the connection
  gate, waiting-connection tracking, `geometry_mirror` parsing, and mixed
  geom vs geom_id in `needs_extension_for`. PENDING comparison against a
  HEAD worktree (`ctest -R MAPL.generic.scenarios.pending`, Rule 1): HEAD
  fails `expression_defer_geom`, `history_1`, `scenario_reexport_twice`,
  `service_service`, `service_with_options`, `statistics`,
  `statistics_real`, `vector_1`; this tree fails `history_1`,
  `service_service`, `service_with_options`, `statistics`,
  `statistics_real`. No scenario fails here that passes at HEAD. The
  remaining ones fail for causes HEAD shares: "materialization reached
  with no import units" (`history_1`, `statistics_real`; cause not
  investigated here) and "MethodGraphNode: argument name already declared"
  (`statistics_real`); neither is a still-DEFERRED GEOMETRY. Remaining
  GEOMETRY references for B4: `get_item_geom` and the `GraphBuilder.F90`
  use list, and the `insert_geom_id_characteristic` import in
  `GraphBuilder_Geometry_smod.F90`. ESSENTIAL is green on NAG and gfortran
  (65/65).
  B3-UngriddedDims: `UngriddedDimsCharacteristic` gained `write_info`
  (ungridded_dims= through `MAPL_FieldSet`/`MAPL_FieldBundleSet`) and
  `is_structural() = .true.`; the factory
  (`make_ungridded_dims_characteristic`) maps `is_mirror()` to MIRRORED,
  else SPECIFIED, and inserts only for FIELD/BRACKET/VECTOR/VECTORBRACKET/
  FIELDBUNDLE (design §4.3: not EXPRESSION/STATE/SERVICE/WILDCARD, which
  previously got an entry). Its `populate_characteristics` block and the
  `StateItemDescriptor%ungridded_dims` duplicate (`get/set_ungridded_dims`)
  are gone; materialization reads the characteristic through
  `get_item_ungridded_dims`. Remaining UNGRIDDED_DIMS references in
  `GraphBuilder*.F90` are that getter, the `merged_item_for_link` branch
  and the `GraphBuilder.F90` use list, all for B4. No absent-means-mirror
  case reaches the graph.
  B3-TypeKind: `TypeKindCharacteristic` gained `write_info` (typekind=
  through `MAPL_FieldSet`/`MAPL_FieldBundleSet`) and `is_structural()
  = .true.`; `MAPL_TYPEKIND_MIRROR` moved from the flag constructor to
  the factory (`make_typekind_characteristic`, which also applies the
  §4.3 allowlist, so STATE/SERVICE/WILDCARD/UNKNOWN no longer get a
  TYPE_KIND entry); its `populate_characteristics` block is gone. Only
  the §6.3 materialization references (`get_item_typekind`,
  `merged_item_for_link`, the `GraphBuilder.F90` use list) remain, for
  B4. No legacy absent-means-mirror case reaches the graph.
  B3-Units details follow. B3-Units deviations (owner-approved): the
  materialization references (`get_item_units`, `merged_item_for_link`,
  `GraphBuilder.F90` use list) stay until B4; the factory is a
  `VariableSpec` submodule now (`specs/VariableSpec/make_characteristics.F90`,
  bringing B5's specs -> graph dependency forward) with the D5 shape
  already in place: type-bound `make_characteristics` loops over a
  factory registry (B3.x tasks add an entry; B5 only deletes
  `populate_characteristics`); base `write_info`
  is a non-deferred `_FAIL` default and `is_structural` defaults to
  `.false.` (make deferred once every kind overrides, B4); Q11
  confirmed (no MIRROR marker). §4.3 applicability applied (no units
  entry for STATE/SERVICE/WILDCARD); EXPRESSION units follow the field
  rule, forcing UNCHECKED stays with B3-Expression (Q3). No production
  path calls `sync_to_info` yet.

**B4. Remove manual dispatch.** Delete `merged_item_for_link`'s kind
chain, the `get_item_<kind>` getters (`get_item_units`/`get_item_typekind`/
`get_item_ungridded_dims`/`get_item_geom`/`get_item_vgrid`, plus any later B3 task adds), scalar args of
`materialize_field_extension` (create from goal descriptor + chain
step instead), `GraphStateItem%ordering`/table. Done also requires
`grep -nE '_CHARACTERISTIC_KIND|[A-Za-z]+Characteristic\b' GraphBuilder*.F90`
to show no kind constants or concrete characteristic types (base-class
and interface names are fine); report its output in the commit body.
  Then update `docs/graph/CONVENTIONS.md` so it states the D6 rule as
  current fact (drop the "not true of the current code" caveat).
- Also parity finding M3: the graph does not materialize EXPRESSION,
  VECTOR or SERVICE items (logs `unsupported_item_class:<shape>` and
  leaves them to legacy `add_to_states`). Until the graph materializes
  a shape, scenarios using it stay `graph_ready = .false.` even if they
  pass (masked).
- `MAPL_FieldGet`/`MAPL_FieldBundleGet` can segfault on a handle whose
  Info is not populated yet (D3-cleanup item). Materializing through
  the setters/getters may hit it; fix the getter (D3-cleanup) rather than
  bypassing it.
- **Status:** not started

**B5. Translation in place.** `populate_characteristics` deleted;
advertise calls `VariableSpec%make_characteristics`.
- **Status:** not started

### Phase C - missing capability

**C1. Geometry/regrid and vertical regrid transforms** on the graph
`Transform` interface (duplicate legacy `RegridTransform` and vertical
regrid logic; don't delegate). Flip `regrid*`/`propagate_geom` to ready
in A1's table; flip `vertical_*` once B3-VerticalCoordinate (M2) has
landed. Flip only scenarios the graph actually populates (see "What
ready means today" in `scenario-parity.md`). May be split into
`C1-geom` and `C1-vertical` if large.
- C1-vertical also adds the VerticalOrientation reversal step (B3-VerticalOrientation
  left `build_transform` unsupported) and a scenario whose expectations can detect a
  flipped index order: `vertical_alignment_same_grid` checks a constant value, and its
  orientation mismatch is masked by a VERTICAL_GRID mismatch. It also covers
  `merged_item_for_link`'s missing VERTICAL_ORIENTATION branch if B4 has not yet
  replaced it.
- **Status:** not started

**C2.x. One task per remaining root cause** (label `C2-<short-name>`),
from "Verified root causes" in `docs/graph/scenario-parity.md`. Each task
starts from that diagnosis, fixes production source, and flips the
affected scenarios to ready (unless masked: M3).
- `C2-geom-push` (G1): geometry pushed to children from a parent with
  no geometry.
- `C2-alias-owner` (R1): export aliasing searches the wrong component's
  state for propagated (qualified) exports.
- `C2-alias-nonfield` (R2): export aliasing assumes a Field; VECTOR and
  SERVICE return rc=13. After `C2-alias-owner` (same procedure).
- `C2-runphase-args` (P1): run-phase method-node arguments keyed by
  short name collide when an import and export share a name; key by
  (intent, short name). The diagnosis's name-mangling experiment broke
  `Test_GraphBuilder_RunPhaseNode`; don't repeat it.
- **Status:** not started

### Phase D - structure

**D1. Split `GraphBuilder_ConnectionResolution_smod.F90`** (2500 lines)
by concern: matching, propagation (import mirroring / export
propagation), extension-chain build, qualified-name resolution. No
behavior change.
- **Status:** not started

**D2. Localize `graph_mode()`** checks (currently in 9 production
files) to lifecycle hooks only.
- **Status:** not started

**D3. Small cleanups (accumulating).** A list of small, unrelated
fixes found by other tasks and owned by none. Meta sessions add items
here instead of creating one-item tasks. It has no dependencies, so it
can run whenever the list is worth one session; anything added after
it runs goes in a follow-up commit with the same trailer. Label
`D3-cleanup`.
- Delete the stray `superstructure/generic/to_itemtype.F90`. It is not
  in the build (CMake compiles `ComponentSpecParser/to_itemtype.F90`).
  It differs from the compiled copy only by a `'vector_bracket'` case
  mapping to `MAPL_STATEITEM_VECTORBRACKET`. Before deleting, check
  whether any YAML/test needs `'vector_bracket'` to parse; if one does,
  ask the owner whether to move that case into the compiled copy.
- `infrastructure/field/FieldGet.F90:82-84`: empty
  `if (fstatus == ESMF_FIELDSTATUS_EMPTY)` block with a misleading
  "already deallocated" comment; delete it.
- `MAPL_FieldGet(geom=)` and `MAPL_FieldBundleGet` segfault on a
  field/bundle whose Info is not yet populated (`FieldGet.F90:79-90`).
  They should fail with an rc. B3-Geometry's harvest reads through ESMF
  directly to avoid it; switch harvest back to the MAPL getter once fixed.
- `ComponentSpecParser/parse_var_specs.F90:77-80`: fix the indentation
  of the `geometry_spec`/`geom_id`/`mapl_geom`/`geom_mgr` declarations.
- Scenario fixtures with `/`-qualified short names (e.g. `A/E_A1`) have
  no field-dictionary entries, so every run logs "not found in field
  dictionary (as an alias)" (`VariableSpec.F90` alias lookup; the lookup
  is correct). Add the entries to the scenario field dictionary, keyed as
  looked up; ask the owner if qualified vs. bare key is unclear.
- **Status:** not started

### Phase E - closing

**E1. Full test run with Intel on bucy.** Runs last, after every other
task. The non-ESSENTIAL ctest labels (e.g.
`MAPL.generic.scenarios.pending` and other tests skipped by
`-L ESSENTIAL`) need data and an Intel build that exist only on bucy.
On bucy, using the `remote-build` skill, `with-intel` (`ifx-stack`,
unversioned): configure a clean Release build of the plan's final HEAD
and run the full ctest suite (no `-L ESSENTIAL`). Also grep every
YAML/RC config there (outside `superstructure/generic/tests/`) for
mirror values (`typekind: mirror`, `vertical_dim_spec: MIRROR`,
`geometry_mirror`, `dims: mirror`, `"<mirror>"`, `vertical_alignment:
mirror`) under an `export:` section, since A21 now rejects them on a
non-EXPRESSION export. For each failure, compare against a clean build of
the commit before Phase B (the `Plan-Task: B0` design commits do not
change code; use the parent of the first `Plan-Task: B1` commit) with the
same build type, then classify it as caused by this plan or as existing
before it. Report findings; do not fix. Fixes become new tasks. Label
`E1-intel`.
- **Status:** not started

Author = writes the task and leaves it uncommitted. Reviewer = the
read-only `reviewer` subagent (`.opencode/agents/reviewer.md`):
`/review-task <label> [tier|provider/model] [focus]` (default tier opus).
It reads the work itself (uncommitted diff, or the task's commits via
the Plan-Task trailer if already committed); never hand it the author's
summary.

### Review and commit
1. Author finishes: changes uncommitted and unstaged (new files left
   untracked; list them in the message body), proposed message in
   `.git/PLAN_TASK_MSG`.
2. Owner reviews the working tree (e.g. magit status), optionally after
   `/review-task <label>` in a separate session.
3. Fixes go into the working tree (owner asks the author session). If
   the review says "rework needed", or the author session is very long,
   the author first writes its unfinished list into the task's Status
   line and the rework runs in a fresh session from the working tree.
   If the parent session does not relay the review, `last-review
   <label>` prints it.
4. Owner commits: `git commit -aF .git/PLAN_TASK_MSG`, or tells the
   author session "commit". (`-a` takes every modified tracked file, so
   check `git status` first.) Whoever commits then deletes
   `.git/PLAN_TASK_MSG`; a stale one would be mistaken for the next
   task's message.
5. Problems found after commit go in a follow-up commit with the same
   `Plan-Task:` trailer.
Senior-developer review happens once, at the final PR to `develop`.

Tier names below (`opus`, `sonnet`, `haiku`, `gemini`, `gpt-sol`,
`gpt-luna`, `gemini-flash`) map to concrete model IDs per provider in
`~/.config/opencode/models.md`; resolve with `model-for <tier>`. Never put
a full model ID in this plan or in an agent file.

### Calibration (before Phase B)

Replay a solved problem to rank `sonnet` vs `opus` (and decide
whether `haiku` may author Fortran):
- Debugging replay: `git worktree` just before the task-6.8 fix
  (around `9fdea0fd`; see `docs/graph/ARCHITECTURE.md` §3.4 or its
  history/ copy after A0). Prompt: "MAPL.generic.scenarios fails under
  NAG; find root cause(s) and fix, following the Rules." Score: all
  three causes found? any false "pre-existing" claim? speculative
  workarounds? tokens used.
- Mechanical replay: redo the Phase 17 `Test_GraphBuilder.pf` split.
  Score: builds on both compilers, no scope creep.
If `sonnet` passes the debugging replay cleanly, the "sonnet?" entries
below move from opus to sonnet. If `haiku` passes the mechanical replay,
it may author D1/A4/B5 with sonnet review.
- **Status:** debugging replay done: `sonnet` (nasa_litellm claude-sonnet-5-5) passed, so the `(sonnet?)` rows moved to sonnet, with opus kept as reviewer. haiku (nasa_litellm) also passed the debugging replay but hedged one fix (kept the old path as a fallback); mechanical replay skipped by owner decision - try haiku on real tasks with review. Benchmark kept outside the repo, so candidates can't see the answer key: `~/.opencode/plan/calibration/README.md`. Real-task trials: haiku authored B3-RenameVerticalGrid (clean) and B3-StringMirror (sonnet review found should-fix items, owner decisions came out of it, no rework session needed); both with sonnet review. B3-MirrorFlagGuard (haiku): clean, sonnet review accept with minor test gaps. B3-MirrorGeneric (haiku): looped several times on pFUnit failures (exact-match message; the extra `status=1` exception from `_RC`) before stopping and reporting; the stop rule worked, and a one-message diagnosis from the meta session unblocked it. Give haiku the pFUnit/`_ASSERT` exception rule up front.

| Task | Author | Reviewer | Notes |
|---|---|---|---|
| A0 | sonnet | opus (spot-check) | gemini also viable (long-context audit) |
| A1 harness | sonnet | opus | |
| A1-diagnosis | opus | - | root-causing failing scenarios |
| A2a | opus | owner | sets naming conventions |
| A2b..n | sonnet | - | follow scenario-naming.md |
| A3 | sonnet | - | |
| A4 | sonnet | diff review always | judgment on which "why" to keep; haiku only after calibration |
| A5 | sonnet | opus | touches infrastructure field/bundle/state APIs |
| A5b | sonnet | - | small; compilers verify distinguishability |
| B0 | opus | owner + gemini/gpt-sol | cross-family review: "what did this miss vs. legacy aspects?" |
| B1 | sonnet | opus | escalate any compiler/gFTL crash |
| B2 | opus | gemini/gpt-sol | core semantics |
| B3-Units | opus | owner | template + adding-a-characteristic.md |
| B3-Geometry | sonnet | opus | geom-as-node, Info id |
| B3-ItemShape | opus | - | D4 ordering, Bracket rule |
| B3-VerticalCoordinate | sonnet | opus | first reuse of Geometry's DEFERRED machinery |
| B3-RenameVerticalGrid | haiku | sonnet | mechanical rename, no behavior change; first haiku authoring trial |
| B3-StringMirror | haiku | sonnet | small factory change; second haiku authoring trial |
| B3-MirrorFlagGuard | haiku | sonnet | constructor/factory assertions + two fixtures |
| B3-MirrorGeneric | haiku | sonnet | one generic export check (constructor + after factory loop); drop per-factory export asserts |
| B3-DimsZLegacy | sonnet | - | legacy GeomAspect change + scenario with two grids; stop if legacy still regrids; plus MirrorGeneric review follow-ups (docs/comments, tests) |
| B3-IncompatiblePair | sonnet | - | new build_transform outcome; conflict reporting stays kind-agnostic (D6) |
| B3-VerticalStagger | sonnet | opus | DEFERRED/harvest; fixture risk |
| B3-VerticalOrientation | sonnet | opus | resolve against grid without comparing it |
| B3-HorizontalDimsSpec | sonnet | opus | changes when Geometry is compared |
| B3-Expression | opus | gemini/gpt-sol | resolve Q2 and amend design.md §2.4 first |
| B3 others | sonnet | sonnet (self-review) | one at a time, after B3-Units lands |
| B4 | sonnet | opus | materialization regressions likely |
| B5 | sonnet (haiku?) | sonnet if haiku | |
| C1 | opus | - | ESMF regrid/route handles |
| C2-geom-push | sonnet | opus | |
| C2-alias-owner | sonnet | opus | |
| C2-alias-nonfield | sonnet | opus | after C2-alias-owner |
| C2-runphase-args | sonnet | opus | changes method-node keying |
| D1 | sonnet (haiku?) | sonnet if haiku | `public ::` rule |
| D2 | sonnet | opus | lifecycle-phase ordering |
| D3-cleanup | sonnet | - | items are small and independent |
| E1-intel | sonnet | - | run and classify only; remote build on bucy |

`(sonnet?)`/`(haiku?)` = move to that model only if it passes calibration.
Always opus regardless: B0, B2, B3-Units, B3-ItemShape, A1-diagnosis,
A2a, C1 - errors there are replicated by every later task.

Light models (`haiku` until calibrated, `gemini-flash`,
`gpt-luna`): log triage / ctest summarization only. Non-Claude
models for Fortran authoring: trial first on D1 or one A4 directory
before promoting.

Quota strategy: run A0 + A1 harness (sonnet) now; reserve opus for B0.
If opus is scarce, stop after B2 + B3-Units - do not let sonnet
replicate an unreviewed template across B3.x.

### Rules for every agent prompt
1. Never call a failure "pre-existing" without reproducing it on a
   clean baseline (a temporary `git worktree` of HEAD, removed
   afterward with `git worktree remove`). Configure the baseline with the
   same `CMAKE_BUILD_TYPE` as the tree it is compared against.
2. Build/test NAG then gfortran; put both `ctest -L ESSENTIAL`
   summary lines in the proposed commit message body. Docs-only tasks
   (no source, CMake or test changes) write "docs only; ctest not run"
   instead.
3. On ICE, nondeterministic segfault, or gFTL copy crash: stop and
   report; do not invent workarounds.
4. Do not modify files outside the task's stated scope without asking.
   If the task needs a lifecycle, status-resolution or user-API rule
   that `design.md` does not cover (or contradicts), stop and report
   before building it; the owner settles it and `design.md` is amended
   first.
5. Follow `docs/graph/CONVENTIONS.md` (after A0).
6. Source comments: no `openspec/changes` paths, task numbers, or change
   history. Comments explain why, not when. Put change names in the
   commit message.
7. Do not commit or stage anything (not even `git add -N`). Leave the
   task in the working tree on the integration branch, new files
   untracked, and write the proposed
   commit message (ending with the `Plan-Task:` trailer) to
   `.git/PLAN_TASK_MSG`. Commit only when the owner says so, and then
   only the task's files (never untracked files you didn't create);
   after committing, delete `.git/PLAN_TASK_MSG`.
   Never push, never rewrite earlier commits.

## Dependency summary

A0 -> A1 (harness) -> A2a -> A3; A1 (harness) -> A1-diagnosis -> C2.x;
A0 -> A4 -> {B1, D1}; A4d -> A5 -> A5b -> B3.x; A0 -> B0 -> B1 -> B2 -> B3.x -> {B4, B5};
B4 -> C1, B3-VerticalCoordinate -> C1 (vertical part), D1 -> D2;
C2-alias-owner -> C2-alias-nonfield. The C2 tasks need only A1-diagnosis
and may run before Phase B. A4 is independent of A1-A3 and B0 (they touch tests/docs, A4 touches
production comments), so their order is free; still one task at a time.
D3-cleanup has no dependencies. E1-intel runs after every other task.

**Run order (owner decision, after A5b):** critical path first:
B0 -> B1 -> B2 -> B3.x -> {B4, B5} -> C1. Within B3.x, B3-Units goes
first (template). Off-path tasks (A2b..n, C2.x, D1, D2, D3-cleanup)
come after; their order is not yet decided.

## Open questions (resolve in B0)
- UNCHECKED Info representation.
- Exact expression semantics (owner to refresh from ExpressionClassAspect).
- Whether `SetSharedCharacteristic` survives.
- Post-legacy cleanup tasks (delete aspects/transforms/registry,
  rename `StateItemCharacteristic*` if desired) - out of scope here.
- Post-freeze graph optimizations (future, owner idea): fuse adjacent
  transform steps through a generic `can_fuse_with`/`fuse` query on the
  transforms, and allocate an intermediate transform-created node's
  storage only between its producer and its single consumer when no
  state, name lookup (History, checkpoint, Expression) or sharing exposes
  it. Keep that "exposed" property derivable from the graph.

## Prompt template for task agents

Fill in each `<...>` before pasting: the label appears twice
(`Task:` and `Plan-Task:`). Delete the `Scope:` line if nothing narrows
the task.

```
Task: <label, e.g. B3-UngriddedDims> from docs/graph/plans/separation-of-concerns.md.

Read first: that plan's header, "Context every task prompt should
include", "Rules for every agent prompt", your phase's introduction
(if any) and your task's section; then any artifact your task depends
on (these take precedence over the plan's summary).

Scope: <optional narrowing / extra constraints>.

Done means:
- ctest -L ESSENTIAL green on NAG and gfortran
- this task's Status line in the plan updated (done/partial, deviations)
- any doc your task owns created/updated
- NOT committed or staged: changes left in the working tree (new
  files untracked), proposed commit message in `.git/PLAN_TASK_MSG`
  ending with `Plan-Task: <label>`, body containing both ctest summary
  lines and listing new files. Commit only when the owner says "commit";
  after committing, delete `.git/PLAN_TASK_MSG`.
Stop and report rather than guess if the plan and source disagree.
```

For open-ended tasks (B0, A1 diagnosis) add a sentence of owner intent.
After B3-Units lands, B3.x prompts may add: "Follow
docs/graph/adding-a-characteristic.md; use the commits from
`git log --grep='^Plan-Task: B3-Units' -p` as the exemplar."
