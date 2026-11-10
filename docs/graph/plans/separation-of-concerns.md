# Plan: Graph layer separation-of-concerns cleanup

> Canonical copy: `docs/graph/plans/separation-of-concerns.md` (in repo).
> Every task MUST record its Status (done/partial, and any deviations
> from this plan) as part of the same change as the work: B3.x and later
> openspec tasks in their `tasks.md` item, other tasks in their section
> here; a B3.x task adds only its label to the B3 Status list below. Artifacts produced by earlier tasks
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
committing (or meta lands it under auto-landing; see "Working with the
owner") (see "Review and commit" below). The integration branch goes to `develop` as
one final PR, followed by a walkthrough with the senior developers.
Every task commit ends with a git trailer naming its task:

```
Plan-Task: B3-Units
```

Labels are the task ids below (`A0`, `A1`, `A1-diagnosis`, `A2a`, `B3-Units`, `B3-TypeKind`,
`C2-<short-name>`, ...). Find a task's work with
`git log --grep='^Plan-Task: B3-Units' -p`. Do not record commit hashes
in this plan (they change if the branch is rebased); the trailer is the
reference. Never rebase or amend existing commits. Task agents never
push; the meta session pushes only the integration branch, fast-forward
only, right after each commit the owner approved or meta auto-landed (owner,
2026-11-05).
Only the owner rebases the branch onto `develop` (last: 2026-11-02, onto
develop through #5473), and only while no outsourced PR is open; after a
rebase, rebuild NAG and gfortran from clean, because incremental builds
missed changed module interfaces.
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
- One main-line task at a time. The meta session runs tasks through
  author and reviewer subagents (`/run-task`), and collects questions
  for upcoming tasks up front (`/preflight`); design-heavy (opus-author)
  tasks still run as interactive sessions. A task that touches none of
  the main-line task's files (D3, A4, docs, investigations) may run in a
  worktree at the same time, with owner approval; B3.x tasks may not.
  How: `~/.opencode/plan/orchestration.md`. No plan edits while a task
  has uncommitted work that touches the plan (wait for its commit, then
  edit); worktree tasks never edit the plan.
- Outsourcing (owner, 2026-11-05): tasks off the critical path may go
  to teammates' agents through GitHub: an issue holding a self-contained
  brief, answered by a draft PR against the integration branch. Their
  questions come as PR comments; the owner answers them through meta.
  Meta verifies (NAG and gfortran ESSENTIAL) and reviews. Accepted PRs
  are squash-merged with the `Plan-Task:` trailer. How:
  orchestration.md, "GitHub outsourcing".
- Questions to the owner: legacy behavior first, in legacy terms; a
  concrete YAML example and what the user sees; options with their
  consequences; a recommendation. No labels invented in the session.
  If the owner asks for more context, re-ask from a lower level.
  Details: orchestration.md, "Question format".
- Every task is reviewed before it commits (reviewer subagent at least
  `sonnet` when the model table says `-`). Sonnet-authored tasks that
  the reviewer accepts and that pass NAG and gfortran Debug ESSENTIAL
  land without the owner's "commit" (owner, 2026-11-06), with trailer
  `Landed-By: meta`; the owner reviews them afterwards. Opus/design,
  test-first, and decision-carrying tasks still wait for him. Rules:
  orchestration.md, "Auto-landing".
- Eliminating legacy is a primary goal, but nothing legacy is disabled
  or deleted until (a) this branch is merged into `develop` and (b)
  external client code builds and passes its tests in graph mode.
  Graph-only capability (C1, B3-InternalReexport, ...) proceeds now;
  switching legacy off is a separate later step gated on (a) and (b).
- No legacy-behavior fixes (owner, 2026-11-04; 3.38 was the last). A
  task that finds legacy behaving differently from the graph design
  follows the design and adds a row to "Legacy and graph behavior
  differences" in `docs/graph/scenario-parity.md` (item, legacy, graph,
  task). It asks the owner (dashboard, 🟡, continuing on the graph side)
  only when the design is silent on the case or the legacy behavior looks
  deliberate and client code may depend on it. The weekly report lists
  new rows. The table is the checklist for switching legacy off.
- Per-task doc updates are limited (owner, 2026-11-04): Status (see the
  header), design.md decisions the task makes, delta specs for changed
  requirements, and docs the task owns. `ARCHITECTURE.md`, `GLOSSARY.md`
  and `rules-review.md` are not updated per task; the meta session
  batches them every few landings (from the commits since the last
  batch), or runs a small doc task. A task brief names the docs to update.
- No `CHANGELOG.md` entries per task: one entry when the graph work is
  complete, pointing at the documentation.
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
- Personal workflow tooling (commands, agents, orchestration notes,
  known traps) lives under `~/.config/opencode/` and `~/.opencode/plan/`,
  not in the repo. The owner's personal
  workflow commands (e.g. `/meta-checkpoint`, `/meta-resume`) live in
  `~/.config/opencode/`, not the repo.
- The owner may run reviews or worktrees from other sessions. Check
  `session_v2` for "Review <label>" sessions before saying no review ran,
  and do not attribute a worktree or test log to a task without checking.

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

**A6. Graph test executable throw hook.** The `MAPL.generic.graph`
pFUnit executable initializes through `graph/tests/GraphCoreTestInit.F90`
and never calls `MAPL_set_throw_method`, so a `_FAIL`/`_ASSERT` only
prints `FAIL at line=` to stderr and `@assertExceptionRaised` cannot work
there. Install the throw hook in `GraphCoreTest_Initialize` (without full
MAPL initialization if possible), then fix every graph test it exposes,
including `test_sync_fails_for_kind_without_write_info` and
`test_sync_restart_mode_fails_on_bundle` (`Test_StateItemDescriptor.pf`),
which must assert all their exceptions (CONVENTIONS.md, pFUnit). One
commit. May run in a worktree only while no B3 task touches graph tests.
- **Status:** not started

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
connection conflict), ExpressionPerConsumer (split from Expression: the
per-consumer evaluation, design.md §2.4, Q2/Q3), ExpressionNativeGrid
(later optimization: evaluate on the inputs' shared native grid),
InternalNoMirror (design.md Q13: an INTERNAL item never has a mirror
property), RulesReview (review of all default, connection and intent
rules; tasks.md 3.26), StandardNameValidation (graph validation mode for
standard_name mismatches, plus the "standard_name required on Fields"
switch; tasks.md 3.27), and RulesReview's follow-ups (design.md A24):
ShapeIntentRules (3.28: BRACKET/VECTORBRACKET never IMPORT, WILDCARD only
IMPORT, in `intent_rule_violation`), ConnectionIntentCheck (3.29:
SimpleConnection EXPORT -> IMPORT in `parse_connections`, re-export
exempt), WildcardFieldOnly (3.30), OneExportPerImport (3.31: dropped), GeomAProbe
(3.32: probe legacy GEOM_A overwrite of item geometry), InternalReexport
(3.33: graph export port for an INTERNAL re-export), FillValue (3.34:
fill from the owning spec at materialization; legacy log text fix),
QuantityTypeSuppliers (3.35: YAML key, `make_VariableSpec` keyword,
harvest of a `MAPL_FieldSet` value), QuantityTypeDeferred (3.36:
undeclared export/internal QuantityType DEFERRED, import UNCHECKED;
warn-and-fall-back by default, error in strict mode) and
QuantityBasisUnsupported (3.37: a basis-only QuantityType mismatch is
UNSUPPORTED, A23 amendment). Run order:
tasks.md §3 header. Placement: 3.28, 3.29, 3.30, 3.32, 3.35 and 3.37 are
small, independent of 3.27, and touch code 3.27 does not (intent rules,
parser, wildcard membership, a legacy probe, QuantityType suppliers and
basis outcome), so
they run first while 3.27's design is settled; 3.38 (legacy GEOM_A fix:
item geometry wins) followed 3.37; 3.39 (mirror requests honored) was
dropped with the no-legacy-fixes rule; 3.36 runs right after
3.27 because its strict/default switch should share 3.27's configuration
surface, and after 3.35 because the owner wants suppliers before the
freeze rule; 3.31 was dropped (owner); 3.33 needs B4's ports and materialization; 3.34
belongs with B4's materialization (4.2), though its legacy log-text fix
may land earlier on its own.
- Later cleanup, not a current task (owner): one "Declared by" keyword per
  characteristic, one mirror mechanism (sentinels instead of
  `mirror_geom`/`mirror_vgrid`), and renaming ItemShape and
  VerticalOrientation (design.md §5.1 note). Rejecting `mirror_geom`/`mirror_vgrid`
  on a non-EXPRESSION export is task B3-MirrorFlagGuard (A21), not part of
  this cleanup.
- Later, not scheduled (owner, 2026-11-03): retire `geom_id` (item,
  `GeomAspect`, `VariableSpec`). Storing an id next to the geom duplicates
  the geometry; graph nodes now serve that purpose. Until legacy can be
  switched off, no new code extends `geom_id` use.
- Later, not scheduled (owner, 2026-11-04): give a MAPL geom id to a geom
  that arrives from outside MAPL (no `mapl/geom/id`) when it is first
  seen, e.g. by registering it with the geom manager (use case: NUOPC+MAPL
  hybrid systems). Then relax `HorizontalRegridTransform`'s no-id failure.
- Later, not urgent (owner, 2026-11-04): `TypeKindConverterTransform`
  handles only R4 <-> R8. Add I4 and I8 -> R4/R8 (not R -> I), and make
  `TypeKindCharacteristic%build_transform` offer it for those pairs. First
  check what legacy `CopyTransform` / `TypeKindAspect` do for integer
  exports.
- Moved (owner): this item goes to the first phase of the planned
  callback-physics openspec change (not yet created); kept here as a
  pointer until that change exists. For consideration: an
  IMPORT and an EXPORT share only the underlying data, not the whole
  field. Today an import is an alias (`MAPL_NamedAlias`) of the export's
  field or of an extension field, so the characteristics that may differ
  per item (restart_mode, long_name) live in Info's per-alias section,
  keyed by alias id. First step: a pros/cons analysis, no code. Pros the
  owner noted: removes the per-alias Info section (a kludge MAPL had ESMF
  enable, retrieving an alias id); closes the undeclared-import long_name
  gap by design (tasks.md 4.2); probably easier with graph machinery than
  with StateRegistry. To analyze: ESMF data sharing, readers of alias ids
  (`RestartHandler`, History), extension chains, bundles, cost per import,
  coexistence with legacy. Also (owner, after 3.22): an import that is its
  own field could keep its own standard_name where legacy permissive mode
  makes "the export's name win", turning the 3.27 mismatch question into
  per-item metadata rather than a connection failure. Related owner question: should import and
  export be allowed to differ in names and restart mode at all (History
  use cases)? Can fold into B3-RulesReview's decision session.
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
  B3-VerticalCoordinate, B3-VerticalStagger, B3-VerticalOrientation, B3-HorizontalDimsSpec, B3-StandardName, B3-Attributes, B3-StringMirror, B3-MirrorFlagGuard, B3-MirrorGeneric, B3-DimsZLegacy, B3-IncompatiblePair, B3-LongName, B3-RestartMode, B3-QuantityType, B3-Conservation, B3-Normalization, B3-IncompatibleSweep, B3-InternalNoMirror, B3-ConnectionIntentCheck, B3-WildcardFieldOnly, B3-GeomAProbe, B3-QuantityTypeSuppliers, B3-QuantityBasisUnsupported, B3-StandardNameValidation, B3-QuantityTypeDeferred, B3-Expression, B3-SetShared, B3-ExpressionPlaceholder and B3-ExpressionPerConsumer done (commit trailers
  `Plan-Task: B3-Units`, `Plan-Task: B3-TypeKind`,
  `Plan-Task: B3-UngriddedDims`, `Plan-Task: B3-Geometry`,
  `Plan-Task: B3-VerticalCoordinate`, `Plan-Task: B3-StandardName`,
  `Plan-Task: B3-Attributes`, `Plan-Task: B3-RenameVerticalGrid`,
  `Plan-Task: B3-VerticalStagger`, `Plan-Task: B3-VerticalOrientation`,
  `Plan-Task: B3-HorizontalDimsSpec`, `Plan-Task: B3-StringMirror`,
  `Plan-Task: B3-MirrorFlagGuard`, `Plan-Task: B3-MirrorGeneric`,
  `Plan-Task: B3-DimsZLegacy`, `Plan-Task: B3-IncompatiblePair`,
  `Plan-Task: B3-LongName`, `Plan-Task: B3-RestartMode`,
  `Plan-Task: B3-QuantityType`, `Plan-Task: B3-Conservation`,
  `Plan-Task: B3-Normalization`, `Plan-Task: B3-IncompatibleSweep`,
  `Plan-Task: B3-InternalNoMirror`, `Plan-Task: B3-ConnectionIntentCheck`,
  `Plan-Task: B3-WildcardFieldOnly`, `Plan-Task: B3-GeomAProbe`,
  `Plan-Task: B3-QuantityTypeSuppliers`, `Plan-Task: B3-QuantityBasisUnsupported`,
  `Plan-Task: B3-StandardNameValidation`, `Plan-Task: B3-QuantityTypeDeferred`,
  `Plan-Task: B3-Expression`, `Plan-Task: B3-SetShared`);
  others not started.
  B3-StandardNameValidation (tasks.md 3.27): done, with one deviation (owner).
  Design A25. `graph/ValidationPolicy.F90` is the graph's only reader of the
  `field_dictionary:` validation keys (new key `require_standard_name`).
  `StateItemCharacteristic%match_warning` (default `''`) reports a tolerated
  mismatch; `resolve_characteristics` collects it for every PAIR_MATCH, and
  `resolve_connections` returns it as `connection_warnings` (connect hook
  logs it). StandardName mismatch: strict INCOMPATIBLE, permissive wired
  with legacy's warning; one-side warning replicated; both honor
  `suppress_standard_name_warnings` (#5443). BRACKET carries StandardName.
  V3 check in `make_VariableSpec` (FIELD only, every intent). Deviation:
  UNSUPPORTED pairs are recorded (`note_unsupported_pair`) and
  `GraphBuilder%check_unsupported_pairs` fails on them in strict mode, but
  the freeze hook does not call it yet: the scenario suite runs strict and 11
  ESSENTIAL scenarios have GEOMETRY / VERTICAL_GRID pairs that legacy
  connects (under pFUnit a failed graph step raises). The legacy-retirement
  step adds the call. Scenario `standard_name_and_ungridded_dims_mismatch`
  now expects the pair wired (2 root nodes, the orphan extension node is
  gone). No scenario-parity row: the graph now follows legacy's rule.
  B3-QuantityBasisUnsupported (tasks.md 3.37): done. `QuantityTypeCharacteristic%build_transform` reports
  UNSUPPORTED for a basis-only mismatch and INCOMPATIBLE for a
  quantity-type mismatch (also when the bases differ), through one
  `pair_outcome` shared with `needs_extension_for`; `QUANTITY_UNKNOWN`
  against a known type with two real bases is UNSUPPORTED. The old
  INCOMPATIBLE test was replaced by five tests. No scenario or GraphBuilder
  test reaches QuantityType; `build_chain` stops at the first failing kind
  and the only reporting change is that a basis-only pair is logged as
  "no registered extension provider" rather than "connection conflict" (and
  is not a conflict entry). No deviations.
  B3-FillValue (tasks.md 3.34): done. Plain `StateItemDescriptor`
  fill_value (not a characteristic), set in `advertise_one`, copied export ->
  extension nodes in `build_chain`, applied by `materialize_field_extension`
  after completion; an import's value is never read. Scope: extension fields
  (the only fields the graph creates today). Graph matches legacy: `make_extension` clones the
  export's aspects and does not substitute the class aspect for Field->Field,
  so legacy fills the extension with the export's value too. Log text is
  "(dst value kept)".
  B3-QuantityTypeSuppliers (tasks.md 3.35): done. YAML key `quantity_type:`
  (full QuantityType name, case-folded; unknown value is an error, checked in
  `parse_var_specs` because `QuantityType(name)` maps unknown to UNKNOWN);
  `quantity_type=` keyword on `make_VariableSpec` (legacy
  `make_QuantityTypeAspect` still ignores it); `CharacteristicDefaults` gained
  `quantity_type` + `basis`; `harvest_quantity_type` in
  `GraphBuilder_Geometry_smod.F90` reads `quantity_type_metadata` through
  `MAPL_FieldGet` / `MAPL_FieldBundleGet`; `QuantityTypeCharacteristic%
  resolve_deferred` takes it for a DEFERRED entry only (the descriptor also
  offers the defaults to UNCHECKED entries; they stay). A mirror (Field, absent
  key) or `QUANTITY_UNKNOWN` (bundle absent key, or explicit) counts as not
  supplied. The harvest keeps its geometry gate: `fill_deferred_from_handle`
  returns before reading anything unless the field is GRIDSET/COMPLETE (or the
  bundle has members), so a non-geometry DEFERRED kind is not harvested before
  the item's geom exists. 3.36's fallback ordering must allow for that.
  The undeclared status is unchanged (still SPECIFIED UNKNOWN), so the
  harvest only reaches a hand-built DEFERRED entry until 3.36. A quantity_type
  on STATE, EXPRESSION, SERVICE or WILDCARD is ignored (owner). D3 item
  recorded earlier covers the Field (mirror) vs bundle (non-mirror UNKNOWN)
  absent-key inconsistency; not fixed here. Docs updated:
  adding-a-characteristic.md (supply paths), rules-review.md R1 example. No
  deviations.
  B3-GeomAProbe (tasks.md 3.32): done; probe CONFIRMED for the geom, the
  `geom_id` and the vertical grid. Three tests in `Test_Couplers.pf`
  build an export declaring its own geom G2 (or vertical grid) with no
  component value, call `registry%set_geometry` with the component's
  (G1), and assert the desired rule (item kept); seed assertions show G1
  and G2 (and the two grids) differ. All three fail today, so they are
  `@disable`d with a reason; the fix task removes `@disable`. Geom:
  `GeomAspect%set_geom` replaces `geom` with G1. geom_id: stale, the
  aspect's geom is G1 but the id stays the item's (G2's), because
  `set_geom` assigns an id only when none is set. Vertical grid:
  `VerticalGridAspect%set_vertical_grid` replaces the item's grid
  unconditionally; the aspect has no id. `target_set_geom` did not error
  on the payload update. rules-review.md §2.6 and R16 record the result.
  With `@disable` temporarily removed, all three fail at the item-wins
  assertions on gfortran as well as NAG (no crash). For the legacy fix
  task: `VerticalGridAspect` ignores its `geom`/`typekind` arguments
  (`VerticalGridAspect.F90` ~128-129); `make_VerticalGridAspect` sets
  FROM_COMP with no vertical grid (`VariableSpec.F90` ~873, ~882),
  unlike `make_GeomAspect`; `StateItemSpec%set_geometry`'s geom
  else-branch (~607) calls `aspects%at` and never inserts. Untested: an
  item declaring only a `geom_id` (no geom) ends with the component geom
  but the item's id. Test only; no source change. ESSENTIAL 69/69 on NAG
  and gfortran.
  B3-GeomAItemWins (tasks.md 3.38): done, with deviations. Item wins at
  GEOM_A. `GeomAspect` and `VerticalGridAspect` carry a private "declared by
  the item" flag (and `GeomAspect` a second one for the `geom_id`), set by
  `make_GeomAspect` / `make_VerticalGridAspect` only when the `VariableSpec`
  has its own geom / vertical grid / `geom_id`. The flag is legacy's stand-in
  for the graph's DEFERRED (design A12); a status-based legacy fix only if
  3.39 or later needs it. `set_geom` / `set_vertical_grid` stay
  unconditional; `set_component_geom` / `set_component_vertical_grid` (used
  by `StateItemSpec%set_geometry`) are no-ops on a declared aspect, and
  replace an earlier component value and (unless declared) the `geom_id`.
  `MAPL_GridCompSetGeometry` passes `force_replace=.true.`: it replaces a
  declared value and marks the item declared. `StateItemSpec%set_geometry`
  returns the `rc` of its contained helper (no latent failure surfaced) and
  its geom else-branch inserts the aspect. `VerticalGridAspect`'s dead
  `geom`/`typekind` arguments are gone. The three `@disable`d probes are
  enabled. ESSENTIAL 69/69 on NAG and gfortran.
  Deviations: (1) stagger-NONE rule: `make_VerticalGridAspect` with no grid
  is MIRRORED as decided, except an item whose stagger is NONE is SPECIFIED.
  Otherwise the `expression` scenarios failed: before, the aspect was
  FROM_COMP (not mirror), so `check_vertical_stagger_consistency` saw a
  NONE-stagger input; as MIRRORED it was skipped and the expression crashed
  in `BasicVerticalGrid%get_coordinate_field`. Owner-confirmed
  2026-11-04; it uses `is_none()`, not the wildcard `==`; revert by deleting the stagger-NONE branch in
  `make_VerticalGridAspect` and teaching that check to treat a NONE-stagger
  mirror as resolved. (2) `make_VerticalGridAspect` lost its `component_geom`
  / `component_geom_id` arguments (they only fed FROM_COMP and the dead
  `geom` argument).
  Left over (3.39 dropped): stale comment `ExpressionClassAspect.F90:395-400`
  ("component vertical grid unconditionally overwrites every item's
  VerticalGridAspect"), for A4/D3; `mirror_vgrid` (and `mirror_geom`) stay
  ignored by legacy (recorded in `scenario-parity.md`). Later: design.md
  ~1028 still says the component grid overwrites every item.
  Non-ESSENTIAL check on NAG (`MAPL.generic.scenarios.pending` with
  `MAPL_SCENARIO_ONLY=statistics_real` / `history_1`): 22 failures each (11
  checks x graph and both modes), all in GraphBuilder ("materialization
  reached with no import units", "MethodGraphNode: argument name already
  declared"); none from `set_geometry`, `GeomAspect` or `VerticalGridAspect`.
  These two were already failing with those messages before 3.38 (see the
  earlier pending-scenario notes), so no latent failure from the now-returned
  `rc` surfaced.
  B3-WildcardFieldOnly (tasks.md 3.30): done. Only FIELD exports are
  Wildcard members, filtered in `resolve_wildcard_destination` and
  `wildcard_import_has_match` through one predicate
  (`is_wildcard_member_shape`, on `var_spec%itemType`); one debug line per
  skipped export and one warning when every match was skipped (import
  unresolved, as for zero matches). Delta spec added
  (`specs/graph/graph-builder/spec.md`, MODIFIED wildcard requirement).
  Two graph-only tests; `build_wildcard_hierarchy` gained `add_non_field`.
  `history_wildcard` unchanged. ESSENTIAL 69/69 on NAG and gfortran. No
  deviations. The warning text is not asserted by a test.
  B3-ConnectionIntentCheck (tasks.md 3.29): done. Parser asserts in
  `parse_connections` (`parse_connections.F90`); `parse_connections` public
  for testing; twelve parser tests. Deviation: the internal
  `parse_connection` became a subroutine (`class(Connection), allocatable,
  intent(out)`) because the function form assigned an unallocated
  polymorphic result on the failure path (latent bug; gfortran SIGSEGV).
  ESSENTIAL 69/69 on NAG and gfortran. Failure messages name the endpoints.
  B3-RulesReview (tasks.md 3.26): done; owner decisions recorded,
  including the R2 follow-up (`docs/graph/rules-review.md` §4a, design.md
  A24, docs only, `Plan-Task: B3-RulesReview`). R1's doc corrections (no
  adopt-on-connect rule; legacy connects only the CLASS aspect) are done
  in the same commit; R13 is folded into 3.27; follow-ups are 3.28-3.36.
  Whether the rules matrix replaces design §5.1 is still undecided (not
  blocking).
  B3-QuantityType, B3-Conservation, B3-Normalization (tasks.md 3.10, done as
  one commit with three trailers): see adding-a-characteristic.md
  "QuantityType, Conservation and Normalization as implemented". The three
  characteristics gain `write_info` (`quantity_type_metadata=`,
  `conservation_metadata=`, `normalization_metadata=` through
  `MAPL_FieldSet` / `MAPL_FieldBundleSet`; either phase; SPECIFIED only; not
  structural, not per-alias, no FieldDelta). Factories 14-16 insert for FIELD,
  BRACKET, VECTOR, VECTORBRACKET and FIELDBUNDLE only (design §4.3; before
  this every shape got an entry); their `populate_characteristics` block and
  the three kinds/types in `GraphBuilder_Advertising_smod.F90` are gone.
  Owner Q9: each factory uses the declared `VariableSpec` value, not legacy's
  fixed UNKNOWN / CONSERVE_NONE / NORMALIZE_NONE scale 1 (parity difference
  with no current user: no keyword or YAML key sets these fields yet;
  field-dictionary defaults are future work). An undeclared quantity type is
  SPECIFIED `QUANTITY_UNKNOWN`; "unknown matches anything" and `BASIS_NONE`
  stay value rules in the subclass, not status. Conservation and
  Normalization are MIRRORED when their metadata `is_mirror()` (factory
  interprets it); both are in `requests_mirror()`, and only the post-loop
  check in `make_characteristics` catches a mirror on an export because the
  fields are assigned after `make_VariableSpec`. A mismatch was
  `BUILD_TRANSFORM_UNSUPPORTED` here; B3-IncompatibleSweep (tasks.md 3.22)
  moved five of the six (all but StandardName) to INCOMPATIBLE together.
  Legacy absent-means-mirror: none reaches the graph (`make_*Aspect` always
  build non-mirror aspects; `QuantityTypeAspect()` and `NormalizationAspect()`
  with no argument are mirrors, `ConservationAspect()` is not). Legacy
  `connect_to_export` copies the export's value onto the import
  unconditionally; the graph compares and does not copy. For QuantityType
  this is a real difference, left open and referred to the owner: the value
  wildcards (`QUANTITY_UNKNOWN`, `BASIS_NONE`) let an import that declares
  nothing match a more specific export without being equal; legacy then
  leaves the import holding the export's quantity type and basis, the graph
  leaves it SPECIFIED UNKNOWN. No visible effect today: legacy
  `make_QuantityTypeAspect` ignores declarations (Q9), so in legacy both
  sides are UNKNOWN, and nothing sets these fields yet. Legacy
  `QuantityTypeAspect` validate (dimensions, molecular weight)
  has no counterpart: `VariableSpec` declares neither (unfinished, left).
  Remaining GraphBuilder references, for B4: the three kinds in
  `merged_item_for_link` (`_ConnectionResolution_smod`) and the
  `GraphBuilder.F90` use list. Production path: `populate_one` calls only
  `sync_per_alias_to_info`, so these field-wide kinds are written only by
  tests until B4. Tests: `Test_StateItemDescriptor` (write both phases,
  mirrored skipped, bundle; the old "kind without write_info fails" test now
  uses a test-only kind), `Test_VariableSpecCharacteristics` (defaults,
  declared values, mirror on import, post-loop export rejection with full
  message, shape sets), `Test_StatusResolution` (value wildcards are not
  status; mirror adopt; a State handle makes `write_info` fail; the
  per-alias sync skips the three kinds). The two
  `..._mirror_instances_do_not_need_extension` tests are kept: the
  constructors still accept mirror metadata and `needs_extension_for` relies
  on the metadata `operator(==)` mirror rule. Deviation: none from tasks.md 3.10. Design
  §4.3's EXPRESSION row now says these kinds are added by 3.12. ESSENTIAL
  green on NAG and gfortran (65/65). `MAPL.generic.scenarios.pending` on NAG:
  Tests run: 373, Failures: 114, Errors: 0, the same five scenarios as the
  known baseline.
  B3-RestartMode (tasks.md 3.11, done): see adding-a-characteristic.md
  "RestartMode as implemented". New kind `RESTART_MODE_CHARACTERISTIC_KIND`
  (16) and `RestartModeCharacteristic` (never compared; `build_transform`
  unsupported; `write_info` writes `restart_mode=` through `MAPL_FieldSet`
  on a Field, any other handle fails). Factory 13: FIELD only; an undeclared
  mode is SPECIFIED `RESTART_REQUIRED` and always written (a mode set
  directly with `MAPL_FieldSet` on a spec that declares none is overwritten;
  documented limitation). Per-alias writes: `StateItemCharacteristic%is_per_alias()`
  (default false; true for RestartMode and LongName) and
  `StateItemDescriptor%sync_per_alias_to_info` (per-alias entries, Field
  handles only); `populate_one` calls it in place of its direct
  `MAPL_FieldSet(restart_mode=)`, so GraphBuilder names no kind. LongName is
  now written from `populate_one` for the first time: an UNCHECKED import
  alias writes nothing (it reads `unknown` under graph-only until B4
  materialization writes the owning field; legacy supplies the export's
  through `connect_to_export`); a bundle is not
  per-alias (design §3.4). `declared_restart_mode` and its advertise block
  are removed. Deviations: none from tasks.md 3.11. Not done (as agreed):
  INTERNAL items are still not populated by the graph (gap until B4); no
  `restart_mode` YAML key. Design §4.3's EXPRESSION row already lists
  RestartMode "when their B3 tasks arrive"; left to B3-Expression (3.12).
  ESSENTIAL green on NAG and gfortran (65/65). `MAPL.generic.scenarios.pending`
  on NAG: Tests run: 373, Failures: 114, Errors: 0, the same five scenarios
  as the known baseline.
  B3-LongName (tasks.md 3.13, done): see adding-a-characteristic.md "LongName
  as implemented". New kind `LONG_NAME_CHARACTERISTIC_KIND` (15) and
  `LongNameCharacteristic` (never compared; `build_transform` unsupported;
  `write_info` writes `long_name=` through `MAPL_FieldSet` /
  `MAPL_FieldBundleSet`, UNCHECKED writes nothing). Factory 12 inserts it for
  FIELD and EXPRESSION only (legacy passes `long_name` to no other class
  aspect). Default chain: declared (even empty), then the field dictionary
  (`make_VariableSpec`), then the StandardName value on every intent but IMPORT (owner decision;
  legacy has no such fallback); an
  import with none is UNCHECKED and takes the export's, which reproduces
  legacy `connect_to_export` `mirror_name` (the one legacy
  "absent means inherit" case). `MAPL_STRING_MIRROR` is stripped into
  `long_name_mirror` by `make_VariableSpec` and added to `requests_mirror()`,
  so the generic A21 checks reject it on exports. Added the `parse_var_specs`
  YAML test for `standard_name: "<mirror>"` and `long_name: "<mirror>"`.
  Design §3.4/§4.3/§5/§5.1 amended (bundle long_name is bundle-wide; EXPRESSION
  row gains LONG_NAME). Deviation: none from tasks.md; it landed without
  `is_per_alias()` and was wired into `populate_one` by B3-RestartMode
  (3.11). ESSENTIAL green on NAG and gfortran (65/65).
  `MAPL.generic.scenarios.pending` on NAG compared with a clean HEAD
  worktree configured Release like `nag/` (Rule 1, removed afterward): both
  fail 114 of 373 checks in the same five scenarios (`history_1`,
  `service_service`, `service_with_options`, `statistics`,
  `statistics_real`), with identical failure messages (sorted logs differ
  only in timings).
  B3-ItemShape: done (`Plan-Task: B3-ItemShape`). Every shape gets a
  SPECIFIED ItemShape from a `make_characteristics` factory (with
  `state_item_variant` if declared); the advertise block is gone. Shape
  matrix per design §4.3: error cells return `BUILD_TRANSFORM_INCOMPATIBLE`;
  VECTORBRACKET -> VECTOR is UNSUPPORTED and every other VECTORBRACKET
  pair is INCOMPATIBLE, as in legacy (VECTORBRACKET -> VECTORBRACKET used
  to match and is now a connection conflict). EXPRESSION -> FIELD is a
  match cell (owner, Q2: evaluation is per consumer; a differing consumer
  shows up as a non-shape mismatch until B3-Expression). No standard-name
  check across BRACKET -> FIELD (legacy PERMISSIVE behavior).
  `get_extension_order` per §4.2 (non-time-dependent BRACKET only, Q4;
  STATE and WILDCARD empty, Q5), applied in `resolve_characteristics`
  with the order re-queried after the ITEM_SHAPE step; full tip
  replacement stays B4. `GraphStateItem%ordering` and its table are
  deleted here (tasks.md 4.3). `write_info` writes only a declared
  variant, and `fieldBundleType=` in STRUCTURAL only. Not done:
  `declared_item_type` and its `GraphBuilder` gates (§6.1 `shape()`
  accessor); the direct setter calls in `CompositeStateMaterialization`
  and `GeomProxyField`. ESSENTIAL green on NAG and gfortran (65/65);
  warnings and the 114 PENDING failures match a clean HEAD baseline.
  B3-IncompatiblePair (A23): `BUILD_TRANSFORM_INCOMPATIBLE` and
  `report_build_transform_incompatible(rc)` sit beside the UNSUPPORTED
  versions; `HorizontalDimsSpecCharacteristic%build_transform` returns it
  both ways (no other kind changed). `find_or_build_extension_chain` and
  `build_chain` gain an optional `conflict` argument holding
  `incompatible <KIND>: export <signature>, import <signature>`, built from
  `to_string()` and `get_signature()`; a caller that omits it still gets the
  kind in `unsupported_characteristic`, so the Geometry and callback sites
  are unchanged. `graphbuilder_resolve_connections` gains an optional
  `connection_conflicts` vector, and the connect hook logs it as
  "connection conflict", apart from "no registered extension provider".
  Decision: incompatible pairs have their own report and descriptor status
  conflicts were left in `unsupported_characteristics`, so no existing
  behavior changed. Owner (review): keep the two channels separate for now;
  the Geometry hooks and callback wiring may keep reporting an incompatible
  pair as unsupported until those sites are retired. Nothing calls `_FAIL`,
  and a test shows the component's other connection still wires. No
  `GraphBuilder*.F90` names a kind. Tests: `Test_StateItemCharacteristicSubclasses`
  (renamed to `..._incompatible`), `Test_ExtensionResolution` (with and
  without `conflict`), `Test_GraphBuilder_ConnectionResolution`
  (`test_incompatible_dims_is_conflict_others_resolve`).
  ESSENTIAL green on NAG and gfortran (65/65); PENDING
  `MAPL.generic.scenarios.pending` on NAG fails the same 114 checks with
  identical messages as a clean HEAD worktree (Release, removed afterward).
  B3-IncompatibleSweep (tasks.md 3.22): Attributes, UngriddedDims,
  QuantityType, Conservation and Normalization call
  `report_build_transform_incompatible`. The criterion is "legacy
  rejects every mismatch of two non-mirror values, and no reachable case
  converts data", not "`supports_conversion` false": QuantityType,
  Conservation and Normalization have `supports_conversion_general` true,
  but `supports_conversion_specific` is true only for matching pairs, and
  the time-dependent branch of `StateItemAspect%can_connect_to` is
  unreachable for them (no caller sets the flag; it would yield only a
  `NullTransform` or `_FAIL`). They must be revisited if a time-dependent
  quantity type, conservation or normalization is ever introduced (C3 list;
  adding-a-characteristic.md). StandardName is OUT of the sweep (owner,
  after review): it fails the criterion, since legacy
  `StandardNameAspect%matches` connects two different names with a warning
  in the default PERMISSIVE `ValidationMode` (the export's name wins) and
  rejects them only in STRICT mode. It stays `BUILD_TRANSFORM_UNSUPPORTED`
  until B3-StandardNameValidation (tasks.md 3.27). Five small edits, no shared base
  default (a default would make future kinds incompatible by accident).
  Tests: five `..._reports_incompatible` in
  `Test_StateItemCharacteristicSubclasses` (StandardName still asserts
  UNSUPPORTED); `test_incompatible_ungridded_dims_is_conflict` shows the
  `connection_conflicts` report is kind-agnostic (D6); the StandardName
  connection test is unchanged. In shadow mode a conflict is still a
  warning. Docs: CONVENTIONS.md, ARCHITECTURE.md, GLOSSARY.md,
  adding-a-characteristic.md, design.md A23, tasks.md 3.10/3.22, and the
  `standard_name_and_ungridded_dims_mismatch` fixture description,
  expectations comment and scenario-naming.md (stale expected-exception
  claim). ESSENTIAL green on NAG and gfortran (65/65); PENDING on NAG:
  `Tests run: 373, Failures: 114, Errors: 0`, same five scenarios
  (history_1, service_service, service_with_options, statistics,
  statistics_real); its warnings name only VERTICAL_GRID, GEOMETRY and
  EXPRESSION, so no message changed and no clean baseline was needed.
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
  B3-ShapeIntentRules (tasks.md 3.28, done): `intent_rule_violation` adds
  `a bracket is not valid on an import`, `a vector bracket is not valid on
  an import` and `a wildcard is only valid on an import` (each `: <name>`),
  enforced in both `make_VariableSpec` and the backstop. INTERNAL bracket
  stays allowed. Scenario grep: no BRACKET/VECTORBRACKET import and no
  non-import WILDCARD in tests, scenarios or gridcomps (the one wildcard,
  `history_wildcard`, is an import). Test changes: `make_spec(WILDCARD)`
  now defaults to IMPORT; the bracket shape loops skip their import half.
  ESSENTIAL (69/69 both) ran after the stack rebuild and the
  harness-comm-leak scenarios sharding.
  B3-InternalNoMirror (tasks.md 3.25, done): A21 generalized, in
  VariableSpec only. `intent_rule_violation` allows a mirror request only on
  an IMPORT or an EXPRESSION export, and an EXPRESSION only as an export;
  `make_VariableSpec` and the backstop `check_intent_rules` both call it
  (the graph needs no check of its own). New messages: `mirror is only valid
  on an import or an expression export: <name>` and `an expression is only
  valid on an export: <name>`. Three tests that built an EXPRESSION with
  IMPORT intent now use EXPORT, and one that declared an EXPRESSION import
  on purpose was deleted. Legacy `make_GeomAspect`/`make_VerticalGridAspect`
  MIRRORED defaults occur on statistics-gridcomp internals; owner decided to
  leave them (design.md Q13).
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
  map copy in `check_no_mirrored_entry` (now `check_intent_rules`) as is.
  The rest is folded into tasks.md 3.20 (owner): stale
  per-kind "error on an export" text in `make_characteristics.F90` header
  comments and `adding-a-characteristic.md` (VerticalStagger,
  VerticalOrientation, StringMirror, HorizontalDimsSpec sections), and
  `requests_mirror()` constructor tests for typekind, horizontal dims,
  ungridded dims, stagger and alignment, plus post-loop tests for typekind
  and ungridded dims. Non-ESSENTIAL tests and non-test configs: E1-intel.
  B3-DimsZLegacy (tasks.md 3.20, done; legacy layer change, A22):
  `GeomAspect%matches` returns true when both sides' `horizontal_dims_spec`
  is NONE, before any geom or geom_id comparison; a MIRROR spec still goes
  through `dims_specs_agree`. Checked: for two NONE items `matches` is true,
  so `can_connect_to` is true and `needs_extension_for` is false, so
  `make_extension` calls no `make_transform` and no regrid is built;
  `connect_to_export` only copies geom_id/geom, nothing more needed. The
  `dims_z_field` scenario now gives `comp_src` (12x13) and `comp_dst` (6x7)
  their own `geometry:` (no `vertical_grid` in the children, which inherit
  the cap's; a grid declared per child gets its own id and would mismatch
  VERTICAL_GRID); expectations unchanged and it passes in all three
  graph modes. New `Test_Aspects` cases: two NONE GeomAspects with different
  geoms match, connect and need no extension; NONE vs GEOM does not match
  (different or same geom); a MIRROR spec still compares geoms. Review
  follow-ups (separate from the legacy change): the six
  `make_VariableSpec` rejection tests now check `status /= 0`; new
  `make_VariableSpec` export rejection tests for `MAPL_TYPEKIND_MIRROR`,
  `HORIZONTAL_DIMS_MIRROR`, a mirror `ungridded_dims`, `VERTICAL_STAGGER_MIRROR`
  and `vertical_alignment='mirror'`, an accept test for a plain export, and
  post-loop rejections for typekind and ungridded dims (full message with the
  kind's `to_string()`, `status=1`); stale per-kind "error on an export" text
  replaced by a pointer to the generic rule in `make_characteristics.F90`
  (HorizontalDimsSpec, StandardName, VerticalStagger, VerticalOrientation
  factory headers; the long line rewrapped) and `adding-a-characteristic.md`.
  The 3.17 deviation sentence is removed. The NONE shortcut also covers a
  NONE export with no geom (tested; it needs no geom extension). Latent: a
  time-dependent `GeomAspect` still forces an extension for two NONE items;
  no production caller sets it, so it is not handled here. PENDING on NAG compared with a
  clean HEAD worktree configured `Release` like `nag/` (Rule 1): both fail
  114 checks in the same five scenarios (`history_1`, `service_service`,
  `service_with_options`, `statistics`, `statistics_real`).
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

**B3-SkipLegacyCouplers** (owner, 2026-11-09; merge blocker). ifx Debug
(`-fpe0`) traps "floating invalid" in `ConvertUnitsTransform` in four
scenarios that set `graph_owns_user_states` and have a legacy units
coupler (`invalidate`, `units_mismatch_run_phase`,
`units_and_typekind_mismatch`, `grandparent_units_run_phase`); NAG and
gfortran compute a NaN silently. Cause (the owner's bucy agent): with the
switch on, A writes into the graph's Field, while the legacy coupler's
input aliases the legacy payload, which nobody writes and which still holds
the FieldFill signaling-NaN sentinel. Owner decision (option a): skip the
legacy import and export couplers in `OuterMetaComponent/run_user.F90`
when `graph_populates_user_states()`; aliasing or copying into the legacy
payload (options b, c) were rejected. The author first verifies that the
graph delivers the data in that mode. The owner confirms ifx on bucy.
Owner commits.
- **Status:** done (a23ad61d0, owner commit; NAG and gfortran Debug
  ESSENTIAL 69/69). Both coupler loops in `run_user.F90` and the
  import-coupler initialize loop in `initialize_user.F90` are skipped when
  `graph_populates_user_states()`. Verified on NAG by tracing: only the
  graph-mode legacy conversions disappear, and the graph
  `UnitsConverterTransform` still delivers `B import T = 5000.`.
  Regression test `check_legacy_couplers` (`Test_Scenarios.pf`): for the
  four units scenarios in graph-owns mode, each legacy coupler's
  `import[1]` and `export[1]` still hold the FieldFill sentinel (compared
  by bits); it fails on all four with the guard reverted. Owner's ifx Debug
  confirmation on bucy pending (2026-11-10). Found on the way:
  `expression_per_consumer_r8` passes on NAG with the switch on; revisit
  its pending flag and parity row. Follow-up idea (reviewer): warn when
  graph-owns mode meets an item class the graph does not materialize (M3),
  which today would be dropped silently.

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
- Split (owner, 2026-11-06) into `B4a-TipReplacement` (tasks.md 4.1),
  `B4b-DescriptorMaterialize` (4.2, test-first) and `B4c-D6Grep` (4.4,
  after B4b); see the model table.
- B4a status: done (tasks.md 4.1). `covered_kinds(tip, goal)` is a
  separate type-bound method (default own kind); `merged_item_for_link`
  deleted; extension nodes carry the tip; `intermediate_node_ids` is per
  step. NAG and gfortran ESSENTIAL 69/69.
- B4b status: implemented (tasks.md 4.2); extension and export fields
  are built from the node's descriptor (empty-create + STRUCTURAL sync +
  `MAPL_FieldEmptyComplete`); all `get_item_<kind>` getters except
  `get_item_expression` (B4c) are gone. NAG and gfortran ESSENTIAL 69/69.
- B4c status: done (tasks.md 4.4); B4 done (4.1-4.4). D6 grep shows only
  base-class names. `StateItemDescriptor%get_expression` and
  `add_expression_result_defaults` (SPECIFIED GEOM for an expression
  result, called
  from `build_expression_evaluation`) replace `get_item_expression` and
  the materialization default. NAG and gfortran ESSENTIAL 69/69.
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
- **Status:** done (ESSENTIAL 69/69 NAG and gfortran).

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
- Owner (2026-11-03): split into C1-geom, then C1-vertical; worktree
  alongside the B3 main line, opus author as a background subagent,
  sonnet review. The preflight showed end-to-end wiring needs B4 4.1
  (tip/goal descriptors, covered kinds) and 4.2 (links created on the
  tip's geometry; materialization gate on outside tests), so each part
  splits (owner):
  - C1-geom-a (now, worktree): a graph regrid Transform duplicating
    `RegridTransform` logic, field only; it asks `RegridderManager` for the
    regridder lazily and rebuilds it when the geoms change (legacy
    `update_transform`), keeping that lookup inside the Transform;
    regridder-param getters on `GeometryCharacteristic` and the legacy
    both-sides rule; isolated tests regridding real fields between two
    grids. No `GraphBuilder*.F90` edits.
  - C1-geom-b (after 4.1/4.2): `GeometryCharacteristic%build_transform`
    (unsupported unless both sides hold an `ESMF_Geom`); `step_kind` in
    `Test_Scenarios.pf` learns geometry (and vertical, orientation);
    chain + value checks; flip `regrid`, `regrid_r8`, and `propagate_geom`
    if it passes (it has no regrid). Decide whether "both sides declare a
    regridder param" stays a `_FAIL` (as legacy) or becomes INCOMPATIBLE.
  - C1-vertical-a / -b: same split. Fixed-levels and basic grids only;
    a model grid (PLE/ZLE coordinate, legacy
    `get_coordinate_field_with_coupler` through `StateRegistry%extend`)
    reports unsupported, and coordinate-as-input-port is a later task.
    Owner: the hardest coupling area, with many players; question the
    owner's decisions and drive it from use cases. A stagger-only
    mismatch on the same grid stays unsupported (legacy connects it with
    `NullTransform`); edge-to-center will likely be needed eventually,
    center-to-edge less so. Add an orientation-only scenario (shared
    parent grid, downward export, upward import, profile check);
    `vertical_alignment_same_grid` is really a grid mismatch and its
    constant value cannot detect a flip.
  - Open C3 design question (owner): with time-varying geometry, route
    handles (and possibly regridders) may need to be graph nodes with
    revisions; C1 keeps `RegridderManager` behind the Transform so that
    change stays local. Owner (2026-11-04): ultimately the Transform's
    same-geom test (today the MAPL geom id) should use the geometry
    node's identity and revision.
  - Doc drift to fix in C1: ARCHITECTURE.md ~294-296 (geometry mismatches
    are reported unsupported, not `_FAIL`), scenario-parity.md M1/M2
    (~53-60, ~147-160), comments at `GraphBuilder_Geometry_smod.F90`
    ~421, ~497 and `Test_ExtensionResolution.pf:310` once
    `build_transform` can succeed.
- **Status:** C1-geom-a done. `HorizontalRegridTransform`
  (graph/transforms/, field only) looks up its regridder in
  `RegridderManager` lazily at the first `compute` and again when a
  bound field's geom (by MAPL geom id) or typekind changes (legacy
  `update_transform` checks only geoms, by ESMF alias).
  `GeometryCharacteristic` has `get/has_regridder_param` and
  `select_regridder_param(goal)` (legacy both-sides rule; both
  declaring one is a `_FAIL`, as legacy). Isolated tests:
  `Test_HorizontalRegridTransform.pf`, `Test_GeometryRegridderParam.pf`.
  Integrated normalization is not duplicated. The Transform requires
  geom-manager geoms (a MAPL geom id on both fields; compute fails
  otherwise), which C1-geom-b's chain links must use. Left for
  C1-geom-b: build the Transform in `build_transform` (calling
  `select_regridder_param`), wiring, scenario flips, remaining doc drift
  (scenario-parity.md M1/M2, `GraphBuilder_Geometry_smod.F90` ~421/~497,
  `Test_ExtensionResolution.pf:310`, `adding-a-characteristic.md` ~344).
  Owner kept the typekind refetch (rare, e.g. ExtData collections of one
  variable in two precisions, but correctness matters), the geom-id
  comparison and the no-id failure (2026-11-04).
  C1-geom-b done (90686236d). Owner decisions (2026-11-08). Both sides declaring a regridder
  param: report a per-connection incompatible pair, not an abort
  ("Accepted recommendation: b. It gives a precise per-connection error
  and fixes the chain-sharing gap. Legacy's abort stays in place until
  legacy goes away."). Geometry mismatch on non-Field items: regrid
  bundles later with a bundle-level Transform that loops over members,
  as legacy does; no per-member nodes ("It keeps legacy's numerics
  (vector rotation, regrid once per bracket refresh) and the graph's rule
  that a node is an advertised item. Per-member nodes cost a design
  change for no gain the regridder does not already give. For C1-geom-b
  now, that means non-Field items report "unsupported", through an
  optional item-kind argument that only geometry reads. I add C1-geom-c
  (bundle regrid, including bracket and vector) to the plan after
  C1-geom-b."). Follow-up: C1-geom-c. Known limit until C1-geom-c-a
  (landed c1dd7cbed; VECTOR and BRACKET now regrid, FIELDBUNDLE and
  VECTORBRACKET still do not): a geometry
  mismatch on a FIELDBUNDLE, BRACKET, VECTOR or VECTORBRACKET item reports
  unsupported GEOMETRY (legacy regrids it in hybrid mode). The regrid
  value check was probed (regrid output overwritten -> the value check
  fails in GRAPH mode for regrid and regrid_r8); it uses constant fields,
  so it guards that the regrid runs, not interpolation accuracy (the
  isolated HorizontalRegridTransform tests cover that). The vertical and orientation step kinds are unexercised
  until C1-vertical.
  C1-vertical-a done (03933d948). C1-vertical-a done. VerticalColumnRegridTransform and
  VerticalFlipTransform (graph/transforms/) with isolated tests in
  Test_VerticalColumnRegridTransform.pf; no characteristic, GraphBuilder or
  scenario changes (all C1-vertical-b). Owner decisions (2026-11-08):
  (1) "build each field's column coordinate in the field's own storage
  order (reversed when its alignment differs from its grid's direction),
  then regrid": done. The legacy experiment showed legacy is wrong when
  alignment differs from the grid's direction: it flips each side's
  levels by alignment, which assumes levels are stored in alignment
  order, but they are stored in grid order. On a 3-level UP grid
  [1000,500,100] hPa to [800,300] hPa with T = 2p+1, a source stored DOWN
  gave [521,1501] instead of [1601,601], and a destination stored DOWN
  gave [1601,601] instead of [601,1601]. Graph gives the correct values
  (parity row added by meta). (2) "basic grids: legacy rule
  (BasicVerticalGrid%matches ...)": the Transform copies or flips for
  matching grids and fails for basic grids with different counts; the
  connect-time INCOMPATIBLE is -b. (3) "reads vgrid, vert_staggerloc,
  vert_alignment from its bound Fields at compute": done, no
  build_transform interface change. (4) "copy the end value and log a
  warning once per weight build ... No Debug abort": done through a new
  optional allow_out_of_range argument on compute_linear_map (smallest
  change; the legacy caller is unchanged). (5) "convert export levels to
  the import's units with udunits if cheap ... else fail": it was cheap
  (one get_converter per weight build); inconvertible units fail.
  (6) "get_column_coordinate ... non-deferred, default reports
  unsupported; FixedLevelsVerticalGrid overrides": done; the Transform
  neither selects on FixedLevels nor uses AspectMap/GeomAspect. (7) "R4
  only ... export the message": vertical_regrid_r8_message(name) is
  public. (8) "conservative ... reports unsupported": a
  CONSERVATIVE-method Transform fails at compute when a real regrid is
  needed; on matching grids it copies or flips, as legacy (meta decision
  2026-11-08). (9) "Orientation-only
  step COPIES now": VerticalFlipTransform copies. Model grids (PLE/ZLE)
  fail: they supply no column coordinate. The legacy direction bug needs
  numeric levels on both sides, so it shows only in regrids between
  fixed-level grids, whose coordinate comes back in grid order (basic
  equal-count grids copy or flip in both legacy and graph). Model-grid
  coordinate fields arrive in the field's own order, where legacy's flip
  should be right; that is from reading the code, not verified by a run. The failure
  tests that assert only status /= 0 (model grid, invalid alignment, EDGE,
  inconvertible units, different dimensions, R8, basic different counts)
  were each run alone and fail with their intended message. A stagger-only mismatch on the
  same grid fails. Left for C1-vertical-b: build_transform for
  VerticalGrid and VerticalOrientation, covered kinds, step_kind
  in Test_Scenarios.pf, the connect-time R8 warning and basic-count
  INCOMPATIBLE, scenario flips, and doc drift (ARCHITECTURE.md ~168,
  ~229, ~407; VerticalOrientationCharacteristic.F90:18 header).
  C1-vertical-b: not started. C1-geom-c-a done (c1dd7cbed, owner commit):
  `HorizontalBundleRegridTransform` regrids a VECTOR (one rotated
  `Regridder%regrid(fb_in, fb_out)` call, exporter basis kept) or a
  BRACKET (only when `bracket_updated`, and on the first compute or after a
  regridder, geom, typekind or member-set change whatever the flag says;
  interpolation weights copied) and creates the output members at compute
  by cloning the input members. `GeometryCharacteristic` dispatches on the
  tip item class (FIELD: Field regrid; VECTOR, BRACKET: bundle regrid;
  others unsupported). A chain link takes its declared item class from its
  ITEM_SHAPE entry (EXPRESSION gives FIELD). Owner-approved GraphBuilder
  exception: `materialize_extension_chain` refuses a link whose item class
  is not FIELD (`unsupported_item_class:<class>`). No bundle payload, no
  scenario change. VECTORBRACKET stays unsupported (follow-up with tests).
  NAG and gfortran 69/69. C1-geom-c-b: done (d9238c088, below).
  Nit for C1-geom-c-b: `Test_HorizontalBundleRegridTransform.pf:21` imports
  `MAPL_FIELDBUNDLETYPE_BASIC` and `MAPL_FIELDBUNDLETYPE_VECTORBRACKET`
  unused.
  - Owner decisions for C1-geom-c-b (2026-11-10, from its preflight,
    `~/.opencode/plan/reports/C1-geom-c-b-preflight.md`): (1) `vector_1`
    stays as it is (its vertical mismatch needs C1-vertical-b); add a new
    scenario `vector_regrid` with equal basic vertical grids and a
    `chain: [geometry]` expectation. (2) A vector with a units or
    precision mismatch must work, as in legacy (`ConvertUnitsTransform`
    and `CopyTransform` convert bundles member by member): the graph
    `UnitsConverterTransform` and `TypeKindConverterTransform` get a
    bundle branch in this task, creating output members at compute like
    the bundle regrid Transform; `vector_regrid` gets a units case.
    Brackets need nothing: they are time-interpolated to a Field first.
    (3) The `extension-reuse` delta spec goes in the active
    `characteristic-separation-of-concerns` change. (4) Vertical regrid of
    bundles (VECTOR, BRACKET; legacy `VerticalRegridTransform`
    `process_fieldbundle`) is part of C1-vertical-b, which flips
    `vector_1`. (5) New task `C2-bundle-exports` (C2.x).
  - C1-geom-c-b **Status:** done (d9238c088, owner commit). Extension
    links whose declared item class is VECTOR or BRACKET are materialized
    as memberless bundles (`ExtensionMaterialization`); exact-match and
    chain-input exports stay FIELD-only (`check_materializable`
    `admit_bundles` only for chain links). Shared `BundleMemberSupport`
    (member creation at compute, output typekind/units, basis and
    weights) used by the bundle regrid, units and typekind Transforms;
    units and typekind convert bundles member by member. New ready
    scenarios `vector_regrid` (`[geometry]`, `[geometry, units]`) and
    `bracket_regrid` (`[geometry, item_shape]`), masked passes; a trace
    confirmed the graph Transforms run. `ProtoBracketGridComp` sets
    weights only (members not filled). Delta spec
    `extension-reuse` in the active change. Owner confirmed (2026-11-10):
    an unaliased VECTOR/BRACKET chain input is refused at resolve time
    until `C2-bundle-exports` (with `graph_owns_user_states` on,
    `vector_regrid` reports `unsupported_item_class:VECTOR`). Found:
    equal basic vertical grids mismatch in the graph (manager grid ids)
    but match in legacy (`BasicVerticalGrid%matches`, layer count);
    parity row added; owner: fix in C1-vertical-b, or a small task first
    if C1-vertical-b is far off. NAG and gfortran 69/69. Sonnet review
    accepted after two fix rounds.
  - Owner (2026-11-08, C1-geom-b): both sides declaring a regridder param
    is INCOMPATIBLE (A23), not a `_FAIL`; the param is part of the
    geometry signature, so imports with different params do not share a
    chain. A geometry mismatch on a non-Field item (bundle, bracket,
    vector) reports unsupported until C1-geom-c. Per-member regrid nodes
    were considered and rejected: they would make composite members
    addressable (against D6), members are often unknown at advertise, and
    vector components must be regridded together (basis rotation).
  - C1-geom-c (after C1-geom-b; owner 2026-11-08): bundle-level horizontal
    regrid, one Transform per bundle looping over members, as legacy
    `Regridder%regrid(fb_in, fb_out)`: plain bundles, VECTOR (rotation),
    BRACKET/VECTORBRACKET (regrid only when `bracket_updated`, then time
    interpolation). The graph must create bundle-shaped extension items.
    Preflight and owner decisions (2026-11-09; report
    `~/.opencode/plan/reports/C1-geom-c-preflight.md`, accepted
    recommendations): (1) shapes: VECTOR and BRACKET (to FIELD) only;
    VECTORBRACKET (needs a graph vector time-interpolation Transform and
    `shape_outcome` CONVERT) is a follow-up; plain FIELDBUNDLE only if a
    client needs it. (2) Members of a bundle extension item: the regrid
    Transform creates (and resizes if the input count changes) the output
    members at compute, cloning each input member onto the destination
    geom; no member count in the descriptor. (3) A regridded VECTOR keeps
    the exporter's basis (legacy); the importer's basis (b) or basis as a
    compared characteristic (c) stays an open question against the
    bundle-extension item (M3). (4) The bundle export gets no graph
    payload in this task: `vector_1` flips only after `C2-alias-nonfield`.
    Split: `C1-geom-c-a` (Transform for VECTOR and BRACKET, dispatch in
    `geometry_build_transform_for_item` by item shape, the chain link
    records the tip's item class, isolated tests, flip the two "bundle
    geometry mismatch is unsupported" tests; no materialization, no
    scenario flip; sonnet author, sonnet review, owner commits because it
    adds a legacy/graph difference row) and `C1-geom-c-b` (after
    `C2-alias-nonfield`: bundle materialization, member creation, a
    bracket test gridcomp, flip `vector_1`, delta spec for the
    `extension-reuse` requirement that a VECTOR reports an explicit
    failure).
  - Owner (2026-11-08, C1-vertical-a): (1) a field whose alignment differs
    from its grid's direction gets its column coordinate in its own
    storage order before regridding (legacy reverses levels that are in
    grid order, giving wrong values; difference row in scenario-parity);
    (2) basic grids: legacy rule, same level count is the same grid (an
    orientation-only difference gets a flip), different counts are
    INCOMPATIBLE at connect; (3) Transforms read vgrid, stagger and
    alignment from their bound Fields at compute (no `build_transform`
    interface change); (4) import levels outside the export's range copy
    the end value (as Release legacy) with one warning per weight build,
    no Debug abort; (5) fixed levels in different units are converted
    (udunits), unconvertible units fail; (6) numeric levels through
    `VerticalGrid%get_column_coordinate` (infrastructure; default
    unsupported, fixed levels override); (7) R8 is not supported: a
    connect-time warning (C1-vertical-b) and a run-time failure name it;
    (8) conservative and normalization stay out (model grid task);
    (9) the orientation-only step copies for now. A pointer view
    (`a(:,:,n:1:-1)` with `ESMF_DATACOPY_REFERENCE`) is deferred; its
    first step is an ESMF test. Owner: it may matter for CTM performance,
    since CTMs often use coordinates reversed from the dynamics.
  - Doc task (owner, 2026-11-08): correct `docs/mapl3/vertical-alignment.md`
    (its upward/downward examples contradict the code and design Q7; it
    documents a `coordinate_direction` key that nothing parses). Done
    (73a1e8a8c). Owner answers (2026-11-09): (1) a `fixed_levels` grid
    derives its direction from the numbers alone (increasing = DOWN), so
    a height grid `[0, 100, 500]` gets DOWN although its index increases
    going up; the owner chose that the direction should depend on
    `physical_dimension` (height and altitude: increasing = UP). That
    changes legacy and graph alike (`FixedLevelsVerticalGrid`
    `derive_direction`); an explicit owner exception to "no legacy-behavior
    fixes". New task `C1-vertical-direction` (below); the doc keeps
    describing the current rule until it lands. (2) Comparing only the two
    resolved directions once the grids match is intended (legacy and
    graph agree). (3) The fixture `vertical_alignment_same_grid` declares
    two separate grids (two grid ids), so it tests a regrid, not a
    same-grid flip: D3 item (below).
  - `C1-vertical-direction` (owner 2026-11-09, answer b): derive a
    `fixed_levels` grid's direction from `physical_dimension` as well as the
    numbers: pressure and depth keep increasing = DOWN; height and altitude
    get increasing = UP. Change `derive_direction`
    (`FixedLevelsVerticalGrid.F90` ~83-103) once, so legacy and graph agree;
    the fixed-levels scenarios and any height-based fixture need checking
    (the C1-vertical-a Transform reads the direction from the grid, so no
    Transform change expected). Needs a preflight first (which fixtures and
    ExtData/GEOS configurations declare height levels; what `layer` and
    unknown dimensions should do); update `docs/mapl3/vertical-alignment.md`
    in the same task. Owner commits. Before C1-vertical-b.
  - **Status:** done (ffc978344, owner commit; NAG and gfortran Debug
    ESSENTIAL 69/69). `derive_direction` takes `physical_dimension`
    (exact, case-sensitive match; unallocated = numbers rule); 15 grid
    tests and 2 column-regrid tests. Owner (2026-11-09, answer a): a
    `with_grid` pair between a height fixed-levels grid (now UP) and a basic
    or model grid of the same level count (DOWN by default) now reverses
    instead of copying; accepted and documented in
    `docs/mapl3/vertical-alignment.md`; a client that hits it sets an
    explicit `direction:` or `vertical_alignment:`. No config in the repo
    uses that case. The D3 fixture item below is now unblocked.

**C3. Time-dependent geometry (reminder; prerequisite for A10's
epoch-dependent ExtData exports and moving nested grids).** Today nothing
marks a geom time-dependent, so the graph ignores the case. Before any
code sets it, this task must: (1) give BRACKET/VECTORBRACKET's
`get_extension_order` the legacy time-dependent branch (ITEM_SHAPE before
GEOM, i.e. time-interpolate then regrid; design.md §4.2 table and Q4);
(2) handle the DimsZLegacy latent case (a time-dependent `GeomAspect`
forces a geom extension for two `dims: z` items); (3) revisit QuantityType,
Conservation and Normalization: their `build_transform` reports INCOMPATIBLE
because legacy's time-dependent branch of `StateItemAspect%can_connect_to`
(which ignores `matches`) is unreachable for them, and a time-dependent
quantity type, conservation or normalization would make it reachable;
(4) make `set_shared_characteristic` also reset the step feeding a changed
import (it resets only downstream steps, so a sampler's regrid would keep
its old weights; found by 3.14). Without (1)-(3), the wrong order runs
silently.
- **Status:** not started; not scheduled until time-dependent geometry
  has a user.

**C2.x. One task per remaining root cause** (label `C2-<short-name>`),
from "Verified root causes" in `docs/graph/scenario-parity.md`. Each task
starts from that diagnosis, fixes production source, and flips the
affected scenarios to ready (unless masked: M3).
- `C2-geom-push` (G1): geometry pushed to children from a parent with
  no geometry.
- `C2-alias-owner` (R1): export aliasing searches the wrong component's
  state for propagated (qualified) exports.
  - **Status:** done (72fa0ece0, owner commit). `try_alias_export_field`
    takes the export port name; `find_alias_owner` walks the export-port
    tables down from the source component (a plain last-slash split is
    wrong for exports propagated through two or more ancestors). Five
    unit tests in `Test_GraphBuilderMaterializeGate.pf`. `statistics`
    flipped to ready as a masked pass (owner 2026-11-10, a): it already
    passed without the fix; the earlier failure was not traced. NAG and
    gfortran ESSENTIAL 69/69. Two opus reviews (first: rework for the
    depth >= 2 split; second: accept with follow-ups, below).
  - Review follow-ups (unscheduled nits; fold into a later C2 or D3
    task): `find_export_node` copies the `PortIdNodeIdMap` per child
    (use a `get_export_ports` pointer); a duplicate X/T port name across
    two children picks the first child silently; no test for a `<self>`
    source combined with a propagated port; no test for a null owner.
- `C2-qualified-src`: a qualified source component (`MID/SRC2`) still
  fails in `component_for` in the Wildcard and callback paths
  (`GraphBuilder_ConnectionResolution_smod.F90` ~117 and ~2229,
  `GraphBuilder.F90` ~972); use `resolve_qualified_component` as
  `C2-alias-owner` did. Found by the C2-alias-owner author; no scenario
  known to need it.
  - **Status:** not started
- `C2-alias-nonfield` (R2): export aliasing assumes a Field; VECTOR and
  SERVICE return rc=13. After `C2-alias-owner` (same procedure).
  - **Status:** done (3bb936230, owner commit). `try_alias_export_field`
    branches on the state item kind: a FieldBundle (VECTOR, SERVICE,
    BRACKET) is aliased with `MAPL_NamedAlias`; any other kind (nested
    State) is not found, without failure. `service_service` and
    `service_with_options` flipped to ready (masked; red before: 24
    failures each, rc=13). `vector_1` already passed before the fix and is
    not flipped; `statistics_real` is now blocked only by P1. Meta
    decisions, owner confirmed 2026-11-10: an expression input whose
    declared item type is not FIELD is reported `unsupported_item_class`
    at resolve time (`ExpressionTransform` reads only Fields; the reviewer
    found every input refused this way was refused before); a created
    FIELD placeholder whose legacy item is a bundle is destroyed. Seven
    unit tests. NAG and gfortran ESSENTIAL 69/69. Opus review accepted
    after one fix round.
- `C2-bundle-exports` (owner 2026-11-10; after `C1-geom-c-b`): the graph
  creates VECTOR, SERVICE and BRACKET export bundles itself, with their
  members as the legacy class aspects' `create`/`allocate` do, so they are
  not silently missing when the graph owns the user states (today the
  graph only aliases the bundle legacy made). The last gap before vectors
  work in every mode; independent of `C4-b`.
  - **Status:** not started
- Owner decisions for `C2-alias-owner` and `C2-alias-nonfield`
  (2026-11-09, from the preflight): one author, two commits (R1, then R2),
  separate reviews. A SERVICE pair is aliased like any bundle (no special
  case). Masked scenarios are flipped to ready with a "masked" note and a
  parity row: `statistics` after R1; `service_service` and
  `service_with_options` after R2. `vector_1` does not flip (it needs
  `C1-geom-c-b`); `statistics_real` stays pending (`C2-runphase-args`).
  Record the failing results first (`MAPL_SCENARIO_PENDING=1
  MAPL_SCENARIO_ONLY=<name>`). `ARCHITECTURE.md` ~524 wrongly says SERVICE
  is excluded (meta's doc batch).
- `C2-runphase-args` (P1): run-phase method-node arguments keyed by
  short name collide when an import and export share a name; key by
  (intent, short name). The diagnosis's name-mangling experiment broke
  `Test_GraphBuilder_RunPhaseNode`; don't repeat it.
- Note for whichever task takes up `statistics`/`statistics_real` (found
  by B3-InternalNoMirror; unchecked): the statistics gridcomp has no geom,
  so its internals (`sum_*`, `counts_*`, ...) get DEFERRED geometry and
  vertical grid in the graph, and only the component sets them later, on
  the field. Check whether that also fails freeze, besides the current
  "resolve_connections step failed".
- **Status:** not started

**C4. Hierarchy-wide forward network** (label `C4-ForwardNetwork`;
openspec change `hierarchy-wide-forward-network`, approved by the owner
2026-11-08). One forward `DependencyNetwork` per hierarchy replaces each
component's default network; reuse (extension chains, expression
evaluations, collections) and transform port bindings become
hierarchy-wide; a pull names only the item; each component pulls its own
imports before its run and advances its exports after it; callback get
edges join the forward network; write-back networks (callback put, inout
return) stay separate and are never pulled. Owner decisions are in the
change's design.md Context; the inout borrower stays as is (deferred,
kept only in case callback states prove unsuitable). It held 3.42,
C1-vertical-b and C1-geom-c; it has landed. Follow-up (`C4-b`, not yet
an openspec change): state population without the descendant walk,
merged geometry push/pull, removal of the `get_child_component_graph`
carve-out. Owner (2026-11-08) on geometry in C4-b: each component gets
explicit import and export geometry items. FROM_PARENT: an import geometry
wired to the parent's (connection made by the parent) and an export
geometry offered to its children. FROM_CHILD: an import wired to that
child, and an export offered to the other children and to its parent.
PROVIDER: an export only, which the parent may connect. Components keep
the legacy declarations; the explicit items are built from them. C4's skip
of the geometry push into a component's own provider child is temporary
until then.
- **Status:** done (commit trailer `Plan-Task: C4-ForwardNetwork`). NAG
  and gfortran Debug ESSENTIAL 69/69; `history_1` now `graph_ready`.
  Deviations: geometry push skips the component's own FROM_CHILD
  provider (pull + push would be a cycle in the one forward network;
  temporary: C4-b redesigns geometry as explicit import/export geoms;
  owner decision 2026-11-08); a second connection into one import is a
  `connection_conflicts` entry naming the item, skipped while the pass
  continues, for ordinary and inout-borrower pairings (legacy: last
  wins; parity row added);
  `CallbackMethodBinding` keeps only `get_put_network_id`. See the
  change's design.md D2 and D5.

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
- B3-Geometry's harvest (`GraphBuilder_Geometry_smod.F90` ~238-242) reads
  geoms through ESMF directly to avoid a suspected `MAPL_FieldGet(geom=)`
  crash on fields whose Info is not populated. Batch a did not reproduce
  the crash (regression tests kept): switch harvest back to the MAPL
  getter and check ESSENTIAL.
- Fixture `vertical_alignment_same_grid` (`superstructure/generic/tests/
  scenarios/vertical_alignment_same_grid/{A,B}.yaml`) declares two separate
  `vertical_grid` blocks, so the two components get different grid ids and the
  scenario tests a regrid, not a same-grid flip (owner, 2026-11-09). Make the
  two components share a parent grid (default `from_parent` geometry) so it
  tests the flip. The fixtures `vertical_alignment_{same_grid,regrid,with_grid}`
  also still write `coordinate_direction:` and `class: fixed_levels`, which
  nothing reads (the code reads `grid_type`; the direction comes from the
  levels), and their `expectations.yaml` comments call decreasing levels
  "downward". Fix those too, after `C1-vertical-direction`.
- `superstructure/generic/tests/Test_FixedLevelsVerticalGrid.pf` is in no
  `CMakeLists.txt` and uses a removed constructor; it is a dead copy of
  `infrastructure/vertical/vertical_grid/tests/Test_FixedLevelsVerticalGrid.pf`
  (found by the D5-AllocatableResults-B review). Delete it.
- Stale `!wdb fixme deleteme` comments about `is_mirror()` in
  `make_transform.F90` (~98) and `VerticalGridAspect.F90` (~418) (found by
  C1-vertical-direction). Check and remove.
- Scenario fixtures with `/`-qualified short names (e.g. `A/E_A1`) have
  no field-dictionary entries, so every run logs "not found in field
  dictionary (as an alias)" (`VariableSpec.F90` alias lookup; the lookup
  is correct). Add the entries to the scenario field dictionary, keyed as
  looked up; ask the owner if qualified vs. bare key is unclear.
- A4 leftovers (found by 3.10; comment-only): source comments cite
  `REQ-CHAR-0xx` IDs that do not exist in `openspec/specs/` (e.g.
  `GeometryCharacteristic.F90`, `CharacteristicStatus.F90`): map each to
  a real ID or delete it. Characteristic module headers still cite
  "merge-characteristic-families task ..." and "(extension-reuse change
  design.md ...)"; `graph/tests/Test_StateItemCharacteristicSubclasses.pf`'s
  header cites `openspec/changes/` paths, as do section banners in
  `tests/Test_GraphBuilder_ConnectionResolution.pf` (e.g. ~89,
  `phase-7c-wildcard-dispatch-wiring`). Remove them (AGENTS.md "Source
  comments").
  Done (dfca610d4, 136 files, comment-only; `check-comment-only.py
  superstructure enums apps` exits 0; NAG and gfortran ESSENTIAL 69/69).
  Every `REQ-*` citation that is not in `openspec/specs/` was removed (only
  `REQ-VIZ-002/003` exist), plus `openspec/changes` paths, task and design
  numbers and change-history wording in source comments. Left for a
  follow-up sweep (`a4-leftovers-b`, not yet scheduled): 68 source lines
  citing `design.md`/`tasks.md`/`proposal.md` without a path
  (`VariableSpec.F90`, `ExpressionTransform.F90`, `ExpressionClassAspect.F90`,
  `VectorClassAspect.F90`, `OuterMetaComponent.F90`, ...), 37 "Task N.M"
  lines in `.pf` headers, remaining "Phase N" mentions
  (`initialize_advertise.F90`, `initialize_accept_transfer.F90`,
  `NormalizationAspect.F90`, `Transform.F90`, `GraphScenarioBootstrap.F90`,
  the `Test_GraphBuilder_*.pf` headers), `REQ-MTH-011` inside two assertion
  message strings (`GraphBuilder_LifecycleHooks_smod.F90:289`,
  `Test_GraphBuilder_ConvergenceCheck.pf:281`; a code change), non-source
  `openspec/changes` paths (`tools/graph_visualization/render_graph.py:14`,
  which also cites the nonexistent `REQ-VIZ-009`, and its README;
  `graph-scenarios/*/description.md`; `superstructure/generic/AGENTS.md`
  lines 35 and 51), and source comments that are wrong after 3.42 or
  after C1-vertical-a (`GraphOwnsUserStates.F90:5` "Outer states stay
  legacy's either way"; `initialize_accept_transfer.F90:62-72`;
  `VerticalStaggerCharacteristic.F90:18`, `VerticalGridCharacteristic.F90:273`
  and `VerticalOrientationCharacteristic.F90:18` say no vertical Transform
  exists; fold the last three into C1-vertical-b).
  Second sweep done (e202491c4, 79 files, comment and text only;
  `check-comment-only.py superstructure enums apps` exits 0; NAG and gfortran
  ESSENTIAL 69/69): the `design.md`/`tasks.md`/`proposal.md` citations,
  "Task N.M" and "Phase N" banners, openspec change names, and the
  wrong or stale comments listed above (`GraphOwnsUserStates.F90`,
  `initialize_accept_transfer.F90`, `units_mismatch_run_phase/description.md`)
  are gone; the `openspec/changes` paths in `tools/graph_visualization` and
  the graph-scenario descriptions are gone. Remaining: `design.md Risks` at
  `GeometryCharacteristic.F90:78` (C1-geom-c-a's file), `REQ-MTH-011` inside
  the two assertion-message strings (a code change),
  `superstructure/generic/AGENTS.md` lines 35 and 51 (pointer to the
  archive), and `capability/name` spec-directory references such as
  `generic/field-name-propagation` and `graph/extension-reuse`, which name
  directories that exist under `openspec/specs/`.
- `infrastructure/field/FieldInfo.F90:~305` (`FieldInfoGetInternal`):
  reading conservation asserts the key exists, while quantity-type and
  normalization reads fall back to a mirror object. Make the three
  consistent; ask the owner which behavior is right (missing key = error,
  or = mirror) if it is not obvious from the callers.
- `parse_connections`: `all_unsatisfied: false` still makes a
  MatchConnection; `all_unsatisfied` silently ignores the other keys; YAML
  intents are case-sensitive while `ComponentSpec%reexport` uses
  `to_lower`. Ask the owner which of these are bugs.
- `docs/graph/ARCHITECTURE.md:~163`: stale line counts.
- A general "key not valid for this item type" check in `parse_var_specs`
  across the field-metadata keys (e.g. `quantity_type` on an expression is
  silently ignored today; owner, 3.35 preflight). Needs a design rule
  first; ask the owner for its scope.
- `FieldBundleInfo.F90:~122-131`: `MAPL_FieldBundleGet` with
  `quantity_type_metadata=` returns a non-mirror `QUANTITY_UNKNOWN` for an
  absent key, while the Field version returns a mirror. Make them agree;
  check the bundle's conservation and normalization keys too.
- Two geom-id counters (3.38 preflight; investigate, then fix if real):
  item geom ids come from the module-level `GeomIdManager` (`GeomId.F90`,
  used by `GeometrySpec` and `parse_var_specs`), while `GeomManager`
  (`GeomManager.F90`, `add_mapl_geom.F90`) numbers geoms from a separate
  counter. Equal numbers for different grids would make
  `GeomAspect%matches` and the graph's id comparison report a false match.
  Likely moot once `geom_id` is retired (Later, below).
- C1 preflight findings with no owner: `VariableSpec%get_regrid_param`
  (field-dictionary defaulting, `VariableSpec.F90` ~573-593) has no
  caller; `VerticalGridAspect%regrid_method` is never set from a
  `VariableSpec`; `VerticalGrid%get_coordinate_field` takes a legacy
  `AspectMap` as `class(*)` (the vertical-grid API leaks the legacy
  layer); `RouteHandleKey` and the graph route-handle role are unused
  outside tests (and the key lacks typekind).
- `RegridderManager%get_regridder` (`infrastructure/regridder_mgr/
  RegridderManager.F90` ~103-137) returns a pointer into a gFTL vector that
  a later `push_back` may move (C1-geom-a review; legacy has the same
  pattern; unverified).
- Not D3, recorded here (harness-comm-leak, `2d137acb3`):
  `pfunit/ESMF_TestCase.F90` creates one gridded component with its own
  VM per test and never destroys it, so every test leaks communicators.
  `MAPL.generic.scenarios` now runs as four ctest shards
  (`MAPL_SCENARIO_SHARD`) to stay under MPICH's 2048 limit. Restoring
  `ESMF_GridCompDestroy` fails inside ESMF's VM-shutdown garbage
  collection ("Not a valid pointer to ESMF Component object") on both
  compilers. Cause: two ESMF bugs in VM match-table slot reuse (stale
  garbage entries; a freed slot keeps its VM local id with a null key),
  reported to the ESMF team with standalone reproducers (owner,
  2026-11-03). When an ESMF release fixes both: restore the destroy in
  `clearInternalState`, rerun ESSENTIAL on both compilers, and decide
  whether to keep the shards. Fallback if more binaries hit the limit
  first: keep one gridded component per PET count and reuse it across
  tests (tried: NAG ESSENTIAL green, unsharded scenarios green).
- `graph/transforms/TransformGraphNode.F90` ~91: `allocate(transformer,
  source=)` plus the HierarchyGraph insert corrupts a `PortSpecMap` with
  2+ entries under gfortran (SIGBUS in deep_copy). `ExpressionTransform`
  works around it by answering port queries from its referenced names;
  fix the copy properly (e.g. ports in a vector) and drop the override.
- **Status:** batch a done (PR #5486, Weiyuan Jiang, `2d98aaa14`): stray
  `to_itemtype.F90` deleted; `FieldGet.F90` empty block removed (getter
  crash not reproduced; regression tests kept); `FieldBundleGet` now
  propagates `get_geom` errors (was dropped), with a test;
  `parse_var_specs.F90` indentation. NAG and gfortran Debug ESSENTIAL
  69/69. The other items remain.

**D4. NAG Debug green.** Owner rule (2026-11-05): every build is Debug,
on every compiler. NAG Debug (`-C=dangling`) aborts about 40 ESSENTIAL
tests at `d63770b13`, nearly all at the gFTL set/map iterator
("dangling pointer THIS%TREE"), plus `Test_Couplers.pf` ~825
(ASPECTS). Usual cause (owner): a missing TARGET on an intervening
dummy argument, which lets NAG pass a copy in and out. Find each site
from the NAG runtime messages and add TARGET (dummy or local) or
restructure; no behavior change. Main-line B tasks keep landing on
gfortran Debug 69/69 meanwhile; NAG ESSENTIAL is rechecked when D4
lands. Candidate for outsourcing (mechanical, off the critical path).
Label `D4-NagDangling`.
- **Status:** done. NAG Debug ESSENTIAL 69/69 and gfortran Debug 69/69.
  Sites (all a missing TARGET; NAG passed a copy): local `characteristics`
  in `VariableSpec/make_characteristics.F90`; `graph_validate` locals
  `producer_ids`/`state_item_ids`/`allowed_ids` in `ComponentGraph.F90`;
  `this` of `end_connection_pass` (interface in `OuterMetaComponent.F90`
  and `OuterMetaComponent/connections_resolved.F90`); local `spec` in two
  `Test_Couplers.pf` tests. Nothing remains.

**D5. Allocatable function results always allocated.** Owner rule
(2026-11-05): a function with an allocatable result allocates it on every
path (allocate at the top, or use a subroutine with intent(out));
polymorphic results: deallocate between two assignments (gfortran). The
ifx crash at `GraphBuilder_Freeze_smod` ~184 (`node%get_short_name()` on
a nameless node) is one case. Sites: survey of 2026-11-05 (meta's
reports, `allocatable-results-survey.md`). Two tasks (owner, 2026-11-06):
- `D5-AllocatableResults-A` (main line, after 3.23b lands and
  D4-NagDangling is in; before B4): sites unallocated on a normal path.
  `StateItemNode%get_short_name` allocates `''` and gains
  `has_short_name()`; callers that test `allocated()` use it
  (`GraphBuilder_Freeze_smod`, `GraphBuilder_Geometry_smod`,
  `ComponentGraph`); callers that assume a name (notably
  `GraphBuilder_LifecycleHooks_smod` ~168, which loops over all nodes) skip
  or handle nameless nodes. Same treatment for
  `ComponentGraph%get_export_port_name`, `qualify_exported_name`,
  `effective_source_name` (`GraphBuilder_ConnectionResolution_smod`),
  `ItemShapeCharacteristic` `bundle_type_of` (add `case default`), the
  four `RouteHandleKey` getters, and the test helpers that return
  unallocated names.
  - **Status:** done. NAG and gfortran Debug ESSENTIAL 69/69.
    - `StateItemNode`: `get_short_name` returns `''` when unset; new
      `has_short_name()`. Freeze and LifecycleHooks use `'<extension>'`
      for nameless nodes; Geometry returns unless `has_short_name()`.
    - `ComponentGraph%get_export_port_name` returns `''` for a non-export
      candidate; `qualify_exported_name` likewise; callers test
      `len(name) == 0`. `effective_source_name` already assigned on all
      paths; left alone.
    - `ItemShapeCharacteristic`: `bundle_type_of` is a subroutine with
      `intent(out)` allocatable (unallocated = not a bundle shape).
    - `RouteHandleKey`: mask getters return size 0 when unset;
      `get_extrapNumLevels`/`get_regridPoleNPnts` return 0 when unset
      and gain `has_*()`.
    - Test helper `find_export_name_in_graph` allocates `''`; asserts use
      `len(...) > 0`. Added tests for nameless node, unbound export port,
      unset RouteHandleKey getters.
- `D5-AllocatableResults-B` (outsourceable; legacy files only): about 25
  functions unallocated only on an error path (`_RC`/`_ASSERT`/`_FAIL`
  before the first allocation): `FieldDictionary` getters, `MAPL_Generic`
  and `OuterMetaComponent` name/checkpoint-dir getters, `StateRegistry`
  `get_specs`, `get_aspect_order` in the spec classes, vertical-grid
  factories, `make_ClassAspect`, `GeomAspect`/`ConservationAspect`
  `make_transform`. Eight results are polymorphic. No behavior change.
  - **Status:** done (c32f21792, PR #5496 by Purnendu, squash-merged
    2026-11-09; NAG and gfortran Debug ESSENTIAL 69/69 on meta's run).
    32 functions allocate a placeholder result first. Owner decisions
    (2026-11-09): the `VerticalGridFactory` wrappers allocate a
    `MirrorVerticalGrid` placeholder (option a); `Subgrid.F90` and the
    other sites the reviewer found go to `D5-AllocatableResults-C`.
- `D5-AllocatableResults-C` (owner, 2026-11-09; before legacy is
  dropped): the remaining error-path sites. `Subgrid.F90`
  `make_subgrids_from_num_grids` / `_from_bounds` (`_RC` before the
  allocate), substates, OpenMP-related allocatable results (the
  `OpenMP_Support` copies), the `create_grid_from_file_metadata` test, and
  the reviewer's candidates: `SharedIO.F90:307,349`,
  `HConfigAs.F90:252,275`, `make_EASEGeomSpec_from_hconfig.F90:114`,
  `LocStreamGeomFactory.F90:185`, `get_coordinates_dim.F90:20`,
  `VerticalCoordinate.F90:232`, `StateFilter.F90:139`,
  `HConfigMatch.F90:68`, `graph/ExtensionResolution.F90:101`. Vet each
  candidate before editing (some may already allocate on every path);
  re-examine the legacy ones in graph context first. Same rule as A and B.
  - **Status:** not started

Both apply the owner's rule only; other ifx problems go to the owner
(he leads ifx porting).

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

**E1-early. Intel run of the current HEAD (outsourced).** Same scope as
E1, on the HEAD at the time the task starts, so Intel surprises surface
before the end (owner, 2026-11-05). Report only; fixes become new tasks;
E1 still runs last. If ifx comes out clean, volunteers' second compiler
switches from `gcc15-stack` to `ifx-stack`.
- **Status:** not started

**E2-TeamWalkthrough** (owner, 2026-11-09; after the merge into
`develop`). A walkthrough of the graph implementation for the owner's
team, with a PowerPoint deck. Length and number of sessions are not
decided. Before authoring, meta asks the owner (dashboard): audience depth,
sessions and length, framing from legacy terms, slides only or a live
scenario demo, and the tool for the deck.
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
2. Under `/run-task` the meta session launches the reviewer when the
   author finishes, has the author fix findings that need no owner
   decision, and brings the owner one message: verdict, report, fixes,
   questions, and the files to commit. In an interactive task session
   the owner runs `/review-task <label>` there instead.
3. Owner decisions go back to the author (meta resumes the same author
   subagent). If the review says "rework needed", or the author session
   is very long, the author first writes its unfinished list into the
   task's Status line and the rework runs in a fresh author from the
   working tree. `last-review <label>` (or `-s <session>`) prints any
   subagent's last report.
4. Commit only when the owner says "commit": `git add` exactly the
   task's files (never `-a`; never the owner's untracked files), then
   `git commit -F .git/PLAN_TASK_MSG`. Whoever commits then deletes
   that file; a stale one would be mistaken for the next task's
   message.
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
| A6 | sonnet | sonnet | throw hook for the graph test executable; fix exposed tests |
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
| B3-DimsZLegacy | sonnet | sonnet (owner-run) | legacy GeomAspect change + scenario with two grids; stop if legacy still regrids; plus MirrorGeneric review follow-ups (docs/comments, tests) |
| B3-IncompatiblePair | sonnet | sonnet (pre-commit) | new build_transform outcome; conflict reporting stays kind-agnostic (D6) |
| B3-IncompatibleSweep | sonnet | sonnet | tasks.md 3.22; five kinds UNSUPPORTED -> INCOMPATIBLE at once (StandardName excluded) |
| B3-VerticalStagger | sonnet | opus | DEFERRED/harvest; fixture risk |
| B3-VerticalOrientation | sonnet | opus | resolve against grid without comparing it |
| B3-HorizontalDimsSpec | sonnet | opus | changes when Geometry is compared |
| B3-Expression | opus | gemini/gpt-sol | mechanical: Expression kind, factory applicability (design.md §4.3, Q3) |
| B3-ExpressionPlaceholder | opus | sonnet | tasks.md 3.23a; test-first (gpt-sol tests); export placeholder, names check, one evaluation per consumer |
| B3-ExpressionPerConsumer | opus | sonnet | tasks.md 3.23b; test-first (gpt-sol tests); precision/units per consumer, sharing by allowlist kinds (design.md §2.4) |
| B3-ExpressionNativeGrid | sonnet | opus | optimization, after B3-ExpressionPerConsumer |
| B3-InternalNoMirror | sonnet | opus | design.md Q13; reports mirror defaults to the owner before changing them |
| B3-RulesReview | opus | gemini/gpt-sol | read-only; report and proposals, no code; then owner session |
| B3-StandardNameValidation | opus | sonnet | design-bearing; owner settles the design (possibly in B3-RulesReview's session); includes the V3 switch (R13) |
| B3-ShapeIntentRules | haiku | sonnet | tasks.md 3.28; two rules in `intent_rule_violation` |
| B3-ConnectionIntentCheck | haiku | sonnet | tasks.md 3.29; parser assert, re-export exempt |
| B3-WildcardFieldOnly | sonnet | sonnet | tasks.md 3.30; filter in `resolve_wildcard_destination` |
| B3-OneExportPerImport | - | - | tasks.md 3.31; dropped |
| B3-GeomAProbe | sonnet | - | tasks.md 3.32; probe test only, report |
| B3-InternalReexport | sonnet | opus | tasks.md 3.33; after B4 |
| B3-FieldPerNode | sonnet | opus | tasks.md 3.41a; after 3.33; every node gets a created Field, INTERNAL populated |
| B3-ExportPortPopulation | sonnet | opus | tasks.md 3.41b; after 3.41a; state population walks export ports |
| B3-OuterStatePopulation | sonnet | sonnet | tasks.md 3.42; low priority; preflight done, one task; outer states from the graph, labelled names via a legacy_only marker |
| B3-VerticalOrientationSync | sonnet | sonnet | tasks.md 3.43; found by 3.41a; owner chose to skip an UNSPECIFIED direction in write_info |
| B3-AdvertisePopulation | sonnet | sonnet | tasks.md 3.44; found by 3.41b; graph populates user states at advertise when the switch is on; preflight first |
| B3-SkipLegacyCouplers | sonnet | sonnet | ifx Debug trap (merge blocker); skip legacy couplers in run_user when the graph owns the user states (owner option a, 2026-11-09); owner commits after bucy ifx check |
| B3-FillValue | sonnet | sonnet | tasks.md 3.34; with or after 4.2 |
| B3-QuantityTypeSuppliers | sonnet | sonnet | tasks.md 3.35; YAML key, keyword, harvest; before 3.27 |
| B3-QuantityTypeDeferred | opus | sonnet | tasks.md 3.36; factory status by intent, soft freeze rule, shares 3.27's switch; after 3.27 |
| B3-SetShared | volunteer | sonnet | tasks.md 3.14; outsourced (GitHub); characteristic only, rebuild is 3.40 |
| B3-SetSharedMaterialize | sonnet | sonnet | tasks.md 3.40; after 3.14 and B4 4.2 |
| B3-QuantityBasisUnsupported | sonnet | sonnet | tasks.md 3.37; basis-only mismatch UNSUPPORTED; before 3.36 |
| B3-GeomAItemWins | sonnet | sonnet | tasks.md 3.38; legacy fix: item geom/vgrid survive GEOM_A; re-enable 3.32 probes; after 3.37, before 3.27 |
| B3 others | sonnet | sonnet (self-review) | one at a time, after B3-Units lands |
| B4 | - | - | split (owner, 2026-11-06): B4a, B4b, B4c below |
| B4a-TipReplacement | sonnet | sonnet | tasks.md 4.1; mostly mechanical since extension nodes carry descriptors |
| B4b-DescriptorMaterialize | opus | opus | tasks.md 4.2; test-first (gpt-sol tests); every extension field's creation path |
| B4c-D6Grep | sonnet | sonnet | tasks.md 4.4; grep clean + CONVENTIONS.md; after B4b |
| B5 | sonnet (haiku?) | sonnet if haiku | |
| B3-GeomMirrorLegacy | - | - | tasks.md 3.39; dropped (no legacy fixes) |
| C1-geom-a | opus | sonnet | Transform + isolated tests; worktree; background author |
| C1-geom-b | opus | sonnet | wiring + scenarios; after B4 4.1/4.2 |
| C1-geom-c-a | sonnet | sonnet | bundle regrid Transform for VECTOR and BRACKET, chain link item class, isolated tests; owner commit (owner 2026-11-09; tier chosen by owner) |
| C1-geom-c-b | sonnet | sonnet | landed d9238c088; bundle materialization, member creation, units/typekind bundle branch, new vector_regrid scenario, bracket test gridcomp, extension-reuse delta spec |
| C1-vertical-direction | sonnet | sonnet | fixed_levels direction depends on physical_dimension (height/altitude increasing = UP), legacy and graph (owner exception 2026-11-09); landed ffc978344 |
| C1-vertical-a | opus | sonnet | after C1-geom-a; numerics duplication, orientation flip; worktree |
| C1-vertical-b | opus | sonnet | after C1-vertical-a and B4; includes bundles (VECTOR, BRACKET), flips vector_1; fix basic-grid id mismatch (parity row) |
| C4-ForwardNetwork | opus | sonnet | openspec hierarchy-wide-forward-network; test-first (gpt-sol tests); owner commit; landed |
| C4-b | opus | sonnet | follow-up: population, geometry merge, carve-out; needs its own openspec change |
| C2-geom-push | sonnet | opus | |
| C2-alias-owner | sonnet | opus | landed 72fa0ece0 |
| C2-alias-nonfield | sonnet | opus | landed 3bb936230 |
| C2-qualified-src | sonnet | sonnet | qualified src_comp in Wildcard/callback paths |
| C2-bundle-exports | sonnet | opus | after C1-geom-c-b: graph-created VECTOR/SERVICE/BRACKET export bundles |
| C2-runphase-args | sonnet | opus | changes method-node keying |
| D1 | sonnet (haiku?) | sonnet if haiku | `public ::` rule |
| D2 | sonnet | opus | lifecycle-phase ordering |
| D3-cleanup | sonnet | - | items are small and independent |
| D4-NagDangling | sonnet | - | mechanical; outsourceable |
| D5-AllocatableResults-A | sonnet | sonnet | main line after 3.23b and D4, before B4; callers' "absent" tests change |
| D5-AllocatableResults-B | volunteer | sonnet | outsourceable; legacy error-path sites, 8 polymorphic; landed (#5496) |
| D5-AllocatableResults-C | sonnet | sonnet | subgrids, substates, OpenMP results, reviewer's 11 candidates (vet first); before legacy is dropped |
| E1-intel | sonnet | - | run and classify only; remote build on bucy |
| E1-early | volunteer | meta (report check) | outsourced; run and classify only, current HEAD, ifx-stack on bucy |
| E2-TeamWalkthrough | opus | sonnet | after the develop merge; slide deck for the owner's team; scope questions first |

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
   `mapl-worktree baseline <name> --configure` does both (and links
   ESMA_cmake and GEOS_FieldDictionary); `mapl-worktree remove <name>`.
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
   In a worktree, `.git` is a file; the message goes in
   `$(git rev-parse --git-dir)/PLAN_TASK_MSG`.
   Never push, never rewrite earlier commits.
8. Line length: keep new or touched source and test lines to 80
   characters. Up to 120 is acceptable only for a compelling reason
   (e.g. an `_ASSERT` with a long message string). Rewrap existing
   longer lines you touch.

## Dependency summary

A0 -> A1 (harness) -> A2a -> A3; A1 (harness) -> A1-diagnosis -> C2.x;
A0 -> A4 -> {B1, D1}; A4d -> A5 -> A5b -> B3.x; A0 -> B0 -> B1 -> B2 -> B3.x -> {B4, B5};
B4 -> C1, B3-VerticalCoordinate -> C1 (vertical part), D1 -> D2;
C2-alias-owner -> C2-alias-nonfield. The C2 tasks need only A1-diagnosis
and may run before Phase B. A4 is independent of A1-A3 and B0 (they touch tests/docs, A4 touches
production comments), so their order is free; still one task at a time.
D3-cleanup has no dependencies. E1-intel runs after every other task
except C3, which waits for a user of time-dependent geometry
(B3-ItemShape -> C3).

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
- ctest -L ESSENTIAL green on NAG and gfortran (while iterating, build
  and run only the affected targets on NAG; ESSENTIAL once at the end)
- this task's Status recorded (tasks.md item for B3.x; see the header)
- only the docs the brief names updated (no ARCHITECTURE/GLOSSARY/
  rules-review edits; meta batches those)
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
