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
| UNCHECKED | match | **error** | match |

- UNCHECKED = "always match, user beware" (bootstrapping MAPL2-era items
  that never quantified the axis; expression outputs).
- DEFERRED = owner supplies later (e.g. component-default geom at
  GEOM_B); must be SPECIFIED or UNCHECKED before freeze, else freeze fails.
- MIRROR resolution is generic: `allocate(c, source=partner_c)` + set
  SPECIFIED. No per-subclass MIRROR sentinels anywhere in the map.
- Subclass methods (`needs_extension_for`, `build_transform`,
  `write_info`, ...) are only ever called with both sides SPECIFIED; the
  base/descriptor enforces this.
- "Absent from map" means only "not applicable to this item's shape".

### D3. ESMF Info facade
- Every characteristic implements deferred `write_info(info, rc)`
  (reference characteristics write a node/registry id).
- `StateItemDescriptor%sync_to_info(handle, rc)`: one loop, called at
  materialization and whenever a DEFERRED value becomes SPECIFIED.
- Info is **write-only** during init: no graph code reads Info back.
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
constant or concrete `*Characteristic` type. Enforced by a scripted
check (grep in a ctest).

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
  `'vector_bracket'`, the latter does not).

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
- **Status:** not started

**A4. Comment hygiene (before Phase B).** One task per directory/file
group (labels A4a, A4b, ...), in this order: `GraphBuilder*.F90`; `graph/`;
`graph/characteristics/`; remaining `superstructure/generic/` files with
`openspec/changes` references. Rationale: ~33% of graph-layer lines
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
- Comment-only: no code, no whitespace reflow of code lines. Verify by
  stripping `!` comments and blank lines from old and new versions of
  each file; the result must be identical (put the script output in the commit message body).
- Done also requires `grep -rn "openspec/changes" --include=*.F90` to be
  empty for the task's directory.
- Must land before B1 and D1 (both rewrite these files).
- **Status:** not started

### Phase B - characteristic architecture

**B0. Openspec design change (no code).** Write
`openspec/changes/characteristic-separation-of-concerns/` covering
D1-D6. Must specifically study and document:
- `specs/ExpressionClassAspect.F90`: how expressions convert to a field
  then use field rules -> defines the EXPRESSION export-MIRROR allowlist
  entry and the expression shape's extension order.
- Legacy `get_aspect_order` for every ClassAspect -> table of required
  shape orders.
- Legacy Info consumers -> UNCHECKED Info policy.
- Inventory of every current `GraphStateItem` component and every
  `populate_characteristics` block -> target characteristic for each.
Review with owner before B1.
- **Status:** not started

**B1. Extract `StateItemDescriptor`.** Mechanical move of the
characteristics map and all `declared_*`/`ungridded_dims`/
`vertical_stagger` into a new type owned by `StateItemNode`. No
behavior change. `GraphStateItem` left as handle only.
- **Status:** not started

**B2. Generic status resolution.** Implement D2 in base class +
descriptor (including freeze check for DEFERRED and the EXPRESSION
allowlist). Convert TypeKind first (only real MIRROR user today:
StatisticsGridComp). Unit tests for every cell of the D2 table.
- **Status:** not started

**B3.x. One task per characteristic.** For each: move to generic status,
implement `write_info`, add VariableSpec factory, remove its block from
`populate_characteristics`, remove any GraphBuilder references to it.
Label `B3-<Name>` with the name as listed (exception: PhysicalUnits is
`B3-Units`). List: PhysicalUnits, TypeKind (finish), UngriddedDims, VerticalCoordinate,
VerticalStagger (new), Geometry, ItemShape (incl. D4 ordering),
StandardName, Attributes, QuantityType, Conservation, Normalization,
RestartMode (new), Expression (new), SetShared (check whether still needed).
Run one at a time after B2 (direct commits; no parallel agents). Produce `docs/graph/adding-a-characteristic.md`
in the first one; later ones refine it.
- **Status:** not started

**B4. Remove manual dispatch.** Delete `merged_item_for_link`'s kind
chain, `get_item_units`/`get_item_typekind`, scalar args of
`materialize_field_extension` (create from goal descriptor + chain
step instead), `GraphStateItem%ordering`/table. Add the D6 grep ctest.
- **Status:** not started

**B5. Translation in place.** `populate_characteristics` deleted;
advertise calls `VariableSpec%make_characteristics`.
- **Status:** not started

### Phase C - missing capability

**C1. Geometry/regrid transform** on the graph `Transform` interface
(duplicate legacy `RegridTransform` logic; don't delegate). Flip
`regrid*`/`propagate_geom` to ready in A1's table.
- **Status:** not started

**C2.x. One task per remaining root cause** (label `C2-<short-name>`) from A1's parity table.
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


## Model assignment

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
3. Fixes go into the working tree (owner asks the author session).
4. Owner commits: `git commit -aF .git/PLAN_TASK_MSG`, then
   `rm .git/PLAN_TASK_MSG`; or tells the author session "commit". (`-a`
   takes every modified tracked file, so check `git status` first.)
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
- **Status:** debugging replay done: `sonnet` (nasa_litellm claude-sonnet-5-5) passed, so the `(sonnet?)` rows moved to sonnet, with opus kept as reviewer. haiku (nasa_litellm) also passed the debugging replay but hedged one fix (kept the old path as a fallback); mechanical replay skipped by owner decision - try haiku on real tasks with review. Benchmark kept outside the repo, so candidates can't see the answer key: `~/.opencode/plan/calibration/README.md`.

| Task | Author | Reviewer | Notes |
|---|---|---|---|
| A0 | sonnet | opus (spot-check) | gemini also viable (long-context audit) |
| A1 harness | sonnet | opus | |
| A1-diagnosis | opus | - | root-causing failing scenarios |
| A2a | opus | owner | sets naming conventions |
| A2b..n | sonnet | - | follow scenario-naming.md |
| A3 | sonnet | - | |
| A4 | sonnet | diff review always | judgment on which "why" to keep; haiku only after calibration |
| B0 | opus | owner + gemini/gpt-sol | cross-family review: "what did this miss vs. legacy aspects?" |
| B1 | sonnet | opus | escalate any compiler/gFTL crash |
| B2 | opus | gemini/gpt-sol | core semantics |
| B3-Units | opus | owner | template + adding-a-characteristic.md |
| B3-Geometry | sonnet | opus | geom-as-node, Info id |
| B3-ItemShape | opus | - | D4 ordering, Bracket rule |
| B3 others | sonnet | sonnet (self-review) | one at a time, after B3-Units lands |
| B4 | sonnet | opus | materialization regressions likely |
| B5 | sonnet (haiku?) | sonnet if haiku | |
| C1 | opus | - | ESMF regrid/route handles |
| C2 triage | opus | - | assign each cause a tier |
| C2.x fixes | per triage | opus | |
| D1 | sonnet (haiku?) | sonnet if haiku | `public ::` rule |
| D2 | sonnet | opus | lifecycle-phase ordering |

`(sonnet?)`/`(haiku?)` = move to that model only if it passes calibration.
Always opus regardless: B0, B2, B3-Units, B3-ItemShape, A1 diagnosis,
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
   afterward with `git worktree remove`).
2. Build/test NAG then gfortran; put both `ctest -L ESSENTIAL`
   summary lines in the proposed commit message body.
3. On ICE, nondeterministic segfault, or gFTL copy crash: stop and
   report; do not invent workarounds.
4. Do not modify files outside the task's stated scope without asking.
5. Follow `docs/graph/CONVENTIONS.md` (after A0).
6. Source comments: no `openspec/changes` paths, task numbers, or change
   history. Comments explain why, not when. Put change names in the
   commit message.
7. Do not commit or stage anything (not even `git add -N`). Leave the
   task in the working tree on the integration branch, new files
   untracked, and write the proposed
   commit message (ending with the `Plan-Task:` trailer) to
   `.git/PLAN_TASK_MSG`. Commit only when the owner says so, and then
   only the task's files (never untracked files you didn't create).
   Never push, never rewrite earlier commits.

## Dependency summary

A0 -> A1 (harness) -> A2a -> A3; A1 (harness) -> A1-diagnosis -> C2.x;
A0 -> A4 -> {B1, D1}; A0 -> B0 -> B1 -> B2 -> B3.x -> {B4, B5};
B4 -> C1, D1 -> D2. A4 is independent of A1-A3 and B0 (they touch tests/docs, A4 touches
production comments), so their order is free; still one task at a time.

## Open questions (resolve in B0)
- UNCHECKED Info representation.
- Exact expression semantics (owner to refresh from ExpressionClassAspect).
- Whether `SetSharedCharacteristic` survives.
- Post-legacy cleanup tasks (delete aspects/transforms/registry,
  rename `StateItemCharacteristic*` if desired) - out of scope here.

## Prompt template for task agents

```
Task: <label, e.g. B3-UngriddedDims> from docs/graph/plans/separation-of-concerns.md.

Read first: that plan's header, "Context every task prompt should
include", "Rules for every agent prompt", and your task's section; then
any artifact your task depends on (these take precedence over the
plan's summary).

Scope: <optional narrowing / extra constraints>.

Done means:
- ctest -L ESSENTIAL green on NAG and gfortran
- this task's Status line in the plan updated (done/partial, deviations)
- any doc your task owns created/updated
- NOT committed or staged: changes left in the working tree (new
  files untracked), proposed commit message in `.git/PLAN_TASK_MSG`
  ending with `Plan-Task: <label>`, body containing both ctest summary
  lines and listing new files. Commit only when the owner says "commit".
Stop and report rather than guess if the plan and source disagree.
```

For open-ended tasks (B0, A1 diagnosis) add a sentence of owner intent.
After B3-Units lands, B3.x prompts may add: "Follow
docs/graph/adding-a-characteristic.md; use the commits from
`git log --grep='^Plan-Task: B3-Units' -p` as the exemplar."
