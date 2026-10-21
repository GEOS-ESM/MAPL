# Plan: Graph layer separation-of-concerns cleanup

> Canonical copy: `docs/graph/plans/separation-of-concerns.md` (in repo).
> Every task PR MUST update its own **Status** line (commit hash,
> deviations from this plan). Artifacts produced by earlier tasks
> (`docs/graph/CONVENTIONS.md`, `docs/graph/scenario-parity.md`, the B0
> openspec design, `docs/graph/adding-a-characteristic.md`, ...)
> **take precedence over this plan's summaries** once they exist. If the
> plan and the source disagree, stop and report rather than guess.

Status: agreed in discussion with tclune, 2026-10-21. Each task = one PR.
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
- Build: `module load nag-stack/7.2.41`; `cmake --build nag -j 8 --target build-tests`;
  `ctest --test-dir nag -L ESSENTIAL`. Then same with `gfortran-stack` / `gfortran`.
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
- **Status:** not started

**A1. Legacy scenarios under graph mode, gated.**
- Parameterize `tests/Test_Scenarios.pf` by graph mode
  (registry/graph/both). Add a `graph_ready` flag per scenario; only
  ready scenarios get graph/both parameters. ESSENTIAL stays green.
- Optional non-ESSENTIAL ctest target running pending scenarios.
- Deliverable: `docs/graph/scenario-parity.md` table: scenario,
  ready?, one-line root cause of failure. Expected causes include
  missing geometry transform (`regrid*`, `propagate_geom`). Flipping a
  flag is how later tasks show progress.
- **Status:** not started

**A2a..A2n. Graph-native scenario ports (~5 scenarios/PR).**
- Port legacy scenarios to `tests/graph-scenarios/` with improved
  import/export/standard names and full `expectations.yaml`. First PR:
  the existing 7 graph-scenarios (add `expectations.yaml`) + write
  `docs/graph/scenario-naming.md`. Later PRs follow it.
- Each scenario gets `description.md` (what it exercises).
- **Status:** not started

**A3. Shared expectations driver for graph scenarios.**
- Graph-scenario tests (`Test_GraphModeDemonstration.pf`,
  `Test_GraphEquivalenceHarness.pf`) consume `expectations.yaml` via the
  same checkers as `Test_Scenarios.pf`; add graph-specific checks
  (node count, transform chain kinds/order). Remove hand-written
  assertions now covered.
- **Status:** not started

**A4. Comment hygiene (before Phase B).** One PR per directory/file
group, in this order: `GraphBuilder*.F90`; `graph/`;
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
  each file; the result must be identical (attach the script output to the PR).
- Done also requires `grep -rn "openspec/changes" --include=*.F90` to be
  empty for the PR's directory.
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

**B3.x. One PR per characteristic.** For each: move to generic status,
implement `write_info`, add VariableSpec factory, remove its block from
`populate_characteristics`, remove any GraphBuilder references to it.
List: PhysicalUnits, TypeKind (finish), UngriddedDims, VerticalCoordinate,
VerticalStagger (new), Geometry, ItemShape (incl. D4 ordering),
StandardName, Attributes, QuantityType, Conservation, Normalization,
RestartMode (new), Expression (new), SetShared (check whether still needed).
Can run in parallel after B2. Produce `docs/graph/adding-a-characteristic.md`
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

**C2.x. One PR per remaining root cause** from A1's parity table.
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

Author = writes the PR. Reviewer = reviews diff against this plan / B0
design before merge. Short names (update here when models change):

- `opus` = `nasa_litellm/claude-opus-5.5`
- `sonnet` = `nasa_litellm/claude-sonnet-5-5` (fallbacks:
  `claude-sonnet-5`, then `claude-sonnet-4.6` for the most mechanical
  tasks B1/B5/D1)
- `haiku` = `nasa_litellm/claude-haiku-5-5` (provisional - see calibration)
- `gemini` = `nasa_litellm/gemini-3.1-pro-preview`
- `gpt-sol` = `nasa_litellm/gpt-5.6-sol`

Note: the LiteLLM proxy uses hyphenated ids for the 5.5 models
(`claude-sonnet-5-5`, `claude-haiku-5-5`). The proxy also lists
`gpt-6-sol/luna/astra` and `gemini-3.8-flash`; these are not
configured in opencode yet and have not been evaluated.

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
- **Status:** not started (benchmark kept outside the repo, so candidates can't see the answer key: `~/.opencode/plan/calibration/README.md`)

| Task | Author | Reviewer | Notes |
|---|---|---|---|
| A0 | sonnet | opus (spot-check) | gemini also viable (long-context audit) |
| A1 harness | sonnet | opus | |
| A1 diagnosis | opus | - | root-causing failing scenarios |
| A2a | opus | owner | sets naming conventions |
| A2b..n | sonnet | - | follow scenario-naming.md |
| A3 | sonnet | - | |
| A4 | sonnet | diff review always | judgment on which "why" to keep; haiku only after calibration |
| B0 | opus | owner + gemini/gpt-sol | cross-family review: "what did this miss vs. legacy aspects?" |
| B1 | sonnet | opus | escalate any compiler/gFTL crash |
| B2 | opus | gemini/gpt-sol | core semantics |
| B3 Units | opus | owner | template + adding-a-characteristic.md |
| B3 Geometry | opus (sonnet?) | opus if sonnet | geom-as-node, Info id |
| B3 ItemShape | opus | - | D4 ordering, Bracket rule |
| B3 others | sonnet | opus (sonnet? self-review if calibrated) | parallel after B3 Units lands |
| B4 | opus (sonnet?) | opus | materialization regressions likely |
| B5 | sonnet (haiku?) | sonnet if haiku | |
| C1 | opus | - | ESMF regrid/route handles |
| C2 triage | opus | - | assign each cause a tier |
| C2.x fixes | per triage | opus | |
| D1 | sonnet (haiku?) | sonnet if haiku | `public ::` rule |
| D2 | opus (sonnet?) | opus if sonnet | lifecycle-phase ordering |

`(sonnet?)`/`(haiku?)` = move to that model only if it passes calibration.
Always opus regardless: B0, B2, B3 Units, B3 ItemShape, A1 diagnosis,
A2a, C1 - errors there are replicated by every later task.

Light models (`haiku` until calibrated, `gemini-3.5-flash`,
`gpt-5.6-luna`): log triage / ctest summarization only. Non-Claude
models for Fortran authoring: trial first on D1 or one A4 directory
before promoting.

Quota strategy: run A0 + A1 harness (sonnet) now; reserve opus for B0.
If opus is scarce, stop after B2 + B3 Units - do not let sonnet
replicate an unreviewed template across B3.x.

### Rules for every agent prompt
1. Never call a failure "pre-existing" without reproducing it on a
   clean `git worktree` of HEAD.
2. Build/test NAG then gfortran; paste both `ctest -L ESSENTIAL`
   summary lines in the PR.
3. On ICE, nondeterministic segfault, or gFTL copy crash: stop and
   report; do not invent workarounds.
4. Do not modify files outside the task's stated scope without asking.
5. Follow `docs/graph/CONVENTIONS.md` (after A0).
6. Source comments: no `openspec/changes` paths, task numbers, or change
   history. Comments explain why, not when. Put change names in the
   commit message.

## Dependency summary

A0 -> A1 -> A2a -> A3; A0 -> A4 -> {B1, D1};
A0 -> B0 -> B1 -> B2 -> B3.x -> {B4, B5};
B4 -> C1, D1 -> D2; A1 -> C2.x. A4 can run in parallel with A1-A3 and B0
(they touch tests/docs, A4 touches production comments).

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

Branch/worktree: <name>. Scope: <optional narrowing / extra constraints>.

Done means:
- ctest -L ESSENTIAL green on NAG and gfortran (paste both summary lines)
- this task's Status line in the plan updated (commit, deviations)
- any doc your task owns created/updated
Stop and report rather than guess if the plan and source disagree.
```

For open-ended tasks (B0, A1 diagnosis) add a sentence of owner intent.
After B3 Units lands, B3.x prompts may add: "Follow
docs/graph/adding-a-characteristic.md; use the B3 Units PR as exemplar."
