# MAPL3 Graph Architecture — Implementation Reference

**Audience:** coding agents and developers working on the graph-native
dependency architecture in this MAPL3 checkout.

**Relationship to other docs:**
- `docs/graph/spec/` — the design *specification* (README, numbered
  sections `01`-`20`). Normative, mostly stable, occasionally amended
  (`[RE-EVALUATED]`) when implementation reveals a design error.
- `openspec/specs/graph/` — machine-checkable requirement/scenario specs
  derived from the design spec, one directory per capability. Treat as
  the authoritative statement of *current, landed* behavior per
  capability.
- `openspec/changes/` — proposals for work not yet merged into
  `openspec/specs/`. `archive/` holds landed-and-folded-in changes;
  top-level entries (currently one: `hierarchy-wide-component-graph`)
  are active/in-flight.
- **This file** — a navigational layer on top of the above, written for
  quick orientation without re-reading the full spec tree. Where this
  file and the spec/openspec sources disagree, the sources win — this
  file may lag.

All facts below were verified by reading the actual source tree and
`git log`/`git diff` output on 2026-10-16, unless marked
**[ASSUMPTION]**. Task Group 10 (full regression pass + spec-doc
updates) was performed and verified on 2026-10-17 (see §2.2, §3's
header note, and §6 for what changed in this pass).

## 1. Module map (`superstructure/generic/`)

### 1.1 Graph-neutral core (`superstructure/generic/graph/`)

No dependency on `OuterComponent`/`StateRegistry` (hard rule,
`07-component-graph.md` REQ-CG-002). ~40 files; most relevant for
current work:

| File | Responsibility |
|---|---|
| `NodeId.F90`, `DependencyNetworkId.F90`, `PortId.F90` | Generated identity types (`IdTemplate.inc` instantiations) |
| `HierarchyId.F90` | Identity type for a whole component hierarchy (landed by `hierarchy-wide-component-graph`, Group 1) |
| `HierarchyGraph.F90` | The actual hierarchy-wide `NodeId→GraphNode` and `DependencyNetworkId→DependencyNetwork` maps + the one shared id-generator pair |
| `HierarchyResources.F90` | Container holding one `HierarchyGraph`, shaped to hold future shared resources |
| (private registry, inside `HierarchyGraph`/`HierarchyResources` module code) | `request_new_hierarchy()` / `get_hierarchy_resources(id)` / `release_hierarchy(id)` — keyed by `HierarchyId`, not a bare singleton (see design.md D1 in the active change) |
| `GraphNode.F90`, `BaseGraphNode.F90`, `StateItemNode.F90`, `OperationGraphNode.F90` | Node hierarchy |
| `GraphStateItem.F90` | Concrete payload type (Field/FieldBundle/State, at most one allocated) |
| `DependencyNetwork.F90` | Acyclic adjacency-list dependency storage |
| `ComponentGraph.F90` | Per-`OuterComponent` owner of resource index, ports, child-port bindings, lifecycle state — **node/network storage itself now lives in `HierarchyGraph`, reached via a stored `HierarchyId`** (post Group-1 refactor) |
| `TransformGraphNode.F90`, `Transform.F90`, `TransformBase.F90` | Transform nodes + named ports |
| `NodeRevision.F90`, `ComponentGraph_DemandDrivenUpdate.F90` | Revision stamps + demand-driven `update()` |
| `MethodGraphNode.F90`, `MethodInvocation*.F90`, `GridCompMethodInvocation.F90`, `StateMethodInvocation.F90`, `GridCompDriverPhaseInvoker.F90` | Method invocation layer |
| `CallbackInterface*.F90`, `CallbackMethodSpec.F90`, `CallbackStateBinding.F90`, `CallbackMethodBinding.F90` | Callback data model |
| `*Characteristic.F90` (Units, TypeKind, Geometry, Attributes, UngriddedDims, QuantityType, Conservation, Normalization, StandardName, VerticalCoordinate) + `CharacteristicStatus.F90`, `StateItemCharacteristicKind.F90` | Mismatch-detection/extension-chain axes |
| `UnitsConverterTransform.F90`, `TypeKindConverterTransform.F90` | Real executing transforms (only two axes with real conversion so far) |
| `RouteHandleKey.F90` | Semantic key for route-handle reuse |
| `CompositeStateMaterialization.F90` | Applies composite-member/variant tagging during materialization |
| `GraphMode.F90` | `registry` / `graph` / `both` 3-way selector (`mapl_GraphMode_mod`) |

### 1.2 Integration layer (`superstructure/generic/`, top level)

| File | Responsibility |
|---|---|
| `GraphBuilder.F90` | Public interface module for the whole integration layer. Declares the `GraphBuilder` type-bound-procedure table (stateless-per-call; every method takes the `OuterMetaComponent` explicitly) and all cross-submodule interfaces. **1550 lines** (grew during Group 6 work on this branch; proposal.md's stated goal is net reduction once proxy removal fully lands). |
| `GraphBuilder_Advertising_smod.F90` | Advertising phase: creates `StateItemNode`s from `VariableSpec`, recurses into composite members |
| `GraphBuilder_ConnectionResolution_smod.F90` | Ordinary connection resolution (`resolve_one`, `for_each_matching_import`), unsatisfied-import checks, and (new, this branch) `graphbuilder_unresolved_import_node_ids` / `graphbuilder_run_propagate_hook` — **1137 lines** |
| `GraphBuilder_Geometry_smod.F90` | Geometry push/pull hook (`run_geometry_hook`) |
| `GraphBuilder_CallbackWiring_smod.F90` | Wildcard/regex callback-argument wiring against the flattened qualified-export namespace |
| `GraphBuilder_LifecycleHooks_smod.F90` | Thin wrappers invoked from `OuterMetaComponent` lifecycle call sites (`run_advertise_hook`, `run_activate_hook`, `run_connect_hook`, `run_propagate_hook`) that log `GraphResolutionEntry`/`UnresolvedImportEntry` results |
| `GraphBuilder_PortsAndProxies_smod.F90` | Port-table helpers (proxy-creation code **already removed**, per Groups 3-5) |
| `GraphBuilder_RunPhaseNode_smod.F90`, `GraphBuilder_RunPhasePull_smod.F90` | `MethodGraphNode` construction + demand-driven pull re-trigger for a run phase |
| `GraphBuilder_StatePopulation_smod.F90` | Populates the real `ESMF_State` from graph-side `StateItemNode`s (`graph`/`both` mode only) |
| `GraphBuilder_Visualization_smod.F90` | DOT/JSON topology export |
| `GraphResolutionEntry.F90`, `GraphResolutionEntryVector.F90` | Structured resolution-report record type (replaces an earlier colon-concatenated-string convention) |
| `OuterMetaComponent.F90` + `OuterMetaComponent/*.F90` (62 files) | `OuterComponent`'s private-state type: owns a `HierarchyId`, a local `ComponentGraph`, child drivers, the legacy `StateRegistry`. `OuterMetaComponent/initialize_advertise.F90` is the lifecycle call site that sequences `run_advertise_hook → run_activate_hook → run_propagate_hook (new) → registry%propagate_unsatisfied_imports → registry%propagate_exports → recurse()`. |

### 1.3 Specification-to-source correspondence

`docs/graph/spec/README.md`'s own glossary and document-map table is the
authoritative name-to-concept mapping; do not re-derive it independently.
Key correspondence for this project's current focus:

- `03-graph-node-hierarchy.md` ↔ `graph/GraphNode.F90` family
- `05-identities.md` ↔ `graph/NodeId.F90`, `graph/HierarchyId.F90`, `graph/include/IdTemplate.inc`
- `06-dependency-network.md` ↔ `graph/DependencyNetwork.F90`
- `07-component-graph.md` ↔ `graph/ComponentGraph.F90`
- `08-graph-builder.md` ↔ `GraphBuilder.F90` + its submodules
- `02-component-hierarchy.md` ↔ `OuterMetaComponent.F90`/`OuterMetaComponent/`

## 2. Important interfaces and dependencies

### 2.1 Layering rule (hard, enforced by review not by compiler)

```
ComponentGraph / DependencyNetwork / GraphNode* / GraphStateItem
    MUST NOT depend on OuterComponent, StateRegistry, or any
    component-hierarchy implementation type.          (REQ-CG-002)

GraphBuilder
    is the ONLY layer permitted to depend on both the graph core
    AND OuterMetaComponent / StateRegistry.             (08-graph-builder.md §8.3)
```

### 2.2 `NodeId` scope — settled; spec docs now updated to match (Task 10.2)

**[UPDATED, this session — Task Group 10]** `docs/graph/spec/02-component-hierarchy.md`,
`05-identities.md`, `07-component-graph.md`, and `08-graph-builder.md`
have now all been updated in place (`[RE-EVALUATED]` sections,
following `07`'s own REQ-CG-003 precedent) to match the landed design
described below — task 10.2 is done; the narrative spec no longer
diverges from implementation on this point. Read those files directly
for the authoritative current wording; this section remains as a
navigational summary only.

As of commit `9ce2ecd5` ("Introduce Hierarchy"), `f629872a` ("Groups
3-5"), and `15a50ec5` ("Task group 9", hierarchy-wide freeze — all
landed on this branch):

- `NodeId`/`DependencyNetworkId` are valid **hierarchy-wide**, not
  scoped to one `ComponentGraph` instance.
- A `ComponentGraph` instance holds a `HierarchyId` value (not a
  pointer) and resolves `get_hierarchy_resources(this%hierarchy_id)`
  internally wherever the shared maps/generators are needed.
- Each `ComponentGraph` instance keeps an explicit, private
  `NodeIdSet`/`DependencyNetworkIdSet` of ids it itself registered
  (`owned_node_ids()`), used by `add_dependency`/`validate`.
- Cross-component edges are legitimate: `add_dependency`/`validate`/
  `bind_port`/`add_import_port` accept an id if it is EITHER owned by
  `this` instance OR registered anywhere in the hierarchy
  (`allowed_foreign_node`, checked via `get_node(id)` being
  associated — never via a reverse lookup; see design.md D2 in the
  active change for the full reasoning, including a documented,
  rejected "reverse map" alternative).
- Proxy-node mechanisms (`proxy_key`, `get_or_make_local_node_id`,
  `parent_geometry_proxy_key`, `push_geometry_to_child`, the
  proxy-creation half of `resolve_qualified_export_node_id`) have
  **already been removed** (Groups 3-5). Cross-component dependency
  edges now connect two real `NodeId`s directly.
- `freeze()` is now a **single hierarchy-wide operation** (Group 9,
  landed, commit `15a50ec5`): a new, explicitly non-recursive
  `GENERIC_INIT_GRAPH_FREEZE` phase, dispatched once at the true
  hierarchy root, drives `GraphBuilder_Freeze_smod.F90`'s
  `freeze_subtree_` helper, which recursively calls `graph%freeze()`
  on every component's own `ComponentGraph` instance in the whole
  tree. `graphbuilder_resolve_connections`'s own second-pass
  idempotency gate no longer relies on `is_frozen()` (which would have
  stayed unfrozen too long once freeze moved off the per-component
  `ACCEPT_TRANSFER` call site) — it uses a new, explicit
  `this%connections_already_resolved()`/`mark_connections_resolved()`
  flag instead (`OuterMetaComponent/connections_resolved.F90`),
  confirmed necessary by a real regression
  (`MAPL3G_Comp_Test_case42`) the naive version produced. See §3 below
  (now archived/historical — Group 9 is landed, not in progress) and
  `docs/graph/spec/07-component-graph.md` §7.3 for the current
  authoritative requirement text.

### 2.3 Import-port table vs. resource index — load-bearing distinction

- **Resource index** (`ComponentGraph%get_resource_index`): populated
  only for a *directly-advertised* item (`advertise_one`'s
  `add_resource_index` call).
- **Import-port table** (`ComponentGraph%get_import_ports` /
  `add_import_port`): includes both directly-advertised imports and
  (new, this branch, Group 6) *mirrored* imports bubbled up from a
  child. A mirrored entry is deliberately **not** given a
  resource-index entry.
- `for_each_matching_import` walks the import-port table, so it finds a
  mirrored import exactly as it would a direct one — this is the
  mechanism Group 6 relies on, with zero changes to
  `for_each_matching_import`/`resolve_one` themselves.
- Consequence (fixed on this branch, see §3): any resolution code that
  used to re-derive a destination's `NodeId` via
  `resolve_local_node_id(...)` (a resource-index lookup) must instead
  use the already-found `StateItemNode`'s own `get_node_id()` directly,
  or it will hard-fail on a mirrored import. `resolve_one` was updated
  for this; audit any *other* caller of `resolve_local_node_id` for the
  destination side before trusting it against a mirrored import.

### 2.4 Lifecycle call-site ordering (verified in `initialize_advertise.F90`)

```
recurse()                              ! bottom-up: every child's own full
                                        ! advertise pass already finished
run_advertise_hook(this)
run_activate_hook(this)                ! must run BEFORE propagate (see below)
run_propagate_hook(this)               ! NEW on this branch — see §3
registry%propagate_unsatisfied_imports ! legacy, unchanged, still runs in parallel
registry%propagate_exports             ! legacy, unchanged
```

`run_propagate_hook` MUST run after `run_activate_hook`, not before —
an import resolved by a connection declared at the current level must
already be reflected before this level's own mirroring step reads
child state, or it would be needlessly re-mirrored upward (confirmed by
a correction recorded in `tasks.md` task 6.2 of the active change —
the original task wording had this backwards).

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

## 4. Verified build and test commands

All verified by inspecting actual log files already present in this
checkout (`nag/*.log`, `gfortran/ctest-task6.log`) — **not re-run by
this documentation pass**. Treat as evidence of a prior run, not a
live guarantee; re-run before relying on them for new work.

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

### 4.3 How to re-run just the graph-relevant suite (fast path)

Verified directory exists in this checkout's NAG build tree
(`nag/superstructure/generic/tests`):

```bash
cd nag/superstructure/generic/tests
module load nag mpi baselibs && make 2>&1 | tee build.log
module load nag mpi baselibs && ctest -R MAPL.generic.components --output-on-failure
```

### 4.4 Correction to the generic `mapl-testing`/`mapl-build` skills

Those skills reference a `generic3g/tests` build subdirectory and
`MAPL.generic3g.*` ctest target names. **This checkout uses
`superstructure/generic/` and `MAPL.generic.*` target names** (verified:
`find . -iname generic3g` returns nothing; actual targets observed in
logs are `MAPL.generic.graph`, `MAPL.generic.scenarios`,
`MAPL.generic.components`, `MAPL.generic.core`, `MAPL.generic.aspects`).
Use the real target names above, not the skill's `generic3g` examples,
on this branch/repo state.

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

## 5. Previous architectural decisions (summarized; see design.md for full reasoning)

Source: `openspec/changes/hierarchy-wide-component-graph/design.md`
(the active, in-flight change). Only the already-landed-or-in-progress
decisions are summarized here; read the source file directly before
making a change that touches any of these.

- **D1 — Shared storage via `HierarchyId`-keyed registry, not threaded
  pointers, not a bare singleton.** Rejected a `SharedGraphStorage`
  pointer threaded through `OuterMetaComponent` construction (would
  have grown a public, user-facing `mapl_GridCompCreate` signature with
  a plumbing concern, and risked a future circular dependency). Rejected
  a bare module-level singleton (`CallbackInterfaceRegistry`'s own
  precedent) because node/network storage is mutated far more heavily,
  earlier, and by more independent test fixtures than callback-interface
  registration — cross-test state bleed would be a real risk, not
  theoretical.
- **D2 — Ownership is explicit (`own_node_ids` set), not inferred from
  storage locality; no reverse `NodeId → owner` map.** Every real
  caller identified only ever needs "is this mine" (forward), never
  "whose is this" (reverse). **Correction recorded mid-implementation**
  (Groups 3-5): `add_dependency`/`validate`/`bind_port`/
  `add_import_port` relaxed to accept either an owned id or any id
  registered anywhere in the hierarchy (`allowed_foreign_node`) — a
  cross-component edge's two endpoints are legitimately two different
  components' own ids, neither "owned" by the instance adding the edge.
- **D3 — Propagation is one recursive mirror step per level** (see §3.1
  above for the full postmortem of the rejected "search the subtree"
  alternative). Explicitly notes a genuinely open follow-up (task 6.7):
  inserting a mirrored import into the real `ESMF_State` under a
  child-qualified name still needs a new branch in
  `graphbuilder_populate_state`, not yet implemented.
- **D4 — Export visibility (History) is a query, not a push.** No
  analog of legacy's unconditional export-push into a nested substate
  tree; a hierarchy-wide consumer walks the `OuterMetaComponent` tree
  once and reads each component's own local resource index directly.
  **Implemented** (Group 7, `query_component_exports`, commit
  `bc51fc9c`).
- **D5 — Qualified-path resolution stays a tree walk; only the
  proxy-creation step at the end was removed.** Component-name
  resolution (`resolve_qualified_component`) is unaffected; only what
  happens after a successful resolution changed.
- **D6 — Composite-member non-addressability and the
  connection-direction invariant are promoted from "incidentally true"
  to explicitly stated, test-covered requirements** — confirmed to
  already hold structurally; no implementation change needed, only new
  test coverage. **Confirmed and test-covered** (Group 8, task 8.1,
  this session, uncommitted — `test_composite_member_not_
  independently_addressable`).
- **D7 — Freeze becomes one hierarchy-wide call** (not yet implemented;
  Group 9, unchecked). Per-component freeze-ordering reasoning (phases
  11a/11b) becomes moot once this lands.

## 6. Completed phases and outstanding work (cross-reference)

See `docs/graph/spec/20-implementation-roadmap.md` for the authoritative,
much longer version of this list (§20.4 and its many sub-sections). This
is a short pointer, not a replacement:

**Landed (confirmed by roadmap text + archived openspec changes):**
Phases 1–4 core (identities, node hierarchy, dependency network,
component graph, transforms, revision/update, GraphBuilder integration,
extension reuse, methods/drivers, callbacks, horizontal geometry 4e,
vertical grid 4f, route handles 4g), Phase 5a (StateItemCharacteristic
hierarchy) + 5a2 (seven more subclasses) + 5b (ordinary inout, direct
alias) + 5c (compiled-execution groundwork), Phase 9
(`GraphResolutionEntry`), Phase 10 a–d (state materialization +
3-mode toggle, equivalence harness, typekind unification, ordinary-phase
`MethodGraphNode`), Phase 11 a/b (export-field aliasing, multi-hop chain
materialization), **Phase 16 (`hierarchy-wide-component-graph`,
architecture revision — recorded as a new roadmap phase this session,
`20-implementation-roadmap.md` §20.4.16): all 10 Task Groups complete
and committed** (`9ce2ecd5`, `f629872a`, `9fdea0fd`, `bc51fc9c`,
`1ea0fdc1`, `15a50ec5`, plus this session's Task Group 10 work —
regression pass + spec-doc updates, no production code change). Only
task 6.7 (bubbled-import insertion into the real `ESMF_State`, explicit
out-of-scope follow-up) remains unimplemented within this phase, filed
as a named follow-up rather than silently left undone.

**Explicitly blocked / not ready to scope:** Phase 5b2 (ordinary inout,
general case — needs a design addendum resolving four named open
points before any code may be written).

**Not yet filed as their own change, named as prerequisites for Phase 6
legacy retirement:** Phase 7a/7b (Bracket/Expression shape dispatch),
Phase 7c narrowed to Wildcard (Service is being retired outright, not
migrated), Phase 12 (5a2 subclasses wired into real dispatch — roadmap
reference, read `20-implementation-roadmap.md` directly for current
numbering), Phase 13 (restart compatibility).

**Do not start Phase 6a (legacy `StateRegistry` retirement) until the
five-item prerequisite list in roadmap §20.4.11 has landed** — this
includes 3b2 (the Group-6 work this branch is mid-implementing).

**New, planning-only phases added this session (no code changed; see
roadmap §20.4.17-§20.4.21 for full scope/sub-sequencing of each) —
Phases 17-21, an independent cleanup/infrastructure/diagnostics lane,
confirmed to proceed in parallel with Phase 6a's own prerequisite list
and Phases 7/12/13 except where explicitly noted:** Phase 17
(`GraphBuilder.F90`/`Test_GraphBuilder.pf` size — characterize before
splitting; first session-sized step is dedup of two duplicated test
fixture helpers), Phase 18 (subdirectory reorganization of
`superstructure/generic/graph/` into `characteristics/`/`transforms/`/
`callback/`/`id_types/` — **sequencing-gated on Phase 12 for the
`characteristics/` move specifically**, not an independent lane
without qualification), Phase 19 (dedup two near-identical
`sorted_names` helpers — the backlog's original "`StringVector`
instead of fixed-width arrays" framing did not survive verification;
no genuine growable-character-array instance was found in the graph
codebase), Phase 20 (test-suite log noise — recorded for
traceability per explicit request, but judged NOT to be
graph-architecture roadmap work in its "field dictionary" half; kept
short, pointing at source rather than restating it), Phase 21
(standalone per-component graph-to-image/JSON diagnostic driver —
reframed from low-priority dev convenience to real diagnostic
tooling for debugging graph structure once Phase 12/7a-c land;
recommended to be sequenced BEFORE those phases, not after, despite
the backlog's own original "last priority" framing).


