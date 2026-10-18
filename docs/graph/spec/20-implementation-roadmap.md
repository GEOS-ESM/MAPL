# 20. Implementation Roadmap

Status: planning record, not a design spec. Captures the phased
implementation approach and repo-strategy decision discussed when scoping
an OpenSpec-driven ("SDD") implementation effort. Update this document as
the plan changes; do not let it silently drift out of sync with what is
actually being built — treat divergence the same way the rest of this
specification treats a stale requirement.

## 20.1 Readiness assessment

The specification is mature enough to plan a phased implementation, on
the following basis:

- **Core graph model is settled**: `03` (GraphNode hierarchy), `05`
  (identities), `06` (DependencyNetwork), `07` (ComponentGraph), `10`
  (transforms/ports), `11` (NodeRevision + update algorithm) are all
  `[SETTLED]`, including the two items formalized in this same pass
  (CHANGELOG 1.3): Q3 (port-binding storage) and Q11 (GraphStateItem/GraphValue
  reconciliation, fully resolved).
- **Genuinely deferred items do not block core work**: `16` (ordinary
  inout) is `[DEFERRED]`; `18` (StateItemCharacteristic) is
  `[SPECULATIVE]` with cross-graph mechanics still open (Q12/Q13); Q9
  (compiled execution) is explicitly a later phase; `19` (visualization
  export) is additive and not on the critical path.
- **Remaining opens are scoped to later phases**, not the core: RouteHandle
  time-dependent renewal detail, geometry shallow-copy-across-grid-
  recreate ESMF verification (`07-component-graph.md` §7.4.1), callback
  term naming (Q6), StateIntent-vs-placement (Q7) — none of these block
  `ComponentGraph`/`DependencyNetwork`/identity work.

**Conclusion:** proceed with Phase 1 (below) now. Do not wait on Q1/Q2/
Q6–Q10/Q12–Q18 — they are either already resolved or are correctly scoped
to phases that come later.

## 20.2 Repo strategy

**Decision:** develop the graph-neutral core (Phase 1–2, §20.3) in a
separate repository — this one, promoted from a specification-only repo
to a real library repo — rather than inside MAPL from the start.

**Rationale**, not merely an LLM-cost convenience, though that is real
too:

1. The specification already mandates the separation architecturally.
   `07-component-graph.md` REQ-CG-002 and `08-graph-builder.md` REQ-GB-001
   are hard rules: `ComponentGraph`/`DependencyNetwork`/`GraphNode*`/
   `GraphValue*` MUST NOT depend on `OuterComponent`, `StateRegistry`, or
   component-hierarchy implementation. `08-graph-builder.md` §8.3 states
   the reason directly — it is what keeps the graph core unit-testable in
   isolation, with synthetic nodes/values, with no ESMF component
   hierarchy present at all.
2. The core's actual external dependencies are narrow: gFTL containers
   (`05-identities.md` REQ-ID-006) and bare ESMF handle types
   (`ESMF_Field`/`FieldBundle`/`State`/`RouteHandle` as `GraphStateItem`
   components, `04-graph-value-hierarchy.md` REQ-SI-002). It does not need
   MAPL, `GriddedComponentDriver`, or the component hierarchy at all for
   Phase 1–2's scope.
3. **LLM context cost**: developing Phase 1–2 inside a full MAPL checkout
   would put the entire MAPL source tree (component hierarchy,
   `StateRegistry`, ESMF bindings, the existing extension/coupler
   machinery, generated code, build system) in scope for every
   agent-assisted change, even though none of it is relevant to
   `ComponentGraph`-internal work. A separate repo bounds the working set
   to what REQ-CG-002/REQ-GB-001 already say is the correct scope.
4. **This saving is real but scoped to Phase 1–2 only.** Phase 3+
   (§20.4) integrates with `StateRegistry`/`OuterComponent`/
   `GriddedComponentDriver` by construction — that work requires MAPL
   source in context regardless of which repo it is edited from. Repo
   separation does not avoid that cost; it just keeps it confined to the
   phases that actually need it.

**Tooling note:** `openspec` (installed locally) has a `store` concept for
standalone repos registered independently of a project's own OpenSpec
root, which fits "core library repo, consumed later by MAPL as a
dependency" directly — prefer that shape over forcing the core repo to
pretend to be a subdirectory of MAPL from day one.

## 20.3 Phase 1–2: graph-neutral core (this repo, or its successor library repo)

Buildable and testable now, no MAPL dependency:

- **Phase 1** — `05` Identities (NodeId + sibling ID types + template),
  `03` GraphNode hierarchy (`GraphNode`/`BaseGraphNode`/`StateItemNode`/
  `OperationGraphNode` stub), `04` §4.6 `GraphStateItem` (incl. REQ-SI-006
  membership maps), `06` DependencyNetwork (adjacency, cycle rejection,
  validation), `07` ComponentGraph (ownership, lifecycle, freeze).
- **Phase 2** — `10` TransformGraphNode + named ports + REQ-XFORM-005
  external port-binding table, `11` NodeRevision + demand-driven update
  algorithm (runtime-interpreted form; compiled form is Q9, later).
- **Phase 2 (additive)** — `19` §19.2's graph-neutral exporter layer
  (REQ-VIZ-003): depends only on the public `ComponentGraph`/
  `DependencyNetwork` query API and `NodeId%to_string()`, so it belongs
  here rather than waiting for Phase 3.

Exit criterion for Phase 1–2: synthetic-graph test suite exercising
construction, wiring, cycle rejection, freeze, demand-driven update, and
graph-neutral export, with no ESMF component/GridComp/StateRegistry
involved anywhere in the tests.

## 20.4 Phase 3+: MAPL integration (inside MAPL, or a MAPL checkout consuming Phase 1–2 as a dependency)

Requires real `StateRegistry`/`OuterComponent`/`GriddedComponentDriver`
context by construction — no repo-separation saving available here:

- **Phase 3** — `08` GraphBuilder (integration layer), `02` component
  hierarchy wiring (OuterComponent ownership, child proxy nodes,
  encapsulation boundary), `09` extension reuse / backward-compatible
  automatic-coupler behavior. `19` §19.2's enrichment layer (REQ-VIZ-004,
  name resolution via `StateRegistry`) ships here. See §20.4.1 for the
  sub-sequencing this phase needs before implementation starts.
- **Phase 4** — `12` MethodGraphNode + invocation adapters +
  `GriddedComponentDriver` + SetServices lifecycle, `15` callbacks
  (CallbackInterface/registry/Handler/Invoker), `13` geometry and
  vertical grids, `14` route handles (RouteHandleValue/Key, sharing).
  Time-dependent geometry/RouteHandle renewal (§13.4/§14.4) and
  exchange-component geometry (REQ-GEO-002a) are explicitly deferred
  out of this phase's initial scope, not silently assumed solved. See
  §20.4.3 for the sub-sequencing this phase needs before implementation
  starts.
- **Phase 5 (speculative/deferred, do not block on these)** — `18`
  StateItemCharacteristic hierarchy, `16` ordinary inout items, Q9
  compiled-execution optimization. See §20.4.4 for the sub-sequencing
  this phase needs before implementation starts.
- **Phase 6 (cleanup, growable — see §20.4.2)** — retire legacy
  `StateRegistry`/`ExtensionFamily`/aspect-based coupling once the
  graph-native paths above are the actual default, plus a running list
  of small, otherwise-easy-to-lose follow-ups that are blocked on that
  retirement (e.g. 3c's own `UnitsConverterTransform` ->
  `ConvertUnitsTransform` rename). Entry criterion ("validated, not
  merely built") is Phase 10's own job to satisfy — see §20.4.8.
- **Phase 7 (shape-changing connection dispatch — discovered gap, not
  previously tracked by any phase above)** — legacy `ClassAspect`'s own
  shape-dispatch role (`09-extension-reuse.md`'s existing
  `Characteristic`/`CharacteristicMap` family has no analog of it yet):
  Bracket/VectorBracket -> Field/Vector time-interpolation conversion,
  Expression -> Field N-ary evaluation, and Wildcard/Service
  pattern-based multi-source fan-in. Directly relevant to eventually
  running `Test_Scenarios` against the graph-native path instead of
  `StateRegistry`, which is the intended strong end-to-end signal that
  all necessary capabilities have been migrated. See §20.4.5 for the
  sub-sequencing this phase needs before implementation starts, and why
  none of it belongs in Phase 5's `StateItemCharacteristic` family
  despite the naming similarity.
- **Phase 8 (add support for `VariableSpec` validation — discovered
  gap, not previously tracked by any phase above)** — `verify_variable_spec`
  (`superstructure/generic/specs/VariableSpec.F90`) already exists but is
  dead code: module-private, not re-exported, and called from nowhere,
  not even `make_VariableSpec` itself. Three already-landed graph-native
  "mark the item" fields (`callback_interface_id` - callback-wiring,
  `state_item_variant` - vertical-grid-graph-state-item,
  `is_inout_borrower` - ordinary-inout-direct-alias) are consequently
  reachable only from synthetic test code: neither real production
  construction path (`MAPL_GridCompAddSpec`/`gridcomp_add_spec`,
  `MAPL_Generic.F90`, or YAML parsing,
  `ComponentSpecParser/parse_var_specs.F90`) exposes any of the three as
  a keyword argument, and both bypass `ComponentSpec%add_var_spec()`
  entirely (pushing directly into `var_specs`), so there is also no
  single already-existing call-through point every construction path
  shares.   `ordinary-inout-direct-alias`'s own mutual-exclusion guard
  (`callback_interface_id%is_valid() .and. is_inout_borrower`) was
  necessarily placed in `GraphBuilder.F90`'s `resolve_one` instead - it
  only fires when a connection happens to resolve against the item, not
  at declaration time, and is reachable from production code even less
  than the fields it guards. See §20.4.6 for scope.
- **Phase 9 (`GraphResolutionEntry` record type — discovered gap, not
  previously tracked by any phase above)** — **landed**
  (`openspec/changes/graphbuilder-resolution-entry`): `GraphBuilder.F90`'s
  `unresolved_imports`/`unsupported_characteristics` resolution reports
  used a `StringVector` of colon-concatenated
  `component_name:short_name[:reason]` strings; replaced with a real
  `GraphResolutionEntry` record (`component_name`/`short_name`/`reason`
  fields) plus a generated `GraphResolutionEntryVector`, mirroring
  `QualifiedExportEntry`'s own precedent for this exact problem shape
  earlier in the same file. Flagged in review discussion before this
  change was filed, but no roadmap entry was ever written for it at the
  time, so the gap was effectively lost until this phase's own review
  caught the omission. See §20.4.7 for scope.
- **Phase 10 (graph-native `Test_Scenarios` validation suite —
  discovered gap, not previously tracked by any phase above)** — Phase
  6's own entry criterion requires every production path's graph-native
  replacement to be *validated*, not merely built, before legacy
  retirement starts; no such validation suite exists yet. Four strictly
  ordered sub-changes: state materialization + a 3-mode
  (`registry`/`graph`/`both`) toggle, a generalized equivalence harness
  on new flat fixtures, a real two-characteristic (`units`+`typekind`)
  extension chain, and (new mechanism, sequenced last) real
  `MethodGraphNode` construction for an ordinary run phase with runtime
  re-invalidation. See §20.4.8 for the sub-sequencing this phase needs
  before implementation starts, and why it is a prerequisite for this
  bullet's own Phase 6, not a renumbering of it.

### 20.4.1 Phase 3 sub-sequencing

Phase 3, taken as one unit, does not fit a single spec-driven change
proposal without an unreasonable context/cost footprint (four
genuinely separable concerns, at least one — 3b below — requiring
real-configuration validation, not just unit tests). Split into four
ordered sub-changes instead. `08-graph-builder.md` §8.2's own
responsibility table includes wildcard/callback connection resolution
and ESMF-relationship compilation rows that belong to Phase 4/5 above,
not Phase 3 — the sub-changes below exclude them accordingly.

- **3a. Component-hierarchy foundation** (`02`, REQ-HIER-001..006) —
  `OuterComponent`/`OuterMetaComponent` ownership shape (own +
  per-child `GriddedComponentDriver`, one local `ComponentGraph`,
  framework-managed states, public ports), the parent-may/child-may-not
  encapsulation boundary (REQ-HIER-003), and the proxy-node *storage*
  mechanism (REQ-HIER-006) only — not yet the logic that populates it
  (that is GraphBuilder's job, 3b). First point requiring a real
  `ESMF_GridComp`; does not yet need `StateRegistry`.
- **3b. GraphBuilder: advertising + ordinary connection resolution**
  (`08` §8.2: advertising, creating `StateItemNode`s, resolving
  ordinary connections, default dependency network, public ports/child
  proxies, validate/freeze) — the slice `17-open-questions.md` Q10
  flags as "the first point where existing MAPL coupler behavior must
  be reproduced exactly," recommending side-by-side comparison against
  the existing imperative coupler on real configurations before
  removing that path. Kept deliberately narrow: REQ-EXT-003's exact-
  match/no-op case only, no mismatch/extension handling yet, and (see
  3b2 immediately below) *single-level* only — a connection is resolved
  only when both endpoints declare the item directly; no cross-level
  propagation.
- **3b2. Cross-component unresolved-import propagation** — REQUIRED
  completion of 3b's own job, not optional polish: discovered missing
  during 3b's implementation and review (`openspec/changes/
  graphbuilder-advertising-connections`). An import left unresolved by
  3b's single-level matching is exactly legacy's own
  `propagate_unsatisfied_imports()` case — the point where an
  unsatisfied import must be re-advertised as needed one level up the
  hierarchy (mirroring `StateRegistry_Propagation_smod.F90`'s
  `childname/itemname` bubbling) so an ancestor's own connections get a
  chance to resolve it, all the way up if necessary. Checking for
  unsatisfied imports with no mechanism to ever satisfy them across
  component boundaries is not useful on its own; this sub-change is
  what makes 3b's activate-time check (`graphbuilder_check_unsatisfied_imports`)
  connect to anything beyond a log message. Depends on 3b's proxy-node
  and resource-index mechanisms; does not require 3c's mismatch/
  extension machinery. Sub-sequencing (3c, 3d below) numbering is
  intentionally left alone rather than renumbered — this slots in
  functionally between 3b and 3c, before 3c's mismatch-driven work
  needs to run on whatever is still unresolved after propagation.
- **3c. Extension reuse** (`09`, REQ-EXT-001/002/003/005) —
  `CharacteristicId`/`Characteristic`/`CharacteristicMap` (graph-native
  analog of legacy `AspectId`/`StateItemAspect`/`AspectMap`, mirroring
  those patterns' shape — type-safe id, a gFTL2 polymorphic map, and a
  deferred `build_transform` method exactly like
  `StateItemAspect%make_transform` — while deliberately independent of
  their implementation), Transform-chain creation for mismatched export/
  import pairs (real execution ships for `units` only, via
  `UnitsCharacteristic%build_transform` and a new `UnitsConverterTransform`
  — named to avoid colliding with legacy's `ConvertUnitsTransform`
  module; rename once that legacy module is removed), and the
  extension-family reuse search restated as a graph-native lookup via
  `ComponentGraph`'s existing resource index. Depends on 3b's wiring and
  Phase 2's `TransformGraphNode`/`Transform`. REQ-EXT-002a stays
  `[SPECULATIVE]`/`[OPEN]`, not resolved by this sub-change.
- **3c2. StateRegistry/OuterComponent visibility for extension items**
  (`09` REQ-EXT-004) — discovered missing during 3c's implementation:
  `StateItemSpec` has no construction path independent of its full
  `AspectMap`, so satisfying REQ-EXT-004 without reusing `StateRegistry`'s
  existing methods (a hard constraint by this point in the effort —
  `StateRegistry` is intended to be retired once graph development is far
  enough along, not extended) means a second, independent aspect-map-
  equivalent construction pipeline — deliberately not attempted inside
  3c itself. Extension items created by 3c are correct graph structure
  (real `NodeId`s, dependency edges, reuse semantics) but are not yet
  visible to `StateRegistry` or any ESMF state; 3c2 is what makes them
  visible. Depends on 3c's chain-creation machinery; does not need real
  providers for characteristics beyond `units` to be useful on its own.
  Numbering intentionally mirrors 3b/3b2's own precedent (a sub-change
  whose own implementation surfaced a required, committed follow-up) —
  slots functionally after 3c, before 3d.
- **3d. Visualization enrichment layer** (`19` REQ-VIZ-004 only) —
  wraps the Phase 1–2 graph-neutral exporter with a `NodeId -> label`
  resolver backed by `StateRegistry`. Independent of 3b/3c's hard
  parts (only needs 3a's `OuterComponent` shape + name lookup); MAY be
  reordered directly after 3a if a lower-risk early win is preferred.
  REQ-VIZ-005 (hierarchy-wide export) stays `[OPEN]` (Q18) and is out
  of scope for this sub-change.

**Repo/tooling note (extends §20.2).** Phase 3+ code (this section)
lives in the MAPL repo/checkout, not the Phase 1–2 core repo — per
§20.2, the LLM-context saving from repo separation does not apply once
`StateRegistry`/`OuterComponent` context is required regardless. The
Phase 1–2 core repo remains a distinct git repository (not merged into
MAPL) and, if referenced as a git submodule during this transition,
that MUST be treated as a temporary bootstrap mechanism only — not a
standing precedent for one subrepo per major MAPL design element. The
  This MAPL checkout now contains full architecture documents under
  `docs/graph/spec/` and executable Phase 1-2 requirement subsets under
  `openspec/specs/graph/`; Phase 3 agents should use these local copies
  rather than requiring access to the former standalone core repository.

### 20.4.2 Phase 6: legacy retirement (cleanup phase)

**Purpose:** every sub-change from 3c onward has deliberately kept the
graph-native path *additive* — real production behavior still runs
through legacy `StateRegistry`/`ExtensionFamily`/`ClassAspect`/
`AspectMap` (`superstructure/generic/registry/`,
`superstructure/generic/specs/*Aspect*.F90`), with the graph-native
equivalent gated off by default (e.g. `extension-registry-visibility`'s
`materialize_extensions` flag) or simply not yet wired into the real
init sequence at all. That additive posture is correct while the graph
path is still being built out, but it also means a small, growing set
of otherwise-easy-to-lose follow-ups keeps accumulating — each one
individually too small to be its own roadmap phase, but genuinely
blocked until legacy is gone, not merely deferred by choice. Phase 6 is
where that debt gets paid: (a) retire `StateRegistry`/`ExtensionFamily`/
the aspect system once the graph-native paths are the *actual* default
(gates removed, not just flippable) and validated at production scale
— not attempted piecemeal inside any earlier sub-change; and (b) work
through the list below once (a) has landed.

**Entry criterion:** do not start (a) until every real production code
path that currently depends on `StateRegistry`/`StateItemSpec`/
`make_StateItemSpec`/`make_aspects`/any `ClassAspect` subclass has a
graph-native replacement that has been validated (not merely built) as
the default behavior — matching each earlier sub-change's own explicit
"read-only background reference, never called into" boundary around
that legacy machinery, now finally made moot by having nothing left
that needs it.

**Growable list — append here, do not let a future sub-change silently
drop its own "blocked until legacy is retired" follow-up elsewhere:**

- Rename `UnitsConverterTransform` -> `ConvertUnitsTransform`
  (`superstructure/generic/graph/UnitsConverterTransform.F90`) once
  legacy `mapl_ConvertUnitsTransform_mod`
  (`superstructure/generic/transforms/ConvertUnitsTransform.F90`) is
  removed — module names share one global namespace, so the two cannot
  coexist under the same name. Originates from 3c (design.md); tracked
  as task 7.1 of `extension-registry-visibility`, confirmed still
  blocked 2026-09-17: legacy `UnitsAspect%make_transform`
  (`superstructure/generic/specs/UnitsAspect.F90:128`) still
  instantiates the legacy type directly from the live
  `StateRegistry_Extensions_smod.F90` dispatch path — load-bearing, not
  dead code.
- Consolidate `VariableSpec%itemType` (`ESMF_StateItem_Flag`,
  `superstructure/generic/specs/VariableSpec.F90`) onto a single,
  graph-native `MAPL_StateItem_Flag`-typed field (per 4f's
  `state_item_variant`, `mapl_StateItemFlag_mod`), with `itemType`
  becoming a derived/truncated view (native-kind parent of whatever
  refined value is stored) rather than a separately-stored field.
  Originates from 4f (`openspec/changes/vertical-grid-graph-state-item`,
  design.md discussion): every graph-native refined value (`GEOM`,
  `VECTOR`/`BRACKET`/`VECTORBRACKET`, `VERTICALGRID`, `ROUTEHANDLE`)
  already implies exactly one native `itemType` parent and never
  contradicts it — the two fields hold no genuinely independent
  information, only `VariableSpec`'s historical split between the
  legacy `mapl_StateItem_mod`/`ClassAspect`-dispatch vocabulary and the
  newer graph-only vocabulary keeps them as two stored fields today
  (unlike `GraphStateItem%itemType()`, which is already a pure derived
  query, not stored, at the node-payload tier). **Blocked**:
  `itemType`'s current `ESMF_StateItem_Flag` values include
  `WILDCARD`/`SERVICE`/`SERVICE_PROVIDER`/`SERVICE_SUBSCRIBER`/
  `EXPRESSION` (`mapl_StateItem_mod`, codes 201-208) with no graph-native
  equivalent yet, dispatched live from `make_ClassAspect`
  (`VariableSpec.F90:759`) and parsed live from YAML
  (`ComponentSpecParser/to_itemtype.F90`) — real, in-use legacy vocabulary,
  not dead code. `SERVICE` is expected to be retired outright (replaced
  by callbacks, `CallbackInterfaceId`), but `WILDCARD`/`EXPRESSION` are
  not yet represented in graph's vocabulary at all. Do not attempt this
  consolidation before `ClassAspect`'s own dispatch is itself retired or
  graph-natively replaced.
- Reconcile overlapping regrid-store settings in
  `superstructure/generic/graph/RouteHandleKey.F90` and
  `infrastructure/regridder_mgr/RoutehandleParam.F90`: evaluate extracting
  shared value representation/defaults or another reuse mechanism to prevent
  field drift, while preserving `RouteHandleKey`'s semantic-key rendering and
  `RouteHandleParam`'s execution role. A graph dependency on infrastructure is
  acceptable; graph code must not depend on `superstructure/generic/registry/`
  or otherwise use registry-layer APIs. Follow-up to Phase 4g, not a reason to
  block current implementation.
- `write_formatted`'s dead debug-dump machinery
  (`StateRegistry.F90:85-86,120-127`,
  `StateRegistry_Lifecycle_smod.F90:42-125`) — confirmed zero production
  callers; delete with the rest of Phase 6a-3 (§20.4.11), no separate
  migration needed. Originates from §20.4.11's own research pass.
- `ModelVerticalGrid%get_coordinate_field()`'s own second, independent
  `registry%extend()` call site (`ModelVerticalGrid.F90:273`) — already
  known to this document as 4f's own design rationale above but never
  tracked as its own "second call site to retire" checklist item.
  Confirm no remaining caller needs this path's side effect before
  deleting it in Phase 6a-3 (§20.4.11).
- `gridcomp_set_geometry`'s own direct single-item bypass of
  `StateRegistry%set_geometry` (`MAPL_Generic.F90:1150-1183`) — a
  second, independently-maintained geometry-setting call chain, easy to
  miss if a future retirement pass only searches for
  `registry%set_geometry(` calls (this one never calls that method
  name). See §20.4.11.
- Dead/disabled `propagate_exports`/`add_to_states` lines in
  `initialize_modify_advertised.F90:31,36` — already commented out;
  delete as part of general cleanup, no behavior change. See §20.4.11.
- The coupler-lifecycle call sites in
  `OuterMetaComponent/run_user.F90:48,72` and
  `initialize_user.F90:29` (`get_import_couplers`/
  `get_export_couplers`) — confirm `graphbuilder_run_pull_hook`
  (`GraphBuilder_RunPhasePull_smod.F90:44-79`) already covers every real
  re-propagation case these calls exist for, before deleting; do not
  assume coverage without checking, since the two mechanisms differ
  (coupler phases are a fixed three-phase ESMF lifecycle; the pull hook
  is a single demand-driven `update()` call). See §20.4.11.
- Service's own retirement — confirm every real production
  `SERVICE`/`SERVICE_PROVIDER`/`SERVICE_SUBSCRIBER` declaration has a
  landed callback-based replacement, then delete
  `ServiceClassAspect.F90` and the three `SERVICE*` itemType values —
  an audit-and-delete item, not a structural rebuild, unlike Wildcard
  (Phase 7c, narrowed — see §20.4.5/§20.4.11).
- Pre-removal compatibility check on `MAPL_GridCompGetRegistry`
  (`API.F90:42`) — public, exported, zero in-tree callers outside a
  test fixture (`superstructure/generic/tests/gridcomps/
  ProtoExtDataGC.F90:10,85,146`), but a real external-compatibility risk
  if any out-of-tree GEOS component calls it. Check against a real
  GEOSgcm checkout before deleting the symbol in Phase 6a-3 (§20.4.11).

### 20.4.3 Phase 4 sub-sequencing

Phase 4, like Phase 3 (§20.4.1), does not fit a single spec-driven
change proposal without an unreasonable context/cost footprint. Unlike
Phase 3, none of Phase 4 is built yet — no `MethodGraphNode`, no
`CallbackInterface`/registry beyond an empty `CallbackInterfaceId`
identity stub, no `RouteHandleValue`/`RouteHandleKey`, no geometry
`GraphStateItem` handling — so this is greenfield work on top of the
completed Phase 1–3 foundation, not an extension of partially-built
Phase 4 code. `17-open-questions.md` Q10 already gives this phase's
internal ordering rationale (methods/lifecycle first since "phases
start flowing through the graph" here; callbacks next as new capability
with no legacy behavior to match; geometry/route-handles last since
they touch the most existing special-cased code and should wait until
the graph plumbing around them is well-exercised). Split into seven
ordered sub-changes:

**Landed prerequisite, discovered ahead of this list (not one of the
original seven, inserted before it):** `composite-state-spec`
(`openspec/changes/archive/2026-09-18-composite-state-spec`,
`openspec/specs/graph/composite-state-spec/spec.md`). `15-callbacks.md`'s
`CallbackStateBinding` (REQ-CB-007, "argument name → member `NodeId`")
requires a callback State's members to be individually graph-visible —
nothing in `VariableSpec`/`GraphBuilder` supported that before this
change landed (a `MAPL_STATEITEM_STATE` declaration produced an empty,
opaque `ESMF_State`, per proposal.md - Why). Resolved by letting
`VariableSpec` declare its own composite member structure directly
(`declare_member`/`get_member`/`get_member_names`, a member being any
ordinary `VariableSpec`, leaf or further-nested) and by
`GraphBuilder`'s `advertise_one` recursing into declared members to
build a real `StateItemNode` tree. Does not block 4a/4b (neither touches
composite structure); is a real prerequisite for 4c/4d below.

- **4a. MethodGraphNode + invocation adapters** (`12` REQ-MTH-001/002/
  004/005/006) — the node type covering both GridComp phase invocation
  and attached-State-method invocation through one invocation-adapter
  abstraction (`GridCompMethodInvocation`/`StateMethodInvocation`,
  `17-open-questions.md` Q2). Synthetic-driver testable, same posture as
  3a: no real `GriddedComponentDriver` wiring yet, no ESMF component
  required.
- **4b. GriddedComponentDriver integration + init lifecycle** (`12`
  REQ-MTH-007..013) — the stable driver-lookup mechanism (REQ-MTH-009),
  the trigger/advance discipline around invocation (REQ-MTH-003a), and
  the full `advertise → modify_advertised → cycle(realize_provided,
  accept_transfer, realize_accepted) → read_restart → user_specific`
  ordering (REQ-MTH-011). REQ-MTH-011 step (c)'s convergence algorithm
  (how progress is detected, iteration-limit behavior, non-convergence
  handling) is explicitly `[OPEN]` in the spec — this sub-change's
  design.md MUST resolve it as a planned, up-front design decision
  before implementation starts, not discover it mid-implementation the
  way 3b's own real-configuration validation surfaced 3b2 unplanned.
  First sub-change requiring real `GriddedComponentDriver`/ESMF context;
  depends on 4a.
- **4b2. Unify `OuterMetaComponent`'s own driver into its child-driver
  map** — discovered during 4b's own code review
  (`openspec/changes/griddedcomponentdriver-integration-lifecycle`), not
  a required completion of 4b's job the way 3b2 was for 3b: 4b's own
  `DriverResolver` already works correctly as shipped, and this
  sub-change is a pure simplification, not a correctness gap. Does not
  block 4c/4d. REQ-MTH-008's "own driver + one driver per child"
  ownership shape currently lives as two separate representations on
  `OuterMetaComponent` — a single `user_gc_driver` field plus a
  `children: GriddedComponentDriverMap` — which forces every
  driver-key-resolution call site (4b's own
  `OuterMetaComponentDriverResolver`, and any future one) to branch on
  "is this the `<self>` sentinel or a real child name" instead of doing
  one uniform lookup. Store the component's own driver in the *same*
  map, under the already-established `<self>` sentinel
  (`GraphBuilder.F90`'s own `SELF_COMPONENT_NAME`, matching
  `StateRegistry_Hierarchy_smod`'s own precedent for treating "the
  component itself" as a reserved name among its children) — unifying
  "the user gridcomp" and "a child gridcomp" as the same underlying
  representation, differing only in which name resolves to which entry.
  Real work, not a one-line rename: `user_gc_driver` is currently
  accessed as a bare field (not only through the existing
  `get_user_gc_driver()` accessor) from several of `OuterMetaComponent`'s
  own submodules (e.g. `initialize_accept_transfer.F90`'s
  `this%user_gc_driver%get_states()`), every one of which would need to
  move to a uniform accessor once the separate field is gone; must also
  confirm no existing code path can ever declare a child literally
  named `<self>` (almost certainly already excluded by the same
  sentinel convention elsewhere, but worth confirming explicitly rather
  than assuming). Scoped to `OuterMetaComponent`'s own driver storage
  only — does not attempt the broader "is a component itself just a
  specially-named child everywhere" unification `GraphBuilder.F90`'s own
  `is_self()`/`SELF_COMPONENT_NAME` checks hint at for connection
  resolution; that is a separate, larger question outside driver
  storage, not assumed solved by this sub-change.

  **Evaluated during planning (2026-09-20), declined — not
  implemented.** Change proposal drafted
  (`openspec/changes/outermetacomponent-driver-map-unification`,
  removed after this decision, never merged) and taken through design
  before implementation began. Two findings killed it:
  1. `this%children` (`OuterMetaComponent.F90:57`) is not merely
     driver storage — it is the live "list of real children" that
     `get_num_children.F90`, `get_child_name.F90`, `recurse.F90`
     (`initialize`/`write_restart`), `run_children_.F90`,
     `run_clock_advance.F90`, `finalize.F90`'s `recurse_finalize_`, and
     `apply_to_children_custom.F90` all iterate directly. Inserting the
     self driver into that same map under `<self>` would have made
     every one of those sites additionally process the self entry as a
     "child," double-invoking `initialize`/`run`/`finalize`/
     `clock_advance` on the user's own driver (on top of the existing
     explicit `user_gc_driver`/`get_user_gc_driver()` calls already
     present in `run_user.F90`, `run_custom.F90`, `finalize.F90`,
     `run_clock_advance.F90`) and off-by-one-ing `get_num_children`/
     `get_child_name`. Filtering `<self>` out of all seven sites was
     considered and rejected: it adds guard logic in seven lifecycle-
     critical loops, the opposite of this sub-change's own stated
     purpose (measurably *reducing* duplicated logic).
  2. With that option off the table, the fallback — collapse
     `OuterMetaComponentDriverResolver.F90`'s self-vs-child `if/else`
     into a new `OuterMetaComponent` accessor without merging the
     underlying storage — was checked against the actual codebase
     first rather than assumed worthwhile: a repo-wide search found
     that branch already exists in exactly one place
     (`OuterMetaComponentDriverResolver.F90:65-82`); every other
     `get_user_gc_driver()` caller already knows it wants the self
     driver and calls it directly, with no `driver_key`-style branch to
     share. There is no second copy of the logic anywhere to
     consolidate, so relocating those four lines to a new method would
     move code sideways for zero measurable reduction in duplication —
     the premise this sub-change was filed under.

  Net: both mechanically-available paths are net-negative or net-zero
  against the sub-change's own justification. 4b's `DriverResolver`
  remains as shipped (two representations, one already-centralized
  branch) — correct, if not maximally elegant. Revisit only if a
  second real consumer of self-or-child driver-key resolution appears;
  until then there is nothing to unify.
- **4c. Callback data model + registry** (`15` §15.2–15.7) —
  `CallbackInterface`/`CallbackArgumentSpec`/`CallbackMethodSpec`/
  `CallbackStateBinding`/`CallbackInterfaceRegistry`. Static and
  unit-testable, no `GraphBuilder` wiring yet. Depends on
  `composite-state-spec` (landed, above) for `CallbackStateBinding`'s
  member-`NodeId` addressability; otherwise no new dependency beyond
  Phase 1–3 — MAY proceed in parallel with 4a/4b if desired, though Q10's
  stated order (methods before callbacks) is the default assumption.
- **4d. Callback wiring** (`15` §15.9–15.10) — `GraphBuilder`
  wildcard/regex expansion against the flattened qualified-export
  namespace (REQ-CB-016, pattern syntax settled as regex per Q5),
  per-method `DependencyNetwork`s for get/put argument flow
  (REQ-CB-018), and the invoke-once-after-all-args-ready discipline
  (REQ-CB-020). Depends on 4a (binds to a `MethodGraphNode`, REQ-CB-019),
  4c, and `composite-state-spec` (landed, above).
- **4e. Horizontal geometry as GraphStateItem** (`13` §13.1–13.2) —
  **landed** (`openspec/changes/horizontal-geometry-graph-state-item`):
  geometry carried as an incomplete `esmf_field` proxy
  (`ESMF_FIELDSTATUS_GRIDSET`), given real graph structure for
  REQ-GEO-001..003's three single-source cases. **Deviation from this
  entry's original wording, discovered during implementation:** "resolved
  through ordinary advertise/connect/transform-if-needed rules" turned out
  to be the wrong framing — `GeometrySpec`/`initialize_geom_a.F90`/
  `initialize_geom_b.F90` already resolve own/from-parent/from-child,
  entirely before `GraphBuilder.F90` ever runs (lifecycle phases 3-4,
  hierarchy-wide, before phase 5's `GENERIC_INIT_ADVERTISE`). The landed
  design is a dedicated `GraphBuilder.F90` hook
  (`run_geometry_hook`/`graphbuilder_advertise_geometry`/
  `graphbuilder_resolve_geometry`) that represents that already-resolved
  outcome as real graph structure (one `StateItemNode` per component, plus
  a cross-`ComponentGraph` dependency edge for the ancestor/child cases,
  reusing `09-extension-reuse.md`'s existing `Characteristic`/mismatch/
  extension-chain machinery) rather than an ordinary `VariableSpec`-based
  connection the way `08-graph-builder.md`'s existing `resolve_one` path
  handles other items - a reserved-name `VariableSpec` for this purpose
  was tried first and abandoned, since `ComponentSpec%var_specs` also
  feeds legacy `StateRegistry%add_to_states`, which would have leaked the
  geometry proxy into real user-facing states (REQ-GEO-003 violation).
  Entirely gated behind a new global toggle, `mapl_GraphMode_mod`'s
  `graph_native_enabled()` (added mid-implementation, general
  infrastructure beyond this sub-change's own scope - see §20.4.3's own
  discussion below and that change's design.md Decision D6), default off.
  **Explicit deferral, as stated in this sub-change's own proposal.md:**
  REQ-GEO-002a (exchange-component geometry, e.g. `SURF`-style
  multi-source `XGrid`) and all of §13.4 (time-dependent geometry renewal
  under freeze) are out of scope — static geometry only. Depended on
  Phase 1–3 only, as planned.
- **4f. VerticalGrid model** (`13` §13.3) —
  **landed** (`openspec/changes/vertical-grid-graph-state-item`):
  `VerticalGrid` as an `esmf_state`-kind `GraphStateItem` with
  `variant() == MAPL_STATEITEM_VERTICALGRID` (REQ-GEO-009),
  physical-dimension-keyed coordinate sets (REQ-GEO-004/004a), and the
  dimension-adaptability check for mismatched vertical grids
  (REQ-GEO-007a). **Significant deviation from this entry's original
  wording, discovered during implementation:** the original plan (wrap
  `OuterMetaComponent%get_vertical_grid()`/legacy `ModelVerticalGrid%
  get_coordinate_field()`, mirroring 4e's `run_geometry_hook` shape) was
  abandoned after tracing `get_coordinate_field()` into
  `StateRegistry%extend()`, which can mutate the registry (build a real
  `ESMF_GridComp` coupler) as a side effect — disallowed by explicit
  project direction: **the graph solution must not use `StateRegistry`**;
  legacy's aspect/extension machinery is at most suggestive of needed
  capability, never something the graph code calls into. The landed
  design instead reuses the already-shipped, zero-`StateRegistry`
  `composite-state-spec` mechanism directly: a component declares its
  vertical grid as an ordinary composite `VariableSpec` (`declare_member`,
  one member per physical dimension), tagged via a new
  `VariableSpec%state_item_variant` field (mirrors the existing
  `callback_interface_id` field's "mark the item, not a new itemType"
  precedent); `CompositeStateMaterialization` gained one line to apply
  the tag via the existing `set_variant`. REQ-GEO-007a's classification
  lives inside the **existing** `VerticalGridCharacteristic` (not a new
  characteristic kind — a first implementation added a separate
  `VerticalGridMembershipCharacteristic`, reverted after review: two
  "vertical grid" characteristics was confusing, and matching by
  dimension-name overlap with no identity-token fallback at all was a
  real false-positive risk, not merely a documented limitation).
  `VerticalGridCharacteristic` instead gained an optional declared
  physical-dimension set alongside its existing opaque identity token —
  identity-token comparison remains authoritative when both sides have
  one (unchanged ordinary-Field behavior); dimension-set equality is the
  fallback, used only when no identity token exists (always true for the
  composite case). Because `get_supported_physical_dimensions()` is a
  pure accessor, dimensions are populated for the ordinary-Field path
  too, so the three-way diagnostic applies uniformly — one pre-existing
  test's expected failure message was updated accordingly. Exercised
  through the *existing*, unmodified `resolve_one`/`build_characteristics`
  path — **no new `GraphBuilder.F90` hook was needed**, unlike 4e's own
  `run_geometry_hook`; no new toggle was needed either, since the new
  field is inert for every declaration that does not opt in. REQ-GEO-007's
  superseded-text `ReferenceCharacteristic`
  link back to horizontal geometry is **not implemented as such** —
  `GraphStateItem` has no persisted characteristics map
  (`18-state-item-characteristics.md` REQ-CHAR-007, Phase 5) — the
  association is instead a structural fact (a coordinate-set item and its
  component's own 4e geometry item always share one `ComponentGraph`).
  Depended on 4e only, as planned; REQ-GEO-005's reserved
  `MAPL_VerticalGrids` materialization, REQ-GEO-007b's general
  multi-candidate-import case, real vertical-regrid execution, and §13.4
  remain explicit deferrals.
- **4g. RouteHandleValue/Key** (`14`) — **landed:** `RouteHandleKey`
  structure (REQ-RH-002/003) and reuse through the existing generic semantic
  index (REQ-RH-004/005), rather than a dedicated gFTL map. No
  `RegridTransform` or `GraphBuilder.F90` wiring was added; real regrid
  execution remains future work. **Explicit deferral:** §14.4 time-dependent
  renewal remains out of scope — reuse-of-existing-handle case only. Depends
  on 4e (needs geometry identity to populate `RouteHandleKey`).

**Repo/tooling note (extends §20.4.1's own note).** Phase 4 code lives
in the MAPL repo/checkout, same as Phase 3, for the same reason: real
`GriddedComponentDriver`/`OuterComponent`/ESMF context is required from
4b onward regardless of repo layout.

### 20.4.4 Phase 5 sub-sequencing

Phase 5, like Phase 3 (§20.4.1) and Phase 4 (§20.4.3), does not fit a
single spec-driven change proposal without an unreasonable context/cost
footprint — it bundles three genuinely independent, speculative/deferred
items (`18` StateItemCharacteristic hierarchy, `16` ordinary inout items,
Q9 compiled-execution optimization), each carrying its own explicit
"do not implement without further design" caveat. Split into sub-changes
as follows, rather than filing one proposal spanning all three.

- **5a. StateItemCharacteristic hierarchy** (`18` §18.2–§18.8) — ready to
  scope now. `18`'s header caveat was tied to Q11, which
  `17-open-questions.md` already records as resolved; what remains are
  ordinary `[OPEN]` naming/mechanism points, not a precondition on the
  order of `16`'s REQ-INOUT-002. This sub-change's own design.md MUST
  resolve, as a planned, up-front design decision before implementation
  starts (same discipline Phase 4's 4b followed for REQ-MTH-011 step (c)):
  `CharacteristicStatus` naming (§18.3), `CharacteristicType` naming
  (§18.4), the ordering-delegation mechanism for chaining mismatch
  Transforms (§18.6, Q13), and the absent-key-vs-`INVALID`
  `characteristics`-map representation (§18.5). Depends only on Phases
  1–3 (`GraphStateItem`, `09-extension-reuse.md`'s existing
  chain-building machinery) — no dependency on `16` or Q9.
- **5a2. Remaining StateItemCharacteristic subclasses** — discovered
  during 5a's own code review (`openspec/changes/
  state-item-characteristics`), not scoped by 5a itself: REQ-CHAR-001's
  table lists only three subclasses and explicitly says so ("almost
  certainly incomplete... treat the list as a starting point, not a
  closed set"), and 5a implemented exactly those three
  (`PhysicalUnitsCharacteristic`, `TypeKindCharacteristic`,
  `GeometryCharacteristic`). But legacy's own `StateItemAspect` hierarchy
  (`superstructure/generic/specs/*Aspect.F90`, `AspectId.F90`) already
  has ten concrete mismatch-detectable axes, not three — `GeomAspect`,
  `UnitsAspect`, `TypekindAspect` (5a's three analogs) plus seven more:
  `VerticalGridAspect`, `AttributesAspect`, `UngriddedDimsAspect`,
  `QuantityTypeAspect`, `ConservationAspect`, `NormalizationAspect`,
  `StandardNameAspect`. Per this document's own "roughly 1-to-1
  correspondence, with exceptions" expectation (confirmed during 5a's
  review), each of those seven needs a `StateItemCharacteristic` analog
  before the graph-native path can claim parity with legacy's own
  mismatch-detection surface — not attempted by 5a, which deliberately
  scoped to the three REQ-CHAR-001 names only. Numbering mirrors this
  project's own established precedent for a review/implementation-
  discovered, required-completion follow-up (3b/3b2, 4b/4b2) — note the
  same distinction already drawn for `5b2`: this is a *parity gap*
  discovered against an existing legacy surface, not a brand-new
  requirement.

  **Scope, one new type per legacy `*Aspect` (name chosen to avoid
  colliding with `graph/extension-reuse`'s own, deliberately independent
  `Characteristic` family — the `GeometryCharacteristic`-not-
  `GeomCharacteristic` precedent, 5a design.md D3 — applies again here
  wherever a name would otherwise collide):**

  | Legacy `AspectId` | Legacy `*Aspect` | New `StateItemCharacteristic` |
  |---|---|---|
  | `VERTICAL_GRID_ASPECT_ID` | `VerticalGridAspect` | a vertical-grid `ReferenceCharacteristic` (name TBD at design time; `graph/extension-reuse` already has an unrelated `VerticalGridCharacteristic` — collision, needs a different name) — REQ-CHAR-012's own text already anticipates this one as "expected to be common," alongside `GeomCharacteristic` |
  | `ATTRIBUTES_ASPECT_ID` | `AttributesAspect` | `AttributesCharacteristic` (`ValueCharacteristic`) |
  | `UNGRIDDED_DIMS_ASPECT_ID` | `UngriddedDimsAspect` | `UngriddedDimsCharacteristic` (`ValueCharacteristic`) |
  | `QUANTITY_TYPE_ASPECT_ID` | `QuantityTypeAspect` | `QuantityTypeCharacteristic` (`ValueCharacteristic`) |
  | `CONSERVATION_ASPECT_ID` | `ConservationAspect` | `ConservationCharacteristic` (`ValueCharacteristic`) — legacy's own `make_transform` is an unconditional `_FAIL("should not be called")`, i.e. this axis is detected but never itself adapted; the graph-native analog's `build_transform` MAY do the same |
  | `NORMALIZATION_ASPECT_ID` | `NormalizationAspect` | `NormalizationCharacteristic` (`ValueCharacteristic`) |
  | `STANDARD_NAME_ASPECT_ID` | `StandardNameAspect` | `StandardNameCharacteristic` (`ValueCharacteristic`) |

  Explicitly out of scope for 5a2, same as 5a: no `GraphBuilder` rewire
  (these remain additive, standalone types exercised by synthetic-node
  pFUnit coverage only, mirroring 5a's own design.md D6); real
  `build_transform` implementations beyond whatever each legacy
  `*Aspect%make_transform` already does unconditionally (most either
  fail explicitly or have no real adaptation logic today — only
  `UnitsAspect`/`GeomAspect`/`TypekindAspect`, 5a's own three, have any
  real executing transform in legacy either, via
  `superstructure/generic/transforms/`). Legacy's `AspectStatus` has a
  sixth value, `FROM_COMP`, that `CharacteristicStatus` (5a, REQ-CHAR-003)
  does not — 5a2's own design.md MUST decide, as a planned, up-front
  decision, whether that is a real gap to close or a legacy-only
  distinction with no graph-native equivalent needed, rather than
  discovering it mid-implementation.

  Depends on 5a (the `StateItemCharacteristic`/`ValueCharacteristic`/
  `ReferenceCharacteristic` hierarchy, `CharacteristicStatus`,
  `StateItemCharacteristicKind`, the sparse map on `GraphStateItem`) —
  not on `16` or Q9. MAY be filed any time after 5a lands.
- **5b. Ordinary inout, direct-alias case** (`16` REQ-INOUT-001 only) —
  **landed** (`openspec/changes/archive/ordinary-inout-direct-alias`): a
  destination item declared an ordinary inout borrower
  (`VariableSpec%is_inout_borrower`) is wired by `GraphBuilder` through
  the same REQ-EXT-003 no-op comparison the ordinary (non-inout) path
  already uses for the forward edge (owner -> borrower), plus a return
  edge (borrower -> owner) in a fresh per-pairing `DependencyNetwork` -
  no new Transform in either direction, as REQ-INOUT-001 requires.
  Mismatched payload, a borrower with no identifiable owner, and
  chained/recursive borrowing are all rejected explicitly (reported, not
  silently extension-chained) rather than attempting anything
  REQ-INOUT-002 reserves. **Explicit deferral, as stated in this
  sub-change's own design.md:** the runtime return-edge propagation
  trigger (§16.1's "after borrower execution" step) is not implemented -
  `ComponentGraph`'s demand-driven `update()` only does active work for a
  `TransformGraphNode` frame (confirmed by reading
  `ComponentGraph_DemandDrivenUpdate.F90` directly), and the real trigger
  event (the borrower's own GridComp run / `MethodGraphNode` invocation
  completing) has no invocation-completion hook in the codebase yet for
  ordinary (non-callback) components - building one is Phase 4
  invocation-lifecycle work, surfaced but not attempted by this narrow
  sub-change. This change lands the forward/return graph structure only,
  mirroring 4e/4g's own "structure now, real execution later" precedent.
  Depended on Phases 1-3 only, as planned; independent of 5a/5c.
- **5b2. Ordinary inout, general case** — **BLOCKED**, not ready to
  scope. REQ-INOUT-002 explicitly requires an explicit design addendum
  resolving four named open points before any non-trivial inout support
  may be implemented: revision authority under two producers (an inout
  owner value is both a forward-network source and a return-network
  target — which write wins is unresolved), lazy direction selection,
  recursion when a borrower is itself an owner, and authority rules in
  general for who may initiate a forward/return cycle. Numbering mirrors
  the project's existing precedent for a closely related follow-up
  sub-change (3b/3b2, 4b/4b2) — but note the distinction explicitly:
  those were follow-ups *discovered* during their parent's own
  implementation, whereas `5b2` is a *pre-existing*, spec-declared
  blocker, known before `5b` is even filed. Do not file `5b2`'s own
  `openspec` proposal until the REQ-INOUT-002 addendum exists.
- **5c. Compiled-execution optimization** (Q9) — ready to scope now,
  following Q9's own recommended approach: walk a *frozen*
  `ComponentGraph`, emit a direct call sequence per `DependencyNetwork`
  equivalent to the interpreted `update()` traversal (REQ-REV-006),
  exploiting the stated low fan-out (0–3) to avoid dynamic
  dispatch/lookup at runtime, and keep the interpreted path alive as a
  reference oracle (REQ-REV-009) rather than deleting it once compilation
  exists. Q9's own gate — "the reference (interpreted) implementation
  being correct and validated first" — is already satisfied: the
  interpreted implementation Q9 would compile (frozen `ComponentGraph`,
  `DependencyNetwork` walk, demand-driven `update()` from Phase 1–2;
  `MethodGraphNode` invocation, callback wiring, route handles from Phase
  3–4) is landed and exercised by each sub-change's own pFUnit suite.
  REQ-REV-009's "validated" does not require the stronger Phase 6 entry
  bar (legacy `StateRegistry` retired at production scale) — Q9 only
  needs the interpreted path to be the trusted oracle, which this
  document's own Phase 6 framing (§20.4.2) already assumes stays alive
  indefinitely, not something gated on legacy removal. Independent of
  `16`/`18` in principle — it compiles whatever `DependencyNetwork`/
  `TransformGraphNode` structure exists at freeze time, generically — but
  sequence it **after 5a**: if `18` lands first, its characteristic-driven
  `TransformGraphNode`s (`ConvertUnitsTransform`, the precision-conversion
  transform, `RegridTransform`) are already part of what 5c needs to
  compile correctly, avoiding a revisit once 5a's new Transform chains
  exist. `5b`'s direct-alias case adds no new Transform, so 5c has no
  ordering dependency on `5b`.

**Resulting order and independence:**

```
Phase 1-4 (landed)
   |
   +--> 5a  StateItemCharacteristic hierarchy        (ready now)
   |       |
   |       +--> 5a2  Remaining Characteristic subclasses  (ready now, after 5a)
   |       |
   |       v
   +--> 5c  Compiled-execution optimization          (ready now, after 5a)
   |
   +--> 5b  Ordinary inout, direct-alias case         (landed, independent)
             |
             v
         5b2 Ordinary inout, general case             (BLOCKED: needs REQ-INOUT-002 addendum)
```

`5a` and `5b` have no dependency on each other and MAY be filed in either
order or in parallel. `5a2` depends only on `5a` (not on `5c`/`5b`/`5b2`)
and MAY be filed any time after `5a` lands, independently of `5c`. `5c`
should follow `5a` (not `5a2` — `5c` compiles whatever Transform chains
exist at freeze time, generically, so it has no ordering dependency on
`5a2`'s own, mostly-non-executing `build_transform`s either way).
`5b2` is not filed until its design addendum exists.

**Repo/tooling note (extends §20.4.1's/§20.4.3's own notes).** Phase 5
code, for whichever sub-change is filed, lives in the MAPL repo/checkout,
same as Phase 3 and Phase 4: `5a`/`5b`/`5c` all build on landed Phase 1–4
MAPL-repo code (`GraphStateItem`, `ComponentGraph`, `DependencyNetwork`,
`MethodGraphNode`, callback wiring, route handles), so no
repo-separation saving (§20.2) applies here either.

### 20.4.5 Phase 7 sub-sequencing

**Discovered gap, not previously tracked by any phase above.** Surfaced
2026-10-08 during review of `5a2`
(`openspec/changes/state-item-characteristic-subclasses`): legacy
`ClassAspect` (`superstructure/generic/specs/ClassAspect.F90`) is the
*shape*-dispatch slot in `VariableSpec`'s `AspectMap` — orthogonal to
every per-characteristic aspect (units, geometry, vertical grid, ...)
`5a`/`5a2` already have graph-native analogs for. `ClassAspect` decides
which other aspects are even relevant (`get_aspect_order`), owns the
actual ESMF payload (`create`/`activate`/`allocate`/`destroy`), and — for
three concrete subclasses — performs a real *shape-changing* conversion
via `make_transform`, something no other legacy aspect does and nothing
in the graph architecture has any analog of today:

| Legacy pair | Transform | Cardinality |
|---|---|---|
| `BracketClassAspect` -> `FieldClassAspect` | `TimeInterpolateTransform` | 1:1 |
| `VectorBracketClassAspect` -> `VectorClassAspect` | `TimeInterpolateTransform` | 1:1 |
| `ExpressionClassAspect` -> `FieldClassAspect` | `EvalTransform` | N:1 (named refs) |

A fourth and fifth legacy mechanism — `WildcardClassAspect` and
`ServiceClassAspect`'s own accumulation role — are *not* shape-conversion
at all and do not belong in that table: both have dead/unreachable
`make_transform` (`matches`/`supports_conversion_*` hard-`.false.`) and
are resolved through a `connect_to_export` side-channel fed by
`MatchConnection` regex-expanding one import pattern against the
flattened sibling export namespace *before* any `AspectMap`-level
dispatch runs — legacy itself does not treat these two uniformly with
the rest of `StateItemAspect`, so graph needing separate machinery for
them is not a new asymmetry; it is the same one legacy already has.

`GraphBuilder.F90` already has an explicit, pre-existing scope boundary
for the pattern-based case: "Wildcard/callback/reexport/simple
connections: out of scope for this slice — deliberately skipped." This
was never promoted to a tracked roadmap gap; this phase is that
promotion, done accurately (§20.4.2's own Phase 6 cleanup list names
`WILDCARD`/`EXPRESSION`/`SERVICE` as missing graph-native *itemType
values* but says nothing about missing *dispatch behavior*, and does not
mention `BRACKET`/`VECTOR`/`VECTORBRACKET` at all — that list entry is
itself incomplete, left as-is rather than rewritten, per this document's
own "append, don't silently drop" discipline for growable lists).

**Does not belong in Phase 5's `StateItemCharacteristic` family
(`5a`/`5a2`)**, despite the naming proximity — two reasons: (1) `5a`
design.md D6 deliberately keeps that whole hierarchy independent of
`GraphBuilder.F90`'s real connection-resolution path, exercised only by
synthetic-node pFUnit tests; shape-dispatch, to be useful, MUST be wired
into real resolution. (2) The architecturally correct home is the
*other*, already-live graph-native characteristic family —
`09-extension-reuse.md`'s `Characteristic`/`CharacteristicMap`/
`CharacteristicId` (3c), already consulted by `GraphBuilder.F90`'s real
`resolve_one`/`ExtensionResolution.F90` chain-building
(`find_mismatched_characteristics`/`build_chain`) for Units and
VerticalGrid today. A new `CharacteristicId` whose `needs_extension_for`
checks `GraphStateItem%variant()` compatibility and whose
`build_transform` returns the Bracket/VectorBracket-equivalent
`TransformGraphNode` would be walked by the *same* `build_chain` loop,
uniformly with Units — preserving exactly the uniformity property
legacy's own `can_connect_to`/`make_extension` loop has for `ClassAspect`.
`variant()` is already a first-class `GraphStateItem` property
(REQ-SI-002b), not a sparse-map characteristic entry, which if anything
mirrors `ClassAspect`'s own privileged (decides-aspect-order,
owns-the-payload) role in legacy more faithfully than modeling shape as
an ordinary characteristic would.

**Three genuinely distinct mechanisms, not one** — do not file a single
change spanning all three:

- **7a. Shape-changing 1:1 dispatch** (`Bracket`->`Field`,
  `VectorBracket`->`Vector`) — mechanically the simplest: a new
  `CharacteristicId`/`Characteristic` subclass in the live
  `graph/extension-reuse` family, triggered by `variant()` mismatch
  instead of a value comparison, dispatched by the existing `build_chain`
  loop exactly like `UnitsCharacteristic` is today. **Lowest urgency**:
  zero `Test_Scenarios` fixtures exercise `BRACKET`/`VECTORBRACKET` end
  to end (`Test_BracketClassAspect.pf`/`Test_VectorBracketClassAspect.pf`
  are unit-level only, `MAPL.generic.aspects`) — nothing currently forces
  this before the other two.
- **7b. Expression -> Field, N:1** — needs `EXPRESSION` added to the
  graph-native `MAPL_StateItem_Flag` vocabulary (currently absent, unlike
  `BRACKET`/`VECTOR`/`VECTORBRACKET` which already have flag values) and
  a `GraphBuilder`-level hook resolving N named references —
  `build_transform`'s two-item `(src, dst)` signature cannot do this
  alone, mirroring legacy's own exception for
  `ExpressionClassAspect%make_transform` (it already reaches outside
  normal scope via `registry%extend()` per referenced variable). Likely
  shaped as a dedicated hook analogous to `4e`'s own `run_geometry_hook`
  precedent (graph-wide resolution, not an ordinary two-item
  `Characteristic`) rather than an extension of 7a's mechanism.
  **Working hypothesis, not yet designed**: an Expression item is likely
  best modeled as a composite `StateItemNode` (reusing the already-shipped
  `composite-state-spec` `declare_member` mechanism — the same mechanism
  `4c`'s `CallbackStateBinding` and `4f`'s `VerticalGrid` already reuse
  for "a state item needs independently-addressable sub-structure" — one
  member per referenced variable), with `TransformGraphNode`'s existing
  N-input port support (already generic since Phase 2,
  `10-transforms-and-ports.md`) doing the N:1 fan-in from those members.
  Needs real design work before implementation, not assumed solved here.
  `Test_Scenarios` evidence: 3 fixtures (`expression`, `expression_match`,
  `expression_defer_geom`) — a real forcing function.
- **7c. Wildcard/Service pattern-based multi-source fan-in** — hardest
  and least understood. Not a `Characteristic`/shape-dispatch problem at
  all: legacy's own mechanism is connection-*topology* discovery (regex
  against the flattened sibling export namespace,
  `VirtualConnectionPt%matches`/`MatchConnection`) happening *before* any
  per-item dispatch, followed by N-way accumulation into one persistent
  destination instance (`WildcardClassAspect%matched_items`).
  `DependencyNetwork%validate()` already explicitly rejects multiple
  producers writing one state item in the same network — that invariant
  is almost certainly correct and should not be weakened to accommodate
  this. Working hypothesis: model each pattern match as its own
  independently-produced composite *member* (reusing
  `composite-state-spec` again) rather than true multi-producer
  semantics — not yet validated against the real sharing/identity
  mechanics `18-state-item-characteristics.md` §18.7 already uses for
  Geom/VerticalGrid. Further complicated by cross-level export
  visibility: `openspec/changes/archive/
  2026-09-16-graphbuilder-advertising-connections`'s own
  `Test_GraphBuilderEquivalence.pf` already tried and rejected porting
  `statistics`/`history_1`/`history_wildcard`/`extdata_1` as equivalence
  targets, specifically because those scenarios need `StateRegistry`'s
  cross-level `propagate_exports`/`propagate_unsatisfied_imports`
  machinery that `GraphBuilder` does not consult beyond `3b2`'s single-
  import upward bubbling — a second, only-partially-solved prerequisite,
  not something 7c can assume away. `Test_Scenarios` evidence: 1 fixture
  for Wildcard (`history_wildcard`), 2 (+1 unused) for Service — real but
  entangled forcing functions.

  **Scope narrowing, recorded in §20.4.11.** Per explicit project
  direction, Service is removed from this phase's own scope: it is
  retired outright, replaced by callbacks
  (`CallbackInterface`/`CallbackInterfaceRegistry`, Phase 4c/4d, already
  landed), not migrated to a graph-native structural analog the way
  Wildcard is. This phase's own future `openspec` change scopes to
  Wildcard-only; Service's own retirement is tracked as a Phase 6b
  audit-and-delete growable-list item (§20.4.2/§20.4.11) instead. The
  Wildcard/Service joint-problem description above is left otherwise
  unmodified — it remains an accurate description of why the two were
  originally bundled, even though the disposition decision has since
  diverged.

**Recommended pilot, if/when this phase is picked up**: `vector_1`
(`VectorClassAspect`, same-shape, no conversion needed at all —
`matches` with per-component `standard_name` comparison only) as a smoke
test of whether `GraphBuilder`'s existing extension-chain model already
generalizes to a second characteristic cleanly, *before* attempting
`7a`'s genuine shape-conversion case or `7c`'s harder topology problem.
`Vector` itself needs no new mechanism beyond what `3c`/`5a2` already
provide — it is scoping/validation work, not new architecture, and tests
the ground floor before building the harder floors on top of it.

**Repo/tooling note (extends §20.4.1's/§20.4.3's/§20.4.4's own notes).**
Phase 7 lives in the MAPL repo/checkout, same as Phases 3-5: all three
sub-mechanisms build on landed `GraphBuilder.F90`/`graph/extension-reuse`/
`composite-state-spec` MAPL-repo code.

### 20.4.6 Phase 8 scope: `VariableSpec` validation

**Discovered gap, not previously tracked by any phase above.** Surfaced
2026-10-09 during post-archive review of `ordinary-inout-direct-alias`
(`openspec/changes/archive/2026-10-09-ordinary-inout-direct-alias`): a
reviewer asked where the new callback+inout mutual-exclusion guard
"should" live, and tracing the actual call graph showed every
`VariableSpec` consistency concern — not just this one — has nowhere
principled to live today.

**What exists:**
- `verify_variable_spec` (`VariableSpec.F90`) already checks
  `state_intent`/`short_name`/`regrid` consistency via
  `VariableSpec_private.F90`'s `verify_state_intent`/`verify_short_name`/
  `verify_regrid` - a real, if incomplete, validation scaffold.
- It is module-`private`, absent from `mapl_VariableSpec_mod`'s own
  `public ::` list, and has zero call sites anywhere in the repo,
  including from `make_VariableSpec` itself. Dead code since it was
  written.

**What this means for the three already-landed marker fields:**
`callback_interface_id`, `state_item_variant`, and `is_inout_borrower`
each followed the same precedent ("mark the item, not a new itemType,"
set via plain post-construction field assignment, no `make_VariableSpec`
keyword, explicitly deferring YAML/SetServices exposure as out of scope
in each one's own design.md). Confirmed by direct search: every
assignment of any of the three, anywhere in the repo, is in a pFUnit test
file (`Test_VariableSpecCallback.pf`, `Test_GraphGeometryHook.pf`,
`Test_GraphBuilder.pf`, `Test_VariableSpecInout.pf`). Neither real
production construction path exposes them:
- `MAPL_GridCompAddSpec`/`gridcomp_add_spec` (`MAPL_Generic.F90`) - the
  public macro real GEOS components call (`gridcomps/statistics/*.F90`
  and others) - builds a `VariableSpec` from its own keyword-argument
  list, which does not include any of the three, then pushes directly
  into `component_spec%var_specs` (bypassing `ComponentSpec%add_var_spec`).
- YAML parsing (`ComponentSpecParser/parse_var_specs.F90`) - same shape:
  builds via `make_VariableSpec(...)` with its own keyword list (also
  missing all three), pushes directly into the vector.

Each deferral was individually deliberate and documented at the time -
this phase is the first point anyone asked whether the accumulation of
three such deferrals, with no shared validation or exposure mechanism
among them, is itself a gap. It is: `ordinary-inout-direct-alias`'s own
mutual-exclusion guard is reachable only from `GraphBuilder.F90`'s
connection-resolution path, meaning a component that declares the
nonsensical combination but never gets a matching connection resolved
against it sails through completely unchecked today.

**Scope for this phase:**
1. Make `verify_variable_spec` real: export it (or an equivalent single
   choke point), and have it also check the cross-field consistency
   rules current code only checks piecemeal or not at all (starting with
   `ordinary-inout-direct-alias`'s callback+inout mutual exclusion -
   `GraphBuilder.F90`'s own copy should be removed once this lands, not
   kept as a redundant second check).
2. Decide, as a planned up-front design decision (same discipline this
   document already asks of other open points, e.g. Phase 4's 4b for
   REQ-MTH-011(c)): does validation run at `make_VariableSpec`'s own
   construction time (requires migrating `callback_interface_id`/
   `state_item_variant`/`is_inout_borrower` from post-construction plain
   assignment to real constructor keyword arguments - the more invasive,
   more principled option), or at both real production choke points
   (`gridcomp_add_spec` and `parse_var_specs.F90`'s per-item construction
   loop - less invasive, but two call sites to keep in sync, and still
   does nothing for construction via direct field assignment in test or
   future code that does not go through either)?
3. Whichever mechanism is chosen, decide whether `callback_interface_id`/
   `state_item_variant`/`is_inout_borrower` should finally get real
   production exposure (keyword args on `MAPL_GridCompAddSpec` and/or new
   YAML fields) as part of this phase, or whether validation should ship
   first while production exposure for each field remains its own,
   separately-scoped follow-up per capability.

**Not blocking anything above or already landed** - this is independent
cleanup/hardening work on construction-time correctness, not new graph
capability; it can be picked up at any time.

### 20.4.7 Phase 9 scope: `GraphResolutionEntry` record type

**Discovered gap, not previously tracked by any phase above.** Flagged
in review discussion prior to this phase, the same way `QualifiedExportEntry`
(`GraphBuilder.F90` ~line 297) replaced an earlier string-keyed
representation for the qualified-export namespace — the reviewer
discussion at the time noted the two resolution-report collections
(`unresolved_imports`, `unsupported_characteristics`) should eventually
get the same treatment, but no `openspec` change or roadmap entry was
ever filed to track it. The gap was effectively lost until this phase's
own review caught the omission — exactly the kind of drift §20's own
header asks this document to guard against.

**What existed:** `graphbuilder_check_unsatisfied_imports` and
`graphbuilder_resolve_connections` populated `type(StringVector)`
out-arguments by concatenating `component_name // ':' // short_name`
(plus, for `unsupported_characteristics`, one more `':' // reason`
segment) at ~10 call sites. The field count varied by call site (plain
`component:short_name` for some `unsupported` cases, a third `:reason`
segment for others), nothing prevented `short_name`/`reason` themselves
from containing a literal `:`, and any caller wanting the identity
(rather than a log-ready string) had to split on `:` itself.

**Landed** (`openspec/changes/graphbuilder-resolution-entry`):
`GraphResolutionEntry` (`component_name`/`short_name`/`reason`, all
`character(:), allocatable`, `reason` set to `''` where not applicable)
plus a generated `GraphResolutionEntryVector`/
`GraphResolutionEntryVectorIterator` (gFTL2 vector template
instantiation, mirroring `mapl_VariableSpecVector_mod`'s own
`VariableSpec`/`VariableSpecVector.F90` split), in two new files
(`GraphResolutionEntry.F90`, `GraphResolutionEntryVector.F90`). Both
`GraphBuilder.F90` output-argument types and every push-back call site
were converted; the two logging hooks
(`graphbuilder_run_activate_hook`/`graphbuilder_run_connect_hook`) read
the three fields directly instead of logging the opaque joined string;
`Test_GraphBuilder.pf`'s ~15 string-literal assertions became per-field
comparisons. Pure internal representation change — no requirement or
scenario in `openspec/specs/graph/graph-builder/spec.md` changed
(`skip_specs: true`), and no production caller outside
`GraphBuilder.F90`'s own two hooks existed to begin with (confirmed by
repo-wide search before landing).

**Not blocking anything above or already landed** - independent
internal-quality hardening, not new graph capability.

### 20.4.8 Phase 10 sub-sequencing

**Discovered gap, not previously tracked by any phase above.** Surfaced
2026-10-09 during planning review (`openspec/changes/
phase-10-sub-sequencing`): §20.4.2's own Phase 6 entry criterion ("do not
retire `StateRegistry`/`ExtensionFamily`/the aspect system until every
real production code path... has a graph-native replacement that has
been *validated*, not merely built, as the default behavior") has no
concrete validation suite to point to. Direct inspection of landed
Phase 3-5 code shows the gap is not cosmetic: ordinary (exact-match)
`StateItemNode`s are never materialized with a real `ESMF_Field`
(`advertise_one()` leaves the payload default; `resolve_one()`'s
exact-match branch only adds a dependency edge; real `FieldCreate` via
`materialize_field_extension()` fires only on the mismatch/extension
branch, gated off by default); nothing populates a real `ESMF_State`
from graph-side `StateItemNode`s (the only thing
`superstructure/generic/tests/Test_Scenarios.pf` — the ~30-fixture,
real-YAML-configured regression suite this project treats as realistic
client-code coverage — ever inspects is the `ESMF_State` tree legacy's
own `StateRegistry%add_to_states` populates); `TypeKindCharacteristic`
(Phase 5a) is never inserted by `GraphBuilder.F90`'s own
`build_characteristics()`, so a real precision-only mismatch is
currently invisible to `resolve_one`'s mismatch detection; no fixture
exercises a genuine two-characteristic extension chain on one edge with
clean, hand-checkable naming; and nothing constructs a real
`MethodGraphNode` for an ordinary component's own `GENERIC_RUN_USER`
phase (the only real construction site, `build_callback_method_binding`,
is the deliberately narrower callback-wiring path — Phase 4b's own
proposal.md named ordinary-phase construction an explicit, not-required
deferral).

Like Phase 3 (§20.4.1), Phase 4 (§20.4.3), and Phase 5 (§20.4.4), this
does not fit a single spec-driven change proposal without an
unreasonable context/cost footprint. Unlike those three, this phase's
four sub-changes are **not** independent tracks — they are a strict
dependency chain, each needing the previous one's result to have
anything to validate against. Split into four ordered sub-changes
accordingly, rather than filing one proposal spanning all four.

**This phase is a prerequisite for §20.4.2's own entry criterion, not a
renumbering of it.** Phase 6 (legacy retirement) is unchanged by this
section; §20.4.8 is the validation suite Phase 6's own "validated, not
merely built" language already presupposes but never named. Numbering
follows this document's own established precedent (Phases 7/8/9): a
newly-discovered phase gets the next free forward number, not a
reordering of existing phase numbers to match dependency order.

- **10a. State materialization + 3-mode toggle** — ordinary
  (exact-match, non-mismatched) `StateItemNode`s get a real
  `FieldCreate()`'d payload at `GENERIC_INIT_ACCEPT_TRANSFER` time,
  removing today's restriction of real materialization to the
  mismatch/extension branch only; a new graph-side equivalent of
  `StateRegistry%add_to_states` walks a component's `ComponentGraph`
  `StateItemNode`s and, in `graph` mode only, populates the real
  `ESMF_State`; `mapl_GraphMode_mod` gains a 3-way selector
  (`registry` / `graph` / `both`) in place of today's binary
  `graph_native_enabled()`. **No `ESMF_State` name collision by
  construction**: `both` mode never inserts graph-materialized data
  into the shared `ESMF_State` that `check()`-style assertions
  observe — it is compared out-of-band, reading the `StateItemNode`'s
  own payload directly (`node%get_payload()%get_field()`), exactly as
  `Test_GraphBuilderEquivalence.pf` already does today for its two
  existing tests. `registry` mode (today's default) and `graph` mode
  (true replacement — legacy's own `add_to_states`/`process_connections`
  skipped entirely for that component) are mutually exclusive by
  construction: only one producer ever inserts a given name into one
  `ESMF_State`. `graph` mode is not attempted by this sub-change or any
  other 10a-10d sub-change; it remains available as a later mode once
  `both` mode has run clean across the fixture set this phase builds —
  "demonstrate before replace." Depends only on landed Phase 1-3
  (`ComponentGraph`, `StateItemNode`, `GraphBuilder`'s existing
  `advertise_one`/`resolve_one`) — no dependency on 3c's extension
  machinery beyond what already exists.
- **10b. Generalized equivalence harness, "both" mode, flat fixtures**
  — a parameterized pFUnit suite shaped like `Test_Scenarios.pf`'s own
  `ScenarioDescription`/testParameter machinery, generalizing
  `Test_GraphBuilderEquivalence.pf`'s existing (two-test) pattern:
  each fixture runs through `graphbuilder_run_*_hook` in `both` mode,
  asserting legacy's real `ESMF_State` and graph's shadow
  `StateItemNode` payload agree (existence, typekind, units, geom,
  shape; value once a run phase has executed). New, clean fixtures —
  deliberately not a reuse of existing `scenario_1`/`scenario_2`-style
  naming, since that naming emerged organically rather than being
  designed for this purpose — each covering one orthogonal
  no-transform capability (plain field, bundle/vector field, 3D field,
  ungridded dims). Depends on 10a.
- **10c. `TypeKindCharacteristic` wired into real resolution + a real
  precision-cast transform** — **amended by
  `openspec/changes/phase-10c-typekind-unification`**: builds on the
  existing, already-shipped `TypeKindCharacteristic`
  (`ValueCharacteristic`-based,
  `superstructure/generic/graph/TypeKindCharacteristic.F90`, landed by
  the archived `state-item-characteristics` change) via an
  adapter/bridge at `GraphBuilder.F90`'s own
  `build_characteristics()`/`CharacteristicMap` insertion point, giving
  that existing type a real adaptation capability (a
  `build_transform`-equivalent) — **not** a second,
  `Characteristic`-extending `TypeKindCharacteristic` mirroring the
  existing `units`/`vertical_grid` entries, as this entry originally
  read. That original wording would have collided outright — same
  module name `mapl_TypeKindCharacteristic_mod`, same type name
  `TypeKindCharacteristic` — with the already-shipped type: a literal
  compile-time collision, unlike the conceptual-only overlap the
  `Units`/`PhysicalUnits`, `Geom`/`Geometry`, and
  `VerticalGrid`/`VerticalCoordinate` pairs already have (each of those
  resolved by renaming the newer side; 10c is the first case where the
  *older* hierarchy's own planned growth is what lands on an
  already-taken name). Whether the bridge is an optional adaptation
  method added to `StateItemCharacteristic`/`ValueCharacteristic`
  itself, or a `Characteristic`-side wrapper holding a
  `TypeKindCharacteristic` by composition, is left to 10c's own future
  design.md to pick once its real implementation constraints are in
  hand (`phase-10c-typekind-unification`'s own design.md, Decisions).
  Framed as the first narrowly-scoped pilot for reconciling
  `graph/extension-reuse`'s `Characteristic` family with
  `graph/state-item-characteristics`'s `StateItemCharacteristic` family
  one axis at a time — not a mandate to migrate `Units`/`VerticalGrid`/
  `Geom` too, and not a reopening of either family's own prior
  deliberate-independence decisions (`state-item-characteristics`
  design.md D6; `state-item-characteristic-subclasses` design.md
  D1/D3). `CopyTransform`
  (`18-state-item-characteristics.md`'s own placeholder name, flagged
  there as poor and in need of a better one before implementation —
  this sub-change's own design.md picks the real name, checked against
  `superstructure/generic/transforms/` and `superstructure/generic/graph/`
  for a collision first, following the precedent `UnitsConverterTransform`/
  `ConvertUnitsTransform`'s own existing collision already set) gets a
  genuine R4<->R8 cast implementation, mirroring `UnitsConverterTransform`'s
  shape (`FieldPointerUtilities`-based, no legacy coupling); one new,
  clean two-characteristic fixture — a single edge whose export and
  import mismatch on BOTH `units` and `typekind` simultaneously (e.g. a
  `hPa`/`R4` producer feeding a `Pa`/`R8` consumer), with
  hand-checkable values. `ExtensionResolution.F90`'s own `build_chain`
  loop already walks an arbitrary-length `mismatched_kinds(:)` array
  generically (confirmed by reading it directly) — no chain-building
  code changes are needed; this sub-change only supplies the second
  characteristic's own detection and transform. **Replaces, does not
  reuse, the existing `invalidate/` fixture** for this specific
  capability: `invalidate/` already exercises a genuine
  `units`+`typekind` chain (confirmed by its own `description.md`:
  "we use units and precision change as the two couplers as their
  consequences are easy to compute directly"), but bundles that with
  `numsteps: 2`/`run:` runtime re-invalidation semantics and
  non-obvious `A`/`B`/`T`/`T(1)`/`T(2)` naming in one fixture — 10c's
  own new fixture isolates the structural (chain exists, correct values
  once) half with clear naming; 10d below extends this same new
  fixture, not a second one, to add the runtime-re-invalidation half,
  deliberately rebuilding what `invalidate/` already combined but as
  the two separately-landable pieces this phase's own sub-sequencing
  requires. Depends on 10a/10b (uses the same harness, extended to
  assert the chain's intermediate materialized values, not just the
  final one).
- **10d. Ordinary-phase `MethodGraphNode` construction + real run-loop
  wiring** — `GraphBuilder` constructs exactly one `MethodGraphNode`
  per component, wrapping that component's own `GENERIC_RUN_USER`
  phase through the already-real, already-landed
  `GridCompDriverPhaseInvoker` (Phase 4b) — no new invoker/adapter
  type, only a new construction call-site following
  `build_callback_method_binding`'s own established
  `declare_argument`/`bind_argument`/`register_node` pattern, keyed to
  the component's own advertised ports instead of a
  `CallbackInterface`'s declared arguments. A new call site — likely
  `OuterMetaComponent`'s own `run_user.F90`, mirroring how
  `gb%run_connect_hook` already sits alongside legacy's `connect()` at
  `ACCEPT_TRANSFER` — invokes this `MethodGraphNode` (triggering
  `update()`/`advance()` per REQ-MTH-003a's existing discipline)
  alongside legacy's own run call, respecting the same 3-mode toggle
  10a introduced. Extends 10c's own fixture (reused, not a new one) to
  additionally exercise the chain re-propagating correctly when the
  producer's exported value changes across a second timestep.
  **Sequenced strictly after 10a-10c, not alongside them**: 10a-10c
  close gaps in already-landed Phase 3-5 code; 10d is new mechanism (no
  prior real construction site for an ordinary-phase `MethodGraphNode`
  exists anywhere in the codebase), deliberately held until the
  lower-risk sub-changes are demonstrably working first. Depends on
  10a-10c; depends on Phase 4a/4b's already-landed `MethodGraphNode`/
  `GridCompDriverPhaseInvoker`/`DriverResolver` and Phase 2's
  `NodeRevision`/`ComponentGraph%update()` — no new dependency beyond
  what Phase 1-4 already provide. Does not depend on and does not
  reopen 4b2's driver-map unification (evaluated and declined, §20.4.3)
  or Q9/5c's compiled-execution optimization — 10d invokes the existing
  interpreted path only.

**Explicitly out of scope, named as prerequisites rather than
reopened.** Three gaps this phase's own fixture set deliberately does not
attempt to unblock, each already named elsewhere in this document (the
third discovered, not merely deferred, during 10b's own implementation):

- **`graph`-mode-alone cross-component population is not yet
  implemented, confirmed by direct experiment during 10b.** 10a's own
  "demonstrate before replace" framing (10a entry above) assumes `both`
  mode validates the materialization/comparison logic before anyone
  switches a fixture's selector to `GRAPH_MODE_GRAPH` alone. Attempting
  exactly that switch on 10b's own flat two-sibling `plain_field`
  fixture (`comp_src`/`comp_dst`, no re-export at the `cap` level) fails
  immediately at `ESMF_StateAdd` (`ESMF_RC_ARG_INCOMP`,
  `GraphBuilder.F90` `populate_one`): `graphbuilder_populate_state()`'s
  "ordinary cross-component case" only ever inserts a resolved field
  into the *parent*'s own `tmp_states`/`outer_states` (the states
  `initialize_accept_transfer.F90` builds for `cap` itself), never into
  either sibling's own real export/import `ESMF_State` — correct only
  for an actual re-export the parent itself declares, which this loop
  does not check for before inserting. No code path exists today that
  populates a sibling-to-sibling connection's own real `ESMF_State`
  under `GRAPH_MODE_GRAPH`. Not filed as its own `openspec` change yet;
  blocks any future sub-change (10c/10d or Phase 6 itself) from
  actually flipping a fixture to `graph`-mode-alone until a real fix
  lands in `graphbuilder_populate_state()`/`populate_one` — the same
  "validated, not merely built" bar §20.4.2's own Phase 6 entry
  criterion already requires, now with a concrete, reproducible failure
  to point to instead of an unverified assumption.
- **Cross-component unresolved-import propagation (3b2)** — named a
  required, committed follow-up in §20.4.1 but never actually filed as
  its own `openspec` change; `graphbuilder_check_unsatisfied_imports()`'s
  own activate-time result is still only logged and discarded today.
  Every scenario fixture of the "propagated" shape (`statistics`,
  `statistics_real`, `history_1`, `history_wildcard`, `extdata_1` — the
  real connected item lives on a grandchild, surfaced only through
  `StateRegistry`'s `propagate_exports`/`propagate_unsatisfied_imports`)
  remains unreachable by `GraphBuilder` until 3b2 lands. 10a-10d's own
  fixture set stays flat (non-propagated) throughout for this reason.
- **Wildcard/Service pattern-based multi-source fan-in (Phase 7c,
  §20.4.5)** — already recorded there as "hardest and least
  understood." Fixtures of this shape (`history_wildcard`,
  `service_service`, `service_with_options`) are additionally expected
  to have no long-term graph-native analog at all: Service-style
  fan-in is intended to be replaced by callbacks (Phase 4c/4d), not
  reproduced structurally — not every legacy `Test_Scenarios` fixture
  needs or will get a graph-native counterpart.

**Repo/tooling note (extends §20.4.1's/§20.4.3's/§20.4.4's own notes).**
Phase 10 code, for whichever sub-change is filed, lives in the MAPL
repo/checkout, same as Phase 3-5: `10a`-`10d` all build on landed
Phase 1-5 MAPL-repo code (`GraphStateItem`, `ComponentGraph`,
`DependencyNetwork`, `MethodGraphNode`, `GridCompDriverPhaseInvoker`),
so no repo-separation saving (§20.2) applies here either.

### 20.4.9 Phase 11 (real run-phase value propagation through an extension
chain — discovered gap, not previously tracked by any phase above)

**Landed** (`openspec/changes/graph-child-state-population`): the
confirmed `GRAPH_MODE_GRAPH`-alone cross-component population gap
§20.4.8's own "Explicitly out of scope" list named (`ESMF_RC_ARG_INCOMP`
at `ESMF_StateAdd`, 10b's own direct experiment) is fixed —
`graphbuilder_populate_state`'s cross-component loop now inserts each
sibling's own resolved field into *that sibling's own* real
`ESMF_State` (reached via the existing one-way `get_child_outer_meta`
parent-to-child accessor, never the reverse), using a
`MAPL_NamedAlias`-under-stable-name + `ESMF_StateAddReplace` pair
(matching legacy's own `FieldClassAspect%add_to_state` precedent) so
the insertion is safe across `GENERIC_INIT_PHASE_SEQUENCE`'s own fixed
two-pass `GENERIC_INIT_ACCEPT_TRANSFER` schedule. A new, narrowly-scoped
`graphbuilder_run_pull_hook` (called from `run_user.F90`, gated on
`GRAPH_MODE_GRAPH`) re-triggers any sibling connection's own
extension-chain transform via a plain `ComponentGraph%update()` pull —
architecturally guaranteed by `method-graph-node`'s own REQ-NODE-007 to
never invoke a `MethodGraphNode` or a second `GriddedComponentDriver%
run()`, so none of Phase 10d's own double-invocation hazard applies.
Also fixed in the same change: `resolve_one`'s own mismatch branch
never materialized the chain's own INPUT node (`export_node_id`) at
all — only the exact-match branch and the chain's final node were ever
materialized — now fixed by also calling `try_materialize_field_extension`
on `export_node_id`, using the export's own typekind/units (not the
import's).

**Explicitly out of scope, discovered during that same change's own
end-to-end demonstration attempt (`units_and_typekind_mismatch` under a
real `GENERIC_RUN_USER` phase) — the actual goal (a changing run-phase
value provably propagating through a real extension chain into a
sibling's own real state) remains unmet. Two further, separate gaps,
each confirmed by direct code reading, stand between here and that
goal:**

- **Multi-hop extension chains have no materialized intermediate
  nodes.** `ExtensionResolution.F90`'s own `build_chain` leaves every
  link's payload "unallocated — not yet realized" by design; only the
  chain's own FINAL node is ever materialized
  (`try_materialize_field_extension`, `GraphBuilder.F90`). Harmless as
  long as nothing ever executes the chain's own `compute()` for real
  (true for every fixture before this phase); a real two-characteristic
  chain (e.g. `units_and_typekind_mismatch`'s own `UnitsConverterTransform`
  → `TypeKindConverterTransform`) fails outright
  (`UnitsConverterTransform%compute()`'s own `output_item%get_field()`
  call, `GraphStateItem: esmf_field is not allocated`) the first time
  anything actually pulls on it. Fixing this needs
  `ExtensionResolution.F90`'s own `find_or_build_extension_chain`/
  `build_chain` to expose every intermediate `NodeId` (not only the
  final one) so each link can be materialized with its own correctly-
  evolved typekind/units/geom (the state after characteristic 1's own
  conversion has notionally applied, before characteristic 2's own
  conversion runs) — a real change to that module's own public
  interface, deliberately not attempted in
  `graph-child-state-population` (narrowed to a single-characteristic
  fixture, `units_mismatch_run_phase`, to avoid needing this).
- **A sibling connection's export-side proxy is never aliased to the
  real export field a run phase actually writes.** Even with the
  export-node materialization fix above, the materialized field is an
  *independent* `FieldCreate` — never a `MAPL_NamedAlias` of `comp_src`'s
  own real export field (the one `ConfigurableGridComp%run()` actually
  writes the YAML-configured `run:` value into). A real run phase's
  changed value therefore never reaches the proxy, confirmed by a
  resulting `NaN` (not a crash) when
  `graph-child-state-population`'s own demonstration test attempted to
  assert the real converted value after a real run phase. Fixing this
  needs the same "always alias, never duplicate" treatment D1-D6 of
  that change already applied to the *import* (sibling-receiving) side,
  mirrored onto the *export* (sibling-producing) side — not attempted
  there; that change's own demonstration test was narrowed to assert
  only static, init-time structural correctness (item count, typekind,
  units, real-field-allocated) rather than a propagated numeric value.

Not yet sequenced relative to Phase 6/7/8/9's own existing numbering
(this is the next free forward number, matching this document's own
established precedent for a newly-discovered phase); likely a
prerequisite for Phase 6's own entry criterion to ever be claimed for
any run-phase-bearing, extension-chain-mediated production code path,
alongside Phase 10d's own still-unresolved double-invocation hazard
(§20.4.8's own "Explicitly out of scope," unaffected by this phase).

### 20.4.10 Phase 11 sub-sequencing

Phase 11, as recorded in §20.4.9 above, is a flat two-item list with no
stated ordering, dependency relationship, or scope boundary between its
two gaps — the same "too heterogeneous for one spec-driven change"
problem Phase 3 (§20.4.1), Phase 4 (§20.4.3), Phase 5 (§20.4.4), and
Phase 10 (§20.4.8) each hit before implementation started on them. This
section extends §20.4.9's own gap inventory with that boundary; it does
not renumber or restate §20.4.9's own gap descriptions, only names each
gap's own sub-change scope, dependencies, and recommended order. See
`openspec/changes/phase-11-sub-sequencing/proposal.md` - Why for the
full rationale.

Split into two sub-changes, confirmed independent of each other (neither
needs the other's code to land first):

- **11a. Export-side field aliasing** — **landed**
  (`openspec/changes/phase-11a-export-field-aliasing`): closes §20.4.9's
  own second gap (export-side proxy never aliased to the real export
  field a run phase writes). Scope: give `try_materialize_field_extension`'s own
  `export_node_id` call site (`GraphBuilder.F90`) the same "always alias
  by name, never duplicate" treatment `populate_one` already applies on
  the import side — a real `MAPL_NamedAlias` of the field a run phase
  actually writes into, under the export item's own known `short_name`,
  rather than today's independent `materialize_field_extension`
  `FieldCreate`. Demonstrated against the already-landed
  `units_mismatch_run_phase` fixture (one-hop chain, so §20.4.9's own
  first gap does not block it) — `Test_GraphModeDemonstration.pf` now
  asserts the real, converted run-phase value (`10.` hPa -> `1000.`
  Pa) reaches `comp_dst`'s own real import state end-to-end, completing
  Phase 11's own stated goal for the single-characteristic case.
  Depended only on landed `graph-child-state-population` work (its own
  D1-D6 alias pattern, D7 pull hook, and the export-node materialization
  call site this sub-change modified) — no dependency on 11b.
- **11b. Multi-hop extension-chain intermediate materialization** —
  **landed** (`openspec/changes/phase-11b-chain-intermediate-materialization`):
  closes §20.4.9's own first gap (no materialized intermediate nodes).
  `find_or_build_extension_chain`'s own public interface
  (`ExtensionResolution.F90`) now also returns every intermediate
  `NodeId` `build_chain` computes internally (not only `final_node_id`)
  — a zero-size array on the reuse-hit path, truncated to only the
  links actually built on an early unsupported-characteristic exit. A
  new `GraphBuilder.F90` procedure, `materialize_extension_chain`,
  replaces each of the four `find_or_build_extension_chain` call
  sites' own prior single-final-node materialization with a loop over
  every returned intermediate `NodeId`, each materialized with its own
  correctly-evolved typekind/units (import's own value for every
  characteristic already resolved by that point in the chain,
  export's own value otherwise — reducing exactly to the prior
  single-node formula at the chain's own final link). Demonstrated
  against the already-landed `units_and_typekind_mismatch` fixture: a
  new `Test_GraphModeDemonstration.pf` case
  (`test_graph_mode_propagates_through_two_characteristic_chain`) runs
  a real `GENERIC_RUN_USER` phase and asserts the real converted value
  (`10.` hPa/R4 -> `1000.` Pa/R8, both unit and typekind conversion
  composed) reaches `comp_dst`'s own real import state end to end —
  fully achieving, not merely attempting, the stretch goal §20.4.10
  itself named.
  **Scope correction, discovered during implementation (recorded here
  per this document's own 3b/3b2, 4b/4b2 discipline for a
  review/implementation-discovered gap):** this entry's own original
  wording ("mirroring `try_materialize_field_extension`'s existing
  per-node materialization call" at "each of `GraphBuilder.F90`'s own
  four call sites") assumed all four call sites already called
  `try_materialize_field_extension` for a chain's own final node.
  Confirmed false for three of the four by direct code reading:
  `resolve_geometry_from_child`/`push_geometry_to_child`
  (`GraphBuilder_Geometry_smod.F90`) and `wire_callback_argument`
  (`GraphBuilder_CallbackWiring_smod.F90`) each call
  `find_or_build_extension_chain` and go straight to `add_dependency`
  — no materialization call ever existed there to extend. Per explicit
  user decision during implementation (expand scope rather than narrow
  it, but add no fabricated materialization against data that cannot
  support it): all four call sites were updated to compile against the
  new `intermediate_node_ids` output, but `materialize_extension_chain`
  was deliberately NOT wired into the three geometry/callback sites —
  `GeomCharacteristic%build_transform` always fails explicitly (no
  real horizontal-regrid Transform exists, Phase 13/REQ-GEO-002a) so
  the geometry mismatch branch is unreachable in practice (already
  guarded by an `_ASSERT`), and `wire_callback_argument`'s own
  `CharacteristicMap`s are always empty today (no real
  `CallbackArgumentSpec` declares units/typekind/vertical_grid yet) so
  its mismatch branch is never entered — neither site has a
  `VariableSpec` available even if it were reachable, which
  `materialize_extension_chain`'s own signature requires. Real
  materialization at these three sites is deferred to whichever future
  capability gives them one (a real horizontal-regrid Transform for
  geometry; a characteristics-aware `CallbackArgumentSpec` for
  callback wiring) — explicitly not attempted here.
  Depended only on landed Phase 1-5/10 work
  (`ExtensionResolution.F90`, `try_materialize_field_extension`) — no
  dependency on 11a's own code path (a different call site in the
  same procedure), confirmed independent as planned.

**Recommended order: 11a before 11b, but not a hard gate.** The two
sub-changes touch disjoint code and neither's own correctness depends on
the other having landed. The recommendation is about payoff order, not a
technical dependency: 11a alone already completes Phase 11's own goal
for the single-characteristic case, at zero chain-interface risk; 11b
alone, without 11a, would let a multi-hop chain materialize cleanly but
still could not demonstrate a *real* propagated value end-to-end,
because the chain's own export-facing input would still be the
independent, un-aliased `FieldCreate` 11a is scoped to fix. **Both have
now landed, in the recommended order, confirming this prediction
directly**: 11b's own demonstration test is the real
`units_and_typekind_mismatch`-style two-characteristic run-phase case
this paragraph anticipated.

**Neither sub-change reopens Phase 10d's own double-invocation hazard.**
Both are state-materialization/aliasing concerns (what a `NodeId`'s own
payload points to), not invocation-count concerns (how many times a
`MethodGraphNode` or `GriddedComponentDriver%run()` fires) — unaffected
by, and not re-litigating, `graph-child-state-population`'s own D7
architectural-safety argument. Neither sub-change touches 3b2's
cross-component unresolved-import propagation or Phase 7c's
pattern-based fan-in either — both remain exactly as recorded in
§20.4.8/§20.4.5, unrelated fixture families.

**Repo/tooling note (extends §20.4.1's/§20.4.3's/§20.4.4's/§20.4.8's own
notes).** Phase 11 code, for either sub-change, lives in the MAPL
repo/checkout, same as Phase 3-5/10: both 11a and 11b build on landed
Phase 1-5/10 MAPL-repo code (`GraphBuilder.F90`,
`ExtensionResolution.F90`, `graph-child-state-population`'s own alias
pattern), so no repo-separation saving (§20.2) applies here either.

### 20.4.11 Phase 6 sub-sequencing

Phase 6 (§20.4.2), like Phase 3 (§20.4.1), Phase 4 (§20.4.3), Phase 5
(§20.4.4), Phase 10 (§20.4.8), and Phase 11 (§20.4.10), does not fit a
single spec-driven change proposal without an unreasonable context/cost
footprint — §20.4.2's own entry criterion ("validated, not merely
built") and growable list bundle a dependency-ordered prerequisite set,
a three-step retirement sequence, and an open-ended cleanup list into
one undifferentiated phase, with no filed sub-change against any of it.
This section gives it the same ordered-sub-change treatment the other
five phases already received before their own implementation started.
It replaces §20.4.2's own single-sentence "(a) retire... once validated"
with a named, dependency-ordered prerequisite set and three concrete
sub-steps; §20.4.2's own "Purpose"/"Entry criterion" text stays in
place, unmodified — this section makes it concrete, it does not restate
or renumber it.

**Prerequisite set — Phase 6a (the actual retirement work) MUST NOT be
filed as its own `openspec` change until all of the following have
landed:**

1. **3b2** (cross-component unresolved-import propagation, named in
   §20.4.1, never filed — confirmed by direct search of
   `openspec/changes/archive`: no change directory is itself named for
   this gap, and no archived `design.md`/`proposal.md` records it as
   landed, only as repeatedly owed — see proposal.md - Why for the full
   fourteen-citation inventory) — without it, every "propagated"
   fixture (`statistics`,
   `statistics_real`, `history_1`, `history_wildcard`, `extdata_1`)
   stays permanently out of reach for any `GRAPH_MODE_BOTH` parity run,
   making 6a-1's own full-fixture pass impossible to attempt honestly.
2. **Phase 7a** (Bracket/VectorBracket 1:1 shape dispatch) and
   **Phase 7b** (Expression N:1) — `VariableSpec` construction itself
   depends on `StateRegistry` through exactly this dispatch path
   (`make_StateItemSpec`/`make_aspects`/`make_ClassAspect`,
   `VariableSpec.F90:568,590,813`, each taking a live `StateRegistry`
   pointer; `ExpressionClassAspect.F90:353` calls `registry%extend()`
   from inside construction). `StateRegistry` cannot be deleted while
   any construction path still calls into it structurally, independent
   of whether a given real configuration happens to use
   Bracket/Expression.
3. **Phase 7c, narrowed to Wildcard only** — see the scope-narrowing
   paragraph below. Needed for the same structural reason as 7a/7b:
   `WildcardClassAspect` is still a live `ClassAspect` subclass
   dispatched from `VariableSpec` construction until replaced.
4. **Phase 12** (new, §20.4.12 below) — 5a2's seven subclasses wired
   into real dispatch. Without this, a real production mismatch on any
   of those seven axes is invisible to graph-native resolution, so
   6a-1's parity run cannot claim coverage of legacy's own
   mismatch-detection surface, only three of its ten axes.
5. **Phase 13** (new, §20.4.13 below) — restart compatibility. Without
   this, flipping any component's materialization to graph-native
   (6a-2) silently breaks restart for that component, with no test in
   today's suite that would catch it (`RestartMode` metadata is simply
   absent, not wrong, so no assertion currently fails on it).

**Phase 6a, split into three ordered sub-steps** (replacing §20.4.2's
own single sentence with the sub-steps it always presupposed but never
named):

- **6a-1. Full-fixture `GRAPH_MODE_BOTH` parity run.** Extend the
  `graph/equivalence-harness` capability's own parameterized suite
  (Phase 10b's own `ScenarioDescription`/testParameter machinery) to
  cover every fixture in `superstructure/generic/tests/scenarios/` and
  `superstructure/generic/tests/graph-scenarios/` — not only the flat,
  non-propagated fixtures Phase 10b itself was deliberately scoped to
  (10b's own "no nested fixture — out of scope until 3b2 lands" note).
  Exit criterion: every fixture passes under `GRAPH_MODE_BOTH`,
  confirming legacy and graph-native agree before any default-mode
  decision is made. This is the "validated, not merely built" bar
  §20.4.2 already named, made concrete. Cannot be attempted honestly
  until the prerequisite set above has landed.
- **6a-2. Flip the default mode; retire the toggle.** Once 6a-1 is
  clean, change `mapl_GraphMode_mod`'s own default from
  `GRAPH_MODE_REGISTRY`/`GRAPH_MODE_BOTH` (whichever is current) to
  `GRAPH_MODE_GRAPH`, and begin removing the three-way selector itself
  (collapsing to graph-native-only). Explicitly gated on Phase 10d's own
  double-invocation hazard (§20.4.8's "Explicitly out of scope") being
  resolved by whichever future change resolves it — not assumed solved
  by this document.
- **6a-3. Delete legacy source.** `superstructure/generic/registry/`
  (`StateRegistry*.F90`), `superstructure/generic/specs/*Aspect*.F90`
  and `AspectId.F90`, `ExtensionFamily` and its own supporting types,
  `superstructure/generic/connection/*.F90`
  (`Connection`/`SimpleConnection`/`MatchConnection`/
  `ReexportConnection`), and the coupler-GridComp mechanism
  (`superstructure/generic/transforms/GenericCoupler.F90`,
  `CouplerMetaComponent.F90`, `superstructure/generic/couplers/`) — once
  6a-2's own flip has run clean in production for whatever burn-in
  period that sub-change's own design.md decides.

  **[UPDATED, `simpleconnection-graph-resolution`]** This bullet's own
  framing of `connection/*.F90` as purely legacy source needing
  wholesale deletion is now incomplete: `GraphBuilder`'s own
  graph-native resolution (`GraphBuilder_ConnectionResolution_smod.F90`)
  dispatches on `SimpleConnection`/`MatchConnection`'s own declared
  types directly (`select type`/`class is`, reading each one's
  `get_source()`/`get_destination()` accessors) — it does not merely
  read these types' data through a copy or an abstraction; it depends
  on the type declarations themselves persisting. `ReexportConnection`
  remains purely legacy (still unresolved by `GraphBuilder`, correctly
  falls to `class default` — see `openspec/specs/graph/graph-builder/
  spec.md`'s "Non-exact-match cases are not resolved by this
  capability" requirement). A future 6a-3 implementation must therefore
  either (a) keep `Connection`/`SimpleConnection`/`MatchConnection`
  (and their shared `ConnectionPt`/`VirtualConnectionPt` support types)
  as a small, retained data-model module independent of
  `StateRegistry`, deleting only the legacy `activate()`/`connect()`
  execution methods and `ReexportConnection` itself, or (b) migrate
  `GraphBuilder`'s own dispatch to a different, graph-native connection
  representation before this deletion can proceed — this bullet's own
  "once 6a-2's flip has run clean" gate is necessary but no longer
  sufficient; whichever sub-change attempts 6a-3 must resolve this
  dependency explicitly, not discover it mid-deletion.

**Phase 7c scope narrowing — Service removed.** §20.4.5's own existing
text bundles Wildcard and Service together as "Wildcard/Service
pattern-based multi-source fan-in." Per explicit project direction:
Service is retired outright, replaced by callbacks
(`CallbackInterface`/`CallbackInterfaceRegistry`, Phase 4c/4d, already
landed) — it does NOT get a graph-native structural analog the way
Wildcard does. 7c's own future `openspec` change scopes to Wildcard
only. Service's own `ClassAspect`/`AspectId` retirement moves to the
Phase 6b growable list below as an audit-and-delete item, not a
structural rebuild.

**Phase 6b's growable list — new entries appended** (the existing three
entries above are left unmodified, per this document's own "append,
don't drop" discipline):

- `write_formatted`'s dead debug-dump machinery
  (`StateRegistry.F90:85-86,120-127`,
  `StateRegistry_Lifecycle_smod.F90:42-125`) — confirmed zero
  production callers; delete with the rest of 6a-3, no separate
  migration needed.
- `ModelVerticalGrid%get_coordinate_field()`'s own second, independent
  `registry%extend()` call site (`ModelVerticalGrid.F90:273`) — already
  known to this document as 4f's own design rationale (why 4f took a
  different design path, §20.4.3's own 4f entry above) but never
  tracked as its own "second call site to retire" checklist item.
  Confirm no remaining caller needs this path's side effect before
  deleting it in 6a-3.
- `gridcomp_set_geometry`'s own direct single-item bypass of
  `StateRegistry%set_geometry` (`MAPL_Generic.F90:1150-1183`) — a
  second, independently-maintained geometry-setting call chain, easy to
  miss if a future retirement pass only searches for
  `registry%set_geometry(` calls (this one never calls that method
  name).
- Dead/disabled `propagate_exports`/`add_to_states` lines in
  `initialize_modify_advertised.F90:31,36` — already commented out;
  delete as part of general cleanup, no behavior change.
- The coupler-lifecycle call sites in
  `OuterMetaComponent/run_user.F90:48,72` and
  `initialize_user.F90:29` (`get_import_couplers`/
  `get_export_couplers`) — confirm `graphbuilder_run_pull_hook`
  (`GraphBuilder_RunPhasePull_smod.F90:44-79`) already covers every
  real re-propagation case these calls exist for, before deleting; do
  not assume coverage without checking, since the two mechanisms differ
  (coupler phases are a fixed three-phase ESMF lifecycle; the pull hook
  is a single demand-driven `update()` call).
- Service's own retirement — confirm every real production
  `SERVICE`/`SERVICE_PROVIDER`/`SERVICE_SUBSCRIBER` declaration has a
  landed callback-based replacement (none found in-tree as of this
  writing — a search for `itemType:.*SERVICE` across all `*.yaml`/`*.yml`
  outside `tests/` returned zero matches), then delete
  `ServiceClassAspect.F90` and the three `SERVICE*` itemType values — an
  audit-and-delete item, not a structural rebuild, unlike Wildcard.
- Pre-removal compatibility check on `MAPL_GridCompGetRegistry`
  (`API.F90:42`) — public, exported, zero in-tree callers outside a test
  fixture (`superstructure/generic/tests/gridcomps/
  ProtoExtDataGC.F90:10,85,146`), but a real external-compatibility risk
  if any out-of-tree GEOS component calls it. Check against a real
  GEOSgcm checkout before deleting the symbol in 6a-3.

**Repo/tooling note (extends §20.4.1's/§20.4.3's/§20.4.4's/§20.4.8's/
§20.4.10's own notes).** Phase 6 code, for whichever sub-change is
filed, lives in the MAPL repo/checkout, same as every prior phase: 6a's
own retirement work and 6b's own cleanup items all build on, and
remove, landed MAPL-repo code — no repo-separation saving (§20.2)
applies here either.

### 20.4.12 Phase 12 scope: wire 5a2's remaining `StateItemCharacteristic` subclasses into real dispatch

**Discovered gap, not previously tracked by any phase above.** 5a2
(`openspec/changes/archive/
2026-10-08-state-item-characteristic-subclasses`) landed seven
additional `StateItemCharacteristic` subclasses
(`AttributesCharacteristic`/`UngriddedDimsCharacteristic`/
`QuantityTypeCharacteristic`/`ConservationCharacteristic`/
`NormalizationCharacteristic`/`StandardNameCharacteristic`/a
vertical-grid-adjacent `ReferenceCharacteristic` analog), by that
change's own explicit, deliberate scope: "standalone types exercised by
synthetic-node pFUnit coverage only," no `GraphBuilder` rewire. This
phase is that rewire.

**Scope:** wire all seven into `GraphBuilder.F90`'s own real
`build_characteristics()`/`CharacteristicMap` insertion point —
mirroring how `UnitsCharacteristic`/`VerticalGridCharacteristic` already
are — so a real production mismatch on any of these seven axes becomes
visible to `resolve_one`'s existing mismatch detection and
`ExtensionResolution.F90`'s existing `build_chain` loop (confirmed
generic over an arbitrary-length `mismatched_kinds(:)` array by 10c's
own prior confirmation — no chain-building code changes needed, only
detection). Real `build_transform` adaptation logic is explicitly NOT
required for every axis — 5a2's own design.md already noted most legacy
`*Aspect%make_transform` implementations either fail explicitly or have
no real adaptation logic today (only Units/Geom/Typekind have one in
legacy either). This phase's job is detection-wiring; a mismatch with no
adaptation available should surface as an explicit, reported failure
(matching legacy's own behavior for e.g.
`ConservationAspect%make_transform`'s unconditional `_FAIL`), not a
silent pass-through.

**Not blocking anything above or already landed on its own merits** —
but named in §20.4.11 as a required Phase 6a prerequisite: without it,
Phase 6a-1's own parity run cannot claim coverage of legacy's own
ten-axis mismatch-detection surface, only the three axes 5a already
wired. Depends on Phase 5a/5a2 (landed) only — independent of 3b2, Phase
7, and Phase 13.

### 20.4.13 Phase 13 scope: restart compatibility under graph-native field materialization

**Discovered gap, not previously tracked by any phase above.**
`RestartHandler.F90`'s own field-selection predicates
(`predicate_skip_restart_`/`predicate_incomplete_`,
`RestartHandler.F90:189-204`) key off `RestartMode` field metadata set
via `FieldInfoSetInternal` — set only by legacy
`FieldClassAspect.F90:498`. No graph-native field-materialization call
site (`try_materialize_field_extension`/`materialize_field_extension`,
`GraphBuilder.F90`) sets this metadata anywhere in the codebase today
(confirmed: a search for `RestartMode` across every file under
`superstructure/generic/graph/` returns zero hits). A
graph-materialized field is silently invisible to, or mishandled by,
restart today.

**Scope:** decide, as a planned up-front design decision (same
discipline this document already asks of other open points — Phase 4's
4b for REQ-MTH-011(c), Phase 5's 5a for `CharacteristicStatus` naming),
how a graph-materialized field's `RestartMode` is populated — likely by
threading `VariableSpec%restart_mode` (already a stored field,
`VariableSpec.F90:99`) through to whichever graph-native materialization
call site creates the real `ESMF_Field`, mirroring
`FieldClassAspect.F90:119-120`'s own construction-time assignment — and
confirm `RestartHandler.F90`'s own predicates behave correctly against a
graph-materialized field under `GRAPH_MODE_GRAPH`, not only under
`GRAPH_MODE_BOTH`/`GRAPH_MODE_REGISTRY` where legacy's own
materialization already sets this metadata regardless. A real
read/write round-trip test against at least one graph-materialized
field is the exit criterion, not merely "the field carries the
metadata."

**Not blocking anything above or already landed on its own merits** —
but named in §20.4.11 as a required Phase 6a prerequisite: without it,
flipping a component's default mode (6a-2) silently breaks restart for
that component with no test today that would catch it. Depends on Phase
10a (landed state-materialization call sites) only — independent of
3b2, Phase 7, and Phase 12.

### 20.4.14 Phase 14 scope: sentinel-first-assignment convention for polymorphic allocatable function results

**Discovered gap, not previously tracked by any phase above.** A
function returning `class(...), allocatable` whose only normal
construction path runs after one or more early-failure returns (the
`_FAIL`/`_RETURN` macros, which expand to a bare `return` on failure —
`include/MAPL_ErrLog.h`) can return with its result unassigned on any
such failure path. The Fortran standard does not reliably guarantee an
unallocated polymorphic function result is left in a universally safe,
caller-assignable state across compilers. This is why
`Characteristic%build_transform`/`StateItemCharacteristic%build_transform`
and `GraphStateItem%get_characteristic` are deliberately subroutines
with an `intent(out), allocatable` dummy argument rather than functions
(`GraphStateItem.F90`'s own comment at the latter's declaration states
this explicitly) — `intent(out)` guarantees the dummy starts
unallocated and stays caller-safe on every return path, with no
workaround needed. Legacy already hit the same hazard the other way:
`UnitsAspect%make_transform` (`UnitsAspect.F90:119-135`) *is* a function
returning `class(ExtensionTransform), allocatable`, and defensively
allocates a `NullTransform()` placeholder immediately before every
`_FAIL` call on its own failure branch, specifically so the function
result is never left unallocated — a tax paid on every failure branch,
in every such function, with no general convention naming the pattern
or its rationale anywhere in this codebase today.

**Scope:** document (and, where a function-returning-polymorphic
-allocatable shape is actually the right choice for other reasons —
e.g. an existing public API this project does not want to break into a
subroutine) apply a named convention: assign the function result to a
simple, trivial placeholder value as the very first executable
statement, before any possible early-return failure path, so every
return — success or failure — leaves the result in an assigned,
caller-safe state. For a `class(Transform)`-shaped result this means
constructing (or reusing) a trivial no-op `Transform` subclass for
exactly this purpose, mirroring legacy's own `NullTransform` precedent
but adopted as a documented, intentional idiom rather than a one-off
workaround discovered independently at each call site. This phase is
documentation/convention work (likely the `fortran-style` skill's own
"Error Handling" section, which already discusses the
subroutine-vs-function tradeoff for fallible procedures and already
flags "some procedures best expressed as functions — discuss with
team") plus, optionally, applying the convention retroactively to
`UnitsAspect%make_transform`'s own existing `NullTransform()` workaround
so it reads as an instance of a named pattern rather than an isolated
fix. No graph-layer code is required to change for this phase to be
complete — `build_transform`'s own subroutine shape (post-Phase 5a/this
roadmap's merge-characteristic-families change) already sidesteps the
hazard by construction; this phase exists so the *next* polymorphic
-allocatable-returning function anywhere in the codebase has a named
convention to follow instead of re-discovering the hazard, or missing
it, independently.

**Not blocking anything above or already landed on its own merits** —
raised during review of `graph-builder-stateitem-resolution`'s own
precursor work (the `Characteristic`/`StateItemCharacteristic`
`build_transform` signature) but applies to the whole codebase, not
only the graph layer; tracked here because this is where comparable
discovered-gap backlog items already accumulate (§20.4.12, §20.4.13).
Depends on nothing landed or planned above — independent of Phase 12,
Phase 13, 3b2, and Phase 7.

### 20.4.15 Phase 15 scope: `Hierarchy`-as-factory redesign of component construction

**Discovered gap, deferred during `hierarchy-wide-component-graph`'s
own design, not previously tracked by any phase above.** That change
introduces `HierarchyId`/`HierarchyResources` (a private,
`HierarchyId`-keyed registry owning the hierarchy-wide `NodeId`/
`DependencyNetworkId` storage every `ComponentGraph` instance now
shares — see that change's design.md D1) specifically so threading a
hierarchy-wide reference down through construction never requires a
pointer, a module-level bare singleton, or a public-signature change to
`mapl_GridCompCreate`. A materially larger alternative was raised
during that change's own design discussion and explicitly deferred,
not folded in: a user-visible `Hierarchy` type, constructed once per
application, whose own factory method (rather than the free function
`mapl_GridCompCreate` is today) creates every `OuterComponent` in that
hierarchy — root and descendant alike — and which eventually also owns
other hierarchy-wide shared resources beyond the graph (e.g. a geometry
registry, so two components needing "the same" geometry do not each
independently construct an equal-but-distinct one).

**Why deferred rather than folded into `hierarchy-wide-component-graph`
directly:** `mapl_GridCompCreate` is a long-established, widely-used
public entry point (re-exported from `API.F90`), not an internal
implementation detail — redesigning its own calling convention into a
factory-method-on-an-object shape is a cross-cutting change to every
existing call site across the GEOS/MAPL codebase, not merely the
`superstructure/generic` files `hierarchy-wide-component-graph`'s own
proposal.md names as its Impact. A `Hierarchy` object owning a pointer
and threaded down through `OuterMetaComponent`'s constructor was also
considered and rejected on narrower grounds: a child component that
declares its own `user_setservices` is constructed through that same
public entry point used to create a brand-new hierarchy's root
(`GenericGridComp.F90`'s `create_grid_comp_primary`), and introducing a
pointer-shaped dependency from `ComponentGraph`/`OuterMetaComponent`
toward a new `Hierarchy` type risked a circular module dependency if
`Hierarchy` itself ever needed to reference either of those types back
(e.g. to expose a `get_component_graph`-style accessor of its own).

**Scope:** redesign `mapl_GridCompCreate`'s calling convention around a
`Hierarchy` type (construction, lifetime, and how an existing
`HierarchyId` 's registry entry relates to it — likely `Hierarchy`
becomes a thin, user-facing wrapper around one `HierarchyResources`
entry, reached by its own `HierarchyId`, rather than a competing
storage mechanism); audit and update every existing
`mapl_GridCompCreate`/`GridCompCreate` call site across the codebase
for the new calling convention; extend `HierarchyResources` to carry
whatever additional shared resources (geometry registry named above,
and any others identified by then) motivated the redesign in the first
place.

**Not blocking anything above** — `hierarchy-wide-component-graph`'s own
`HierarchyId`/`HierarchyResources` mechanism is designed to compose with
this later redesign (a `Hierarchy` type can be layered on top of an
existing registry entry) without requiring that change's own storage
mechanism to be reworked first, so this phase can land independently,
whenever a second hierarchy-wide shared resource (motivating the
"doesn't repeat the threading problem" rationale) actually materializes.

### 20.4.16 Phase 16: `hierarchy-wide-component-graph` — architecture revision, node/network storage made hierarchy-wide

**Architecture-revision phase, not a feature phase** — recorded here
per this document's own "discovered gap, not previously tracked"
precedent (Phases 7/10/14/15 above), landed as
`openspec/changes/hierarchy-wide-component-graph` (Task Groups 1-10,
all `[x]` except 6.7, an explicitly out-of-scope follow-up). Unlike
Phases 7/10/14/15, this phase was not discovered mid-flight during
unrelated work — it was proposed directly, in response to four
independent cross-component features (3a/3b ordinary connections, 4e
geometry push/pull, 4c/4d callback wiring's flattened export
namespace, and 3b2's own abandoned, never-settled unresolved-import
propagation attempt) each having invented an incompatible, one-off
mechanism to reference a node belonging to a different
`ComponentGraph` instance, because `NodeId` was only valid within the
instance that issued it (`proposal.md` "Why").

**What this phase changed** (see `02-component-hierarchy.md` §2.2/§2.3,
`05-identities.md` §5.1-§5.3, `07-component-graph.md` §7.1/§7.3,
`08-graph-builder.md` §8.2 — all `[RE-EVALUATED]` by this phase):

- `NodeId → GraphNode` and `DependencyNetworkId → DependencyNetwork`
  storage, plus the `NodeIdGenerator`/`DependencyNetworkIdGenerator`
  pair, moved from per-`ComponentGraph` to one shared instance per
  component hierarchy (`HierarchyId`/`HierarchyGraph`/
  `HierarchyResources`, a private `HierarchyId`-keyed registry — not a
  bare module-level singleton, not threaded pointers; design.md D1).
  Every other `ComponentGraph` responsibility (resource index, ports,
  child-port bindings, lifecycle state) remained exactly as
  component-local as before.
- Every proxy-node mechanism built to work around per-component graph
  storage was retired, now structurally unnecessary: `proxy_key`/
  `get_or_make_local_node_id`'s cross-graph branch (ordinary
  connections), `parent_geometry_proxy_key`/`push_geometry_to_child`
  (geometry push/pull), and `resolve_qualified_export_node_id`'s
  proxy-creation half (callback wiring). A cross-component dependency
  edge now connects two real `NodeId`s directly.
- 3b2's abandoned cross-component unresolved-import propagation was
  replaced by a corrected design (design.md D3, after an initial
  "search the subtree" draft was rejected before any code was written
  against it): a recursive, one-level-at-a-time import-mirroring hook,
  reusing the existing unmodified single-level resolution machinery
  with no new search or materialized-copy mechanism — plus a required,
  initially-unscoped companion (export-side propagation, discovered as
  a real regression during this phase's own verification, not optional
  polish; see tasks.md task 6.8's own three-bug postmortem).
- Freeze became a single hierarchy-wide operation (one new,
  non-recursive-dispatch init phase, `GENERIC_INIT_GRAPH_FREEZE`,
  invoked once at the true hierarchy root), replacing the former
  per-component freeze call riding a phase
  (`GENERIC_INIT_ACCEPT_TRANSFER`) that was not actually one-shot — a
  real, found-not-assumed hazard (duplicate graph mutation on that
  phase's second pass), fixed by a new explicit
  `connections_already_resolved()` idempotency flag rather than
  continuing to rely on `is_frozen()` for that purpose.
- Component-name uniqueness, previously an unenforced convention
  History relied on informally, became a checked, hierarchy-wide
  invariant.
- History's own deep-hierarchy export-visibility need was given a
  direct, non-propagating query (`query_component_exports`) rather
  than a sixth ad hoc crossing mechanism.

**Verification (this session, Task Group 10):** full regression pass —
`ctest -L ESSENTIAL` (64 tests) 100% passing under both NAG and
gfortran; full top-level `ctest` (75 tests) 71/75 passing under both
compilers, the only 4 failures being the pre-existing, unrelated
`ll-ll`/`cs-cs`/`cs-ll`/`ll-cs` Regrid_Util regression-data tests
(missing `LOCAL_REGRESSION_DATA_DIR`, nothing to do with graph work —
confirmed identical failure set on both compilers). No regression
found in `superstructure/generic/tests/scenarios/` or `graph-scenarios/`
under any of the three `graph_mode()` values (`registry`/`graph`/
`both`) exercised by the existing test suite.

**Did not land as predicted:** `proposal.md`'s own stated expectation
("most of the reduction comes from deleted proxy mechanisms rather
than new code") did not hold once the full phase landed. `GraphBuilder.F90`
plus its submodules measured 4585 total lines at this phase's own
baseline (`proposal.md`'s own figure, confirmed against commit
`224f5a1d`) and 5442 lines at this phase's completion (commit
`15a50ec5`) — a net increase of 857 lines (+19%), not a decrease.
Proxy removal alone (Task Groups 3-5, commit `f629872a`) did shrink the
total by 81 lines as predicted; Task Group 6 (import/export
propagation, commit `9fdea0fd`, +707 lines) and Task Group 9
(hierarchy-wide freeze, commit `15a50ec5`, +160 lines) each added
substantial new, necessarily-scoped functionality — recursive
mirroring in both directions plus a required idempotency-flag fix, and
a new one-shot init phase plus a hierarchy-wide freeze/convergence
walk, respectively — that outweighed the removal (Task Group 7's
export-visibility query, commit `bc51fc9c`, added a further +71 lines;
Task Group 8 added no lines, test-only). The prediction was reasonable
at proposal time (proxy mechanisms were real, measurable bulk) but did
not anticipate 3b2's companion export-propagation requirement (task
6.8) being discovered as load-bearing rather than optional, nor the
freeze-relocation hazard (task 9's own blocker) requiring a new
explicit idempotency mechanism rather than a one-line call-site move.

### 20.4.17 Phase 17 scope: `GraphBuilder.F90`/`Test_GraphBuilder.pf` size — characterize before splitting

**Discovered gap, not previously tracked by any phase above.** Raised
directly by the codebase owner (not surfaced during an openspec
change's own review, unlike Phases 7-16): agents spend too much time
processing `GraphBuilder.F90` and its submodules, and the test file is
"also enormous." Before scoping a split, this phase's own first job is
to characterize *why* the files are the size they are — this document's
own established "measure, don't assume" discipline (§2's own citation
culture) applies as much to a refactor's justification as to a
feature's.

**Measured, this session (not the secondhand figures a prior informal
pass reported — see the discrepancies named below):**

| File | Lines |
|---|---|
| `GraphBuilder.F90` | 1732 |
| `GraphBuilder_ConnectionResolution_smod.F90` | 1387 |
| `GraphBuilder_CallbackWiring_smod.F90` | 670 |
| `GraphBuilder_StatePopulation_smod.F90` | 352 |
| `GraphBuilder_Advertising_smod.F90` | 286 |
| `GraphBuilder_Geometry_smod.F90` | 284 |
| `GraphBuilder_RunPhaseNode_smod.F90` | 149 |
| `GraphBuilder_Freeze_smod.F90` | 168 |
| `GraphBuilder_LifecycleHooks_smod.F90` | 164 |
| `GraphBuilder_RunPhasePull_smod.F90` | 93 |
| `GraphBuilder_PortsAndProxies_smod.F90` | 89 |
| `GraphBuilder_Visualization_smod.F90` | 68 |
| **Total (11 submodules + parent)** | **5442** |

Confirms the prior session's own `5442`-line total (commit `15a50ec5`)
and the `4585`-line pre-`hierarchy-wide-component-graph` baseline both
hold up (§20.4.16's own "Did not land as predicted" accounting) — those
two aggregate figures were correct. What was NOT independently
verified before (and is corrected here): `Test_GraphBuilder.pf` is
**3607** lines (confirmed, matches), but the combined-suite figure
needs its own file list spelled out, not just a sum — see below.

**Why `GraphBuilder.F90` (the parent module, 1732 lines) cannot simply
be split further into submodules, even after this phase runs:** Fortran
`submodule` bodies may call a plain (non-`module`-interface) procedure
declared in their ancestor module, but a plain procedure can only live
in ONE place — the parent module or exactly one submodule — not be
shared verbatim across multiple submodules without either duplicating
it or promoting it to a `module interface` (adding, not removing,
boilerplate). `GraphBuilder.F90` carries 16 such plain shared helpers
(`item_key`, `component_for`, `resolve_qualified_component`,
`try_materialize_field_extension`, `materialize_extension_chain`,
`resolve_local_node_id`, `resolve_callback_import`,
`resolve_callback_destination`, and 8 more) — `item_key` alone is
called from 7 of the 11 submodules (confirmed by direct grep), so it
cannot move into any one of them without either a second copy or a new
module-interface entry. These 16 procedures plus the full
type-bound-procedure interface table account for 865 of the file's 1732
lines (`contains` at line 865) — the remaining 867 lines are the plain
procedure bodies themselves. This is a genuine, structural reason a
sixth split cannot shrink the parent module much further by moving
code sideways alone — any further reduction has to come from either
(a) deleting something (the proxy-removal precedent, which already
happened once and produced only an 81-line net reduction against this
phase's own +707/+160-line additions, §20.4.16) or (b) a real design
change to how cross-submodule helpers are shared, not a mechanical
file-boundary move.

**`Test_GraphBuilder.pf` (3607 lines): measured contributor breakdown.**
Direct `git show <commit>:...` line counts across the Group 6-9 span
(`9ce2ecd5` → `15a50ec5`, the span the backlog item's own secondhand
"Groups 6-9" framing names) show this file grew only **327 lines**
(3280 → 3607) in that span — Group-6-9 is NOT the dominant growth
driver the secondhand report implied; most of the file's bulk predates
that work. The file carries 52 `@test` annotations across 46 distinct
test subroutines (a few parameterized cases share one subroutine body)
spanning at least 10 orthogonal sub-topics with no shared setup beyond
two fixture
helpers (advertising, ordinary-connection resolution, inout, freeze,
materialize-extensions gating, composite/vertical-grid composite
wiring, qualified-export namespace/`query_component_exports`, callback
resolution/binding, run-phase-node construction, qualified-component
path resolution) — this is "genuinely many bundled concerns," not one
verbose concern repeated.

**Confirmed, measurable duplication exists, separately from file
size — wider than first measured in this session, corrected here.**
`make_outer_meta` (31 lines) is duplicated **byte-identical** (confirmed
via checksum) across **7** files, not 4 as first reported: the 4
`Test_GraphBuilder*.pf`/`Test_GraphBuilder_ConvergenceCheck.pf` files
named below, PLUS `Test_ComponentHierarchyGraph.pf`,
`Test_GraphGeometryHook.pf`, and
`Test_OuterMetaComponentDriverResolver.pf` — all 7 missed on first pass
by searching only the `Test_GraphBuilder*`-prefixed files; a plain
`grep -rl "subroutine make_outer_meta"` across the test directory
finds the other 3. `make_var` (8-10 lines) is duplicated across only
**4** of those 7 (`Test_GraphBuilder.pf`,
`Test_GraphBuilderPropagation.pf`, `Test_GraphBuilderMaterializeGate.pf`,
`Test_GraphBuilder_ConvergenceCheck.pf` — the other 3 `make_outer_meta`-
only files have no `make_var` of their own) — only
`Test_GraphBuilderEquivalence.pf` among the generic-components suite
has neither. **`make_var` is not byte-identical across its 4 copies**:
`Test_GraphBuilder.pf` and `Test_GraphBuilderMaterializeGate.pf` declare
a 3-argument form with an `optional, intent(in) :: units` parameter;
`Test_GraphBuilder_ConvergenceCheck.pf` and
`Test_GraphBuilderPropagation.pf` declare a 2-argument form with no
`units` parameter at all — a real signature difference, not drift. The
3-argument form is a strict superset (every real call site across all
4 files passes only `state_intent`/`short_name` positionally; none
passes `units`), so **the extracted shared helper must keep `units`
optional** — forcing the two 2-argument call sites to start passing a
dummy `units` value would be a behavior-irrelevant but unnecessary
call-site change this dedup does not need to make. Roughly 150-180
lines of pure, mechanical duplication from `make_outer_meta` alone (7
copies × 31 lines, after accounting for the one canonical copy) plus a
smaller amount from `make_var`'s 4 copies, not counting any other
shared fixture logic not yet audited.
Combined with `Test_GraphBuilderPropagation.pf` (979),
`Test_GraphBuilderMaterializeGate.pf` (763), and
`Test_GraphBuilder_ConvergenceCheck.pf` (307), the five-file combined
total is **5663** lines (confirmed by direct `wc -l`, not the backlog
item's own secondhand "~5700" figure, which also omitted
`Test_GraphBuilder_ConvergenceCheck.pf` from its own two-file
addition — the combined total needs all five files named, not two).

**Scope for this phase — characterize, then decide, in this order:**

1. **Dedup the fixture helpers first** (`make_outer_meta` across all
   7 confirmed call sites; `make_var` across its 4, preserving the
   optional `units` parameter) — a mechanical, low-risk
   extraction into a shared test-support module (mirroring
   `superstructure/generic/graph/tests/IdGeneratorTestSupport.F90`'s
   own precedent for exactly this shape: test-only helper code shared
   across multiple `.pf` files in the same directory, not production
   code). This alone removes real, measured duplication regardless of
   what the characterization below concludes about the rest of the
   file.
2. **Characterize `GraphBuilder.F90`'s own 867 lines of plain shared
   helpers**: for each of the 16, confirm (by the same cross-submodule
   call-count method used above for `item_key`) whether it is
   genuinely shared (stays in the parent module, correctly) or
   actually only used by one submodule today (a candidate to move,
   or to promote to a `module interface` if a second caller is
   expected soon). Do not assume all 16 are correctly placed just
   because `item_key` is.
3. **Characterize `Test_GraphBuilder.pf`'s own ~20 finer-grained
   sub-topics** (a finer split of the 10 coarse groups named above —
   e.g. "vertical-grid composite wiring" alone covers 6 distinct test
   subroutines on its own): for
   each, confirm whether it is testing `GraphBuilder.F90`'s own shared
   logic (stays, by the same "shared code colocated with its test"
   reasoning) or a single submodule's logic exclusively (a genuine
   split-into-`Test_GraphBuilder_<Submodule>.pf` candidate, mirroring
   this directory's own existing precedent —
   `Test_GraphBuilder_ConvergenceCheck.pf` is already exactly this
   shape, split out from the monolith for the freeze/convergence
   check specifically). **Only after this characterization** decide
   split-further vs. leave-as-is per sub-topic — do not split
   mechanically by line count alone.
4. **Decide whether `GraphBuilder.F90`'s own 16 shared helpers that
   turn out to be genuinely cross-cutting (most, per the `item_key`
   evidence) justify a *second* parent-level file** (e.g. a
   `GraphBuilder_SharedHelpers.F90` plain module `GraphBuilder.F90`
   itself and every submodule can `use`, rather than every submodule
   reaching through host association into one large parent) — this is
   the one structural option that could meaningfully shrink
   `GraphBuilder.F90` itself without duplicating code or reopening the
   module-interface boilerplate tradeoff named above. Not pre-decided
   here; this phase's own design.md should evaluate it against the
   module-interface alternative before committing to either.

**Discrepancies found versus the prior informal pass's secondhand
report — corrected here, not silently:**
- `GraphBuilder.F90` + 11 submodules total (5442) and the pre-change
  baseline (4585) both confirmed accurate.
- `Test_GraphBuilder.pf` (3607) confirmed accurate.
- The claimed "~5700 lines of test code for one capability" combining
  only `Test_GraphBuilder.pf` + `Test_GraphBuilderPropagation.pf` (979)
  + `Test_GraphBuilderMaterializeGate.pf` (763) omitted
  `Test_GraphBuilder_ConvergenceCheck.pf` (307) from the same directory
  testing the same capability — the real combined total across all
  five related files is 5663, not ~5700 from three files understating
  a fifth.
- The claim that Group 6-9 work is what made these files large does
  not hold for `Test_GraphBuilder.pf` specifically (+327 of 3607 lines,
  ~9%, attributable to that span) — most of this file's size predates
  Group 6-9 and reflects the ~20 bundled sub-topics named above, not
  recent growth.

**Not blocking anything above or already landed** — this is
independent code-organization/test-infrastructure work, not new graph
capability. **Dependency note (explicit, per this phase's own
requirement to check rather than assume):** confirmed independent of
Phase 6a's own five-item prerequisite list (§20.4.11), Phases 7a/7b/7c,
12, and 13 — none of those touch `GraphBuilder.F90`'s own plain-helper
section or `Test_GraphBuilder.pf`'s own fixture duplication; this can
proceed in parallel. **Does interact with Phase 18 (§20.4.18) below**:
whatever file layout this phase settles on for `GraphBuilder.F90`
itself should land, or at minimum be designed, before Phase 18's own
directory move touches the adjacent `superstructure/generic/graph/`
tree, to avoid two independent reorganizations colliding on the same
review pass — see §20.4.18's own sequencing note.

**Sub-sequencing, in recommended order:**

- **17-1. DONE.** Dedup `make_outer_meta` (7 call sites) and `make_var` (4
  call sites, preserving the optional `units` parameter) across the
  seven files that duplicate `make_outer_meta`. Sized for one
  session: mechanical extraction, no production code touched. Exit
  criterion: a single shared test-support source (new file under
  `superstructure/generic/tests/`, naming TBD at implementation time)
  defines `make_outer_meta` once and `make_var` once (3-argument form,
  `units` optional); all seven files that had their own
  `make_outer_meta` (`Test_GraphBuilder.pf`,
  `Test_GraphBuilderPropagation.pf`, `Test_GraphBuilderMaterializeGate.pf`,
  `Test_GraphBuilder_ConvergenceCheck.pf`, `Test_ComponentHierarchyGraph.pf`,
  `Test_GraphGeometryHook.pf`, `Test_OuterMetaComponentDriverResolver.pf`)
  `use` the shared version instead of their own copy, and the four
  that also had their own `make_var` do the same for it; `ctest -L
  ESSENTIAL` passes with 0 new failures and identical pass/fail
  results to before. All seven files compile into the single
  `MAPL.generic.components` ctest target (confirmed via
  `superstructure/generic/tests/CMakeLists.txt` — there is no separate
  per-file suite), so verification is one target run, not seven:
  `ctest -L ESSENTIAL -R MAPL.generic.components`.

  **Implemented this session.** All 7 files' own `make_outer_meta`
  bodies re-verified byte-identical (md5, 31 lines each) and both
  `make_var` signatures re-verified exactly as described (3-argument
  `units`-optional form in `Test_GraphBuilder.pf`/
  `Test_GraphBuilderMaterializeGate.pf`; 2-argument form in
  `Test_GraphBuilderPropagation.pf`/`Test_GraphBuilder_ConvergenceCheck.pf`)
  before any edit — no drift from this section's own planning-time
  measurement. New shared module:
  `superstructure/generic/tests/GraphBuilderTestSupport.F90`
  (`GraphBuilderTestSupport_mod`, mirroring
  `IdGeneratorTestSupport.F90`'s own shape: plain Fortran module, not
  a `.pf` file), built into the existing `MAPL.generic.test_common`
  shared library (`superstructure/generic/tests/CMakeLists.txt`) that
  already backs all 6 of this directory's pFUnit targets, including
  `MAPL.generic.components` — no new CMake target needed, one line
  added to an existing `add_library` source list. All 7 `.pf` files'
  own local copies removed entirely (not commented out) and replaced
  with `use GraphBuilderTestSupport_mod, only: make_outer_meta[, make_var]`;
  the 2-argument call sites needed no changes, confirming this
  section's own "strict superset" claim. Net: 280 lines removed across
  the 7 files, 85 lines added (1 new module + `use` lines + removal
  comments); diff stat per file:
  `Test_ComponentHierarchyGraph.pf` +4/-33,
  `Test_GraphBuilder.pf` +4/-44,
  `Test_GraphBuilderMaterializeGate.pf` +4/-45,
  `Test_GraphBuilderPropagation.pf` +1/-43,
  `Test_GraphBuilder_ConvergenceCheck.pf` +9/-48,
  `Test_GraphGeometryHook.pf` +4/-32,
  `Test_OuterMetaComponentDriverResolver.pf` +4/-34. Verified both
  compilers: NAG (`nag-stack/7.2.41`) and gfortran
  (`gfortran-stack`), `cmake --build <dir> --target build-tests`
  clean on both (zero new warnings/errors), `ctest --test-dir <dir> -L
  ESSENTIAL -R MAPL.generic.components` 1/1 pass on both, matching a
  pre-change baseline run captured via `git stash` before touching any
  file (also 1/1 pass, identical). Full `ctest -L ESSENTIAL` (64
  tests) also run on both compilers post-change as a wider regression
  check beyond this step's own exit criterion: 64/64 pass on both,
  matching §4's own documented baseline exactly.
- **17-2. DONE.** 19 of 16-estimated helpers found (3 missed by the
  prior informal count): 5 genuinely cross-cutting, 7 single-caller
  clean move candidates, 4 single-caller-but-plausible-second-caller
  promote candidates, and 3 with ZERO real callers anywhere — dead
  code, a category the roadmap's own framing did not anticipate. See
  table below.
- **17-3. DONE.** 51 `@test`/51 distinct subroutines confirmed (not
  "52/46" — both prior figures were off, in opposite directions: this
  session's own repo state is one commit (Phase 21) past the `3607`-
  line/`46`-subroutine snapshot §20.4.17's own measurement section
  above cites, which itself predates 17-1's dedup). Of 23 finer-
  grained sub-topic buckets, 19 are clean split-into-their-own-file
  candidates (one submodule exercised exclusively), 2 stay (shared-
  logic colocation), 2 are ambiguous (one of the 2 is structural, not
  a logic question). See table below.

  **Implemented this session** (characterization only — no code
  touched in `GraphBuilder.F90`, any `_smod.F90`, or any `.pf` file;
  verified via `git status` showing zero modified source/test files
  before and after this session).

  **17-2 method (repeatable):** for each plain procedure appearing
  between `contains` (line 867) and `end module` (line 1734) in
  `superstructure/generic/GraphBuilder.F90`, ran
  `grep -nE '\bNAME\s*\(' GraphBuilder.F90 GraphBuilder_*_smod.F90`
  (11 submodule files, listed in ARCHITECTURE.md §1.2) and manually
  excluded matches inside `!`-comments (a small number of helpers are
  referenced-in-prose by other helpers' own header comments without
  being called — `component_spec_for`/`enumerate_qualified_exports`
  specifically, see below) and the declaration/`public ::` lines
  themselves. Cross-checked against the whole-repo `.pf`/`.F90` tree
  (`grep -rn NAME --include="*.F90" --include="*.pf" .`, excluding
  `build/`/`gfortran/`/`nag/` generated-copy directories) to catch any
  caller outside the 11 submodules (none found calling any of the 19
  from outside `GraphBuilder.F90`'s own family, aside from tests,
  which do not count as "submodule callers" for this audit's purpose
  — a test calling a helper directly is evidence for 17-3, not 17-2).

  **First correction — the count is 19, not 16.** The roadmap's own
  "16... and 8 more" estimate (the passage directly above this one,
  "Why `GraphBuilder.F90`... cannot simply be split further") named 8
  explicitly and asserted 8 more without enumerating them. Reading the
  file directly (`contains` at 867 through `end module` at 1734) finds
  19 plain procedures total: the 8 named
  (`item_key`, `component_for`, `resolve_qualified_component`,
  `try_materialize_field_extension`, `materialize_extension_chain`,
  `resolve_local_node_id`, `resolve_callback_import`,
  `resolve_callback_destination`) plus 11, not 8, more: `is_self`,
  `component_spec_for`, `enumerate_qualified_exports`,
  `collect_qualified_export_index`, `append_own_export_index_entries`,
  `query_component_exports`, `get_item_units`, `get_item_typekind`,
  `merged_item_for_link`, `itemtype_name`,
  `is_ordinary_resolvable_import`. The "16" figure undercounted by 3 —
  a plausible explanation, not confirmed: `get_item_units`/
  `get_item_typekind`/`merged_item_for_link` are all small (23/21/47
  lines), un-exported-sounding, and physically adjacent to
  `try_materialize_field_extension`/`materialize_extension_chain`
  (one of which IS in the named-8 list) — an informal pass skimming
  for "helper-shaped" procedures near the named ones could plausibly
  have folded these three into the ones they directly support instead
  of counting them separately. This is the session's own best guess
  at the discrepancy's origin, not a verified fact — flagged as such.

  | Helper | Body lines | Parent self-calls | Calling submodule(s) (count) | Classification |
  |---|---|---|---|---|
  | `item_key` | 16 | 2 | Advertising(1), CallbackWiring(2), Geometry(5), RunPhaseNode(2), RunPhasePull(1), StatePopulation(2), Visualization(1) — **7 submodules** | (a) cross-cutting |
  | `is_self` | 6 | 3 | CallbackWiring(2) — **1 submodule** | (b) single-caller — candidate to move into CallbackWiring |
  | `component_spec_for` | 16 | 0 | **none anywhere** (only referenced in two comments, `GraphBuilder_ConnectionResolution_smod.F90:111,204`, explaining why a *different* function, `resolve_qualified_component`, replaced it) | **dead code** — zero real callers in production or test code; candidate for deletion, not move/promote |
  | `component_for` | 19 | 3 | ConnectionResolution(1) — **1 submodule** | (c) single-caller, but see note below — the `public ::` comment at line 199 documents it as needed for "gfortran cross-submodule linkage" precedent already paid; a genuine future-caller signal exists in CallbackWiring's own header reasoning (`resolve_callback_import` calls it, and CallbackWiring is architecturally the module most likely to gain a second qualified-lookup need) — promote-candidate, not move-candidate, though weakly evidenced |
  | `resolve_qualified_component` | 41 | 2 | CallbackWiring(1), ConnectionResolution(7) — **2 submodules** | (a) cross-cutting |
  | `enumerate_qualified_exports` | 12 | 0 | **none anywhere** — zero real callers; only `QualifiedExportIndexEntry` (its result type) appears in `Test_GraphBuilder.pf`, declared but the function itself is never invoked by any test or submodule. `query_component_exports` (below) uses the same result type but calls `append_own_export_index_entries` directly, not `enumerate_qualified_exports` | **dead code** — zero real callers; candidate for deletion. Note: `openspec/changes/archive/2026-10-12-graph-builder-resolution-index/tasks.md` task 1.2 added it specifically to generalize `collect_qualified_exports`, but no consumer was ever wired to it — task 2.2 of that same change explicitly deferred migrating `collect_qualified_exports`'s own callers onto it as "optional polish... do it only if low-risk" and that migration apparently never happened |
  | `collect_qualified_export_index` | 27 | 2 (self-recursive + called once from `enumerate_qualified_exports`) | **none** — only reachable through `enumerate_qualified_exports`, itself dead | **dead code** (transitively — see `enumerate_qualified_exports` above) |
  | `append_own_export_index_entries` | 50 | 2 (`collect_qualified_export_index`, `query_component_exports`) | **none** — both its own callers are parent-module-internal | (a) cross-cutting *within the parent's own dead/live split* — stays regardless, since `query_component_exports` (live, see below) depends on it directly |
  | `query_component_exports` | 16 | 0 | **none from submodules** — called only from `Test_GraphBuilder.pf` (5 call sites, all in the two `test_query_component_exports_*` tests) and documented in tasks.md 7.1 as the History-support query primitive (a real, named future consumer: History itself, not yet wired as of this session) | (c) single-caller-today (tests only) but a strong, named, non-speculative future caller (History) exists per design.md D4/tasks.md 7.1 — promote-candidate is moot since it is already `public` for the gfortran-linkage reason; the real open question is "unused in production yet" rather than placement |
  | `try_materialize_field_extension` | 78 | 0 | ConnectionResolution(2) — **1 submodule** | (b) single-caller — candidate to move into ConnectionResolution, BUT see note: calls `get_item_units`/`get_item_typekind`/`component_for`/`resolve_export_geom`/`resolve_export_vgrid`, several of which are themselves parent-only-called (circular shared-helper dependency — moving this one alone would also drag `get_item_units`/`get_item_typekind` along or leave them as a 2-line residual parent section) |
  | `get_item_units` | 23 | 4 (`try_materialize_field_extension` x1, `materialize_extension_chain` x1, `merged_item_for_link` x2) | **none** — called only by the other 18 shared helpers themselves, never directly by a submodule | (a) cross-cutting-among-the-helpers-themselves — a second-tier shared helper, not a submodule-facing one; stays with whichever of `try_materialize_field_extension`/`materialize_extension_chain`/`merged_item_for_link` ends up where, or stays parent-level if those three stay split across different destinations |
  | `get_item_typekind` | 21 | 2 (`try_materialize_field_extension` x1, `merged_item_for_link` x1) | **none** — same second-tier pattern as `get_item_units` | (a) cross-cutting-among-the-helpers-themselves, same caveat |
  | `materialize_extension_chain` | 69 | 0 | ConnectionResolution(1) — **1 submodule** | (b) single-caller — candidate to move, same circular-dependency caveat as `try_materialize_field_extension` (shares `get_item_units`/`get_item_typekind`/`merged_item_for_link`/`component_for`/`resolve_export_geom`/`resolve_export_vgrid`) |
  | `merged_item_for_link` | 47 | 1 (`materialize_extension_chain`) | **none** — called only by `materialize_extension_chain` | (b) single-caller (transitively, through `materialize_extension_chain`) — same move-candidate caveat; would move together with `materialize_extension_chain` if that one moves |
  | `itemtype_name` | 25 | 4 | CallbackWiring(1) — **1 submodule** | (c) single-caller-in-submodules but 4 parent-internal self-calls (`try_materialize_field_extension` x2, `materialize_extension_chain` x2) make this a true second-tier shared helper like `get_item_units` — cannot move without also considering those two call sites; if `try_materialize_field_extension`/`materialize_extension_chain` both move to ConnectionResolution, `itemtype_name` would need either duplication (cheap — 25 lines, pure/stateless) or a module interface (since CallbackWiring would still need it) |
  | `resolve_local_node_id` | 27 | 1 | ConnectionResolution(1) — **1 submodule** | (b) single-caller — candidate to move into ConnectionResolution; no second-tier dependency issue (self-contained) |
  | `is_ordinary_resolvable_import` | 14 | 0 | ConnectionResolution(1) — **1 submodule** | (b) single-caller — candidate to move; already needed an explicit `public ::` fix for gfortran linkage (tasks.md 6.8 postmortem, §20.4.17's own module header note) specifically BECAUSE it is submodule-only-called, which is direct, documented evidence this one was placed in the parent for no architectural reason — a clean move candidate |
  | `resolve_callback_import` | 50 | 1 | ConnectionResolution(2) — **1 submodule** | (b) single-caller-among-submodules, but see `component_for`'s own note above: CallbackWiring's own test-direct-call precedent (`test_resolve_callback_import_matches_and_rejects`/`test_resolve_callback_import_no_match_is_empty` call it directly, not through `gb%resolve_connections`) suggests this was designed be called from multiple sites including possibly CallbackWiring itself someday — weak promote signal, not strong enough to override the "move" default without a named concrete second caller |
  | `resolve_callback_destination` | 39 | 0 | ConnectionResolution(1) — **1 submodule** | (b) single-caller — candidate to move; self-contained (calls `resolve_local_node_id`, `resolve_callback_import`, `materialize_callback_collection`, all either moving together or already in the submodule) |

  **17-2 classification summary:** 19 total. (a) genuinely
  cross-cutting: `item_key`, `resolve_qualified_component`,
  `append_own_export_index_entries`, `get_item_units`,
  `get_item_typekind` — **5**, plus `itemtype_name` is a borderline
  (a)/(c) case counted once below, **6** if `itemtype_name` is folded
  in. (b) single-caller, clean move candidate: `is_self`,
  `try_materialize_field_extension`, `materialize_extension_chain`,
  `merged_item_for_link`, `resolve_local_node_id`,
  `is_ordinary_resolvable_import`, `resolve_callback_destination` —
  **7**. (c) single-caller-but-plausible-second-caller (promote
  candidate): `component_for`, `query_component_exports`,
  `itemtype_name`, `resolve_callback_import` — **4**. **Dead code**
  (zero real callers anywhere, a category the roadmap's own item 2
  framing did not anticipate): `component_spec_for`,
  `enumerate_qualified_exports`, `collect_qualified_export_index` —
  **3** (the third is transitively dead through the second). 5+7+4+3 =
  19. Headline: unlike `item_key`'s own single, clean cross-cutting
  example, the 16-shared-helpers section is NOT "mostly correctly
  placed" — only 5-6 of 19 (26-32%) are unambiguously cross-cutting;
  7 (37%) are clean move candidates; and a previously-unmeasured 3
  (16%) are dead code the roadmap's own characterization request did
  not anticipate finding. The "second-tier shared helper" pattern
  (`get_item_units`/`get_item_typekind` called only by other parent
  helpers, never directly by a submodule) is also a structural wrinkle
  17-4's own design.md needs to address explicitly — these two are
  cross-cutting in the narrow sense of "called from more than one
  place" but that "more than one place" is entirely other parent-level
  helpers, not submodules, which the roadmap's own item 4 framing
  (`GraphBuilder_SharedHelpers.F90` for helpers "every submodule can
  use") did not contemplate.

  **17-3 method (repeatable):** `grep -n '^\s*subroutine test_'
  Test_GraphBuilder.pf` (51 matches) cross-checked against `grep -c
  '@test(' Test_GraphBuilder.pf` (51 matches, confirming no
  parameterized case shares a body in THIS file post-Phase-21 — the
  roadmap's own "52 `@test`/46 subroutines" figure cited above was
  accurate for the `15a50ec5` snapshot it was measured against, one
  commit before 17-1's own dedup commit `06c583f5`; Phase 21
  (`8577a802`, landed between 17-1 and this session, per `git log
  --oneline -5`) added 5 more test subroutines with no parameterized
  sharing, which is why this session's own count differs from both
  prior figures in both directions — not a contradiction, two
  different points in time). For each test, read the actual body (not
  just the name/comment) to find which `gb%<method>` type-bound call
  it drives, or — for 9 tests with no `gb%` call at all — which
  specific shared-helper or submodule-internal procedure it calls
  directly by name (confirmed via a per-test `grep 'gb%\w*\(' <test
  body line range>` plus a manual read of each zero-`gb%`-match test).
  Submodule attribution for each `gb%<method>`/direct-call target was
  cross-checked against the interface-block comments in
  `GraphBuilder.F90` (e.g. "! Ordinary connection resolution procedures
  (GraphBuilder_ConnectionResolution_smod.F90)" at line 460) and
  ARCHITECTURE.md §1.2's own module map — both agree.

  **Second correction — fixture/support subroutines exist that are not
  tests.** Beyond the 51 `@test` subroutines, the file also contains
  3 plain (non-`@test`) subroutines/functions used as shared
  per-sub-topic test support, found by the same `contains`-section
  read: `single_predecessor` (13 lines, line 1376 — used once, inside
  the materialize-extensions-gating bucket), `build_tracer_hierarchy`
  (64 lines, line 2718 — used 4 times, across the callback-wiring
  bucket's own 4 tests that need a multi-level tracer fixture), and
  `resolve_vertical_grid_membership_mismatch` (69 lines, line 2533 —
  used twice, inside the vertical-grid-composite-overlap bucket). None
  of these is `make_outer_meta`/`make_var` (17-1's own scope, already
  deduped) — they are sub-topic-LOCAL fixture builders, each used only
  within their own single bucket, so (unlike `make_outer_meta`) they
  present no cross-bucket dependency risk for 17-5's own splitting —
  each would move cleanly with its one bucket into a new file.

  | # | Sub-topic bucket | Test subroutines | Submodule(s) actually exercised | Classification | Shared-fixture note |
  |---|---|---|---|---|---|
  | 1 | Advertising: basic create/reachable/idempotent | `test_advertise_creates_state_item_nodes`, `test_public_port_recorded_and_reachable_by_name` | Advertising (`graphbuilder_advertise`→`advertise_one`) exclusively | (b) split candidate | none beyond `make_outer_meta`/`make_var` |
  | 2 | Ordinary (`MatchConnection`) resolution: match/unresolved | `test_ordinary_connection_wires_matching_and_reports_unresolved`, `test_ordinary_connection_units_mismatch_creates_chain` | ConnectionResolution (`resolve_match_connection`) exclusively | (b) split candidate | none |
  | 3 | Inout borrowing (direct-alias/mismatch/no-owner/chained) | `test_inout_direct_alias_wires_forward_and_return_edges`, `test_inout_mismatch_is_rejected_not_chained`, `test_inout_borrower_with_no_owner_is_rejected`, `test_inout_chained_borrowing_is_rejected` | ConnectionResolution (`resolve_inout_destination`) exclusively | (b) split candidate | none |
  | 4 | Activate-time unsatisfied-import check (two-phase timing) | `test_check_unsatisfied_imports_reports_without_mutating_graph` | ConnectionResolution (`graphbuilder_check_unsatisfied_imports`/`check_match_connection_unsatisfied`) exclusively | (b) split candidate (1-test bucket — marginal value alone, see 17-5 recommendation) | none |
  | 5 | `SimpleConnection` resolution (exact/renamed/chain/unresolved/idempotent/inout) | `test_simple_connection_resolves_exact_match`, `test_simple_connection_resolves_with_different_src_dst_names`, `test_simple_connection_resolves_through_extension_chain`, `test_simple_connection_unresolved_import_is_reported`, `test_simple_connection_resolution_is_idempotent`, `test_simple_connection_resolves_inout_different_names` | ConnectionResolution (`resolve_match_connection`'s `class(Connection)` dispatch, `effective_source_name` per ARCHITECTURE.md §1.2) exclusively | (b) split candidate | none |
  | 6 | Freeze (success/conflict) | `test_freeze_succeeds_when_valid`, `test_freeze_fails_and_leaves_graph_unfrozen_on_conflict` | Freeze (`graphbuilder_freeze`) exclusively | (b) split candidate — mirrors `Test_GraphBuilder_ConvergenceCheck.pf`'s own precedent almost exactly (that file already split out the hierarchy-wide convergence/freeze-adjacent check; this bucket is the per-component freeze call itself, a sibling concern, not yet split) | none |
  | 7 | Materialize-extensions gating (disabled/unsupported-class/vgrid-mismatch/registry-mode) | `test_materialize_extensions_disabled_leaves_placeholder`, `test_materialize_extensions_reports_unsupported_item_class`, `test_materialize_extensions_vgrid_mismatch_unaffected`, `test_exact_match_not_materialized_in_registry_mode` | ConnectionResolution (`resolve_match_connection`→`try_materialize_field_extension`, a parent-helper call from inside ConnectionResolution) | (b) split candidate — tests ConnectionResolution's own call-site behavior around the gate, not the parent helper's internals directly (no test here calls `try_materialize_field_extension` by name) | uses `single_predecessor` (local to this bucket) |
  | 8 | Visualization label map | `test_build_label_map_labels_own_advertised_items` | Visualization (`graphbuilder_build_label_map`) exclusively | (b) split candidate (1-test bucket) | none |
  | 9 | Composite advertise/readvertise (tree structure) | `test_composite_advertise_creates_node_tree`, `test_composite_advertise_multi_level`, `test_composite_readvertise_does_not_duplicate` | Advertising (`advertise_one`'s composite-member recursion) exclusively | (b) split candidate — same submodule as bucket 1, a strong argument for merging buckets 1+9 into ONE `Test_GraphBuilder_Advertising.pf` rather than two separate new files (see 17-5 recommendation) | none |
  | 10 | Composite connection wiring (whole-composite match, member non-addressability) | `test_ordinary_connection_wires_matching_composite`, `test_composite_member_not_independently_addressable` | ConnectionResolution (`for_each_matching_import` against composite member nodes) exclusively | (b) split candidate — same submodule as buckets 2/3/4/5/7, another strong merge-not-separate-file argument (see 17-5 recommendation) | none |
  | 11 | Vertical-grid composite tagging (variant selection) | `test_vertical_grid_composite_tags_variant`, `test_untagged_composite_keeps_plain_state_variant` | Advertising (`advertise_one`'s `state_item_variant` tagging) exclusively | (b) split candidate — same submodule as buckets 1/9 | none |
  | 12 | Vertical-grid composite overlap resolution (match/single/zero/multiple) | `test_vertical_grid_composites_match_wires_directly`, `test_vertical_grid_composites_single_overlap_fails_explicitly`, `test_vertical_grid_composites_zero_overlap_fails_incompatible`, `test_vertical_grid_composites_multiple_overlap_fails_ambiguous` | ConnectionResolution (`resolve_match_connection`→`find_mismatched_characteristics`/`VerticalCoordinateCharacteristic`, confirmed via `grep VerticalCoordinateCharacteristic GraphBuilder_ConnectionResolution_smod.F90`) exclusively | (b) split candidate — same submodule as bucket 10 | uses `resolve_vertical_grid_membership_mismatch` (local to this bucket) |
  | 13 | Qualified-export namespace building | `test_build_qualified_export_namespace_multilevel`, `test_qualified_name_matches_virtualconnectionpt_convention` | CallbackWiring (`build_qualified_export_namespace`/`collect_qualified_exports`) exclusively | (b) split candidate | none |
  | 14 | `query_component_exports` (History-support query) | `test_query_component_exports_scoped_to_resolved_component`, `test_query_component_exports_deep_hierarchy_no_propagation` | **Neither CallbackWiring nor ConnectionResolution** — calls the parent-module shared helper `query_component_exports` directly, never through any `gb%` type-bound method | (a) tests `GraphBuilder.F90`'s own shared logic — stays, same "shared code colocated with its test" reasoning as `item_key`'s own tests. Note: this bucket is currently physically adjacent to bucket 13 (CallbackWiring) in the file, which is why the roadmap's own coarse-group list above lumped it under "qualified-export namespace/`query_component_exports`" as one group — characterization shows these are two DIFFERENT sub-topics, one submodule-specific (13) and one parent-shared (14) |
  | 15 | Non-callback destination unaffected (negative control) | `test_non_callback_destination_unaffected` | ConnectionResolution (ordinary `resolve_match_connection` path, confirming callback dispatch is NOT triggered) | (b) split candidate, but see ambiguity note: this test exists specifically to prove ConnectionResolution and CallbackWiring do NOT interact for this input — a split that separates ConnectionResolution tests from CallbackWiring tests needs this test to stay with ConnectionResolution (it tests ConnectionResolution's own dispatch decision), not move to a hypothetical CallbackWiring file, even though its name/comment reads as "callback-adjacent" |
  | 16 | `resolve_callback_import` (direct, matches/empty) | `test_resolve_callback_import_matches_and_rejects`, `test_resolve_callback_import_no_match_is_empty` | **Parent-module shared helper**, called directly (not through `gb%`) — but `resolve_callback_import` itself is classified 17-2(c) as CallbackWiring-leaning (its only non-test caller is ConnectionResolution, yet it exists to serve callback resolution conceptually) | (c) ambiguous — tests a parent-level helper whose own 17-2 classification is itself ambiguous (single-caller-in-ConnectionResolution but conceptually CallbackWiring's own domain); splitting this bucket requires 17-4 to first decide where `resolve_callback_import` itself lives, so 17-5 cannot resolve this bucket independently of 17-4's own outcome | none |
  | 17 | Callback collection materialization + wiring (full pipeline) | `test_callback_collection_materializes_and_wires_import` | ConnectionResolution (`resolve_callback_destination`, called via `gb%resolve_connections`) | (b) split candidate — despite "callback" in the name, this test drives the full pipeline through ConnectionResolution's own `graphbuilder_resolve_connections` entry point, exercising CallbackWiring only transitively (`resolve_callback_destination` itself lives in ConnectionResolution per 17-2's own finding, calling into CallbackWiring's `materialize_callback_collection`) — belongs with bucket 2/3/4/5/7/10/12/15 (ConnectionResolution), not bucket 13/16 (CallbackWiring), a genuine "test named for a user-facing behavior actually exercises a different submodule" case the task prompt's own framing anticipated |
  | 18 | Callback state/method binding construction (direct) | `test_build_callback_state_binding_uses_real_member_nodeids`, `test_callback_method_binding_builds_get_put_networks` | CallbackWiring (`build_callback_state_binding`/`build_callback_method_binding`, both called directly, not through `gb%`) exclusively | (b) split candidate | none |
  | 19 | Run-phase-node construction (build/idempotent/registry-mode-noop) | `test_build_run_phase_node_constructs_node_from_ports`, `test_build_run_phase_node_is_noop_in_registry_mode`, `test_build_run_phase_node_is_idempotent` | RunPhaseNode (`graphbuilder_build_run_phase_node`) exclusively | (b) split candidate | none |
  | 20 | `resolve_qualified_component` (direct path resolution) | `test_resolve_qualified_component_single_segment_self`, `test_resolve_qualified_component_multi_level`, `test_resolve_qualified_component_cannot_name_ancestor` | **Parent-module shared helper**, called directly — 17-2(a) genuinely cross-cutting (CallbackWiring + ConnectionResolution both call it) | (a) tests `GraphBuilder.F90`'s own shared logic — stays, same reasoning as bucket 14 |
  | 21 | Multi-level qualified-component connection resolution (end-to-end) | `test_ordinary_connection_resolves_through_multi_level_names` | ConnectionResolution (`resolve_match_connection`, exercising `resolve_qualified_component` transitively through the real connection-resolution path, not directly) | (b) split candidate — contrast with bucket 20: this test calls `gb%resolve_connections`, never `resolve_qualified_component` directly, so it is testing ConnectionResolution's OWN use of the shared primitive, not the primitive itself — a real example of "a test exercising a shared helper's logic indirectly through one specific submodule's call site," distinct from bucket 20's direct-call shape |
  | 22 | Node-identity-based resolution (no live `VariableSpec` dependency) | `test_resolution_succeeds_with_no_live_var_spec_reference` | ConnectionResolution (`resolve_match_connection`) exclusively | (b) split candidate (1-test bucket); could merge into bucket 2 (both exercise `resolve_match_connection`'s ordinary path, this one as a regression-style single test) | none |
  | 23 | Materialize-extensions-gate `@before`/`@after` fixture plumbing | (not a test — the `reset_materialize_extensions_gate`/`restore_materialize_extensions_gate` pair, lines 102-116) | N/A — pFUnit lifecycle hooks, not production-submodule logic | (c) ambiguous/file-structural, not submodule-attributable — but load-bearing for 17-5: per this file's own header comment (lines 74-101), pFUnit's `@before`/`@after` typing is NOT disambiguated per-test, so EVERY `@test` in whatever file ends up containing bucket 7 (materialize-extensions gating) MUST be `MpiTestMethod`-typed like this pair — this is exactly the constraint that already forced 5 `ESMF_TestMethod` tests out to `Test_GraphBuilderMaterializeGate.pf` (its own separate file, already landed) for the SAME reason; 17-5 splitting bucket 7 into its own file needs to either carry this exact `@before`/`@after` pair along or confirm every other test remaining in `Test_GraphBuilder.pf` is still MpiTestMethod-typed after the split |

  **17-3 classification summary:** 23 buckets covering all 51 tests
  (bucket sizes: 2,2,4,1,6,2,4,1,3,2,2,4,2,2,1,2,1,2,3,3,1,1,0-tests/
  structural — sums to 51 across the 22 test-bearing buckets). (a)
  stays — tests `GraphBuilder.F90`'s own shared logic: buckets 14, 20
  — **2 buckets, 5 tests**. (b) clean split candidate — tests exactly
  one submodule exclusively: buckets 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
  12, 13, 15, 17, 18, 19, 21, 22 — **19 buckets, 44 tests** (correcting
  the prompt's own "~20 sub-topics" framing — the real finer-grained
  count is 23 total buckets, 19 of which are clean splits, slightly
  MORE than "~20" implied as the ceiling if read as "most will stay";
  note bucket 15 is counted here, not in (c), despite its
  "callback"-sounding name — its own table row's "ambiguity" is about
  WHERE it is placed relative to CallbackWiring, not whether its own
  logic is cleanly single-submodule, which it is). (c) ambiguous:
  buckets 16, 23 — **2 buckets** (16 is ambiguous because its target
  helper's own 17-2 placement is undecided; 23 is structural, not a
  logic question, but gates how 17-5 must execute any split touching
  bucket 7). Headline: the roadmap's own expectation going into this
  phase — "genuinely many bundled concerns... not one verbose concern
  repeated" — is confirmed, but the FINER conclusion is sharper than
  "~20 sub-topics, split where justified" suggested: a strong majority
  (19 of 23, ~83%) are clean, single-submodule split candidates,
  concentrated overwhelmingly in ConnectionResolution (buckets
  2,3,4,5,7,10,12,15,17,21,22 — 11 of 19 clean-split buckets, 27 of 44
  clean-split tests, ~2486 lines by this session's own per-test line
  counts above, summed directly from the per-test line-count table
  earlier in this characterization) — meaning a
  `Test_GraphBuilder_ConnectionResolution.pf` extraction alone would
  move 27 tests and roughly 2500 lines out of the parent file, by far
  the single largest available reduction, dwarfing every other
  submodule's own share (the next largest, Advertising, covers only 3
  buckets/7 tests).

- **17-4. Decide and implement (or explicitly decline) the shared-
  helpers-module split (item 4 above), informed by 17-2's findings.**
  Not sized here — depends on what 17-2 finds; could be anywhere from
  "no change justified" (if most of the 16 turn out single-caller) to
  a real multi-file restructuring. Defer sizing to this step's own
  design.md once 17-2 lands. **17-2's own actual finding (this
  session):** NOT "most are single-caller" (only 7/19, 37%) and NOT
  "most are correctly-placed-shared" either (5-6/19, 26-32%) — the
  real distribution is split three ways with a nontrivial dead-code
  slice (3/19, 16%) the original framing did not anticipate. 17-4's
  own first task, before any design.md split-vs-no-split decision, is
  therefore deleting the 3 confirmed-dead helpers
  (`component_spec_for`, `enumerate_qualified_exports`,
  `collect_qualified_export_index`) — a strict net-reduction with no
  design tradeoff at all, independent of whatever 17-4 decides for the
  remaining 16. For the remaining 16: 7 clean moves, but NOT all to
  the same destination — `is_self` (6 lines) moves to
  `GraphBuilder_CallbackWiring_smod.F90` (its one caller lives there);
  the other 6 (`try_materialize_field_extension` 78,
  `materialize_extension_chain` 69, `merged_item_for_link` 47,
  `resolve_local_node_id` 27, `is_ordinary_resolvable_import` 14,
  `resolve_callback_destination` 39 — 274 lines) move to
  `GraphBuilder_ConnectionResolution_smod.F90` (every one of their
  single callers lives there) — a real, sizeable (274 + 6 = 280 line)
  reduction with NO module-interface tradeoff, since none of the 7 has
  a second caller anywhere. The 4 promote-candidates
  (`component_for`, `query_component_exports`, `itemtype_name`,
  `resolve_callback_import`) are weaker evidence each (see 17-2 table
  notes) — 17-4's own design.md should treat these as "leave in parent
  for now, revisit if/when History actually calls `query_component_exports`
  or CallbackWiring actually grows a second `component_for`/
  `itemtype_name`/`resolve_callback_import` call site" rather than
  preemptively promoting to module interfaces on speculative evidence
  alone. The remaining 5-6 genuinely cross-cutting helpers
  (`item_key`, `resolve_qualified_component`,
  `append_own_export_index_entries`, `get_item_units`,
  `get_item_typekind`, borderline `itemtype_name`) are the real
  `GraphBuilder_SharedHelpers.F90` candidates IF 17-4 decides that
  structural option is worth it — but note `get_item_units`/
  `get_item_typekind` are only cross-cutting AMONG THE OTHER SHARED
  HELPERS, never called by a submodule directly; if the 7 clean-move
  helpers above all move to ConnectionResolution, `get_item_units`/
  `get_item_typekind`/`merged_item_for_link`'s own caller set changes
  (their only callers would then live in ConnectionResolution too) and
  they could plausibly move along with the group rather than staying
  parent-level or getting a dedicated shared-helpers file — 17-4's own
  design.md needs to re-derive this dependency chain AFTER deciding
  the 7-helper move, not independently of it.
- **17-5. Split `Test_GraphBuilder.pf` per 17-3's findings, where
  justified.** Same caveat as 17-4 — sizing depends on 17-3's own
  output; likely several sessions if a genuine per-submodule split is
  warranted across ~20 sub-topics, one session if 17-3 concludes most
  sub-topics are legitimately testing shared cross-submodule behavior
  and should stay put. **17-3's own actual finding (this session):**
  19 of 23 buckets (44 of 51 tests) are clean single-submodule split
  candidates, concentrated heavily in ConnectionResolution (11 buckets,
  27 tests) — 17-5's own design.md should NOT create one new file per
  bucket (19 tiny files would be worse than one large one); instead,
  group clean-split buckets BY SUBMODULE, mirroring
  `Test_GraphBuilder_ConvergenceCheck.pf`'s own one-file-per-submodule-
  concern precedent:
  `Test_GraphBuilder_ConnectionResolution.pf` (buckets 2,3,4,5,7,10,12,
  15,17,21,22 — 11 buckets/27 tests/~2500 lines, by far the largest
  extraction and the one that actually shrinks the parent file
  meaningfully),
  `Test_GraphBuilder_Advertising.pf` (buckets 1,9,11 — 3 buckets/7
  tests, all already same-submodule per the merge note in the table),
  `Test_GraphBuilder_CallbackWiring.pf` (buckets 13,18 — 2 buckets/4
  tests; buckets 16 deferred — see below),
  `Test_GraphBuilder_Freeze.pf` (bucket 6 — 2 tests; small, but a
  direct sibling of the already-split `Test_GraphBuilder_ConvergenceCheck.pf`,
  so splitting it maintains that file's own precedent rather than
  leaving an inconsistent "freeze is split here but not there" shape),
  `Test_GraphBuilder_RunPhaseNode.pf` (bucket 19 — 3 tests),
  `Test_GraphBuilder_Visualization.pf` (bucket 8 — 1 test; marginal
  value for a 1-test file — 17-5's own design.md should explicitly
  decide whether a 1-test file is worth the overhead or whether bucket
  8 stays in whatever remains of the parent file as a deliberate
  exception). The 2 parent-shared buckets (14, 20 — `query_component_exports`/
  `resolve_qualified_component` direct tests) stay in
  `Test_GraphBuilder.pf` itself, alongside bucket 23's own `@before`/
  `@after` pair (which must travel with whichever tests still need
  MpiTestMethod typing — likely most of what remains after the
  ConnectionResolution extraction, since bucket 7's materialize-
  extensions-gating tests are themselves moving to the new
  ConnectionResolution file and would need to carry this exact pair
  with them, confirmed by this file's own header comment explaining
  why the ESMF_TestMethod tests already had to move OUT for the
  opposite reason). Bucket 16 (`resolve_callback_import` direct tests)
  is explicitly NOT assigned to a destination file by this
  characterization — 17-4 must first settle `resolve_callback_import`'s
  own placement (parent vs. move vs. promote) before 17-5 can decide
  whether bucket 16 travels with it or stays put; sequencing 17-5
  strictly after at least the `resolve_callback_import`-specific part
  of 17-4 avoids rework. Bucket 15 (`test_non_callback_destination_unaffected`)
  goes to `Test_GraphBuilder_ConnectionResolution.pf` despite its
  "callback"-sounding name, per the table's own note — a concrete
  instance of the task prompt's own warned-about trap ("a test named
  for a user-facing behavior may actually be exercising one specific
  submodule's procedure"). Estimated net effect if this grouping is
  followed in full: `Test_GraphBuilder.pf` itself shrinks from 3935
  lines to roughly 5 tests' worth of real content (buckets 14+20, 5
  tests, ~270 lines by this session's own measured test-body-line
  counts above) plus shared module-level fixtures/`@before`/`@after`/
  `use` statements (currently ~130 lines of file header before the
  first test) — call it ~400 lines remaining, a ~90% reduction in
  this one file's own size, with the removed content redistributed
  across 6 new submodule-specific files rather than deleted. This is
  advisory sizing input for 17-5's own design.md, not a commitment.

### 20.4.18 Phase 18 scope: subdirectory reorganization of `superstructure/generic/graph/`

**Discovered gap, not previously tracked by any phase above.** Raised
directly by the codebase owner: large families of related files in
`superstructure/generic/graph/` are hard for a human to see the
structure of, flat in one directory. Requested subdirectories:
`characteristics/`, `transforms/`, `callback/`, `id_types/` (folding
`include/` into it).

**Measured family membership, this session (corrects the prior
informal pass's own secondhand counts in two places, named below):**

- **`characteristics/`** — 15 files, confirmed:
  `AttributesCharacteristic.F90`, `CharacteristicStatus.F90`,
  `ConservationCharacteristic.F90`, `GeometryCharacteristic.F90`,
  `NormalizationCharacteristic.F90`, `PhysicalUnitsCharacteristic.F90`,
  `QuantityTypeCharacteristic.F90`, `SetSharedCharacteristic.F90`,
  `StandardNameCharacteristic.F90`, `StateItemCharacteristic.F90`,
  `StateItemCharacteristicDetection.F90`,
  `StateItemCharacteristicKind.F90`, `TypeKindCharacteristic.F90`,
  `UngriddedDimsCharacteristic.F90`,
  `VerticalCoordinateCharacteristic.F90`. Matches the prior pass's own
  count (15).
- **`transforms/`** — 5 files, confirmed: `Transform.F90`,
  `TransformBase.F90`, `TransformGraphNode.F90`,
  `TypeKindConverterTransform.F90`, `UnitsConverterTransform.F90`.
  Matches.
- **`callback/`** — **7 files at the top level**, not the prior pass's
  reported figure (which conflated top-level files with
  `containers/*Callback*.F90` gFTL map instantiations):
  `CallbackArgumentSpec.F90`, `CallbackInterface.F90`, `CallbackInterfaceId.F90`,
  `CallbackInterfaceRegistry.F90`, `CallbackMethodBinding.F90`,
  `CallbackMethodSpec.F90`, `CallbackStateBinding.F90`. The five
  `containers/Callback*Map.F90` gFTL instantiations
  (`CallbackArgumentSpecMap.F90`,
  `CallbackInterfaceIdInterfaceMap.F90`,
  `CallbackMethodAttachmentMap.F90`, `CallbackMethodSpecMap.F90`,
  `CallbackServiceNameIdMap.F90`) already live under `containers/` — a
  separate, pre-existing convention this phase's own design.md should
  decide whether to fold into `callback/containers/` (consistent with
  the family-subdirectory idea) or leave under the shared top-level
  `containers/` (consistent with every OTHER family's own map
  instantiations staying there too, e.g.
  `PortIdStringMap.F90`/`NodeIdGraphNodeMap.F90` are not characteristic-
  or transform-specific either) — **recommend the latter**, to avoid
  this phase inventing a per-family `containers/` convention nothing
  else in the directory follows.
- **`id_types/`** — 5 real Id-type files use the template via a
  genuine `#include` (confirmed by grepping for the literal
  `#include "IdTemplate.inc"` directive, not just a comment mentioning
  it): `NodeId.F90`, `DependencyNetworkId.F90`, `PortId.F90`,
  `CallbackInterfaceId.F90`, `HierarchyId.F90`. **Correction to the
  prior pass's own count of 6**: `CallbackInterfaceRegistry.F90` and
  `superstructure/generic/graph/tests/IdGeneratorTestSupport.F90` each
  only *mention* `IdTemplate.inc` in a comment — neither has a real
  `#include` directive. The true count is 5, not 6; `CallbackInterfaceRegistry.F90`
  stays in `callback/` either way (it is not an Id type
  itself), so this correction does not change the "does
  `CallbackInterfaceRegistry.F90` move with the Id types or stay in
  `callback/`" open question's own framing, only the file count cited
  in support of it — recommend it stays in `callback/`, since it is a
  registry *of* callback interfaces, not an identity type.
  `include/IdTemplate.inc` (one file, 111 lines, confirmed) folds into
  `id_types/` directly, matching the backlog item's own suggested
  disposition; `target_include_directories(MAPL.generic PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}/include)` in
  `superstructure/generic/graph/CMakeLists.txt` must be updated to
  point at `id_types/` (or removed if every `#include` site switches
  to a relative/full path instead) as part of this move.

**`CMakeLists.txt` mechanics, confirmed trivial as the backlog item
expected**: `target_sources(MAPL.generic PRIVATE ...)` is a flat list
of 85 relative path strings
(`superstructure/generic/graph/CMakeLists.txt`, confirmed by direct
read) — a directory move is a
path-string edit per relocated file, no CMake-structural change
(no new `add_subdirectory`, no new target).
`superstructure/generic/graph/tests/CMakeLists.txt` likely needs the
same per-file path-string
treatment for every relocated production file's own test counterpart;
not separately audited here but expected to be the same mechanical
shape.

**Real dependency found and documented explicitly, per this phase's
own requirement not to leave a silent landmine:** Phase 12
(§20.4.12, wiring 5a2's seven remaining `StateItemCharacteristic`
subclasses into real dispatch) directly edits
`build_characteristics()`'s own insertion point in `GraphBuilder.F90`
and is expected to touch some subset of the same seven
`characteristics/`-bound files this phase would relocate
(`AttributesCharacteristic.F90`, `UngriddedDimsCharacteristic.F90`,
`ConservationCharacteristic.F90`, `NormalizationCharacteristic.F90`,
`QuantityTypeCharacteristic.F90`, `StandardNameCharacteristic.F90`,
and the vertical-grid-adjacent `ReferenceCharacteristic` analog).
Phase 7a (§20.4.5, Bracket/VectorBracket 1:1 shape dispatch) is
expected to add a NEW file to this same `characteristics/` family (a
`variant()`-mismatch-triggered `CharacteristicId`/`Characteristic`
subclass) and likely a new file to `transforms/` too
(`TimeInterpolateTransform`, per that phase's own table) — though 7a
is explicitly named "lowest urgency" there, so less time-pressured
than Phase 12. A directory move that lands WHILE either is in flight
forces that work to resolve a rename/move conflict on every file it
touches, on top of its own real work — pure git-mechanics tax, not a
design conflict, but a real one. **This phase should NOT be sequenced
as "third on a list of five," simply because it was listed third in
the backlog** — it should be sequenced either (a) last among items
1/2/3 (this item, 17, and the log-noise item), after Phase 12 has
landed, or (b) explicitly timed into the specific gap after Phase 12
lands and before Phase 7a starts (if that gap is real and not merely
assumed) — whichever the next planning pass finds actually open. Do
not schedule this phase to start while Phase 12 is in flight.

**Scope — four independent moves, each a pure `git mv` + CMake
path-string update + import-statement audit (nothing in this phase
changes any module's own public interface or behavior):**

1. Move the 15 `characteristics/` files.
2. Move the 5 `transforms/` files.
3. Move the 7 top-level `callback/` files (leaving
   `containers/Callback*Map.F90` where every other family's own map
   instantiations already live, per the recommendation above).
4. Move the 5 `id_types/` files plus `include/IdTemplate.inc`; update
   the `#include` directive in each of the 5 moved files (relative
   path, likely unchanged if `IdTemplate.inc` moves to the same new
   directory) and the `target_include_directories` CMake entry.

**Not blocking anything above or already landed on its own technical
merits** — pure file reorganization, zero behavior change by
construction. **Dependency note (explicit):** confirmed independent of
Phase 6a's prerequisite list (§20.4.11) and Phase 13 — neither touches
these files. **Real, documented dependency on timing relative to Phase
12 and (lower-urgency) Phase 7a**, as described above — this is the
one item in this backlog where "independent lane, proceeds in
parallel" does NOT hold without qualification; the qualification is
sequencing, not a technical blocker in either direction.

**Sub-sequencing, in recommended order (sequencing-gated, not
session-size-gated — each move below is small on its own):**

- **18-1. Move `transforms/` (5 files).** Smallest family, no
  Phase-12 overlap at all (Phase 12 does not touch any of the 5
  transform files). Sized for one session, including the CMake
  path-string edits and a full-suite `ctest -L ESSENTIAL` rerun to
  confirm the move alone changed nothing. Safe to do anytime,
  independent of Phase 12/7a timing.
- **18-2. Move `id_types/` + fold in `include/`.** Also no Phase-12
  overlap. Sized for one session: 5 tiny files (12-16 lines each) plus
  the template; the only real risk is the `target_include_directories`
  path update and confirming every `#include "IdTemplate.inc"` site
  still resolves. Exit criterion: `cmake --build <dir> --target
  build-tests` succeeds with no "file not found" error on any of the 5
  `#include` sites, and `ctest -L ESSENTIAL` passes with 0 new
  failures.
- **18-3. Move `callback/` (7 top-level files).** No Phase-12 overlap
  (Phase 12 does not touch the callback family). Sized for one
  session.
- **18-4. Move `characteristics/` (15 files) — sequence AFTER Phase 12
  lands, not before, per the real dependency documented above.** Not
  sized as "one session" here deliberately — restate at the time this
  step is actually picked up, once it is confirmed Phase 12 (and, if
  still pending, Phase 7a) has landed or is confirmed not concurrently
  touching these files. If Phase 12 is confirmed to have landed by the
  time this step starts, this is likely also a one-session mechanical
  move; the caveat is entirely about timing, not mechanical
  complexity.

### 20.4.19 Phase 19 scope: deduplicate the two `sorted_names` helpers (item 5, narrowed)

**Discovered gap, not previously tracked by any phase above — scope
corrected mid-investigation, recorded here per this document's own
3b/3b2 discipline for a planning-session-discovered correction.** The
backlog's own original item 5 ("some growable arrays of strings should
be `StringVector` instead of raw fixed-width character arrays") named
four candidate call sites
(`GraphBuilder_CallbackWiring_smod.F90:606,640` and
`AttributesCharacteristic.F90`/`VerticalCoordinateCharacteristic.F90`'s
own `sorted_names` helpers). **Direct verification during this planning
session found none of the four are `StringVector` candidates by the
backlog owner's own stated criterion** (known size at allocation time
stays a plain array): all four `allocate(character(LEN) ::
name_array(known_size))` at a size already known from a map's own
`%size()` call before the loop runs — nothing grows unboundedly here.
Corrected line numbers while verifying, as a secondary finding: the two
`GraphBuilder_CallbackWiring_smod.F90` call sites are at lines **610
and 645** (`allocate(character(MAX_CALLBACK_ARGUMENT_NAME_LEN) ::
names(...))`), not 606/640 as originally reported — those two line
numbers are the `character(:), allocatable :: names(:)` DECLARATIONS,
two lines above each real `allocate` call.

**The real, actionable issue at these four sites is a different one
than the backlog described: duplication, not typing.**
`AttributesCharacteristic.F90`'s own `sorted_names` (lines 149-177) and
`VerticalCoordinateCharacteristic.F90`'s own `sorted_names` (lines
329-357) are near-identical insertion-sort implementations over a
`StringVector` input — the former's own comment admits this directly:
"reimplemented here... since that module's own copy is private."
Scope: extract one shared `sorted_names(names) result(sorted)` helper
(signature `type(StringVector), intent(in) :: names` ->
`character(:), allocatable :: sorted(:)`, exactly matching both
existing copies) into a small, new or existing shared utility module
under `superstructure/generic/graph/` (exact home TBD at
implementation time — a new `StringSortUtilities.F90`, or added to an
existing general-utility module if one already fits), and have both
`characteristics/`-family call sites `use` it instead of each keeping
its own copy. The two `GraphBuilder_CallbackWiring_smod.F90` call
sites are unrelated to this dedup (different problem shape — iterating
a `StateItemMemberMap`'s keys into a plain array for an ordinary
indexed loop, not sorting) and need no change under this phase.

**The genuine growable-fixed-width-character-array-grown-in-loop
pattern this backlog item's criterion describes was searched for
exhaustively across `superstructure/generic/` (the graph/generic
codebase) and NOT FOUND.** Every `character(...), allocatable ::
name(:)`-shaped declaration in that tree (44 total matches across the
repo, enumerated and individually checked) is either (a) sized once
from a known count before any loop runs (the four backlog-named sites;
`GraphStateItem.F90`/`MethodInvocation.F90`/`OpenMP_Support.F90`'s own
ESMF-item-query patterns, same shape), or (b) a scalar character
buffer, not an array. The one real repo-wide instance of the pattern
item 5's criterion actually describes — a character array
`allocate`d, then explicitly grown one element at a time via a
`deallocate`+bigger-`allocate`+copy sequence —
is `mp_utils/profiler/reporting/CsvProfileReporter.F90:add_column`'s
own `column_names` growth (confirmed by direct read, lines 128-139),
which is **outside the graph/generic codebase entirely** (profiler
reporting infrastructure, unrelated capability, not touched by any
graph-architecture work). **Recommendation: do not scope a Phase 19
code change against this file** — it is a real instance of the
anti-pattern the backlog's criterion names, but converting it is
general code-quality work in an unrelated subsystem, not graph-roadmap
work, and should be tracked (if at all) wherever that subsystem's own
maintenance backlog lives, not here. See §20.4.20 below for where this
distinction (graph-roadmap vs. general-codebase cleanup) is discussed
more fully for item 2's log-noise case, which raises the identical
scoping question.

**Not blocking anything above or already landed.** Independent of
Phase 6a's prerequisite list, Phases 7/12/13, and Phases 17/18 above
(disjoint files: the two `characteristics/` files this phase touches
are read-only for Phase 12's own purposes — Phase 12 adds dispatch
wiring in `GraphBuilder.F90`, not inside either `sorted_names`
helper).

**Sub-sequencing:** single step, sized for well under one session —
extract one shared function, update two call sites, re-run the
existing `Test_StateItemCharacteristicSubclasses.pf`/
`Test_StateItemCharacteristic.pf` coverage (whichever already exercises
`AttributesCharacteristic%get_signature`/
`VerticalCoordinateCharacteristic%get_signature`, the two call paths
that reach `sorted_names`) with 0 new failures as the exit criterion.
Small enough to fold into Phase 18's own `characteristics/` move
(18-4) as a drive-by cleanup if convenient, but does not require
waiting for that move — may land independently, anytime.

### 20.4.20 Phase 20 scope: test-suite log noise — placement decision, not a code-change phase

**Discovered gap, not previously tracked by any phase above — raised
directly by the codebase owner.** Two distinct, compounding noise
sources were named: (a) "field dictionary not found"-style warnings
fired by nearly every test fixture that does not load one, and (b)
`GraphBuilder: <step> step failed with rc=N - continuing without graph
representation for this step`, fired even by tests deliberately
exercising a failure path (e.g.
`test_unresolved_import_after_freeze_fails_explicitly`,
`superstructure/generic/tests/Test_GraphBuilder_ConvergenceCheck.pf`),
making "intentional negative-path test" indistinguishable from
"something actually broke" in a scan of the log.

**Verified, source-level, this session (log-volume claims from a prior
informal pass could not be independently re-verified against a live
`ctest -V` run — this planning session's own explicit non-goals
exclude running builds/tests; what follows is confirmed from source
and from existing, already-present log files only, with the gap
between those two named honestly):**

- **(a) is confirmed real and NOT graph-specific.** The warning text
  ("short_name/standard_name ... not found in field dictionary ...")
  originates in `superstructure/generic/specs/VariableSpec.F90`'s own
  `apply_field_dictionary_defaults_` (lines 411-430), called from
  ordinary `VariableSpec` construction — this fires for ANY test
  fixture using a `short_name`/`standard_name` the loaded field
  dictionary (or no field dictionary at all) does not recognize,
  completely independent of graph-native vs. legacy code paths. A
  pre-existing, already-present log file predating all
  `hierarchy-wide-component-graph` work (`nag/ctest-full.log`, dated
  Oct 15, confirmed via `git log` to predate the Oct 17 `b86eb4dc`
  commit that
  landed that whole change) already shows 61 occurrences of "field
  dictionary" and 74 total `WARNING` lines in a single `ctest`
  run — this noise source predates and is unrelated to the graph
  architecture work this roadmap otherwise tracks.
- **(b) is confirmed real, graph-specific, and reproduced from an
  existing log** (`nag/ctest-generic2.log`, a `-V`-style per-test
  console-output capture from an earlier working-tree state before
  Group 9's freeze relocation): 31 occurrences of `GraphBuilder: ...
  step failed with rc=` in that one log, several in immediate
  back-to-back triplets/quadruplets (e.g. three consecutive
  `resolve_connections step failed` lines against one
  `CAP_EQUIVALENCE` test) — confirming the "indistinguishable from a
  real failure in a casual scan" complaint directly, not just in
  principle. **Caveat, stated honestly rather than extrapolated:**
  this log predates Group 9's rename of several `report_if_failed`
  step-name arguments (e.g. `convergence_check` in that log is now
  `graph_freeze:convergence_check`, confirmed by reading
  `GraphBuilder_Freeze_smod.F90` directly) — the specific step-name
  strings in a fresh run would differ, but the underlying mechanism
  (`report_if_failed`, `GraphBuilder_LifecycleHooks_smod.F90:148-160`)
  and its 25 real call sites across
  `GraphBuilder_ConnectionResolution_smod.F90`,
  `GraphBuilder_Freeze_smod.F90`, `GraphBuilder_Geometry_smod.F90`,
  and `GraphBuilder_LifecycleHooks_smod.F90` are unchanged, so the
  volume/ambiguity
  problem itself is still live. **The prior informal pass's own
  "~830 warning-level log lines in one `ctest -L ESSENTIAL -V` run"
  figure was NOT independently reproduced this session** (no current
  `-V` run was performed, per this session's own non-goals) — it is
  plausible given the two confirmed-real sources above, but is
  reported here as unverified, not as a number this roadmap entry
  relies on.

**Judgment call, made explicitly rather than silently (per this task's
own instruction): does this belong in `20-implementation-roadmap.md`
as a numbered phase, or elsewhere?** This document's own §1 states its
purpose is "the phased implementation approach" for "the graph-native
architecture" specifically. Source (a) above is confirmed NOT
graph-specific — it is a pre-existing `VariableSpec`/field-dictionary
interaction that would exist with zero graph-architecture code in the
tree at all. Source (b) IS graph-specific (`GraphBuilder.F90`'s own
`report_if_failed` convention), but its fix is a pure log-hygiene
change (e.g. demoting routine/expected `report_if_failed` calls from
`warning` to a lower level, or having negative-path tests themselves
suppress/expect the warning rather than changing the production code
path) — it does not sequence against, block, or get blocked by any
other phase in this document, unlike every other discovered-gap phase
above (7-18), each of which is tied to a real graph-architecture
capability or cleanup milestone. **Checked, as instructed, whether a
general code-quality/maintenance backlog document already exists
elsewhere under `docs/` for this kind of cross-cutting item: it does
not** (`docs/` contains only `Ford/`, `mapl3/`, `graph/`, `user_guide/`
— no `backlog.md`/`maintenance.md`/`quality.md` or equivalent, confirmed
by directory listing). **Decision: record this phase number and scope
here, for traceability (the backlog owner explicitly asked for it to
be "worked into the roadmap"), but keep the entry itself short and
treat the actual fix as a one-line pointer to source (b)'s own
mechanism rather than a graph-sequencing problem** — mirroring this
document's own convention (used throughout §20.4) of a short pointer
in one document to detail that properly lives in the source/code
itself, not a duplicate restatement. If a general cross-cutting
maintenance backlog document is ever created under `docs/` (it is not,
as of this writing), source (a) above belongs there instead of here,
since it has nothing to do with the graph architecture; source (b)
would still arguably belong in a graph-specific document like this
one, since the mechanism it concerns (`report_if_failed`) is
graph-specific even though the complaint (log noise) is a generic
test-hygiene concern.

**Scope, if/when picked up:**
1. Source (b) only (in-scope for this document): decide, as a planned
   design decision, whether `report_if_failed`'s own `lgr%warning`
   call should become `lgr%info` or `lgr%debug` for the specific case
   where the caller already knows the failure is an expected
   negative-path outcome (not generally true — most callers legitimately
   want a warning for a real unexpected failure) — likely requiring a
   new optional `expected` flag on `report_if_failed` itself, set only
   by call sites that are themselves part of an explicitly-tested
   negative path, rather than a blanket severity change that would
   also silence genuinely unexpected failures.
2. Source (a) (out of scope for this document per the judgment call
   above): no action recommended here; flag for whoever owns
   `VariableSpec.F90`/field-dictionary test fixture setup generally,
   independent of any graph-architecture work.

**Not blocking anything above or already landed — independent of every
other phase in this section**, including Phases 17-19 immediately
above (disjoint concern: logging severity, not file organization or
type choice).

### 20.4.21 Phase 21 scope: standalone hierarchy-wide graph diagnostic app

**Discovered gap, not previously tracked by any phase above — raised
directly by the codebase owner, reframed during this planning session
from a low-priority dev convenience into a diagnostic-tooling need.**
Original backlog wording: "a standalone app that takes a top-level
component's YAML, builds the full graph, and renders it as a picture in
a file — one diagram per scenario." **Reframing, per explicit
clarification during this session:** this is not a one-off developer
script. Two concrete drivers: (1) GEOS today mostly relies on default
characteristic matching with regridding confined to I/O — no system has
yet operated at the complexity graph-native mode actually supports, so
there is no accumulated human intuition for what a real, non-trivial
graph looks like; a way to actually SEE the resulting graph has
diagnostic value no amount of source-reading substitutes for. (2) the
intended audience includes a human debugging a failing/complex scenario
test *alongside an agent*, not only a developer running a script
locally — output needs to be usable both ways: a human looking at a
rendered image, and an agent parsing structured data (the existing JSON
output, REQ-VIZ-009) to reason about the same graph programmatically in
the same session.

**Checked against Q18/REQ-VIZ-005 and Phase 3d's own scope note before
scoping this as a new phase, per this session's own required
check:** Q18 (`docs/graph/spec/17-open-questions.md`) is about whether
a HIERARCHY-WIDE (multi-`ComponentGraph`, composed-at-proxy-boundaries)
*export* is needed for v1.0, versus per-component export alone being
sufficient — it recommends deferring hierarchy-wide composition "until
the global graph view... is itself undertaken." Phase 3d
(§20.4.1) landed only the per-component enrichment layer
(`graphbuilder_build_label_map`, confirmed read directly,
`GraphBuilder_Visualization_smod.F90`) — its own header comment states
plainly: "`GraphExport.F90` itself remains single-ComponentGraph-scoped
(REQ-VIZ-005 hierarchy-wide export is still not implemented)." **This
phase is NOT simply "the thing that unblocks Q18"** — it does not
require solving the general hierarchy-wide stitching-at-proxy-
boundaries problem Q18 defers (proxy nodes no longer exist at all,
post-`hierarchy-wide-component-graph` — Q18's own "stitched at proxy-
node boundaries" framing is itself now stale text, not something this
phase should resurrect). What this phase actually needs is simpler and
different in kind: a driver program that walks the real
`OuterMetaComponent` tree (already-real, already-landed:
`get_num_children`/`get_child_name`/`get_child_outer_meta`,
confirmed used by `GraphBuilder_Freeze_smod.F90`'s own
`freeze_subtree_` for exactly this kind of whole-tree walk) and calls
the EXISTING per-component `export_graph_dot`/`export_graph_json`
(`GraphExport.F90`, confirmed landed) once per component in that tree,
emitting one `.dot`/`.json` file per component (not one stitched
hierarchy-wide graph) — closer to "automate what a human would do by
hand, once per component, across a whole real configuration" than to
Q18's own harder "compose into one combined graph" question.
**Recommendation: do not create a new roadmap phase purporting to
resolve Q18 itself** (that remains open, correctly, pending the actual
global-graph-view work named in §2.4) — **this phase instead supplies
the missing driver/orchestration layer that lets per-component export
(already landed) be exercised against a real multi-component
configuration without hand-writing a test fixture for each one**, which
is a smaller, already-unblocked problem. Cross-reference added to Q18's
own entry (`17-open-questions.md`) below, rather than describing the
same work twice.

**What already exists, confirmed, that this phase builds on rather than
duplicates:**
- `GraphExport.F90` — per-`ComponentGraph` DOT/JSON export, landed,
  schema-versioned (`GRAPH_EXPORT_SCHEMA_VERSION`, confirmed read
  directly).
- `GraphBuilder_Visualization_smod.F90`'s own
  `graphbuilder_build_label_map` — per-component `NodeId -> label`
  enrichment, landed (Phase 3d).
- `tools/graph_visualization/render_graph.py` — standalone,
  stdlib-only Python script that shells out to Graphviz `dot` to turn
  a `.dot` file into an image (confirmed implemented, matching
  `openspec/specs/graph/visualization-render-script/spec.md`
  verbatim). **This already covers the "renders it as a picture in a
  file" half of the original backlog wording** — it is NOT something
  this phase needs to build.
- `superstructure/generic/tests/Test_Scenarios.pf`'s own `setup()` —
  the exact bootstrap sequence (`ESMF_GridCompCreate` from a YAML-
  backed `ESMF_Config`, `ESMF_GridCompSetServices`, then
  `GENERIC_INIT_PHASE_SEQUENCE`'s own ordered phase loop) a standalone
  app needs to reproduce to go from "a YAML file" to "a real, fully
  advertised-and-connected `OuterMetaComponent` tree, graph populated"
  — confirmed this exact sequence already exists and works, just
  embedded in a pFUnit test fixture rather than exposed as a reusable
  driver.
- `superstructure/generic/tests/graph-scenarios/` (6 existing fixture
  directories: `plain_field`, `ungridded_dims`, `vertical_field`,
  `units_mismatch_run_phase`, `units_and_typekind_mismatch`,
  `graphbuilder_propagation`) and
  `superstructure/generic/tests/scenarios/` (36 existing directories)
  — the "one diagram per scenario" half of the original wording maps
  directly onto running this phase's own new driver against each of
  these already-existing fixture directories; no new fixtures need to
  be authored to get initial diagnostic value.

**What this phase actually needs to build:**
1. A standalone driver (Fortran app under `apps/`, mirroring
   `apps/`'s own existing mix of Fortran/Python standalone tools, or a
   thin wrapper reusing `Test_Scenarios.pf`'s own `setup()` logic
   factored out of the test file into a reusable, non-pFUnit-coupled
   subroutine) that: takes a path to a top-level YAML config, builds
   the real `OuterMetaComponent` tree through the real
   `GENERIC_INIT_PHASE_SEQUENCE` (same as `Test_Scenarios.pf` does
   today), then walks the resulting tree and calls
   `graphbuilder_build_label_map` + `export_graph_dot` +
   `export_graph_json` once per component, writing one `.dot`+`.json`
   pair per component to a caller-specified output directory (naming
   convention TBD at design time — e.g. `<output_dir>/<component_path>
   .dot`, mirroring the qualified-component-path naming
   `resolve_qualified_component`/`enumerate_qualified_exports` already
   use elsewhere in `GraphBuilder.F90`).
2. Wiring to `tools/graph_visualization/render_graph.py` so a single
   invocation (or a thin shell/Python wrapper calling both the
   Fortran driver and the render script in sequence) produces actual
   image files, not just `.dot` text — closing the loop from "YAML in"
   to "picture out" the original backlog wording asked for.
3. **Audience-driven output requirement (per this session's
   reframing):** the JSON output (already schema-versioned,
   `GraphExport.F90`) must remain a first-class output of this driver,
   not an afterthought behind the image — an agent debugging alongside
   a human needs the structured form to reason about programmatically
   in the same session a human is looking at the rendered picture.
   Both outputs come from the same two already-landed Fortran export
   calls (`export_graph_dot`/`export_graph_json`), so this is a
   near-zero-marginal-cost requirement given item 1's work above, not
   a second implementation effort — flagging it as a requirement here
   specifically so a future implementer does not drop the JSON output
   as "redundant with the image."

**Sequencing recommendation — weigh against scenario-test complexity
increasing soon, per this session's reframing.** The original backlog
explicitly marked this item low-priority/last. Given the diagnostic-
value reframing above, and specifically that Phase 12 (five more
`StateItemCharacteristic` axes wired into real dispatch) and Phases
7a/7b/7c (shape-changing dispatch, Expression, Wildcard) are all
currently unimplemented, next-in-line pieces of exactly the kind of
non-trivial graph structure (multi-characteristic extension chains,
shape conversion, pattern-based fan-in) that has no accumulated
operating experience yet — **recommend sequencing this phase's own
first step (item 1 above, the driver) BEFORE Phase 12/7a-c land, not
after**, so whoever implements those phases has a working diagnostic
tool available while debugging their own new, unfamiliar graph shapes,
rather than building the diagnostic tool only after the fact once the
debugging need has already passed. This reverses the backlog's own
original "do this last" framing deliberately, with the reasoning
stated explicitly rather than silently reordered.

**Not blocking anything above.** Confirmed independent of Phase 6a's
own prerequisite list (§20.4.11): this phase only reads already-landed
`GraphExport.F90`/`graphbuilder_build_label_map`/`OuterMetaComponent`
tree-walk accessors, writes no new production graph-core code, and
Phase 6a's own prerequisites (3b2, 7a/7b, 7c-Wildcard, 12, 13) do not
depend on this phase in return. **Soft dependency, not a blocker:**
most diagnostic value for Phase 12/7a-c's own debugging accrues if this
phase's driver lands before those phases' implementation sessions
start — see the sequencing recommendation above — but nothing prevents
filing this phase's own `openspec` change independently of that timing
if a future session judges otherwise.

**Sub-sequencing:**

- **21-1. Factor `Test_Scenarios.pf`'s own `setup()` bootstrap into a
  reusable, non-pFUnit-coupled subroutine** (YAML path in, a
  constructed+fully-initialized `OuterMetaComponent` tree out) that
  both the existing test and the new standalone driver can call.
  Sized for one session: mechanical extraction, the test's own
  existing behavior is the regression check (same scenario fixtures,
  same expectations, now calling the extracted subroutine instead of
  inline code). Exit criterion: `MAPL.generic.scenarios` ctest target
  still passes 100%, identical to its pre-extraction baseline.
- **21-2. Build the standalone driver app** (item 1 above: walk the
  tree, call `graphbuilder_build_label_map`+`export_graph_dot`+
  `export_graph_json` once per component, write per-component output
  files) using 21-1's extracted bootstrap. Sized for one session
  given 21-1 is done first. Exit criterion: running the driver against
  at least 2 of the existing `graph-scenarios/` fixtures produces one
  valid `.dot`+`.json` pair per component in each fixture's tree, with
  no Fortran runtime error, and the JSON output round-trips through a
  plain `json.loads()` (or equivalent) with no parse error.
- **21-3. Wire `render_graph.py` end-to-end and confirm "YAML in,
  picture out" for at least one real scenario fixture.** Sized for
  well under one session (both halves already work independently;
  this step is only the glue + a smoke-test confirmation). Exit
  criterion: a single documented invocation sequence (driver, then
  render script) run against one real fixture produces a viewable
  image file.
- **21-4 (optional, size TBD, not required for diagnostic value):**
  extend the driver to batch-process every fixture under
  `graph-scenarios/`/`scenarios/` in one invocation, producing a full
  per-scenario image gallery — explicitly optional polish, not needed
  for the core diagnostic use case (a human/agent debugging ONE
  specific failing scenario, not browsing all of them).

**Cross-reference to add to `docs/graph/spec/17-open-questions.md` Q18
(not duplicating this phase's own text there — a pointer only, per
this document's own "short pointer, not a replacement" convention):**
note that a standalone, per-component (not hierarchy-stitched) export
driver now exists/is planned as Phase 21
(`20-implementation-roadmap.md` §20.4.21), satisfying the "debugging
use case within one component's own wiring" Q18 itself already
identifies as the more common failure mode — without resolving, and
not attempting to resolve, Q18's own harder hierarchy-wide-composition
question, which remains open exactly as recorded.

## 20.5 Cross-reference

This roadmap does not restate or supersede any requirement in `01`–`19`;
it only sequences them. If a phase boundary above turns out to be wrong
once implementation starts (e.g. a Phase 4 item turns out to be needed
earlier), update this document rather than letting the plan silently
diverge from practice — same discipline as the rest of this
specification.
