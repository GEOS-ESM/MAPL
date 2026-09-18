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
  compiled-execution optimization.
- **Phase 6 (cleanup, growable — see §20.4.2)** — retire legacy
  `StateRegistry`/`ExtensionFamily`/aspect-based coupling once the
  graph-native paths above are the actual default, plus a running list
  of small, otherwise-easy-to-lose follow-ups that are blocked on that
  retirement (e.g. 3c's own `UnitsConverterTransform` ->
  `ConvertUnitsTransform` rename).

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
- **4c. Callback data model + registry** (`15` §15.2–15.7) —
  `CallbackInterface`/`CallbackArgumentSpec`/`CallbackMethodSpec`/
  `CallbackStateBinding`/`CallbackInterfaceRegistry`. Static and
  unit-testable, no `GraphBuilder` wiring yet. No new dependency beyond
  Phase 1–3 — MAY proceed in parallel with 4a/4b if desired, though Q10's
  stated order (methods before callbacks) is the default assumption.
- **4d. Callback wiring** (`15` §15.9–15.10) — `GraphBuilder`
  wildcard/regex expansion against the flattened qualified-export
  namespace (REQ-CB-016, pattern syntax settled as regex per Q5),
  per-method `DependencyNetwork`s for get/put argument flow
  (REQ-CB-018), and the invoke-once-after-all-args-ready discipline
  (REQ-CB-020). Depends on 4a (binds to a `MethodGraphNode`, REQ-CB-019)
  and 4c.
- **4e. Horizontal geometry as GraphStateItem** (`13` §13.1–13.2) —
  geometry carried as an incomplete `esmf_field` proxy
  (`ESMF_FIELDSTATUS_GRIDSET`), resolved through ordinary
  advertise/connect/transform-if-needed rules with no special-case code
  paths (REQ-GEO-001..003). **Explicit deferral, to be stated in this
  sub-change's own proposal.md:** REQ-GEO-002a (exchange-component
  geometry, e.g. `SURF`-style multi-source `XGrid`) and all of §13.4
  (time-dependent geometry renewal under freeze) are out of scope —
  static geometry only. Depends on Phase 1–3 only.
- **4f. VerticalGrid model** (`13` §13.3) — `VerticalGrid` as an
  `esmf_state`-kind `GraphStateItem` with `variant() ==
  MAPL_STATEITEM_VERTICALGRID` (REQ-GEO-009), physical-dimension-keyed
  coordinate sets (REQ-GEO-004/004a), the dimension-adaptability check
  for mismatched vertical grids (REQ-GEO-007a), and the
  `ReferenceCharacteristic` link back to horizontal geometry. Depends on
  4e.
- **4g. RouteHandleValue/Key** (`14`) — `RouteHandleKey` structure
  (REQ-RH-002/003), the `RouteHandleKey -> NodeId` semantic index for
  reuse (REQ-RH-004/005). **Explicit deferral, to be stated in this
  sub-change's own proposal.md:** §14.4 time-dependent renewal is out of
  scope — reuse-of-existing-handle case only. Depends on 4e (needs
  geometry identity to populate `RouteHandleKey`).

**Repo/tooling note (extends §20.4.1's own note).** Phase 4 code lives
in the MAPL repo/checkout, same as Phase 3, for the same reason: real
`GriddedComponentDriver`/`OuterComponent`/ESMF context is required from
4b onward regardless of repo layout.

## 20.5 Cross-reference

This roadmap does not restate or supersede any requirement in `01`–`19`;
it only sequences them. If a phase boundary above turns out to be wrong
once implementation starts (e.g. a Phase 4 item turns out to be needed
earlier), update this document rather than letting the plan silently
diverge from practice — same discipline as the rest of this
specification.
