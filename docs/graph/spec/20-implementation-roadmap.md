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
  (CallbackInterface/registry/Handler/Invoker), `14` route handles
  (RouteHandleValue/Key, sharing/renewal), `13` geometry and vertical
  grids (time-dependent geometry, renewal under freeze).
- **Phase 5 (speculative/deferred, do not block on these)** — `18`
  StateItemCharacteristic hierarchy, `16` ordinary inout items, Q9
  compiled-execution optimization.

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
  match/no-op case only, no mismatch/extension handling yet.
- **3c. Extension reuse** (`09`, REQ-EXT-001..005) — Transform-chain
  creation for mismatched export/import pairs, extension-family search
  as a `DependencyNetwork` traversal, `StateRegistry` registration of
  extension items. Depends on 3b's wiring and Phase 2's
  `TransformGraphNode`/`Transform`. REQ-EXT-002a stays `[SPECULATIVE]`/
  `[OPEN]`, not resolved by this sub-change.
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

## 20.5 Cross-reference

This roadmap does not restate or supersede any requirement in `01`–`19`;
it only sequences them. If a phase boundary above turns out to be wrong
once implementation starts (e.g. a Phase 4 item turns out to be needed
earlier), update this document rather than letting the plan silently
diverge from practice — same discipline as the rest of this
specification.
