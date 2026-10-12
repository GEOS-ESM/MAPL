## Context

See proposal.md - Why for the full gap inventory and citations. This
document records exactly what text goes into the three new roadmap
sections (`§20.4.11`, `§20.4.12`, `§20.4.13`) and the two edits to
existing sections (`§20.4.2`'s growable list, `§20.4.5`'s Phase 7c
text), before `docs/graph/spec/20-implementation-roadmap.md` itself is
edited.

Confirmed by direct code reading (summarized here; full citations in
proposal.md - Why):

- 3b2 (cross-component unresolved-import propagation) is named as an
  owed, committed follow-up in at least fourteen archived
  `design.md`/`proposal.md` files from 2026-09-16 through 2026-10-12,
  and has never been filed as its own `openspec` change. It blocks
  every `Test_Scenarios` fixture of the "propagated" shape.
- `VariableSpec` construction (`VariableSpec.F90:568,590,813`) takes a
  live `StateRegistry` pointer, not merely as a connection-time
  convenience — `ExpressionClassAspect.F90:353` calls
  `registry%extend()` from inside construction. Phase 7 (Bracket/
  VectorBracket/Expression dispatch) must land before `VariableSpec`
  itself can become registry-independent, which Phase 6a's own
  retirement step requires.
- 5a2's seven additional `StateItemCharacteristic` subclasses
  (Attributes/UngriddedDims/QuantityType/Conservation/Normalization/
  StandardName/vertical-grid-reference) are landed but, by that
  change's own explicit scope, never inserted by `GraphBuilder.F90`'s
  real `build_characteristics()` — synthetic-node-only today.
- `get_import_couplers`/`get_export_couplers`
  (`run_user.F90:48,72`, `initialize_user.F90:29`) invoke
  auto-generated coupler `ESMF_GridComp`s' own
  `UPDATE`/`INVALIDATE`/`INITIALIZE` phases around every user-component
  phase. No phase in this document states whether
  `graphbuilder_run_pull_hook` (`GraphBuilder_RunPhasePull_smod.F90:44-79`,
  landed) already subsumes this for the graph-native path, or whether
  it needs its own replacement.
- `RestartHandler.F90`'s own field-selection predicates key off
  `RestartMode` field metadata that only legacy
  `FieldClassAspect.F90:498` ever sets. No graph-native materialization
  call site sets it (`grep -rn "RestartMode"
  superstructure/generic/graph/*.F90` → zero hits).
- Service (`ServiceClassAspect`, three `SERVICE*` itemTypes) is
  explicit user direction: retire outright, replaced by callbacks — not
  migrate to a graph-native structural analog the way Wildcard/
  Bracket/VectorBracket/Expression are planned to.

Two existing precedent shapes this document reuses rather than
re-designs:

- The "sub-sequencing" section format itself
  (§20.4.1/§20.4.3/§20.4.4/§20.4.8/§20.4.10): ordered or
  explicitly-parallel-safe sub-items, each with its own scope boundary,
  dependencies, and cross-reference back to the gap(s) it closes.
- The "discovered gap, scope-only" section format
  (§20.4.6 Phase 8, §20.4.7 Phase 9): used here for Phase 12/13 since
  each is a single, mechanically-bounded gap, not a multi-concern phase
  needing its own internal ordering.

## Goals / Non-Goals

**Goals:**
- Give Phase 6, as currently recorded (§20.4.2's single undifferentiated
  entry criterion plus a three-item growable list), the same
  ordered-prerequisite treatment every other multi-concern phase in this
  document already received before its own implementation started.
- Name, for the first time in writing anywhere in this document, every
  prerequisite phase/gap that must close before Phase 6a's own
  retirement work can begin — not just restate §20.4.2's existing
  "validated, not merely built" language.
- Record the explicit, user-directed scope narrowing of Phase 7c
  (Service removed, Wildcard-only) and Service's own reclassification
  as a Phase 6b audit-and-delete item rather than a Phase 7c
  structural-analog item.
- Promote two newly-discovered, single-concern gaps (5a2 dispatch
  wiring; restart compatibility) to their own named phases, matching
  this document's own Phase 8/9 precedent for a gap too small to be a
  sub-sequencing section but too real to leave unnamed.

**Non-Goals:**
- Implementing any of Phase 6/7/12/13's own work. This proposal only
  records the plan; 3b2, 7a/7b/7c, 12, 13, and 6a/6b each get filed as
  their own, separate `openspec` change when picked up.
- Redesigning Phase 7's own existing scope boundary for 7a/7b
  (Bracket/VectorBracket/Expression) — unchanged by this document.
- Deciding Phase 6a-1's own fixture list, pass/fail criteria, or
  timeline — left to that sub-change's own future design.md once Phase
  6's prerequisites (3b2, 7a/7b/7c, 12, 13) have actually landed and the
  real remaining fixture gap is known.
- Reopening Phase 10d's own double-invocation hazard — unaffected;
  Phase 6a-2's own "flip the default mode" step is explicitly gated on
  that hazard already being resolved by whichever future change
  resolves it (not assumed solved here).

## Decisions

### §20.4.11 Phase 6 sub-sequencing — content

**Prerequisite set, named explicitly for the first time:** Phase 6a
(the actual `StateRegistry` retirement work) MUST NOT be filed as an
`openspec` change until all of the following have landed:

1. **3b2** (cross-component unresolved-import propagation, named in
   §20.4.1, never filed) — without it, every "propagated" fixture
   (`statistics`, `statistics_real`, `history_1`, `history_wildcard`,
   `extdata_1`) stays permanently out of reach for any
   `GRAPH_MODE_BOTH` parity run, making Phase 6a-1's own full-fixture
   pass impossible to attempt honestly.
2. **Phase 7a** (Bracket/VectorBracket 1:1 shape dispatch) and
   **Phase 7b** (Expression N:1) — `VariableSpec` construction itself
   depends on `StateRegistry` through exactly this dispatch path
   (`ExpressionClassAspect.F90:353`); `StateRegistry` cannot be deleted
   while any construction path still calls into it structurally,
   independent of whether a given real configuration happens to use
   Bracket/Expression.
3. **Phase 7c, narrowed to Wildcard only** — see below. Needed for the
   same structural reason as 7a/7b: `WildcardClassAspect` is still a
   live `ClassAspect` subclass dispatched from `VariableSpec`
   construction until replaced.
4. **Phase 12** (new, §20.4.12 below) — 5a2's seven subclasses wired
   into real dispatch. Without this, a real production mismatch on any
   of those seven axes is invisible to graph-native resolution, so
   Phase 6a-1's parity run cannot claim coverage of legacy's own
   mismatch-detection surface, only three of its ten axes.
5. **Phase 13** (new, §20.4.13 below) — restart compatibility. Without
   this, flipping any component's materialization to graph-native
   (Phase 6a-2) silently breaks restart for that component, with no
   test in today's suite that would catch it (`RestartMode` metadata is
   simply absent, not wrong, so no assertion currently fails on it).

**Phase 6a, split into three ordered sub-steps** (replacing §20.4.2's
own single-sentence "(a) retire... once validated" with the sub-steps
that sentence always presupposed but never named):

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
  §20.4.2 already named, made concrete.
- **6a-2. Flip the default mode; retire the toggle.** Once 6a-1 is
  clean, change `mapl_GraphMode_mod`'s own default from
  `GRAPH_MODE_REGISTRY`/`GRAPH_MODE_BOTH` (whichever is current) to
  `GRAPH_MODE_GRAPH`, and begin removing the three-way selector itself
  (collapsing to graph-native-only). Explicitly gated on Phase 10d's
  own double-invocation hazard (§20.4.8's "Explicitly out of scope")
  being resolved by whichever future change resolves it — not assumed
  solved by this document.
- **6a-3. Delete legacy source.** `superstructure/generic/registry/`
  (`StateRegistry*.F90`), `superstructure/generic/specs/*Aspect*.F90`
  and `AspectId.F90`, `ExtensionFamily` and its own supporting types,
  `superstructure/generic/connection/*.F90`
  (`Connection`/`SimpleConnection`/`MatchConnection`/
  `ReexportConnection`), and the coupler-GridComp mechanism
  (`superstructure/generic/transforms/GenericCoupler.F90`,
  `CouplerMetaComponent.F90`, `superstructure/generic/couplers/`) —
  once 6a-2's own flip has run clean in production for whatever burn-in
  period that sub-change's own design.md decides.

**Phase 7c scope narrowing — Service removed.** §20.4.5's own existing
text bundles Wildcard and Service together as "Wildcard/Service
pattern-based multi-source fan-in." Per explicit user direction for
this effort: Service is retired outright, replaced by callbacks
(`CallbackInterface`/`CallbackInterfaceRegistry`, Phase 4c/4d, already
landed) — it does NOT get a graph-native structural analog the way
Wildcard does. §20.4.11 records this narrowing; §20.4.5's own text gets
a one-paragraph cross-reference (not a rewrite) pointing here.

**Phase 6b's growable list — new entries appended** (existing three
entries, lines ~327-374, left verbatim, per the document's own
"append, don't drop" discipline):

- `write_formatted`'s dead debug-dump machinery
  (`StateRegistry.F90:85-86,120-127`,
  `StateRegistry_Lifecycle_smod.F90:42-125`) — confirmed zero
  production callers; delete with the rest of 6a-3, no separate
  migration needed.
- `ModelVerticalGrid%get_coordinate_field()`'s own second,
  independent `registry%extend()` call site
  (`ModelVerticalGrid.F90:273`) — already known to this document as
  4f's own design rationale (why 4f took a different design path,
  roadmap lines 562-568) but never tracked as its own "second call site
  to retire" checklist item. Confirm no remaining caller needs this
  path's side effect before deleting it in 6a-3.
- `gridcomp_set_geometry`'s own direct single-item bypass of
  `StateRegistry%set_geometry` (`MAPL_Generic.F90:1152-1181`) — a
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
  writing — `grep` for `itemType:.*SERVICE` across all `*.yaml`/`*.yml`
  outside `tests/` returned zero matches), then delete
  `ServiceClassAspect.F90` and the three `SERVICE*` itemType values —
  an audit-and-delete item, not a structural rebuild, unlike Wildcard.
- Pre-removal compatibility check on `MAPL_GridCompGetRegistry`
  (`API.F90:42`) — public, exported, zero in-tree callers outside a
  test fixture (`superstructure/generic/tests/gridcomps/
  ProtoExtDataGC.F90:10,85,146`), but a real external-compatibility
  risk if any out-of-tree GEOS component calls it. Check against a real
  GEOSgcm checkout before deleting the symbol in 6a-3.

### §20.4.12 Phase 12 scope — content

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
mirroring how `UnitsCharacteristic`/`VerticalGridCharacteristic`
already are — so a real production mismatch on any of these seven axes
becomes visible to `resolve_one`'s existing mismatch detection and
`ExtensionResolution.F90`'s existing `build_chain` loop (confirmed
generic over an arbitrary-length `mismatched_kinds(:)` array by 10c's
own prior confirmation — no chain-building code changes needed, only
detection). Real `build_transform` adaptation logic is explicitly NOT
required for every axis — 5a2's own design.md already noted most
legacy `*Aspect%make_transform` implementations either fail explicitly
or have no real adaptation logic today (only Units/Geom/Typekind have
one in legacy either). This phase's job is detection-wiring; a mismatch
with no adaptation available should surface as an explicit, reported
failure (matching legacy's own behavior for e.g.
`ConservationAspect%make_transform`'s unconditional `_FAIL`), not a
silent pass-through.

**Dependency:** Phase 5a/5a2 (landed) only. Independent of 3b2, Phase
7, and Phase 13 — this phase touches detection/dispatch wiring, not
propagation, shape-conversion, or restart.

### §20.4.13 Phase 13 scope — content

**Discovered gap, not previously tracked by any phase above.**
`RestartHandler.F90`'s own field-selection predicates
(`predicate_skip_restart_`/`predicate_incomplete_`,
`RestartHandler.F90:189-204`) key off `RestartMode` field metadata set
via `FieldInfoSetInternal` — set only by legacy
`FieldClassAspect.F90:498`. No graph-native field-materialization call
site (`try_materialize_field_extension`/`materialize_field_extension`,
`GraphBuilder.F90`) sets this metadata anywhere in the codebase today.

**Scope:** decide, as a planned up-front design decision (same
discipline this document already asks of other open points — Phase 4's
4b for REQ-MTH-011(c), Phase 5's 5a for `CharacteristicStatus`
naming), how a graph-materialized field's `RestartMode` is populated —
likely by threading `VariableSpec%restart_mode` (already a stored field,
`VariableSpec.F90:99`) through to whichever graph-native materialization
call site creates the real `ESMF_Field`, mirroring
`FieldClassAspect.F90:119-120`'s own construction-time assignment — and
confirm `RestartHandler.F90`'s own predicates behave correctly against a
graph-materialized field under `GRAPH_MODE_GRAPH`, not only under
`GRAPH_MODE_BOTH`/`GRAPH_MODE_REGISTRY` where legacy's own
materialization already sets this metadata regardless. A real read/write
round-trip test against at least one graph-materialized field is the
exit criterion, not merely "the field carries the metadata."

**Dependency:** Phase 10a (landed state-materialization call sites) only.
Independent of 3b2, Phase 7, and Phase 12 — this phase is specific to
restart I/O, not propagation, dispatch, or characteristic detection.

### §20.4.5 cross-reference edit

Append one paragraph to the existing Phase 7c bullet (§20.4.5, roadmap
lines ~902-928): record that Service is removed from this phase's own
scope per explicit user direction (link to §20.4.11), narrowing 7c to
Wildcard-only; leave the existing Wildcard/Service joint problem
description otherwise unmodified (it remains accurate as a description
of why the two were originally bundled, even though the disposition
decision has since diverged).

## Risks / Trade-offs

- **[Risk]** Naming 3b2/7a/7b/7c/12/13 as hard prerequisites for Phase
  6a could read as "Phase 6 is now blocked on six separate efforts,"
  discouraging anyone from picking up Phase 6 work at all →
  **Mitigation:** each prerequisite is already independently valuable
  and separately scoped (3b2 unblocks real scenario coverage generally,
  not only Phase 6; 7a/7b/7c close Phase 7's own already-named gap;
  Phase 12/13 are each small, single-concern phases). None of them are
  *new* work invented by this proposal — they already existed as named
  (3b2, 7a/7b/7c) or newly-discovered-but-small (12, 13) gaps; this
  document's only contribution is making the dependency explicit so
  Phase 6a is not attempted prematurely and then blocked mid-way.
- **[Risk]** Phase 6a-1's "full-fixture parity run" could discover
  additional gaps beyond what 3b2/7/12/13 close, the same way Phase 10's
  own 10a-10d sub-changes each discovered scope beyond their own
  original wording → **Mitigation:** 6a-1's own future design.md should
  explicitly budget for this (matching Phase 10/11's own precedent of
  recording scope corrections discovered during implementation rather
  than treating the original plan as final) rather than being surprised
  by it.
- **[Risk]** Removing Service without a structural analog could be
  read, by a future reader unfamiliar with this decision's own
  rationale, as an oversight rather than a deliberate choice →
  **Mitigation:** §20.4.11's own text states the user-directed removal
  explicitly, with the audit-before-delete step named in Phase 6b, so
  the decision and its own verification step are both on record.
- **[Risk]** Phase 13's restart-compatibility gap might turn out to be
  larger than "thread `restart_mode` through" if `RestartHandler.F90`
  has other registry-shaped assumptions not surfaced by this proposal's
  own research pass → **Mitigation:** Phase 13's own scope already
  requires a real read/write round-trip test as its exit criterion, not
  merely a metadata-presence check — any additional gap would surface
  there before the phase is considered closed, and can be recorded as a
  scope correction the same way Phase 4e/4f/10c each recorded one.

## Migration Plan

Not applicable — this is a planning-document change only. The migration
plan for Phase 6a-2's own default-mode flip (the actual production
migration this document is scoping toward) is explicitly deferred to
that sub-change's own future design.md, per this document's own
Non-Goals.

## Open Questions

None — this document's own job is to name the prerequisite set and
split Phase 6/12/13's scope; each named prerequisite's own
implementation-level decisions (6a-1's exact fixture list and pass bar;
6a-2's burn-in period before 6a-3; Phase 13's exact `RestartMode`
threading mechanism) are left to that sub-change's own future
design.md, matching this document's own established precedent
(§20.4.10's own "Open Questions: None" for the same reason).
