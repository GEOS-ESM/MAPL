## Why

`docs/graph/spec/20-implementation-roadmap.md` §20.4.2 names Phase 6
("legacy retirement") an entry criterion — "do not retire
`StateRegistry`/`ExtensionFamily`/the aspect system until every real
production code path... has a graph-native replacement that has been
*validated*, not merely built" — and a growable list of small follow-ups,
but it names no sub-changes, no ordering, and (confirmed by direct code
reading for this proposal) is missing several prerequisites that were
never written down anywhere in Phases 1–11, including this document
itself before now:

- **A hard, previously-named-but-never-filed dependency on 3b2**
  (cross-component unresolved-import propagation, `§20.4.1`). Every
  `Test_Scenarios` fixture of the "propagated" shape (`statistics`,
  `statistics_real`, `history_1`, `history_wildcard`, `extdata_1` — the
  connected item lives on a grandchild, surfaced only through legacy's
  own `StateRegistry_Propagation_smod.F90` `propagate_exports`/
  `propagate_unsatisfied_imports`) remains unreachable by `GraphBuilder`
  until 3b2 lands. 3b2 is named as an owed, committed follow-up in at
  least fourteen separate archived `design.md`/`proposal.md` files
  (`graphbuilder-advertising-connections` through
  `phase-11b-chain-intermediate-materialization`) spanning 2026-09-16
  through 2026-10-12, and has never once been filed as its own
  `openspec` change (`find openspec/changes/archive -iname "*propagat*"
  -o -iname "*3b2*"` → zero matches). Phase 6's own entry criterion
  cannot be met for any propagated-shape production path without it.
- **A hard, previously-unstated dependency on Phase 7** (`ClassAspect`
  shape dispatch, §20.4.5). `VariableSpec.F90:568,590,813`
  (`make_StateItemSpec`/`make_aspects`/`make_ClassAspect`) all require a
  live `StateRegistry` pointer as a construction-time argument, not
  merely a connection-time convenience — confirmed by
  `ExpressionClassAspect.F90:353`
  (`new_extension => src%registry%extend(v_pt, goal_spec, _RC)`), which
  reaches outside its own two-item scope into the registry's extension
  machinery at construction time. `VariableSpec` itself — the
  construction entry point for every item, not only mismatched ones —
  cannot become registry-independent until Phase 7's replacement for
  Bracket/VectorBracket/Expression dispatch exists.
- **5a2's seven additional `StateItemCharacteristic` subclasses are
  built but never wired into real dispatch.** `AttributesCharacteristic`/
  `UngriddedDimsCharacteristic`/`QuantityTypeCharacteristic`/
  `ConservationCharacteristic`/`NormalizationCharacteristic`/
  `StandardNameCharacteristic`/the vertical-grid `ReferenceCharacteristic`
  analog (landed, `openspec/changes/archive/
  2026-10-08-state-item-characteristic-subclasses`) are, by that
  change's own explicit scope, "additive, standalone types exercised by
  synthetic-node pFUnit coverage only" — never inserted by
  `GraphBuilder.F90`'s real `build_characteristics()`. A real production
  mismatch on any of those seven axes is invisible to `resolve_one`'s
  mismatch detection today, and no change has ever been filed to close
  this gap.
- **The legacy coupler-GridComp phase-invocation mechanism has no named
  graph-native disposition.** `OuterMetaComponent/run_user.F90:48,72`
  and `initialize_user.F90:29` call `registry%get_import_couplers()`/
  `get_export_couplers()` to invoke
  `MAPL_GENERIC_COUPLER_UPDATE`/`_INVALIDATE`/`_INITIALIZE` phases on
  auto-generated coupler `ESMF_GridComp`s
  (`superstructure/generic/transforms/GenericCoupler.F90`,
  `CouplerMetaComponent.F90`) around every user-component phase. Nothing
  in Phases 1–11 states whether this is retired outright (subsumed by
  `graphbuilder_run_pull_hook`'s demand-driven pull,
  `GraphBuilder_RunPhasePull_smod.F90:44-79`, landed by
  `graph-child-state-population` D7) or needs its own graph-native
  replacement — the question has simply never been asked in writing.
- **Restart read/write has no graph-native materialization story.**
  `RestartHandler.F90`'s own field-selection predicates
  (`predicate_skip_restart_`/`predicate_incomplete_`,
  `RestartHandler.F90:189-204`) key off `RestartMode` field metadata set
  via `FieldInfoSetInternal` — set only by legacy
  `FieldClassAspect.F90:498` (`call FieldInfoSetInternal(info, alias_id,
  this%restart_mode, _RC)`). No graph-native field-materialization call
  site (`GraphBuilder.F90`'s `try_materialize_field_extension`/
  `materialize_field_extension`, Phase 10a's own state-materialization
  work) sets this metadata anywhere (confirmed:
  `grep -rn "RestartMode" superstructure/generic/graph/*.F90` → zero
  hits). A graph-materialized field is silently invisible to, or
  mishandled by, restart today, and no phase names this.

Phase 3 (§20.4.1), Phase 4 (§20.4.3), Phase 5 (§20.4.4), Phase 10
(§20.4.8), and Phase 11 (§20.4.10) each hit the same "too
heterogeneous/dependency-chained for one spec-driven change" problem
before implementation started and were each resolved with their own
sub-sequencing section before any sub-change was filed. Phase 6, as
currently recorded, has never received that treatment.

**Separately, user direction for this effort is explicit:** Service
(`ServiceClassAspect`, `SERVICE`/`SERVICE_PROVIDER`/`SERVICE_SUBSCRIBER`
itemTypes) is **not** to be migrated to a graph-native structural
analog — it is retired outright, replaced by callbacks
(`CallbackInterface`/`CallbackInterfaceRegistry`, Phase 4c/4d, already
landed). This narrows Phase 7c's own existing scope (§20.4.5, currently
"Wildcard/Service pattern-based multi-source fan-in" as one bundled
item) and removes Service from Phase 6a's own prerequisite list
entirely — it becomes an audit-and-delete item instead.

## What Changes

- Add a new **§20.4.11 Phase 6 sub-sequencing** section to
  `docs/graph/spec/20-implementation-roadmap.md`, in the same shape as
  the existing sub-sequencing sections (§20.4.1/§20.4.3/§20.4.4/§20.4.8/
  §20.4.10), replacing §20.4.2's undifferentiated "(a) retire once
  validated / (b) growable list" framing with:
  - An explicit, named **prerequisite set** that must land before
    Phase 6a's own retirement work may begin: Phase 3b2 (still unfiled,
    named in §20.4.1), Phase 7a (Bracket/VectorBracket 1:1), Phase 7b
    (Expression N:1), a **narrowed** Phase 7c (Wildcard pattern-based
    fan-in only — Service removed, see below), the new Phase 12 (5a2
    dispatch wiring, named below), and the new Phase 13 (restart
    compatibility, named below).
  - An explicit **scope narrowing of Phase 7c and disposition of
    Service**: Phase 7c's own future `openspec` change scopes to
    Wildcard only. Service's own `ClassAspect`/`AspectId` retirement is
    reclassified as a Phase 6b growable-list item — an audit (confirm
    every real production `SERVICE`/`SERVICE_PROVIDER`/
    `SERVICE_SUBSCRIBER` declaration has a landed callback-based
    replacement) followed by deletion, not a structural graph-native
    rebuild.
  - Three ordered sub-steps replacing Phase 6a's own single sentence:
    **6a-1** (large-scale `GRAPH_MODE_BOTH` parity run across the full
    `Test_Scenarios`/`graph-scenarios` fixture sets — not only Phase
    10b/10c's own narrow flat fixtures — confirming agreement before
    any default-mode flip is considered); **6a-2** (flip the default
    mode / remove the `registry`/`both` toggle options once 6a-1 is
    clean); **6a-3** (delete `StateRegistry`/`*Aspect*`/`AspectId`/
    `ExtensionFamily`/the `Connection` family/the coupler mechanism
    source files).
  - New entries **appended** to Phase 6b's existing growable list (the
    existing three entries, §20.4.2 lines ~327–374, are left unmodified
    per the document's own "append, don't drop" discipline): removal of
    `write_formatted`'s dead debug-dump machinery;
    `ModelVerticalGrid%get_coordinate_field()`'s own second
    `registry%extend()` call site (`ModelVerticalGrid.F90:273`);
    `gridcomp_set_geometry`'s own direct single-item bypass of
    `StateRegistry%set_geometry` (`MAPL_Generic.F90:1152-1181`); the
    dead/disabled `propagate_exports`/`add_to_states` lines in
    `initialize_modify_advertised.F90:31,36`; the coupler-lifecycle call
    sites in `run_user.F90`/`initialize_user.F90` (confirm subsumed by
    `graphbuilder_run_pull_hook`'s demand-driven pull before deleting,
    not assumed); Service's own retirement (confirmed-migrated-to-
    callbacks, not reproduced structurally); and a pre-removal
    compatibility check on the public, in-tree-unused
    `MAPL_GridCompGetRegistry` symbol (`API.F90:42`) against a real
    out-of-tree GEOS checkout before deleting it.
- Add a new **§20.4.12 Phase 12 scope: wire 5a2's remaining
  `StateItemCharacteristic` subclasses into real dispatch** section
  (discovered gap, not previously tracked by any phase above) — same
  "scope" format as the existing Phase 8/9 sections (§20.4.6/§20.4.7),
  since this is a single, mechanically-scoped gap rather than a
  multi-concern phase needing its own internal sub-sequencing.
- Add a new **§20.4.13 Phase 13 scope: restart compatibility under
  graph-native field materialization** section (discovered gap, not
  previously tracked by any phase above), same "scope" format.
- Add a short scope-narrowing cross-reference note to the existing
  §20.4.5 (Phase 7c) text pointing at the new §20.4.11 for the
  Service-removal rationale — §20.4.5's own gap description is left
  otherwise unmodified.
- No source code changes in this change itself — this proposal only
  establishes where Phase 6 fits once its own real prerequisites are
  named, and names two new phases whose own `openspec` changes get
  filed separately once each is picked up.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

None — this proposal only extends a planning-record document
(`docs/graph/spec/20-implementation-roadmap.md`); it does not change any
requirement in an `openspec/specs/` capability. `.openspec.yaml` for this
change sets `skip_specs: true`, matching the
`phase-5-sub-sequencing`/`phase-10-sub-sequencing`/
`phase-11-sub-sequencing` precedent.

## Impact

- **Affected file:** `docs/graph/spec/20-implementation-roadmap.md` only
  (new `§20.4.11`, `§20.4.12`, `§20.4.13` sections; `§20.4.2`'s own
  existing growable list extended, not rewritten; `§20.4.5`'s own Phase
  7c text gets a one-paragraph scope-narrowing cross-reference, not a
  rewrite).
- **Downstream effect:** once this section exists, 3b2 (already named,
  never filed) has an explicit, documented reason to be filed next
  rather than remaining a dangling footnote; Phase 12 and Phase 13 can
  each be filed as their own `openspec` change; Phase 6a's own three
  sub-steps and Phase 6b's extended growable list give any future
  "start retiring `StateRegistry`" session a concrete,
  dependency-ordered checklist instead of a single under-specified
  entry criterion.
- **No code, build, or test impact.** This is a documentation-only
  planning change.
