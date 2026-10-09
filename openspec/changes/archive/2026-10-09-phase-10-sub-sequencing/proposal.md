## Why

`docs/graph/spec/20-implementation-roadmap.md` §20.4.2 ("Phase 6: legacy
retirement") has an explicit entry criterion: do not retire
`StateRegistry`/`ExtensionFamily`/the aspect system until every real
production code path that depends on them "has a graph-native
replacement that has been validated (not merely built) as the default
behavior." No such validation exists today. Direct inspection of the
landed Phase 3/4/5 code
(`superstructure/generic/GraphBuilder.F90`,
`superstructure/generic/graph/ExtensionMaterialization.F90`,
`superstructure/generic/graph/ExtensionResolution.F90`) shows the gap is
not cosmetic:

- Ordinary (exact-match) advertised items are never materialized on the
  graph side. `advertise_one()` leaves a `StateItemNode`'s payload
  default/unallocated; `resolve_one()`'s exact-match branch only calls
  `add_dependency()` — real `ESMF_Field` creation for an ordinary item
  still happens entirely through legacy's `Connection%connect()` ->
  `StateItemSpec%allocate()`. `materialize_field_extension()` (real
  `FieldCreate`) exists but fires only on the mismatch/extension branch,
  gated off by default (`materialize_extensions_enabled()`).
- Nothing populates a real `ESMF_State` from graph-side `StateItemNode`s.
  `superstructure/generic/tests/Test_Scenarios.pf` — the existing
  end-to-end regression suite exercising ~30 real YAML-configured MAPL
  hierarchies, and the project's own stated rationale for believing a
  graph-native path is realistic client-code coverage — only ever
  inspects the `ESMF_State` tree that legacy's own
  `StateRegistry%add_to_states` (`registry/StateRegistry_Actions_smod.F90`)
  populates. No graph-native equivalent of `add_to_states` exists.
- `TypeKindCharacteristic` (Phase 5a, `graph/TypeKindCharacteristic.F90`)
  is never inserted by `GraphBuilder.F90`'s own
  `build_characteristics()` — confirmed by direct search, zero
  references outside its own file and Phase 5a's synthetic pFUnit
  coverage. A real precision (typekind-only) mismatch is therefore
  invisible to `resolve_one`'s mismatch detection today; it would be
  silently treated as a no-op exact match.
- No fixture in `superstructure/generic/tests/scenarios/` exercises a
  genuine two-characteristic extension chain on a single edge except
  `invalidate/` — which bundles that case with harder runtime
  re-invalidation semantics and non-obvious `A`/`B`/`T`/`T(1)`/`T(2)`
  naming, and is not a clean demonstration fixture on its own.
- Nothing in the codebase constructs a real `MethodGraphNode` for an
  ordinary component's own `GENERIC_RUN_USER` phase. The only real
  `MethodGraphNode` construction site today
  (`GraphBuilder.F90`'s `build_callback_method_binding`) is the
  deliberately narrower callback-wiring path. Phase 4b's own
  `proposal.md` named this explicitly as an "Explicit deferral" ("out of
  scope unless required to exercise this change's driver-resolution/
  trigger-advance code against something real") — it was not required
  there, so it was never built. Without it, graph-side `NodeRevision`
  bookkeeping is never exercised against a real run loop; it is only
  invoked directly by pFUnit tests on synthetic graphs.

Separately, `graphbuilder-advertising-connections`'s own design.md
(archived) already named a required, committed follow-up — "3b2,
cross-component unresolved-import propagation" — slotted between
sub-changes 3b and 3c in §20.4.1. No `openspec` change implementing it
was ever filed; `graphbuilder_check_unsatisfied_imports()`'s own
activate-time result is still only logged and discarded today. Every
scenario fixture of the "propagated" shape (`statistics`,
`statistics_real`, `history_1`, `history_wildcard`, `extdata_1` — where
the real connected item lives on a grandchild, surfaced through
`StateRegistry`'s `propagate_exports`/`propagate_unsatisfied_imports`)
remains unreachable by `GraphBuilder` until 3b2 lands, exactly as that
proposal's own "Also out of scope, discovered during implementation"
section already recorded.

Phase 3 (§20.4.1), Phase 4 (§20.4.3), and Phase 5 (§20.4.4) each hit the
same "too heterogeneous for one spec-driven change" problem and were
resolved by adding an explicit sub-sequencing section to the roadmap
before implementation started. This proposal does the same for a newly
identified, forward-only phase: a validation suite that exercises the
graph-native path end-to-end against real YAML-configured MAPL
hierarchies — the `Test_Scenarios`-shaped evidence that was previously
only cited after the fact (Phase 4e/4f/4g's own "`Test_Scenarios`
evidence" framing, §20.4.5's own per-mechanism fixture counts) — built
deliberately, as its own phase, rather than continuing to be discovered
piecemeal.

This phase is **not** a renumbering of the existing §20.4.2 "Phase 6"
(legacy retirement). It is a prerequisite for Phase 6's own entry
criterion, discovered after Phase 6 was already named — following this
roadmap's own established precedent (Phases 7/8/9) of inserting a
newly-discovered phase at the next free forward number rather than
reordering existing phase numbers to match dependency order.

## What Changes

- Add a new `§20.4.8 Phase 10 sub-sequencing` section to
  `docs/graph/spec/20-implementation-roadmap.md`, following the same
  shape as §20.4.1/§20.4.3/§20.4.4: ordered sub-changes, each with its
  own scope boundary, dependencies, and explicit cross-reference to
  Phase 6's entry criterion.
- Name four ordered sub-changes (10a-10d), their scope, their
  dependencies on each other and on landed Phase 1-5 work, and an
  explicit note that 10d (new mechanism: real `MethodGraphNode`
  construction for an ordinary run phase) is sequenced strictly after
  10a-10c (closing gaps in already-landed code; no new mechanism).
- Record the three-mode state-materialization toggle design
  (`registry` / `graph` / `both`) as this phase's own answer to "how do
  we get a safety net without import-state name collisions" —
  `both` mode never inserts graph-materialized data into the shared
  `ESMF_State`; it is compared out-of-band against a `StateItemNode`'s
  own payload, mirroring `Test_GraphBuilderEquivalence.pf`'s existing,
  already-landed inspection pattern.
- Record the still-unfiled 3b2 (cross-component unresolved-import
  propagation) as the explicit prerequisite for the "propagated-shape"
  fixture family (`statistics`/`history_1`/`extdata_1`/...), and the
  existing §20.4.5 Phase 7c (wildcard/service pattern-based fan-in) as
  the explicit, already-recorded prerequisite for the
  pattern-based fixture family — neither reopened or redesigned here,
  only cross-referenced as out-of-scope-for-now dependencies of the new
  phase's own fixture set.
- No source code changes, no `openspec/specs/` capability requirement
  changes in this change itself — this proposal only establishes the
  sub-sequencing plan that 10a (a separate, future `openspec` change)
  will implement first.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

None — this proposal only extends a planning-record document
(`docs/graph/spec/20-implementation-roadmap.md`); it does not change any
requirement in an `openspec/specs/` capability. `.openspec.yaml` for
this change sets `skip_specs: true` accordingly, matching the existing
`phase-5-sub-sequencing` precedent.

## Impact

- **Affected file:** `docs/graph/spec/20-implementation-roadmap.md`
  only (new `§20.4.8` section; no other section's content changes).
- **Downstream effect:** once this section exists, sub-change 10a
  (state materialization + 3-mode toggle) can be filed as its own
  `openspec` change — the first implementation step, scoped
  independently so it can be worked in a separate session/context from
  this planning change.
- **No code, build, or test impact.** This is a documentation-only
  planning change.
