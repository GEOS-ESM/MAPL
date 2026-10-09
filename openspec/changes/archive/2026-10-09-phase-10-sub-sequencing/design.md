## Context

See proposal.md - Why for the full gap inventory (unmaterialized ordinary
items, no graph-side `add_to_states` equivalent, `TypeKindCharacteristic`
not wired into real resolution, no clean two-characteristic chain
fixture, no real `MethodGraphNode` for an ordinary run phase, 3b2 still
unfiled). This document records how those gaps are split into ordered
sub-changes and what technical decisions each one is bound by before its
own `openspec` change is filed.

Three existing, already-landed mechanisms this phase deliberately reuses
rather than re-designs:

- `mapl_GraphMode_mod` (`superstructure/generic/graph/GraphMode.F90`): a
  single, global, default-off switch, already used by Phase 4e's
  geometry hook (`graph_native_enabled()`). 10a extends this switch's
  domain (what it selects), not its shape.
- `Test_GraphBuilderEquivalence.pf`
  (`superstructure/generic/tests/Test_GraphBuilderEquivalence.pf`): the
  only existing precedent for "run a real (DSO-backed) MAPL
  configuration through both legacy and graph, compare." It already
  inspects a graph-materialized `StateItemNode`'s payload directly
  (`node%get_payload()%get_field()`) rather than through the shared
  `ESMF_State` — the exact pattern 10a's "both" mode and 10b's harness
  generalize.
- `build_callback_method_binding`
  (`superstructure/generic/GraphBuilder.F90`): the only existing
  real-construction site for a `MethodGraphNode`
  (`declare_argument`/`bind_argument`/`register_node`, bound to a
  `GridCompDriverPhaseInvoker`). 10d reuses this exact pattern, pointed
  at a component's own advertised import/export ports instead of a
  `CallbackInterface`'s declared arguments.

## Goals / Non-Goals

**Goals:**
- Give Phase 6's own entry criterion ("every production path has a
  *validated*, not merely built, graph-native replacement") a concrete
  validation suite to point to.
- Produce an early, demonstrable "graph path runs a real MAPL
  configuration and produces the same answer" result, covering both a
  no-transform case and a genuine two-characteristic extension-chain
  case, including the chain's runtime re-invalidation behavior.
- Do so as a strictly additive validation layer: no change to legacy
  behavior when the toggle is in its default (`registry`) position.

**Non-Goals:**
- Retiring any legacy code path (`StateRegistry`/`ExtensionFamily`/
  `ClassAspect`) — that remains §20.4.2's own job, gated on this phase's
  completion, not performed by it.
- Cross-component propagation (3b2) or pattern-based fan-in (existing
  §20.4.5 Phase 7c) — both are named as explicit prerequisites for
  specific fixture families this phase's sub-changes do NOT attempt to
  unblock. Fixtures of those shapes are out of scope for 10a-10d.
- General compiled execution (Q9/5c) — 10d's `MethodGraphNode`
  construction is narrowly scoped to invoking one ordinary run phase
  through the existing interpreted `update()`/`advance()` path; it is
  not a step toward compiled execution.
- `4b2`'s driver-map unification — evaluated and declined previously
  (§20.4.3); 10d does not depend on it and does not reopen that
  decision.

## Decisions

**Sub-change order is strict: 10a -> 10b -> 10c -> 10d.** Unlike Phase
5's three independent items, these four are a single dependency chain,
not parallel tracks — 10b's harness needs 10a's materialization to have
anything to compare; 10c's fixture needs 10b's harness to run it
through; 10d is new mechanism, deliberately sequenced last so the
lower-risk, gap-closing sub-changes (10a-10c, all closing holes in
already-landed Phase 3-5 code) land and are demonstrably working before
any new invocation machinery is built.

- **10a. State materialization + 3-mode toggle** — scope: (1) ordinary
  (exact-match, non-mismatched) `StateItemNode`s get a real
  `FieldCreate()`'d payload at `GENERIC_INIT_ACCEPT_TRANSFER` time,
  removing today's restriction of real materialization to the
  mismatch/extension branch only; (2) a new graph-side equivalent of
  `StateRegistry%add_to_states` — walks a component's `ComponentGraph`
  `StateItemNode`s and (in `graph` mode only, never in `both` mode)
  populates the real `ESMF_State`; (3) `mapl_GraphMode_mod` gains a
  3-way selector (`registry` / `graph` / `both`) in place of today's
  binary `graph_native_enabled()`, applied per the "no `ESMF_State`
  name collision" rule below. Depends only on landed Phase 1-3
  (`ComponentGraph`, `StateItemNode`, `GraphBuilder`'s existing
  `advertise_one`/`resolve_one`) — no dependency on 3c's extension
  machinery beyond what already exists.
- **10b. Generalized equivalence harness, "both" mode, flat fixtures**
  — scope: a parameterized pFUnit suite shaped like
  `Test_Scenarios.pf`'s own `ScenarioDescription`/testParameter
  machinery, but running each fixture through `graphbuilder_run_*_hook`
  in `both` mode and asserting legacy's real `ESMF_State` and graph's
  shadow `StateItemNode` payload agree (existence, typekind, units,
  geom, shape; value once a run phase has executed). New, clean
  fixtures — not a reuse of existing `scenario_1`/`scenario_2`-style
  naming (per proposal.md's organic-naming note) — each covering one
  orthogonal no-transform capability (plain field, bundle/vector field,
  3D field, ungridded dims). Depends on 10a.
- **10c. `TypeKindCharacteristic` wired into real resolution + a real
  precision-cast transform** — scope: (1) `GraphBuilder.F90`'s
  `build_characteristics()` gains a `TYPE_KIND_CHARACTERISTIC_ID` entry
  whenever a `VariableSpec%typekind` is declared, mirroring the
  existing `units`/`vertical_grid` entries; (2) `CopyTransform`
  (`18-state-item-characteristics.md`'s own placeholder name, flagged
  there as poor and in need of a better one before implementation —
  this sub-change's own design.md picks the real name) gets a genuine
  R4<->R8 cast implementation, mirroring `UnitsConverterTransform`'s
  shape (`FieldPointerUtilities`-based, no legacy coupling); (3) one
  new, clean two-characteristic fixture — a single edge whose export
  and import mismatch on BOTH `units` and `typekind` simultaneously
  (e.g. a `hPa`/`R4` producer feeding a `Pa`/`R8` consumer), with
  hand-checkable values, replacing reliance on the existing
  `invalidate/` fixture's cryptic `A`/`B`/`T`/`T(1)`/`T(2)` naming for
  this specific capability. `build_chain`'s own loop
  (`ExtensionResolution.F90`) already walks an arbitrary-length
  `mismatched_kinds(:)` array generically — no chain-building code
  changes are needed, confirmed by reading `build_chain` directly; this
  sub-change only supplies the second characteristic's own detection
  and transform. Depends on 10a/10b (uses the same harness, extended to
  assert the chain's intermediate materialized values, not just the
  final one).
- **10d. Ordinary-phase `MethodGraphNode` construction + real run-loop
  wiring** — scope: (1) `GraphBuilder` constructs exactly one
  `MethodGraphNode` per component, wrapping that component's own
  `GENERIC_RUN_USER` phase through the already-real, already-landed
  `GridCompDriverPhaseInvoker` (Phase 4b) — no new invoker/adapter
  type, only a new construction call-site following
  `build_callback_method_binding`'s own established
  `declare_argument`/`bind_argument`/`register_node` pattern, keyed to
  the component's own advertised ports instead of a
  `CallbackInterface`'s declared arguments; (2) a new call site —
  likely `OuterMetaComponent`'s own `run_user.F90`, mirroring how
  `gb%run_connect_hook` already sits alongside legacy's `connect()` at
  `ACCEPT_TRANSFER` — invokes this `MethodGraphNode` (triggering
  `update()`/`advance()` per REQ-MTH-003a's existing discipline)
  alongside legacy's own run call, respecting the same 3-mode toggle
  10a introduced. Extends 10c's own fixture (reused, not a new one) to
  additionally exercise the chain re-propagating correctly when the
  producer's exported value changes across a second timestep. Depends
  on 10a-10c; depends on Phase 4a/4b's already-landed
  `MethodGraphNode`/`GridCompDriverPhaseInvoker`/`DriverResolver` and
  Phase 2's `NodeRevision`/`ComponentGraph%update()` — no new
  dependency beyond what Phase 1-4 already provide.

**Three-mode toggle semantics: no `ESMF_State` name collision by
construction.** `both` mode never inserts graph-materialized data into
the shared `ESMF_State` that `check()`-style assertions (legacy
`Test_Scenarios.pf`, 10b's own harness) observe — it is compared
out-of-band, reading the `StateItemNode`'s own payload directly
(`node%get_payload()%get_field()`), exactly as
`Test_GraphBuilderEquivalence.pf` already does today for its two
existing tests. `registry` mode (today's default) and `graph` mode
(true replacement — legacy's own `add_to_states`/`process_connections`
skipped entirely for that component) are mutually exclusive by
construction: only one producer ever inserts a given name into one
`ESMF_State`. `graph` mode is not attempted by any 10a-10d sub-change —
it remains available as a later mode once `both` mode has run clean
across the fixture set built here, matching this phase's own
"demonstrate before replace" posture.

**10c's fixture replaces, not extends, `invalidate/`.** `invalidate/`
already exercises a genuine two-characteristic (`units`+`typekind`)
chain, confirmed by reading its own `description.md` ("we use units and
precision change as the two couplers as their consequences are easy to
compute directly") — but it bundles that with `numsteps: 2`/`run:`
runtime re-invalidation semantics and non-obvious `T(1)`/`T(2)` naming
in the same fixture, discovered during this phase's own planning rather
than designed in from the start. 10c's own new fixture isolates the
structural (chain exists, correct values once) half with clear naming;
10d's own extension of that same new fixture — not a second new fixture
— adds the runtime-re-invalidation half once 10d's own mechanism exists,
deliberately mirroring what `invalidate/` already combined, but built
up in the two separately-landable pieces this phase's own sub-sequencing
requires.

## Risks / Trade-offs

- **[Risk] `both` mode doubles materialization cost for every fixture
  it runs, permanently, for as long as it exists** → **Mitigation**:
  scoped to this validation suite's own test binaries, not real
  production initialization; `graph` mode (the eventual zero-overhead
  replacement) is explicitly deferred past this phase, not attempted
  here.
- **[Risk] 10d's new `MethodGraphNode` construction could diverge from
  `build_callback_method_binding`'s own established pattern in a subtle
  way (e.g. argument-kind constraints, port-binding order) since it is
  a second, independent call site doing a structurally similar job** →
  **Mitigation**: 10d's own design.md (filed separately, when 10d is
  reached) must explicitly diff its construction sequence against
  `build_callback_method_binding`'s, not merely gesture at similarity.
- **[Risk] Discovering a cross-component (3b2-shaped) or pattern-based
  (Phase 7c-shaped) gap mid-implementation of 10a-10d, the way 3b's own
  real-configuration validation surfaced 3b2 unplanned** →
  **Mitigation**: 10a-10d's own fixture set is deliberately restricted
  to the flat (non-propagated, non-pattern-based) shape throughout —
  every new fixture this phase introduces is checked against that
  restriction before being added, not discovered to violate it after
  the fact.
- **[Risk] `CopyTransform`'s real name (10c) collides with another
  module in the same global Fortran module-name namespace, the way
  `UnitsConverterTransform`/`ConvertUnitsTransform` already collide
  (§20.4.2's own growable list, first entry)** → **Mitigation**: 10c's
  own design.md must check the existing `superstructure/generic/
  transforms/` and `superstructure/generic/graph/` directories for a
  name collision before picking `CopyTransform`'s real name, following
  the same precedent `UnitsConverterTransform`'s own naming decision
  already established.

## Open Questions

- Exact fixture directory naming convention for 10b/10c's new,
  "clean-name" fixtures (e.g. `scenarios/graph_demo_*` vs. a separate
  top-level directory) — does not affect scope, approach, or
  sub-change boundaries; left for 10a/10b's own task breakdown to
  decide, following whichever convention keeps them clearly
  distinguishable from the legacy-oracle-only fixture set without
  requiring a wholesale rename of existing fixtures.
