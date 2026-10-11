## Why

`docs/graph/spec/20-implementation-roadmap.md` §20.4.8 names 10b as the
next Phase 10 sub-change after 10a (landed,
`openspec/changes/archive/2026-10-09-phase-10a-state-materialization`):
a generalized `both`-mode equivalence harness over new flat fixtures,
replacing the current one-off pattern. Today, side-by-side verification
that graph-native materialization agrees with the legacy imperative
coupler exists only as two hand-written tests in
`Test_GraphBuilderEquivalence.pf` (`test_ordinary_resolution_matches_legacy_coupler`,
`test_both_mode_materializes_without_legacy_collision`), both built around
one fixture (`graphbuilder_equivalence/`, a single scalar `R4` field with
no vertical dimension). Every orthogonal no-transform capability
`Test_Scenarios.pf`'s own real fixtures already exercise for the legacy
path alone — 3D fields, ungridded dimensions — has no `both`-mode
equivalence coverage at all, and adding one by hand-writing a dedicated
pFUnit subroutine per fixture (as the two existing tests do) does not
scale to the fixture set 10c/10d still need. 10c (`TypeKindCharacteristic`
wiring + a real precision-cast transform) and 10d (ordinary-phase
`MethodGraphNode` construction) both depend on this harness existing
first — §20.4.8 names 10b as this phase's own second strictly-ordered
step.

## What Changes

- A new, parameterized pFUnit suite — shaped like `Test_Scenarios.pf`'s
  own `ScenarioDescription`/testParameter machinery, not a hand-written
  subroutine per fixture — that runs a named fixture through the full
  `GENERIC_INIT_PHASE_SEQUENCE` under `GRAPH_MODE_BOTH` and asserts, for
  every exact-match (non-mismatched) connection the fixture declares:
  (a) legacy's own real `ESMF_State` entries are byte-for-byte identical
  to what `GRAPH_MODE_REGISTRY` would produce (existence, no additional
  or duplicate entry), reusing 10a's own "no collision by construction"
  guarantee as the thing under test, not re-deriving it; and (b) the
  graph-native export node's own materialized payload (typekind, units,
  geom, shape — and value, once a run phase has executed) agrees with
  legacy's oracle value, read out of band via
  `node%get_payload()%get_field()`, exactly as the two existing
  hand-written tests already do.
- New, deliberately clean flat fixtures — each covering one orthogonal
  no-transform capability: plain scalar field, 3D field (vertical
  dimension present), ungridded dims. Named for the graph capability
  each one exercises (`plain_field`, `vertical_field`,
  `ungridded_dims`), not for the equivalence-check mechanism that
  currently drives them — matching legacy's own `Test_Scenarios.pf`
  fixture-naming precedent (`3d_specs`, `ungridded_dims`, `vector_1`:
  capability-named, not mechanism-named). This matters because the
  fixtures are durable (they describe graph behavior that must hold
  once `StateRegistry`/`registry` mode are gone, per roadmap §20.4.2's
  Phase 6) while the `both`-mode comparison mechanism itself is
  explicitly temporary scaffolding — Phase 6 retires the legacy oracle
  these checks compare against, at which point the equivalence-check
  machinery (not the fixtures) is expected to be deleted or repurposed
  into plain graph-mode-only fixtures. Every new fixture is flat (direct
  parent/two-children shape, mirroring `graphbuilder_equivalence/`'s own
  existing shape) — not nested — because `GraphBuilder` does not yet see
  connections resolved only through `StateRegistry`'s cross-level
  propagation (3b2, still unfiled; `Test_GraphBuilderEquivalence.pf`'s
  own header comment already documents this limitation for the existing
  fixture).
- **New fixture root, separate from `scenarios/`**:
  `superstructure/generic/tests/graph-scenarios/`, a new sibling
  directory to the existing `scenarios/` tree, not a subdirectory of it.
  `scenarios/` remains legacy's own ~30-fixture `Test_Scenarios.pf`
  regression set (registry-mode only) plus the two pre-existing
  `graphbuilder_equivalence*` fixtures, untouched by this sub-change;
  every fixture this sub-change adds is graph-native-equivalence-
  specific (requires `GRAPH_MODE_BOTH`, asserts graph-side payload
  parity) and lives in `graph-scenarios/` instead, so the two fixture
  families don't mix as the graph-native set grows across 10c/10d and
  beyond. The two pre-existing fixtures are left in place under
  `scenarios/` — not relocated by this sub-change, to avoid touching
  already-landed, already-passing test paths outside this sub-change's
  own scope.
- **Deviation from this sub-change's own roadmap wording, found before
  implementation starts (recorded here rather than silently dropped,
  same discipline this document's roadmap already asks for elsewhere,
  e.g. 4e's own "deviation... discovered during implementation" and 4f's
  "significant deviation... discovered during implementation" notes):**
  §20.4.8's own 10b bullet names a "bundle/vector field" fixture
  alongside the no-transform set above. 10a's own materialization
  (`try_materialize_field_extension`/`materialize_field_extension`) is
  explicitly Field-only (10a design.md Non-Goals: "Any change to
  composite-item materialization... or non-Field item classes — scope
  stays identical to `try_materialize_field_extension`'s own existing
  Field-only restriction"); no graph-native `FieldBundle`/`Vector`
  materialization exists yet to assert equivalence against. A
  bundle/vector fixture under this harness would have nothing real on
  the graph side to compare against legacy's own oracle. This
  sub-change's own fixture set is therefore Field-only (`plain_field`,
  `vertical_field`, `ungridded_dims`) — a bundle/vector equivalence
  fixture is deferred until FieldBundle/Vector materialization itself
  exists (no sub-change currently scopes that work; it is not assumed
  to be 10c's or 10d's job either).
- No change to `TypeKindCharacteristic` wiring, extension-chain
  construction, or run-loop `MethodGraphNode` construction — those
  remain 10c's and 10d's own scope (§20.4.8), sequenced after this
  sub-change. No change to 10a's own materialization or state-population
  code — this sub-change is test infrastructure and fixtures only.

## Capabilities

### New Capabilities
- `graph/equivalence-harness`: the parameterized, fixture-driven
  contract that `both`-mode graph-native materialization never
  diverges from the legacy imperative coupler's own real `ESMF_State`
  contents, and that the graph-native payload itself matches legacy's
  oracle value for every exact-match connection a fixture declares.
  **Named for, and scoped to, the comparison mechanism itself, not the
  fixtures it drives — explicitly temporary.** This capability's own
  contract depends on a legacy oracle (`registry` mode /
  `StateRegistry`) existing to compare against; roadmap §20.4.2's Phase
  6 retires that oracle once graph-native paths are the validated
  default, at which point this capability's requirements become
  vacuous and the capability itself is expected to be removed from
  `openspec/specs/` (not merely its implementation) rather than
  reinterpreted — the three fixtures it currently exercises
  (`plain_field`, `vertical_field`, `ungridded_dims`) are the durable
  artifact; this capability is not.

### Modified Capabilities
(none — this sub-change adds new test infrastructure and fixtures; it
exercises 10a's already-specified `graph/graph-mode` and
`graph/graph-builder` contracts, it does not change their requirements)

## Impact

- **Affected code (tests only, no production code changes):**
  `superstructure/generic/tests/Test_GraphBuilderEquivalence.pf` — new
  parameterized test case/testParameter type; three new fixture
  directories (`plain_field`, `vertical_field`, `ungridded_dims`) under
  a new `superstructure/generic/tests/graph-scenarios/` root;
  `superstructure/generic/tests/CMakeLists.txt` gains a second
  `copy_graph_scenarios`-style custom target (mirroring the existing
  `copy_scenarios` target) so the new directory is copied to the build
  tree alongside `scenarios/`.
- **No production code is touched.** This sub-change exercises existing
  10a materialization/population code and existing `GRAPH_MODE_BOTH`
  selector behavior; it does not add or change any `GraphBuilder.F90`,
  `GraphMode.F90`, or `ExtensionMaterialization.F90` logic.
- **Downstream effect:** unblocks 10c (`TypeKindCharacteristic` wiring +
  precision-cast transform, extended to assert the chain's intermediate
  materialized values via this same harness) and, transitively, 10d —
  both depend on this harness existing first per §20.4.8's strict
  ordering.
