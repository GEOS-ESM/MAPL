## Context

`try_materialize_field_extension()`/`materialize_field_extension()`
(landed, 10a) already materialize a real field on the export side of an
ordinary exact-match connection when the graph mode is `graph` or
`both`; `graphbuilder_populate_state()` (also 10a) walks each node's
predecessor edge to find the real producing field. Two hand-written
pFUnit subroutines in `Test_GraphBuilderEquivalence.pf` already exercise
this for exactly one fixture
(`scenarios/graphbuilder_equivalence/`): a single `R4` scalar field, no
vertical dimension, no ungridded dims. `Test_Scenarios.pf` already has
the parameterization machinery this sub-change needs to generalize that
pattern — `@testParameter`/`ESMF_TestParameter`, a
`testCase(constructor=..., testParameters={get_parameters()})` type, and
one `@test` subroutine driving N fixtures — but that machinery drives
legacy-only checks (`check_field_rank`, `check_field_value`, etc. reading
only the real `ESMF_State`); it has no graph-side read and no
`GRAPH_MODE_BOTH` selection. See proposal.md for full motivation.

## Goals / Non-Goals

**Goals:**
- One parameterized pFUnit test case that runs each covered fixture
  once under `GRAPH_MODE_BOTH` and performs both equivalence checks
  (no state leak; graph-native payload parity) generically, driven by a
  per-fixture description — not one hand-written subroutine per
  fixture.
- Three new flat fixtures (`plain_field`, `vertical_field`,
  `ungridded_dims`), each a direct parent + two children, each declaring
  exactly one exact-match connection whose both sides share one short
  name. Named for the graph capability each one exercises, not for the
  equivalence-check mechanism (proposal.md "New, deliberately clean
  flat fixtures").
- Reuse 10a's own selector (`set_graph_mode`/`GRAPH_MODE_BOTH`/
  `GRAPH_MODE_REGISTRY`) and resource-index lookup
  (`get_resource_index(proxy_key(...))`) exactly as the two existing
  hand-written tests already do — no new graph-side API.

**Non-Goals:**
- No change to `TypeKindCharacteristic`, extension-chain construction,
  or `MethodGraphNode` construction (10c/10d's own scope).
- No mismatched (units/typekind-differing) fixture — this harness
  targets ordinary exact-match connections only, same scope as 10a's
  own exact-match-branch materialization; the existing
  `test_units_mismatch_resolution_matches_legacy_coupler` (extension-
  chain case) is left as-is, not folded into this harness.
- No `FieldBundle`/`Vector` fixture (proposal.md's own recorded
  deviation) — deferred until graph-native `FieldBundle`/`Vector`
  materialization exists.
- No nested (propagated-import) fixture — out of scope until 3b2 lands
  (unfiled), same limitation `Test_GraphBuilderEquivalence.pf`'s own
  header comment already documents for its existing fixture.
- No change to the two existing hand-written tests in
  `Test_GraphBuilderEquivalence.pf` — they stay as direct, fixture-
  specific regression coverage for the exact-match and mismatch cases
  respectively; this sub-change adds a third, generalized test
  alongside them, it does not replace or absorb them.

## Decisions

**Fixture directory names reflect the graph capability under test, not
the equivalence-check mechanism; the mechanism's own names keep
"equivalence."** `plain_field`/`vertical_field`/`ungridded_dims`
(fixture directories, Task 1) are durable in a way the comparison
mechanism driving them is not: roadmap §20.4.2's Phase 6 retires
`StateRegistry`/`registry` mode once graph-native paths are the
validated default, at which point there is no legacy oracle left for a
`both`-mode check to compare against, and this capability's own
machinery (`EquivalenceFixtureDescription`, `EquivalenceFixture`, the
`graph/equivalence-harness` capability spec itself) is expected to be
deleted or repurposed — the fixtures are expected to outlive it,
most plausibly as plain `graph`-mode-only regression fixtures once that
day comes. Keeping "equivalence" in the mechanism's own names (the
testParameter/testCase types, the capability spec) is intentional, not
an oversight: those names accurately describe what the mechanism does
*today*, and renaming them to something mechanism-neutral now would not
make them more correct, only less descriptive in the interim. This
mirrors legacy's own `Test_Scenarios.pf` precedent — fixture directories
(`3d_specs`, `ungridded_dims`, `vector_1`) are named for capability,
never for whichever check function currently reads them.

**New `@testParameter`/`@testCase` pair in its own new file,
`Test_GraphEquivalenceHarness.pf` — not in `Test_GraphBuilderEquivalence.pf`
itself, a deviation from this document's own original wording found
during implementation (recorded here, same discipline this document
asks for elsewhere).** The original intent was to add the new
testParameter/testCase pair directly into
`Test_GraphBuilderEquivalence.pf`, alongside its existing two
`@test(npes=[0])` subroutines, reusing that file's own `use`-list
rather than duplicating it in a new module. Implementation discovered
this does not compile: pFUnit's preprocessor generates exactly one
testcase-wrapper type per module, keyed off whichever `@testCase` type
that module declares. Adding `EquivalenceFixture` as a second
`@testCase` in the same file as the existing `MpiTestMethod`-based
`@test(npes=[0])` subroutines causes the generated driver to force
`EquivalenceFixture` as the `this` argument into those existing
subroutines too — a real NAG compile error (`Actual procedure ...
argument 1 data type MPITESTMETHOD differs from dummy procedure
argument data type EQUIVALENCEFIXTURE`), not a design-time choice.
`Test_Scenarios.pf` avoids this because every `@test` in that file
already shares its one `Scenario` testCase; it never mixes a bare
`MpiTestMethod` test with a `testCase`-based one in the same module.
Of the two design.md non-goals this change still has to satisfy ("No
change to the two existing hand-written tests") and the one that
motivated the original "not a new file" wording ("new type lives in
a `.pf` scoped to this harness"), the only option that satisfies both
is a new, dedicated file: `Test_GraphEquivalenceHarness.pf`. Both files
build into the same `MAPL.generic.scenarios` pFUnit executable
(`CMakeLists.txt`'s `scenarios_test_srcs` list) and share one
`WORKING_DIRECTORY`, so `./graph-scenarios/...` fixture paths and the
`mapl_GraphMode_mod`/`mapl_GraphBuilder_mod` imports work identically
to the original single-file design — only the module/file boundary
changed, not the fixture layout, the testParameter/testCase shape, or
the generic `check` subroutine's own logic. The small testParameter
type — `EquivalenceFixtureDescription { name, root,
connection_short_name, has_run_phase }` — and the
`@testCase(constructor=new_EquivalenceFixture,
testParameters={get_equivalence_fixtures()})` type live in this new
file alone.

**One generic `@test` subroutine, driven by the fixture description,
replaces N hand-written subroutines.** Setup (config load, `cap.yaml`
init-phase run under `GRAPH_MODE_BOTH`, teardown to `GRAPH_MODE_REGISTRY`)
is identical across fixtures and is lifted directly from the existing
`test_both_mode_materializes_without_legacy_collision`'s own body,
parameterized only on `this%fixture%name` (used to build
`'./graph-scenarios/' // name // '/cap.yaml'` — the new, separate
fixture root (see proposal.md "New fixture root"), following
`Test_Scenarios.pf`'s own `setup`'s identical path-building pattern
otherwise) and
`this%fixture%connection_short_name` (used for both the
`ESMF_StateGet(dst_import_state, name, field, ...)` legacy-oracle read
and the `proxy_key(..., name)` graph-side lookup). Both equivalence
checks (state-entry-count parity; typekind/units/geom/shape parity via
`FieldGet` on both the legacy field and
`node%get_payload()%get_field()`) run unconditionally inside this one
subroutine for every parameter value — no per-check-type branching, since
every covered fixture needs both checks, unlike `Test_Scenarios.pf`'s own
per-check-type parameterization (there, a given fixture is checked once
per registered check function; here, a given fixture is checked once,
performing every check).

**New fixtures live under a new `graph-scenarios/` root, copied by a new
CMake target mirroring `copy_scenarios`.** Per proposal.md's own "New
fixture root" decision, the three new fixture directories live under
`superstructure/generic/tests/graph-scenarios/`, a sibling of
`scenarios/`, not inside it. `superstructure/generic/tests/CMakeLists.txt`
gains a `copy_graph_scenarios` custom target, copying
`${CMAKE_CURRENT_SOURCE_DIR}/graph-scenarios` to
`${CMAKE_CURRENT_BINARY_DIR}/graph-scenarios` at build time (same
`add_custom_target(... ALL COMMAND ${CMAKE_COMMAND} -E copy_directory
...)` shape as the existing `copy_scenarios` target), and
`MAPL.generic.scenarios`'s own `add_dependencies(...)` line gains
`copy_graph_scenarios` alongside its existing `copy_scenarios`
dependency — the same pFUnit executable (`Test_GraphBuilderEquivalence.pf`
already builds into `MAPL.generic.scenarios`) reads from both
`./scenarios/...` (its own two existing tests, unchanged) and
`./graph-scenarios/...` (new) at the same `WORKING_DIRECTORY`, so no new
pFUnit executable or CTest target is needed - only the one new copy
target.

**Fixed component/connection names across all three new fixtures,
following `graphbuilder_equivalence/`'s own existing convention,
despite the new fixtures living in a different directory tree.** Each
new fixture directory (under `graph-scenarios/`) uses `comp_src`/
`comp_dst` as its two children (not `A`/`B`, the convention
`3d_specs`/`ungridded_dims` use under `scenarios/`) — matching
`graphbuilder_equivalence/`'s own naming exactly, since this harness is
a direct generalization of that fixture's own existing test, not a new
family unrelated to it; the directory-tree separation (proposal.md) is
about keeping graph-mode-equivalence fixtures from mixing with legacy-
only ones, not about using a different naming convention for them. Each
fixture's single exact-match connection reuses one short name shared by
both export and import (mirroring `graphbuilder_equivalence/`'s own
`T`/`T` naming) rather than `3d_specs`'s own `E_*`/`I_*` convention (that
convention exists there to support multiple distinctly-named connections
per fixture; this harness's fixtures each declare exactly one).

**Shape/value parity is read via `FieldGet`, not raw `ESMF_FieldGet`.**
`mapl_FieldGet_mod`'s `FieldGet` (already `use`d in this file for the
existing mismatch test) is reused for both the legacy-oracle field and
the graph-native payload field — one call pattern for typekind/units,
plus a second, ordinary `ESMF_FieldGet(field, rank=..., farrayPtr=...)`
call (ESMF's own, not `FieldGet`'s) for rank/shape/value, mirroring how
`Test_Scenarios.pf`'s own `check_field_rank`/`check_field_value` already
read a field for the legacy-only case. No new field-comparison helper
module is introduced — comparison logic lives inline in the one new
`@test` subroutine, matching this file's own existing style (no shared
helper beyond the file's own `single_predecessor` function, which this
sub-change does not need since every covered fixture is a direct
exact-match with no extension chain to walk through).

**Run-phase value comparison was dropped from this sub-change's own
scope, found during implementation (recorded here, same discipline
this document asks for elsewhere).** `plain_field`'s own `cap.yaml`
still declares a `GENERIC_RUN_USER` phase call and `has_run_phase` is
still a real field on `EquivalenceFixtureDescription`/
`EquivalenceFixture` (a future `graph`-mode-alone sub-change can reuse
both once the gap below is fixed), but the harness's own `check`
subroutine no longer compares `farrayPtr` values after that run phase.
Direct experiment (switching `plain_field`'s own `set_graph_mode` call
from `GRAPH_MODE_BOTH` to `GRAPH_MODE_GRAPH`, to see whether
`graph`-mode-alone could satisfy this requirement) surfaced a real,
previously-unconfirmed gap instead:
`graphbuilder_populate_state()`'s "ordinary cross-component case"
(`GraphBuilder.F90`, `populate_one`) only ever inserts a resolved field
into the *parent*'s own scratch `MultiState` (`tmp_states`/
`outer_states`, built for `cap` itself in
`initialize_accept_transfer.F90`) — never into either sibling's own
real export/import `ESMF_State`. `ESMF_StateAdd` rejects the attempt
(`ESMF_RC_ARG_INCOMP`) on `plain_field`'s own flat, non-propagated
shape (`cap` has no declared states of its own to receive it). No
production code path populates a sibling-to-sibling connection's own
real `ESMF_State` under `GRAPH_MODE_GRAPH` today - confirmed by this
experiment, not merely inferred from `GraphBuilder.F90`'s own existing
"not exercised end-to-end" comments. Recorded in
`docs/graph/spec/20-implementation-roadmap.md` §20.4.8's own
"Explicitly out of scope" list as a new, named prerequisite, not filed
as its own `openspec` change yet. Given this, `both` mode's own
structural design (graph-native materialization never shares the
`ESMF_State` the run phase writes to, by construction - see
roadmap/proposal.md "no state leak" guarantee) means a `both`-mode
run-phase value comparison can never pass regardless of this gap: the
graph-native field, once allocated via `FieldCreate`/`FieldFill`, is
simply never written to by any run phase under `both` mode. Softening
`spec.md`'s "Data value agrees after a run phase" requirement (removed
scenario, requirement text updated to explain why) is therefore not a
quality compromise on this sub-change's own part - it is the correct
scope for what `both` mode can structurally ever demonstrate.
`plain_field`'s own run phase is retained anyway (static attributes -
typekind/units/geom/shape - are still checked identically whether or
not a run phase has executed, and the fixture itself remains valid,
reusable `graph`-capability coverage once a future sub-change actually
fixes `graphbuilder_populate_state` and flips the selector for real).
new, differently-shaped fixtures.

## Risks / Trade-offs

- **[Risk] A new, generalized test subroutine could silently diverge
  from the two existing hand-written tests' own exact setup/teardown
  sequence (e.g. forgetting to reset `GRAPH_MODE_REGISTRY` on an early
  failure path), leaking `both` mode into a later test.** → **Mitigation**:
  lift the setup/teardown body directly from
  `test_both_mode_materializes_without_legacy_collision`'s own working
  code rather than rewriting it; `set_graph_mode(GRAPH_MODE_REGISTRY)`
  is called unconditionally after the init-phase loop in that existing
  test's own body, before any assertion runs — the same ordering is
  preserved here.
- **[Risk] Shared `comp_src`/`comp_dst` naming across all three new
  fixture directories could cause a stale-state collision if a future
  change ever loads more than one of them into the same test
  process/DSO.** → **Mitigation**: each fixture already lives in its own
  directory with its own `cap.yaml`/`comp_src.yaml`/`comp_dst.yaml`,
  loaded via `ESMF_HConfigCreate(filename=...)` per fixture, same
  isolation `3d_specs`/`ungridded_dims`/`graphbuilder_equivalence`
  already rely on side by side today — not a new risk this sub-change
  introduces.
- **[Risk] A second fixture root (`graph-scenarios/`) alongside the
  existing `scenarios/` could confuse a future contributor about which
  tree a new graph-equivalence fixture belongs in.** → **Mitigation**:
  `graph-scenarios/` is scoped narrowly and documented in proposal.md -
  only fixtures exercised under `GRAPH_MODE_BOTH`/graph-side payload
  assertions belong there; every other fixture (including the two
  pre-existing `graphbuilder_equivalence*` ones, left under `scenarios/`
  per proposal.md) stays in `scenarios/`. A module-header comment in
  `Test_GraphBuilderEquivalence.pf`'s own new testParameter section
  states this rule explicitly for the next contributor.
- **[Risk] This harness duplicates fixture-authoring effort already
  present in `Test_Scenarios.pf`'s own fixture set (e.g. `3d_specs`,
  `ungridded_dims` already exist there) rather than reusing those exact
  directories.** → **Mitigation**: deliberate, not accidental — those
  existing fixtures are nested in places (`3d_specs` has two
  bidirectional connections between `A`/`B`, not a single one-directional
  exact match) and were authored for `Test_Scenarios.pf`'s own
  legacy-only, per-check-type parameterization; reusing them as-is would
  require this harness to either generalize to multi-connection fixtures
  (deferred complexity, no current forcing function) or hand-pick a
  single connection out of a multi-connection fixture (fragile,
  behavior-coupled to that fixture's own unrelated-to-this-harness
  future edits). New, minimal, single-connection fixtures are
  deliberately simpler to reason about and keep this harness decoupled
  from `Test_Scenarios.pf`'s own fixture evolution.
