## 1. Fixtures

- [x] 1.1 Create `superstructure/generic/tests/graph-scenarios/`, a new
      sibling directory to the existing `scenarios/` tree, for
      graph-mode-equivalence fixtures only.
- [x] 1.2 Create `graph-scenarios/plain_field/` (`cap.yaml`,
      `comp_src.yaml`, `comp_dst.yaml`): single `R4` scalar field
      connection, `comp_src`/`comp_dst` naming, `graphbuilder_equivalence/`
      shape (one `all_unsatisfied: true` connection), plus a
      `GENERIC_RUN_USER` phase that sets a known data value on the
      export side. Named for the capability under test (plain field), not
      for the equivalence-check mechanism (design.md "Fixture directory
      names reflect the graph capability").
- [x] 1.3 Create `graph-scenarios/vertical_field/`: same shape as
      1.2 but the connection's field declares a vertical dimension
      (`vertical_dim_spec: CENTER` or equivalent), static (no run
      phase).
- [x] 1.4 Create `graph-scenarios/ungridded_dims/`: same
      shape as 1.2 but the connection's field declares at least one
      `ungridded_dims` entry, static (no run phase).
- [x] 1.5 Verify each new fixture's `cap.yaml` loads and resolves under
      the existing legacy (`registry`-mode) path by running it through
      `Test_Scenarios.pf`-style setup manually (or a scratch test) before
      wiring the new harness - confirms the fixture itself is valid
      independent of this sub-change's own new test code. Confirmed via
      the harness's own no-leak check (task 4.2), which asserts the
      legacy field exists with the same single-entry count
      `GRAPH_MODE_REGISTRY` would produce, for all three fixtures, in
      the passing full-suite run.

## 2. Build wiring

- [x] 2.1 Add a `copy_graph_scenarios` custom target to
      `superstructure/generic/tests/CMakeLists.txt`, mirroring the
      existing `copy_scenarios` target
      (`${CMAKE_COMMAND} -E copy_directory
      ${CMAKE_CURRENT_SOURCE_DIR}/graph-scenarios
      ${CMAKE_CURRENT_BINARY_DIR}/graph-scenarios`).
- [x] 2.2 Add `copy_graph_scenarios` to `MAPL.generic.scenarios`'s own
      `add_dependencies(...)` call, alongside its existing
      `copy_scenarios` dependency.

## 3. Harness scaffolding

- [x] 3.1 Add `EquivalenceFixtureDescription` `@testParameter` type to
      a new `Test_GraphEquivalenceHarness.pf` (`name`, `root`,
      `connection_short_name`, `has_run_phase` fields; `tostring`
      override) - a new file, not `Test_GraphBuilderEquivalence.pf`
      itself (deviation from the original plan, discovered during
      implementation and recorded in design.md Decisions: pFUnit
      generates one testcase-wrapper type per module, so a second
      `@testCase` in the same file as the existing `MpiTestMethod`-based
      `@test(npes=[0])` subroutines does not compile). Add a
      module-header comment stating that `graph-scenarios/` is reserved
      for `GRAPH_MODE_BOTH`-exercised, graph-side-payload-asserting
      fixtures only (design.md Risk mitigation). Register the new file
      in `CMakeLists.txt`'s `scenarios_test_srcs` list alongside
      `Test_GraphBuilderEquivalence.pf`.
- [x] 3.2 Add `EquivalenceFixture` `@testCase` type
      (`constructor=new_EquivalenceFixture`,
      `testParameters={get_equivalence_fixtures()}`), holding the
      resolved `cap_gc`/`cap_meta` state needed across setup and the
      test body.
- [x] 3.3 Implement `get_equivalence_fixtures()` returning the three
      parameter entries from Task 1 (`plain_field`: `has_run_phase=.true.`;
      `vertical_field` and `ungridded_dims`: `has_run_phase=.false.`).

## 4. Generic equivalence test

- [x] 4.1 Implement the one `@test` subroutine: load
      `./graph-scenarios/<fixture%name>/<fixture%root>`, run
      `GENERIC_INIT_PHASE_SEQUENCE` under `GRAPH_MODE_BOTH`, reset to
      `GRAPH_MODE_REGISTRY` immediately after (mirroring
      `test_both_mode_materializes_without_legacy_collision`'s existing
      ordering), then run the fixture's `GENERIC_RUN_USER` phase when
      `has_run_phase` is true.
- [x] 4.2 Implement the no-leak check: `ESMF_StateGet` the destination's
      real import `ESMF_State` by `connection_short_name`, assert the
      field exists, and assert `itemCount` on that state equals what it
      would be in `registry` mode (1, for every covered fixture's single
      connection).
- [x] 4.3 Implement the payload-parity check: resolve the export's proxy
      node via `get_resource_index(proxy_key('comp_src',
      ESMF_STATEINTENT_EXPORT, connection_short_name))`, read its
      `GraphStateItem%get_field()`, and compare `FieldGet`-reported
      typekind/units/geom against the legacy oracle field's own
      `FieldGet` results.
- [x] 4.4 Extend the payload-parity check with rank/shape comparison via
      `ESMF_FieldGet(..., rank=..., farrayPtr=...)` on both fields (not
      `FieldGet`, which does not report shape directly).
- [x] 4.5 **Dropped, found during implementation (design.md Decisions
      "Run-phase value comparison was dropped from this sub-change's own
      scope"; spec.md no longer requires it).** Originally: for the
      `plain_field` fixture only (`has_run_phase` true), extend the
      check with a data-value comparison between the two `farrayPtr`s
      after the run phase. Direct experiment (switching `plain_field`'s
      `set_graph_mode` call to `GRAPH_MODE_GRAPH`, to see whether
      `graph`-mode-alone could satisfy this) surfaced a real
      production-code gap instead - `graphbuilder_populate_state()`
      does not populate a sibling-to-sibling connection's own real
      `ESMF_State` under `GRAPH_MODE_GRAPH` (fails with
      `ESMF_RC_ARG_INCOMP`), recorded as a new "Explicitly out of scope"
      entry in `docs/graph/spec/20-implementation-roadmap.md` §20.4.8,
      not filed as its own change. Independent of that gap, `both`
      mode's own structural design (graph-native materialization never
      shares the `ESMF_State` a run phase writes to) means this
      comparison could never pass under `both` mode regardless -
      `plain_field`'s own run phase still executes in setup (fixture
      stays valid, reusable coverage for a future `graph`-mode-alone
      sub-change) but the harness's `check` subroutine no longer
      compares values.

## 5. Verification

- [x] 5.1 Build and run `Test_GraphEquivalenceHarness.pf` (NAG or
      gfortran, per `mapl-testing` skill); confirm 3 new parameterized
      cases pass, alongside the 2 existing hand-written tests in
      `Test_GraphBuilderEquivalence.pf` (now unmodified from main),
      5 total across the two files. Confirmed: full suite builds and
      passes.
- [x] 5.2 Confirm no change in behavior/pass status for
      `Test_Scenarios.pf`'s own existing fixtures (regression check -
      this sub-change touches no production code, and `scenarios/` is
      untouched by it). Confirmed: full suite (322 tests) passes with
      this sub-change's changes in place.
- [x] 5.3 Run `openspec validate --change phase-10b-equivalence-harness
      --strict` and fix any reported issues.
