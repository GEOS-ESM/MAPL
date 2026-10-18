## 1. Widen dispatch and resolution to cover SimpleConnection

- [x] 1.1 Add a new small helper function
      `effective_source_name(conn, dst_node)` in
      `GraphBuilder_ConnectionResolution_smod.F90`: `class is
      (SimpleConnection)` returns `conn%get_source()%get_esmf_name()`
      (the connection's own real declared source name); `class
      default` (i.e. `MatchConnection`, matching legacy's own
      per-match `SimpleConnection` construction in
      `MatchConnection.F90:91-92,148-149`) returns
      `dst_node%get_short_name()` — byte-for-byte what every call site
      already computes today, so `MatchConnection`'s own resolution
      outcome is unchanged. Per design.md D1's correction — this is
      the ONLY place either connection subtype's own dynamic type is
      inspected; everything else stays untouched.
- [x] 1.2 Change `resolve_match_connection`'s dummy argument from
      `type(MatchConnection), intent(in) :: conn` to
      `class(Connection), intent(in) :: conn`. Replace the one call
      site inside `resolve_one` that currently passes
      `dst_node%get_short_name()` as a *source*-side lookup key
      (`find_export_node_id(this, src_pt%component_name,
      dst_node%get_short_name(), ...)`, the main exact-match/
      mismatch path) with `effective_source_name(conn, dst_node)`
      instead. Do NOT change any call site that correctly uses
      `dst_node%get_short_name()` for a *destination*-side purpose
      (e.g. `GraphResolutionEntry` reporting, `import_node_id`) —
      those remain unchanged. `resolve_callback_destination`'s own
      `dst_short_name` parameter is confirmed NOT to have this bug
      (its source-side resolution goes through
      `resolve_callback_import`'s own pattern-match against `src_pt`,
      never substituting the destination's name) — leave it
      unchanged.
- [x] 1.2b Fix the identical bug in `resolve_inout_destination`
      (called from `resolve_one`'s inout-borrower branch) — found to
      have TWO instances of the same substitution bug, not one: (a)
      its own `find_export_node_id(this, src_pt%component_name,
      dst_node%get_short_name(), owner_node_id, owner_node, _RC)`
      call (the owner lookup), and (b) its own chained-borrower scan,
      which compares each of the owner's own import ports against
      `dst_node%get_short_name()` to detect "is the owner itself a
      borrower of the same name" — that comparison must use
      `owner_node%get_short_name()` (the OWNER's own, correctly
      resolved name) instead, or chained-borrower detection silently
      compares against the wrong name whenever `src_name /= dst_name`.
      Fix (a) by adding an `effective_src_name` parameter to this
      subroutine's signature (passed from `resolve_one` as
      `effective_source_name(conn, dst_node)`) and using it in the
      `find_export_node_id` call; fix (b) by comparing against
      `owner_node%get_short_name()` (already resolved by (a)'s own
      fix), not `dst_node%get_short_name()`, needing no additional
      parameter. No current fixture exercises an inout-borrower
      declaration at all (confirmed: no `is_inout_borrower` scenario
      fixture exists today), so both instances were latent, not
      observed via the real-fixture regression — fixed here for
      consistency with the same bug class, with a dedicated new test
      (4.7 below) added specifically because nothing else exercises
      it.
- [x] 1.3 Apply the same dummy-argument widening to
      `check_match_connection_unsatisfied`, and the same
      `effective_source_name(conn, node)` substitution inside
      `check_one`'s own `find_export_node(src_meta,
      node%get_short_name())` call.
- [x] 1.4 Update `graphbuilder_resolve_connections`'s `select type`
      block (`:1028-1034` pre-edit numbering) to add
      `class is (SimpleConnection)` calling `resolve_match_connection`,
      alongside the existing `class is (MatchConnection)` arm. Update
      the `class default` comment to name only
      `ReexportConnection`/wildcard-only forms as remaining out of
      scope.
- [x] 1.5 Apply the same `class is (SimpleConnection)` addition to
      `graphbuilder_check_unsatisfied_imports`'s `select type` block
      (`:427-433` pre-edit numbering).
- [x] 1.6 Add `use mapl_SimpleConnection_mod, only: SimpleConnection`
      (or confirm it's already reachable via host association from
      `GraphBuilder.F90`'s own `use`) in this submodule.
- [x] 1.7 Verify any `public ::` declaration needed for gfortran
      cross-TU linkage (per `docs/graph/ARCHITECTURE.md` §4.2's own
      lesson about this file) — check whether widening an existing
      dummy-argument type, or adding `effective_source_name` as a
      plain (non-`module`) procedure called only from within this same
      submodule, requires any new public symbol; if not, explicitly
      confirm and note why.
- [x] 1.8 Build (`cmake --build nag --target build-tests`) and run the
      REAL scenario suite (`ctest --test-dir nag -L ESSENTIAL`, not
      just synthetic unit tests) before proceeding to Group 2 —
      confirm `names_1`/`statistics_real`/every other
      `SimpleConnection`-bearing fixture passes. This step is listed
      explicitly, separate from Group 6's own final regression pass,
      because the first attempt at this group skipped it and shipped
      a real regression (211/326 test failures) that only surfaced
      once real fixtures were actually run — see design.md D1's own
      correction narrative.

## 2. Give SimpleConnection the same structural check MatchConnection already uses in connection_resolves_import

- [x] 2.1 In `connection_resolves_import`, extract the existing
      `class is (MatchConnection)` branch's body into a new shared
      function `connection_pt_pair_resolves(owner, target_meta, node,
      c, rc)` taking `class(Connection), intent(in) :: c` — moved
      verbatim, EXCEPT its own source-export lookup
      (`associated(find_export_node(src_meta, node%get_short_name()))`)
      must change to
      `associated(find_export_node(src_meta, effective_source_name(c, node)))`,
      using the same `effective_source_name` helper from Group 1 (per
      design.md D3's own correction) — a `SimpleConnection` with
      `src_name /= dst_name` must be checked against its own real
      source name here too, not against `node`'s destination-side
      name.
- [x] 2.2 Replace the `class default` branch's destination-name-
      matching logic with a new `class is (SimpleConnection)` arm
      calling the same shared function from 2.1. `class default` (now
      covering only `ReexportConnection` and other genuinely
      out-of-scope subtypes) keeps `resolved = .false.` with no
      further logic — per design.md D3 (corrected: no
      `has_predecessors`/graph-edge query anywhere in this function —
      no dependency edge exists yet at this call site's point in the
      lifecycle).
- [x] 2.3 Confirm the `class is (MatchConnection)` arm's own observable
      behavior is unchanged after the extraction (same callback and
      exact-export-match early-exits, same `exit`/`cycle` control
      flow) — `effective_source_name`'s own `class default` branch
      returning `dst_node%get_short_name()` guarantees this.
- [x] 2.4 Confirm `SimpleConnection` is reachable in this function's
      scope (shares the submodule-wide `use` from Group 1.6).
- [x] 2.5 Build and re-run the real scenario suite again (same
      reasoning as task 1.8) — `connection_resolves_import` feeds
      propagation/mirroring, a different code path from Group 1's own
      `resolve_one`/`check_one`, so Group 1's own passing run does not
      guarantee this group is also correct.

## 3. Spec and roadmap documentation

- [ ] 3.1 Sync the modified `graph/graph-builder` requirements from
      this change's delta spec into `openspec/specs/graph/graph-builder/spec.md`
      once implementation is verified (via `openspec-sync-specs` or
      archive flow — not by hand-editing ahead of landing the code).
- [x] 3.2 Add a cross-reference at
      `docs/graph/spec/20-implementation-roadmap.md:1649` noting that
      `SimpleConnection` is now resolved by `GraphBuilder` (pointing at
      this change), replacing its current framing as purely a
      legacy-file deprecation candidate.
- [x] 3.3 Update `docs/graph/ARCHITECTURE.md` if its own
      `GraphBuilder_ConnectionResolution_smod.F90` summary (§1.2 table,
      §2.3) still describes resolution as `MatchConnection`-only after
      this change lands — per that file's own stated maintenance
      expectation of staying in sync with source.

## 4. Tests: SimpleConnection resolution coverage

- [x] 4.1 Add `test_simple_connection_resolves_exact_match` to
      `Test_GraphBuilder.pf`: declare a `SimpleConnection` between a
      matching export/import pair, call `gb%resolve_connections`,
      assert a dependency edge exists from export to import node
      (mirrors existing `MatchConnection` exact-match test).
- [x] 4.1b Add `test_simple_connection_resolves_with_different_src_dst_names`:
      a `SimpleConnection` with `src_name /= dst_name` (e.g. source
      export `E_A1` wired to destination import `I_B1`, mirroring
      `scenarios/names_1/parent.yaml`'s real shape) — this is the
      exact case the real-fixture regression in design.md D1 was
      caught by; a dedicated unit test makes the fix's own motivating
      bug directly testable in isolation, not just via the full
      `statistics_real`/`names_1` scenario run.
- [x] 4.2 Add `test_simple_connection_resolves_through_extension_chain`:
      a `SimpleConnection` between a mismatched export/import pair
      (also with `src_name /= dst_name`, to cover both bugs this
      change fixes in one test), assert delegation to
      `find_or_build_extension_chain` produces the same chain-wiring
      outcome an equivalent `MatchConnection` match would.
- [x] 4.3 Add `test_simple_connection_unresolved_import_is_reported`:
      a `SimpleConnection` whose declared source has no matching
      export, assert it is reported via `unresolved_imports`, not
      silently dropped.
- [x] 4.4 Add `test_simple_connection_resolution_is_idempotent`: call
      `gb%resolve_connections` twice for a component with a declared
      `SimpleConnection`, assert the second call is a no-op (mirrors
      existing `connections_already_resolved()` guard behavior).
- [x] 4.5 Replace `test_non_matchconnection_is_skipped`
      (`Test_GraphBuilder.pf:879-909`) — its current assertion
      (`unresolved%size() == 0` after resolving a `SimpleConnection`,
      with no dependency edge created) is exactly the behavior this
      change removes. Replace with a test asserting a declared
      `SimpleConnection` and an equivalent single-match
      `MatchConnection` produce identical outcomes (per the new
      "A declared SimpleConnection is resolved exactly as an
      equivalent MatchConnection match would be" spec scenario).
- [x] 4.6 Add a test for the D3 propagation fix: a `SimpleConnection`
      naming a destination whose source does NOT actually resolve to a
      real export; assert the import is still reported unresolved and
      still mirrored to the parent (per the new "A SimpleConnection
      naming a destination with no real matching export does not
      suppress mirroring" spec scenario) — this is a regression test
      for the false-negative design.md identifies.
- [x] 4.7 Add `test_simple_connection_resolves_inout_different_names`
      (name shortened from the original draft — NAG's 63-character
      procedure-name limit):
      a `SimpleConnection` with `src_name /= dst_name` where the
      destination item is declared an ordinary inout borrower
      (`is_inout_borrower()`), exercising `resolve_inout_destination`'s
      own corrected `effective_source_name` call (task 1.2b) — no
      existing fixture or test exercises inout-borrower resolution via
      a non-same-name connection at all, so without this test task
      1.2b's own fix would ship unverified.

## 5. Tests: real-fixture equivalence coverage

- [x] 5.1 Add a `statistics_real`-based test case
      (`test_statistics_real_simple_connections_match_legacy_coupler`)
      to `Test_GraphBuilderEquivalence.pf`, running the existing
      `scenarios/statistics_real/` fixture and asserting dependency-
      edge parity between GraphBuilder's own resolution and the
      legacy coupler's real ESMF import-state entry, at `history`'s
      own level (`stat` → `collection_1`, the fixture's flat,
      sibling-to-sibling `SimpleConnection`s — the cap-level
      `root`→`history` connection remains out of scope per this
      module's own existing header comment: cross-level propagation,
      not this change's concern). Uses `build_scenario_tree`
      (`mapl_GraphScenarioBootstrap_mod`) rather than hand-rolling the
      bootstrap sequence.
- [x] 5.2 Real-lifecycle run surfaced a gap, documented rather than
      silently absorbed: running under `GRAPH_MODE_BOTH` (as
      originally specified) crashes with "materialization reached
      with no import units - internal inconsistency"
      (`GraphBuilder.F90:1230`) — `collection_1.yaml`'s own imports
      declare no `units:` at all (legal, legacy-tolerated), but
      `GRAPH_MODE_BOTH`'s materialization step wrongly treats that as
      an internal inconsistency. Root-caused as a pre-existing
      materialization-layer gap, orthogonal to connection-type
      dispatch (would fire for any connection type, including
      `MatchConnection`, wiring into a units-less import under
      `GRAPH_MODE_BOTH` — never previously exercised). **Decision:
      out of scope for this change** (per user direction) — filed as
      a separate follow-up, not fixed here. The test instead uses the
      default `GRAPH_MODE_REGISTRY` (matching
      `test_ordinary_resolution_matches_legacy_coupler`'s own
      pattern), which proves dependency-edge parity — the actual
      claim this change makes — without depending on the unrelated
      materialization gate. design.md D4 updated to record this
      correction.

## 6. Full regression verification

- [x] 6.1 Build and run `ctest -L ESSENTIAL` under NAG per
      `docs/graph/ARCHITECTURE.md` §4.1 — confirm 100% pass, zero
      regression.
- [x] 6.2 Build and run `ctest -L ESSENTIAL` under gfortran per §4.2 —
      confirm 100% pass, zero regression, matching NAG.
- [x] 6.3 Run the full (non-`ESSENTIAL`-filtered) `ctest` suite under
      both compilers; confirm the only failures are the pre-existing,
      documented `ll-ll`/`cs-cs`/`cs-ll`/`ll-cs` app-regression tests
      (§4.1/§4.5), not anything new.
- [x] 6.4 Specifically re-run `MAPL.generic.scenarios` and
      `MAPL.generic.components` (the targets covering
      `statistics_real` and the new `SimpleConnection` unit tests) with
      `-v` to confirm each new/changed test individually passes.
