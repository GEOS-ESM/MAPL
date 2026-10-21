## 1. `GraphBuilder_ConnectionResolution_smod.F90`: new `resolve_one` branch (design.md D1-D5)

- [x] 1.1 Inside `resolve_one` (`GraphBuilder_ConnectionResolution_
      smod.F90`'s `resolve_match_connection`), add a new branch checking
      `dst_node%get_payload()%get_declared_item_type() ==
      MAPL_STATEITEM_WILDCARD`, placed immediately after the existing
      inout-borrower branch (and before the ordinary exact-match
      `import_node_id = dst_node%get_node_id()` code that follows it) -
      same "check, dispatch, `_RETURN(_SUCCESS)` on this path" idiom as
      the callback-interface and inout-borrower branches immediately
      above it.
- [x] 1.2 Add a header comment on the new branch citing design.md D1-D5
      and proposal.md's own "Why this is a composite-member fan-in
      problem, not a multi-producer `DependencyNetwork` problem"
      rationale, matching the callback-interface branch's own citation
      style immediately above it.
- [x] 1.3 Add a new module procedure `resolve_wildcard_destination(this,
      src_pt, dst_pt, dst_node, unresolved, rc)` (co-located with
      `resolve_callback_destination`/`resolve_inout_destination`,
      signature mirroring `resolve_inout_destination`'s shape -
      `this`/`src_pt`/`dst_pt`/`dst_node`/an output-reporting vector/
      `rc`), called from the new branch exactly as
      `resolve_callback_destination` is called from its own branch.
- [x] 1.4 Implement `resolve_wildcard_destination` (design.md D4):
      compute the idempotency key `'WILDCARD_COLLECTION:' //
      dst_node%get_node_id()%to_string()`; if
      `graph%get_resource_index(key)` is already associated, `_RETURN
      (_SUCCESS)` immediately (no re-resolution, no duplicate
      reporting).
- [x] 1.5 Implement the source-side match scan (design.md D2): resolve
      `root_meta => component_for(this, src_pt%component_name)`
      (reusing `component_for`'s own existing self-or-child dispatch,
      the same call `resolve_callback_import` already makes), then
      `namespace = build_qualified_export_namespace(root_meta, _RC)`
      (reused as-is, no change to that function). For each entry,
      `candidate_v_pt = VirtualConnectionPt(ESMF_STATEINTENT_EXPORT,
      namespace(i)%qualified_name)`; keep it when `dst_pt%v_pt%
      matches(candidate_v_pt)` (note the operand order: `dst_pt%v_pt`
      is `this` in `matches()`, mirroring `for_each_matching_import`'s
      own existing destination-side call exactly, substituting the
      EXPORT-side candidate for the IMPORT-side one `resolve_callback_
      import` uses `src_pt%v_pt` for - design.md D2's own explicit
      note on this asymmetry). Accumulate matches via the same
      fresh-temporary + `move_alloc` growth idiom `collect_qualified_
      exports`/`resolve_callback_import` already use (not a
      self-referencing array constructor - known ICE precedent cited
      throughout this file).
- [x] 1.6 Zero-match case (design.md D5): if no entry matched, `call
      unresolved%push_back(GraphResolutionEntry(dst_pt%component_name,
      dst_node%get_short_name()))` and `_RETURN(_SUCCESS)` - do not
      create a resource-index entry for this key (so a later real
      connection, if one ever supplies a match, is not blocked by a
      stale "already resolved" guard for what was actually a failure).
- [x] 1.7 One-or-more-match case (design.md D1/D3): `payload =>
      dst_node%get_payload_ptr()`; `state = ESMF_StateCreate(_RC)`;
      `call payload%set(state, _RC)`; for each match, `member_id =
      resolve_qualified_export_node_id(this, src_pt%component_name,
      matches(i), _RC)` then `call payload%add_state_member(matches(i)%
      qualified_name, member_id, _RC)` - both calls reused verbatim
      from `materialize_callback_collection`'s own per-match loop body,
      applied to `dst_node`'s own existing payload pointer instead of a
      freshly-allocated local `GraphStateItem` (design.md D1's "in
      place, not synthesize a fresh node").
- [x] 1.8 Record completion: `call graph%add_resource_index(key,
      dst_node%get_node_id(), _RC)` (the key from 1.4; the value is
      this import's own existing `NodeId`, not a freshly-minted one -
      design.md D1).
- [x] 1.9 Confirm (by inspection, before writing tests) that no
      `add_dependency` call appears anywhere in the new code path -
      composite membership is pure reference binding (design.md
      Context's `DependencyNetwork` paragraph), zero edges added to any
      `DependencyNetwork`.

## 2. `is_ordinary_resolvable_import`: narrow the exclusion (design.md D6)

- [x] 2.1 In `is_ordinary_resolvable_import`
      (`GraphBuilder_ConnectionResolution_smod.F90`, ~line 1989-2001),
      remove the `declared_type == MAPL_STATEITEM_WILDCARD .or.` arm;
      leave the `MAPL_STATEITEM_SERVICE` arm unchanged.
- [x] 2.2 Update that procedure's own header comment (and the nearby
      module-header paragraph citing "WILDCARD/SERVICE - has no
      graph-native resolution path at all") to record the narrowing,
      citing this change and design.md D6, noting a mechanism now
      exists for `WILDCARD` specifically (mirroring Phase 7b's own
      identical comment-update precedent for `EXPRESSION`).

## 3. Unit tests: `resolve_wildcard_destination` (design.md D1-D5)

- [x] 3.1 Add test cases to `superstructure/generic/tests/Test_
      GraphBuilder_ConnectionResolution.pf` (or confirm during
      implementation whether a different existing file's fixture setup
      fits better, following the same "confirm during implementation"
      latitude Phase 7b's own task 9.1 used): construct a component
      with a `WILDCARD`-typed import (declared short name a regex, e.g.
      `'A/E_.*'`) and a sibling/descendant component tree with two or
      more real exports matching that pattern under
      `build_qualified_export_namespace`'s qualified naming, drive the
      match through a real `MatchConnection`/`all_unsatisfied`-style
      connection, run connection resolution, and assert: the wildcard
      import's own `NodeId` now reports `itemType() == ESMF_STATEITEM_
      STATE`, and `state_members()` contains exactly one entry per
      matching export, each bound to that export's own real `NodeId`.
      (`test_wildcard_import_fans_out_into_composite_members`, built on
      a new shared `build_wildcard_hierarchy` fixture mirroring
      `history_wildcard`'s own root/A, root/B shape - both exports
      reached only through `A`'s own qualified name, satisfying 3.5's
      cross-level-qualification requirement in the same test.)
- [x] 3.2 Case: exactly one match (degenerate N=1) resolves through the
      identical code path as a multi-match case - no separate
      single-match branch exists (confirm by covering it, not by
      inspection alone).
      (`test_wildcard_import_resolves_single_match`.)
- [x] 3.3 Case: zero matches reports a `GraphResolutionEntry` via the
      `unresolved` output (design.md D5) and creates no resource-index
      entry, no composite payload (`dst_node`'s own `itemType()` stays
      whatever it was before resolution was attempted - confirm it is
      NOT `ESMF_STATEITEM_STATE`).
      (`test_wildcard_import_zero_matches_reports_unresolved`.)
- [x] 3.4 Case: re-running connection resolution a second time for the
      same already-resolved wildcard import does not duplicate
      `state_members()` entries and does not re-create the `ESMF_State`
      payload (idempotency, design.md D4).
      (`test_wildcard_import_resolution_is_idempotent`.)
- [x] 3.5 Case: a matching export reachable only through a descendant's
      own qualified export name (not a direct export of the
      connection's own source component) is still matched and included
      - exercises `build_qualified_export_namespace`'s cross-level
      qualification directly through this new code path (design.md
      Context's "already populated... as a byproduct of already-landed
      work" claim), not merely asserted by reading the Group 6/6.8
      code.
      (Covered by 3.1's own test - see that entry's note; `build_
      wildcard_hierarchy`'s 'A'/'B' are children of root, not root
      itself, so every match in 3.1-3.4's tests already goes through
      qualified names like `'A/E_A1'`, never a bare `'E_A1'`. No
      separate test added - would duplicate 3.1's own assertions with
      no new code path exercised.)
- [x] 3.6 Case: after resolution, `graphbuilder_unresolved_import_
      node_ids` (the only reachable observation point for the private
      `is_ordinary_resolvable_import`, same approach Phase 7b's own
      task 9.4 used) no longer reports the resolved `WILDCARD` import
      as unconditionally excluded (design.md D6), while a `SERVICE`-
      typed import on the same component still is (negative control).
      (`test_wildcard_no_longer_excluded_from_gate`, companion to the
      pre-existing `test_resolved_expression_no_longer_excluded_from_
      gate`, which already exercises the `SERVICE`-stays-excluded
      negative control on the same gate - not duplicated here.)

  All 4 new tests (plus the 2 pre-existing gate tests above) pass under
  both NAG and gfortran (`ctest -R "^MAPL.generic.components$"`, full
  suite 1/1 Passed both compilers; see Section 5 verification below).

## 4. Fixture: `history_wildcard` end-to-end coverage (proposal.md, design.md Risks)

- [x] 4.1 Confirm whether any existing `GRAPH_MODE_GRAPH`/`GRAPH_MODE_
      BOTH`-capable harness (`Test_GraphBuilderEquivalence.pf`,
      `Test_GraphEquivalenceHarness.pf`, `Test_GraphModeDemonstration.pf`)
      can reach `history_wildcard` at all - `Test_GraphBuilderEquivalence.
      pf`'s own module header already documents that GraphBuilder's
      ordinary resolution does not see items living on a grandchild
      reached only through `StateRegistry`'s own propagate_exports/
      propagate_unsatisfied_imports view, and that every existing
      `scenarios/` fixture (`history_wildcard` included) is exactly
      that shape - confirm this is still the case post-Group-6/6.8 (the
      cross-level propagation this change's own proposal.md claims is
      now available may have changed this), and record the finding
      rather than assuming either answer.
      (Finding, confirmed empirically via a throwaway probe test under
      `build_scenario_tree`/`GRAPH_MODE_BOTH`, not by inspection alone:
      `history_wildcard` IS now reachable, unlike this module's own
      other grandchild-nested fixtures (`statistics_real`). Reason:
      `history_wildcard`'s shape differs from the grandchild-visibility
      gap the module header describes - `cap`'s own `all_unsatisfied`
      connection names `root`/`history` directly, and `history`'s own
      *immediate* child `collection_1` is where the `WILDCARD` import
      is actually advertised (one level of nesting, not two). Combined
      with Group 6/6.8's own already-landed cross-level NodeId
      mirroring plus this change's own `find_owning_component_graph`/
      `wildcard_import_has_match` fixes (Bugs #3/#4, this change's own
      implementation), `resolve_one`'s new WILDCARD branch reaches and
      resolves it correctly even though the governing connection is
      declared two levels above `collection_1` itself. The module
      header's "GraphBuilder does not see those" claim remains correct
      for the grandchild-reached-only-through-propagate_exports shape
      `statistics_real` exercises - `history_wildcard` was simply never
      that shape to begin with, a pre-existing mischaracterization in
      the header's own fixture list, not something this change altered.)
- [x] 4.2 If 4.1 finds `history_wildcard` now reachable through one of
      these harnesses (post-Group-6/6.8 propagation), add a
      `GRAPH_MODE_BOTH` equivalence case for it, asserting the
      graph-native result's own composite membership for `history/
      collection_1`'s wildcard import (`A/E_A1`, `A/E_A2`, `B/E_B2` -
      `expectations.yaml`) matches the legacy-coupler result's own
      `MultiState` membership one-for-one.
      (`test_history_wildcard_composite_matches_legacy_coupler`, added
      to `Test_GraphBuilderEquivalence.pf`: legacy-coupler oracle checks
      `collection_1`'s real import `ESMF_State` contains `A/E_A1`,
      `A/E_A2`, `B/E_B2`; graph-native side checks the wildcard import's
      own `NodeId` reports `itemType() == ESMF_STATEITEM_STATE` with
      exactly 2 `state_members()` entries, `A/E_A1` and `A/E_A2` (`B/
      E_B2` is bound as an ordinary import, not wildcard composite
      membership - not part of this assertion). Passes under both NAG
      and gfortran as part of `MAPL.generic.scenarios`'s own 330-test
      run; see Section 5 verification below.)
- [x] 4.3 If 4.1 finds it still unreachable (the grandchild-visibility
      gap `Test_GraphBuilderEquivalence.pf`'s header describes is a
      property of THAT harness's own fixture-tree-walking, not of
      GraphBuilder's own resolution capability, and may not be fixed by
      this change alone), document this as a known limitation in this
      task's own completion note - consistent with Phase 7b's own task
      10 precedent of documenting an out-of-scope harness gap rather
      than silently expanding scope to fix it - and rely on task 3's
      direct unit coverage (synthetic component trees, not the real
      YAML fixture) as this change's own sufficient forcing-function
      coverage instead.
      (N/A - 4.1 found `history_wildcard` reachable; 4.2 implemented
      instead. No limitation to document.)
- [x] 4.4 Confirm the existing `MAPL.generic.scenarios` run of
      `history_wildcard` under its current (legacy-coupler,
      `GRAPH_MODE_REGISTRY`) mode is unaffected by this change - no
      YAML change, no legacy-code change, zero behavior difference.
      (Confirmed by direct reading: `Test_Scenarios.pf` (which runs
      `history_wildcard` via its own `ScenarioDescription` table) never
      calls `set_graph_mode`, so `graph_mode()` defaults to
      `GRAPH_MODE_REGISTRY` per `GraphMode.F90`'s own documented
      default - the legacy-coupler-only path, untouched by this
      change's own `GraphBuilder_ConnectionResolution_smod.F90` edits.
      Also confirmed empirically: `MAPL.generic.scenarios`'s own 330-
      test run, including `history_wildcard`'s own `Test_Scenarios.pf`
      coverage, passes unchanged under both compilers.)

## 5. Regression and verification

- [x] 5.1 Build MAPL (NAG, per the active compiler-switching skill)
      with all modified files included - clean build, zero warnings/
      errors introduced by this change's own files. Also build under
      gfortran and confirm identically clean.
      (`cmake --build nag --target MAPL.generic -j8` and `cmake --build
      gfortran --target MAPL.generic -j8` both clean, no warnings/
      errors from this change's own files - only pre-existing,
      unrelated style warnings elsewhere in the tree.)
- [x] 5.2 Run `ctest -R "^MAPL.generic.graph$"` - 0 failures, both
      compilers.
      (NAG: 1/1 Passed, 0.28s. gfortran: 1/1 Passed, 0.37s.)
- [x] 5.3 Run `ctest -R "^MAPL.generic"` (full `MAPL.generic.*` label
      set) - all suites pass, zero regressions, both compilers, paying
      particular attention to `MAPL.generic.scenarios` (`history_
      wildcard`'s own existing legacy-mode run, task 4.4) and any
      `WILDCARD`-adjacent fixture.
      (NAG: 8/8 Passed (37.55s total), `MAPL.generic.scenarios` 34.75s
      including `history_wildcard`'s own `Test_Scenarios.pf` run and
      the new `test_history_wildcard_composite_matches_legacy_coupler`.
      gfortran: 8/8 Passed (31.46-31.51s total, re-run twice to rule
      out flakiness after one transient environment-induced segfault
      from chaining `module purge`+`module load` in a single command -
      not reproducible on a clean `module load`, confirmed by two
      consecutive clean passes), `MAPL.generic.scenarios` 26.45s. Zero
      regressions either compiler.)
- [x] 5.4 Confirm no existing non-`WILDCARD` connection in any current
      fixture is affected by the Section 2 gate narrowing (every
      existing `Test_Scenarios`/`graph-scenarios` fixture still passes
      unchanged - the narrowing only removes exclusion for `WILDCARD`,
      leaving `SERVICE`'s own gate unaffected).
      (Confirmed by 5.3's own full-suite results: every pre-existing
      `Test_Scenarios.pf`/`graph-scenarios` fixture (none of which
      declare a `WILDCARD` import except `history_wildcard`) passes
      unchanged, both compilers. `test_resolved_expression_no_longer_
      excluded_from_gate`'s own pre-existing `SERVICE`-stays-excluded
      negative control, and `test_wildcard_no_longer_excluded_from_
      gate`'s own companion `SERVICE` negative control, both still
      pass - confirming the gate narrowing is WILDCARD-specific as
      intended.)
- [x] 5.5 Run `openspec validate phase-7c-wildcard-dispatch-wiring
      --strict` - change is valid.
      (`openspec validate phase-7c-wildcard-dispatch-wiring --strict` ->
      "Change 'phase-7c-wildcard-dispatch-wiring' is valid".)
