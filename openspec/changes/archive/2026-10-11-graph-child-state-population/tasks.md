## 1. GraphBuilder.F90: fix the cross-component destination state

- [x] 1.1 In `graphbuilder_populate_state`'s existing cross-component
      loop (`do i = 1, n` over `this%get_num_children()`), look up each
      child's own `OuterMetaComponent` once per iteration via
      `get_child_outer_meta(child_name, rc)` (design.md D1/D3 - already
      exists, already used identically by
      `graphbuilder_resolve_geometry`).
- [x] 1.2 From that child's own `OuterMetaComponent`, build the child's
      own destination `MultiState` via
      `child_meta%get_user_gc_driver()%get_states()` (design.md D3).
- [x] 1.3 Pass this child's own `MultiState` (not the procedure's own
      `multi_state` argument) to `populate_one` for every `var_spec`
      resolved against that child's own `proxy_key` lookup, for both
      IMPORT and EXPORT items (design.md D2/D4 - no `populate_one`
      signature change; only which `MultiState` value the
      cross-component loop passes in changes).
- [x] 1.4 Leave the same-component (degenerate) branch above the
      cross-component loop unchanged - it keeps using the procedure's
      own `multi_state` argument (design.md D1).
- [x] 1.5 Add a short comment at the cross-component loop's own call
      site explaining why its destination state differs from the
      degenerate branch's (design.md Risks - avoid a future reader
      assuming both branches share one destination-resolution
      mechanism).
- [x] 1.6 **Found during implementation (design.md D1-D6, idempotency
      correction):** `populate_one`'s original bare `ESMF_StateAdd`
      fails outright on `GENERIC_INIT_PHASE_SEQUENCE`'s second
      `GENERIC_INIT_ACCEPT_TRANSFER` pass, since the cross-component
      case's destination is now a child's own PERSISTENT real state
      (unlike the parent's own fresh-per-call scratch `MultiState`
      before this change). Fixed in two parts: (a) `populate_one` now
      threads `short_name` through from each call site's own
      already-available `var_spec%short_name`, builds
      `alias = MAPL_NamedAlias(field, name=short_name, _RC)`
      (`mapl_FieldNamedAlias_mod`, matching legacy's own
      `FieldClassAspect%add_to_state` precedent exactly), and inserts
      `alias` via `ESMF_StateAddReplace` instead of the raw field via
      a bare `ESMF_StateAdd` - ESMF matches/replaces purely by name,
      and the chain's own final item has no explicit name
      (`try_materialize_field_extension`'s mismatch-branch call site
      never passes `name=`), so inserting the raw field could never
      reliably re-match itself on a second pass. (b) Confirmed by
      direct reading that `materialize_field_extension` is already
      idempotent at the node-payload level ("already materialized -
      no-op"), so the underlying field object is genuinely identical
      across both passes - only the STATE ENTRY's own name identity
      needed fixing, not the field materialization itself.

## 2. Unit coverage (synthetic graph, no real driver run)

- [x] 2.1 Add a new pFUnit test to `Test_GraphBuilder.pf`, mirroring
      `test_exact_match_materializes_field_in_graph_mode`'s own
      synthetic parent/two-children construction style
      (`make_outer_meta`/`add_child`/`ChildSpec`): build a parent with
      two children (`child_src` export, `child_dst` import, same short
      name, exact match), declare an ordinary `MatchConnection` between
      them, resolve under `GRAPH_MODE_GRAPH`, call
      `gb%populate_state(parent_meta, multi_state, status)` with the
      parent's own scratch `MultiState`, then assert:
      - `child_src`'s own `get_user_gc_driver()%get_states()%exportState`
        contains the resolved field under its own short name
        (`ESMF_StateGet`)
      - `child_dst`'s own `get_user_gc_driver()%get_states()%importState`
        contains the resolved field under its own short name
      - the parent's own `multi_state` (passed into `populate_state`)
        contains NEITHER field - confirms the fix, not just that
        population happened somewhere (design.md D6).
- [x] 2.2 Add a second assertion (same test or a follow-up one)
      confirming the two children's own states received the SAME field
      object the export side materialized (`FieldsAreAliased`,
      matching `test_populate_state_aliases_export_and_skips_unresolved`'s
      own existing precedent) - not merely two independently-created
      fields that happen to compare equal.

## 3. YAML-configured demonstration scenario

- [x] 3.1 Attempt running the existing
      `superstructure/generic/tests/graph-scenarios/units_and_typekind_mismatch/`
      fixture unmodified under `GRAPH_MODE_GRAPH` (design.md D5) -
      confirm by direct experiment whether its existing
      `cap.yaml`/`comp_src.yaml`/`comp_dst.yaml` shape, run through the
      standard `GENERIC_INIT_PHASE_SEQUENCE`, now succeeds (no
      `ESMF_RC_ARG_INCOMP`). **Confirmed for the static (init-time-only)
      case** - no `ESMF_RC_ARG_INCOMP`, structurally correct. **Could
      not be used for the full run-phase demonstration** (design.md
      D8's own gap 1): its two-characteristic chain has an intermediate
      node `try_materialize_field_extension` never materializes, which
      only becomes visible once a real run phase actually pulls on the
      chain - never attempted by this fixture's own
      `has_run_phase=.false.` static-only coverage before this change.
- [x] 3.2 **Design correction (design.md D8):** switched to a new,
      distinctly-named, single-characteristic fixture
      (`units_mismatch_run_phase/`, `units` only) whose one-hop chain
      has no distinct intermediate node to expose gap 1 on - avoids
      needing to change `ExtensionResolution.F90`'s own chain-building
      interface (out of scope per design.md Non-Goals). Also required
      a one-line fixture bug fix, found during implementation: the
      original `units_and_typekind_mismatch/comp_src.yaml`'s own
      `run: T: [10.]` used a float literal, but
      `ConfigurableGridComp.F90`'s own `run()` parses `run:` values via
      `esmf_HConfigAsI4MapVal` (integer-only) - every other `run:`
      fixture in the repo already uses a plain integer; `10.` fails
      outright the first time anything actually executes a real run
      phase against it (never exercised before this change). Fixed in
      both fixtures (`[10.]` -> `[10]`).

## 4. GraphBuilder.F90: re-propagation pull after a run phase (design.md D7, new scope)

- [x] 4.1 **Found during implementation of the demonstration test
      (section 5 below):** `graphbuilder_populate_state` only ever
      runs once, at `GENERIC_INIT_ACCEPT_TRANSFER` (init time) -
      `comp_src.yaml`'s own `run: T: [10]` section only writes `10`
      when `GENERIC_RUN_USER` actually executes, strictly after init,
      so nothing re-populates `comp_dst`'s own import state afterward.
      Added `graphbuilder_run_pull_hook(this, rc)` to `GraphBuilder.F90`'s
      existing procedure family: no-op when
      `graph_mode() /= GRAPH_MODE_GRAPH`; otherwise, for each of
      `this`'s own declared children (the same
      `get_num_children`/`get_child_name`/`get_child_component_spec`
      enumeration `graphbuilder_populate_state`'s own cross-component
      loop already uses), for each child's own advertised IMPORT
      `var_spec`, resolves `proxy_id = graph%get_resource_index(
      proxy_key(child_name, ESMF_STATEINTENT_IMPORT, var_spec%
      short_name))` and calls `graph%update(graph%
      get_default_network_id(), proxy_id, rc=status)` directly -
      design.md D7's own architectural guarantee (REQ-NODE-007:
      `ComponentGraph%update()` never invokes a method node or a
      `GriddedComponentDriver%run()`) is what makes this safe; no
      `MethodGraphNode` construction or invocation anywhere in it.
- [x] 4.2 Called this new hook from
      `superstructure/generic/OuterMetaComponent/run_user.F90`,
      immediately after the existing
      `call this%user_gc_driver%run(phase_idx=phase, _RC)` line
      (design.md D7 - `cap`'s own `run_user.F90` invocation is what
      actually runs `comp_src` then `comp_dst` via
      `ConfigurableGridComp`'s own `run_children_phases_`, so pulling
      immediately afterward is the first point both children have
      already run).
- [x] 4.3 Confirmed by direct reading (not new code) that no second
      `populate_one`/`ESMF_StateAddReplace` call is needed: the
      cross-component fix (section 1) already inserts a
      `MAPL_NamedAlias` sharing the chain's final field's own
      underlying array, so re-executing the chain via `graph%update()`
      would update that same array in place, IF the chain's own input
      were itself correctly connected to the real run-phase write -
      which design.md D8's own gap 3 (not fixed in this change) shows
      it is not yet, for the export side.
- [x] 4.4 **Found during implementation (design.md D8, item 2), not in
      the original plan:** `resolve_one`'s own mismatch branch never
      materialized the chain's own INPUT node (`export_node_id`) at
      all - only the exact-match branch and the chain's final node
      ever were. Fixed in `GraphBuilder.F90`'s `resolve_one`: the
      mismatch branch now also calls `try_materialize_field_extension`
      on `export_node_id`, passing `export_var_spec` for both the
      export and import `VariableSpec` arguments (that procedure's own
      contract always materializes using the second argument's
      typekind/units - passing `export_var_spec` twice is what
      correctly resolves to the export's own values for this node, as
      opposed to the chain's final node, which correctly keeps using
      the import's own values via the existing, unchanged call).

## 5. Demonstration test

- [x] 5.1 Added a new pFUnit test file,
      `Test_GraphModeDemonstration.pf`: creates the `cap` GridComp
      from the `units_mismatch_run_phase` fixture (section 3), sets
      `GRAPH_MODE_GRAPH` (not `BOTH`), runs the full
      `GENERIC_INIT_PHASE_SEQUENCE`, performs a real
      `ESMF_GridCompRun(cap_gc, phase=GENERIC_RUN_USER, ...)` call
      while `GRAPH_MODE_GRAPH` is still active (exercises the new pull
      step, section 4, with no crash), then resets the graph mode to
      `GRAPH_MODE_REGISTRY` (matching
      `Test_GraphEquivalenceHarness.pf`'s own existing ordering
      convention).
- [x] 5.2 **Design correction (design.md D8's own "Final scope"):**
      asserts `comp_dst`'s own real import `ESMF_State` (reached via
      `cap_meta%get_child('comp_dst')%get_gridcomp()` ->
      `get_outer_meta` -> `get_user_gc_driver()%get_states()%importState`,
      the same reach `Test_GraphEquivalenceHarness.pf`'s own `check()`
      already uses) contains field `T` with the fixture's own
      documented expected typekind (R4) and units (Pa), and that the
      export proxy node has a real, allocated field (confirms
      design.md D8 item 2's own fix structurally). Does NOT assert a
      propagated numeric value (design.md D8 item 3, not fixed in this
      change - the export proxy's own field is confirmed real but not
      yet aliased to `comp_src`'s own real export field, so a run
      phase's changed value does not yet reach it).
- [x] 5.3 Asserts the legacy `StateRegistry` path was never the real
      producer for this run: `comp_dst`'s own import `ESMF_State` has
      exactly one entry for `T` (no duplicate-insertion check needed
      the way `BOTH` mode's harness requires, since legacy's own
      `add_to_states` is unconditionally skipped under
      `GRAPH_MODE_GRAPH` - confirmed by reading
      `initialize_accept_transfer.F90`'s own existing
      `if (graph_mode() /= GRAPH_MODE_GRAPH)` guard, not by adding new
      production code).

## 6. Regression and verification

- [x] 6.1 Build and run `ctest -R
      "MAPL.generic.aspects$|MAPL.generic.core$|MAPL.generic.components$|MAPL.generic.graph$"`
      - confirmed: all pass, no existing test's expectations changed.
- [x] 6.2 Run `Test_GraphEquivalenceHarness.pf`'s existing
      `units_and_typekind_mismatch` case (its own `BOTH`-mode coverage)
      unmodified - confirmed: still passes, proving this fix's new
      per-child lookup logic is never reached under `BOTH` mode
      (design.md Risks). (Its own `comp_src.yaml` `run:` value was
      corrected from `[10.]` to `[10]` per task 3.2's own found fixture
      bug, but that fixture's own `has_run_phase=.false.` means this
      value is never actually read by the harness - confirmed by its
      own continued pass.)
- [x] 6.3 Run the full `Test_Scenarios.pf` suite (the ~30-fixture
      real-YAML regression suite) unmodified under the default
      (`registry`) graph mode - confirmed: no existing fixture's
      expected output changed (part of the same `MAPL.generic.scenarios`
      ctest run as 6.1/6.2).
- [x] 6.4 Run the new tests from sections 2 and 5 and confirm they
      pass. Confirmed: all pass.
- [x] 6.5 `openspec validate graph-child-state-population --strict`
      reports the change valid.
