## 1. `mapl_GraphMode_mod`: 3-way selector

- [x] 1.1 Replace `graph_native` logical/`set_graph_native_enabled`/
      `graph_native_enabled` with a 3-way `registry`/`graph`/`both`
      selector (`set_graph_mode`/`graph_mode`), default `registry`
      (design.md Decisions - representation left to implementer).
- [x] 1.2 Update `graphbuilder_run_geometry_hook`'s own gate
      (`GraphBuilder.F90`) to the new selector (active for `graph` and
      `both`, matching the old flag's `.true.` exactly — no behavior
      change for that hook).
- [x] 1.3 Update `Test_GraphGeometryHook.pf`'s existing setter calls to
      the new API.

## 2. Exact-match-branch field materialization

- [x] 2.1 Add an optional `name` argument to
      `mapl_ExtensionMaterialization_mod::materialize_field_extension`,
      passed through to `FieldCreate(name=name, ...)`; omitted by the
      existing mismatch-branch call site (no behavior change there).
- [x] 2.2 In `GraphBuilder.F90`'s `resolve_one()`, no-mismatch branch:
      call `try_materialize_field_extension` on `export_node_id` (not an
      extension-chain node), passing the export's own `short_name` as
      `name`, gated on the new selector reporting `graph` or `both` (not
      `materialize_extensions_enabled()` — a different, orthogonal gate).
- [x] 2.3 Confirm `try_materialize_field_extension`'s existing failure-
      reporting path (`unsupported_item_class`/`unresolved_geom`) is
      reused as-is for this new call site — same `GraphResolutionEntry`
      reporting as the mismatch branch already has.

## 3. Graph-native state population

- [x] 3.1 Implement a new procedure in `GraphBuilder.F90` (e.g.
      `graphbuilder_populate_state(this, multi_state, rc)`) walking (a)
      `this`'s own `ComponentSpec%var_specs` via `item_key` (degenerate
      same-component case), AND (b) each of `this`'s own declared
      children's `VariableSpec`s
      (`get_num_children`/`get_child_name`/`get_child_component_spec`)
      via `proxy_key(child_name, ...)` in `this`'s OWN graph (design.md
      "Correction, found while writing this sub-change's own pFUnit
      coverage" - `get_or_make_local_node_id` always returns a
      PARENT-local proxy for any non-`<self>` comp_name, so (b) is the
      only walk that finds anything for the ordinary cross-component
      case every real fixture uses). For Field-typed items found either
      way: export side inserts its own (now-materialized) field
      directly; import side walks `network%get_predecessors()` back to
      the first node whose `GraphStateItem%itemType()` is
      `ESMF_STATEITEM_FIELD` (design.md "predecessor-walk" decision),
      skipping silently if no predecessor/proxy exists (unresolved
      import, or never connected).
- [x] 3.2 Insert the resolved field into the passed-in `MultiState`'s
      import or export `ESMF_State` (per the item's own `state_intent`).
      No `name=` override to `ESMF_StateAdd` (it has none) - findable by
      short name only because `materialize_field_extension`'s own `name`
      argument already set it at `FieldCreate` time.
- [x] 3.3 Gate the real `ESMF_State` insert strictly to the selector
      reporting `graph` (never `both` — design.md's "structural, not
      runtime-checked" no-collision decision).
- [x] 3.4 Wire a call site into `initialize_accept_transfer.F90`,
      immediately after `gb%run_connect_hook(this)`, mirroring where
      legacy's own `add_to_states(mode='user', ...)` already runs in
      that file; failures caught/logged internally
      (`report_if_failed`-style), never propagated into
      `initialize_accept_transfer`'s own error path.
- [x] 3.5 Per the graph-mode spec's "`graph` mode makes graph-native
      population the real producer" requirement (legacy population
      SHALL be skipped entirely in `GRAPH_MODE_GRAPH`, not merely run
      alongside graph-native population): wrap both existing
      `this%registry%add_to_states(...)` calls in
      `initialize_accept_transfer.F90` (`mode='user'` and
      `mode='outer'`) in `if (graph_mode() /= GRAPH_MODE_GRAPH)`.
      Design.md addendum: "`GRAPH_MODE_GRAPH` skips legacy's own
      `add_to_states` entirely." Not exercised end-to-end by any
      fixture this phase (consistent with the `graph`-mode Non-Goal) -
      verified via `mapl_GraphMode_mod`'s own unit coverage and code
      review only.

## 4. pFUnit coverage

- [x] 4.1 Exact-match materialization: a synthetic-graph test (style of
      `Test_GraphBuilder.pf`'s existing cases) asserting the export
      node's payload is a real, allocated field after resolution when
      the selector is `graph`/`both`, and still the unallocated
      placeholder when `registry` (spec "Matching export is
      materialized..."/"...is not materialized in registry mode").
- [x] 4.2 Graph-native state population: a direct (non-end-to-end) test
      calling the new population procedure against a hand-built
      `ComponentGraph`/`MultiState` pair, covering: (a) exact-match
      import's real field is the same field object as the export's
      (spec "An exact-match import's real field is found via its
      export"), (b) no insert occurs when mode is `registry` or `both`
      (spec "Graph mode governs..."), (c) an unresolved import produces
      no insert and no error (spec "Unresolved import is not
      populated").
- [x] 4.3 `mapl_GraphMode_mod`: unit coverage of the 3-way selector's
      default value and round-trip set/get for all three values.
- [x] 4.4 Extend `Test_GraphBuilderEquivalence.pf`'s existing exact-match
      fixture (`scenarios/graphbuilder_equivalence/`) with a `both`-mode
      run: legacy's real `ESMF_State` still contains exactly the same
      entries as today (no graph-native insert leaks in), while the
      export node's own payload now reports a real, correctly-shaped
      field (decision parity check, same style as the existing
      units-mismatch test's own field-shape assertions).
- [x] 4.5 **Correction, found while writing task 4.4 (design.md "pre-
      existing geometry-hook phase-timing bug"), not this sub-change's
      own new code**: `graphbuilder_run_geometry_hook`'s production call
      site was wired to the wrong lifecycle phase
      (`initialize_advertise.F90`, before `this%has_geom()` is ever true
      for a `GEOMETRY_PROVIDER` component in the real
      `GENERIC_INIT_PHASE_SEQUENCE`) - exposed only now because turning
      the shared mode selector on against a real, DSO-backed fixture for
      the first time (task 4.4) actually exercises that hook outside
      `Test_GraphGeometryHook.pf`'s own synthetic, lifecycle-bypassing
      tests. Fixed by moving the call to `initialize_geom_b.F90`, after
      that file's own `recurse()` call; corrected the
      `GraphBuilder.F90` module header's own wrong phase-ordering claim
      in the same pass. No change to `Test_GraphGeometryHook.pf` itself
      (still passes unmodified) or to the hook's own internal logic.

## 5. Verification

- [x] 5.1 Build MAPL (NAG, per repo convention) and run the full
      `MAPL.generic.*` ctest label set; no regression in any existing
      case.
- [x] 5.2 Run the new/updated cases from section 4 directly with `-v`;
      confirm all pass.
- [x] 5.3 `openspec validate phase-10a-state-materialization --strict`
      reports the change valid.
