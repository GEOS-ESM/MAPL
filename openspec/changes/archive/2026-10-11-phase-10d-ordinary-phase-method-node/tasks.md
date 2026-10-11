## 1. OuterMetaComponent: store the constructed node id

- [x] 1.1 Add a new private `type(NodeId) ::
      run_phase_method_node_id = INVALID_NodeId` field to
      `OuterMetaComponent` (`superstructure/generic/OuterMetaComponent.F90`),
      per design.md D1. Not `allocatable` - a `NodeId` carries its own
      built-in "unset" sentinel (`INVALID_NodeId`/`is_valid()`).
- [x] 1.2 Add an accessor (`get_run_phase_method_node_id`, returns
      `type(NodeId)` **by value**, no pointer) and a setter (asserts
      `.not. this%run_phase_method_node_id%is_valid()` before storing)
      used only by `GraphBuilder.F90`'s construction step (section 2) -
      per design.md D1, mirrors `BaseGraphNode%get_node_id`/
      `CallbackMethodBinding%get_method_node_id`'s own "return a plain
      NodeId by value" precedent, not `get_vertical_grid`'s
      pointer-to-a-polymorphic-field precedent and not the
      container-lookup pointer idiom
      (`ComponentGraph%get_resource_index`) either - neither applies to
      a bare scalar field.

## 2. GraphBuilder.F90: run-phase node construction

- [x] 2.1 Add `graphbuilder_build_run_phase_node(this, rc)` as a new
      private procedure in `GraphBuilder.F90`'s existing submodule
      family, following `build_callback_method_binding`'s own
      construction shape (design.md D2/D3). Header comment states
      explicitly, as the first thing a reader sees: this constructs a
      node with a real, driver-targeting adapter attached (D4) but
      never invokes it - invocation is this change's own named
      Non-Goal (proposal.md Why), blocked on a legacy-run-call-skip
      mechanism and `GRAPH_MODE_GRAPH`-alone population, neither of
      which exists yet. Do not add an invocation call site against this
      node without resolving both first.
- [x] 2.2 Guard the entire procedure as a no-op (return immediately,
      `rc` success) when `graph_mode() == GRAPH_MODE_REGISTRY`.
- [x] 2.3 Idempotent re-entry: if
      `this%get_run_phase_method_node_id()%is_valid()` is already
      true, return immediately (success) - mirrors `advertise_one`'s
      own "already indexed -> nothing to do" guard, since
      `graphbuilder_run_connect_hook` (this procedure's own caller) may
      run more than once per the fixed two-pass
      `GENERIC_INIT_PHASE_SEQUENCE` schedule.
- [x] 2.4 Iterate `this%get_component_spec()%var_specs` (the same
      `VariableSpecVector` `graphbuilder_advertise` already walks),
      filtering on `state_intent`; for each IMPORT item resolve its
      `NodeId` via `graph%get_resource_index(item_key(var_spec%
      state_intent, var_spec%short_name))` and call
      `declare_argument(name, MAPL_ACCESS_IN, rc)` then
      `bind_argument(name, member_id, rc)`; for each EXPORT item use
      `MAPL_ACCESS_OUT`. Skip INTERNAL items. No `expected_kind`/
      `actual_kind` (design.md D3 - `VariableSpec` has no
      `MAPL_StateItem_Flag`-typed field to source one from). Do not use
      `ComponentGraph%get_import_port`/`get_export_port` - those are
      point lookups (`PortId -> NodeId`), not an enumeration.
- [x] 2.5 Construct the node's invocation adapter (design.md D4): a
      `GridCompMethodInvocation(SELF_COMPONENT_NAME, phase_idx=1,
      invoker)` where `invoker` is a `GridCompDriverPhaseInvoker` built
      from `OuterMetaComponentDriverResolver(this)` and
      `ESMF_METHOD_RUN` - matching Phase 4b's existing constructor
      signatures exactly (no new adapter/resolver type). Attached to
      the node at construction (`MethodGraphNode(method_node_id,
      adapter)`) even though nothing in this change ever calls
      `invoke()` on it (design.md D4's own rationale: an adapter-less
      node would be permanently uninvocable regardless of future
      wiring).
- [x] 2.6 `call graph%register_node(method_node, rc)` as the final
      step, mirroring `build_callback_method_binding`'s own last step;
      store the resulting `NodeId` via the section-1 setter.
- [x] 2.7 Call `graphbuilder_build_run_phase_node` from
      `graphbuilder_run_connect_hook`'s own body immediately AFTER
      `graphbuilder_resolve_connections` returns and BEFORE
      `graphbuilder_freeze` runs (design.md D2 - placing it after
      freeze would hard-fail unconditionally, since
      `ComponentGraph%register_node`/`next_node_id` both
      `_ASSERT(.not. this%is_frozen())`). Guarded the same way the
      hook's existing steps already are (`report_if_failed`, never
      breaks real init under `registry` mode default).

## 3. Unit coverage (synthetic graph, no real driver run)

- [x] 3.1 Add a new pFUnit test to `Test_GraphBuilder.pf` (or a new,
      narrowly-scoped `.pf` file alongside it) using the existing
      `make_outer_meta`/`make_var` synthetic-component helpers (no real
      `GriddedComponentDriver%run()` call anywhere in this test,
      mirroring every other construction-only test in that file):
      build a component declaring at least one IMPORT and one EXPORT
      `VariableSpec`, advertise it (`gb%advertise`), set a non-default
      graph mode, call `gb%build_run_phase_node(outer_meta, status)`,
      and assert: `status == 0`; exactly one `MethodGraphNode` exists
      in the component's graph (resolve via
      `get_run_phase_method_node_id()%is_valid()` then
      `graph%get_node(node_id)`, `select type` on `MethodGraphNode`);
      the node's declared arguments match the IMPORT/EXPORT short
      names with the correct access modes
      (`node%get_argument(name)%get_access()`); the node's bindings
      resolve to the same `NodeId`s `graph%get_resource_index` already
      has for those items.
- [x] 3.2 Add a second test asserting the `GRAPH_MODE_REGISTRY` no-op:
      call `gb%build_run_phase_node(outer_meta, status)` under the
      default graph mode and assert `get_run_phase_method_node_id()`
      remains `INVALID_NodeId` (`.not. is_valid()`) afterward.
- [x] 3.3 Add a third test asserting idempotency: call
      `gb%build_run_phase_node` twice under a non-default graph mode
      and assert the second call leaves `get_run_phase_method_node_id()`
      unchanged (same `NodeId` value) and does not fail.

## 4. Regression and verification

- [x] 4.1 Build and run `ctest -R
      "MAPL.generic.aspects$|MAPL.generic.core$|MAPL.generic.graph$"` -
      confirm no existing test's expectations change.
- [x] 4.2 Run the full `Test_Scenarios.pf` suite (the ~30-fixture
      real-YAML regression suite) unmodified under the default
      (`registry`) graph mode - confirm no existing fixture's expected
      output changes (the real-world check that this change's new
      no-op guard (2.2) holds for every production fixture).
- [x] 4.3 Confirm by direct reading that
      `OuterMetaComponent/run_user.F90`,
      `Test_GraphEquivalenceHarness.pf`, and the
      `units_and_typekind_mismatch` fixture were NOT modified by this
      change (proposal.md Non-Goals - verification, not
      implementation).
- [x] 4.4 Confirm by direct reading that
      `mapl_MethodInvocation_mod%invoke_on_default_network`,
      `GridCompDriverPhaseInvoker`, and
      `OuterMetaComponentDriverResolver` were not modified by this
      change (design.md Context - reused, not changed).
- [x] 4.5 `openspec validate phase-10d-ordinary-phase-method-node
      --strict` reports the change valid.
