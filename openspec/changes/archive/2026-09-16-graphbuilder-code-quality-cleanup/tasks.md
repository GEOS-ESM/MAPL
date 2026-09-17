## 1. Loop idiom conversion

- [x] 1.1 Convert `graphbuilder_advertise`'s loop over `comp_spec%var_specs`
      to `ftn_begin()/ftn_end()` with `call iter%next()` at the top.
- [x] 1.2 Convert `check_match_connection_unsatisfied`'s loop over
      `dst_spec%var_specs` (will be superseded by task 2.1's extracted
      helper, but land as its own mechanical step first for a clean diff
      history — or fold directly into 2.1 if cleaner in practice).
- [x] 1.3 Convert `graphbuilder_check_unsatisfied_imports`'s loop over
      `comp_spec%connections`.
- [x] 1.4 Convert `resolve_match_connection`'s loop over
      `dst_spec%var_specs` (same note as 1.2 re: task 2.1).
- [x] 1.5 Convert `graphbuilder_resolve_connections`'s loop over
      `comp_spec%connections`.
- [x] 1.6 Re-run `Test_GraphBuilder.pf` and
      `Test_GraphBuilderEquivalence.pf`; confirm no behavior change.

## 2. Flatten nesting in ordinary-connection resolution

- [x] 2.1 Extract a shared private helper (e.g.
      `for_each_matching_import`) that iterates `dst_spec%var_specs`,
      guard-clauses past non-`IMPORT` and non-matching entries (using the
      `ftn_begin/ftn_end` idiom from Task 1), and invokes a per-match step
      for each remaining `var_spec`.
- [x] 2.2 Rewrite `check_match_connection_unsatisfied` on top of the
      shared helper: per-match step checks `var_specs_has_export` and
      pushes to `unresolved` when absent.
- [x] 2.3 Rewrite `resolve_match_connection` on top of the shared helper:
      per-match step resolves the node ids, adds the dependency edge when
      an export exists, else pushes to `unresolved`.
- [x] 2.4 Confirm both procedures are now at most 1-2 levels of nesting
      inside the shared helper's callback.
- [x] 2.5 Re-run `Test_GraphBuilder.pf` and
      `Test_GraphBuilderEquivalence.pf`; confirm no behavior change,
      including the unresolved-imports scenarios.

## 3. Child-spec/child-graph accessors on OuterMetaComponent

- [x] 3.1 Add `get_child_component_spec(this, child_name, rc)` to
      `mapl_OuterMetaComponent_mod` (interface in `OuterMetaComponent.F90`,
      implementation as a new submodule file under `OuterMetaComponent/`,
      following the `get_component_spec.F90`/`get_component_graph.F90`
      one-file-per-procedure pattern), doing the same
      `get_child()->get_gridcomp()->get_outer_meta()->get_component_spec()`
      chain `get_child_meta` does today.
- [x] 3.2 Add `get_child_component_graph(this, child_name, rc)` the same
      way, ending in `get_component_graph()`.
- [x] 3.3 Document both procedures inline as framework-internal
      (REQ-GB-002-style carve-out), not part of `OuterMetaComponent`'s
      general public API, and confirm neither is added to any user-facing
      re-export/aggregator list.
- [x] 3.4 In `GraphBuilder.F90`, replace `component_spec_for`'s and
      `get_or_make_local_node_id`'s use of `get_child_meta(...)
      %get_component_spec()/%get_component_graph()` with the new direct
      accessors.
- [x] 3.5 Delete `get_child_meta` and the now-unused
      `use mapl_GriddedComponentDriver_mod, only: GriddedComponentDriver`
      import from `GraphBuilder.F90`.
- [x] 3.6 Re-run `Test_GraphBuilder.pf` and
      `Test_GraphBuilderEquivalence.pf`; confirm no behavior change.

## 4. GraphBuilder as a type with type-bound procedures

- [x] 4.1 Define `type :: GraphBuilder` (empty, no components) in
      `GraphBuilder.F90`, replacing the current 7 free public procedures
      (`graphbuilder_advertise`, `graphbuilder_check_unsatisfied_imports`,
      `graphbuilder_resolve_connections`, `graphbuilder_freeze`,
      `graphbuilder_run_advertise_hook`, `graphbuilder_run_activate_hook`,
      `graphbuilder_run_connect_hook`) with type-bound procedures of the
      same name minus the `graphbuilder_` prefix (`advertise`,
      `check_unsatisfied_imports`, `resolve_connections`, `freeze`,
      `run_advertise_hook`, `run_activate_hook`, `run_connect_hook`),
      each still taking `this => OuterMetaComponent` as its first
      explicit argument. `item_key`/`proxy_key` remain free functions,
      unchanged.
- [x] 4.2 Update `initialize_advertise.F90` to construct a `GraphBuilder`
      value and call `gb%run_advertise_hook(this)` /
      `gb%run_activate_hook(this)`.
- [x] 4.3 Update `initialize_accept_transfer.F90` to construct a
      `GraphBuilder` value and call `gb%run_connect_hook(this)`.
- [x] 4.4 Update `Test_GraphBuilder.pf` call sites
      (`graphbuilder_advertise`, `graphbuilder_check_unsatisfied_imports`,
      `graphbuilder_resolve_connections`, `graphbuilder_freeze`) to the
      type-bound form.
- [x] 4.5 Update `Test_GraphBuilderEquivalence.pf` call sites the same
      way.
- [x] 4.6 Re-run `Test_GraphBuilder.pf` and
      `Test_GraphBuilderEquivalence.pf`; confirm no behavior change.

## 5. Final verification

- [x] 5.1 Full `mapl_generic` test suite build + run (not just the two
      GraphBuilder test files) to catch any missed call site.
- [x] 5.2 Confirm `mapl_GraphBuilder_mod`'s public list is now
      `GraphBuilder`, `item_key`, `proxy_key` only.
- [x] 5.3 Re-read `GraphBuilder.F90` module header comment (lines 1-75)
      for any statements that describe the old free-function/`get_child_meta`
      shape and update them to match the new structure.
