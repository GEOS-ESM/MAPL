## 1. Preparation

- [x] 1.1 Confirm current build is clean (gfortran) before any change - baseline for comparison, per `compiler-switching`/`mapl-build` skills.
- [x] 1.2 Record the current full procedure list and section mapping (design.md's own table) as a checklist - every one of the 57 procedures (design.md's corrected count, including the 4 found missing from the original 53-count analysis during this task) must be accounted for exactly once by the end of this change.
- [x] 1.3 Confirm no other file in the repo declares `submodule (mapl_GraphBuilder_mod)` already (should be none - `GraphBuilder` has no existing submodule split) before adding new ones.

## 2. Ancestor module restructuring

- [x] 2.1 Move design.md Decision D2's shared helpers (`item_key`, `proxy_key`, `component_for`, `component_spec_for`, `get_or_make_local_node_id`, `try_materialize_field_extension`, `itemtype_name`, `resolve_callback_import`, `resolve_callback_destination`, `is_self`) to a new "Shared helpers" section near the top of `GraphBuilder.F90`, ahead of the `type :: GraphBuilder` declaration - no signature or body change, position only.
- [x] 2.2 Confirm `type :: GraphBuilder` and `type :: QualifiedExportEntry` stay exactly where they are (design.md D3) - no move.
- [x] 2.3 Add the `module function`/`module subroutine` interface block for every procedure that WILL move to a submodule (sections 3-8 below), grouped and commented by section exactly as design.md D3/D1's table specifies - bodies not yet moved; this step only adds declarations, so the file must still compile standalone with the (temporarily duplicate) old bodies still present. Verify by building before deleting any old body.
- [x] 2.4 Once 2.3's interface block is confirmed to compile, remove the now-interface-only procedures' bodies from the ancestor file (they will be re-added as submodule bodies in sections 3-8) - do this section-by-section, not all at once, so each removal can be immediately paired with its own submodule addition and build-verified.

## 3. Advertising submodule

- [x] 3.1 Create `superstructure/generic/GraphBuilder_Advertising_smod.F90`: `submodule (mapl_GraphBuilder_mod) GraphBuilder_Advertising_smod`, containing `graphbuilder_advertise` and `advertise_one` as `module subroutine` bodies (per design.md D1's table) - move the bodies and their own existing comments verbatim, change only the `subroutine`/`end subroutine` keywords to `module subroutine`/`end subroutine` (or add `module` as needed per Fortran submodule syntax) and add whatever `use` statements these two procedures actually need (check each reference, do not blanket-copy the ancestor's full `use` list).
- [x] 3.2 Add the new file to `superstructure/generic/CMakeLists.txt`'s source list, immediately after `GraphBuilder.F90`.
- [x] 3.3 Build `MAPL.generic`; fix any missing `use`/visibility error before proceeding.
- [x] 3.4 Run `Test_GraphBuilder.pf` (and any other test exercising `graphbuilder_advertise`/`advertise_one` directly); confirm no change in pass/fail status versus baseline.

## 4. ConnectionResolution submodule

- [x] 4.1 Create `GraphBuilder_ConnectionResolution_smod.F90` with `graphbuilder_check_unsatisfied_imports`, `for_each_matching_import`, `check_match_connection_unsatisfied`, `graphbuilder_resolve_connections`, `resolve_match_connection`, `build_characteristics`, `resolve_inout_destination`, `find_export_var_spec`, `find_inout_borrower_var_spec`, `find_export_node_id`, `try_alias_export_field` - same per-procedure `use`-statement discipline as 3.1.
- [x] 4.2 Add to CMakeLists.txt; build; fix any error.
- [x] 4.3 Run `Test_GraphBuilder.pf`, `Test_GraphBuilderEquivalence.pf`; confirm no change versus baseline.

## 5. PortsAndProxies submodule

- [x] 5.1 Create `GraphBuilder_PortsAndProxies_smod.F90` with `resolve_export_geom`, `resolve_export_vgrid`, `get_or_make_local_node_id`'s own section-local siblings (confirm against design.md's table: `component_spec_for`/`component_for`/`try_materialize_field_extension`/`itemtype_name`/`get_or_make_local_node_id` already moved to the ancestor's shared-helpers section in task 2.1 - this submodule, per the table, is left with none of its own section-exclusive procedures beyond `resolve_export_geom`/`resolve_export_vgrid`; confirm this against the design.md table directly before writing the file, since every one of this section's other procedures turned out to be a cross-called shared helper).
- [x] 5.2 Add to CMakeLists.txt; build; fix any error.
- [x] 5.3 Run `Test_GraphBuilder.pf`; confirm no change versus baseline.

## 6. Geometry submodule

- [x] 6.1 Create `GraphBuilder_Geometry_smod.F90` with `graphbuilder_run_geometry_hook`, `graphbuilder_advertise_geometry`, `parent_geometry_proxy_key`, `graphbuilder_resolve_geometry`, `resolve_geometry_from_child`, `push_geometry_to_child`.
- [x] 6.2 Add to CMakeLists.txt; build; fix any error.
- [x] 6.3 Run `Test_GraphGeometryHook.pf` and any other geometry-hook test; confirm no change versus baseline.

## 7. Freeze, StatePopulation, RunPhasePull submodules

- [x] 7.1 Create `GraphBuilder_Freeze_smod.F90` with `graphbuilder_freeze`.
- [x] 7.2 Create `GraphBuilder_StatePopulation_smod.F90` with `graphbuilder_populate_state`, `populate_one`, `get_field_if_materialized`, `find_materialized_predecessor`.
- [x] 7.3 Create `GraphBuilder_RunPhasePull_smod.F90` with `graphbuilder_run_pull_hook`.
- [x] 7.4 Add all three to CMakeLists.txt; build; fix any error.
- [x] 7.5 Run `Test_GraphModeDemonstration.pf`, `Test_GraphEquivalenceHarness.pf`; confirm no change versus baseline.

## 8. Visualization, CallbackWiring, RunPhaseNode, LifecycleHooks submodules

- [x] 8.1 Create `GraphBuilder_Visualization_smod.F90` with `graphbuilder_build_label_map`.
- [x] 8.2 Create `GraphBuilder_CallbackWiring_smod.F90` with `build_qualified_export_namespace`, `collect_qualified_exports`, `resolve_descendant`, `resolve_qualified_export_node_id`, `export_conforms_to_interface`, `materialize_callback_collection`, `build_callback_state_binding`, `build_callback_method_binding`, `wire_callback_argument`, `invoke_callback_method`, `pull_callback_get_arguments`, `advance_callback_put_arguments` (`resolve_callback_import`/`resolve_callback_destination`/`is_self` already moved to ancestor shared helpers, task 2.1 - confirm against design.md table before writing).
- [x] 8.3 Create `GraphBuilder_RunPhaseNode_smod.F90` with `graphbuilder_build_run_phase_node`.
- [x] 8.4 Create `GraphBuilder_LifecycleHooks_smod.F90` with `graphbuilder_run_advertise_hook`, `graphbuilder_run_activate_hook`, `graphbuilder_run_connect_hook`, `graphbuilder_run_populate_state_hook`, `assert_converged`, `report_if_failed`.
- [x] 8.5 Add all four to CMakeLists.txt; build; fix any error.
- [x] 8.6 Run the full test suite for every pFUnit file referencing `mapl_GraphBuilder_mod` (`Test_GraphBuilder.pf`, `Test_GraphBuilderEquivalence.pf`, `Test_GraphModeDemonstration.pf`, `Test_GraphEquivalenceHarness.pf`, `Test_GraphGeometryHook.pf`, `Test_Scenarios.pf`, any other match found by `grep -rl mapl_GraphBuilder_mod --include=*.pf`); confirm no change versus baseline.

## 9. Final verification

- [x] 9.1 Confirm `GraphBuilder.F90` now contains only: module header comment, `use` statements it still needs, shared helpers (task 2.1's list), `type :: GraphBuilder` + bindings, `type :: QualifiedExportEntry`, and the interface block - no stray procedure body left behind.
- [x] 9.2 Cross-check the final file set against design.md's own table - every one of the 57 original procedures accounted for in exactly one file (ancestor or exactly one submodule), none duplicated, none missing.
- [x] 9.3 Rebuild and run the complete `MAPL.generic`/`MAPL.generic.scenarios`/`MAPL.generic.components` test suites (not just the `GraphBuilder`-specific subset) to catch any indirect regression.
- [x] 9.4 Build under NAG and, if available, Intel/ifx (per `compiler-switching` skill) - not just gfortran - since `ComponentGraph`'s own precedent found a real, compiler-specific (ifx) submodule issue that gfortran/NAG did not surface; confirm this split hits no analogous issue (expected: none, since no new module-level type is introduced per design.md D3, but confirm by building, not by assumption).
- [x] 9.5 Diff the full set of moved procedure bodies against their pre-change originals (e.g. `git diff --word-diff` restricted to body content, ignoring the mechanical `subroutine`->`module subroutine` keyword change and file-move) to confirm no accidental logic change crept in during the move.
