## 1. Shared primitives in GraphBuilder.F90

- [x] 1.1 Add `resolve_qualified_component(root, path, rc)` to
      `GraphBuilder.F90`'s shared-helpers section (CONTAINS, host-
      associated by every submodule - same tier as `component_for`/
      `is_self`): relocate `resolve_descendant`'s own existing body
      (`GraphBuilder_CallbackWiring_smod.F90`) verbatim, with one fix -
      special-case the path's first segment against
      `is_self(root, segment)` (mirroring `component_for`'s own
      existing `is_self` short-circuit) before calling
      `get_child_outer_meta`, so a single-segment path equal to
      `root%get_name()` resolves to `root` itself rather than
      attempting a child lookup (design.md Decisions/Risks).
- [x] 1.2 Add `enumerate_qualified_exports(root, rc)` to the same
      shared section: generalize `collect_qualified_exports`'s own
      recursive descent (same traversal, same `/`-joined path
      construction) to return a plain array of
      `(qualified_path, owner, node_id)` tuples (new, small, local
      derived type - not `QualifiedExportEntry`) instead of
      `QualifiedExportEntry`, resolving each export's `NodeId` via the
      owner's own `ComponentGraph%get_resource_index(item_key(...))`
      rather than carrying a `var_spec` reference (design.md
      Decisions).
- [x] 1.3 Make both new primitives public from `mapl_GraphBuilder_mod`
      (same visibility precedent as `component_for`/`is_self`/
      `item_key` - public for submodule cross-linkage per gfortran's
      own internal-linkage limitation already documented at the top of
      `GraphBuilder.F90`, not part of any user-facing API).

## 2. Rewire callback-wiring to the shared primitives

- [x] 2.1 Replace `resolve_descendant`'s own body
      (`GraphBuilder_CallbackWiring_smod.F90`) with a one-line forward
      to `resolve_qualified_component` - keep the `resolve_descendant`
      entry point itself (its own existing call sites,
      `materialize_callback_collection`/`build_callback_state_binding`,
      need no change).
- [x] 2.2 Confirm `collect_qualified_exports`/`build_qualified_export_namespace`
      are left as-is for this change (design.md Non-Goals - migrating
      them onto `enumerate_qualified_exports` is optional polish, not
      required for this change's own scope; do it only if it is a
      small, low-risk simplification once task 1.2 exists - otherwise
      leave `collect_qualified_exports`'s own existing recursion
      untouched and record that decision in this task's own commit
      message).

## 3. Rewire ordinary connection resolution

- [x] 3.1 In `check_match_connection_unsatisfied`
      (`GraphBuilder_ConnectionResolution_smod.F90`), replace the
      `component_spec_for(this, dst_pt%component_name, _RC)`/
      `component_spec_for(this, src_pt%component_name, _RC)` calls
      with `resolve_qualified_component(this, dst_pt%component_name,
      _RC)%get_component_spec()` / the source-side equivalent.
- [x] 3.2 Make the same replacement in `resolve_match_connection`'s own
      `dst_spec => component_spec_for(...)` call.
- [x] 3.3 Confirm `find_export_node_id`'s own
      `component_spec_for(this, comp_name, _RC)` call (used to reach
      `src_spec` before scanning for the export) is updated the same
      way - or, if `graph-builder-stateitem-resolution` has already
      landed by the time this task runs, confirm its own replacement
      (reading the source `ComponentGraph`'s export ports directly)
      already resolves the component via `resolve_qualified_component`
      rather than `component_spec_for`/`component_for`'s single-level
      dispatch; do not leave a single-level reach in this call path
      regardless of which change's own code is doing the component
      lookup.

## 4. pFUnit coverage

- [x] 4.1 `test_resolve_qualified_component_single_segment_self`: a
      single-segment path equal to the calling component's own name
      resolves to that component itself, not a child lookup attempt
      (design.md Risks - the fixed gap).
- [x] 4.2 `test_resolve_qualified_component_multi_level`: a three-level
      synthetic hierarchy (parent/child/grandchild); a two-segment path
      (`child/grandchild`) resolves to the grandchild's own
      `OuterMetaComponent`.
- [x] 4.3 `test_ordinary_connection_resolves_through_multi_level_names`:
      a `MatchConnection` naming a parent-level child and a sibling
      child, where the real export/import items are advertised on a
      grandchild beneath each named endpoint (the shape every real
      `connect_all` fixture already has) - assert resolution succeeds
      and wires correctly, where it would have failed to find anything
      under the old single-level `component_spec_for` dispatch.
- [x] 4.4 Confirm every existing `Test_GraphBuilder.pf`/callback-wiring
      test case passes unmodified (pure refactor for the single-level
      and callback-wiring cases - no existing assertion should need to
      change).
- [x] 4.5 Register new cases in `MAPL.generic.components`
      (`superstructure/generic/tests/CMakeLists.txt`), alongside the
      existing `Test_GraphBuilder.pf` suite. (New cases added directly
      to `Test_GraphBuilder.pf`, already registered in that CMakeLists
      target - no CMakeLists change was needed.)

## 5. Verification

- [x] 5.1 Build MAPL with NAG (`nag` build dir); run `cmake --build nag
      -j 8` and `build-tests`; confirm no new build regressions.
- [x] 5.2 Run `MAPL.generic.components` and `MAPL.generic.scenarios`
      directly with `-v`; confirm no regression against the pre-change
      baseline.
- [x] 5.3 Run full `ctest` for the `MAPL.generic.*` label set; confirm
      no regression outside the same pre-existing, unrelated
      environment gaps earlier changes' own tasks.md files already
      documented.
- [x] 5.4 `openspec validate graph-builder-resolution-index --strict`
      reports the change valid.
