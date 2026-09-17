## 1. OuterMetaComponent: ComponentGraph field

- [x] 1.1 Add `use mapl_ComponentGraph_mod, only: ComponentGraph` and a
      private `type(ComponentGraph) :: local_graph` field to
      `OuterMetaComponent` in `superstructure/generic/OuterMetaComponent.F90`
      (under the existing "Hierarchy" section, alongside `children`/
      `registry`).
- [x] 1.2 Declare a new `get_component_graph` module function in the
      submodule-interfaces block of `OuterMetaComponent.F90`, returning a
      `type(ComponentGraph), pointer`, matching the `target`/pointer-result
      convention used by `get_registry`/`get_component_spec`.
- [x] 1.3 Add `get_component_graph` to the type-bound procedure list.

## 2. Construction wiring

- [x] 2.1 Initialize `local_graph` via `ComponentGraph()`'s constructor,
      unconditionally (not lazy), per design.md Decisions. **Deviation
      from literal task text:** implemented in `init_meta`
      (`OuterMetaComponent/init_meta.F90`), not `new_outer_meta`, matching
      the existing `registry = StateRegistry(name)` pattern in the same
      procedure — `new_outer_meta`'s own header comment states it "only
      copies basic parameters," and `ComponentGraph()` is non-trivial (it
      seeds `default_network_id` and inserts the default network via
      `graph%generators%network%next()`), so it needs the same explicit,
      non-default-initialization treatment as `registry`. Every call site
      invokes `OuterMetaComponent(...)` immediately followed by
      `%init_meta(_RC)` (`GenericGridComp.F90`, `add_child_by_spec.F90`),
      so behavior is identical to the task's stated intent.
- [x] 2.2 Implement `get_component_graph` in a new submodule file
      (`superstructure/generic/OuterMetaComponent/get_component_graph.F90`),
      following the existing one-procedure-per-file convention, mirroring
      `get_registry.F90`.
- [x] 2.3 Confirm `attach_outer_meta`/`free_outer_meta` correctly
      construct/destroy the new field with no leak. Verified: no
      `OuterMetaComponent` field (including `registry`, `children`) is
      explicitly deallocated/finalized in `free_outer_meta` today — the
      pointer obtained via `_GET_NAMED_PRIVATE_STATE` is never
      `deallocate`d in this codebase. `local_graph` follows the exact
      same lifecycle as its siblings; `ComponentGraph`'s own private
      components are plain gFTL containers/scalars with no raw pointers
      requiring manual free, so no new leak is introduced beyond what
      already exists for the type as a whole (pre-existing condition, out
      of scope for this change).

## 3. Export surface

- [x] 3.1 Checked `superstructure/generic/API.F90`: it re-exports only
      free `GridComp*` procedures from `mapl_Generic_mod`/
      `mapl_GenericGridComp_mod` for user/component-author consumption; it
      never exposes `OuterMetaComponent`'s type-bound procedures directly.
      `get_component_graph` correctly stays unexported here — REQ-HIER-006
      requires proxy/graph internals be hidden from user-facing APIs. No
      change needed.

## 4. Tests

All added in `superstructure/generic/tests/Test_ComponentHierarchyGraph.pf`
(registered in `superstructure/generic/tests/CMakeLists.txt`'s
`components_test_srcs`), using a test-local `make_outer_meta` helper that
constructs `OuterMetaComponent` directly via
`attach_outer_meta`/`OuterMetaComponent()`/`init_meta` (bypassing
SetServices/DSO machinery — see file header).

- [x] 4.1 `test_new_component_owns_valid_empty_graph`: a newly constructed
      `OuterMetaComponent` exposes a valid `ComponentGraph` via
      `get_component_graph` (non-null pointer, `is_frozen()` false,
      `is_finalized()` false, `get_node_ids()` returns an empty set).
- [x] 4.2 `test_sibling_components_have_distinct_graphs`: two children of
      the same parent (added via `add_child`) have distinct
      `ComponentGraph` instances — registering a node in one graph does
      not make `owns_node` true for the other.
- [x] 4.3 `test_graph_port_tables_reachable_through_accessor`: exercises
      `get_component_graph`'s existing port tables end-to-end — register a
      node, bind it via `add_import_port`/`add_export_port`/
      `add_child_port_binding` through the accessor's returned pointer,
      confirm `get_import_port`/`get_export_port`/`get_child_port_binding`
      return it — demonstrating REQ-HIER-002/006 are satisfied by reuse,
      not new storage.
- [x] 4.4/4.5 `test_parent_reaches_child_only_through_driver`: covers
      REQ-HIER-003/005 as far as Fortran allows a runtime test to. The
      **runtime-checkable** half is verified: the only public path from a
      parent to a child (`get_child`) returns a `GriddedComponentDriver`
      value, never the child's `OuterMetaComponent`/`ComponentGraph`. The
      **not runtime-checkable** half — "no procedure exists anywhere in
      `OuterMetaComponent`'s public interface that returns a parent
      reference, a child's internal graph, or a child's `NodeId`/
      `DependencyNetworkId` sets" — cannot be expressed as a pFUnit
      assertion in Fortran (no reflection API for "this symbol does not
      exist"), exactly as design.md - Decisions anticipated. This is
      enforced by code review against
      `superstructure/generic/OuterMetaComponent.F90`'s interface block
      instead, and is documented as such in the test file's header/inline
      comments so it is a discoverable, named regression anchor rather
      than an implicit assumption. Flagging this explicitly since it
      falls short of "automated regression test" despite satisfying the
      task's intent.

## 5. Verification

- [x] 5.1 Built MAPL with NAG (`nag` build dir, `nag-stack` module).
      Required one fix beyond the original task list: the new
      `get_component_graph.F90` submodule had to be added to
      `superstructure/generic/CMakeLists.txt`'s `esma_add_fortran_submodules`
      source list (missed in the original task breakdown) — without it the
      link failed with an undefined symbol. After that fix, full build
      succeeds cleanly. Ran the full `ctest` suite (75 tests): 68 passed,
      7 failed, all 7 pre-existing environment gaps unrelated to this
      change (missing `netCDF4` Python package for extdata dry-run
      verification in `MAPL3G_Comp_Test_case02/11/23`; missing
      `LOCAL_REGRESSION_DATA_DIR` for `ll-ll`/`cs-cs`/`cs-ll`/`ll-cs`
      Regrid_Util tests) — none touch `OuterMetaComponent`, `ComponentGraph`,
      or generic superstructure code.
- [x] 5.2 Ran `MAPL.generic.components` (contains the new test file) and
      the full `MAPL.generic.*` label (7 suites: graph, scenarios,
      transforms, vertical, aspects, components, core) — 100% pass.
      Verified individually via direct executable run with `-v` that all
      4 new test cases (`test_new_component_owns_valid_empty_graph`,
      `test_sibling_components_have_distinct_graphs`,
      `test_graph_port_tables_reachable_through_accessor`,
      `test_parent_reaches_child_only_through_driver`) ran and passed.
- [x] 5.3 `openspec validate component-hierarchy-foundation --strict`
      reports the change valid (verified during planning; re-verify after
      these implementation edits before archiving).
