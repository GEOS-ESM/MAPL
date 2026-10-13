## 1. StateItemNode: new identity and resolution-mode fields

- [x] 1.1 Add `short_name` (character, allocatable), `state_intent`
      (`ESMF_StateIntent_Flag`), `is_inout_borrower` (logical, default
      `.false.`), and `callback_interface_id` (`CallbackInterfaceId`)
      fields to `StateItemNode` (`StateItemNode.F90`), with get-only
      accessors (`get_short_name`, `get_state_intent`,
      `is_inout_borrower`, `get_callback_interface_id`). Extend the
      `StateItemNode` constructor (`new_StateItemNode`) to accept these
      as optional arguments, defaulted so every existing call site
      (synthetic test nodes, etc.) compiles unchanged.

      **Design correction (surfaced during implementation):** also
      added setters (`set_short_name`/`set_state_intent`/
      `set_is_inout_borrower`/`set_callback_interface_id`) - not
      anticipated by this task's original "get-only, no setter" plan.
      Needed because a composite `VariableSpec`'s top-level node is
      built by `materialize_composite`
      (`CompositeStateMaterialization.F90`), which the constructor
      never sees with identity args at all (its own signature has no
      var_spec-identity parameters) - `advertise_one` sets identity on
      that node post-construction via these setters, the same
      post-construction pattern it already uses for characteristics
      (task 2.2).
- [x] 1.2 Add `ungridded_dims` and `vertical_stagger` fields (mirroring
      `VariableSpec`'s own field types) to `GraphStateItem`
      (`GraphStateItem.F90`), alongside the existing `characteristics`
      map, with get/set accessors following that map's own
      "independent of which handle component is allocated" contract
      (not reset by `clear_components`).

      **Design correction (surfaced during implementation):** also
      added `declared_geom` (`ESMF_Geom`, allocatable),
      `declared_vertical_grid` (`class(VerticalGrid)`, allocatable),
      and `declared_item_type` (`ESMF_StateItem_Flag`) - not in the
      original task list. Needed because `try_materialize_field_extension`/
      `materialize_extension_chain`'s own `itemType`/geom/vgrid gates
      and `resolve_export_geom`/`resolve_export_vgrid`'s own per-item
      explicit-override checks had no node-level equivalent otherwise;
      without these three fields `resolve_one` would still need a live
      `VariableSpec` reference for the two sides being compared,
      defeating this change's own core claim. (The owning component's
      *component-wide* geom/vgrid defaults are NOT captured here - those
      remain read live via `OuterMetaComponent%has_geom()`/`get_geom()`/
      `get_vertical_grid()`, since GEOM_B genuinely runs after ADVERTISE
      and has no "once at advertise time" value to capture.)

## 2. Advertise-time population

- [x] 2.1 In `advertise_one` (`GraphBuilder_Advertising_smod.F90`), pass
      `var_spec%short_name`/`%state_intent`/`%is_inout_borrower`/
      `%callback_interface_id` into the `StateItemNode` constructor
      (task 1.1) when registering a non-composite node. Confirm
      `materialize_composite`'s own node-construction path (composite
      members) is updated the same way if it independently constructs
      `StateItemNode`s - check its implementation before assuming only
      one call site needs this.

      Confirmed `materialize_composite` builds its own top-level node
      with no identity args; `advertise_one` sets identity on it
      post-construction via the new setters (task 1.1 correction)
      instead of threading args through `materialize_composite`'s own
      signature - `CompositeStateMaterialization.F90` itself is
      unmodified.
- [x] 2.2 Move `build_characteristics`'s own field-to-characteristic
      logic (units/typekind/vertical_grid/state_item_variant ->
      `UnitsCharacteristic`/`TypeKindCharacteristicAdapter`/
      `VerticalGridCharacteristic`, unchanged) into `advertise_one`,
      called once per newly-registered node, writing into
      `GraphStateItem%characteristics` via the existing
      `set_characteristic` method. Do not call this for a node that
      already existed (idempotent re-advertisement guard, unchanged).

      **Design correction (surfaced during implementation, real bug
      fixed):** `TYPE_KIND_CHARACTERISTIC_KIND` is NOT inserted when
      `var_spec%typekind == MAPL_TYPEKIND_MIRROR`
      (`infrastructure/esmf/TypeKind.F90`) - a declared "inherit the
      matched export's kind" placeholder (e.g.
      `StatisticsGridComp.F90`'s own mirrored imports), not a real
      `ESMF_TypeKind_Flag`. The old `build_characteristics` never hit
      this case in practice (it ran on-demand inside `resolve_one`, by
      which point MAPL's own `TypekindAspect` mirroring machinery had
      already resolved the real kind elsewhere); running this at
      advertise time exposed it as a real `error stop` crash
      (`MAPL.generic.scenarios`'s `statistics_real` scenario) - fixed
      by gating the insert the same way `allocated(var_spec%units)`
      gates the units branch immediately above it. Also broadened
      `TypeKindCharacteristic.F90`'s `typekind_to_label`/
      `label_to_typekind` beyond R4/R8 to I4/I8/LOGICAL (mirroring
      `mapl_FieldInfo_mod`'s own precedent, `infrastructure/field/
      FieldInfo.F90`) as defensive broadening, independent of the
      MIRROR fix.
- [x] 2.3 Populate the new `ungridded_dims`/`vertical_stagger` fields
      (task 1.2) from `var_spec%ungridded_dims`/`%vertical_stagger` in
      the same `advertise_one` pass.

      Also populates the task-1.2-correction fields
      (`declared_geom`/`declared_vertical_grid`/`declared_item_type`)
      from `var_spec%geom`/`%vertical_grid`/`%itemType` in the same
      pass.

## 3. Rewrite destination-match discovery

- [x] 3.1 Change `for_each_matching_import`'s `op` interface
      (`I_match_op`, private to `mapl_GraphBuilder_mod`) from
      `subroutine(var_spec, rc)` to `subroutine(node, rc)` where `node`
      is `class(StateItemNode)`. Rewrite `for_each_matching_import`'s
      own body to iterate the destination `ComponentGraph`'s import
      ports (new iteration need - check whether `PortIdNodeIdMap`
      already supports forward iteration the way `var_specs%ftn_begin()`
      does; add it if not) instead of `dst_spec%var_specs`, retrieving
      each port's `NodeId` -> `StateItemNode` via `graph%get_node(id)`,
      and filtering via the node's own `get_short_name()`/
      `get_state_intent()` against `dst_pt%v_pt%matches(...)`.

      Confirmed `PortIdNodeIdMap` already supports `ftn_begin()`/
      `ftn_end()` forward iteration (same gFTL2 map template every
      other container in this codebase uses) - no container change
      needed. **Design correction:** `ComponentGraph` itself had no way
      to enumerate the *whole* import/export-port table (only
      single-key `get_import_port(port_id)` lookups existed) - added
      `ComponentGraph%get_import_ports()`/`get_export_ports()`
      (`ComponentGraph.F90`), returning a copy of the full table
      (mirrors `get_node_ids()`'s own "copy, empty once finalized"
      convention). Not in the original task list; `ComponentGraph.F90`
      is accordingly affected code beyond proposal.md's listed files.
- [x] 3.2 Update `for_each_matching_import`'s two call sites
      (`check_match_connection_unsatisfied`'s `check_one`,
      `resolve_match_connection`'s `resolve_one`) to receive a
      `StateItemNode` instead of a `VariableSpec`, reading
      `node%is_inout_borrower()`/`node%get_callback_interface_id()`/
      `node%get_short_name()` wherever the old code read the
      corresponding `var_spec%...` field. Change `for_each_matching_import`'s
      own signature to pass the destination `ComponentGraph` (not
      `dst_spec`) as its first argument; update both call sites to
      resolve that graph via `component_for(this, dst_pt%component_name)`
      + `get_component_graph()` (or `get_child_component_graph` for a
      named child) instead of `component_spec_for`.

      **Design correction (surfaced during implementation):**
      `check_one`/`resolve_one`'s own callback-interface branch calls
      `resolve_callback_import`/`resolve_callback_destination`
      (`GraphBuilder.F90`, not in `GraphBuilder_CallbackWiring_smod.F90`
      - not covered by proposal.md's "Explicitly out of scope" list,
      which only names that submodule's own three procedures) - both
      took a full `VariableSpec` argument, reading only
      `%callback_interface_id`/`%short_name` off it. Changed both
      signatures to take `CallbackInterfaceId`/`character` arguments
      directly instead, so the callback branch needs no `VariableSpec`
      either. Updated three direct test call sites in
      `Test_GraphBuilder.pf` accordingly.
- [x] 3.2a (not in original task list) Discovered and fixed a real bug
      in `resolve_one`'s own rewrite: `find_export_node_id`'s first
      argument must be `this` (the component whose own graph
      `get_or_make_local_node_id` mutates), not the already-resolved
      `src_meta` - passing `src_meta` caused `get_or_make_local_node_id`
      to look for a child named `src_pt%component_name` *relative to
      src_meta itself* instead of relative to `this`, failing with
      "Child not found" for every ordinary (non-multi-level) connection.
      Caught by `Test_GraphBuilder.pf`'s own pre-existing suite (task
      6.1), not a new test.

## 4. Rewrite characteristics and source-side lookups

- [x] 4.1 Remove `build_characteristics`. Update
      `resolve_match_connection`'s `resolve_one` and
      `resolve_inout_destination` to call
      `export_node%get_characteristics()`/`import_node%get_characteristics()`
      (or the destination node's own, for the inout case) directly,
      where `export_node`/`import_node` are the `StateItemNode`s
      already retrieved via their resolved `NodeId`s.
- [x] 4.2 Remove `find_export_var_spec`. Update `find_export_node_id`
      (which currently calls it) to find the export by scanning the
      source `ComponentGraph`'s own export ports for a node whose
      `get_short_name()` matches, instead of scanning
      `src_spec%var_specs`; `find_export_node_id`'s own return contract
      (an `export_node_id`) is unchanged, only its internal lookup
      mechanism changes - its `export_var_spec` output argument
      becomes an `export_node` (`StateItemNode`, by pointer) output
      instead, since callers need the node's fields now, not a
      `var_spec`'s.

      Factored the port-table scan into a separate pure helper,
      `find_export_node(src_meta, short_name)` (read-only, no graph
      mutation), called both by `find_export_node_id` (which
      additionally mutates via `get_or_make_local_node_id`) and
      directly by `check_match_connection_unsatisfied`'s own `check_one`
      - preserving that procedure's own module-header contract
      ("WITHOUT creating any graph structure") that calling
      `find_export_node_id` there would have violated.
- [x] 4.3 Remove `find_inout_borrower_var_spec`. Update
      `resolve_inout_destination`'s chained-borrowing check (currently
      calling it on `owner_comp_spec`) to scan the owner's own import
      ports for a node with the same short name whose
      `is_inout_borrower()` is true, via the same `ComponentGraph`
      already available from `component_for(this, src_pt%component_name)`
      + `get_component_graph()`.
- [x] 4.4 Update every remaining `export_var_spec%...`/`var_spec%...`
      read in `resolve_one`/`resolve_inout_destination` (branch checks
      on `callback_interface_id%is_valid()`, `is_inout_borrower`) to
      read the corresponding `StateItemNode` field instead. Confirm (by
      grep, not just by memory) that no `var_spec`-typed local variable
      remains live past the point its originating node has been
      resolved, anywhere in `GraphBuilder_ConnectionResolution_smod.F90`.

      Confirmed by grep: zero occurrences of `dst_spec%var_specs`/
      `src_spec%var_specs`/`comp_spec%var_specs` remain in
      `GraphBuilder_ConnectionResolution_smod.F90` outside comments
      (task 6.3).

## 5. Rewrite materialization inputs

- [x] 5.1 Update `try_materialize_field_extension` and
      `materialize_extension_chain` (`GraphBuilder.F90`) to read
      `ungridded_dims`/`vertical_stagger` off the export `StateItemNode`'s
      own `GraphStateItem` payload (task 1.2's new fields) instead of
      `export_var_spec%ungridded_dims`/`%vertical_stagger`. Remove the
      `export_var_spec`/`import_var_spec` dummy arguments from both
      procedures' signatures where they are no longer needed for
      anything but `itemType`/`typekind`/`units` - check whether any of
      those three still need a `var_spec`-typed argument or can also
      move to node-based reads (characteristics already cover
      typekind/units via task 2.2; `itemType` has a `GraphStateItem`
      equivalent, `itemType()`, already in play - prefer it over
      `var_spec%itemType` if both are available at each call site).

      **Design decision (user-directed correction):** `GraphStateItem%
      itemType()` is a *runtime* query (which ESMF handle component is
      currently allocated - always `NOTFOUND` before materialization,
      the exact state these two procedures run in) and cannot
      substitute for `VariableSpec%itemType`'s *declared*
      classification. Resolved by adding `declared_item_type` to
      `GraphStateItem` (task 1.2 correction) instead - both procedures'
      `export_var_spec`/`import_var_spec` arguments are replaced
      entirely with `export_item`/`import_item` (`GraphStateItem`, by
      value), with `itemType`/units/typekind/ungridded_dims/
      vertical_stagger all read off these two items via new helpers
      `get_item_units`/`get_item_typekind` (`GraphBuilder.F90`) plus
      the task-1.2-correction accessors. `TypeKindCharacteristic_mod`'s
      `label_to_typekind` was made public to support the typekind
      round-trip. `merged_var_spec_for_link` was renamed
      `merged_item_for_link` and reworked to the same item-based
      inputs. User noted a related but out-of-scope naming issue:
      `VariableSpec%itemType` should eventually be renamed
      `extended_item_type` for clarity - tracked as a follow-up, not
      done here (would require touching client code beyond this
      change's scope).
- [x] 5.2 Update every call site of `try_materialize_field_extension`/
      `materialize_extension_chain` in
      `GraphBuilder_ConnectionResolution_smod.F90` for the new
      signatures. Confirm the `component_for(this, export_comp_name)`
      call inside both procedures (currently used only to reach
      `resolve_export_geom`/`resolve_export_vgrid`, which read
      component-wide `has_geom()`/`get_geom()`/`get_vertical_grid()` -
      NOT per-item `var_spec` fields) is left in place - that reach is
      legitimate and out of scope for removal; only the `var_spec`
      -specific `ungridded_dims`/`vertical_stagger` reads move per task
      5.1.

      `resolve_export_geom`/`resolve_export_vgrid`
      (`GraphBuilder_PortsAndProxies_smod.F90`) also changed their own
      first argument from `VariableSpec` to `GraphStateItem` (reading
      the task-1.2-correction `declared_geom`/`declared_vertical_grid`
      fields for the per-item override check); their own
      component-wide fallback via `owner%has_geom()`/`get_geom()`/
      `get_vertical_grid()` is unchanged, confirming this task's own
      "left in place" expectation for that half.

## 6. Regression verification (no observable behavior change)

- [x] 6.1 Run `Test_GraphBuilder.pf`'s full existing suite (every case
      listed in `superstructure/generic/tests/CMakeLists.txt`'s
      `components_test_srcs`) with no assertion changes; every existing
      case must pass with its existing expected values unchanged
      (proposal.md Impact: "no existing assertion changes" is a
      correctness check for this change).

      All passing (`MAPL.generic.components`, NAG). Caught two real
      implementation bugs during this run (both fixed, see tasks 2.2
      and 3.2a above) before the suite went green.
- [x] 6.2 Add `test_resolution_succeeds_with_no_live_var_spec_reference`:
      construct the synthetic parent/child/grandchild shape already
      used elsewhere in `Test_GraphBuilder.pf`, advertise, then (before
      calling `resolve_connections`) deliberately let every local
      `VariableSpec`/`ComponentSpec` variable the test itself declared
      go out of scope or be overwritten with a different, mismatching
      `VariableSpec` under the same short name - demonstrating that
      resolution's outcome depends only on the already-advertised
      node, not on re-reading whatever `ComponentSpec%var_specs`
      currently holds. (If `ComponentSpec%var_specs` cannot practically
      be mutated post-advertise in a realistic test shape, achieve the
      same proof by asserting - via a targeted unit test on
      `for_each_matching_import`/`build_characteristics`'s replacement
      directly - that neither reads `ComponentSpec%var_specs` at all;
      pick whichever is more directly verifiable and record which one
      was used in the test's own header comment.)

      Used the "overwrite" variant: advertises a matching T export/
      import pair (units='K' both sides), then overwrites both sides'
      `ComponentSpec%var_specs` with a MISMATCHING declaration under
      the same short name (units='mismatched_units') before calling
      `resolve_connections`, and asserts the connection still wires
      directly (no extension chain, no unsupported-characteristic
      report) - proving the stale/mismatching var_specs are never
      consulted. Added to `Test_GraphBuilder.pf`; passing.
- [x] 6.3 Grep-confirm (as a build-time check, not just a one-time
      search) that `GraphBuilder_ConnectionResolution_smod.F90` contains
      no remaining reference to `dst_spec%var_specs`/`comp_spec%var_specs`/
      `src_spec%var_specs` outside of `for_each_matching_import`'s own
      now-removed body (i.e. zero occurrences after this change).

      Confirmed: zero occurrences outside comments.

## 7. Verification

- [x] 7.1 Build MAPL with NAG (`nag` build dir); run `cmake --build nag
      -j 8` and `build-tests`; confirm no new build regressions.

      Full NAG build and build-tests both succeed, no errors.
- [x] 7.2 Run `MAPL.generic.components` and `MAPL.generic.scenarios`
      directly with `-v`; confirm no regression against the pre-change
      baseline (same pass counts as the most recent prior change's own
      documented baseline).

      Both pass via `ctest` (`MAPL.generic.components`,
      `MAPL.generic.scenarios`, `MAPL.generic.graph` all green).
- [x] 7.3 Run full `ctest` for the `MAPL.generic.*` label set; confirm
      no regression outside the same pre-existing, unrelated
      environment gaps earlier changes' own tasks.md files already
      documented.

      Full `ctest` run: 71/78 pass; the 7 failures are pre-existing
      environment gaps unrelated to this change (`LOCAL_REGRESSION_DATA_DIR`
      not set for the four `Regrid_Util` ll/cs regression cases;
      missing Python `netCDF4` package for `MAPL3G_Comp_Test_case02`/
      `case11`/`case23`'s own extdata dry-run verification step) -
      same class of gap this project's own prior changes' tasks.md
      files already documented.
- [x] 7.4 `openspec validate graph-builder-stateitem-resolution
      --strict` reports the change valid.

      Confirmed: "Change 'graph-builder-stateitem-resolution' is valid".

- [x] 7.5 (not in original task list) gfortran portability
      verification and fix. The original NAG-only verification above
      (7.1-7.3) passed cleanly on first implementation, but a
      subsequent gfortran build + test run surfaced a real,
      nondeterministic SIGSEGV (~10-20% of `MAPL.generic.components`
      runs) not present under NAG at all, across many consecutive
      stress-test runs.

      **Root cause:** `GraphStateItem%get_payload()`/`state_members()`/
      `field_bundle_members()` all did a plain intrinsic assignment
      (`x = this%payload`/`this%state_members_map`/
      `this%field_bundle_members_map`) of a type containing a gFTL2
      `StateItemMemberMap` component. For a COMPOSITE item (a non-empty
      member map - exactly the new code path this change's own
      design-correction fields/materialize_composite interaction
      exercises for the first time at this frequency), gfortran's own
      compiler-generated defined-assignment (`deep_copy`, walking the
      map's own tree) nondeterministically corrupted memory under
      `-fcheck=all`/Debug. NAG never reproduced it. Confirmed NOT a
      gFTL2 container-logic bug (`StateItemMemberMap` is a simple
      character-key -> `NodeId` map with extensive prior
      working-under-gfortran usage elsewhere in this codebase) - a real
      gfortran codegen issue for this exact "intrinsic-assign a
      nested-defined-assignment-bearing derived type" pattern, present
      regardless of this change's own correctness.

      **Fix (pure memory-safety hardening, zero design/architecture
      change):**
      - Added `get_payload_ptr()` to `StateItemNode.F90` and
        `state_members_ptr()`/`field_bundle_members_ptr()` to
        `GraphStateItem.F90` - pointer accessors into the already-owned
        storage, never copying. Updated every call site this change
        introduced (`populate_characteristics`, `resolve_one`,
        `resolve_inout_destination`, `build_callback_state_binding`)
        plus pre-existing call sites reached by this change's own
        exercised composite paths (`GraphBuilder_CallbackWiring_smod.F90`,
        and ~15 call sites across `Test_GraphBuilder.pf`/
        `Test_GraphGeometryHook.pf`/`Test_GraphStateItem.pf`) from `=`
        to `=>`.
      - Converted every `character(:), allocatable`/other-allocatable
        plain `x = expr` assignment this change introduced
        (`StateItemNode%short_name`'s get/set/constructor,
        `GraphStateItem%set_vertical_stagger`/`set_declared_geom`/
        `set_declared_vertical_grid`, `TypeKindCharacteristic.F90`'s
        label conversions, `GraphBuilder_CallbackWiring_smod.F90`'s
        loop-reassigned path variables) to explicit
        `allocate(..., source=...)` - a separate, independent known
        gfortran realloc-on-assignment hardening, applied defensively
        even though it did not by itself resolve the crash.
      - Discovered and fixed an unrelated pFUnit limitation along the
        way: `@before`/`@after` is NOT disambiguated by a `@test`
        subroutine's own dummy-argument type - a pair declared anywhere
        in a `.pf` file applies to EVERY `@test` in that file regardless
        of declared type (confirmed via NAG's own clear compile error,
        far clearer than gfortran's, which silently miscompiled instead
        of rejecting the mismatch). This blocked adding a `@before`/
        `@after` pair to reset `mapl_ExtensionResolution_mod`'s own
        module-level global `set_materialize_extensions` flag (a real,
        independent bug: a failing test between its own `.true.`/
        `.false.` calls leaks the flag into every later test) without
        first splitting all 5 of `Test_GraphBuilder.pf`'s own
        `ESMF_TestMethod` tests into a new file,
        `Test_GraphBuilderMaterializeGate.pf`, each file now
        single-method-type throughout.

      **Verification:** 100+ total gfortran stress-test runs (ctest
      loops and single-process lldb loops) with zero crashes after the
      fix, versus a consistent ~10-20% failure rate before it. Full
      `MAPL.generic.*` ctest suite green under gfortran. Full NAG
      rebuild + `MAPL.generic.*` suite re-confirmed 100% green
      (unaffected by this fix, as expected - NAG never exhibited the
      bug).

