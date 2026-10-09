## 1. VariableSpec: borrower declaration

- [x] 1.1 Add `is_inout_borrower` (or equivalent marker, per design.md
      Decision 1) to `VariableSpec`/`VariableSpec_private.F90`, defaulting
      to `.false.` for every existing declaration.
- [x] 1.2 Add an accessor/query for the marker, following the existing
      "mark the item, not a new itemType" precedent (`state_item_variant`,
      `callback_interface_id`). Neither precedent field has a dedicated
      accessor method either - the plain `logical` field itself is the
      query, read directly (`var_spec%is_inout_borrower`), consistent with
      both.
- [x] 1.3 pFUnit coverage: a `VariableSpec` declared as an inout borrower
      reports itself as such; one not so declared does not.
      (`Test_VariableSpecInout.pf`, registered in
      `superstructure/generic/tests/CMakeLists.txt`.)

## 2. GraphBuilder: inout resolution branch

- [x] 2.1 Add the new branch to real connection resolution (parallel to
      the existing callback-interface branch in `GraphBuilder.F90`): a
      matched destination item marked `is_inout_borrower` is delegated to
      ordinary-inout resolution instead of the ordinary single-direction
      path.
- [x] 2.2 Implement ordinary-inout resolution reusing the existing
      `build_characteristics`/`find_mismatched_characteristics` check
      (`resolve_one`, `GraphBuilder.F90:689-782`) for the forward
      comparison - no new comparison logic duplicated.
- [x] 2.3 On exact match: add the forward-network edge (owner -> borrower)
      in the component's existing default dependency network, with no
      `TransformGraphNode`.
- [x] 2.4 On exact match: create a fresh return `DependencyNetwork` for
      this pairing via `this_graph%create_network()` (design.md Decision 2,
      revised - mirrors callback wiring's own per-binding
      `create_network()` precedent) and add the return-network edge
      (borrower -> owner), with no `TransformGraphNode`.
- [x] 2.5 On mismatch: reject the pairing explicitly (reported resolution
      failure), and do NOT call `find_or_build_extension_chain` or create
      any transform.
- [x] 2.6 Detect and reject a borrower with no identifiable owner.
- [x] 2.7 Detect and reject chained/recursive borrowing (owner item is
      itself a declared borrower in another pairing).
- [x] 2.8 **Added during review**: detect and reject a declaration that
      combines inout borrower intent with an expected callback interface
      (`var_spec%callback_interface_id%is_valid() .and.
      var_spec%is_inout_borrower`) - checked before either branch runs,
      so neither silently wins and the other declaration is ignored.
      Reviewer caught that the callback-interface branch runs first and
      `_RETURN`s early, so a combined declaration would previously have
      resolved as callback-only with the inout marker silently dropped.
      Code change only - not rebuilt/retested in this session (will
      surface in the next build/test pass if wrong).

## 3. Return-edge propagation trigger

- [x] 3.1 **Deferred (design.md Decision 3, revised, user-confirmed during
      implementation):** `ComponentGraph_DemandDrivenUpdate.F90`'s
      `finish_update_frame` only does active work for a
      `TransformGraphNode` frame; a `StateItemNode` frame (what every
      no-transform inout edge is, by REQ-INOUT-001's own requirement) is
      always a no-op. The real trigger event (the borrower's own GridComp
      run / `MethodGraphNode` invocation completing) has no real
      invocation-completion hook in this codebase yet, even for the
      already-landed callback-wiring sub-change. Building that hook is
      Phase 4 invocation-lifecycle work, not this change's scope. This
      change lands the return edge as validated graph structure only
      (tasks 2.4, 4.3); the runtime trigger is explicit follow-up work,
      tracked in design.md's Decision 3 (revised) and Risks sections -
      not implemented here, not silently dropped.
- [x] 3.2 Confirm REQ-DEP-008a's cross-network "never written twice in
      the same pass" validation already covers the inout forward/return
      pattern with no code change: the forward edge's target is the
      borrower's `NodeId`, the return edge's target is the owner's
      `NodeId` - two different ids, so `validate_cross_network_writes`
      passes by the same structural reasoning already covering callback
      get/put (confirmed by reading `ComponentGraph.F90` directly - no
      allow-list exists or is needed; see design.md Risk 2). Covered by
      the regression test in task 4.3.

## 4. Tests

- [x] 4.1 pFUnit: direct-alias pairing wires forward and return edges with
      no `TransformGraphNode`, modeled on
      `Test_GraphBuilder.pf:202-307`'s
      `test_ordinary_connection_wires_matching_and_reports_unresolved`.
- [x] 4.2 **Inherited, not independently tested at this layer** (same as
      the ordinary REQ-EXT-003 no-op case already is): payload sharing
      happens through real ESMF Field materialization outside
      GraphBuilder/ComponentGraph entirely (confirmed: neither
      `GraphStateItem.F90` nor `CompositeStateMaterialization.F90` calls
      `MAPL_NamedAlias`/`ESMF_NamedAlias` - that lives in legacy
      `FieldClassAspect%add_to_state`). Since task 2.3's forward edge is
      wired identically to the ordinary no-op case, payload sharing is
      inherited from that same path. Neither
      `test_ordinary_connection_wires_matching_and_reports_unresolved`
      (the precedent this change's own tests model) nor any other test in
      `Test_GraphBuilder.pf` exercises real Field materialization either -
      all assert graph-edge structure only. Documented in
      `test_inout_direct_alias_wires_forward_and_return_edges`'s own
      header comment.
- [x] 4.3 pFUnit: forward-network edge (owner -> borrower) and
      return-network edge (borrower -> owner) exist in two distinct
      `DependencyNetwork`s for one pairing, and `ComponentGraph%validate()`
      (REQ-DEP-008a cross-network check) succeeds - the non-overlapping-
      pass guarantee this change provides is structural (two different
      target `NodeId`s in two different networks), not a runtime-timing
      behavior to observe (task 3.1's runtime trigger is deferred; there
      is no "borrower executes" event to order against yet).
- [x] 4.4 pFUnit: mismatched pairing is rejected, not chained - assert no
      extension chain / transform is created, modeled on
      `Test_GraphBuilder.pf:316`'s
      `test_ordinary_connection_units_mismatch_creates_chain` (inverted
      expectation).
- [x] 4.5 pFUnit: borrower with no identifiable owner is rejected.
- [x] 4.6 pFUnit: chained/recursive borrowing is rejected.
- [x] 4.7 pFUnit: non-inout connections are fully unaffected (existing
      ordinary-connection tests still pass unchanged).

## 5. Build and validate

- [x] 5.1 Build with NAG: `module load nag/7.2.41 mpi baselibs`, then
      configure/build per `.opencode/skills/mapl-build` /
      `.opencode/skills/compiler-switching`.
- [x] 5.2 Run the full `ctest` suite. Confirm pass/fail count matches
      baseline plus the new tests - the 7 pre-existing, unrelated
      failures on this platform are expected and are not regressions to
      chase; do not let them block this change, but do confirm the
      failing set is exactly that known 7 (no new failures introduced).
- [x] 5.3 Run `openspec validate --strict` for this change.

## 6. Documentation

- [x] 6.1 Update `docs/graph/spec/16-inout-items.md` to note REQ-INOUT-001
      is implemented, without changing the document's overall
      `[DEFERRED]`/`[SPECULATIVE]` status (which still covers the general
      case, REQ-INOUT-002/5b2).
- [x] 6.2 Update `docs/graph/spec/20-implementation-roadmap.md` §20.4.4 to
      mark 5b landed, following the same "landed" annotation style already
      used for 4e/4f/4g.
