## 1. VariableSpec callback marker

- [x] 1.1 Add `callback_interface_id : type(CallbackInterfaceId)` to
      `superstructure/generic/specs/VariableSpec.F90` (default
      unset/invalid - `is_valid() == .false.`), additive only, no
      constructor/aspect changes (design.md Decision 0). `VariableSpec`
      has no `private` clause, so the field is directly gettable/
      settable like every other simple field (`units`, `geom`, ...).
      A `make_VariableSpec()` optional keyword was tried and then
      reverted after review (mirroring `geom_id`'s own constructor
      coverage): it had exactly one caller anywhere in the tree - its
      own unit test - since the real `ComponentSpecParser` call site
      does not pass it and no YAML/`SetServices` entry point exists yet
      to drive one. Speculative, uncalled surface area, reverted in
      favor of documenting the real gap as an explicit deferral (see
      proposal.md/design.md's own "Explicit deferrals" - mirrors
      `composite-state-spec`'s identical precedent for `members`: a
      plain-field/method-driven capability with the DSL/YAML wiring
      left for later, not silently implied to already exist).
- [x] 1.2 Add a test confirming an ordinary `VariableSpec` (field not
      set) reports `callback_interface_id%is_valid() == .false.`, and a
      `VariableSpec` with it explicitly set reports `.true.` and returns
      the same id, via direct field access (the only supported way to
      set it, per 1.1's own revert).
      (`Test_VariableSpecCallback.pf`)

## 2. Flattened qualified-export namespace

- [x] 2.1 Add a `GraphBuilder.F90` procedure (e.g.
      `build_qualified_export_namespace(root, rc) -> entries(:)`) that,
      given a root `OuterMetaComponent`, recursively walks its own
      advertised exports plus every descendant's advertised exports (via
      the existing `get_child_component_spec`/`get_child_outer_meta`
      accessors, recursing across all levels, not just one), producing
      an array of entries keyed by qualified name (own exports under
      their short name; descendant exports under
      `comp_path // '/' // short_name`, composing one segment per level
      for multi-level descendants), each entry carrying its own
      `comp_path` and `VariableSpec` (design.md Decisions 1-2). Public
      `QualifiedExportEntry` type + `build_qualified_export_namespace`
      (mirrors `item_key`/`proxy_key`'s own "exposed for tests" precedent).
- [x] 2.2 Add a test constructing a multi-level descendant hierarchy
      (parent -> child -> grandchild, each exporting under a
      short name that collides with a sibling's) and confirm: each
      component's own export is visible under its own short name; each
      descendant's export is visible under a qualified name; two
      same-short-name descendant exports remain distinguishable;
      re-querying does not duplicate an entry.
      (`test_build_qualified_export_namespace_multilevel`)
- [x] 2.3 Add the cross-check test from design.md Risks: for a given
      multi-level descendant export, confirm the namespace's own
      qualified name matches what
      `VirtualConnectionPt(comp_name=...)%get_full_name()` would produce
      for an equivalent single-level case (drift-detection, not a shared
      implementation).
      (`test_qualified_name_matches_virtualconnectionpt_convention`)

## 3. Callback branch inside ordinary MatchConnection resolution

- [x] 3.1 In `GraphBuilder.F90`'s `for_each_matching_import`-driven
      dispatch (`check_match_connection_unsatisfied`'s `check_one` and
      `resolve_match_connection`'s `resolve_one`), add a branch checked
      first: if the matched destination `var_spec%callback_interface_id
      %is_valid()`, delegate to this change's own callback resolution
      (tasks 4-8) instead of the existing exact-name/extension-chain
      logic; otherwise fall through unchanged (design.md Decision 0).
- [x] 3.2 Add a test confirming a destination with no
      `callback_interface_id` set continues through the exact existing
      code path with no behavior change (byte-for-byte same result as
      before this branch existed, for both the unsatisfied-check and
      real-resolution entry points).
      (`test_non_callback_destination_unaffected`; every pre-existing
      `Test_GraphBuilder.pf` test - none of which set
      `callback_interface_id` - continues to pass unchanged, confirming
      no regression across the whole existing suite too.)

## 4. Wildcard/callback wiring resolution

- [x] 4.1 Add a `GraphBuilder.F90` procedure (e.g.
      `resolve_callback_import(this, src_pt, dst_var_spec, rejected,
      rc) -> matches(:)`) that: resolves `src_pt%component_name` to its
      `OuterMetaComponent` (via the existing `component_for` helper);
      builds its flattened namespace (task 2.1); for each entry,
      constructs a synthetic, `comp_name`-less
      `VirtualConnectionPt(EXPORT, entry%qualified_name)` and tests it
      against `src_pt%v_pt%matches(...)` (design.md Decision 3 - no new
      regex helper); for each match, validates interface conformance
      (member names/kinds cover every `CallbackArgumentSpec` the
      expected `CallbackInterface` declares, via
      `VariableSpec%get_member_names()`/`get_member()`) and rejects
      (reports, does not silently drop) a match that fails conformance
      (design.md Decision 4).
- [x] 4.2 Add a test with several synthetic descendant callback states
      (mirroring `15-callbacks.md`'s `DU`/`SS`/`SU`/`TR` tracer-callback
      example), a `MatchConnection` whose source pattern matches all of
      them and whose destination declares the expected interface, and
      confirm: every conformant match is selected; a deliberately
      non-conformant export (missing the `tracers` member) is rejected
      and reported, not selected; a pattern matching nothing resolves to
      an empty, non-error result.
      (`test_resolve_callback_import_matches_and_rejects`,
      `test_resolve_callback_import_no_match_is_empty`)

## 5. Callback-collection materialization

- [x] 5.1 Add a `GraphBuilder.F90` procedure that, given a resolved
      match list, allocates a fresh `NodeId`, constructs a
      `GraphStateItem` of `ESMF_STATEITEM_STATE` kind with a real,
      structurally-empty `ESMF_State`, registers a `StateItemNode`
      wrapping it in the destination's graph, and calls
      `add_state_member(matched_qualified_name, matched_node_id, rc)`
      once per match (design.md Decision 5) - built directly, not routed
      through `CompositeStateMaterialization`'s `VariableSpec`-tree walk.
- [x] 5.2 Add the orchestration in `resolve_one`'s callback branch
      (task 3.1): resolve matches (task 4.1), materialize the collection
      (task 5.1), and wire it to the already-advertised destination
      import via `ComponentGraph%add_dependency()` on the connection's
      default network - guarded by a resource-index reuse key derived
      from the destination import's own `NodeId` (mirroring
      `find_or_build_extension_chain`'s own `chain_key` idiom) so a
      repeat `resolve_connections()` call is a no-op, not a duplicate
      collection (design.md Decision 5).
      (`resolve_callback_destination`)
- [x] 5.3 Add a test confirming the materialized collection's
      `state_members()` map has exactly one entry per resolved match,
      each retrievable by its matched qualified name, that it is the
      destination import's sole producer in the default network, and
      that re-resolving the same connection does not duplicate entries
      or create a second collection node.
      (`test_callback_collection_materializes_and_wires_import`)

## 6. CallbackStateBinding real member wiring

- [x] 6.1 For each matched callback state (task 4.1's match list), add a
      `GraphBuilder.F90` procedure that constructs a real
      `CallbackStateBinding` (Phase 4c) using the matched state's own
      declared composite member `NodeId`s - read directly from that
      state's own already-materialized `GraphStateItem%state_members()`
      map in its owning component's own graph (resolved via a path walk
      generalizing `get_child_outer_meta` to the match's own
      `comp_path`), not re-derived from `VariableSpec` and not
      caller-supplied stand-ins - the first real, non-test caller of
      `CallbackStateBinding%bind_argument`.
      (`build_callback_state_binding`)
- [x] 6.2 Add a test confirming a `CallbackStateBinding` built this way
      from a real advertised composite callback state resolves its
      bound argument `NodeId`s to the same `NodeId`s that state's own
      `GraphStateItem%state_members()` map reports for its declared
      members.
      (`test_build_callback_state_binding_uses_real_member_nodeids`)

## 7. CallbackMethodBinding and per-method networks

- [x] 7.1 Create `superstructure/generic/graph/CallbackMethodBinding.F90`
      (`mapl_CallbackMethodBinding_mod`): stores the invoked
      `MethodGraphNode`'s `NodeId`, get/put `DependencyNetworkId`s, and
      two `StateItemMemberMap`s (`get_bindings`, `put_bindings`, both
      `character -> NodeId`) for argument source/target identities
      (design.md Decision 6, REQ-CB-019).
- [x] 7.2 Add a `GraphBuilder.F90` procedure that, for one bound
      callback method (a `CallbackStateBinding` plus a method name),
      registers a `MethodGraphNode` (holding the `StateMethodInvocation`
      attachment `CallbackStateBinding%bind_method` already produced),
      creates a get network and a put network via
      `ComponentGraph%create_network()`, wires each declared argument's
      dependency edge into the appropriate network by access mode
      (reusing `find_mismatched_characteristics`/
      `find_or_build_extension_chain` exactly like ordinary connection
      resolution, though both sides' `CharacteristicMap`s are empty at
      this data-model tier so the direct-edge branch is what actually
      runs today), and confirms each network independently passes
      acyclicity validation (REQ-CB-018). Also populates the
      `MethodGraphNode`'s own declared-argument/binding storage
      (REQ-MTH-002) so its `invoke()` is self-sufficient (see design.md
      Decision 7's own header note on `invoke_callback_method` not
      reusing `invoke_on_default_network` directly).
      (`build_callback_method_binding`, `wire_callback_argument`)
- [x] 7.3 Add a test declaring an INOUT callback argument, confirming
      both networks are constructed, share the same underlying data
      items where expected, and each independently validates as
      acyclic even when their union would not.
      (`test_callback_method_binding_builds_get_put_networks`)
- [x] 7.4 Add `Test_CallbackMethodBinding.pf` covering construction and
      accessor retrieval for the binding type itself.

## 8. Invoke-once-after-all-ready discipline

- [x] 8.1 Add a `GraphBuilder.F90` (or `CallbackMethodBinding`-adjacent)
      `invoke_callback_method(graph, binding, rc)` procedure mirroring
      `mapl_MethodInvocation_mod%invoke_on_default_network`'s exact
      three-phase shape (design.md Decision 7): for each get-network
      argument, ensure the provider side is up to date
      (`graph%update(get_network_id, provider_id, rc)`); invoke the
      bound `MethodGraphNode` exactly once; on success, advance the
      `NodeRevision` of every put-network argument's provider-side
      target. Never registers the callback method node into
      demand-driven dispatch (REQ-CB-020).
      (`invoke_callback_method`, `pull_callback_get_arguments`,
      `advance_callback_put_arguments`, synthetic-`ComponentGraph`-only,
      no `OuterMetaComponent` needed since `invoke_callback_method`
      itself only takes a `ComponentGraph` and a `CallbackMethodBinding`.)
- [x] 8.2 Add a test confirming: invocation is deferred while a required
      argument path is not yet ready; a ready binding invokes its method
      node exactly once; preparing multiple argument paths as part of
      one invocation request still results in exactly one invocation,
      not one per argument.
      (`test_invoke_pulls_get_then_invokes_then_advances_put`, in
      `Test_CallbackMethodBinding.pf` - "not yet ready" is exercised via
      a `TransformGraphNode` producer that has never executed, confirmed
      pulled current, via `graph%update()`, strictly before `invoke()`
      fires, mirroring `Test_MethodInvocation.pf`'s own established
      fixture pattern.)

## 9. Build wiring

- [x] 9.1 Add `CallbackMethodBinding.F90` to
      `superstructure/generic/graph/CMakeLists.txt` (and its
      `containers/` listing if a new small container module is needed
      for its own storage). No new container module was needed - its
      two `StateItemMemberMap`/`DependencyNetworkId` fields reuse
      existing types.
- [x] 9.2 Add all new `Test_*.pf` files to the relevant
      `tests/CMakeLists.txt` `test_srcs` lists.
      (`Test_VariableSpecCallback.pf` ->
      `superstructure/generic/tests/CMakeLists.txt`;
      `Test_CallbackMethodBinding.pf` ->
      `superstructure/generic/graph/tests/CMakeLists.txt`;
      `Test_GraphBuilder.pf` already registered, extended in place.)

## 10. Verification

- [x] 10.1 Build MAPL with the new sources and confirm no compile
      errors (see `.opencode/skills/mapl-build`). Built with
      `nag-stack` (Debug): `MAPL.generic`/full `MAPL`/`build-tests` all
      succeeded with no errors (only pre-existing, unrelated warnings).
      Two real bugs found and fixed during this build: (1) a name
      collision between `CallbackMethodBinding`'s private
      `get_network_id`/`put_network_id` fields and its own accessor
      procedures of the same name (NAG: "GET_NETWORK_ID is the name of
      an existing structure component") - fixed by renaming the private
      fields to `get_net_id`/`put_net_id`; (2) two `_RC` macro misuses
      in `build_callback_method_binding` where `_RC` (which closes the
      call's own parenthesis) was followed by more keyword arguments -
      fixed by writing those two calls out explicitly (`rc=status` +
      `_VERIFY(status)`) instead of using the macro. Also verified with
      `gfortran-stack` (Debug) at the user's request: surfaced the same
      known gfortran cpp-preprocessor `&`-continuation-inside-a-macro-
      call limitation `callback-data-model-registry`'s own task 7.1
      already documented - fixed by collapsing the 9 affected
      `_ASSERT(...)` calls (7 in `GraphBuilder.F90`, 2 in
      `CallbackMethodBinding.F90`) to single physical lines. Re-verified
      clean on both compilers afterward.
- [x] 10.2 Run the `MAPL.generic`/`MAPL.generic.graph` pFUnit test
      suites and confirm all new and existing tests pass; run the full
      `ctest` suite and confirm no new failures beyond the pre-existing,
      known-harmless ones already documented by prior Phase 4 changes.
      `ctest --test-dir nag -R "MAPL.generic.components|MAPL.generic.graph|MAPL.generic.core"`:
      100% passed (all pre-existing tests plus every new one in this
      change). One real test bug found and fixed along the way: a
      dangling-pointer runtime crash (NAG: "Dangling pointer A%TREE used
      as argument to intrinsic function ASSOCIATED") in
      `export_conforms_to_interface`/`build_callback_state_binding`/
      `build_callback_method_binding` - three local gFTL2 map variables
      (`arguments`, `arguments`, `accesses`) were missing the `target`
      attribute their own `ftn_begin()`/`ftn_end()` iteration requires
      (the same requirement `CallbackStateBinding.F90`'s own
      `build_arguments_for_method` already documents for its `accesses`
      local) - fixed by adding `target` to all three. One test bug
      found and fixed too: `Test_CallbackMethodBinding.pf`'s own
      rebinding-rejection test mixed an explicit `rc=status` call with
      `@assertExceptionRaised` (which only applies when `rc` is
      omitted, per this codebase's own established convention,
      e.g. `Test_CallbackInterface.pf`) - fixed to
      `@assertTrue(status /= 0)`. Full `ctest --test-dir nag`: 68/75
      passed - the 7 failures (`ll-ll`, `cs-cs`, `cs-ll`, `ll-cs`,
      `MAPL3G_Comp_Test_case02/11/23`) are the pre-existing,
      known-harmless failures (missing datasets / missing netCDF4
      Python package) already documented by prior Phase 4 changes' own
      verification notes - not caused by this change. Also verified
      with `gfortran-stack`: targeted suites 100% passed; full
      `ctest --test-dir gfortran`: 68/75 passed, the same 7
      pre-existing failures, same test names - no gfortran-specific
      regressions.

      Follow-up after review (see "no way to set/get callback_interface_id"
      question): adding the `make_VariableSpec` keyword (task 1.1) an
      *incremental* `cmake --build nag --target <one-suite>` build
      initially showed real-looking crashes (NAG: bus error inside
      `make_variablespec`, reached from real `ComponentSpecParser`
      code, not just tests) across suites/`MAPL3G_Comp_Test_case*` I
      had not directly rebuilt. Root-caused as a stale-object/module
      artifact from NAG's own submodule dependency tracking under
      incremental CMake builds, not a real bug: a fully clean rebuild
      (`rm -rf nag && cmake -B nag ... && cmake --build nag -j 8 &&
      cmake --build nag -j 8 --target build-tests`) reproduced none of
      it. Re-verified with a from-scratch clean build on both
      `nag-stack` and `gfortran-stack`: full `ctest` 68/75 on both,
      identical 7 pre-existing failures, zero regressions. Lesson for
      future work on this codebase: after changing a widely-`use`d
      module's public interface (e.g. `VariableSpec`), prefer a full
      `cmake --build <dir> -j 8 && cmake --build <dir> -j 8 --target
      build-tests` over building only the one test target that
      exercises the new code, since incremental per-target builds have
      shown gaps here before.

      Follow-up after further review: the `make_VariableSpec` keyword
      itself was then reverted (task 1.1 - it had no real caller
      anywhere outside its own now-also-reverted unit test; direct field
      assignment is the only supported way to set
      `callback_interface_id`, matching `composite-state-spec`'s own
      `declare_member` precedent and documented as an explicit deferral
      in proposal.md/design.md). Re-verified with fully clean, from-
      scratch rebuilds (`rm -rf nag && cmake -B nag ... && cmake --build
      nag -j 8 && cmake --build nag -j 8 --target build-tests`, same for
      `gfortran`) after the revert: full `ctest` 68/75 on both
      compilers, identical 7 pre-existing failures, zero regressions
      from the revert. (One transient MPI-oversubscription flake seen
      mid-sequence on `MAPL.generic.components` under `gfortran` -
      passed standalone and on the very next full-suite rerun;
      consistent with the environmental flakiness already observed
      earlier in this same verification pass, not a real failure.)
- [x] 10.3 Re-read `docs/graph/spec/20-implementation-roadmap.md`
      §20.4.3 and confirm this change's own bullet (4d) accurately
      reflects what was actually built; update the roadmap text if
      anything diverged during implementation (per §20.5's own
      discipline). Reviewed: no divergence - the roadmap's own 4d
      bullet ("GraphBuilder wildcard/regex expansion against the
      flattened qualified-export namespace... per-method
      DependencyNetworks... invoke-once-after-all-args-ready
      discipline... Depends on 4a... 4c, and composite-state-spec") does
      not assert any particular connection-declaration mechanism, so
      this change's mid-implementation pivot away from a dedicated
      `CallbackConnection` type (toward a `VariableSpec%
      callback_interface_id` marker instead, design.md Decision 0) is
      an implementation detail the roadmap text was never specific
      enough to contradict. No roadmap edit needed.
