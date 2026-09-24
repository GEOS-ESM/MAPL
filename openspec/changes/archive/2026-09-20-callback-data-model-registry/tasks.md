## 1. CallbackArgumentSpec

- [x] 1.1 Create `superstructure/generic/graph/CallbackArgumentSpec.F90`
      (`mapl_CallbackArgumentSpec_mod`): name + required
      `MAPL_StateItem_Flag` expected kind, `get_name`/`get_expected_kind`
      accessors (design.md Decision 1).
- [x] 1.2 Create `superstructure/generic/graph/containers/CallbackArgumentSpecMap.F90`
      (`mapl_CallbackArgumentSpecMap_mod`): gFTL map, `character ->
      CallbackArgumentSpec`, mirroring `containers/ArgumentSpecMap.F90`.
- [x] 1.3 Add `Test_CallbackArgumentSpec.pf` covering construction and
      accessor retrieval.

## 2. AccessSpecMap and CallbackMethodSpec

- [x] 2.1 Create `superstructure/generic/graph/containers/AccessSpecMap.F90`
      (`mapl_AccessSpecMap_mod`): gFTL map, `character -> AccessSpec`
      (design.md Decision 2).
- [x] 2.2 Create `superstructure/generic/graph/CallbackMethodSpec.F90`
      (`mapl_CallbackMethodSpec_mod`): thin wrapper around
      `AccessSpecMap` with `declare_argument_access`/`is_argument_access`/
      `get_argument_access`/`get_argument_accesses`, no-silent-replace on
      a re-declared argument name (design.md Decision 2).
- [x] 2.3 Create `superstructure/generic/graph/containers/CallbackMethodSpecMap.F90`
      (`mapl_CallbackMethodSpecMap_mod`): gFTL map, `character ->
      CallbackMethodSpec`.
- [x] 2.4 Add `Test_CallbackMethodSpec.pf` covering per-method
      argument-access declaration, duplicate-name rejection, and
      retrieval.

## 3. CallbackInterface

- [x] 3.1 Create `superstructure/generic/graph/CallbackInterface.F90`
      (`mapl_CallbackInterface_mod`): owns a `CallbackArgumentSpecMap`
      and a `CallbackMethodSpecMap`. `declare_argument(name,
      expected_kind, rc)`, `is_argument`, `get_argument`,
      `get_arguments`; `declare_method(name, rc)`, `is_method`,
      `get_method`, `get_methods`; `set_method_argument_access(method_name,
      argument_name, access, rc)` validating both the method and the
      argument are already declared on the interface before mutating the
      method's own `CallbackMethodSpec` via `%at()` pointer access
      (design.md Decision 3). No service-name field (spec requirement
      "identity carried externally").
- [x] 3.2 Add `Test_CallbackInterface.pf` covering: argument declaration
      + duplicate rejection; method declaration + duplicate rejection;
      `set_method_argument_access` success, rejection for an undeclared
      argument, and rejection for an undeclared method; the full
      `PassiveTracer` example from `15-callbacks.md` §15.3 (one
      `tracers` FieldBundle argument, `get`/`put` methods with
      `OUT`/`IN` access respectively).

## 4. CallbackInterfaceRegistry

- [x] 4.1 Create `superstructure/generic/graph/CallbackInterfaceRegistry.F90`
      (`mapl_CallbackInterfaceRegistry_mod`): private `CallbackInterfaceRegistry`
      derived type owning a `CallbackInterfaceId -> CallbackInterface`
      map, a `character -> CallbackInterfaceId` service-name map, and a
      `CallbackInterfaceIdGenerator`; private module-level singleton
      instance; public wrapper procedures
      `register_callback_interface(service_name, interface, id, rc)`,
      `get_callback_interface(id, rc)`,
      `get_callback_interface_by_name(service_name, rc)`,
      `lookup_callback_interface_id(service_name, rc)` (design.md
      Decision 4). Reject a duplicate `service_name`
      (no-silent-replace).
- [x] 4.2 Add a test-only `reset_registry()` only if 4.3's test suite
      proves service-name collisions across test cases to be a real
      problem in practice (design.md Risks - not planned up front).
      Not needed: 4.3's tests each use a unique `service_name`
      (`test-registry-*`), the same convention `ExtensionResolution.F90`'s
      own singleton relies on - no collision arose, so no
      `reset_registry()` was added.
- [x] 4.3 Add `Test_CallbackInterfaceRegistry.pf` covering: registration
      assigns a retrievable identity; retrieval by identity and by
      service name both succeed after registration; duplicate
      service-name registration is rejected and leaves the original
      registration unchanged; lookup of an unregistered service name
      fails explicitly.

## 5. CallbackStateBinding

- [x] 5.1 Create `superstructure/generic/graph/containers/CallbackMethodAttachmentMap.F90`
      (`mapl_CallbackMethodAttachmentMap_mod`): gFTL map, `character ->
      StateMethodInvocation` (reusing the existing Phase 4a adapter type
      unchanged, design.md Decision 5).
- [x] 5.2 Create `superstructure/generic/graph/CallbackStateBinding.F90`
      (`mapl_CallbackStateBinding_mod`): constructed from a
      `CallbackInterfaceId`, a `CallbackInterface` value, and the
      callback state's own `NodeId` (design.md Decision 6). Owns a
      `StateItemMemberMap` (argument name -> member `NodeId`) and a
      `CallbackMethodAttachmentMap` (method name -> `StateMethodInvocation`).
      `bind_argument(name, member_id, rc)` validates `name` against the
      held `CallbackInterface`'s declared arguments before inserting.
      `bind_method(name, rc, invoker)` validates `name` against the held
      `CallbackInterface`'s declared methods, then constructs
      `StateMethodInvocation(state_node_id, name, invoker)` (invoker
      optional) and inserts it. `get_argument_binding`,
      `get_method_attachment`, `get_interface_id`, `get_state_node_id`
      accessors. `invoke_method(name, rc)` looks up the attachment and
      calls its own `invoke()`, propagating the "no invoker attached
      fails loudly" behavior `StateMethodInvocation` already provides.
- [x] 5.3 Add `Test_CallbackStateBinding.pf` covering: construction
      retrieves interface id / state node id; argument binding success
      and retrieval; argument binding rejection for a name outside the
      interface; method binding success and retrieval; method binding
      rejection for a name outside the interface; invoking a bound
      method with a synthetic test-double invoker attached succeeds
      exactly once; invoking a bound method with no invoker attached
      fails loudly; an end-to-end `PassiveTracer` scenario (register the
      interface from Task 3's test fixture, bind a synthetic callback
      state `NodeId` with a synthetic `tracers` member `NodeId`, bind
      `get`/`put` methods, invoke `get` through a synthetic invoker).

## 6. Build wiring

- [x] 6.1 Add all new source files to
      `superstructure/generic/graph/CMakeLists.txt` (module files and
      their `containers/` counterparts), following the existing
      Phase 4a block's ordering convention (spec/type before the map
      that keys on it, dependents after their dependencies). Also
      includes `containers/CallbackInterfaceIdInterfaceMap.F90` and
      `containers/CallbackServiceNameIdMap.F90` - two small container
      modules `CallbackInterfaceRegistry.F90` (task 4.1) needs for its
      own `CallbackInterfaceId -> CallbackInterface` and `service name
      -> CallbackInterfaceId` maps, not called out as separate files in
      the original task list but required by design.md Decision 4's
      already-committed two-map shape.
- [x] 6.2 Add all new `Test_*.pf` files to
      `superstructure/generic/graph/tests/CMakeLists.txt`'s `test_srcs`
      list.

## 7. Verification

- [x] 7.1 Build MAPL with the new sources and confirm no compile errors
      (see `.opencode/skills/mapl-build` for the local build workflow).
      Built with `nag-stack` (Debug); `MAPL.generic`/`MAPL`/`build-tests`
      all succeeded with no errors (`nag/build.log`,
      `nag/build-tests.log`). Fixed two real bugs surfaced during the
      test build: a missing `operator(==)` import for
      `CallbackInterfaceId` in `Test_CallbackStateBinding.pf`, and a
      missing `target` attribute on `CallbackStateBinding.F90`'s local
      `AccessSpecMap` (needed for its iterator's internal pointer to
      stay valid across the loop - the same reason
      `GraphExport.F90`'s own `reverse_find_name` declares its map
      argument `target`); the latter caused a runtime crash
      ("dangling pointer") the first time `invoke_method` actually ran
      the iterator, not a compile failure.

      Also verified with `gfortran-stack` (Debug), at the user's
      request, after the NAG build/tests above were already green.
      Surfaced one real portability bug NAG's own parser had silently
      tolerated: gfortran's cpp-based preprocessor does not honor a
      Fortran `&` line-continuation *inside* a macro call's argument
      list, so every multi-line `_ASSERT(cond, &` / `'message')`
      invocation across the five new modules (`CallbackMethodSpec.F90`,
      `CallbackInterface.F90` x2, `CallbackInterfaceRegistry.F90` x2,
      `CallbackStateBinding.F90` x4 - 9 call sites total) failed with
      "Syntax error in argument list". Fixed by collapsing each call to
      a single physical line (all 9 fit well under NAG's own `-132`
      column limit, confirmed by measurement before editing, so this
      does not risk re-breaking NAG) - matches this module family's own
      existing precedent of never splitting an `_ASSERT` call across
      lines in the first place (e.g. `MethodGraphNode.F90`). Re-verified
      clean on both compilers afterward.
- [x] 7.2 Run the `MAPL.generic.graph` pFUnit test suite and confirm all
      new and existing tests pass. `ctest --test-dir nag -R
      "MAPL.generic.graph"`: 100% passed (`nag/ctest-graph.log`). Also
      ran the full `ctest --test-dir nag` suite: 68/75 passed: the 7
      failures (`ll-ll`, `cs-cs`, `cs-ll`, `ll-cs`,
      `MAPL3G_Comp_Test_case02/11/23`) are the pre-existing,
      known-harmless failures (missing datasets / stale external
      libraries, e.g. case23's missing netCDF4 Python package) called
      out at the start of this apply session - not caused by this
      change (`nag/ctest-full.log`).

      Also verified with `gfortran-stack`: `ctest --test-dir gfortran -R
      "MAPL.generic.graph"`: 100% passed (`gfortran/ctest-graph.log`).
      Full `ctest --test-dir gfortran` suite: 68/75 passed, the same 7
      pre-existing known-harmless failures as the NAG run, same test
      names (`gfortran/ctest-full.log`) - no gfortran-specific
      regressions.
- [x] 7.3 Re-read `docs/graph/spec/20-implementation-roadmap.md` §20.4.3
      and confirm this change's own bullet (4c) accurately reflects what
      was actually built; update the roadmap text if anything diverged
      during implementation (per §20.5's own discipline). Reviewed: no
      divergence - the roadmap's own 4c bullet ("Callback data model +
      registry... Static and unit-testable, no `GraphBuilder` wiring
      yet") matches what was built exactly. No roadmap edit needed.
