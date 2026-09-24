## 1. DriverResolver abstraction

- [x] 1.1 In `superstructure/generic/graph/MethodInvocationAdapter.F90`,
      add abstract type `DriverResolver` with one deferred function
      `resolve(this, driver_key, rc) result(driver)` returning
      `class(GriddedComponentDriver), pointer` (design.md Decisions -
      "A new DriverResolver abstraction decouples the real
      GridCompPhaseInvoker from OuterMetaComponent"). Requires importing
      `mapl_GriddedComponentDriver_mod` here for the return type only —
      confirm this does not introduce a dependency on
      `mapl_OuterMetaComponent_mod` (it must not).
- [x] 1.2 `Test_DriverResolver.pf` (or fold into 2.x's test file): a
      synthetic `DriverResolver` test double returning a driver wrapping
      a minimal real `ESMF_GridComp` for a known key and failing for an
      unknown one, confirming the abstract interface's shape is usable
      standalone. Folded into `Test_GridCompDriverPhaseInvoker.pf`
      (`RecordingDriverResolver`).

## 2. Real GridCompPhaseInvoker

- [x] 2.1 In `superstructure/generic/graph/GridCompMethodInvocation.F90`,
      change `phase_name : character(:), allocatable` to
      `phase_idx : integer`; rename `gridcomp_get_phase_name`/
      `get_phase_name` to `gridcomp_get_phase_idx`/`get_phase_idx`;
      update `new_GridCompMethodInvocation`'s second positional argument
      and its call to `invoker%invoke_phase` accordingly (design.md
      Decisions - "GridCompMethodInvocation gains phase_idx in place of
      phase_name").
- [x] 2.2 In `superstructure/generic/graph/MethodInvocationAdapter.F90`,
      update `gridcomp_invoke_phase_interface`'s `phase_name : character`
      parameter to `phase_idx : integer`.
- [x] 2.3 Create `superstructure/generic/graph/
      GridCompDriverPhaseInvoker.F90` (or similar name): concrete
      `type, extends(GridCompPhaseInvoker) :: GridCompDriverPhaseInvoker`
      holding `class(DriverResolver), allocatable :: resolver` and an
      `ESMF_Method_Flag` indicating which of `initialize`/`run`/
      `finalize` this instance calls. `invoke_phase(this, driver_key,
      phase_idx, arguments, bindings, clock, rc)` resolves the driver via
      `this%resolver%resolve(driver_key, rc)`, then calls the resolved
      driver's `initialize`/`run`/`finalize` with `phase_idx=phase_idx`
      per the held method flag — nothing else touches `arguments`/
      `bindings` (REQ-MTH-003: the driver call itself is the only ESMF
      interaction).
- [x] 2.4 `Test_GridCompDriverPhaseInvoker.pf`: using a real, minimal
      ESMF test `GridComp` (no-op SetServices phases, following existing
      test-gridcomp precedent e.g.
      `superstructure/generic/tests/gridcomps/`) wrapped in a real
      `GriddedComponentDriver`, and a synthetic `DriverResolver` test
      double returning that driver for a known key: confirm invoking
      through `GridCompDriverPhaseInvoker` for each of
      initialize/run/finalize actually exercises the underlying driver's
      corresponding phase (e.g. via a phase-invocation counter/flag in
      the test GridComp), with no separately-reimplemented ESMF call
      path; confirm an unresolvable `driver_key` propagates the
      resolver's failure rather than being swallowed.
      Implementation notes (discovered while implementing, not
      anticipated in design.md): (1) lives in
      `superstructure/generic/tests/` instead of
      `superstructure/generic/graph/tests/` - it needs a real, working
      `ESMF_Clock`/`ESMF_GridComp` lifecycle, and this suite's own
      `EXTRA_INITIALIZE` (`mapl_pFUnit_Initialize_mod`, calling
      `MAPL_initialize()`) already sets
      `defaultDefaultCalKind=ESMF_CALKIND_GREGORIAN`, unlike the
      graph/tests suite's deliberately minimal `GraphCoreTestInit`; (2)
      `ESMF_GridCompSetEntryPoint` alone does not start the component's
      VM - a real `ESMF_GridCompSetServices` call is required (ESMF's
      own diagnostic: "No VM was started for this Component - missing
      SetServices() call?"); (3) uses `@Test(type=ESMF_TestMethod,
      npes=[1])`, not `MpiTestMethod`/`npes=[0]`, matching
      `Test_GraphBuilder.pf`'s own documented precedent for real ESMF
      collective calls under this suite's `MAX_PES 4` - a plain
      `MpiTestMethod` sub-communicator does not reconfigure ESMF's own
      default VM.

## 3. OuterMetaComponentDriverResolver

- [x] 3.1 Create `superstructure/generic/
      OuterMetaComponentDriverResolver.F90`: concrete
      `type, extends(DriverResolver) :: OuterMetaComponentDriverResolver`
      holding `class(OuterMetaComponent), pointer :: owner`.
      `resolve(this, driver_key, rc)` returns `this%owner%get_user_gc_driver()`
      when `driver_key` is `SELF_COMPONENT_NAME` ('<self>', reusing
      `GraphBuilder.F90`'s existing sentinel — export it from
      `GraphBuilder.F90` or promote it to a small shared constant module
      if needed to avoid a new circular dependency) or empty; otherwise
      returns a pointer to `this%owner%children%at(driver_key)`, asserting
      loudly if not found (design.md Decisions - "The concrete
      DriverResolver implementation lives in superstructure/generic/").
      Confirm accessing `children` (currently `private` on
      `OuterMetaComponent`) from this new file requires either a new
      accessor returning `class(GriddedComponentDriverMap), pointer` on
      `OuterMetaComponent`, or placing this type as a new submodule of
      `mapl_OuterMetaComponent_mod` instead of a fully separate module —
      pick whichever keeps `children` un-exposed to any wider audience
      than this resolver and `GraphBuilder.F90` already have, and record
      the choice made in a short comment at the top of the new file.
- [x] 3.2 `Test_OuterMetaComponentDriverResolver.pf`: against a real
      `OuterMetaComponent` (own driver set, one child added), confirm
      `<self>`/empty key resolves to the own driver, a known child name
      resolves to that child's driver, and an unknown key fails loudly
      (spec scenarios "Own-driver key resolves...", "Child-driver key
      resolves...", "Unresolvable driver key fails loudly").

## 4. Trigger/advance discipline (REQ-MTH-003a)

- [x] 4.1 Create `superstructure/generic/graph/MethodInvocation.F90`
      (`mapl_MethodInvocation_mod`): subroutine
      `invoke_on_default_network(graph, node_id, rc, clock)` implementing
      REQ-MTH-003a's three steps against the node's bound `IN`/`INOUT`
      arguments (pull, via `graph%update()`) and bound `OUT`/`INOUT`
      arguments (advance, via each bound `NodeId`'s `NodeRevision%advance()`,
      reached through whatever existing `ComponentGraph`/`GraphStateItem`
      accessor already exposes a node's `NodeRevision` — reuse, do not
      duplicate) (design.md Decisions - "REQ-MTH-003a's trigger/advance
      discipline is one new graph-neutral procedure").
- [x] 4.2 `Test_MethodInvocation.pf`: synthetic `ComponentGraph` (Phase
      1-3 style, no ESMF) with a `MethodGraphNode` bound to one `IN`, one
      `OUT`, and one `INOUT` argument backed by `StateItemNode`s on the
      default network: confirm all bound `IN`/`INOUT` arguments are
      current before the attached (synthetic) adapter's `invoke()` fires;
      confirm bound `OUT`/`INOUT` revisions advance only after a
      successful invocation (spec scenarios "Bound inputs are made
      current before invocation", "Bound outputs are advanced after
      successful invocation"); confirm a failed invocation (synthetic
      adapter returns non-zero `rc`) leaves bound `OUT`/`INOUT` revisions
      unadvanced (spec scenario "Failed invocation does not advance
      bound outputs").
      Implementation note (gfortran-only failure, found and fixed during
      review, not anticipated in design.md): a shared `make_fixture()`
      test helper returning a freshly-built `ComponentGraph` through an
      `intent(out)` dummy argument reliably lost internal
      `StateItemMemberMap`/iterator state under gfortran (NAG was
      unaffected) - the same "state does not survive returning up the
      call chain" bug class `ComponentGraph_DemandDrivenUpdate.F90`/
      `DependencyNetwork.F90` already document (there triggered by
      literal recursion; here by an ordinary call/return across a
      procedure boundary for the graph object itself). Fixed by
      inlining each test's own fixture directly (no shared helper
      returning the graph) - confirmed correct under both NAG and
      gfortran. `MethodInvocation.F90`'s own `pull_bound_inputs`/
      `advance_bound_outputs` were also hardened defensively to a
      collect-then-process shape (matching this module family's
      existing explicit-worklist precedent) while diagnosing this,
      independent of the actual root cause.

## 5. REQ-MTH-011 convergence: hard-error unresolved-import check

- [x] 5.1 In `superstructure/generic/GraphBuilder.F90`'s
      `graphbuilder_run_connect_hook`, after `graphbuilder_freeze`
      succeeds, add a call to the existing `check_unsatisfied_imports`
      requesting `unresolved_imports`; if the returned list is non-empty,
      raise an `_ASSERT`-level failure (still caught and downgraded to a
      logged failure by this hook's own existing
      `report_if_failed`/try-catch convention — do not change that outer
      safety net) instead of only logging a warning (design.md Decisions
      - "REQ-MTH-011 step (c) convergence"). Implemented as a new
      `assert_converged(unresolved, rc)` helper (its own real `rc`
      dummy, `_FAIL` inside it) called from
      `graphbuilder_run_connect_hook` and reported through
      `report_if_failed` exactly like the hook's other three steps -
      not an inline `_ASSERT` with no `rc` of its own to propagate.
- [x] 5.2 `Test_GraphBuilder_ConvergenceCheck.pf` (or extend an existing
      `GraphBuilder` connect-hook test): a component with a declared,
      never-resolvable required import reaches `run_connect_hook` and
      the graph ends up frozen with that import still unresolved;
      confirm the new hard-error path fires (spec scenario "Cycle
      reaching the iteration limit without convergence fails
      explicitly") and that a component whose imports do all resolve by
      the time the graph freezes proceeds normally (spec scenario
      "Cycle converging within the iteration limit proceeds normally").
      Confirm `report_if_failed`'s existing logging-only-continue
      behavior at the `run_connect_hook` call site is unchanged for
      external callers (real component `Initialize` still proceeds; the
      failure is visible only via the logger, exactly as any other
      `GraphBuilder`-internal failure already is today).
- [x] 5.3 Confirm (regression, no code change expected) that
      `graphbuilder_run_connect_hook` invoked a second time on an
      already-frozen graph (the real ESMF-driven second ACCEPT_TRANSFER
      pass) remains a no-op and does not re-run the new check a second
      time with stale state (spec scenario "Cycle stops once an
      iteration makes no further progress").

## 6. Verification

- [x] 6.1 Add all new source files
      (`MethodInvocationAdapter.F90`'s additions are in-place edits, not
      new files) to `superstructure/generic/graph/CMakeLists.txt` and
      `superstructure/generic/CMakeLists.txt` as appropriate, and all new
      test files to their respective `tests/CMakeLists.txt`.
- [x] 6.2 Build MAPL with NAG (`nag` build dir, per
      `.opencode/skills/mapl-build`).
- [x] 6.3 Run the full `ctest` suite; confirm no regressions outside the
      new test files (compare against the pre-existing pass/fail
      baseline) — pay particular attention to any existing
      `GraphBuilder`/`OuterMetaComponent` real-configuration test that
      exercises `GENERIC_INIT_ACCEPT_TRANSFER` twice, given task 5's
      behavior change. Full suite (NAG): 68/75 passed, the same 7
      pre-existing failures (4 missing-data, excluded by
      `-L ESSENTIAL`; 3 stale external libs) as before this change - no
      new failures. `MAPL.generic.graph` and `MAPL.generic.components`
      both pass. Also verified with gfortran (`gfortran` build dir):
      full `MAPL.generic.*` suite (graph/scenarios/transforms/vertical/
      aspects/components/core) 7/7 passed after fixing the
      gfortran-only `Test_MethodInvocation.pf` issue noted under task
      4.2, and `-L ESSENTIAL` shows only the same 3 pre-existing
      missing-data failures.
- [x] 6.4 Run the specific new test labels individually with `-v` to
      confirm each new test case actually executed and passed.
- [x] 6.5 `openspec validate griddedcomponentdriver-integration-lifecycle
      --strict` reports the change valid.
