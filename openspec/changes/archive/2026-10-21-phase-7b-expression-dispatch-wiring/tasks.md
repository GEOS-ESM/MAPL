## 1. `MAPLStateItemFlag.F90`: register the new variant (design.md Context/D1)

- [x] 1.1 Add `MAPL_STATEITEM_EXPRESSION = MAPL_StateItem_Flag(15)` to
      `superstructure/generic/graph/MAPLStateItemFlag.F90` (next unused
      value after `MAPL_STATEITEM_VERTICALGRID = 14`), under the
      "Refinements" block, with its own `public ::` declaration.
- [x] 1.2 Add a `case (15); name = 'EXPRESSION'` branch to
      `flag_to_string` and a matching `case ('EXPRESSION'); flag =
      MAPL_STATEITEM_EXPRESSION` branch to `flag_from_string`.
- [x] 1.3 Short header-comment note next to the new parameter citing
      this change, matching the existing per-variant comment style.

## 2. `GraphStateItem.F90`: carry the expression string (design.md D3)

- [x] 2.1 Add `character(:), allocatable :: declared_expression` to
      `GraphStateItem`'s private component list, immediately adjacent
      to `declared_item_type`/`declared_restart_mode` (design.md D3:
      "same kind of materialization input").
- [x] 2.2 Add `get_declared_expression`/`set_declared_expression`
      accessors, following `get/set_declared_restart_mode`'s exact
      pattern (allocatable-output-only-if-allocated get; deallocate-
      then-reallocate set).
- [x] 2.3 Add both new procedure bindings to the type's `contains`
      block, immediately after `get/set_declared_restart_mode`.

## 3. `ExpressionTransform`: the new graph-native Transform (design.md D4/D5)

- [x] 3.1 Create
      `superstructure/generic/graph/transforms/ExpressionTransform.F90`,
      module `mapl_ExpressionTransform_mod`, type `ExpressionTransform`
      extending `Transform` (`mapl_Transform_mod`), following
      `UnitsConverterTransform.F90`'s file structure (header comment
      explaining independence from legacy `EvalTransform`/
      `ExtensionTransform`, per design.md D5; holds `expression`
      (allocatable string) and the `StringVector` of referenced
      variable names used as port names).
- [x] 3.2 Constructor `new_ExpressionTransform(expression, rc)`:
      `parser_variables_in_expression(expression, rc)`
      (`mapl_StateArithmeticParser_mod` - confirm module name during
      implementation) to get the referenced-name set, call
      `declare_input_port(name)` once per referenced name (design.md
      D4 - name itself is the port name, no `expected_kind`), and
      `declare_output_port('destination_field')`.
- [x] 3.3 `compute(this, inputs, outputs, rc)` (design.md D5): build a
      fresh `ESMF_State` via `ESMF_StateCreate`; for each of `this`'s
      own declared input port names, `inputs%at(name)%get_field(_RC)`
      and `ESMF_StateAdd(state, [field], _RC)` under that same name;
      read `outputs%at('destination_field')%get_field(_RC)`; call
      `StateEval(state, this%expression, destination_field, rc)`
      (`mapl_StateArithmeticParser_mod::StateEval`); `ESMF_StateDestroy`
      the assembled state before returning.
- [x] 3.4 Add `transforms/ExpressionTransform.F90` to
      `superstructure/generic/graph/CMakeLists.txt`, immediately after
      `transforms/BracketTimeInterpolationTransform.F90`, and confirm
      `MAPL.generic`'s existing `DEPENDENCIES ... MAPL.state` entry
      already covers the new `use mapl_StateArithmeticParser_mod`
      (design.md Context - already confirmed, no CMake change
      expected; flag if wrong).

## 4. `GraphBuilder_Advertising_smod.F90`: set `declared_expression` (design.md D3)

- [x] 4.1 In `advertise_one`'s per-item setup (alongside the existing
      `set_declared_item_type`/`set_declared_geom`/`set_declared_
      restart_mode` calls), add: when `var_spec%itemType ==
      MAPL_STATEITEM_EXPRESSION .and. allocated(var_spec%expression)`,
      `call payload%set_declared_expression(var_spec%expression)`.
- [x] 4.2 One-line comment above the insertion citing this change and
      design.md D3.

## 5. `GraphBuilder_ConnectionResolution_smod.F90`: new expression hook (design.md D2)

- [x] 5.1 Add `graphbuilder_run_expression_hook(this)` interface
      declaration to `GraphBuilder.F90`'s `GraphBuilder` type (new
      `procedure, nopass :: run_expression_hook =>
      graphbuilder_run_expression_hook` binding, header comment citing
      design.md D2/D7 and following `run_geometry_hook`'s own comment
      style for "what triggers this, what it does, what it does not
      do").
- [x] 5.2 Add the corresponding `module subroutine
      graphbuilder_run_expression_hook(this)` interface body to
      `GraphBuilder.F90`'s module-procedure interface block
      (immediately after `graphbuilder_run_geometry_hook`'s own
      interface body, matching that block's existing per-hook
      grouping).
- [x] 5.3 Implement `graphbuilder_run_expression_hook` in
      `GraphBuilder_ConnectionResolution_smod.F90` (co-located with
      `find_export_node_id`/`resolve_callback_destination`, the
      procedures it reuses): iterate this component's own graph nodes,
      select `StateItemNode`s whose `get_payload()%
      get_declared_item_type() == MAPL_STATEITEM_EXPRESSION`, and for
      each: skip (idempotent no-op) if the resource-index key
      `'EXPRHOOK:' // node_id%to_string()` already exists (design.md D2
      step 6); otherwise read `get_declared_expression()`, call
      `parser_variables_in_expression`, resolve each referenced name via
      `find_export_node_id(this, SELF_COMPONENT_NAME, name, ...)`
      (design.md D2 step 3), reporting any miss through the same
      `GraphResolutionEntry`/unresolved-reporting convention
      `graphbuilder_unresolved_import_node_ids` already uses (new
      optional `unresolved` output argument, following that
      procedure's own signature shape) rather than `_FAIL`ing outright.
- [x] 5.4 On a fully-resolved reference set: build one
      `ExpressionTransform(expression)`, `graph%register_node` it,
      `graph%bind_port` once per referenced name (input) plus once for
      `'destination_field'` (output, bound to the expression item's own
      `node_id`), then `graph%add_dependency(net_id, transform_id,
      node_id)` and one `graph%add_dependency(net_id, resolved_export_
      id, transform_id)` per referenced name (design.md D2 step 5), and
      finally `graph%add_resource_index('EXPRHOOK:' //
      node_id%to_string(), transform_id, _RC)` to record completion.
- [x] 5.5 Add a header comment on the new procedure citing design.md
      D2 for the "why a dedicated hook, not `resolve_one`/`build_chain`"
      rationale, mirroring `graphbuilder_advertise_geometry`'s own
      citation style.

## 6. `is_ordinary_resolvable_import`: narrow the exclusion (design.md D6)

- [x] 6.1 In `is_ordinary_resolvable_import`
      (`GraphBuilder_ConnectionResolution_smod.F90`, ~line 1973-1986),
      remove the `.or. declared_type == MAPL_STATEITEM_EXPRESSION` arm;
      leave the `WILDCARD`/`SERVICE` arms unchanged.
- [x] 6.2 Update that procedure's own header comment to record the
      narrowing, citing this change and design.md D6 (matching task
      6.8's own prior "no mechanism moved it there" comment style, now
      noting that a mechanism exists for `EXPRESSION` specifically).

## 7. Lifecycle wiring: invoke the new hook (design.md D7)

- [x] 7.1 In `initialize_accept_transfer.F90`, add `call
      gb%run_expression_hook(this)` immediately after `call
      gb%run_connect_hook(this)` (design.md D7's chosen call site and
      ordering - after `process_connections`/`run_connect_hook`, before
      `registry%propagate_exports`).
- [x] 7.2 Add a comment at that call site citing design.md D7's timing
      rationale (every same-component sibling export guaranteed
      advertised by this point; why not `initialize_advertise.F90`).

## 8. Unit tests: `ExpressionTransform` (design.md D5)

- [x] 8.1 Add
      `superstructure/generic/graph/tests/Test_ExpressionTransform.pf`,
      registered in `superstructure/generic/graph/tests/CMakeLists.txt`
      immediately after `Test_BracketTimeInterpolationTransform.pf`.
- [x] 8.2 Construct two or three real `ESMF_Field`s with known values,
      wrap each in a `GraphStateItem`, bind into a
      `PortNameGraphStateItemMap` under its own referenced-variable
      name, and a destination `ESMF_Field`/`GraphStateItem` under
      `'destination_field'`; call `compute()` directly (no
      `GraphBuilder`/`ComponentGraph` involved), mirroring Phase 7a's
      own `Test_BracketTimeInterpolationTransform.pf` idiom.
- [x] 8.3 Assert the destination field's values equal the hand-computed
      expression result (e.g. `expression = "(A + B)/C"` against known
      `A`/`B`/`C` values).
- [x] 8.4 Case: an expression referencing only one variable (degenerate
      N=1 case) still resolves and evaluates correctly.
- [x] 8.5 Case: an expression referencing three or more variables
      evaluates correctly (confirms real N:1, not just N=2).

## 9. Unit tests: `graphbuilder_run_expression_hook` (design.md D2/D6)

- [x] 9.1 Add test cases to
      `superstructure/generic/graph/tests/Test_GraphBuilder_
      ConnectionResolution.pf` (or a new dedicated file if that one's
      fixture setup doesn't fit - confirm during implementation):
      construct a component with one `EXPRESSION`-typed export (two or
      more referenced names) and matching sibling `FIELD` exports for
      each referenced name, run `run_expression_hook`, and assert: one
      `ExpressionTransform`-backed `TransformGraphNode` was created,
      every input port is bound to the correct sibling export's
      `NodeId`, and the output port is bound to the expression item's
      own `NodeId`.
      Completion note: implemented directly in the existing
      `superstructure/generic/tests/Test_GraphBuilder_
      ConnectionResolution.pf` (its established inline `.pf` pattern
      fit fine; no new file needed) as
      `test_expression_hook_wires_transform_to_sibling_exports`, three
      referenced names (A/B/C). Found and fixed two real bugs in
      `resolve_expression_item`/`graphbuilder_run_expression_hook` while
      making this test pass under both compilers: (1) growing a
      deferred-length `CHARACTER` array via `[source_names, name]`
      aborts under gfortran at runtime ("Different CHARACTER lengths in
      array constructor") once referenced names differ in length -
      `source_names` is now a `StringVector` (consistent with
      `referenced_names`'s own type), not a `CHARACTER(:), allocatable
      :: (:)` array; (2) `graphbuilder_run_expression_hook`'s own local
      `node_ids` (`type(NodeIdSet)`, holding the result of
      `graph%get_node_ids()`) was missing the `target` attribute present
      on every structurally identical local elsewhere in this file
      (e.g. `import_ports`) - NAG caught this at runtime
      ("Dangling pointer A%TREE used as argument to intrinsic function
      ASSOCIATED... RETURNed from procedure
      MAPL_NODEIDSET_MOD:SET_FTN_BEGIN", gFTL2 Set's `ftn_begin`
      requires a `target, intent(in)` actual argument) while gfortran
      silently tolerated the dangling association. Fixed by adding
      `target` to `node_ids`'s declaration.
- [x] 9.2 Case: re-running `run_expression_hook` for the same component
      a second time does not create a second transform node or
      duplicate port bindings (idempotency, design.md D2 step 6).
      Completion note: `test_expression_hook_is_idempotent`.
- [x] 9.3 Case: an expression referencing a name with no matching
      sibling export is reported as unresolved (not silently dropped,
      not a hard `_FAIL`) and no transform node is built for that item.
      Completion note: `test_expression_hook_reports_unresolved_reference`.
- [x] 9.4 Case: after `run_expression_hook` has run for a component,
      `is_ordinary_resolvable_import` (or
      `graphbuilder_unresolved_import_node_ids`, whichever is the more
      direct observation point) no longer reports that component's own
      resolved `EXPRESSION` item as unconditionally unresolved (design.md
      D6), while a `WILDCARD`/`SERVICE`-typed import on the same
      component still is (negative control).
      Completion note: `test_resolved_expression_no_longer_excluded_from_gate`
      (renamed from a longer name that exceeded gfortran's 63-character
      symbol-name limit). Observes the gate through
      `graphbuilder_unresolved_import_node_ids`, the only reachable call
      site (`is_ordinary_resolvable_import` is private). Verified the
      actual post-D6 polarity by running with a temporary debug print
      before finalizing assertions, rather than assuming it: once
      `EXPRESSION` is no longer unconditionally excluded, it falls
      through to the scan's ordinary Connection-based check - which
      never succeeds for it (expression resolution wires dependency
      edges directly, outside any `Connection`, design.md D1) - so
      `'expr'` *does* appear in the unresolved-imports report post-D6
      (the correct, harmless signal per design.md's own Risk note that
      this list membership has no real consequence for an item that is
      never mirrored/propagated). `'wild'` (WILDCARD, untouched by this
      change) remains excluded from the scan throughout, unaffected -
      the negative control.

Group verification note (all of 9.1-9.4, both compilers): full
`MAPL.generic.*` suite (all 8 ctest targets, including the pre-existing
`expression`/`expression_match`/`expression_defer_geom`
`MAPL.generic.scenarios` fixtures) passes with zero failures under both
gfortran and NAG (`nag-stack/7.2.41`) after the two bugfixes recorded
under 9.1. NAG requires `module load nag-stack/7.2.41` specifically
(not a bare `nag`/`mpi`/`baselibs` chain) to pick up a working
`NAG_KUSARI_FILE`/license and matching MPI/baselibs combination.

## 10. Fixture: `Test_Scenarios` end-to-end coverage (proposal.md, design.md Risks)

- [x] 10.1 Confirm the existing `expression`/`expression_match`/
      `expression_defer_geom` fixtures
      (`superstructure/generic/tests/scenarios/`) run correctly under
      `GRAPH_MODE_GRAPH` (or `GRAPH_MODE_BOTH`, whichever
      `Test_Scenarios.pf`'s existing parameter list already exercises
      for other fixtures) with no change to their own YAML; if a gap
      surfaces (e.g. a referenced variable needing extension-chain
      conversion - design.md Non-Goals), record it in this task's own
      completion note rather than expanding scope silently.
      Completion note: `Test_Scenarios.pf`'s parameter list does not
      exercise ANY fixture under `GRAPH_MODE_GRAPH`/`GRAPH_MODE_BOTH` -
      confirmed by direct reading, it has no `set_graph_mode` call at
      all and always runs under the module default (`GRAPH_MODE_
      REGISTRY`). None of this tree's three `GRAPH_MODE_GRAPH`/`BOTH`-
      capable harnesses (`Test_GraphBuilderEquivalence.pf`,
      `Test_GraphEquivalenceHarness.pf`,
      `Test_GraphModeDemonstration.pf`) reference the `expression`/
      `expression_match`/`expression_defer_geom` fixtures either - they
      use their own, separate fixture roots (`graph-scenarios/`). So
      "run under GRAPH_MODE_GRAPH" cannot be confirmed for these three
      fixtures through any existing harness; doing so would mean
      building new scenario-mode test machinery, not merely running
      existing coverage - out of this task's own "confirm... with no
      change" framing. This absence is not specific to the expression
      fixtures or to this change: it matches the project's own already-
      documented state (`docs/generic/ARCHITECTURE.md` §2.3/
      `docs/graph/ARCHITECTURE.md` §6's "demonstrate before replace"
      note) that `GRAPH_MODE_GRAPH` is "not yet exercised end-to-end
      against any real fixture by design." Separately, direct tracing
      of `resolve_expression_item`/`graphbuilder_populate_state`/
      `ComponentGraph_DemandDrivenUpdate.F90`/`ExtensionMaterialization.
      F90` this session found no call site that ever materializes a
      real `ESMF_Field` for an `EXPRESSION`-typed item's own destination
      node (`materialize_field_extension`/`FieldCreate` are called for
      ordinary extension-chain nodes only) - so even a new `GRAPH_MODE_
      GRAPH` scenario test built today would hit `ExpressionTransform%
      compute()`'s `output_item%get_field()` assertion on an
      unallocated field. This is a pre-existing, general "who
      materializes a TransformGraphNode's own output field" gap
      spanning every `Transform` subclass already landed (not
      introduced by, or specific to, `ExpressionTransform`) - fixing it
      is squarely outside design.md's Goals (wiring: resolve
      references, build the transform, bind ports) and would be new,
      separate work.
- [x] 10.2 If 10.1 passes cleanly, confirm (and record) that no new
      fixture file is needed - the three existing fixtures are
      sufficient forcing-function coverage for this change's own scope,
      matching proposal.md's own expectation.
      Completion note: N/A as stated (10.1 did not pass cleanly - no
      harness exercises these fixtures under `GRAPH_MODE_GRAPH`/`BOTH`
      at all, see 10.1). The three existing fixtures remain sufficient
      for this change's own actual scope: they already exercise and
      pass the new `graphbuilder_run_expression_hook` wiring indirectly
      (the pre-existing `MAPL.generic.scenarios` run, REGISTRY mode,
      confirmed passing under task 9's group verification note) and,
      combined with task 9's direct unit coverage of the hook itself
      and task 8's direct unit coverage of `ExpressionTransform%
      compute()`'s evaluation correctness, satisfy proposal.md's stated
      task-10 test-coverage intent (hook + transform unit coverage) in
      full. No new fixture file is needed; what is missing is scenario-
      level `GRAPH_MODE_GRAPH` test *infrastructure*, not fixture
      content - see 10.3.
- [x] 10.3 If 10.1 surfaces a real gap within this change's own scope
      (same-component direct sibling references, no further
      conversion needed), fix it and re-run; if the gap is out of scope
      (design.md Non-Goals), document it as a known limitation in this
      task's completion note and leave the fixture's own
      `GRAPH_MODE_GRAPH` coverage as a documented follow-on instead of
      expanding this change.
      Completion note: the gap found in 10.1 (no existing harness runs
      these fixtures under `GRAPH_MODE_GRAPH`/`BOTH`; no production code
      materializes an `EXPRESSION` item's own destination field) is out
      of this change's own scope - it is a general, pre-existing `GRAPH_
      MODE_GRAPH` maturity limitation (documented project-wide, not
      introduced here) affecting every fixture and every `Transform`
      subclass, not a same-component direct-sibling-reference defect
      within design.md's actual Goals. Documented here as a known
      limitation; building scenario-level `GRAPH_MODE_GRAPH`/`BOTH`
      coverage for the `expression`/`expression_match`/
      `expression_defer_geom` fixtures, and the general TransformGraphNode-
      output-materialization mechanism it would depend on, is left as a
      documented follow-on, not undertaken as part of this change.

## 11. Regression and verification

- [x] 11.1 Build MAPL (NAG, per the active compiler-switching skill)
      with all modified/new files included - clean build, zero
      warnings/errors introduced by this change's own files.
      Completion note: `cmake --build . --target build-tests -j 8` in
      `nag/`, under `nag-stack/7.2.41` - `[100%] Built target
      build-tests`, zero errors, zero warnings from any file this
      change touches (the one pre-existing NAG "Questionable" diagnostic
      in the build log, `Test_GraphBuilder_ConvergenceCheck.pf` line
      237, is unrelated to this change, confirmed not modified by it).
      Also re-verified gfortran (`gfortran-stack`, `gfortran/`):
      `cmake --build . --target build-tests -j 8` - `[100%] Built
      target build-tests`, zero errors.
- [x] 11.2 Run `ctest -R "^MAPL.generic.graph$"` - 0 failures.
      Completion note: NAG - 1/1 passed (0.53s/0.25s across two runs).
      gfortran - 1/1 passed (0.89s/0.31s across two runs).
- [x] 11.3 Run `ctest -R "^MAPL.generic"` (full `MAPL.generic.*` label
      set) - all suites pass, zero regressions, paying particular
      attention to `MAPL.generic.scenarios` (the `expression*` fixtures
      - task 10) and `MAPL.generic.aspects`/`MAPL.generic.components`
      (legacy `ExpressionClassAspect`/`EvalTransform` coverage,
      confirmed unaffected per proposal.md's own "no change to legacy"
      claim).
      Completion note: NAG - all 8 `MAPL.generic.*` targets passed,
      100%, 37.44s total (`.graph`, `.scenarios` incl. the three
      expression fixtures in their existing REGISTRY-mode form,
      `.transforms`, `.vertical`, `.aspects` incl. legacy
      `ExpressionClassAspect`, `.components` incl. legacy
      `EvalTransform` coverage, `.components.quiet`, `.core`). gfortran -
      same 8/8, 100%, 32.36s total. Zero regressions either compiler.
- [x] 11.4 Confirm no existing non-`EXPRESSION` connection in any
      current fixture is affected by the Section 6 gate narrowing
      (every existing `Test_Scenarios`/`graph-scenarios` fixture still
      passes unchanged - the narrowing only removes exclusion for
      `EXPRESSION`, leaving every other item class's own gate
      unaffected).
      Completion note: confirmed by 11.3's own full-suite pass (every
      `Test_Scenarios`/`graph-scenarios` fixture, both compilers, zero
      failures) plus task 9.4's direct unit-level negative control
      (`test_resolved_expression_no_longer_excluded_from_gate`:
      `WILDCARD`-typed `'wild'` remains excluded from the scan
      throughout, unaffected by the narrowing, while `EXPRESSION`-typed
      `'expr'` is the only item class whose exclusion changed).
- [x] 11.5 Run `openspec validate phase-7b-expression-dispatch-wiring
      --strict` - change is valid.
      Completion note: `openspec validate phase-7b-expression-dispatch-wiring
      --strict` -> "Change 'phase-7b-expression-dispatch-wiring' is
      valid".
