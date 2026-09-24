## 1. AccessSpec (general MAPL concept, REQ-CB-005)

- [x] 1.1 Create `superstructure/generic/graph/AccessSpec.F90`
      (`mapl_AccessSpec_mod`), following `MAPLStateItemFlag.F90`'s exact
      pattern: a derived type `AccessSpec` wrapping a private integer
      code, `parameter` constants `MAPL_ACCESS_IN`, `MAPL_ACCESS_OUT`,
      `MAPL_ACCESS_INOUT`, `MAPL_ACCESS_UNSPECIFIED`,
      `operator(==)`/`operator(/=)`, and a `to_string()` method
      (design.md Decisions - "AccessSpec follows the existing
      MAPL_StateItem_Flag pattern exactly").
- [x] 1.2 `Test_AccessSpec.pf`: each constant compares equal to itself
      and unequal to every other constant; `to_string()` returns a
      distinct, non-empty name for each constant.

## 2. ArgumentSpec / ArgumentSpecMap

- [x] 2.1 Create `superstructure/generic/graph/ArgumentSpec.F90`
      (`mapl_ArgumentSpec_mod`), mirroring `PortSpec.F90`'s shape: name,
      `AccessSpec`, optional expected `MAPL_StateItem_Flag` kind
      constraint (`is_kind_constrained()`/`get_expected_kind()`), plus
      `get_name()`/`get_access()`. Two constructors mirroring
      `PortSpec`'s `new_PortSpec_any`/`new_PortSpec_with_kind` pair.
- [x] 2.2 Create `superstructure/generic/graph/containers/
      ArgumentSpecMap.F90` (`mapl_ArgumentSpecMap_mod`), a gFTL map
      (`Key = __CHARACTER_DEFERRED`, `T = ArgumentSpec`, `Map =
      ArgumentSpecMap`), mirroring `containers/PortSpecMap.F90` exactly
      (own module, own `map/template.inc` include, for the same
      implicit-none/private conflict reason documented there).
- [x] 2.3 `Test_ArgumentSpec.pf`: construct unconstrained and
      kind-constrained specs; `get_access()`/`get_name()` return what
      was constructed; `is_kind_constrained()`/`get_expected_kind()`
      behave correctly for both cases.

## 3. MethodInvocationAdapter abstraction and two concrete adapters

- [x] 3.1 Create `superstructure/generic/graph/
      MethodInvocationAdapter.F90` (`mapl_MethodInvocationAdapter_mod`):
      abstract type `MethodInvocationAdapter` with one deferred
      `invoke(this, arguments, bindings, clock, rc)` (`arguments:
      type(ArgumentSpecMap), intent(in)`; `bindings:
      type(StateItemMemberMap), intent(in)`; `clock: type(ESMF_Clock),
      optional, intent(in)`; `rc: integer, optional, intent(out)`). No
      dependency on `mapl_MethodGraphNode_mod` (design.md Decisions -
      "MethodInvocationAdapter is a decoupled abstract type").
- [x] 3.2 In the same module (or a small sibling module reused by both
      concrete adapters below), declare the two injectable abstract
      interfaces:
      - `GridCompPhaseInvoker`: deferred `invoke_phase(this, driver_key,
        phase_name, arguments, bindings, clock, rc)`.
      - `StateMethodInvoker`: deferred `invoke_method(this,
        state_node_id, method_name, arguments, bindings, rc)` (no
        clock, per REQ-CB-001).
- [x] 3.3 Create `superstructure/generic/graph/
      GridCompMethodInvocation.F90` (`mapl_GridCompMethodInvocation_mod`):
      `type, extends(MethodInvocationAdapter) :: GridCompMethodInvocation`
      holding `driver_key` (`character(:), allocatable`), `phase_name`
      (`character(:), allocatable`), and `class(GridCompPhaseInvoker),
      allocatable :: invoker`, injected at construction. `invoke()`
      asserts `invoker` is allocated, then calls `invoker%invoke_phase`
      with `this%driver_key`/`this%phase_name` plus the passed
      arguments/bindings/clock - nothing else (REQ-MTH-003).
- [x] 3.4 Create `superstructure/generic/graph/
      StateMethodInvocation.F90` (`mapl_StateMethodInvocation_mod`):
      `type, extends(MethodInvocationAdapter) :: StateMethodInvocation`
      holding `state_node_id` (`type(NodeId)`), `method_name`
      (`character(:), allocatable`), and `class(StateMethodInvoker),
      allocatable :: invoker`, injected at construction. `invoke()`
      asserts `invoker` is allocated, then calls `invoker%invoke_method`
      with `this%state_node_id`/`this%method_name` plus the passed
      arguments/bindings - nothing else (REQ-MTH-003). `clock` argument
      accepted (interface uniformity with `MethodInvocationAdapter`) but
      unused/ignored, since the underlying `StateMethodInvoker` call has
      none.
- [x] 3.5 `Test_GridCompMethodInvocation.pf`: a synthetic
      `GridCompPhaseInvoker` test double records the
      driver_key/phase_name/arguments/bindings/clock it was called
      with; confirm `invoke()` calls it exactly once with the expected
      values and returns success; confirm `invoke()` fails loudly if
      `invoker` was never set.
- [x] 3.6 `Test_StateMethodInvocation.pf`: mirrors 3.5 for
      `StateMethodInvocation`/`StateMethodInvoker`, including confirming
      no clock-related failure occurs when no clock is supplied.

## 4. MethodGraphNode

- [x] 4.1 Create `superstructure/generic/graph/MethodGraphNode.F90`
      (`mapl_MethodGraphNode_mod`): `type, extends(OperationGraphNode)
      :: MethodGraphNode` holding `class(MethodInvocationAdapter),
      allocatable :: adapter`, `type(ArgumentSpecMap) :: arguments`,
      `type(StateItemMemberMap) :: bindings` (reused type, design.md
      Decisions - "Argument bindings reuse the existing
      StateItemMemberMap"). Constructor `MethodGraphNode(id, adapter)`
      mirroring `TransformGraphNode`'s `new_TransformGraphNode(id,
      transformer)` shape.
- [x] 4.2 Implement `declare_argument(this, name, access, rc,
      expected_kind)`: rejects a duplicate name (spec scenario
      "Duplicate argument name is rejected"), leaving the existing
      declaration unchanged on failure.
- [x] 4.3 Implement `is_argument(this, name)`, `get_argument(this, name,
      rc) -> ArgumentSpec`, `get_arguments(this) -> ArgumentSpecMap`.
- [x] 4.4 Implement `bind_argument(this, name, target_id, rc)`: asserts
      `name` was already declared (spec scenario "Binding an undeclared
      argument is rejected"); if the declaration is kind-constrained and
      `target_id` resolves to a value whose kind can be checked by the
      caller-supplied kind (this node has no `ComponentGraph` reference,
      so the kind-match check itself takes the actual kind as a direct
      argument rather than looking it up - mirrors `ComponentGraph
      %bind_port()`'s equivalent check but performed one layer up, since
      `MethodGraphNode` deliberately has no graph reference of its own),
      reject a mismatched kind, leaving the existing binding (if any)
      unchanged.
- [x] 4.5 Implement `get_argument_binding(this, name) -> NodeId`
      (pointer/optional, null/absent if unbound), `get_argument_bindings
      (this) -> StateItemMemberMap`.
- [x] 4.6 Implement `invoke(this, rc, clock)`: asserts `this%adapter` is
      allocated (spec scenario "No adapter attached is not invocable"),
      then calls `this%adapter%invoke(this%arguments, this%bindings,
      clock, rc)` - no other logic, no branching on adapter's dynamic
      type (REQ-MTH-002, REQ-MTH-003).
- [x] 4.7 `Test_MethodGraphNode.pf`: construct with each adapter kind and
      confirm identical argument-declaration/binding shape (spec
      scenarios "Node constructed for a GridComp-phase call shape" /
      "...State-callback call shape"); invoking with no adapter fails
      loudly; declare one argument per `AccessSpec` value and confirm
      each is retrievable; duplicate-name declaration rejected;
      kind-constrained argument accepts a matching binding and rejects a
      mismatched one; binding an undeclared argument is rejected; an
      import-side-flavored and an export-side-flavored argument coexist
      in the same flat argument set with no distinguishing query (spec
      scenario "Arguments from both sides coexist without distinction");
      `invoke()` calls exactly the attached adapter's entry point once,
      confirmed via each of the two synthetic test doubles from tasks
      3.5/3.6, with equivalent outcomes across both adapter kinds (spec
      scenario "Invocation succeeds identically regardless of adapter
      kind"); invoking with a clock passes it through with no argument/
      binding created for it; invoking with no clock succeeds.

## 5. Regression: demand-driven update ignores MethodGraphNode

- [x] 5.1 Add (or extend an existing) test registering a
      `MethodGraphNode` in a `ComponentGraph` alongside ordinary
      `StateItemNode`/`TransformGraphNode` wiring, then calling
      `ComponentGraph%update()` to resolve some other node's value;
      confirm the `MethodGraphNode`'s adapter is never invoked (spec
      scenario "Demand-driven update does not invoke a registered method
      node", exercising the existing, unmodified `class default` branch
      in `mapl_ComponentGraph_DemandDrivenUpdate_smod` against this new
      node kind - no production code change expected from this task).

## 6. Verification

- [x] 6.1 Add all new source files to
      `superstructure/generic/graph/CMakeLists.txt` (there is no
      separate `containers/CMakeLists.txt` - `containers/*.F90` files
      are listed directly in the parent `CMakeLists.txt`, matching
      `containers/PortSpecMap.F90`'s existing entry) and all new test
      files to `superstructure/generic/graph/tests/CMakeLists.txt`.
- [x] 6.2 Build MAPL with NAG (`nag` build dir, per
      `.opencode/skills/mapl-build`).
- [x] 6.3 Run the full `ctest` suite; confirm no regressions outside the
      new test files (compare against the pre-existing pass/fail
      baseline).
- [x] 6.4 Run the specific new test labels individually with `-v` to
      confirm each new test case actually executed and passed.
- [x] 6.5 `openspec validate method-graph-node --strict` reports the
      change valid.
