## 1. VariableSpec self-recursion support (new, small, dedicated modules)

- [x] 1.1 Create `superstructure/generic/specs/VariableSpecTag.F90`
      (`mapl_VariableSpecTag_mod`): a bare `type, abstract ::
      VariableSpecTag; end type` — no components, no deferred procedures,
      zero dependencies (design.md Decisions). Exists purely so
      `VariableSpecMemberMap` (task 1.2) can be templated polymorphically
      over it, breaking the `VariableSpec`/`VariableSpecMemberMap` module
      cycle (Fortran does not support a derived type containing a map of
      itself without this indirection).
- [x] 1.2 Create `superstructure/generic/specs/VariableSpecMemberMap.F90`
      (`mapl_VariableSpecMemberMap_mod`), a gFTL2 map (`Key =
      __CHARACTER_DEFERRED`, `T = VariableSpecTag`, `T_polymorphic`, `Map
      = VariableSpecMemberMap`), using the single `map/template.inc`
      include with `T_polymorphic` added — same shape as
      `containers/NodeIdGraphNodeMap.F90` (polymorphic map over a
      foreign, already-defined abstract type, one dedicated module).

## 2. VariableSpec: member declaration API (additive)

- [x] 2.1 In `superstructure/generic/specs/VariableSpec.F90`: add `use
      mapl_VariableSpecTag_mod, only: VariableSpecTag` and `use
      mapl_VariableSpecMemberMap_mod`; change `type VariableSpec` to
      `type, extends(VariableSpecTag) :: VariableSpec`; add a private
      `members : type(VariableSpecMemberMap)` component (plain, not
      allocatable — always present, empty unless populated, mirroring
      `GraphStateItem`'s own `state_members_map` component shape).
- [x] 2.2 Add `declare_member(this, name, member, rc)`: asserts
      `this%itemType == MAPL_STATEITEM_STATE` (fails loudly otherwise —
      design.md Decisions "declare_member asserts... it does not
      implicitly set it"); asserts no existing member already declared
      under `name` (spec scenario "Duplicate member name at the same
      level is rejected"), leaving the existing entry unchanged on
      failure; `member`'s own `short_name` field is never read (design.md
      Decisions - "A member's own short_name field is never read").
- [x] 2.3 Add `has_member(this, name)`, `get_member(this, name, rc) ->
      type(VariableSpec)`, `get_member_names(this) -> type(StringVector)`
      (for the recursive materialization walk, task 3).
- [x] 2.4 Type-bound procedure declarations for the above, added to
      `VariableSpec`'s existing `contains` block alongside
      `make_virtualPt`/`make_dependencies`/etc. — no existing procedure
      declaration changes.

## 3. GraphBuilder: recursive advertise materialization

- [x] 3.1 Create
      `superstructure/generic/graph/CompositeStateMaterialization.F90`
      (`mapl_CompositeStateMaterialization_mod`), mirroring the
      `ExtensionMaterialization`/`ExtensionResolution` separation
      `GraphBuilder.F90` already depends on for a different concern
      (module header, `GraphBuilder.F90:1-79`). Depends on
      `mapl_VariableSpec_mod`, `mapl_ComponentGraph_mod`,
      `mapl_StateItemNode_mod`, `mapl_GraphStateItem_mod`,
      `mapl_NodeRevision_mod` — never `ComponentSpec`/`OuterMetaComponent`.
- [x] 3.2 Implement `materialize_member(graph, var_spec, rc) -> NodeId`:
      if `var_spec%get_member_names()` is empty, create an unallocated
      `GraphStateItem`-payload `StateItemNode` exactly as `advertise_one`
      does today (`GraphBuilder.F90:207-252`) — a leaf; otherwise (has
      members), call `materialize_composite(graph, var_spec, rc)` (task
      3.3) and return its top-level `NodeId`.
- [x] 3.3 Implement `materialize_composite(graph, var_spec, rc) ->
      NodeId`: create a `StateItemNode` whose `GraphStateItem` has its
      `esmf_state` component allocated via a fresh, memberless
      `ESMF_StateCreate` (design.md Decisions - "Nested-state
      materialization allocates a real, structurally-empty ESMF_State");
      for each name in `var_spec%get_member_names()`, recursively call
      `materialize_member(graph, var_spec%get_member(name, rc), rc)` and
      insert the returned `NodeId` into the new `GraphStateItem`'s
      `state_members` map under that name (REQ-SI-006). Build the
      complete payload (all children materialized and inserted) *before*
      registering the node once — avoids ever needing to fetch the node
      back out of the graph to mutate its payload after registration.
- [x] 3.4 In `GraphBuilder.F90`'s existing `advertise_one`
      (`GraphBuilder.F90:207-252`): add a branch — if
      `var_spec%get_member_names()` is non-empty, delegate node creation
      to `materialize_composite` (task 3.3) instead of constructing an
      unallocated-payload `StateItemNode` directly. The existing identity
      key (`item_key`), resource-index registration, and import/export
      port registration in `advertise_one` are unchanged and apply
      uniformly to both the flat and composite cases — no new procedure,
      no new `GraphBuilder` public entry point, no new lifecycle hook
      (design.md Decisions).
- [x] 3.5 Confirm (do not implement — verify by reading, then cover with
      a test in task 4) that `for_each_matching_import`/
      `find_export_var_spec`/`build_characteristics`
      (`GraphBuilder.F90`) require **no changes**: they already iterate
      `ComponentSpec%var_specs`, which now includes composite items
      directly.

## 4. Tests

- [x] 4.1 `Test_VariableSpecMembers.pf`
      (`superstructure/generic/tests/`): declare a leaf member and a
      nested member and retrieve both by name; `declare_member` on a
      non-`MAPL_STATEITEM_STATE` spec fails loudly; duplicate member name
      at one level is rejected, original entry unchanged; same name
      reused at a different nesting level succeeds; multi-level nesting
      (>=3 levels) is declarable and walkable top-down via
      `get_member`/`get_member_names`; two members declared under the
      same parent with different characteristics (e.g. different units)
      retrieve those characteristics independently; a member's own
      `short_name` is never consulted by `declare_member`/`get_member`
      (declare a member whose `short_name` differs from its map key,
      confirm retrieval is still by map key only).
- [x] 4.2 `Test_GraphBuilderComposite.pf` (or added to the existing
      `superstructure/generic/tests/Test_GraphBuilder.F90` suite):
      advertising a single-level composite `VariableSpec` produces a
      top-level node plus one node per member, each reachable via
      `state_members`; advertising a multi-level composite produces the
      full tree; re-advertising an unchanged composite declaration does
      not duplicate its node tree.
- [x] 4.3 `test_ordinary_connection_wires_matching_composite` (added to
      `Test_GraphBuilder.pf`): two components each advertise a composite
      `VariableSpec` under the same top-level short_name via ordinary
      `add_var_spec`; an ordinary `MatchConnection` between them wires
      the top-level composite items directly (no extension chain, since
      neither carries `units`/`vertical_grid`) — using the **existing**
      `graphbuilder_resolve_connections`/`for_each_matching_import` code
      path completely unmodified. This is the test that would have failed
      against the first draft's separate-vector design and closes the
      gap that draft's implementation surfaced.

## 5. Verification

- [x] 5.1 Build MAPL with NAG (`nag` build dir, per
      `.opencode/skills/mapl-build`), including the new source files in
      `superstructure/generic/specs/CMakeLists.txt` and
      `superstructure/generic/graph/CMakeLists.txt`.
- [x] 5.2 Run the full `ctest` suite; confirm no regressions outside the
      new test files (compare against the pre-existing pass/fail
      baseline).
- [x] 5.3 Run the specific new/modified test labels individually with
      `-v` to confirm each new test case actually executed and passed.
- [x] 5.4 `openspec validate composite-state-spec --strict` reports the
      change valid.
