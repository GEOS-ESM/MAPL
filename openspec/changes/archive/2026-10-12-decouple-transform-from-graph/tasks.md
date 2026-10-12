## 1. New container type

- [x] 1.1 Create `superstructure/generic/graph/containers/PortNameGraphStateItemMap.F90`
      (`mapl_PortNameGraphStateItemMap_mod`), a gFTL map of port name
      (`__CHARACTER_DEFERRED` key) -> `GraphStateItem`, mirroring
      `PortNameRevisionMap.F90` exactly (same `map/template.inc`
      instantiation pattern, substituting `GraphStateItem` for
      `NodeRevision`). Add it to the appropriate `CMakeLists.txt` next to
      `PortNameRevisionMap.F90`'s own entry.

## 2. `Transform` interface

- [x] 2.1 In `Transform.F90`, change the abstract `compute` interface from
      `compute(this, rc)` to
      `compute(this, inputs, outputs, rc)`, where `inputs`/`outputs` are
      both `type(PortNameGraphStateItemMap), intent(in)`. Update the
      module's `use` list to import `mapl_PortNameGraphStateItemMap_mod`.

## 3. `TransformGraphNode`

- [x] 3.1 In `TransformGraphNode.F90`, change `execute(this, rc)` to
      `execute(this, inputs, outputs, rc)`, forwarding both arguments
      unchanged into `this%transformer%compute(inputs, outputs, rc)`. No
      stored `ComponentGraph`/`NodeId` reference is added to
      `TransformGraphNode` itself - both maps are caller-supplied.
- [x] 3.2 Update `Test_TransformGraphNode.pf`'s two local `Transform`
      doubles (`CountingCompute%compute`, `SelfDescribingTransform%compute`)
      for the new `compute` signature (accept and ignore the two new
      arguments - neither double's own test behavior needs to read
      them). Update every `node%execute(...)` call site in that file to
      pass two (possibly empty) `PortNameGraphStateItemMap` arguments.

## 4. Concrete `Transform` subclasses

- [x] 4.1 In `UnitsConverterTransform.F90`: remove the stored
      `graph`/`input_node_id`/`output_node_id` fields and the
      corresponding constructor arguments (`new_UnitsConverterTransform`
      keeps only `src_units`/`dst_units`). Change `units_compute`'s
      signature to `units_compute(this, inputs, outputs, rc)`; replace
      the `this%graph%get_node(...)`/`select type`/`get_payload()` lookup
      block with `input_item = inputs%at(INPUT_PORT_NAME)` /
      `output_item = outputs%at(OUTPUT_PORT_NAME)` (module-local
      `INPUT_PORT_NAME`/`OUTPUT_PORT_NAME` parameters, unchanged).
- [x] 4.2 In `TypeKindConverterTransform.F90`: identical change to 4.1 -
      remove stored `graph`/`input_node_id`/`output_node_id`, update
      `new_TypeKindConverterTransform`'s own argument list, change
      `typekind_compute`'s signature and body the same way.
- [x] 4.3 Update `Test_UnitsConverterTransform.pf`: its one test
      constructs `UnitsConverterTransform(graph, input_id, output_id,
      'm', 'km')` and calls `transform%compute(status)` directly - update
      the constructor call to drop `graph`/`input_id`/`output_id`, and
      build the `inputs`/`outputs` `PortNameGraphStateItemMap` arguments
      from the test's own `input_payload`/`output_payload` (already
      constructed locally) before calling
      `transform%compute(inputs, outputs, status)`.

## 5. `Characteristic%build_transform` interface

- [x] 5.1 In `Characteristic.F90`, change the abstract `I_build_transform`
      interface from
      `build_transform(this, graph, input_node_id, output_node_id, goal, transformer, rc)`
      to `build_transform(this, goal, transformer, rc)`. Remove the now
      -unused `ComponentGraph`/`NodeId` imports from this module's `use`
      list if nothing else in the file needs them.
- [x] 5.2 Update every concrete implementation's signature and drop the
      three removed arguments from each constructor call it makes:
      `units_build_transform` (`UnitsCharacteristic.F90`) ->
      `UnitsConverterTransform(this%units, goal%units)`;
      `adapter_build_transform` (`TypeKindCharacteristicAdapter.F90`) ->
      `TypeKindConverterTransform(label_to_typekind(...), label_to_typekind(...))`;
      `vgrid_build_transform` (`VerticalGridCharacteristic.F90`) - no
      constructor call changes (always `_FAIL`s before reaching one), only
      the signature; `geom_build_transform` (`GeomCharacteristic.F90`) -
      same, signature only.
- [x] 5.3 Update `Test_ExtensionResolution.pf`'s `FakeCharacteristic%build_transform`
      (`fake_build_transform`) for the new four-argument signature -
      drop its own unused `graph`/`input_node_id`/`output_node_id` dummy
      arguments; its body is otherwise unchanged (it already ignores
      them except for the `class(ComponentGraph), target, intent(in)`
      declaration itself).

## 6. `ExtensionResolution.F90`'s one call site

- [x] 6.1 In `build_chain` (`ExtensionResolution.F90`, ~line 250), update
      the `export_characteristic%build_transform(...)` call to the new
      four-argument form:
      `call export_characteristic%build_transform(import_characteristic, transformer, status)`.
      Confirm `graph`, `current_node_id`, `extension_node_id` remain used
      elsewhere in the same subroutine (registering `extension_node`,
      `graph%bind_port` calls) - only this one call site's own argument
      list shrinks, nothing else in `build_chain` changes.

## 7. Demand-driven update orchestration

- [x] 7.1 In `ComponentGraph_DemandDrivenUpdate.F90`'s `finish_transform`,
      extend the existing input-port loop (the one that already resolves
      each declared input port's bound `NodeId`/`StateItemNode` to
      extract a `NodeRevision` for `current_input_revisions`) to also
      extract that same `StateItemNode`'s `GraphStateItem` (via
      `get_payload()`, already public) into a new local
      `PortNameGraphStateItemMap`, in the same loop iteration - no new
      traversal.
- [x] 7.2 Add a second, new loop over `output_ports` (built the same way
      the existing post-execution output-port loop already is, via
      `transform_node%get_output_ports()`), run *before* calling
      `execute()`, resolving each declared output port's bound
      `NodeId`/`StateItemNode`'s `GraphStateItem` into a second
      `PortNameGraphStateItemMap`. Keep the existing post-execution
      output-port loop (the one that calls `advance_revision()`)
      unchanged and separate - do not merge the two.
- [x] 7.3 Update the `transform_node%execute(_RC)` call to
      `transform_node%execute(input_items, output_items, _RC)` using the
      two maps built in 7.1/7.2.

## 8. Verification

- [x] 8.1 Build MAPL with NAG (`nag` build dir); run `cmake --build nag
      -j 8` and `build-tests`; confirm no new build regressions.
- [x] 8.2 Run `Test_TransformGraphNode.pf`, `Test_UnitsConverterTransform.pf`,
      `Test_Characteristic.pf`, `Test_ExtensionResolution.pf` directly
      with `-v`; confirm every existing assertion passes unchanged (no
      test's expected values are modified by this change - only
      construction/call-site mechanics).
- [x] 8.3 Run full `ctest` for the `MAPL.generic.*` label set; confirm no
      regression outside the same pre-existing, unrelated environment
      gaps earlier changes' own tasks.md files already documented.
- [x] 8.4 Grep-confirm `ComponentGraph` no longer appears anywhere in
      `Characteristic.F90`'s own `use` list or `I_build_transform`
      interface, and no concrete `Transform` subclass
      (`UnitsConverterTransform.F90`/`TypeKindConverterTransform.F90`)
      declares a `class(ComponentGraph)` component - confirming the
      module-use cycle through `GraphStateItem` this change exists to
      remove is actually gone, not just narrowed.
- [x] 8.5 `openspec validate decouple-transform-from-graph --strict`
      reports the change valid.
