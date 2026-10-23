## 0. Design review (B0)

- [x] 0.1 Write proposal.md, design.md, and spec deltas for D1-D6 with the
      tables required by B0 (design.md §1, §3.4, §4.2, §6).
- [x] 0.2 Owner answers Q1, Q8, Q10, Q12 (needed for B1/B2); recorded
      in design.md "Owner decisions".
- [ ] 0.3 Remaining questions are answered in the B3 task named beside
      each in design.md. Each such task records the answer in design.md
      (or its successor doc) before implementing.

## 1. Extract StateItemDescriptor (B1, no behavior change)

- [x] 1.1 Add `graph/StateItemDescriptor.F90` holding a
      `StateItemCharacteristicMap`, with `has/get/set_characteristic` and
      `get_characteristics`. Add it to `graph/CMakeLists.txt`.
- [x] 1.2 `StateItemNode` owns a descriptor, with `get_descriptor`
      (pointer) accessor.
- [x] 1.3 Move the characteristics map and the seven description
      components (design.md §6.1) from `GraphStateItem` to the descriptor,
      keeping their current accessors' names on the descriptor. Update
      every caller.
- [x] 1.4 Done when ESSENTIAL passes on NAG and gfortran and
      `GraphStateItem` has only the handle and member-map components.

## 2. Generic status resolution (B2)

- [x] 2.1 Implement `resolve_pair` per design.md §2.2 in the descriptor
      module; pFUnit test for every cell.
- [x] 2.2 Add the (shape, kind) MIRRORED-export allowlist (§2.4) as a
      static table; tests for an allowlisted and a non-allowlisted pair.
- [x] 2.3 Add the DEFERRED freeze check (§2.3) in `run_graph_freeze_hook`
      through a descriptor method; test that freeze fails naming item
      and kind.
- [x] 2.4 Route `ExtensionResolution%find_mismatched_characteristics`
      through `resolve_pair`. Keep the one-sided-absence rule (§2.6);
      align or delete `StateItemCharacteristicDetection`.
- [x] 2.5 Convert TypeKind first: `MAPL_TYPEKIND_MIRROR` -> MIRRORED entry
      instead of omission. Verify StatisticsGridComp scenarios.
- [ ] 2.6 Every B3 factory that maps "absent" to a status lists the
      legacy absent-means-MIRROR cases it found (design.md §5) in its
      commit message.
- [x] 2.7 Done when ESSENTIAL passes on NAG and gfortran.

## 3. One task per characteristic (B3-<Name>)

Each task: move to generic status, implement `write_info` (§3.4),
add the VariableSpec factory (§5), remove its `populate_characteristics`
block and GraphBuilder references (§6.2, §6.3). Done when ESSENTIAL
passes on NAG and gfortran and `grep -n <KIND> GraphBuilder*.F90` finds
nothing for that kind, except the §6.3 materialization references
(`get_item_units`, `get_item_typekind`, `merged_item_for_link`, the
`GraphBuilder.F90` use list), which B4 removes; list any that remain in
the commit message.

- [x] 3.1 B3-Units (template; also writes
      `docs/graph/adding-a-characteristic.md`). Owner-approved deviation:
      `get_item_units`/`merged_item_for_link` (materialization) and the
      `GraphBuilder.F90` use list still name PHYSICAL_UNITS; they go in
      B4 (§6.3), so the per-kind grep is not yet empty.
- [x] 3.2 B3-TypeKind (finish). `get_item_typekind`, the TYPE_KIND
      branch of `merged_item_for_link` and the `GraphBuilder.F90` use
      list remain for B4 (§6.3).
- [ ] 3.3 B3-UngriddedDims (removes `GraphStateItem%ungridded_dims` duplicate)
- [ ] 3.4 B3-VerticalCoordinate (M2: component grid / DEFERRED)
- [ ] 3.5 B3-VerticalStagger (new kind; also add the
      (EXPRESSION, VERTICAL_STAGGER) entry to the §2.4 allowlist, which
      B2 left out because the kind did not exist)
- [ ] 3.6 B3-Geometry (M1: item-level entry; DEFERRED; moves
      `_Geometry_smod` inserts behind a helper)
- [ ] 3.7 B3-ItemShape (all shapes; `get_extension_order` per §4.2;
      shape matrix per §4.3)
- [ ] 3.8 B3-StandardName (delete the subclass status check)
- [ ] 3.9 B3-Attributes (always insert; report any scenario change)
- [ ] 3.10 B3-QuantityType, B3-Conservation, B3-Normalization
- [ ] 3.11 B3-RestartMode (new kind; `populate_one` uses
      `sync_to_info(DESCRIPTIVE)`)
- [ ] 3.12 B3-Expression (new kind; per-consumer semantics pending Q2.
      Since B2, an allowlisted MIRRORED export resolves only as a legal
      match; this task gives each consumer its own geometry/precision,
      §2.4)
- [ ] 3.13 B3-LongName (provisional; drop if no graph-created field
      needs it)
- [ ] 3.14 B3-SetShared: retarget `set_shared_characteristic` to mutate
      the descriptor (§7.3); check and report whether import-side
      geometry changes (History samplers) are handled. Since B1 it
      installs only the ESMF handle and leaves the descriptor unchanged,
      so until this task it cannot change a characteristic.

## 4. Dispatch removal (B4)

- [ ] 4.1 Replace `merged_item_for_link` with generic tip replacement
      (§4.1).
- [ ] 4.2 Materialize extension fields by empty-create +
      `sync_to_info(STRUCTURAL)` + `MAPL_FieldEmptyComplete` (§3.2.1);
      delete `get_item_units` / `get_item_typekind`.
- [ ] 4.3 Delete `GraphStateItem%ordering` and
      `characteristic_ordering_table`; replace their tests with golden
      `get_extension_order` tests per shape.
- [ ] 4.4 Done when the D6 grep (`_CHARACTERISTIC_KIND` and concrete
      `*Characteristic` names in `GraphBuilder*.F90`, outside comments)
      finds nothing, and ESSENTIAL passes on NAG and gfortran.

## 5. VariableSpec translation (B5)

- [x] 5.1 Add `VariableSpec%make_characteristics` in a new `specs/`
      submodule as a loop over the factory list (§5). (Done in B3-Units;
      later B3 tasks add factories to the list.)
- [ ] 5.2 Reduce `populate_characteristics` to one call; then delete it.
- [ ] 5.3 Done when ESSENTIAL passes on NAG and gfortran.
