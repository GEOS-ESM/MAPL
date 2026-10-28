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
(the `get_item_<kind>` getters, `merged_item_for_link`, the
`GraphBuilder.F90` use list), which B4 removes; list any that remain in
the commit message. A new `get_item_<kind>` getter is acceptable only
if materialization needs the value before B4; add it to 4.2's list.

Remaining order (owner): 3.15, 3.5, 3.16, 3.17, 3.18, then 3.7, 3.13,
3.11, 3.10, 3.12, 3.14 as their open questions allow (3.10: Q9; 3.12: Q2).
Numbers are labels, not run order. Every task whose characteristic has a
mirror value rejects it on a non-EXPRESSION export in the factory, at
advertise, with a test (A19; B3-VerticalStagger is the pattern). A
harvest reads through `MAPL_*Get`, never raw Info (A19).

- [x] 3.1 B3-Units (template; also writes
      `docs/graph/adding-a-characteristic.md`). Owner-approved deviation:
      `get_item_units`/`merged_item_for_link` (materialization) and the
      `GraphBuilder.F90` use list still name PHYSICAL_UNITS; they go in
      B4 (§6.3), so the per-kind grep is not yet empty.
- [x] 3.2 B3-TypeKind (finish). `get_item_typekind`, the TYPE_KIND
      branch of `merged_item_for_link` and the `GraphBuilder.F90` use
      list remain for B4 (§6.3).
- [x] 3.3 B3-UngriddedDims (removes the `StateItemDescriptor%ungridded_dims`
      duplicate). `get_item_ungridded_dims`, the UNGRIDDED_DIMS branch of
      `merged_item_for_link` and the `GraphBuilder.F90` use list remain
      for B4 (§6.3).
- [x] 3.4 B3-VerticalCoordinate (M2: component grid / DEFERRED). Same
      component-wide rule as Geometry (§5): absent is DEFERRED from the
      component `vertical_grid`, reusing `CharacteristicDefaults` and
      `resolve_deferred`. Owner-approved addition (design §5): an item
      with `vertical_stagger == NONE` gets no entry, and a DEFERRED entry
      whose component has no vertical grid is dropped at GEOM_B
      (`StateItemDescriptor%drop_unsupplied`). Keyword: `mirror_vgrid` /
      YAML `vertical_grid_mirror: true`; Statistics and History imports
      without their own grid use it. `get_item_vgrid`,
      `resolve_export_vgrid` and the VERTICAL_COORDINATE branch of the
      mismatch classification remain for B4 (§6.3).
- [x] 3.15 B3-RenameVerticalGrid (no behavior change). Rename the
      VerticalCoordinate characteristic to VerticalGrid (A18): file
      `VerticalCoordinateCharacteristic.F90` -> `VerticalGridCharacteristic.F90`,
      type `VerticalGridCharacteristic`, kind
      `VERTICAL_GRID_CHARACTERISTIC_KIND`, display `"VERTICAL_GRID"`, its
      tests, CMake lists, and the kind name in docs/graph and
      `specs/graph/state-item-descriptor/spec.md`. Plan Status lines that say
      `B3-VerticalCoordinate` stay as history.
- [x] 3.5 B3-VerticalStagger. New kind; also add the
      (EXPRESSION, VERTICAL_STAGGER) entry to the §2.4 allowlist, which
      B2 left out because the kind did not exist. Follow design.md §5
      (VerticalStagger row), A17 and Q6. In short: no built-in default
      (A17); MIRROR is MIRRORED on imports only (error on an export);
      `mirror_vgrid` with no stagger defaults to MIRRORED; an explicit
      NONE stays SPECIFIED NONE; an undeclared stagger is DEFERRED at
      advertise (not an error), becomes SPECIFIED NONE at GEOM_B when the
      component has no vertical grid, otherwise it is harvested, or freeze
      fails. Alignment is not part of this task (3.16). Test for NONE/MIRROR with `is_none()`/`is_mirror()`:
      `VerticalStaggerLoc`'s `==` treats MIRROR as a wildcard (the NONE
      test in `resolve_export_vgrid` still uses `==`, so a MIRROR export
      stagger would read as 2D; fix it here). Replaces
      `StateItemDescriptor%vertical_stagger` and its readers
      (`resolve_export_vgrid`, two in `_ConnectionResolution_smod`).
      Risk: many scenario items in components with a vertical grid give
      no `vertical_dim_spec`. If harvest does not fill them and freeze
      fails, stop and report the list; do not add a default.
- [x] 3.16 B3-VerticalOrientation (new kind; name provisional). Follow
      design.md §5 (VerticalOrientation row) and Q7: vertical index order,
      one value per item, independent of VerticalStagger; unspecified ->
      the item's grid direction, resolved once the grid is known; explicit
      mirror value on imports only; never reads VerticalGrid when
      comparing. Add the (EXPRESSION, VERTICAL_ORIENTATION) allowlist
      entry. A mismatch needs a reversal step; if no Transform exists,
      `build_transform` fails like Geometry's and the task reports it.
- [x] 3.17 B3-HorizontalDimsSpec (new kind, A14). Move
      `horizontal_dims_spec` out of `GeometryCharacteristic` into
      `HorizontalDimsSpecCharacteristic`
      (`HORIZONTAL_DIMS_SPEC_CHARACTERISTIC_KIND`), ordered before
      GEOMETRY (§4.2). GEOM default; NONE vs GEOM fails in both directions;
      mirror allowed on imports. With NONE the factory makes Geometry
      uncompared: first check whether field creation needs the item's geom
      for a NONE item; if it needs a "filled but never compared" state,
      stop and report before adding it. Tests: a new `dims: z` scenario
      connecting across different horizontal grids, and failure tests for
      both mismatch directions. This replaces the "no horizontal geom"
      check planned for ItemShape/B4.
- [ ] 3.18 B3-StringMirror (A13). Add one named string sentinel to
      `VariableSpec`; the StandardName factory maps it to MIRRORED
      (imports only; an export is an error). LongName (3.13) reuses it.
- [x] 3.6 B3-Geometry (M1: item-level entry; DEFERRED; moves
      `_Geometry_smod` inserts behind a helper). Deviations from design
      §2.2/§2.3, owner-approved: a DEFERRED side at connect time waits
      (`PAIR_WAIT`, retried on a later pass) instead of being a conflict,
      and a DEFERRED value may outlive GEOM_B until the component fills it
      in its own init phase (harvested read-only from the
      component's field or bundle in the state; imports skipped). A component still
      waiting at freeze fails freeze. A new
      `CharacteristicDefaults` type carries the component default geom to
      `resolve_deferred`. An EXPRESSION with no declared geom is MIRRORED
      (§2.4 allowlist). Legacy absent-means-mirror is not reproduced: a
      component says so explicitly with `mirror_geom` (`AddSpec`,
      `VariableSpec`) or `geometry_mirror: true` (YAML); Statistics and
      History collection imports (no `geom:`) do. `build_transform`
      returns a quiet nonzero rc.
      Statistics now advertises only the chosen variance algorithm's
      internal fields, since the other kernel's fields are never given a
      geometry.
- [ ] 3.7 B3-ItemShape (all shapes; `get_extension_order` per §4.2;
      shape matrix per §4.3). Known starting point: the connect hook
      reports "no registered extension provider" for ITEM_SHAPE
      mismatches (seen in `expression_defer_geom`). Since B3-StandardName,
      only FIELD carries a StandardName entry, so a BRACKET/VECTOR/
      FIELDBUNDLE/EXPRESSION standard-name mismatch is not detected.
      Confirm the shape step adopts the import's name after BRACKET->FIELD,
      as §4.3 assumes; if not, stop and report.
- [x] 3.8 B3-StandardName (delete the subclass status check). Factory
      inserts for FIELD only (§4.3); an undeclared FIELD name is UNCHECKED.
      `write_info` is descriptive (both phases).
- [x] 3.9 B3-Attributes (always insert; report any scenario change).
      Inserted for FIELD/BRACKET/VECTOR/VECTORBRACKET/FIELDBUNDLE (§4.3
      excludes EXPRESSION). `write_info` is a successful no-op. No
      scenario changed (ESSENTIAL green; PENDING identical to HEAD).
- [ ] 3.10 B3-QuantityType, B3-Conservation, B3-Normalization
- [ ] 3.11 B3-RestartMode (new kind; `populate_one` uses
      `sync_to_info(DESCRIPTIVE)`). Default `RESTART_REQUIRED`, never
      compared, no transform (A15). `write_info` writes the item's own
      alias, so imports sharing an export keep their own modes.
- [ ] 3.12 B3-Expression (new kind; per-consumer semantics pending Q2.
      Since B2, an allowlisted MIRRORED export resolves only as a legal
      match; this task gives each consumer its own geometry/precision,
      §2.4. Also address ungridded dims: B3-UngriddedDims gives
      EXPRESSION no UNGRIDDED_DIMS entry, so an EXPRESSION export
      silently matches a FIELD import that declares ungridded dims;
      owner accepted this until this task. Known starting point: the
      connect hook reports "no registered extension provider" for
      EXPRESSION mismatches. Resolve Q2 with the owner and amend §2.4
      before implementing)
- [ ] 3.13 B3-LongName (A8; after 3.18). Default chain: declared, field
      dictionary, then StandardName; string mirror on imports. Per-alias
      `write_info`.
- [ ] 3.14 B3-SetShared: retarget `set_shared_characteristic` to mutate
      the descriptor (§7.3); check and report whether import-side
      geometry changes (History samplers) are handled. Since B1 it
      installs only the ESMF handle and leaves the descriptor unchanged,
      so until this task it cannot change a characteristic.

## 4. Dispatch removal (B4)

- [ ] 4.1 Replace `merged_item_for_link` with generic tip replacement
      (§4.1). `build_transform` gets read-only tip and goal descriptors
      and returns the kinds its step covers (default: its own); the tip
      replacement replaces all of them (A16).
- [ ] 4.2 Materialize extension fields by empty-create +
      `sync_to_info(STRUCTURAL)` + `MAPL_FieldEmptyComplete` (§3.2.1);
      delete `get_item_units` / `get_item_typekind` /
      `get_item_ungridded_dims` / `get_item_geom` / `get_item_vgrid` /
      `get_item_vertical_stagger`, move `harvest_stagger` out of
      `GraphBuilder_Geometry_smod.F90` behind the characteristic, and the
      `insert_geom_id_characteristic` import in
      `GraphBuilder_Geometry_smod.F90` (these name concrete
      `*Characteristic` types in `select type`, so 4.4 depends on this).
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
