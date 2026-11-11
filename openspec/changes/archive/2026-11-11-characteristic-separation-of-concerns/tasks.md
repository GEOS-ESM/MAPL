## 0. Design review (B0)

- [x] 0.1 Write proposal.md, design.md, and spec deltas for D1-D6 with the
      tables required by B0 (design.md §1, §3.4, §4.2, §6).
- [x] 0.2 Owner answers Q1, Q8, Q10, Q12 (needed for B1/B2); recorded
      in design.md "Owner decisions".
- [x] 0.3 Remaining questions are answered in the B3 task named beside
      each in design.md. Each such task records the answer in design.md
      (or its successor doc) before implementing. (Closed by the owner,
      2026-11-10: every B3 task that carried a question has landed and
      recorded its answer.)

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
- [x] 2.6 DROPPED (owner, 2026-11-10: a process note for the B3 commits,
      all landed and not auditable afterwards; design.md §5 holds the
      absent-means-MIRROR cases). Original text: Every B3 factory that maps "absent" to a status lists the
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

Remaining order (owner): 3.29, 3.30, 3.32, 3.35, 3.37, 3.27, 3.36,
3.12, 3.23, 3.14 (3.31 and 3.39 dropped; 3.12/3.23: Q2, Q3 answered; 3.24 later; 3.27
before B4; 3.36 after 3.27, whose switch it shares, and after 3.37;
3.34 with or after 4.2; 3.33 after B4; 3.41a, 3.41b, 3.42 after 3.33, in that order).
3.7, 3.10, 3.11, 3.13, 3.20, 3.21, 3.22 and 3.28 are done.
Numbers are labels, not run order. A task whose characteristic adds a
mirror value adds its spelling to `VariableSpec%requests_mirror()`; the
generic checks (A21) then reject it on anything but an import or an
EXPRESSION export, and
the factory adds no export assert of its own. A
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
- [x] 3.18 B3-StringMirror (A13). Add one named string sentinel to
      `VariableSpec`; the StandardName factory maps it to MIRRORED
      (imports only; an export is an error). LongName (3.13) reuses it.
- [x] 3.19 B3-MirrorFlagGuard (A21). In `make_VariableSpec`
      (`specs/VariableSpec.F90`, beside the `vector_basis_kind` check),
      reject on an export: `mirror_geom` or `mirror_vgrid` unless the item
      is an EXPRESSION, and `MAPL_STRING_MIRROR` (`standard_name_mirror`)
      always. Add the same `mirror_geom` / `mirror_vgrid` assertion to the
      Geometry and VerticalGrid factories (`make_characteristics.F90`),
      matching the existing HORIZONTAL_DIMS_MIRROR / StringMirror ones; keep
      the StringMirror factory assertion. Fix the two test fixtures that put
      a mirror flag on an export: `Test_GraphGeometryHook.pf`
      `advertise_and_hook` (export `M` with `mirror_vgrid`: give it a real
      vertical grid or make it an import, keeping the test's intent that a
      non-DEFERRED entry survives GEOM_B) and `advertise_undeclared_stagger`
      (set `mirror_geom` only for an import; give an export a `geom_id`).
      Also set `state_intent = ESMF_STATEINTENT_IMPORT` (owner-approved) in
      the seven `Test_VariableSpecCharacteristics.pf` tests that inherit
      `make_spec()`'s export default while setting a flag:
      `test_mirror_geom_is_mirrored`,
      `test_declared_geom_id_overrides_mirror_geom`,
      `test_none_item_ignores_mirror_geom`, `test_mirror_vgrid_is_mirrored`,
      `test_declared_vgrid_overrides_mirror_vgrid`,
      `test_mirror_vgrid_mirrors_vertical_orientation`,
      `test_mirror_vgrid_without_stagger_is_mirrored`; their assertions stay
      the same. The export check comes first: a flag on a non-EXPRESSION
      export is an error even when a declared geom/grid would override it
      (one rejection test covers flag plus declared value).
      Tests: constructor rejects a FIELD export with each flag, accepts an
      EXPRESSION export with `mirror_geom`/`mirror_vgrid`, accepts imports;
      each factory rejects a hand-built FIELD export. No production caller
      or scenario sets these flags on an export (Statistics, History
      collection, `history_1`, `statistics_real` are all imports); if one
      does, stop and report. Update adding-a-characteristic.md (Geometry,
      VerticalGrid and StringMirror sections).
- [x] 3.19b B3-MirrorGeneric (A21 as revised: no mirror of any kind on a
      non-EXPRESSION export; one rule, no per-characteristic cases;
      generalized to internals and the message reworded by 3.25).
      3.19 landed the per-flag version; this replaces it. Two checks, as in
      design.md A21:
      (1) Add `VariableSpec%requests_mirror()`: true when any field holds its
      mirror value (`MAPL_TYPEKIND_MIRROR`, `mirror_geom`, `mirror_vgrid`,
      `HORIZONTAL_DIMS_MIRROR`, `VERTICAL_STAGGER_MIRROR`,
      `vertical_alignment` mirror, `ungridded_dims%is_mirror()`,
      `standard_name_mirror`; grep the factories in `make_characteristics.F90`
      for every `MIRRORED` branch and cover each one). In `make_VariableSpec`
      replace the 3.19 per-flag asserts with one `_ASSERT`: not
      (`requests_mirror()` and export and not EXPRESSION), message
      `mirror is only valid on an import or an expression: <name>`.
      (2) In `make_characteristics`, after the factory loop: if the item is
      a non-EXPRESSION export, fail on any MIRRORED entry in the descriptor,
      naming the kind via `to_string()` (loop over the descriptor; name no
      kind in the code).
      Remove every per-factory export assert in `make_characteristics.F90`
      (mirror_geom, mirror_vgrid, HORIZONTAL_DIMS_MIRROR,
      MAPL_STRING_MIRROR, VERTICAL_STAGGER_MIRROR, vertical_alignment
      mirror) and their header-comment sentences. Note: the
      HORIZONTAL_DIMS_MIRROR and MAPL_STRING_MIRROR asserts today also
      reject an EXPRESSION export; under A21 an EXPRESSION may declare any
      mirror, so that rejection goes away (whether it resolves is the §2.4
      allowlist, unchanged). If removing it breaks a test or scenario that
      relies on it, stop and report.
      Existing export-error tests (constructor and factory) keep their
      setup and expect the new message (constructor) or the post-loop
      message (hand-built `VariableSpec`, factory path). Keep the 3.19
      fixture changes. Typekind and ungridded-dims mirrors had no export
      check before, so other tests may now trip it: if such a test checks
      the mirror mechanism, switch it to import intent (assertions
      unchanged) and list it in the commit message. New tests: `requests_mirror()` true for each
      spelling and false for a plain spec; constructor rejects a FIELD
      export with a mirror, accepts an EXPRESSION export and an import;
      post-loop check rejects a hand-built FIELD export and names the kind.
      No production caller or scenario puts a mirror value on an export; if
      one does, stop and report. Update adding-a-characteristic.md: replace
      each per-kind "error on an export" note with a pointer to the generic
      rule, and say a characteristic that adds a mirror value adds it to
      `requests_mirror()`. Also update the §3 header sentence "Every task
      whose characteristic has a mirror value rejects it ... in the
      factory" to point at the generic check.
- [x] 3.20 B3-DimsZLegacy (A22; legacy layer change). In
      `specs/GeomAspect.F90` `matches`, when both sides'
      `horizontal_dims_spec` is NONE, return `.true.` without comparing
      geom or geom_id (a MIRROR spec still goes through
      `dims_specs_agree` as today). Check that `make_transform` and
      `connect_to_export` need nothing more for that case; if legacy still
      builds a regrid, stop and report. Then make `dims_z_field` use two
      different horizontal grids for `comp_src` and `comp_dst` (keep it
      ready; expectations unchanged: no chain), and update its
      `description.md`. Add a `Test_Aspects.pf` case: two NONE GeomAspects
      with different geoms match; NONE vs GEOM still does not. Remove the
      deviation sentence from 3.17 and the plan Status. Graph code is not
      touched.
      Also (B3-MirrorFlagGuard review, minor): the `make_VariableSpec`
      rejection tests in `Test_VariableSpecCharacteristics.pf`
      (`test_make_VariableSpec_rejects_*`, `test_export_mirror_flag_with_*`)
      check only the message; add a check that `status` is nonzero.
      Also (B3-MirrorGeneric review, owner: fold here):
      (a) Replace the stale per-kind "imports only" / "error on an export"
      text with a one-line pointer to the generic rule (A21):
      `make_characteristics.F90` header comments of the HorizontalDimsSpec,
      VerticalStagger and VerticalOrientation factories, and
      `adding-a-characteristic.md` (VerticalStagger and VerticalOrientation
      tables, StringMirror and HorizontalDimsSpec sections; grep for
      "export" to find each). Rewrap the 122-character comment line in the
      HorizontalDimsSpec factory header.
      (b) Tests: a `make_VariableSpec` export rejection for each remaining
      `requests_mirror()` spelling (`MAPL_TYPEKIND_MIRROR`,
      `HORIZONTAL_DIMS_MIRROR`, a mirror `ungridded_dims`, a mirror
      `vertical_stagger`, `vertical_alignment='mirror'`) and an accept for a
      plain export; post-loop (hand-built spec) export rejections for
      typekind and ungridded dims, expecting the full message with the
      kind's `to_string()` and `status=1` (CONVENTIONS.md pFUnit rule).
      These are separate from the legacy change; list them in the commit
      message.
- [x] 3.21 B3-IncompatiblePair (A23). Add `BUILD_TRANSFORM_INCOMPATIBLE`
      beside `BUILD_TRANSFORM_UNSUPPORTED` in
      `graph/characteristics/StateItemCharacteristic.F90`, with a
      `report_build_transform_incompatible(rc)` helper (raises nothing).
      `HorizontalDimsSpecCharacteristic%build_transform` returns it (every
      SPECIFIED pair that reaches it is GEOM vs NONE). In
      `graph/ExtensionResolution.F90` (the loop at the `build_transform`
      call) and its caller in `GraphBuilder_ConnectionResolution_smod.F90`,
      report an incompatible kind as a connection conflict naming the kind
      and both values (use `get_signature` or `to_string`; no kind named in
      GraphBuilder, D6), distinct from the existing "no registered extension
      provider" warning. No `_FAIL`. Tests: HorizontalDimsSpec returns
      INCOMPATIBLE both ways; a connection-resolution test shows the
      conflict text and that other connections in the component still
      resolve. Update CONVENTIONS.md (the build_transform rule) and
      adding-a-characteristic.md (HorizontalDimsSpec section). If an
      existing test asserts UNSUPPORTED for GEOM vs NONE, change it; if any
      other kind's test changes, stop and report.
- [x] 3.22 B3-IncompatibleSweep (A23; after 3.10). Every kind whose legacy
      aspect rejects every mismatch of two non-mirror values, with no
      reachable case that converts data: Attributes, UngriddedDims,
      QuantityType, Conservation, Normalization. Their `build_transform`
      returns `BUILD_TRANSFORM_INCOMPATIBLE` instead of UNSUPPORTED, all in
      one change (owner: one general rule, not per kind). Update their
      tests and CONVENTIONS.md's build_transform rule. Check each against
      its legacy aspect first; if one does convert in some case, stop and
      report.
      Done (owner decisions on two review questions). QuantityType,
      Conservation and Normalization have `supports_conversion_general`
      true, but `supports_conversion_specific` is true only for matching
      pairs (or a mirror), and the one branch of
      `StateItemAspect%can_connect_to` that ignores `matches`
      (time-dependent) is unreachable for them (no caller sets the flag);
      it would yield only a `NullTransform` or a `_FAIL`. So the criterion
      was reworded from "`supports_conversion` false". StandardName was
      in the original list and is OUT: it fails the criterion, since legacy
      `StandardNameAspect%matches` connects two different names with a
      warning in the default PERMISSIVE `ValidationMode` (the export's name
      wins) and rejects them only in STRICT mode. It stays
      `BUILD_TRANSFORM_UNSUPPORTED` until 3.27 B3-StandardNameValidation.
      Five small edits (one `report_build_transform_incompatible` call
      each); there is no shared base implementation, since the rule is a
      property of the legacy aspect and a base default would make every
      future kind incompatible by accident. Tests: five renamed
      `..._reports_incompatible` in `Test_StateItemCharacteristicSubclasses`
      (the StandardName test still asserts UNSUPPORTED);
      `test_incompatible_ungridded_dims_is_conflict` in
      `Test_GraphBuilder_ConnectionResolution` asserts the
      `connection_conflicts` entry for a non-HorizontalDimsSpec kind. No
      scenario behavior changed (one fixture description reworded);
      PENDING identical to HEAD (see the plan Status).
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
- [x] 3.7 B3-ItemShape (all shapes; `get_extension_order` per §4.2;
      shape matrix per §4.3). Known starting point: the connect hook
      reports "no registered extension provider" for ITEM_SHAPE
      mismatches (seen in `expression_defer_geom`). Since B3-StandardName,
      only FIELD carries a StandardName entry, so a BRACKET/VECTOR/
      FIELDBUNDLE/EXPRESSION standard-name mismatch is not detected
      (owner: accepted, legacy PERMISSIVE behavior; §4.3 note). The §4.3
      "error" cells return `BUILD_TRANSFORM_INCOMPATIBLE` (A23), not
      UNSUPPORTED. VECTORBRACKET -> VECTOR is UNSUPPORTED; every other
      VECTORBRACKET pair is INCOMPATIBLE. `write_info` writes only a
      declared variant, and `fieldBundleType=` in STRUCTURAL only.
      EXPRESSION -> FIELD is a match cell (owner: evaluation is per
      consumer, Q2; a differing consumer shows up as a non-shape
      mismatch until 3.12). Ordering runs in `resolve_characteristics`;
      `GraphStateItem%ordering` and its table are deleted here. Done.
- [x] 3.8 B3-StandardName (delete the subclass status check). Factory
      inserts for FIELD only (§4.3); an undeclared FIELD name is UNCHECKED.
      `write_info` is descriptive (both phases).
- [x] 3.9 B3-Attributes (always insert; report any scenario change).
      Inserted for FIELD/BRACKET/VECTOR/VECTORBRACKET/FIELDBUNDLE (§4.3
      excludes EXPRESSION). `write_info` is a successful no-op. No
      scenario changed (ESSENTIAL green; PENDING identical to HEAD).
- [x] 3.10 B3-QuantityType, B3-Conservation, B3-Normalization. Q9
      answered: each factory uses the declared `VariableSpec` value
      (legacy's fixed defaults are not reproduced; report it in the commit
      message as a parity difference with no current user, since no
      keyword or YAML key sets these fields yet). Field-dictionary defaults
      for them are future work, not this task. Preflight answers (owner):
      a mismatch was `BUILD_TRANSFORM_UNSUPPORTED` here, like Attributes,
      UngriddedDims and StandardName; task 3.22 moved five of the six
      (all but StandardName) to INCOMPATIBLE together. An undeclared quantity type stays SPECIFIED
      `QUANTITY_UNKNOWN`, and "unknown matches anything" (and `BASIS_NONE`
      matching any basis) stays a value rule in the subclass; say in
      adding-a-characteristic.md that such value wildcards are not status.
      Done as one commit with three trailers. `write_info` for each (own
      key only, either phase, SPECIFIED only); factories 14-16 for FIELD,
      BRACKET, VECTOR, VECTORBRACKET and FIELDBUNDLE (EXPRESSION is 3.12);
      conservation and normalization `is_mirror()` added to
      `requests_mirror()`. `merged_item_for_link` and the `GraphBuilder.F90`
      use list still name the three kinds (B4, §6.3). No scenario changed.
- [x] 3.11 B3-RestartMode (new kind). Default `RESTART_REQUIRED`, never
      compared, no transform (A15). `write_info` writes the item's own
      alias, so imports sharing an export keep their own modes.
      Preflight answers (owner):
      - The source field's mode is never used: every item, import or
        export, gets REQUIRED unless it declares its own, and an alias can
        override it. So an undeclared mode is SPECIFIED REQUIRED and is
        always written (a mode set directly with `MAPL_FieldSet` on a field
        whose spec declares none is overwritten; no repo code does that).
      - Per-alias writes: a plain `sync_to_info(DESCRIPTIVE)` on an alias
        would also rewrite field-wide keys (e.g. an import's UNCHECKED
        standard_name over the export's). Add
        `StateItemCharacteristic%is_per_alias()` (default false; true for
        RestartMode, and LongName once 3.13 lands) and a descriptor method
        that writes only per-alias entries; `populate_one` calls it.
        GraphBuilder names no kind. Amend design §3.5/§6.1.
      - INTERNAL items are not populated by the graph yet; not in this
        task (gap until B4; say so in the commit message).
      - No `restart_mode` YAML key; unit tests only.
      Done: `RestartModeCharacteristic` (kind 16, FIELD only), factory 13,
      `is_per_alias()` (true for RestartMode and LongName),
      `StateItemDescriptor%sync_per_alias_to_info`, `populate_one` calls it
      with the alias wrapped in a `GraphStateItem`; `declared_restart_mode`
      and its advertise block are gone. LongName is now written from
      `populate_one` (an UNCHECKED import alias writes nothing, so it
      reads `unknown` under graph-only until B4 writes the owning field's
      slot; a declared one is written; a bundle is never written per alias, because its
      long_name is bundle-wide). INTERNAL items stay unpopulated (gap
      until B4). Design §3.4/§3.5/§5.1/§6.1 amended.
- [x] 3.12 B3-Expression (new kind; mechanical half of the owner's split).
      Add the Expression characteristic and give the EXPRESSION factory
      applicability per design §4.3 (every FIELD kind except
      STANDARD_NAME, from the declaration; units by the Field rule, Q3).
      Evaluation is unchanged (still `run_expression_hook`). Also closes
      the ungridded-dims gap: B3-UngriddedDims gave EXPRESSION no
      UNGRIDDED_DIMS entry, so an EXPRESSION export silently matches a
      FIELD import that declares ungridded dims (owner accepted this
      until this task). The units rule for inputs (no conversion to the
      expression's units, Q3) is 3.23, not this task.
      Landed: new `ExpressionCharacteristic` (ValueCharacteristic,
      `needs_extension_for` always false, no Info key, build_transform
      unsupported) and `EXPRESSION_CHARACTERISTIC_KIND`. Widened the
      shared `has_field_metadata` helper (now just delegates to
      `carries_field_description`, which already lists EXPRESSION) to
      give EXPRESSION an entry for UngriddedDims, Attributes,
      QuantityType, Conservation and Normalization in one change, plus
      widened the RestartMode factory's FIELD-only guard. Removed the
      `declared_expression` field/accessors from `StateItemDescriptor`;
      connection resolution now reads the expression string off the new
      characteristic (`get_item_expression` in
      `GraphBuilder_ConnectionResolution_smod.F90`).
      Deviations: (1) `make_expression_characteristic` defaults to an
      empty string, rather than asserting `spec%expression` is always
      allocated, because `to_itemtype.F90` only asserts
      "`has_expression` implies `class: expression`", not the converse
      - an EXPRESSION item with no `expression:` key is not rejected at
      parse time, and several pre-existing test fixtures build such
      items for other characteristics' tests. (2) also updated the
      QuantityType/Conservation/Normalization "carrying shapes" tests in
      `Test_VariableSpecCharacteristics.pf`, not only the UngriddedDims/
      Attributes/RestartMode ones named in the brief, since they share
      the same widened `has_field_metadata` helper.
- [x] 3.23 B3-ExpressionPerConsumer (after 3.12). Build the per-consumer
      evaluation per design §2.4: a dedicated expression path in the
      connection code chosen by item type (like wildcard imports); inputs
      converted to the evaluation's geometry, vertical grid and precision
      in their own units; result carries the declared characteristics,
      then ordinary chains to each consumer; evaluation at the
      consumer's precision; no `vertical_dim_spec` takes the consumer's
      stagger; consumers that agree on geometry, vertical grid and
      precision share one evaluation; the export is a placeholder (never
      evaluated, never in a state; ACCEPT_TRANSFER only checks the names
      exist); an expression referring to another expression is reported
      unsupported. Tests: a ready scenario whose consumers differ in
      precision/units (B: R4; C: R8 in cm; expect two evaluations, C sees
      300 cm) and a `check_graph` expression entry.
      `expression_defer_geom` stays pending until C1. Known starting
      point: the connect hook reports "no registered extension provider"
      for EXPRESSION mismatches.
      Preflight answers (owner, 2026-11-05):
      1. The evaluation is the consumer's: inputs are converted to the
         consumer's geometry, vertical grid and precision, then evaluated
         there (as legacy). Evaluating on the inputs' native grid is 3.24.
         Until C1, a grid or vertical-grid difference between inputs and
         consumer is reported unsupported ("no registered extension
         provider") and the import stays unwired, never silently wired on
         the wrong grid.
      2. Two consumers share an evaluation when the kinds of the
         MIRRORED-export allowlist (design §2.4: geometry, vertical grid,
         vertical stagger, vertical orientation, precision) agree. The
         list comes from the allowlist table through a descriptor method;
         GraphBuilder names no kind (D6). Other differences (units, ...)
         are ordinary steps after the shared result.
      3. Each input's target is its own descriptor with only the
         allowlist kinds replaced by the evaluation's. An input's
         ungridded dims, attributes, conservation and units are never
         compared with the consumer's (legacy compared the whole consumer
         description). A mismatch between result and consumer is still a
         connection conflict, at the result's end.
      4. Evaluation is always at the consumer's precision; a declared
         `typekind:` on an expression has no effect (say so in docs and
         the commit message).
      5. Evaluation nodes and their input chains belong to the connecting
         (parent) graph, like every other chain.
      6. Confirmed: a unit-less expression's result is unchecked (no units
         step; the result takes the import's units); `check_graph` gains an
         `expression: true` form for multi-input transforms (per-input
         steps, then the result's chain); the expression hook only checks
         names, and an expression naming an expression (or itself), a
         missing name, or an empty expression ('' from 3.12) is reported
         there even with no consumer; the result copies the expression's
         characteristics after the 3.36 fallback; if graph mode changes
         what `<root>` exposes (e.g. `A/expr(1)`), the test author runs
         the current graph mode first and returns to the owner before
         changing registry-mode expectations.
      Split (owner): 3.23a B3-ExpressionPlaceholder, then 3.23b
      B3-ExpressionPerConsumer, each test-first.
      - [x] 3.23a B3-ExpressionPlaceholder: the export becomes a
        placeholder (no edges, never materialized); the hook becomes the
        names check (missing, nested, self, empty); a connect-time
        expression path builds one evaluation per consumer, in the
        connecting graph, shared only when everything matches; idempotent
        on a second `resolve_connections`; `check_graph` `expression:`
        form; existing `expression`/`expression_match` scenarios keep
        passing (expectation changes go to the owner first).
        Owner (2026-11-05): a problem the hook finds (missing name, nested
        expression, self-reference, empty expression) fails the
        component's initialize, in both validation modes: the production
        call passes the report vectors and fails on a non-empty one
        (legacy fails at registry time on a missing name). A positive
        "consumers differing in precision get two evaluations" test is
        3.23b, since 3.23a reports any input conversion unsupported.
        Unit-less result (owner, 2026-11-05): structural only: no units
        step, and the field created for the result carries the import's
        units; the result's descriptor keeps units UNCHECKED.
        Status: done. Hook = names check (missing -> unresolved; nested,
        self, empty -> unsupported), fatal via
        `fail_on_expression_reports` in ACCEPT_TRANSFER. Connect-time
        `build_expression_evaluation` (key `EXPREVAL:`) in the connecting
        graph; result then resolved as an ordinary pair. Allowlist kinds
        via `StateItemDescriptor%get_consumer_adopted_kinds`.
        `ExpressionTransform` overrides the input-port queries (gfortran
        polymorphic-copy defect with multi-entry port maps). No scenario
        expectation changes beyond the test author's `check_graph` entry.
      - [x] 3.23b B3-ExpressionPerConsumer: inputs converted per answers
        1-4 (precision steps on inputs, no units steps on inputs), sharing
        by the allowlist kinds (answer 2), result's own units then ordinary
        steps to each consumer, stagger from the consumer when the
        expression has no `vertical_dim_spec`, the ready scenario (B: R4;
        C: R8 in cm, 300 cm) and its `check_graph` entry.
        Preflight answers (owner, 2026-11-06): (1) a SPECIFIED value
        declared on the expression wins over the consumer's for the
        adopted kinds, except typekind (always the consumer's); a
        mismatch is then an ordinary result-to-consumer step. (2) A
        DEFERRED adopted kind on the consumer or an input: build nothing,
        wait and retry like an ordinary pair. (3) A unit-less input that
        needs a precision step: allow empty units when materializing its
        chain if field creation tolerates them, else report it
        unsupported with a note. (4) An incompatible adopted kind on an
        input is a connection conflict. (5) Unsupported expression
        inputs also feed `note_unsupported_pair` (strict freeze).
        Status: done. Each input reaches
        the result's adopted-kind values through an ordinary extension
        chain (`find_or_build_extension_chain`, goal = only the adopted
        kinds the input has), built and materialized before the transform
        and result node; precision gets real steps, other adopted kinds
        report their kind name as unsupported (and `note_unsupported_pair`),
        an incompatible outcome goes to `conflicts`. Never a units step on
        an input. A DEFERRED adopted value on the consumer or an input
        builds nothing and waits. The allowlist gained `declared_wins`
        (`get_consumer_adopted_kinds(..., declared_wins=)`): a SPECIFIED
        value declared on the expression wins except typekind.
        Generic change: `build_chain` now gives every extension node a
        descriptor (the export's characteristics with the import's values
        for the kinds converted so far); the tests read it. Unit-less
        inputs: chain materialization accepts empty units
        (`allow_unitless`). An input status conflict is reported
        unsupported without `note_unsupported_pair`, as an ordinary
        pair's status conflict is.
        `expression_per_consumer_r8` failed in registry mode at the
        legacy coupler ("unsupported itemType") before the flip.
        Status (C2-expr-r8-flip): `expression_per_consumer_r8` is ready,
        graph mode only, with `graph_owns_user_states` in its table entry
        (the switch skips the legacy couplers); 11 checks pass on NAG and
        gfortran. With the switch dropped it fails again at the legacy
        coupler.
        Quiet-INCOMPATIBLE adopted-kind input test waived: no adopted
        kind's build_transform returns INCOMPATIBLE; VerticalGrid's
        incompatible path _FAILs.
        The unit-less materialization test needed an ESMF geom id on its
        raw geom (fixture fix, owner): geometry compares geom objects by
        that id, as legacy GeomAspect does.
- [x] 3.24 B3-ExpressionNativeGrid: MOVED to the post-merge backlog of
      `docs/graph/plans/separation-of-concerns.md` (owner, 2026-11-10);
      not started, nothing depends on it. Original text: (optimization, after 3.23; owner
      request). If all inputs share one geometry (or vertical grid),
      evaluate on that native grid and transform only the result to each
      consumer.
- [x] 3.25 B3-InternalNoMirror (design Q13; owner: "very important to
      settle this correctly"). An INTERNAL item never has any mirror
      property, and an EXPRESSION is EXPORT only (never IMPORT or
      INTERNAL; a re-exported internal is never an expression). Owner
      scope: enforce in VariableSpec only, one rule used by both A21
      checks (a mirror request is allowed only on an IMPORT or an
      EXPRESSION export); the graph needs no check of its own, since its
      factories give MIRRORED only for an explicit request or an
      EXPRESSION. Report each legacy mirror default on an internal to the
      owner before changing it.
      Done: VariableSpec only. `intent_rule_violation` in
      `VariableSpec_private.F90` is the one rule, by intent and item type: a mirror
      request is allowed only on an IMPORT or an EXPRESSION export, and an
      EXPRESSION only as an export. `make_VariableSpec` and the backstop
      `check_intent_rules` (was `check_no_mirrored_entry`) both call it;
      the YAML parser and `MAPL_GridCompAddSpec` reach it through
      `make_VariableSpec`. A YAML `expression:` key under `import:` or
      `internal:` is not stopped by the parser, only by this rule. Legacy
      defaults left alone (owner): see design.md Q13. New messages: `mirror
      is only valid on an import or an expression export: <name>` (+
      ` (<KIND>)` from the backstop) and `an expression is only valid on an
      export: <name>`.
- [x] 3.26 B3-RulesReview (owner; after 3.25, before 3.12). The default,
      connection and intent rules emerged from use cases, and the better
      generalization was often not obvious at the time. Review them
      together; read-only, output is a report and proposals, no code.
      1. One rules matrix, a row per characteristic: applicable shapes;
         intents that may declare it; undeclared result per intent
         (SPECIFIED default, DEFERRED, UNCHECKED, dropped); mirror (where
         allowed, mechanism); compared at connect, and its Transform;
         field-wide or per-alias write; legacy behavior. Plus a shape x
         intent table (which shapes may be IMPORT, EXPORT, INTERNAL).
      2. Irregularities, each with a proposed generalization and its
         consequences (tests, legacy behavior, later tasks). Seeds: flags
         vs string sentinel for mirror; several "Declared by" keywords;
         DEFERRED vs UNCHECKED for undeclared values; per-intent
         fallbacks (LongName); shape-specific exemptions (EXPRESSION);
         rules in the graph but not legacy or the reverse; rules found
         late (Q13, expression export-only); value wildcards that match
         anything (QUANTITY_UNKNOWN, BASIS_NONE): should "matches
         anything" also mean "adopt the export's value"? Legacy
         `QuantityTypeAspect%connect_to_export` would copy the export's
         quantity type and basis onto the import, but it has no production
         caller (the review's finding: `StateItemSpec%connect_to_export`
         connects only the CLASS aspect), so legacy has no adopt rule in
         practice; the graph compares and does not copy, so an undeclared
         import stays UNKNOWN. Owner (after 3.10): fairly certain the
         legacy rule is incorrect; owner (R1): no adopt rule. From develop (#5470): legacy restart read now marks a
         field missing from the restart file as `bootstrapped`
         (`FieldGet(..., bootstrapped=)`, field-wide Info key); relate it
         to RestartMode (who reads it, whether the graph needs it).
      3. The owner's rules list (kept by the owner outside the repo; meta
         passes it in) is an input: for each rule, list every source or
         legacy behavior that violates it and any use case against it.
         Report conflicts as open questions; never resolve one in either
         direction without the owner. First known conflict: the owner
         allows import and export to differ in standard_name, but
         standard_name is field-wide (not per-alias) in legacy and graph.
      4. One owner decision session; each accepted correction becomes its
         own small task. The matrix likely replaces or extends design
         §5.1 as a stable doc (decided at the session).
      Status: done. Owner decisions recorded, including the R2 follow-up
      (`docs/graph/rules-review.md` §4a, session 2026-11-02; design.md
      A24). Follow-up tasks 3.28-3.36; R1's doc corrections done here
      (this seed, design.md §1/§4.3/§5.1/A23, `adding-a-characteristic.md`);
      R13 folded into 3.27. Item 4's "the matrix likely replaces or
      extends design §5.1" was not decided; the matrix stays in
      `rules-review.md` for now. That open item does not block this task:
      it is a documentation-home choice, with no rule depending on it.
      Report:
      matrix, shape x intent table, irregularities, owner-list check,
      sixteen ordered questions R1-R16.
      Finding: legacy `StateItemSpec%connect_to_export` calls only the
      CLASS aspect's, so the outer-map value aspects' `connect_to_export`
      (including QuantityType "adopt") never runs in production; only
      QuantityType, Conservation, Normalization and Geom have unit-test
      callers. Exception: `VectorClassAspect%connect_to_export` calls
      `StandardNameAspect%connect_to_export` per component, so VECTOR
      standard_name adoption is live. Also: legacy fill_value keeps the
      import's declared value when both differ, despite its "src value
      wins" log text.
- [x] 3.27 B3-StandardNameValidation (owner, after 3.22; decision may be
      taken in 3.26's session; must land before B4). Legacy
      `StandardNameAspect%matches` consults the field-dictionary
      `ValidationMode`: STRICT rejects two different specified names;
      PERMISSIVE (the default, `FieldDictionaryConfig.F90`) connects with a
      warning and the export's name wins. The graph has no validation mode
      and never connects such a pair (StandardName is
      `BUILD_TRANSFORM_UNSUPPORTED`). Decide and implement the graph
      behavior (permissive: match with a warning, export's name wins;
      strict: INCOMPATIBLE), reading the mode without making the graph
      depend on StateRegistry. This is also the "graph validation mode"
      design §4.3 defers (standard name across BRACKET -> FIELD). Related:
      the owner's "system-wide setting for enforcing standard_name for
      Fields" (V3): in scope here (owner, 3.26 R13). Design one
      configuration surface for both: the validation mode for a mismatch,
      and a switch that makes a FIELD with no declared standard_name an
      error (or warning) at advertise, applied in both paths. Design-bearing
      (opus author; owner settles the design). Also settle here, in general
      form (owner, 3.37 review): whether an UNSUPPORTED pair (legal, no
      Transform built yet: basis-only QuantityType, geometry before C1,
      a stagger-only mismatch) is fatal at freeze in strict mode once
      legacy is off; proposed: warn by default, fail in strict mode.
      Owner: concrete use cases decide what must be supported.
      From develop (#5443): a configurable option now suppresses the
      PERMISSIVE-mode standard_name warnings; the graph behavior must honor
      it too.
      Done (owner settled the design 2026-11-04; design A25): one surface,
      the `field_dictionary:` keys (`validation_mode`,
      `suppress_standard_name_warnings`, new `require_standard_name`), read
      by graph code only through `graph/ValidationPolicy.F90`. General
      tolerated-mismatch hook `StateItemCharacteristic%match_warning`,
      collected by `resolve_characteristics` for every PAIR_MATCH and
      reported as `connection_warnings`. StandardName: strict INCOMPATIBLE;
      permissive wired directly with legacy's warning; UNCHECKED export vs
      declared import warns; both suppressible. BRACKET carries StandardName.
      V3 in `make_VariableSpec`: FIELD, any intent, mirror counts as
      declared, `'<unknown>'` does not; error in strict, warning otherwise.
      UNSUPPORTED pairs are recorded per component and
      `GraphBuilder%check_unsupported_pairs` fails on them in strict mode;
      deviation (owner): not called from `run_graph_freeze_hook` yet,
      because 11 strict ESSENTIAL scenarios have GEOMETRY / VERTICAL_GRID
      pairs legacy connects. The legacy-retirement step adds the call. 3.36
      gets `validation_is_strict()` and the method shape in A25.
- [x] 3.28 B3-ShapeIntentRules (3.26 R5: owner chose (a); after 3.26,
      before 3.27). Add two shape x intent rules to `intent_rule_violation`
      (`VariableSpec_private.F90`), so both `make_VariableSpec` (both modes)
      and the `check_intent_rules` backstop apply them: BRACKET and
      VECTORBRACKET are never IMPORT (INTERNAL stays allowed, owner:
      conceivable), and WILDCARD is IMPORT only. New messages in the A21
      style, e.g. `a bracket is not valid on an import: <name>` and `a
      wildcard is only valid on an import: <name>`. Legacy keeps its
      connect-time `_FAIL` (now unreachable for a declared import). Grep
      tests and scenarios for a BRACKET/VECTORBRACKET import or a
      non-import WILDCARD first and report any (the review found only the
      `history_wildcard` import). Small; same function as 3.25. Author
      haiku (sonnet review).
- [x] 3.29 B3-ConnectionIntentCheck (3.26 R10: owner chose (a) "Except for
      Re-export connections!"; R9 (a); before 3.27). In
      `parse_connections` (`ComponentSpecParser/parse_connections.F90`),
      `_ASSERT` that a SimpleConnection has `src_intent: export` and
      `dst_intent: import` (the defaults). A ReexportConnection
      (`dst_intent: export`) is exempt from that rule; its `src_intent`
      must be `export` or `internal`, as `ComponentSpec%reexport` already
      requires. MatchConnection (`all_unsatisfied`) is unaffected. Parser
      test for each case. The review's scenario grep found no violator.
      Preflight answer (owner): yes, assert the ReexportConnection
      `src_intent` too ("YAML parsing is mostly for testing; robustness
      welcome"). Make `parse_connections` public for testing if the
      tests need it. Small parser task. Author haiku (sonnet review).
- [x] 3.30 B3-WildcardFieldOnly (3.26 R6: owner chose (a), "We will relax
      if we encounter reasonable use cases."; before 3.27). In
      `resolve_wildcard_destination`
      (`GraphBuilder_ConnectionResolution_smod.F90`), add only FIELD
      exports (ItemShape) as Wildcard members; skip others (VECTOR,
      BRACKET, EXPRESSION, FIELDBUNDLE, STATE, SERVICE); a skip is not a
      connection conflict. Preflight answers (owner): one debug-level log
      line per skipped export, plus one warning when every match was
      skipped. Apply the same filter in `wildcard_import_has_match`, via
      one shared predicate on `var_spec%itemType`, so the two cannot
      disagree. Add a graph-builder delta spec: the landed spec says every
      match is a member. Legacy parity: legacy production fails on a
      non-Field match (`WildcardClassAspect.F90:84-95`); legacy is not
      changed here. Test with a Wildcard whose regex matches a field and a
      bundle; check `history_wildcard` is unchanged. Author sonnet.
- [x] 3.31 B3-OneExportPerImport: DROPPED (owner, 2026-11-04, with the
      no-legacy-fixes rule). The graph keeps its silent "first wins" skip.
      Original text kept for reference. (3.26 R8: owner chose (a); after 3.27,
      before 3.12/3.23, which change the same connection-resolution
      code). First grep real configs (`tests/scenarios`, and any GEOS
      configurations the owner points to) for two connections to one
      import, including a
      MatchConnection overlapping an explicit SimpleConnection; report
      before changing code. Then make a second connection to an import an
      error in both paths, Wildcard exempt: legacy restores the
      commented-out assert in `SimpleConnection%activate`
      (`SimpleConnection.F90:105`); the graph reports a connection
      conflict in `resolve_one` instead of the silent "first wins" skip.
      Follow-up only if the grep finds Match/Simple overlaps: ask the owner
      whether the explicit SimpleConnection may win. Author sonnet.
- [x] 3.32 B3-GeomAProbe (3.26 R16: owner chose (a); before 3.27;
      independent). A probe test (legacy registry path) for a component
      with a component geom G1 and an export declaring an item geom G2:
      does `StateRegistry%set_geometry` / `GeomAspect%set_geom`
      (`StateRegistry_SetGeometry_smod.F90:27`, `GeomAspect.F90:205-224`)
      replace G2 with G1? Report the result. If confirmed, the owner treats
      it as a legacy bug (item wins); the fix is a separate small legacy
      task. If not confirmed, record that in `rules-review.md` §2.6. Test
      only. Preflight answers (owner): if confirmed, commit the item-wins
      probe with `@disable` and the reason; if refuted, commit it enabled
      and add the §2.6 note. Probe the vertical grid the same way ("vgrid
      and geom should be symmetric in most regards"), and report whether
      a stale `geom_id` remains after the overwrite. Author sonnet.
- [x] 3.33 B3-InternalReexport (3.26 R11: owner chose (a); after B4).
      Graph path for an INTERNAL -> EXPORT re-export: the graph skips
      ReexportConnection today (`class default`,
      `GraphBuilder_ConnectionResolution_smod.F90:977`) and internals have
      no port (`GraphBuilder_Advertising_smod.F90:115`). Give a re-exported
      internal an export port bound to the internal's node (no mirror on
      the internal, Q13; the export is an export, A21), and resolve
      child-export re-exports (R9) the same way or through export
      propagation, whichever the design shows is one rule. Must pass
      `scenario_1`, `scenario_2`, `propagate_geom` and
      `scenario_reexport_twice` in graph mode. Required before graph-only
      mode. Author sonnet (opus review).
      Status: done. `run_reexport_hook` (after `run_advertise_hook`,
      before export propagation) adds an export port named by the
      destination, bound to the source's existing node (own internal by
      resource index, descendant export by port name). Flipped
      `scenario_reexport_twice`; `propagate_geom` stays pending on the
      geometry gap (C1-geom-b). State population still reads var specs,
      not export ports; a re-exported internal gets its user Field in the
      export state only through legacy until 3.41b populates from ports.
- [x] 3.34 B3-FillValue (3.26 R14: owner chose (c); with or after B4's
      materialization, 4.2). fill_value is not a characteristic: when the
      graph materializes a field, it fills it from the owning
      `VariableSpec`'s `fill_value` (the export's, or the internal's); an
      import's declared fill_value has no effect, as in legacy data today
      (rules-review.md §0). Also fix the legacy log text in
      `FieldClassAspect%connect_to_export` (`FieldClassAspect.F90:345`,
      "(src value wins)" while the destination keeps its value) to say
      what happens; that one-line part may land earlier on its own. Also
      update design §1's `fill_value` row. Author sonnet.
      Status: done. `StateItemDescriptor` carries a plain `fill_value`
      (`set_fill_value`/`get_fill_value(found)`), recorded by
      `advertise_one` for every intent; `build_chain` copies the export
      node's value onto each extension node; `materialize_field_extension`
      calls `FieldSet` after `MAPL_FieldEmptyComplete`. An import's value is
      never read. Scope is extension fields (the only fields the graph
      creates today; exports/internals still go through legacy `allocate`,
      which fills). Matches legacy: `make_extension` clones the export's
      aspects and does not substitute the class aspect for Field->Field.
      No import-ignored test: `find_or_build_extension_chain` takes only
      the import map, so an import's value is unreachable in that API. The
      legacy log text reads "(dst value kept)".
- [x] 3.35 B3-QuantityTypeSuppliers (3.26 R2: owner chose (c) and
      rollout (b), suppliers first; before 3.27, independent of it). Give
      QuantityType its three supply paths, with no change to the undeclared
      status yet (3.36 does that):
      1. YAML key `quantity_type:` in `parse_var_specs` (values: the
         `QuantityType` names, case-folded; unknown value is an error).
         Key name confirmed by the owner.
      2. A `quantity_type=` keyword on `make_VariableSpec` (today only
         field assignment sets `VariableSpec%quantity_type`,
         `VariableSpec.F90:244-253`). Legacy `make_QuantityTypeAspect`
         still ignores it (Q9).
      3. Harvest: extend `graphbuilder_harvest_deferred` /
         `fill_deferred_from_handle` (`GraphBuilder_Geometry_smod.F90`) to
         read `quantity_type_metadata` set by the user's `MAPL_FieldSet`
         (through `MAPL_FieldGet`, A19) into `CharacteristicDefaults`, and
         give `QuantityTypeCharacteristic` a `resolve_deferred` that takes
         it. `MAPL_FieldGet` returns mirror metadata for an absent key;
         treat that as "not supplied". Imports stay skipped. Tests: YAML
         and keyword paths set the field; a hand-built DEFERRED entry is
         filled from a field whose component called `MAPL_FieldSet`.
      No YAML key for basis: `VariableSpec` has no basis field; adding one
      is out of scope unless the owner asks. Preflight answer (owner): a
      `quantity_type` declared on a shape that has no QuantityType
      (STATE, EXPRESSION, SERVICE, WILDCARD) is ignored, as other
      field-metadata keys are today; a general check is a D3 item.
      Author sonnet (sonnet review).
      Done: YAML key, `quantity_type=` keyword and harvest landed;
      `resolve_deferred` takes the value only for a DEFERRED entry.
      Mirror (Field) and `QUANTITY_UNKNOWN` (bundle, or explicit) read as
      not supplied; the harvest keeps its geometry gate, so a
      non-geometry DEFERRED kind is not read before the item's geom
      exists (3.36's fallback ordering must allow for it). Tests: parser
      (name, case, UNKNOWN, bad value), keyword, `resolve_deferred`
      (DEFERRED/SPECIFIED/UNCHECKED), harvest (Field, bundle, import).
- [x] 3.36 B3-QuantityTypeDeferred (3.26 R2: owner chose (c), rollout (b)
      and imports (a); after 3.27 and 3.35). Change the QuantityType
      factory (`make_quantity_type_characteristic`): undeclared on an
      EXPORT or INTERNAL is DEFERRED; undeclared on an IMPORT is
      UNCHECKED; declared stays SPECIFIED. Applicability is unchanged (the
      `has_field_metadata` shapes), so a shape without the kind never
      fails freeze. Then the switch: by default an unsupplied DEFERRED
      QuantityType at freeze is a warning and falls back so nothing
      breaks; strict mode makes it an error. Design points (report before
      building; stop and ask if any needs a new rule):
      - Use the configuration surface 3.27 designs (the reason this runs
        after 3.27), not a second switch.
      - Keep the freeze check kind-agnostic (D6): the characteristic
        says whether an unsupplied DEFERRED is fatal under the current
        mode; the descriptor does not name QuantityType.
      - The fallback must happen before a waiting connection is reported:
        a DEFERRED side makes `resolve_pair` return PAIR_WAIT, and freeze
        fails on waiting connections (`GraphBuilder_Freeze_smod.F90`).
        Proposed fallback: UNCHECKED, with the warning, at the last
        harvest point before resolution completes.
      - UNKNOWN value wildcard: proposed to treat a declared
        `QUANTITY_UNKNOWN` as undeclared (as PhysicalUnits does for
        `'<unknown>'`) and drop the UNKNOWN wildcard from
        `needs_extension_for`. Preflight question for the owner: does the
        BASIS_NONE wildcard stay (nothing can declare a basis today)?
        Frame it knowing the owner plans to split basis into two axes
        (mass/volume and dry/moist) in the callback-physics change's
        first phase.
      - No adopt rule (R1): an UNCHECKED import matches and copies
        nothing.
      Tests: `Test_StatusResolution`, `Test_VariableSpecCharacteristics`
      QuantityType rows, a freeze test for each mode, and a scenario check
      that the default mode changes no ESSENTIAL result. Also (owner, 3.37
      review): a GraphBuilder-level test that a basis-only pair (same
      quantity type, two real bases, declared with `quantity_type:`) is
      listed as unsupported, not in `connection_conflicts`. Update design
      §5.1's QuantityType row and A24 to "landed". Strict as the default
      is a later change, once real configs comply (owner). Owner may tweak
      the rules after concrete use cases. Author opus (sonnet review).
      Preflight answers (owner, 2026-11-05): the permissive fallback logs
      one summary warning per component (count and the first few names),
      not one per item; BASIS_NONE stays a basis wildcard (the
      basis-axis split redoes it); a declared `QUANTITY_UNKNOWN` counts as
      undeclared, the UNKNOWN wildcard leaves `pair_outcome`, the two 3.37
      tests that hand-build SPECIFIED `QUANTITY_UNKNOWN` are rewritten, and
      A23's UNKNOWN sentence is marked superseded. For the
      conservative-regridding characteristics in general, do the expedient
      thing: nothing uses them yet; use cases and ExtData support come
      after legacy is dropped. Run test-first.
      Implementation answers (owner, 2026-11-05): strict `validation_mode`
      alone does not make an unsupplied QuantityType fatal (the ESSENTIAL
      scenarios run strict and declare none). A new `field_dictionary:` key
      `require_quantity_type` (default false), read through
      `graph/ValidationPolicy.F90` like `require_standard_name`, makes it
      fatal; otherwise it falls back with the summary warning in both
      modes. The fallback also runs at the start of `resolve_connections`
      (its only production caller is the connect hook), so tests that call
      it directly keep working.
      Done: `make_quantity_type_characteristic` branches on state_intent
      (EXPORT/INTERNAL undeclared -> DEFERRED, IMPORT undeclared ->
      UNCHECKED, declared -> SPECIFIED; a declared `QUANTITY_UNKNOWN`
      counts as undeclared). `pair_outcome` drops both `QUANTITY_UNKNOWN`
      disjuncts (BASIS_NONE stays a basis wildcard).
      `QuantityTypeCharacteristic%is_deferred_fatal` returns
      `quantity_type_required()` (`graph/ValidationPolicy.F90`), backed by
      the new `field_dictionary: require_quantity_type` key
      (`FieldDictionaryConfig.F90`, same pattern as `require_standard_name`;
      no legacy counterpart). The kind-agnostic fallback is
      `StateItemDescriptor%fallback_unsupplied_deferred` (per descriptor)
      plus `GraphBuilder%fallback_unsupplied_deferred` (per component: one
      summary `GraphResolutionEntry` per characteristic kind that fell
      back, count plus first 5 item names plus "..."). The connect hook
      calls it explicitly before `resolve_connections` and returns it
      through the hook's own `fallback_warnings` optional argument, logged
      there; `resolve_connections` also applies it, recursively over its
      own component and every descendant, before resolving any connection
      pair (deviation from the preceding paragraph's "its only production
      caller is the connect hook": a connection pairs this component's
      items with a descendant's via
      `resolve_qualified_component`/`find_export_node_id`, so a direct
      `resolve_connections()` call on a parent needs its children's own
      DEFERRED items converted too, not just its own - otherwise
      `resolve_pair` returns `PAIR_WAIT` for the whole pair and skips
      wiring it, which segfaulted an existing
      `Test_GraphBuilder_ConnectionResolution.pf` test downstream). Both
      call sites are idempotent (a descriptor already converted has
      nothing left to report), so the summary warning is produced exactly
      once per component regardless of which path reaches it first.
      ctest -L ESSENTIAL: NAG 69/69, gfortran 69/69.
- [x] 3.37 B3-QuantityBasisUnsupported (owner; amends A23 for basis only;
      before 3.36). Today any QuantityType mismatch is
      `BUILD_TRANSFORM_INCOMPATIBLE` (`QuantityTypeCharacteristic.F90`
      `needs_extension_for` and `build_transform`). Change: two values
      with the same quantity type that differ only in basis (e.g.
      `BASIS_DRY_MASS` vs `BASIS_WET_MASS`, neither `BASIS_NONE`) report
      `BUILD_TRANSFORM_UNSUPPORTED` (a conversion is possible, not built
      yet); a quantity-type mismatch stays INCOMPATIBLE. Tests for
      both, in the QuantityType rows of the characteristic tests. Update
      design §5.1's QuantityType row. Preflight answer (owner): the
      quantity axis "matches" by the existing wildcard rule, so
      `QUANTITY_UNKNOWN` against a known type with two different real
      bases is also UNSUPPORTED (one rule with `needs_extension_for`).
      Author sonnet (sonnet review).
      Done: `QuantityTypeCharacteristic` has one private `pair_outcome`
      (match / basis-only / quantity mismatch) shared by
      `needs_extension_for` and `build_transform`; the quantity axis wins,
      so a quantity mismatch with differing bases stays INCOMPATIBLE. Tests
      in `Test_StateItemCharacteristicSubclasses.pf` (both outcomes, both
      directions, UNKNOWN + differing bases, BASIS_NONE pairs). Docs:
      design §5.1 row and A23, ARCHITECTURE.md, adding-a-characteristic.md,
      rules-review.md. No scenario or GraphBuilder test reaches
      QuantityType (no scenario or builder test declares a quantity type);
      the reporting path is kind-agnostic (UNSUPPORTED is logged as "no
      registered extension provider ..." instead of "connection conflict").
- [x] 3.38 B3-GeomAItemWins (owner: legacy bug, item wins; after 3.37,
      before 3.27). Legacy fix for the overwrite 3.32 confirmed. At GEOM_A,
      an item that declares its own geom or vertical grid keeps it:
      `GeomAspect%set_geom` and `VerticalGridAspect%set_vertical_grid` do
      not replace an item value with the component's, and the item's
      `geom_id` stays consistent with the geom it ends with. Remove
      `@disable` from the three `Test_Couplers.pf` probes. Also fix, or
      report with a reason, the findings 3.32 recorded (plan, B3-GeomAProbe
      Status): `VerticalGridAspect` ignores its `geom`/`typekind` arguments;
      `make_VerticalGridAspect` sets FROM_COMP with no vertical grid
      (unlike `make_GeomAspect`); `StateItemSpec%set_geometry`'s geom
      else-branch calls `aspects%at` and never inserts; add a test for an
      item that declares only a `geom_id`. Update rules-review.md §2.6/R16
      to "fixed". Author sonnet (sonnet review).
      Preflight answers (owner):
      - Shape of the fix (preflight proposal, accepted): `set_geom` /
        `set_vertical_grid` stay unconditional (the aspect constructor,
        `update_from_payload` and `ExpressionClassAspect` also call them);
        a private "declared by the item" flag, set by `make_GeomAspect` /
        `make_VerticalGridAspect` only when the `VariableSpec` has its own
        geom / vertical grid, makes new component-path setters no-ops;
        `StateItemSpec%set_geometry` uses those. A component geom still
        replaces an earlier component geom.
      - An item declaring only a `geom_id`: keep today's result (component
        geom, item's id) and pin it with a test. Owner: `geom_id` next to
        the geom was a mistake; graph nodes serve that purpose now. Do not
        extend `geom_id` use (retirement is a Later item in the plan).
      - `MAPL_GridCompSetGeometry` (explicit, one named item) still
        replaces; item wins only against the component-wide push at
        GEOM_A.
      - Mirror requests (`geometry_mirror`, vertical-grid mirror) are out
        of scope: task 3.39.
      - Fix the swallowed `rc` in `StateItemSpec%set_geometry` (contained
        `target_set_geom`, then `_RETURN(_SUCCESS)`); if latent failures
        surface, stop and report.
      - Fix the three other probe findings: delete `VerticalGridAspect`'s
        dead `geom`/`typekind` arguments; `make_VerticalGridAspect` with no
        vertical grid is MIRRORED, not FROM_COMP; the `set_geometry`
        else-branch inserts the aspect.
      - Pipeline trial (owner): tests first by a separate test author on a
        different model family, a test review, then a sonnet implementer
        that may not edit the tests, then the implementation review.
      Done. Deviation: `make_VerticalGridAspect` with no grid is MIRRORED
      except stagger NONE, which is SPECIFIED (the expression scenarios need
      it; owner-confirmed 2026-11-04; revertible: drop the `is_none()` branch
      in `make_VerticalGridAspect`; 3.39 revisits it). `make_VerticalGridAspect` lost its
      `component_geom` / `component_geom_id` arguments.
- [x] 3.39 B3-GeomMirrorLegacy: DROPPED (owner, 2026-11-04: no more
      legacy-behavior fixes; the difference is recorded in
      `docs/graph/scenario-parity.md`, "Legacy and graph behavior
      differences"). Original text kept for reference. Legacy
      `make_GeomAspect` / `make_VerticalGridAspect` ignore `mirror_geom` /
      `mirror_vgrid`, so the GEOM_A push gives a mirroring item the
      component geom and it becomes SPECIFIED, while the graph keeps it
      MIRRORED and adopts the exporter's geometry at connection. Make
      legacy honor the mirror request: the component push skips mirroring
      items, which take the exporter's geometry at connection. Affects
      `statistics_real` and `history_1` (not ESSENTIAL); report their
      results. Author sonnet (sonnet review).
- [x] 3.40 B3-SetSharedMaterialize (after 3.14 and B4 4.2; owner,
      2026-11-05). `set_shared_characteristic` rebuilds the affected ESMF
      payloads after changing a structural characteristic (§7.3's
      `sync_to_info`), using B4's empty-create + `sync_to_info(STRUCTURAL)`
      + `MAPL_FieldEmptyComplete` helper, not the FieldDelta path (§3.2).
      Tests use a real field. Author sonnet (sonnet review).
      Done: `set_shared_characteristic` rebuilds the node's payload
      when the new characteristic `is_structural()` and the payload is an
      `ESMF_Field`, via `rebuild_field_extension` (shares
      `build_field_payload` with `materialize_field_extension`: same
      name, fill value applied). Non-structural kinds and non-Field
      payloads behave as in 3.14, which closes 3.14's "payload not
      rebuilt" gap; direct successors are reset, not rebuilt. Reported
      gap: a Field already inserted into an `ESMF_State` or aliased by
      importers is not replaced there, so a production caller (History
      samplers) must re-insert the new field. The import-side gap stays
      as in 3.14 (plan C3 item 4). NAG 69/69, gfortran 69/69.
- [x] 3.41a B3-FieldPerNode (landed 0b62e575d; checkbox ticked by the
      owner's decision 2026-11-10) (owner, 2026-11-07, answer c; after 3.33;
      first half of the old 3.41). Legacy creates every declared item at
      advertise (`advertise_variable` -> `create()`: empty Field,
      `allocation_status=CREATED`), activates an INTERNAL always and an
      EXPORT only when a connection or re-export chain needs it, allocates
      only active items (`allocate_items`), and `add_to_states(mode='user')`
      places the primary spec of every family, active or not (an
      unconnected export is an empty created Field in the user export
      state). The graph has no counterpart: a node gets a Field only when
      a connection needs one, built complete and ALLOCATED in one step
      (`build_field_payload`). Give the graph the same three statuses
      using the existing `allocation_status` values CREATED, ACTIVE,
      ALLOCATED: every declared node (all intents) gets a Field at
      advertise, complete from its descriptor (geometry, units, fill
      value, per-alias keys) but not allocated; an INTERNAL is activated
      and allocated always; an EXPORT is allocated only when a connection
      consumes it (owner, 2026-11-07: an unconnected re-export is NOT
      allocated; legacy allocates it because its re-export chain activates
      the source, a legacy bug; follow the design and add a row to the
      legacy/graph differences table in scenario-parity.md), so
      materialization completes the existing Field instead of creating a
      second one. An unconnected export, re-exported or not, stays created
      (GRIDSET once its geometry is written). Split `build_field_payload`
      so completion and allocation can happen separately. Then add
      INTERNAL to the
      declared-item walk in `graphbuilder_populate_state` so the user
      INTERNAL state receives the node's Field under the item's name (the
      gridcomp writes this Field). Preflight points: the tmp_states
      scratch export in `initialize_accept_transfer.F90` (the component's
      own export walk lands in a throwaway state; the real user states
      are `this%user_gc_driver%get_states()`); freeze and readers of a
      payload must tolerate a created-not-allocated Field; hybrid mode
      keeps legacy's own Field in the user states (graph writes by
      `StateAddReplace` and is a no-op in BOTH mode).
      Owner answer (2026-11-07, option c): an explicit switch, off by
      default like `set_materialize_extensions`, "graph owns the user
      states". Off: production behavior unchanged (legacy fills the
      states; the graph creates, completes and aliases Fields). On: the
      legacy user-state population (`add_to_states(mode='user')` at
      advertise and at REALIZE_ACCEPTED) is skipped and the graph
      populates the real user import, internal and export states, the
      outer states remaining legacy's until 3.42. Scenarios that cannot
      pass with the switch on (`plain_field`, `dims_z_field`, ExtData and
      history ones) are measured and listed as the pending worklist before
      graph-only can be the default; they are not fixed here. Done when a graph
      population unit test (GRAPH mode, no `registry%add_to_states`)
      finds the Field for an internal, an unconnected export (created,
      not allocated) and a connected export in the right user states.
      Author sonnet (opus review).
      Status (in progress, item stays open): Feasibility holds: one Field
      object goes empty-create (`create_field_extension`,
      `allocation_status=CREATED`, ESMF EMPTY), then `sync_to_info`
      STRUCTURAL puts the geom on it (ESMF GRIDSET, what legacy shows for
      an unconnected export or import; `describe_created_field`, at
      population), then `MAPL_FieldEmptyComplete` + fill + ALLOCATED
      (`complete_field`). Built: `advertise_one` creates the Field for
      INTERNAL, EXPORT and IMPORT nodes of FIELD class when graph mode is
      not REGISTRY (skipped: composites, bundles, vectors, brackets,
      expressions, services, state items, and names with a slash, which
      are not valid ESMF Field names); `materialize_field_extension`
      completes a CREATED Field in place and is a no-op for any other
      Field; `rebuild_field_extension` is a no-op on a created Field.
      "Created" means status CREATED and an original, not a named alias
      (alias id 0): legacy writes the same status key on its Fields, and
      the only legacy Field a node holds is an alias of one, so a legacy
      Field still marked CREATED is not mistaken for a graph-created one
      (tested). The switch `set_graph_owns_user_states` (test-only setter),
      read as `graph_populates_user_states()` (switch and GRAPH mode) at
      every gate (`graph/GraphOwnsUserStates.F90`;
      `MAPL_SCENARIO_GRAPH_OWNS_USER_STATES` for Test_Scenarios). OFF:
      legacy fills the user states; populate inserts only completed
      Fields, skips INTERNAL, and the call site passes the HEAD scratch
      states; `try_alias_export_field` still aliases legacy's Field and
      destroys the orphaned created one (not if it shares a host). ON:
      legacy user population is skipped (`initialize_advertise`,
      `initialize_realize_accepted`; accept_transfer already was), the
      call site passes the real user states, populate completes and
      allocates INTERNAL and describes created exports, an import with a
      producer gets the producer's Field (its own created Field stays on
      its node, created, in no state) and one without gets its own
      (GRIDSET), `allocate_internals` at REALIZE_ACCEPTED allocates any
      INTERNAL whose descriptor is complete by then (an incomplete one
      stays created), and `try_alias_export_field` finds nothing to alias
      so the created Field is completed in place. An unconnected export,
      re-exported or not, stays created (owner Q2).
      `find_materialized_predecessor` walks past the import's own created
      Field, and returns not found at any created-only Field further
      along (the connection did not complete it; nothing there to walk
      for). `fill_deferred_from_handle` reads the component's own field
      when the payload Field has no geom yet. Tests: updated
      `Test_GraphGeometryHook.test_harvest_fills_deferred_from_realized_field`;
      new `test_populate_state_field_per_node_graph_mode` (ON, fresh
      states, two passes, unconnected import),
      `..._registry_unchanged`,
      `test_populate_state_switch_off_keeps_legacy_fields` (states
      pre-filled with empty Fields, E_conn ACTIVE, E_unconn CREATED, Z
      ACTIVE), `test_legacy_field_marked_created_is_not_graph_created`,
      `test_connected_export_not_aliased_when_graph_owns_states`,
      `test_import_not_fed_a_created_only_export_field`,
      `test_import_with_producer_gets_producer_field`,
      `test_allocate_internals_hook_completes_only_complete`,
      `test_create_then_materialize_keeps_one_field`.
      Pending worklist, measured with the switch ON in GRAPH mode on NAG
      (not fixed here). Graph-ready scenarios failing: `plain_field`,
      `units_mismatch_run_phase`, `units_and_typekind_mismatch`,
      `invalidate` ([value]: the gridcomp writes its export into the user
      Field while the outer state holds legacy's; 3.42);
      `graphbuilder_propagation` (the import in a grandchild: populate
      reaches direct children only, so it gets its own empty Field);
      `dims_z_field` (completing the export from its descriptor fails:
      `drop_unsupplied` dropped the vertical grid at GEOM_B, so the Field
      has no levels; separate finding); `export_dependency` (E2 stays
      GRIDSET: legacy allocates it through the export dependency);
      `history_wildcard`, `extdata_1` (ExtData and history read exports by
      propagated or wildcard name; 3.41b/3.42); `expression`,
      `expression_match` (import standard_name `<unknown>`);
      `scenario_1`, `scenario_2`, `scenario_reexport_twice` (the
      re-exported internal ZZ_A1 is not in the user export state; now
      passing with the switch ON, fixed by 3.41b's port walk). Now
      passing: `names_1`. Pending scenarios with the switch
      ON: `vertical_regridding_3` ABORTS the run (ConfigurableGridComp.F90:82
      dereferences the pointer of a created, unallocated export);
      `vertical_regridding_2` now FAILS (describing the import's own
      created Field runs sync_to_info, and its VerticalOrientation is
      SPECIFIED with no direction: StateItemDescriptor.F90:726); `regrid`,
      `regrid_r8`, `expression_defer_geom` (status gridset where complete
      is expected for an export or import fed by a regrid or deferred
      chain), `propagate_geom` (re-exported internal not in the state; now
      passing with the switch ON, fixed by 3.41b) and `vector_1` (bundle
      `uv` not in the state), `history_1`, `statistics` (rc 41
      reading propagated exports); `expression_per_consumer_r8`,
      `service_service`, `service_with_options`, `statistics_real` fail as
      with the switch off; `vertical_alignment_regrid`,
      `vertical_alignment_with_grid`, `vertical_regridding` pass.
      Everything else graph-ready passes. Row added to scenario-parity.md:
      unconnected re-exported export.
- [x] 3.41b B3-ExportPortPopulation (owner, 2026-11-06; after 3.41a;
      before E1 and any graph-only default). Second half of the old
      3.41. Make `graphbuilder_populate_state` walk the component's
      export-port table too: for each port, the Field of the port's
      bound node (3.41a guarantees it exists) is added to the real user
      export state under the PORT's name, including a port bound to an
      INTERNAL-intent node (a re-exported internal), a renamed re-export
      (`Eparent_B1`) and a propagated name (`child_B/E_B1`, inserted
      through the substate-aware path `FieldClassAspect%add_to_state`
      uses). Per-alias keys (long_name, restart mode) come from the bound
      node's descriptor via `sync_alias_info`. Deduplicate against the
      declared-item walk (every declared export also has a port). Done
      when `scenario_1`, `scenario_2` and `scenario_reexport_twice` get
      those items from the graph (a unit test that fails if the legacy
      path were absent: assert on graph-populated state, not on the
      scenario run alone). Author sonnet (opus review).
      Status: done. `graphbuilder_populate_state` walks the export-port
      table after the declared-item walk, only when
      `graph_populates_user_states()` (switch on, GRAPH mode); with the
      switch off or in BOTH mode the walk adds nothing and legacy
      `add_to_states` stays the producer. For each port the bound node's
      Field is aliased into the user export state under the port name
      (`populate_export_ports`, `populate_export_port` in
      GraphBuilder_StatePopulation_smod.F90). A name with a slash goes
      into a substate made by `get_substate`, as
      `FieldClassAspect%add_to_state` does; `sync_alias_info` copies the
      bound node's long_name and restart mode onto the alias. A port bound
      to an INTERNAL node is completed and allocated; a port bound to an
      export is only described, so an unconnected re-exported export stays
      CREATED (GRIDSET). A port is skipped as a duplicate only when the
      declared export of the same name resolves to the same node; a
      same-named re-export of a child's export is a different node and is
      added. Non-Field nodes are skipped. Tests (in
      Test_GraphBuilderMaterializeGate.pf):
      `test_populate_export_ports_graph_mode` (no `add_to_states`,
      populate twice: a renamed re-export of an INTERNAL is the internal's
      ALLOCATED Field with its long_name; a renamed re-export of an
      unconnected export is the producer's Field, GRIDSET and not
      allocated; propagated `child_src/E_conn` is in a substate, aliased
      to the producer's Field; item counts equal after the second pass;
      fails with the port walk off. Since 3.42 a propagated name is
      written only to the outer export state, not to a user-export
      substate) and
      `test_populate_export_ports_off_in_hybrid_and_both` (switch off, and
      BOTH mode with the switch on: the export state stays empty; fails if
      the walk ignores the switch). Scenarios with the switch ON (NAG,
      one at a time): `scenario_1`, `scenario_2`,
      `scenario_reexport_twice` and `propagate_geom` pass all five
      checks.
      `history_1`, `statistics`, `history_wildcard` and `extdata_1` still
      fail with the switch ON and pass with it OFF (`history_1` and
      `statistics` re-run in GRAPH and BOTH mode with the switch off:
      pass); the port walk did not change them. (a) `statistics`: status
      41 from GENERIC_INIT_ADVERTISE. `ProtoStatGridComp.F90:63`
      (init_modify_advertised) reads export `avg_T` from the user export
      state; with the switch on, legacy `add_to_states` at advertise
      (`initialize_advertise.F90:116`) is skipped and the graph populates
      only at accept_transfer, so the item is not there yet. Follow-up
      3.44. (b) `history_1`: fails [item type] (and other checks) for the
      collection's imports `A/E_A1`, `B/E_B2`, `B/E_B3`. Suspected cause,
      from reading the code only: those declared names contain a slash,
      and advertise creates no Field for a declared name with a slash
      (`GraphBuilder_Advertising_smod.F90:~116`), so population has
      nothing to insert. Not verified by a run; part of 3.44's
      preflight. (c) `history_wildcard` and `extdata_1` fail [item type]
      (`Test_Scenarios.pf` ~961/988; `ProtoExtDataGC.F90:220`); their
      readers take propagated or wildcard names; 3.44 or 3.42 to decide.
      Since 3.42 a propagated name is written only to the outer export
      state.
      Dedup: `test_populate_export_ports_graph_mode` declares a parent
      export `PE` and asserts it is in the state once and is its own
      Field. Adding it twice would still give the same count (the add is
      a replace), so the guard is mostly defensive: it avoids a second
      insertion and a second descriptor sync. `sync_alias_info` is
      pinned: the test sets the internal's own Field restart mode and
      long_name to values other than the declared ones, runs a third
      pass, and fails with the `sync_alias_info` call removed. Known
      limitation: a port name that collides with an existing non-port item
      replaces it silently; `reexport_one` guards the common case, and the
      port walk adds no duplicate-name assertion. Pending, not here:
      bundles and composites
      (`vector_1`), populate depth to grandchildren
      (`graphbuilder_propagation`), outer states (3.42). Production
      default stays hybrid. NAG and gfortran ESSENTIAL 69/69.
- [x] 3.42 B3-OuterStatePopulation (owner, 2026-11-07, low priority;
      after 3.41b, before any graph-only default). Legacy
      `add_to_states(mode='outer')` fills the ESMF import/export states
      the parent sees; the graph never writes them. The owner expects
      the outer states are not user-visible and exist mostly so the
      component behaves as an ESMF gridcomp, so this may reduce to a
      small task. Preflight first (what reads the outer states: the
      parent's coupler, extension fields, `<root>` expectations).
      Carried over from 3.41a (switch ON): the user export state and the
      outer export state must hold the same Field (`plain_field`), and
      each INTERNAL is allocated twice, once by the graph and once by
      legacy's `registry%allocate`, so restart or history readers that go
      through the registry see legacy's Field; both must be closed here.
      Preflight done; owner decisions (2026-11-07), one task (not split):
      (1) the graph writes the outer states by reusing `populate_one` and
      `populate_export_ports` on the outer MultiState (a third populate
      call at `initialize_realize_accepted.F90` (owner, 2026-11-09: not
      accept_transfer, where a child's fill runs before the parent's
      connect step), gated by `graph_populates_user_states()`, replacing
      the legacy outer `add_to_states` there when the switch is on;
      INTERNAL items and the child walk skipped), so the user and outer
      aliases share one Field (`plain_field`; test with
      `mapl_FieldsAreAliased`); (2) a propagated child export (a port
      name with a slash) leaves the USER export pass of the 3.41b port
      walk and goes only to the OUTER export state, as legacy does
      (`StateRegistry_Actions_smod.F90:73`); this corrects 3.41b and
      changes the scenario-parity difference row and the ARCHITECTURE
      limitation; (3) the position-labelled names (`T(1)`, `T(2)`,
      `I_A1(1)`) are NOT written by the graph: add a `legacy_only` marker
      to those expectation items (about 63 entries in 19 files) that
      `Test_Scenarios` skips when the graph owns the states; (4) the
      INTERNAL double allocation is closed by (1): no graph-side reader
      sees the registry's Field, which stays an unread shadow until the
      legacy spec path is retired. Flips `plain_field`; `units_*` and
      `invalidate` need the marker.
      Status: done. `graphbuilder_populate_outer_state` (and the hook
      `run_populate_outer_hook`) fills the outer MultiState at
      `initialize_realize_accepted.F90`, replacing the legacy
      `add_to_states(mode='outer')` there when
      `graph_populates_user_states()`: the declared IMPORT and EXPORT
      items through `populate_one` (own_only false, fallback allowed), then
      `populate_export_ports` with `include_propagated=.true.`; INTERNAL
      items and descendants are skipped; each component fills its own
      outer states at its own REALIZE_ACCEPTED. `populate_export_ports`
      has the new argument `include_propagated`; every user pass
      (advertise own-only and accept_transfer) passes false and skips a
      port whose name has a slash, after the declared-id check. The
      outer and user aliases share one Field. Unchanged: switch off and
      BOTH mode (legacy fills the outer states); no change to legacy
      advertise or allocate, so the registry's INTERNAL Field stays an
      unread shadow. No reader of the outer states in production code
      (only `Test_Scenarios.get_substates`). Marker: 58 live `(n)`
      expectation entries in 18 files carry `legacy_only: true`;
      `Scenario%graph_owns_states` records the switch as setup applied it,
      and `check_items_in_state` skips such an item when it is true. Tests
      (`Test_GraphBuilderMaterializeGate.pf`): new
      `test_populate_outer_state_shares_user_field`,
      `_propagated_only_in_outer`, `_skips_internal_and_children`,
      `_off_in_hybrid_and_both`, `_idempotent`,
      `test_populate_state_own_only_skips_propagated`; changed
      `test_populate_export_ports_graph_mode` (the propagated
      `child_src/E_conn` is now absent from the user export state, 3 items).
      `Test_GraphModeDemonstration.pf`:
      `test_switch_on_outer_states_share_user_fields` (phases 1-8 of
      `plain_field`: outer export `T` of comp_src, outer import `T` of
      comp_dst and root export `comp_src/T` alias the user Fields). Mutation
      checks (NAG): slash filter dropped fails `test_populate_export_ports_graph_mode`,
      `test_populate_state_own_only_skips_propagated` and
      `..._propagated_only_in_outer`; outer populate a no-op fails
      `..._shares_user_field`, `..._propagated_only_in_outer`,
      `..._skips_internal_and_children` and `..._idempotent`; the
      switch gate removed fails `..._off_in_hybrid_and_both`; INTERNAL
      included in the outer walk fails `..._skips_internal_and_children`;
      the legacy outer call restored fails
      `test_switch_on_outer_states_share_user_fields` and the [value]
      check of `plain_field`, `units_mismatch_run_phase` and `invalidate`.
      Scenarios measured with the switch ON, GRAPH mode, NAG: `plain_field`,
      `units_mismatch_run_phase`, `units_and_typekind_mismatch`,
      `invalidate`, `graphbuilder_propagation`, `scenario_1`, `scenario_2`,
      `scenario_reexport_twice`, `propagate_geom`, `names_1`, `statistics`
      all pass. `graph_owns_user_states` is now set in the table for
      `plain_field`, `units_mismatch_run_phase`,
      `units_and_typekind_mismatch`, `invalidate`,
      `graphbuilder_propagation` and `scenario_reexport_twice` (the
      slash-filter regression). Pending, not here: scenarios outside the
      list above with the switch on (3.41a worklist items other than the
      six flipped). Production default stays hybrid. NAG and gfortran
      ESSENTIAL 69/69.
- [x] 3.43 B3-VerticalOrientationSync (found by 3.41a, 2026-11-07).
      Creating Fields for imports exposed a disagreement:
      `get_incomplete_kinds()` reports the descriptor complete, but its
      VerticalOrientation is SPECIFIED with no direction, and
      `sync_to_info` then fails
      (`StateItemDescriptor.F90:~726` via
      `VerticalOrientationCharacteristic.F90:~138`). Seen as a
      `vertical_regridding_2` failure with the switch ON. Cause: a
      single-level `fixed_levels` grid has direction UNSPECIFIED
      (`FixedLevelsVerticalGrid.F90:76-95`), and with_grid resolves the
      orientation to SPECIFIED holding that direction. Owner decision
      (2026-11-07, option a): `orientation_write_info` skips an
      UNSPECIFIED direction and writes nothing, so the Field keeps
      with_grid, as legacy does (`FieldInfo.F90:350` reads an unwritten
      alignment as with_grid); an INVALID direction still fails; the
      characteristic stays complete. UNSPECIFIED against an explicit up or
      down still compares unequal (no vertical Transform); leave it until
      a scenario needs it. Done when a unit test that fails today (a
      single-level orientation write returns status 0; INVALID returns
      nonzero) passes, the module header comment says so, and
      `vertical_regridding_2` passes with the switch ON. Author sonnet
      (sonnet review).
      Status: done. `orientation_write_info` returns early for
      `VCOORD_DIRECTION_UNSPECIFIED` (after the phase and status
      checks); INVALID still fails with the existing message; the module
      header comment says so. Tests in
      `Test_VariableSpecCharacteristics.pf`:
      `test_unspecified_direction_is_written_as_nothing` (single-level
      grid, entry SPECIFIED with UNSPECIFIED, write_info status 0, Field
      still reads with_grid; red before the fix with the "has no up or
      down direction" failure), `test_invalid_direction_write_info_fails`
      (exact message, status /= 0) and
      `test_up_and_down_directions_are_written` (UP and DOWN read back).
      `vertical_regridding_2` passes with
      `MAPL_SCENARIO_GRAPH_OWNS_USER_STATES=1` (NAG, pending target,
      graph and both modes); the table flag is unchanged. No second
      failure was found. NAG and gfortran ESSENTIAL 69/69.
- [x] 3.44 B3-AdvertisePopulation (found by 3.41b review, 2026-11-07;
      before any graph-only default). With the switch on the graph must
      populate the user states at advertise, as legacy `add_to_states`
      does at advertise (`initialize_advertise.F90:116`), because user
      gridcomps read their own states in INIT_MODIFY_ADVERTISED
      (`statistics`: `ProtoStatGridComp.F90:63` reads export `avg_T`, so
      status 41 today). Fields exist at advertise since 3.41a. Preflight
      first: what must be complete at advertise (descriptors, geoms), and
      qualified names for `history_1` (declared names with a slash have
      no Field today). Author sonnet (sonnet review).
      Preflight done; owner decisions (2026-11-07): (1) at advertise the
      graph also writes the descriptive-phase keys (units, standard name,
      long name; not geometry or typekind) onto the Field, as legacy's
      `update_payload_from_aspects` does; the author tests that repeating
      it at accept_transfer is harmless; (2) Field items only: bundles,
      states and vectors stay with later work and are recorded as a known
      limit (`vector_1` stays pending). Design from the preflight: populate
      at the end of ADVERTISE, after export propagation
      (`initialize_advertise.F90:115-119`), in an own-only insert mode (no
      describe or complete, no child walk); `populate_one` becomes
      substate-aware, so a qualified import such as `A/E_A1` goes into
      substate `A` as legacy does; a declared name with a slash gets a
      Field at advertise, named by the part after the last slash (verify
      ESMF accepts it); switch off is unchanged. For `history_1`, first
      read the failing run's log for `populate_state` errors.
      Status: done. `initialize_advertise.F90` now calls
      `run_populate_state_hook(this, user_states, own_only=.true.)` at the
      end of ADVERTISE when the switch is on (legacy `add_to_states`
      otherwise). Hook style, not `_RC`: a failure is logged and advertise
      continues, like the other graph hooks; accept_transfer populates again.
      A swallowed failure resurfaces as status 41 in the user gridcomp that
      reads its own state in INIT_MODIFY_ADVERTISED; the log
      warning `GraphBuilder: populate_state step failed with rc=...` is the
      clue.
      `populate_state` and the hook gained an optional `own_only` (after
      `rc`, so existing callers are unchanged). Own-only: no child walk, no
      `prepare_created_field` (no describe, no complete, no allocation), and
      a no-op with the switch off. Instead each Field gets the descriptive
      keys through a new `SYNC_PHASE_ADVERTISED` (`write_advertised_keys` in
      `ExtensionMaterialization.F90`): the non-structural keys only (units,
      standard name, long name, restart mode, ...), never geom or typekind.
      `PhysicalUnitsCharacteristic%write_info` writes units in STRUCTURAL
      and ADVERTISED only; the existing DESCRIPTIVE phase still skips them
      (units on a completed Field enter FieldDelta). Repeating the writes at
      accept_transfer is harmless: tested by populating twice more after
      the advertise-time call (same export Field, same units, standard name
      and long name, same item counts). `populate_one` is substate-aware
      (helper `get_target_substate`, shared with `populate_export_port`): a
      qualified import `A/E_A1` is inserted as `E_A1` in substate `A`, for
      every intent. A declared name with a slash now gets a created Field at
      advertise, named by the part after the last slash (ESMF accepts it).
      Deviation, needed by `statistics`: the accept_transfer walk over
      children now recurses to every descendant (`populate_descendants`),
      each with the network of the component whose hook runs, because the
      dependency edge of a grandchild import such as `history/collection_1`
      `A/T` lives in the grandparent network. Below the direct children an
      import with no producer in that network gets no own-created fallback
      (an ancestor that does resolve it inserts the right Field; the
      fallback would replace it). Switch off is unchanged.
      The switch-off guard (`populate_state` returns early for own-only
      with the switch off) prevents the key write onto graph-created
      Fields, not an insertion: with it off, `populate_one` would write the
      declared units onto the node Fields and then return at the
      created-only check, inserting nothing.
      Tests (each mutation-checked): `test_populate_state_own_only_at_advertise`
      (before connect: internal created and not allocated, export created
      with units, standard name and long name readable and the same Field
      as its node, import its own created Field, `A/X` as `X` in substate
      `A`; after two accept_transfer populates the counts, the export Field
      and the keys are unchanged and the internal is completed; fails with
      the own-only populate or the key write off);
      `test_populate_state_own_only_off_changes_nothing` (nothing inserted
      and no units written on the node Fields; fails with the switch guard
      removed); the own-only call added to
      `test_populate_state_switch_off_keeps_legacy_fields`;
      `test_populate_state_reaches_grandchild_without_replacing` (three
      levels, switch on: the grandchild import `T`, whose edge is only in
      the top network, gets the producer Field, and `X`, resolved only in
      the middle network, keeps the lower Field; fails with the recursion
      removed and, separately, with the no-fallback rule removed);
      `test_switch_on_user_reads_own_export_at_modify_advertised` (runs the
      first three init phases of the `statistics` tree with the switch on,
      so the `initialize_advertise.F90` branch is wired; fails with the
      advertise-time call removed; lives in `Test_GraphModeDemonstration.pf`,
      part of the scenarios target, which is ESSENTIAL);
      `test_advertise_creates_field_for_slash_name` (fails with the slash
      skip back); `test_sync_advertised_writes_units` and
      `test_sync_advertised_skips_typekind`.
      Scenarios with the switch ON (NAG, one at a time): `statistics`
      passes (gfortran too); `scenario_1`, `scenario_2`,
      `scenario_reexport_twice` and `propagate_geom` still pass.
      `history_1` still fails two checks. [status] `B/E_B3` stays gridset:
      the log shows no extension provider for the VERTICAL_GRID mismatch,
      the known no-vertical-regrid limit. [value] `A/E_A1` reads 1 where
      100 is expected (m to cm). Suspicion S1 (an ancestor pass replacing a
      converted Field with the raw export) is ruled out by logging every
      pass for `A/E_A1`: the pass at `history` (direct child, network
      without the edge) finds no producer and inserts the import own
      created Field; the pass at the top (descendant, network with the
      edge) walks five hops and inserts an unnamed chain tip, complete,
      never the raw export Field. The tip holds 1.0 as R8 and the raw
      export 1.0 as R4, so the chain exists but its conversion has not
      run. That is a run-phase effect: the pull hook
      (`graphbuilder_run_pull_hook`) walks only the direct children of each
      component, so the grandchild `collection_1` import `A/E_A1` is never
      updated. An experiment (not kept) making the pull recursive made the
      [value] check pass and left only [status] `B/E_B3`. Not fixed here;
      follow-up for the pull hook (a candidate for its own task).
      (`units_mismatch_run_phase` fails on an export value,
      `comp_src::export::T`, a different class: 3.42.)
      `graphbuilder_propagation`: its 3.41b failure (populate reached direct
      children only, so a grandchild import got its own empty Field) is
      fixed by `populate_descendants`; it now fails a different check, the
      `comp_src` export `T` [value] (outer-state class, 3.42).
      `history_wildcard` still fails [item type] for the collection imports
      (wildcard members) and [status] for `root/A` exports; `extdata_1`
      still fails in the run phase (`ProtoExtDataGC.F90:220`, status 41);
      `vector_1` still fails [item type] (bundle). The other 3.41b worklist
      results are unchanged. Known limits: Field items only (bundles,
      states, vectors and composites are not populated at advertise or
      later); items a user gridcomp advertises itself with
      `mapl_GridCompAdvertiseVariable` are not in the graph, so not
      populated; legacy `user` mode skips propagated (`comp_name`) families
      and the own-only populate does not (3.41b, 3.42). Suspicion S2 (a
      Field handle cached at advertise going stale): the only caller of
      `rebuild_field_extension` is `set_shared_characteristic` (structural
      change), which no production code calls; and `rebuild_field_extension`
      returns at once for a Field that is only created, so a Field made at
      advertise is never replaced there. `materialize_field_extension`
      completes a created Field in place. No stale handle found.
      `graph_ready` flags are unchanged. NAG and gfortran ESSENTIAL 69/69.
- [x] 3.13 B3-LongName (A8; after 3.18). Default chain: declared, field
      dictionary, then StandardName; string mirror on imports. Per-alias
      `write_info`. Reuse `MAPL_STRING_MIRROR` (one constant, owner) the
      way B3-StringMirror did: `make_VariableSpec` strips it into a
      `long_name_mirror` flag so no legacy reader sees the string. Note a
      mirrored long_name must not fall back to the field dictionary or
      StandardName. Also add the YAML path test B3-StringMirror skipped:
      `parse_var_specs` reading `standard_name: "<mirror>"` (and
      `long_name: "<mirror>"`) sets the flag. Preflight answers (owner):
      - Shapes: FIELD and EXPRESSION (legacy passes `long_name` only to
        `FieldClassAspect` and `ExpressionClassAspect`). Amend the §4.3
        EXPRESSION row. Also fix §3.4/§4.3: a bundle's `long_name` is
        bundle-wide (id 0), not per-alias.
      - The StandardName fallback (A8's third step) applies to every
        intent except IMPORT (export, internal); legacy has no such
        fallback, a deliberate difference. An import with no declared
        long_name stays UNCHECKED and writes nothing (legacy: inherits the
        export's via `connect_to_export`; graph path: gap until B4 (4.2)).
      - No per-alias wiring here: 3.11 added `is_per_alias()` and the
        `populate_one` call, and sets LongName's flag.
      Done: `LongNameCharacteristic` (kind 15, FIELD and EXPRESSION only),
      factory 12, `long_name_mirror`, YAML test. Per-alias wiring landed
      with 3.11, which added `is_per_alias()` (true for LongName) and the
      `populate_one` call.
- [x] 3.14 B3-SetShared: retarget `set_shared_characteristic` to mutate
      the descriptor (§7.3); check and report whether import-side
      geometry changes (History samplers) are handled. Since B1 it
      installs only the ESMF handle and leaves the descriptor unchanged,
      so until this task it cannot change a characteristic.
      Preflight answers (owner, 2026-11-05): change the characteristic
      only; the ESMF field rebuild (§7.3's `sync_to_info`) is 3.40, after
      B4. The import-side gap (only downstream steps are reset, so the
      regrid step feeding a changed import keeps its weights) is reported
      in the Status and in plan C3, not pinned by a test. Outsourced to a
      volunteer through GitHub (plan "Working with the owner"): Status
      comes back in the PR. Author volunteer (meta review, sonnet).
      Done (PR #5484, Purnendu Chakraborty): `set_shared_characteristic`
      takes `(graph, node_id, kind, characteristic, rc)` and sets that
      descriptor entry; the ESMF payload is untouched (3.40). Steps 2 and
      3 (eager reset of direct successors, revision bump) unchanged. The
      import-side gap is stated in the module header and plan C3 item 4.
      No production caller yet. NAG 69/69, gfortran 69/69 (meta, local);
      on ifx 37/72 ESSENTIAL fail identically on the base (E1-early).

## 4. Dispatch removal (B4)

Split (owner, 2026-11-06): B4a-TipReplacement = 4.1; B4b-DescriptorMaterialize
= 4.2, test-first (it changes behavior and carries more than three rules),
opus implementer and review; B4c-D6Grep = 4.4, after B4b. 3.34 runs after
B4b as its own task.

- [x] 4.1 Replace `merged_item_for_link` with generic tip replacement
      (§4.1). `build_transform` gets read-only tip and goal descriptors
      and returns the kinds its step covers (default: its own); the tip
      replacement replaces all of them (A16).
      Status: `merged_item_for_link` deleted. Covered kinds are a
      separate non-deferred `StateItemCharacteristic%covered_kinds(tip,
      goal)` (tip/goal are `StateItemCharacteristicMap`; default = own
      kind), so no `build_transform` override changed. `build_chain`
      builds a tip map per step, replaces all covered kinds with the
      goal's, skips later mismatched kinds already covered, and each
      extension node's descriptor is the tip after its step.
      `intermediate_node_ids` is one entry per step.
      `materialize_extension_chain` reads units/typekind from each
      link's own descriptor and lost its `mismatched_kinds` argument.
- [x] 4.2 Materialize extension fields by empty-create +
      `sync_to_info(STRUCTURAL)` + `MAPL_FieldEmptyComplete` (§3.2.1);
      delete `get_item_units` / `get_item_typekind` /
      `get_item_ungridded_dims` / `get_item_geom` / `get_item_vgrid` /
      `get_item_vertical_stagger`, move `harvest_stagger` and
      `harvest_quantity_type` (its mirror/UNKNOWN "not supplied" rule is
      QuantityType knowledge) out of `GraphBuilder_Geometry_smod.F90`
      behind the characteristic, and the
      `insert_geom_id_characteristic` import in
      `GraphBuilder_Geometry_smod.F90` (these name concrete
      `*Characteristic` types in `select type`, so 4.4 depends on this).
      Precondition (B3-HorizontalDimsSpec follow-up): harvest skips a node
      with no DEFERRED kinds, so a `dims: z` (NONE) export in a component
      with no default geom keeps an UNCHECKED Geometry with no geom, and a
      descriptor-built field for it would have none. Make harvest also
      visit a node whose UNCHECKED Geometry holds no geom (a characteristic
      query, not a GraphBuilder kind check), and add a
      `Test_GraphGeometryHook` case: such an export gets its geom from the
      realized field.
      Also (owner, after B3-RestartMode; the owner is not certain this is
      the right behavior: it is about user convenience, not right or
      wrong): an import that declares no long_name must read the
      export's, as legacy `FieldClassAspect%connect_to_export`
      (`mirror_name`) gives. Since B3-RestartMode it reads `unknown` on
      the graph path. Cover a direct match (no extension field: the
      export's long_name must be on the owning field that the import's
      alias copies) and an import behind an extension chain (the
      extension field), with a populate-level test for each. Revisit if
      the shared-data item (plan, Later) is adopted.
      Never run a full `sync_to_info` on an import's alias: an import's
      default SPECIFIED values (CONSERVE_NONE, NORMALIZE_NONE,
      QUANTITY_UNKNOWN, ...) would overwrite the export's field-wide keys.
      Imports write per-alias keys only (design §3.2, §3.5).
      Status: `materialize_field_extension(graph, node_id [, name], rc)`
      empty-creates, writes `horizontal_dims_spec=GEOM` as the creation
      default (an expression result declares none; kept there rather than
      on the result descriptor, see design §3.2), runs the node's own
      `sync_to_info(STRUCTURAL)`, `MAPL_FieldEmptyComplete`, then
      `allocation_status=ALLOCATED` (as `FieldCreate`). Geom and
      vgrid come only from the descriptor; the owner-geom/vgrid fallback
      (`GraphBuilder_PortsAndProxies_smod.F90`) is deleted. The
      `unresolved_geom` check is the generic
      `StateItemDescriptor%get_incomplete_kinds()` (new
      `StateItemCharacteristic%is_complete`, Geometry overrides), reported
      as `incomplete_characteristic:<kind>`; harvest also visits a node with
      incomplete kinds. `harvest_stagger`/`harvest_quantity_type` are
      `VerticalStagger`/`QuantityType` overrides of the new
      `StateItemCharacteristic%harvest(handle, defaults)` (no-op default),
      called through `StateItemDescriptor%harvest`. The geometry-proxy
      identity goes through `GeomProxyField%add_geom_proxy_identity`. The
      owning field's full sync writes long_name/restart_mode under alias 0,
      which the import alias copies, so the long_name fallback needs no
      populate change. `get_item_expression` stays for 4.4.
      Getter fix: `MAPL_FieldGet(vert_staggerloc=, num_levels=)` left the
      stagger unset (`FieldInfo.F90`). A unit-less expression input link
      now carries `<unknown>` units, as legacy `UnitsAspect` writes for
      UNCHECKED units. Owner exception: tests under
      `superstructure/generic/tests/` may read ESMF Info directly
      (read-only); production stays facade-only.
- [x] 4.3 Delete `GraphStateItem%ordering` and
      `characteristic_ordering_table`; replace their tests with golden
      `get_extension_order` tests per shape. Done in B3-ItemShape.
- [x] 4.4 Done when the D6 grep (`_CHARACTERISTIC_KIND` and concrete
      `*Characteristic` names in `GraphBuilder*.F90`, outside comments)
      finds nothing, and ESSENTIAL passes on NAG and gfortran.
      Also: route `get_item_expression` generically, and move the
      `horizontal_dims_spec=GEOM` default out of
      `ExtensionMaterialization` into graph-layer expression-result
      descriptor construction (a SPECIFIED GEOM HorizontalDimsSpec;
      not GraphBuilder), then drop the design.md §3.2 "As landed" note.
      Status: done (B4c). `StateItemDescriptor%get_expression` (fails
      when the entry is absent, as before; both callers still treat an
      empty string as nothing to do) replaces `get_item_expression`.
      `StateItemDescriptor%add_expression_result_defaults`, called
      generically by
      `build_expression_evaluation`, gives the result a SPECIFIED GEOM
      HorizontalDimsSpec when it has none; `ExtensionMaterialization`
      writes no default. The D6 grep shows only base-class names
      (`StateItemCharacteristic` at ConnectionResolution:1671, 1848,
      1890; GraphBuilder.F90:65, 366). NAG and gfortran ESSENTIAL 69/69.

## 5. VariableSpec translation (B5)

- [x] 5.1 Add `VariableSpec%make_characteristics` in a new `specs/`
      submodule as a loop over the factory list (§5). (Done in B3-Units;
      later B3 tasks add factories to the list.)
- [x] 5.2 Reduce `populate_characteristics` to one call; then delete it.
      (Done in B5-TranslationInPlace: declared item type and fill value
      moved into `make_characteristics`; advertise calls it directly.)
- [x] 5.3 Done when ESSENTIAL passes on NAG and gfortran. (69/69 both.)
