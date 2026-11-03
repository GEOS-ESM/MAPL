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

Remaining order (owner): 3.29, 3.30, 3.32, 3.35, 3.37, 3.27, 3.36,
3.31, 3.12, 3.23, 3.14 (3.12/3.23: Q2, Q3 answered; 3.24 later; 3.27
before B4; 3.36 after 3.27, whose switch it shares, and after 3.37;
3.34 with or after 4.2; 3.33 after B4).
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
- [ ] 3.12 B3-Expression (new kind; mechanical half of the owner's split).
      Add the Expression characteristic and give the EXPRESSION factory
      applicability per design §4.3 (every FIELD kind except
      STANDARD_NAME, from the declaration; units by the Field rule, Q3).
      Evaluation is unchanged (still `run_expression_hook`). Also closes
      the ungridded-dims gap: B3-UngriddedDims gave EXPRESSION no
      UNGRIDDED_DIMS entry, so an EXPRESSION export silently matches a
      FIELD import that declares ungridded dims (owner accepted this
      until this task). The units rule for inputs (no conversion to the
      expression's units, Q3) is 3.23, not this task.
- [ ] 3.23 B3-ExpressionPerConsumer (after 3.12). Build the per-consumer
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
- [ ] 3.24 B3-ExpressionNativeGrid (optimization, after 3.23; owner
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
- [ ] 3.27 B3-StandardNameValidation (owner, after 3.22; decision may be
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
      (opus author; owner settles the design).
      From develop (#5443): a configurable option now suppresses the
      PERMISSIVE-mode standard_name warnings; the graph behavior must honor
      it too.
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
- [ ] 3.31 B3-OneExportPerImport (3.26 R8: owner chose (a); after 3.27,
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
- [ ] 3.33 B3-InternalReexport (3.26 R11: owner chose (a); after B4).
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
- [ ] 3.34 B3-FillValue (3.26 R14: owner chose (c); with or after B4's
      materialization, 4.2). fill_value is not a characteristic: when the
      graph materializes a field, it fills it from the owning
      `VariableSpec`'s `fill_value` (the export's, or the internal's); an
      import's declared fill_value has no effect, as in legacy data today
      (rules-review.md §0). Also fix the legacy log text in
      `FieldClassAspect%connect_to_export` (`FieldClassAspect.F90:345`,
      "(src value wins)" while the destination keeps its value) to say
      what happens; that one-line part may land earlier on its own. Also
      update design §1's `fill_value` row. Author sonnet.
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
- [ ] 3.36 B3-QuantityTypeDeferred (3.26 R2: owner chose (c), rollout (b)
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
      that the default mode changes no ESSENTIAL result. Update design
      §5.1's QuantityType row and A24 to "landed". Strict as the default
      is a later change, once real configs comply (owner). Owner may tweak
      the rules after concrete use cases. Author opus (sonnet review).
- [ ] 3.37 B3-QuantityBasisUnsupported (owner; amends A23 for basis only;
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
- [ ] 3.38 B3-GeomAItemWins (owner: legacy bug, item wins; after 3.37,
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
- [x] 4.3 Delete `GraphStateItem%ordering` and
      `characteristic_ordering_table`; replace their tests with golden
      `get_extension_order` tests per shape. Done in B3-ItemShape.
- [ ] 4.4 Done when the D6 grep (`_CHARACTERISTIC_KIND` and concrete
      `*Characteristic` names in `GraphBuilder*.F90`, outside comments)
      finds nothing, and ESSENTIAL passes on NAG and gfortran.

## 5. VariableSpec translation (B5)

- [x] 5.1 Add `VariableSpec%make_characteristics` in a new `specs/`
      submodule as a loop over the factory list (§5). (Done in B3-Units;
      later B3 tasks add factories to the list.)
- [ ] 5.2 Reduce `populate_characteristics` to one call; then delete it.
- [ ] 5.3 Done when ESSENTIAL passes on NAG and gfortran.
