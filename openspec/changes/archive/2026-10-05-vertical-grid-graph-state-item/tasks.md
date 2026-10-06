## 1. VariableSpec variant tag + composite materialization

- [x] 1.1 Add `state_item_variant : type(MAPL_StateItem_Flag), allocatable`
      to `VariableSpec` (`superstructure/generic/specs/VariableSpec.F90`),
      unallocated by default, set via plain field assignment only
      (mirroring `callback_interface_id`'s own precedent, design.md D2) -
      no new `make_VariableSpec` keyword.
- [x] 1.2 In `mapl_CompositeStateMaterialization_mod%materialize_composite`
      (`superstructure/generic/graph/CompositeStateMaterialization.F90`),
      after `ESMF_StateCreate` and before `payload%set(state, rc)`, call
      the existing `set_variant(state, var_spec%state_item_variant, rc)`
      when `var_spec%state_item_variant` is allocated (design.md D2).
      Leave non-vertical-grid composites (`state_item_variant`
      unallocated) behaviorally unchanged.
- [x] 1.3 Add unit tests (`superstructure/generic/graph/tests/
      Test_CompositeStateMaterialization.pf` or equivalent existing test
      file for this module): a composite `VariableSpec` with
      `state_item_variant = MAPL_STATEITEM_VERTICALGRID` and declared
      members (one per physical dimension) materializes a `StateItemNode`
      whose `GraphStateItem%variant()` reports `MAPL_STATEITEM_VERTICALGRID`
      and whose `state_members()` map contains exactly the declared
      dimension names, each resolving to its own independently
      node-identified leaf item; a composite with `state_item_variant`
      left unallocated continues to materialize as plain `STATE`
      (regression check).

## 2. REQ-GEO-007a dimension-overlap classification (unified into `VerticalGridCharacteristic`)

**Revised after implementation and review** (design.md Context
"Unification"/D3): group 2 originally added a separate
`VerticalGridMembershipCharacteristic`/`VERTICAL_GRID_MEMBERSHIP_CHARACTERISTIC_ID`.
That was built, tested, and then reverted after review - judged
architecturally wrong (two "vertical grid" characteristic kinds is
confusing, and dimension-name-overlap matching with no identity-token
fallback at all was a real false-positive risk). The tasks below
describe the corrected, final shape: no new `CharacteristicId`, no new
module - the existing `VerticalGridCharacteristic` is extended.

- [x] 2.1 Add a new optional `dimensions : type(StringVector)` field to
      `VerticalGridCharacteristic`
      (`superstructure/generic/graph/VerticalGridCharacteristic.F90`),
      alongside its existing `grid_id` field (made optional in the
      constructor - it was already an allocatable component). No new
      `CharacteristicId`.
- [x] 2.2 Update `needs_extension_for`: when both sides have `grid_id`
      allocated, keep the existing exact-match comparison unchanged
      (ordinary Field-to-Field behavior). When either side lacks one
      (always true for a `MAPL_STATEITEM_VERTICALGRID`-tagged
      composite), fall back to dimension-set equality (design.md D3).
- [x] 2.3 Update `build_transform` to implement the REQ-GEO-007a
      three-way classification from `dimensions` on both sides,
      regardless of which comparison path detected the mismatch
      (design.md D3):
      - exactly one overlapping dimension -> `_FAIL`, message names that
        dimension as the identified (not-yet-implemented) adaptation
        candidate.
      - zero overlapping dimensions -> `_FAIL`, explicit "incompatible"
        message.
      - more than one overlapping dimension -> `_FAIL`, explicit
        "ambiguous" message naming the overlapping dimensions.
      No real transform is allocated in any case.
- [x] 2.4 Update `get_signature()` to append the sorted, joined
      dimension list when `dimensions` is non-empty, so two
      characteristics differing only in dimensions get distinct
      `ExtensionResolution.F90` chain-reuse cache keys.
- [x] 2.5 Extend `build_characteristics`'s existing ordinary-Field
      `vertical_grid` branch (`GraphBuilder.F90`) to also supply
      `dimensions = var_spec%vertical_grid%get_supported_physical_dimensions()`
      (confirmed a pure accessor, no `StateRegistry` involvement) - so
      the improved three-way diagnostic applies uniformly to ordinary
      Field-to-Field vertical-grid mismatches too, not only to the new
      composite path.
- [x] 2.6 Update unit tests for `VerticalGridCharacteristic`
      (`superstructure/generic/graph/tests/Test_Characteristic.pf` and/or
      a dedicated file): identical dimension sets (no identity token)
      report no extension needed; differing sets with exactly one
      overlap, zero overlap, and more than one overlap each report
      extension needed, and `build_transform` fails with a
      distinguishable message for each of the three overlap cases;
      existing identity-token-based tests (`grid_a`/`grid_b`) continue
      to pass unchanged.
- [x] 2.7 Update the one pre-existing test that hardcoded the old flat
      failure message
      (`test_materialize_extensions_vgrid_mismatch_unaffected`,
      `superstructure/generic/tests/Test_GraphBuilder.pf`) to the new
      REQ-GEO-007a-classified message (its `mapl_BasicVerticalGrid`
      fixtures both report a single, identical `"<unknown>"` physical
      dimension - the "exactly one overlap" case).

## 3. Wiring into ordinary connection resolution

- [x] 3.1 In `GraphBuilder.F90`'s `build_characteristics`, add a new
      branch: when `allocated(var_spec%state_item_variant)` and it
      equals `MAPL_STATEITEM_VERTICALGRID`, insert a
      `VerticalGridCharacteristic` built with `dimensions` only (no
      `grid_id`) from `var_spec%get_member_names()`, keyed by the
      **same** `VERTICAL_GRID_CHARACTERISTIC_ID` the ordinary-Field
      branch already uses. No change to the existing `units` branch.
- [x] 3.2 Add an integration test exercising the *existing*, unmodified
      `resolve_match_connection`/`resolve_one` path
      (`superstructure/generic/tests/Test_GraphBuilder.pf` or a new
      focused test file) with two components: an export and an import
      each declaring a `MAPL_STATEITEM_VERTICALGRID`-tagged composite
      under the same `short_name`. Cover: identical dimension sets wire
      a direct dependency edge (no extension chain); differing sets with
      one/zero/multiple overlapping dimensions each delegate to
      `find_or_build_extension_chain` and fail explicitly and
      distinguishably (REQ-EXT-001/003/005 dispatch shape, unmodified).

## 4. Geometry-association test (no new production code)

- [x] 4.1 Add a test confirming the already-true structural fact
      (design.md D4): a component with both a Phase-4e horizontal
      geometry item and a `MAPL_STATEITEM_VERTICALGRID`-tagged composite
      has both reachable from the same `ComponentGraph` - specifically,
      each coordinate-set member's `NodeId` and the result of
      `graph%get_resource_index(item_key(ESMF_STATEINTENT_EXPORT,
      GEOMETRY_ITEM_NAME))` belong to the same graph. No new
      reference-characteristic module is added - this
      task is verification only.

## 5. Documentation and roadmap bookkeeping

- [x] 5.1 Update `docs/graph/spec/13-geometry-and-vertical-grids.md`'s
      status line with an "Implementation status (Phase 4f, landed)"
      note: REQ-GEO-004/004a/009 satisfied via ordinary composite
      declaration (`state_item_variant` tagging), REQ-GEO-007a satisfied
      via the existing `VerticalGridCharacteristic`'s three-way
      classification (unified, not a separate characteristic kind - see
      design.md Context "Unification"), the geometry-association
      requirement satisfied via shared `ComponentGraph` ownership (not a
      new per-item reference - REQ-GEO-007's original
      "ReferenceCharacteristic" wording is not implemented as such),
      REQ-GEO-005's reserved-state convention and REQ-GEO-007b remaining
      open/deferred, and the mid-implementation pivot away from
      `OuterMetaComponent%get_vertical_grid()`/`StateRegistry` (recorded
      so a future reader does not assume that path was used).
- [x] 5.2 Update `docs/graph/spec/20-implementation-roadmap.md` §20.4.3's
      4f entry: mark landed, document the real deviation from the
      original plan (composite declaration instead of a legacy-wrapping
      constructor/hook, and why - `StateRegistry` mutation risk),
      mirroring 4e's own entry style.
- [x] 5.3 Add a new entry to §20.4.2's Phase 6 growable list
      (`docs/graph/spec/20-implementation-roadmap.md`): consolidate
      `VariableSpec%itemType` onto a single graph-native
      `MAPL_StateItem_Flag`-typed field (subsuming this change's own
      `state_item_variant`), with `itemType` becoming a derived/truncated
      view rather than a separately-stored field - blocked until legacy
      `ClassAspect` dispatch (`make_ClassAspect`, `to_itemtype.F90`) no
      longer needs `itemType`'s `WILDCARD`/`SERVICE`/`EXPRESSION` values,
      which graph's vocabulary does not yet cover. Raised during review
      of this change (see design.md discussion); mirrors the
      `UnitsConverterTransform` entry's own precedent (a real
      simplification discovered during/after a sub-change's own work,
      genuinely blocked on legacy retirement, not merely deferred by
      choice).
