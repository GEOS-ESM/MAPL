## Why

Roadmap Phase 4f (`docs/graph/spec/20-implementation-roadmap.md` §20.4.3)
is next: `VerticalGrid` is still only an opaque per-component attribute
(`OuterMetaComponent%get_vertical_grid()`) that ordinary connection
resolution reduces to a single integer identity token
(`VerticalGridCharacteristic`, matched via `VerticalGrid%get_id()` in
`GraphBuilder.F90`'s `build_characteristics`) — real enough to detect
"these two Fields were given completely different vertical grids," but
nothing today gives the vertical grid *itself* real graph structure.
`docs/graph/spec/13-geometry-and-vertical-grids.md` §13.3 (REQ-GEO-004/
004a/007a/009) requires `VerticalGrid` to become an ordinary,
graph-visible `GraphStateItem` — an `esmf_state`-kind item whose members
are the individual physical-dimension coordinate-set Fields — and
requires mismatched-vertical-grid connections to be resolved by an
explicit dimension-overlap check, not a single all-or-nothing identity
comparison. The supporting plumbing (`MAPL_STATEITEM_VERTICALGRID` in
`MAPL_StateItem_Flag`, `GraphStateItem`'s existing `esmf_state`
component plus its `state_members_map`, REQ-SI-006) already landed with
Phase 1–2 but is unused by any real code path; nothing today constructs
or advertises a `VerticalGrid`-variant `GraphStateItem`.

## What Changes

**Revised during implementation** (see design.md Context/D0): the
original plan assumed `VerticalGrid`-item construction would wrap
`OuterMetaComponent%get_vertical_grid()`/legacy `ModelVerticalGrid%
get_coordinate_field()`. Tracing that call confirmed it routes through
`StateRegistry%extend()`, which can mutate the registry (build a real
`ESMF_GridComp` coupler) as a side effect — unsafe, and explicitly
disallowed: **the graph solution must not use `StateRegistry`**; legacy's
aspect/extension machinery is at most suggestive of what's needed, never
something the graph code calls into. The revised plan below needs no
legacy `VerticalGrid` access at all.

**Revised a second time after review** (see design.md Context
"Unification"): the first implementation of the plan below added a
brand-new, independent `VerticalGridMembershipCharacteristic`/
`CharacteristicId`, living alongside the pre-existing
`VerticalGridCharacteristic`. On review this was judged architecturally
wrong — two separate "vertical grid" characteristics is confusing, and
matching by dimension-*name* overlap alone (with no identity check at
all) is a real false-positive risk (two unrelated grids that happen to
declare the same dimension names would wrongly "match"). The design
below describes the corrected, final shape: the existing
`VerticalGridCharacteristic` carries the membership information
directly — no second characteristic.

- Add `VariableSpec%state_item_variant` (allocatable `MAPL_StateItem_Flag`,
  mirroring the existing `callback_interface_id` field's "mark the item,
  not a new itemType, plain field assignment" precedent): a component
  marks a composite `VariableSpec` (declared via the already-shipped
  `declare_member`/`composite-state-spec` mechanism, one member per
  physical dimension) as a vertical grid. The already-shipped
  `materialize_composite` gains one line to tag the resulting
  `esmf_state` with this variant via the existing `set_variant` — giving
  REQ-GEO-004/004a/009 real graph structure (member name = physical
  dimension, `variant() == MAPL_STATEITEM_VERTICALGRID`) with no new
  constructor module and zero `StateRegistry` involvement.
- Extend the existing `VerticalGridCharacteristic` (not a new
  characteristic kind) with an optional declared physical-dimension set
  (`dimensions`, a `StringVector`), alongside its existing opaque
  identity token (`grid_id`, now itself optional — a composite has no
  legacy `VerticalGrid` object to pull one from):
  - `needs_extension_for`: when both sides carry an identity token, that
    remains the authoritative exact-match check (unchanged behavior for
    ordinary Fields). When either side lacks one — always true for a
    `MAPL_STATEITEM_VERTICALGRID`-tagged composite — dimension-*set
    equality* is the fallback comparison.
  - `build_transform`, on mismatch, implements REQ-GEO-007a's
    dimension-overlap classification (exactly one overlapping dimension
    identified / zero reported as incompatible / more than one reported
    as ambiguous) from `dimensions` on both sides, regardless of which
    comparison path detected the mismatch — still failing explicitly in
    all three cases, same "structure and detection now, execution
    later" posture already shipped for `units`/geometry.
  - Because `VerticalGrid%get_supported_physical_dimensions()` is a pure
    accessor (unlike `get_coordinate_field()`, no `StateRegistry`
    involvement), dimensions are populated for the ordinary Field path
    too — so the improved three-way diagnostic applies uniformly, not
    just to the new composite path. One pre-existing test asserting the
    old flat "no vertical-regrid Transform is implemented yet" message
    was updated accordingly (it now gets the more specific classified
    message).
  - Exercised automatically through the already-shipped `resolve_one`/
    `find_mismatched_characteristics`/`find_or_build_extension_chain`
    connection-resolution path — no new `GraphBuilder.F90` hook.
- The horizontal-geometry-reference requirement is satisfied by an
  already-true structural fact rather than a new mechanism: a
  coordinate-set item materialized this way is always registered into
  the same `ComponentGraph` as that component's own Phase-4e geometry
  item, so it is identifiable via the existing, already-public
  `get_resource_index` lookup — no new `ReferenceCharacteristic`/
  `GeomReferenceCharacteristic` module is added (see design.md D4 for why
  the originally-planned per-item characteristic isn't buildable without
  spec 18's un-built persisted-characteristics-map machinery).
- Explicit deferrals, carried forward from the roadmap and stated here so
  they are not silently assumed solved:
  - REQ-GEO-007b's general case (an import declaring *multiple*
    acceptable coordinate systems) — out of scope; only the "import
    declares a single required system" case is handled.
  - Real vertical-regrid execution — `build_transform` keeps failing
    explicitly. A later change may reuse legacy's low-level regrid
    numerics where separable from `StateRegistry`-coupled orchestration,
    not attempted here.
  - §13.4 (time-dependent geometry/vertical-grid renewal under freeze) —
    out of scope; only static, pre-freeze vertical-grid wiring is
    covered.
  - REQ-GEO-005's "MAY materialize" reserved `MAPL_VerticalGrids` nested
    state — not introduced; a component uses whatever `short_name` it
    chooses for its own vertical-grid composite.
  - The full `StateItemCharacteristic`/`ReferenceCharacteristic`
    hierarchy (`18-state-item-characteristics.md`) — explicitly Phase 5
    in the roadmap.

## Capabilities

### New Capabilities
- `graph/vertical-grid`: `VerticalGrid` as a first-class, graph-visible
  `GraphStateItem` — per-physical-dimension coordinate-set member
  structure, the REQ-GEO-007a dimension-adaptability mismatch check
  (via the existing `VerticalGridCharacteristic`), and the
  coordinate-set-to-geometry structural association, per REQ-GEO-004/
  004a/007a/009.

### Modified Capabilities
(none — `state-item`'s existing `esmf_state`/`state_members_map`
structure, REQ-SI-006's membership-map allowance, and
`extension-reuse`'s existing mismatch/chain-delegation machinery already
cover the structural and resolution behavior this change exercises; this
change is a new consumer of all three, not a modification of any of
their own requirements. `VerticalGridCharacteristic` gains a new
optional field and a dimension-overlap classification on its existing
mismatch path, but this is an implementation refinement of a
Characteristic that was never shipped with a real `build_transform` to
begin with — not a change to any requirement already recorded for it.)

## Impact

- New source: one new field on `VariableSpec`
  (`superstructure/generic/specs/VariableSpec.F90`,
  `state_item_variant`), one new step in
  `mapl_CompositeStateMaterialization_mod%materialize_composite`
  (`superstructure/generic/graph/CompositeStateMaterialization.F90`),
  and one new branch in `GraphBuilder.F90`'s existing
  `build_characteristics`. No new `CharacteristicId`, no new
  `Characteristic` module, no new constructor module, no new
  `GraphBuilder.F90` hook.
- `VerticalGridCharacteristic`
  (`superstructure/generic/graph/VerticalGridCharacteristic.F90`) gains a
  new optional `dimensions` field and a dimension-overlap-classifying
  `build_transform` — used both by the pre-existing ordinary-Field
  identity-token path (now also diagnosing REQ-GEO-007a's three-way
  outcome on mismatch) and by this change's own composite path (no
  identity token available, dimension-set equality only). This is the
  **same** `CharacteristicId`/`Characteristic`, not a new one.
- `GraphBuilder.F90`'s existing `build_characteristics`/`resolve_one`
  (ordinary `VariableSpec`-based connection resolution) is **not
  replaced** — this change adds one more `VerticalGridCharacteristic`
  construction path (no identity token, dimensions only), exercised
  through that already-shipped path unmodified.
- Zero use of `OuterMetaComponent%get_vertical_grid()`, legacy
  `mapl_VerticalGrid_mod`/`ModelVerticalGrid`, or `StateRegistry`
  anywhere in this change.
- No change to `GraphStateItem`'s structure (its `esmf_state` component
  and `state_members_map` already support this shape, per REQ-SI-006),
  to `MAPLStateItemFlag.F90`'s existing `MAPL_STATEITEM_VERTICALGRID`
  tag, or to `mapl_StateItemVariantInfo_mod`'s existing `set_variant`/
  `get_variant` (`ESMF_State` overload already exists).
- One pre-existing test (`test_materialize_extensions_vgrid_mismatch_unaffected`,
  `superstructure/generic/tests/Test_GraphBuilder.pf`) updated: its
  expected failure message changed from the old flat
  "no vertical-regrid Transform is implemented yet" to the new
  REQ-GEO-007a-classified message (both its `mapl_BasicVerticalGrid`
  fixtures report a single, identical `"<unknown>"` physical dimension,
  so this is the "exactly one overlap" case).
