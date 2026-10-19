## Why

`docs/graph/spec/20-implementation-roadmap.md` §20.4.12 (Phase 12) identifies
a gap left by `openspec/changes/archive/2026-10-08-state-item-characteristic-subclasses`
(5a2): that change landed seven additional `StateItemCharacteristic`
subclasses by its own explicit, deliberate scope — "standalone types
exercised by synthetic-node pFUnit coverage only," with no `GraphBuilder`
rewire. Today `GraphBuilder_Advertising_smod.F90`'s `populate_characteristics`
only ever inserts `PhysicalUnitsCharacteristic`/`TypeKindCharacteristic`/
`VerticalCoordinateCharacteristic` into a real `GraphStateItem`'s
characteristics map — a genuine production mismatch on any of the other
six axes (attributes, ungridded-dims, quantity-type, conservation,
normalization, standard-name) is invisible to `resolve_one`'s mismatch
detection and `ExtensionResolution.F90`'s chain-building, even though the
comparison logic for all six has existed, tested, since 5a2 landed.

**Correction to the roadmap's framing, confirmed by direct reading before
this proposal was drafted:** §20.4.12 describes this as "wire all seven"
remaining axes, but `VerticalCoordinateCharacteristic` (the vertical-grid
axis, kind value 4) is already wired — `populate_characteristics` inserts
it today (both the legacy-`VerticalGrid` branch and the
`MAPL_STATEITEM_VERTICALGRID`-composite branch), and
`GraphBuilder_ConnectionResolution_smod.F90`'s `merged_item_for_link`
already enumerates it explicitly as a no-op case. Only **six** axes are
genuinely unwired. This proposal scopes to those six and documents the
correction; it does not re-verify or re-wire vertical-grid.

A second gap, found while scoping how to wire the three axes with
mismatch-relevant data (quantity-type, conservation, normalization):
`VariableSpec` has no field a YAML declaration or `make_VariableSpec`
caller can use to set a non-default quantity-type, conservation, or
normalization value — every real construction path
(`make_QuantityTypeAspect`/`make_ConservationAspect`/
`make_NormalizationAspect`, `VariableSpec.F90:736-765`) unconditionally
builds the same default/mirror-inferred value regardless of input. Wiring
`populate_characteristics` to read "the declared value" for these three
axes is therefore impossible without first giving `VariableSpec` a real
field to read. This proposal adds those three fields (plain-field-
assignment only, mirroring the existing `callback_interface_id`/
`state_item_variant` precedent — no new `make_VariableSpec` keyword, no
YAML/parser wiring), so wiring for all six axes is genuine rather than a
permanently-default no-op.

## What Changes

- Add three new `VariableSpec` fields — `quantity_type`
  (`MAPL_QuantityType`, default `QUANTITY_UNKNOWN`), `conservation`
  (`ConservationMetadata`, default-constructed), `normalization`
  (`NormalizationMetadata`, default-constructed) — set via plain field
  assignment only, mirroring `callback_interface_id`'s own "mark the item,
  no `make_VariableSpec` keyword" precedent exactly. No
  `ComponentSpecParser`/YAML entry point is added for them (explicit
  deferral, same as `callback_interface_id`/`state_item_variant`).
- `GraphBuilder_Advertising_smod.F90`'s `populate_characteristics` gains
  six new insertions into the node's real `GraphStateItem%characteristics`
  map, mirroring the existing `PhysicalUnitsCharacteristic`/
  `TypeKindCharacteristic` insertion pattern exactly:
  - `AttributesCharacteristic` ← `var_spec%attributes` (gated on non-empty,
    mirroring `allocated(var_spec%units)`'s "not declared" gate, adapted
    for a `StringVector` with no natural "unset" allocatable state).
  - `UngriddedDimsCharacteristic` ← `var_spec%ungridded_dims` (always
    present, non-allocatable with a default — mirrors typekind's
    unconditional-insert precedent from phase-10c, not units' allocated
    gate).
  - `StandardNameCharacteristic` ← `var_spec%standard_name` (gated on
    `allocated`).
  - `QuantityTypeCharacteristic` ← the new `var_spec%quantity_type` field
    (unconditional insert, default `QUANTITY_UNKNOWN` is itself a valid,
    "no extension needed against anything" value per
    `QuantityTypeCharacteristic%needs_extension_for`'s own logic).
  - `ConservationCharacteristic` ← the new `var_spec%conservation` field
    (unconditional insert).
  - `NormalizationCharacteristic` ← the new `var_spec%normalization` field
    (unconditional insert).
- No change to `ExtensionResolution.F90`'s `find_mismatched_characteristics`/
  `build_chain` — both already iterate an arbitrary-length characteristics
  map/`mismatched_kinds(:)` array generically (confirmed by direct reading
  and by 10c's own prior confirmation); detection-wiring alone is
  sufficient to make these six axes visible.
- `GraphBuilder_ConnectionResolution_smod.F90`'s `merged_item_for_link`
  gains six new enumerated no-op branches (one per new kind), matching the
  existing `VERTICAL_COORDINATE_CHARACTERISTIC_KIND` branch's own pattern:
  each axis has no real `build_transform` adaptation (every one of the six
  `*Characteristic%build_transform` implementations already `_FAIL`s
  explicitly, by 5a2's own design), so a real mismatch on any of them
  surfaces through the existing "unsupported characteristic fails loudly"
  path (`ExtensionResolution.F90`'s `build_chain`,
  already-generic/no-code-change) rather than silently passing through or
  needing a merge computation.
- New fixture(s) under `superstructure/generic/tests/graph-scenarios/`
  exercising at least one real mismatch on one of the six now-detectable
  axes, confirming it is reported as an explicit, unsupported-characteristic
  failure rather than silently ignored.
- `superstructure/generic/tests/Test_GraphBuilder.pf` (or a new test file)
  gains coverage confirming a matched (non-mismatched) declaration on each
  of the six axes is accepted without triggering a spurious extension
  attempt — the common "false positive" risk of enabling new detection.

## Capabilities

### New Capabilities
None.

### Modified Capabilities
- `graph/graph-builder`: "Ordinary connections are resolved into
  dependency edges" currently resolves mismatches using "each item's own
  already-established identity and characteristics" — today that set of
  established characteristics is limited in practice to units/typekind/
  vertical-coordinate. This change extends what "established
  characteristics" means at advertise time to include six more axes,
  without changing the requirement's text (the requirement is already
  worded generically over "characteristics," not an enumerated list) — no
  delta needed for the existing requirement text itself; a delta is filed
  only to add a new scenario documenting that a mismatch on one of these
  six axes is detected and (absent a real adaptation) reported as an
  explicit unsupported-characteristic failure, not silently passed
  through.

Note: `graph/state-item-characteristics`' existing "Known concrete
characteristic subclasses are not limited to units, type-kind, and
geometry" requirement already describes all seven subclasses' existence
and classification (value vs. reference) — this change does not modify
that type hierarchy, only `GraphBuilder`'s own use of it, so no delta is
filed against that capability. `graph/extension-reuse`'s existing
"registered characteristic succeeds... unregistered characteristic fails
loudly" requirements already describe this behavior generically,
independent of which characteristic kinds happen to have a provider — no
delta needed there either (same precedent as phase-10c's own proposal.md
Capabilities note).

## Impact

- **Affected code:** `superstructure/generic/specs/VariableSpec.F90` (three
  new fields), `superstructure/generic/GraphBuilder_Advertising_smod.F90`
  (`populate_characteristics`), `superstructure/generic/
  GraphBuilder_ConnectionResolution_smod.F90` (`merged_item_for_link`), a
  new fixture directory (or directories) under `superstructure/generic/
  tests/graph-scenarios/`, `superstructure/generic/tests/
  Test_GraphBuilder.pf` and/or `Test_GraphEquivalenceHarness.pf`.
- **Not affected:** `ExtensionResolution.F90` (confirmed already generic,
  no change needed); the seven `*Characteristic` subclasses themselves
  (5a2 already landed them, tested, with correct comparison/signature
  logic — this change is a consumer, not a modifier, of that code);
  `ComponentSpecParser`/YAML (no new declaration surface added for the
  three new `VariableSpec` fields, same explicit deferral as
  `callback_interface_id`); `VerticalCoordinateCharacteristic` wiring
  (already landed, out of scope here per the correction above).
- **Downstream:** closes the gap §20.4.11 names as a required Phase 6a
  prerequisite — without this, Phase 6a-1's parity run cannot claim
  coverage of legacy's own ten-axis mismatch-detection surface (only three
  axes would be wired). Does not block, and is not blocked by, 3b2, Phase
  7, or Phase 13.
