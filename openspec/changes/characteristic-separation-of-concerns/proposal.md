## Why

The graph layer's description of a state item is spread across three
places: the `StateItemCharacteristicMap` on `GraphStateItem`, seven
`declared_*` / `ungridded_dims` / `vertical_stagger` components on the
same type, and per-characteristic dispatch chains inside `GraphBuilder`
(`populate_characteristics`, `merged_item_for_link`, `get_item_units`,
`get_item_typekind`, the `GEOMETRY_CHARACTERISTIC_KIND` inserts in
`GraphBuilder_Geometry_smod.F90`). Status handling (MIRROR, UNCHECKED)
is subclass logic or VariableSpec sentinel gating (`MAPL_TYPEKIND_MIRROR`
-> no entry), and Transform ordering comes from a variant-keyed table
(`characteristic_ordering_table`) with no legacy basis. Adding a
characteristic today means editing GraphBuilder in several places.

This change records the target design agreed with the owner
(`docs/graph/plans/separation-of-concerns.md` D1-D6) and the results of
the B0 study of legacy behavior, so that tasks B1-B5 can implement it
mechanically, one characteristic at a time.

## What Changes

- **D1** New `StateItemDescriptor` owned by `StateItemNode`, holding the
  characteristics map and nothing else. `GraphStateItem` keeps only the
  ESMF handle and member maps. Every `declared_*` component becomes a
  characteristic (three new ones: `VerticalStagger`, `RestartMode`,
  `Expression`; `ItemShape` widened to every shape).
- **D2** Status resolution moves to the base class / descriptor and is
  table-driven (export x import status). MIRROR resolution is a generic
  copy. Subclass methods only ever see SPECIFIED/SPECIFIED pairs. A
  per-(shape, kind) allowlist admits MIRROR exports for EXPRESSION.
  `FROM_COMP` is not introduced; `DEFERRED` gets a freeze check.
- **D3** Each characteristic contributes keyword values to one
  `MAPL_FieldSet` / `MAPL_FieldBundleSet` / `MAPL_StateSet` call made by
  `StateItemDescriptor%sync_to_info`. Structural keywords (geom,
  typekind, vgrid, ungridded dims, stagger) are creation inputs only and
  are never sent to a COMPLETE field, so the `FieldDelta` path is never
  entered. UNCHECKED is written as the legacy `'<unknown>'` marker for
  units and standard_name and as no entry for every other key.
- **D4** Extension order is owned by the export's shape
  (`ItemShapeCharacteristic%get_extension_order`), with the per-shape
  tables in design.md derived from legacy `get_aspect_order`.
  `GraphStateItem%ordering` and `characteristic_ordering_table` are
  removed.
- **D5** `VariableSpec%make_characteristics` replaces
  `populate_characteristics`, implemented as a loop over a
  per-characteristic factory list.
- **D6** GraphBuilder names no `*_CHARACTERISTIC_KIND` constant and no
  concrete `*Characteristic` type (checked by B4's one-off grep).
- **BREAKING (internal API)** `GraphStateItem%{get,set}_declared_*`,
  `get/set_ungridded_dims`, `get/set_vertical_stagger`, `ordering`,
  `has/get/set_characteristic`, `get_characteristics` move to the
  descriptor. No user-facing API or Info key changes.

## Capabilities

### New Capabilities
- `graph/state-item-descriptor`: the descriptor type, generic status
  resolution, the Info facade, and VariableSpec translation.

### Modified Capabilities
- `graph/state-item`: the characteristics map and ordering strategy
  leave `GraphStateItem`.
- `graph/state-item-characteristics`: status semantics become generic;
  ordering is owned by the export's shape; one-sided absence is not a
  mismatch.

## Impact

- `superstructure/generic/graph/` (`GraphStateItem`, `StateItemNode`,
  `characteristics/*`, `ExtensionResolution`, `ExtensionMaterialization`,
  new `StateItemDescriptor`).
- `superstructure/generic/GraphBuilder*.F90` (dispatch removal only).
- `superstructure/generic/specs/VariableSpec.F90` (new
  `make_characteristics`; `make_aspects` untouched).
- Infrastructure setters: no change required (design.md §3.2, §3.4).
  Optional hardening (drop `units` from `FieldSet`'s `FieldDelta`
  trigger), and `MAPL_StateSet` / bundle `restart_mode=` only if a later
  task needs them.
- No change to legacy `specs/*Aspect`, `registry/`, `transforms/`
  ("duplicate, do not delegate").
- No Info key or layout changes: History, ExtData, `SharedIO` unaffected.
