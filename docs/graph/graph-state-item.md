# GraphStateItem

Source: `superstructure/generic/graph/GraphStateItem.F90` (825 lines).
Related: `StateItemFlag.F90` → `mapl_StateItemFlag_mod`
(`MAPL_StateItem_Flag`), `StateItemVariantInfo.F90`.

## What it is

`GraphStateItem` is the **concrete, non-polymorphic** payload held by
every `StateItemNode` (one per node, `node-hierarchy.md`). It fully
supersedes an earlier polymorphic `GraphValue` hierarchy design — see
`open-questions.md` (Q11) for why.

```fortran
type :: GraphStateItem
   private
   type(ESMF_Field), allocatable :: esmf_field
   type(ESMF_FieldBundle), allocatable :: esmf_field_bundle
   type(ESMF_State), allocatable :: esmf_state
   type(StateItemMemberMap) :: field_bundle_members_map
   type(StateItemMemberMap) :: state_members_map
   type(StateItemCharacteristicMap) :: characteristics
   type(UngriddedDims) :: ungridded_dims
   type(VerticalStaggerLoc), allocatable :: vertical_stagger
   type(ESMF_Geom), allocatable :: declared_geom
   class(VerticalGrid), allocatable :: declared_vertical_grid
   type(ESMF_StateItem_Flag) :: declared_item_type = ESMF_STATEITEM_FIELD
end type
```

(`GraphStateItem.F90:74-123`.) **At most one** of
`esmf_field`/`esmf_field_bundle`/`esmf_state` is ever allocated. No
separate route-handle component — a route handle is `esmf_state`
allocated in a persistent **wrapper role** holding exactly one
`ESMF_RouteHandle` member (see `route-handles.md`).

All allocation goes through the single generic `set` binding
(`:133`, dispatches to `set_field`/`set_field_bundle`/`set_state`/
`set_route_handle`) — the one choke point enforcing the at-most-one
invariant and membership-map gating. Raw component access is private;
there is no way around `set`/`get_*` from outside this module.

## Two-tier classification

- **`itemType()`** (`:182`) — native ESMF classification: which
  component is allocated (`FIELD`/`FIELDBUNDLE`/`STATE`/`NOTFOUND`).
  Purely mechanical, no role interpretation.
- **`variant()`** (`:219`) — extensible `MAPL_StateItem_Flag`, backed
  by `ESMF_Info` on the underlying handle (`StateItemVariantInfo.F90`'s
  `get_variant()`). Answers "which *role/subclass*", e.g.:
  - a `FIELD` with no tag → `MAPL_STATEITEM_FIELD`; tagged → `GEOM`
    (a geometry proxy field, `ESMF_FIELDSTATUS_GRIDSET`, see
    `characteristics.md`), or other refinements.
  - a `FIELDBUNDLE` → plain, or `VECTOR`/`BRACKET`/`VECTORBRACKET`
    (time-interpolation bracket vs. vector quantity — same ESMF shape,
    disambiguated only by this tag, not by type).
  - a `STATE` → plain, `VERTICALGRID` (members = per-physical-dimension
    coordinate Fields), or `ROUTEHANDLE` (checked directly via
    `state_is_route_handle_wrapper`, never via `Info` — a structural
    check, not a tag).

This two-tier split is why a geometry proxy, a vertical grid, and a
route handle all fit inside the same four-component shape without a
fifth component or a new node kind — see `open-questions.md` (Q11/Q14)
for the resolution history.

## Membership maps

`field_bundle_members_map`/`state_members_map` (REQ-SI-006): name →
`NodeId`, active only for the matching kind. Only graph-visible members
are tracked here — this is deliberately a sparse map, not a mirror of
every ESMF-level member.

## Characteristics

`characteristics: StateItemCharacteristicMap` — sparse,
`StateItemCharacteristicKind`-keyed map of this item's established or
deferred characteristics (units, type/kind, geometry, vertical
coordinate, ...). Independent of which ESMF component is allocated —
`clear_components()` does not reset this map. See `characteristics.md`.

## Materialization inputs

`ungridded_dims` (always-present, mirrors `VariableSpec%ungridded_dims`),
`vertical_stagger` (allocatable, mirrors `VariableSpec%vertical_stagger`
— legitimately absent for a 2D field), `declared_geom`/
`declared_vertical_grid`/`declared_item_type` — the originating
`VariableSpec`'s own per-item explicit overrides, captured once by
`advertise_one` (`GraphBuilder_Advertising_smod.F90`) so later
materialization code never needs to re-reach a `VariableSpec` by
component name.

## Copy hazard

`GraphStateItem` has a defined-assignment-bearing component
(`StateItemMemberMap`, via gFTL2 deep-copy) — plain `=` assignment of a
*populated composite* item has triggered nondeterministic gfortran heap
corruption under `-fcheck=all -finit-real=snan` (not reproduced under
NAG). Prefer `StateItemNode%get_payload_ptr()` over
`get_payload()`/intrinsic assignment when only reading or mutating in
place — see `node-hierarchy.md`.
