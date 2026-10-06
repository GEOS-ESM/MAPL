## Why

Roadmap Phase 4g (`docs/graph/spec/20-implementation-roadmap.md` §20.4.3)
is next. `14-route-handles.md` requires a semantic `RouteHandleKey`
(REQ-RH-002/003) and a `RouteHandleKey -> NodeId` reuse index on
`ComponentGraph` (REQ-RH-004/005, already named in REQ-CG-001's own
"semantic resource indexes" bullet). Neither exists today: a repo-wide
search finds no `RouteHandleKey` anywhere in source. This blocks any
future regrid `Transform` from being able to ask "has an equivalent
`RouteHandle` already been created for this geometry pair/options
combination" before creating a new, expensive one — the entire point of
REQ-RH-004/005.

The dependency this sub-change needs is already satisfied: 4e
(`horizontal-geometry-graph-state-item`, landed) gives every component's
horizontal geometry a real, graph-visible `NodeId` to reference as
`RouteHandleKey`'s source/destination geometry. The `RouteHandleValue`
representation itself (REQ-SI-002a/002b, REQ-CG-010 — a persistent
wrapper `ESMF_State` holding one `ESMF_RouteHandle` member, reporting
`variant() == MAPL_STATEITEM_ROUTEHANDLE`) is also already implemented
(`GraphStateItem%set_route_handle`/`get_route_handle`, landed with
Phase 1-2) — not part of this change's own scope.

## What Changes

- Add a new `RouteHandleKey` value type covering REQ-RH-002's required
  fields: source geometry `NodeId`, destination geometry `NodeId`,
  regridding method, masks, extrapolation options, normalization
  options, and the remaining ESMF regrid-store settings that affect
  which `RouteHandle` is produced. The field set is not invented fresh —
  it is taken directly from the existing, in-production
  `infrastructure/regridder_mgr/RoutehandleParam.F90`, which already
  enumerates exactly the settings `ESMF_FieldRegridStore` accepts and
  that REQ-RH-002's bullets describe; `RouteHandleKey` reuses that field
  set rather than re-deriving a parallel one (design.md D1).
- Give `RouteHandleKey` a canonical, deterministic string rendering
  (`to_string()`) sufficient to use as a key into `ComponentGraph`'s
  already-existing, already-generic semantic-index mechanism
  (`add_resource_index`/`get_resource_index`, `character(:) -> NodeId`,
  REQ-CG-001 — the same mechanism `item_key`/`proxy_key`/
  `ExtensionResolution`'s own chain-reuse key already use). **No new
  `ComponentGraph` API, storage, or gFTL map type is added** — REQ-RH-004
  ("`ComponentGraph` MAY maintain a semantic index: `RouteHandleKey ->
  NodeId`") and REQ-RH-005 ("MUST only locate, MUST NOT own") are both
  already true of the existing `resource_index` mechanism; this change
  is a new consumer of it, mirroring how `GeomCharacteristic`'s own
  `geom_id` is an opaque token rendered to text rather than a dedicated
  comparison type.
- REQ-RH-003 (same geometry pair MAY legitimately need several distinct
  `RouteHandle`s — e.g. linear vs. conservative — and the key MUST
  distinguish them) is satisfied by including `regridmethod` (and the
  other distinguishing fields) in the canonical rendering, verified by
  tests.
- Explicit deferral, stated here so it is not silently assumed solved,
  matching the roadmap entry's own wording: §14.4 time-dependent renewal
  (REQ-RH-006) is out of scope — this change covers the
  reuse-of-an-existing-handle case only, not renewal/invalidation.
  REQ-CG-008/009/010's in-place-update discipline for post-freeze
  renewal is a `ComponentGraph`/`GraphStateItem` concern already settled
  independently of this change and is not revisited here.
- No `RegridTransform`/real regrid-execution `Transform` is added. No
  `GraphBuilder.F90` wiring that constructs a `RouteHandleKey` from a
  declared connection is added either — building a real regrid
  `Transform` that would actually call `add_resource_index` with a
  `RouteHandleKey` is later work (the same "structure and detection now,
  execution later" posture 4e/4f already shipped for horizontal
  geometry/vertical grids, whose own `build_transform` still fails
  explicitly). This change makes `RouteHandleKey` itself exist, correct,
  and verified against the existing reuse mechanism; it does not wire
  a caller.

## Capabilities

### New Capabilities
- `graph/route-handle`: `RouteHandleKey`, a semantic key type
  distinguishing `RouteHandle` requests by source/destination geometry
  and regrid-relevant ESMF settings (REQ-RH-002/003), and its use as a
  reuse-lookup key via `ComponentGraph`'s existing semantic-index
  mechanism (REQ-RH-004/005).

### Modified Capabilities
(none — `component-graph`'s existing `resource_index`/
`add_resource_index`/`get_resource_index` mechanism, and its Requirement
text already naming "semantic resource indexes" generically, already
cover everything REQ-RH-004/005 need; this change is a new consumer of
an existing, general-purpose mechanism, not a change to its own
requirement, the same reasoning `vertical-grid`'s own proposal.md
recorded for `state-item`/`extension-reuse`.)

## Impact

- New source: one new module, `RouteHandleKey.F90`
  (`superstructure/generic/graph/`), defining the `RouteHandleKey` type,
  its constructor, accessors, `to_string()`, and `operator(==)`.
- No change to `ComponentGraph.F90`, `GraphStateItem.F90`,
  `GraphBuilder.F90`, or any existing `Characteristic`/`Transform`
  module.
- No change to `infrastructure/regridder_mgr/RoutehandleParam.F90` —
  reused as a field-set reference only, not called into (same
  "suggestive, not a dependency" posture `vertical-grid`'s design.md
  applied to legacy `VerticalGridAspect`).
- New tests exercising `RouteHandleKey` construction, its canonical
  rendering's distinguishing behavior (REQ-RH-003), and round-trip
  reuse through `ComponentGraph%add_resource_index`/`get_resource_index`
  (REQ-RH-004/005) against synthetic geometry `NodeId`s.
