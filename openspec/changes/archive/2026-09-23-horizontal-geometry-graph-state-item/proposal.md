## Why

Roadmap Phase 4e (`docs/graph/spec/20-implementation-roadmap.md` §20.4.3)
is next: horizontal geometry (`Grid`/`Mesh`/`LocStream`/`XGrid`) is still
handled entirely through legacy special-cased inheritance logic
(`GeomAspect`, `StateRegistry`'s grid-propagation code), not through the
graph. `docs/graph/spec/13-geometry-and-vertical-grids.md` §13.1–13.2
requires geometry to become an ordinary graph-visible item — carried as
an incomplete `esmf_field` proxy (`ESMF_FIELDSTATUS_GRIDSET`) and resolved
through the same advertise/connect/transform machinery every other
`GraphStateItem` already uses — replacing that special-casing rather than
adding a parallel path next to it. The supporting plumbing (the `GEOM`
variant tag in `MAPL_StateItem_Flag`, the `variant()`-tagged-field
structural allowance in `state-item`) already landed with Phase 1–2 but
is unused by any real code path; nothing today creates, advertises, or
connects a geometry `GraphStateItem`.

## What Changes

- Add a geometry-proxy-field constructor: an incomplete `ESMF_Field`
  (`ESMF_FieldEmptyCreate`, left in `ESMF_FIELDSTATUS_GRIDSET` — grid
  assigned, no data array allocated) tagged with the existing
  `MAPL_STATEITEM_GEOM` variant, wrapped as an ordinary `GraphStateItem`
  (REQ-GEO-001, REQ-GEO-003).
- Add a `GeomCharacteristic` (mirroring the existing
  `VerticalGridCharacteristic` precedent exactly: opaque identity-token
  comparison, `build_transform` failing explicitly) so two geometry items
  match when their owning components' `OuterMetaComponent%get_geom_id()`
  values agree.
- Add a dedicated `GraphBuilder.F90` hook (`run_geometry_hook`) that gives
  the three REQ-GEO-002 single-source relationships — a component
  providing its own geometry, receiving it from an ancestor, or receiving
  it from a child — real graph structure: one `StateItemNode` per
  component (wrapping whatever geometry `GeometrySpec` already resolved),
  plus a real cross-`ComponentGraph` dependency edge for the ancestor/child
  cases (reusing the existing cross-boundary proxy machinery for the
  "from child" pull direction; a new, symmetric helper for the "from
  parent" push direction, which has no existing precedent to reuse).
  **Revised from the original plan**: geometry connectivity is not
  re-resolved by this hook — `GeometrySpec`/`initialize_geom_a.F90`/
  `initialize_geom_b.F90` already fully resolve it, in lifecycle phases
  that complete hierarchy-wide before this hook ever runs (discovered
  mid-implementation; see design.md D3). The hook only represents that
  already-correct outcome in the graph.
- Add `mapl_GraphMode_mod`, a single global `graph_native_enabled()` /
  `set_graph_native_enabled()` toggle (added mid-implementation at
  explicit request, mirroring `mapl_ExtensionResolution_mod`'s own
  `materialize_extensions` precedent), gating the entire new geometry hook
  — default off, so no real production run's behavior changes. Intended
  as general, reusable infrastructure for future graph-native/legacy
  conflicts beyond this change's own scope.
- Explicit deferrals, carried forward from the roadmap and stated here so
  they are not silently assumed solved:
  - REQ-GEO-002a (exchange-component geometry, e.g. `SURF`-style
    multi-source `XGrid`) — a distinct many-to-one connectivity shape,
    out of scope.
  - §13.4 (time-dependent geometry renewal under freeze) — out of scope;
    only static, freeze-time geometry wiring is covered.
  - Real mismatched-geometry regridding execution: a mismatch is
    detected (via the new `GeomCharacteristic`) and reported through the
    existing extension-reuse delegation path (`graph-builder`'s
    "mismatched export and import are wired through an extension chain"
    behavior), but `GeomCharacteristic%build_transform` fails explicitly
    rather than building a real regrid — same posture
    `VerticalGridCharacteristic` already shipped for vertical grids. A
    mismatch surfaces as an explicit, reported failure to wire, not a
    silent incorrect connection and not a crash.

## Capabilities

### New Capabilities
- `graph/horizontal-geometry`: geometry as a first-class `GraphStateItem`
  — proxy-field construction and the cross-boundary graph structure
  (`GraphBuilder.F90`'s geometry hook) that represents REQ-GEO-002's three
  single-source relationships, per REQ-GEO-001/002/003.

### Modified Capabilities
(none — `state-item`'s existing "no dedicated geometry node kind"
scenario, `graph-builder`'s existing cross-boundary proxy machinery
[`get_or_make_local_node_id`], and extension-reuse's existing mismatch/
chain-delegation machinery already cover the structural and resolution
behavior this change exercises; this change is a new consumer of all
three, not a modification of any of their own requirements)

## Impact

- New source: a geometry-proxy-field constructor
  (`superstructure/generic/graph/GeomProxyField.F90`), a new
  `GeomCharacteristic` type and `GEOM_CHARACTERISTIC_ID`
  (`GeomCharacteristic.F90`/`CharacteristicId.F90`), a new global toggle
  (`GraphMode.F90`), a new `OuterMetaComponent%get_geom_id()` accessor, and
  a new self-contained `GraphBuilder.F90` hook
  (`run_geometry_hook`/`graphbuilder_advertise_geometry`/
  `graphbuilder_resolve_geometry`/`resolve_geometry_from_child`/
  `push_geometry_to_child`).
- `GraphBuilder.F90`'s existing `resolve_one`/`build_characteristics`
  (ordinary `VariableSpec`-based connection resolution) are **untouched**
  — the original plan to add a branch there was abandoned (design.md D3);
  the new hook is entirely separate.
- `initialize_advertise.F90` gains one new call
  (`gb%run_geometry_hook(this)`), a no-op unless
  `set_graph_native_enabled(.true.)` has been called — true for every
  real production run today.
- Reuses existing `mapl_GeomId_mod`'s `GeomId` type as
  `GeomCharacteristic`'s identity token, but reads it via the new
  `OuterMetaComponent%get_geom_id()` accessor, not via the geom's own
  `ESMF_Info` (`MAPL_GeomGetId`) — a change from the original plan,
  discovered mid-implementation (design.md D2).
- No change to `GraphStateItem`'s structure, to `ComponentSpec%var_specs`,
  or to `GeometrySpec`/`initialize_geom_a.F90`/
  `initialize_geom_b.F90`/`propagate_geom_to_children.F90` — this change
  is a consumer of all of them, not a modification.
- Legacy `GeomAspect`/`StateRegistry` grid-inheritance code is untouched by
  this change (additive posture, same as Phase 3/4's prior sub-changes) —
  retirement is Phase 6 business.
