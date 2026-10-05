## 1. Geometry identity and Characteristic

- [x] 1.1 Add `GEOM_CHARACTERISTIC_ID` to `mapl_CharacteristicId_mod`
      (`superstructure/generic/graph/CharacteristicId.F90`), following the
      existing `UNITS_CHARACTERISTIC_ID`/`VERTICAL_GRID_CHARACTERISTIC_ID`
      pattern (`to_string()` case added).
- [x] 1.2 Add `mapl_GeomCharacteristic_mod`
      (`superstructure/generic/graph/GeomCharacteristic.F90`), extending
      `Characteristic`, structured like `VerticalGridCharacteristic.F90`:
      opaque identity-token constructor, `get_signature`,
      `needs_extension_for` (token equality, `class default` reports
      mismatch), `build_transform` failing explicitly (`_FAIL`, no
      horizontal-regrid provider yet).
- [x] 1.3 Add unit tests for `GeomCharacteristic`
      (`superstructure/generic/graph/tests/Test_GeomCharacteristic.pf`,
      mirroring `Test_Characteristic.pf`'s vertical-grid coverage):
      matching tokens report no extension needed, differing tokens report
      extension needed, `build_transform` fails with a distinguishable
      error (checked via returned `rc`, not `@assertExceptionRaised` -
      no precedent for that macro in this test target).

## 2. Geometry proxy field construction

- [x] 2.1 Add `mapl_GeomProxyField_mod`
      (`superstructure/generic/graph/GeomProxyField.F90`):
      `ESMF_FieldEmptyCreate` → `ESMF_FieldEmptySet(field, geom=geom)`
      (leaves the field in `ESMF_FIELDSTATUS_GRIDSET`) → tag with
      `MAPL_STATEITEM_GEOM` via `set_variant` → wrap as a `GraphStateItem`
      via `set_field`. Revised from the original plan (design.md D1):
      does **not** touch `GeomId` at all - identity is a separate concern
      handled entirely by `OuterMetaComponent%get_geom_id()` at the
      `GraphBuilder.F90` call sites (group 4), not by this constructor.
- [x] 2.2 Confirmed via unit test: the resulting field reports a grid
      assignment but no allocated data array, and `variant()` reports
      `MAPL_STATEITEM_GEOM` after wrapping.
- [x] 2.3 Add unit tests
      (`superstructure/generic/graph/tests/Test_GeomProxyField.pf`):
      grid-assigned/no-data status, `GEOM` variant tag, field-kind
      `itemType()`.

## 3. Global legacy/graph-native toggle (added - not in the original plan)

Introduced mid-implementation, at the user's request, anticipating that
graph-native code making real decisions (not just building parallel,
additive structure) will increasingly conflict with the
`OuterMetaComponent`/`StateRegistry` layer it runs alongside.

- [x] 3.1 Add `mapl_GraphMode_mod`
      (`superstructure/generic/graph/GraphMode.F90`): a single, global,
      default-`.false.` switch (`graph_native_enabled()`/
      `set_graph_native_enabled()`), mirroring
      `mapl_ExtensionResolution_mod`'s own `materialize_extensions`
      precedent exactly in shape.
- [x] 3.2 Gate `GraphBuilder.F90`'s entire geometry hook
      (`run_geometry_hook`) behind `graph_native_enabled()` - a no-op for
      every real production run today, since no production
      initialization code calls `set_graph_native_enabled(.true.)`.
      Differs from `materialize_extensions`'s narrower scope (which
      gates only real payload *materialization*, with chain *structure*
      always built) because geometry has no equivalent structure-vs-
      materialize split - there is no real allocation step to gate
      separately.
- [x] 3.3 Add a unit test confirming the hook is a no-op (creates no
      graph structure) when graph-native mode is disabled
      (`test_hook_is_noop_when_graph_native_disabled`,
      `Test_GraphGeometryHook.pf`).

## 4. Geometry advertise + cross-boundary dependency-edge wiring (revised from the original plan)

The original plan (design.md D5: declare the geometry item as an
ordinary reserved-name `VariableSpec` so it flows through
`resolve_one`/`build_characteristics` unmodified) turned out to be
unworkable and unnecessary, discovered mid-implementation:

- **Unworkable**: `initialize_advertise.F90`'s `self_advertise` walks
  `ComponentSpec%var_specs` and feeds every entry to
  `this%registry`/`StateRegistry%add_to_states` - a reserved-name
  `VariableSpec` inserted there would leak into real user-facing
  import/export states, directly violating REQ-GEO-003.
- **Unnecessary**: `GeometrySpec`/`initialize_geom_a.F90`/
  `initialize_geom_b.F90` already fully resolve which geometry a
  component ends up with (`GEOMETRY_PROVIDER`/`GEOMETRY_FROM_PARENT`/
  `GEOMETRY_FROM_CHILD`), in lifecycle phases 3-4, entirely before
  `GENERIC_INIT_ADVERTISE` (phase 5) - and hierarchy-wide, before any
  component reaches ADVERTISE. `GraphBuilder` does not need to
  re-resolve that decision via `VariableSpec`-based matching; it only
  needs to give the already-resolved outcome graph-native structure.

Revised, implemented design: a dedicated `GraphBuilder.F90` hook,
entirely separate from the `var_specs`-based advertise/connection-
resolution path, driven directly by `GeometrySpec%kind` and
`OuterMetaComponent%get_geom()`/`get_geom_id()`.

- [x] 4.1 Add `OuterMetaComponent%get_geom_id()` accessor
      (`superstructure/generic/OuterMetaComponent/get_geom_id.F90`),
      alongside the existing `has_geom()`/`get_geom()`/`set_geom()`.
- [x] 4.2 Add `graphbuilder_advertise_geometry(this)` (`GraphBuilder.F90`):
      if `this%has_geom()`, wraps `this%get_geom()` via
      `new_geom_proxy_item` and registers it as an ordinary
      `StateItemNode`, indexed under the reserved key
      `item_key(EXPORT, GEOMETRY_ITEM_NAME)` (`GEOMETRY_ITEM_NAME =
      'MAPL_Geometry'`, now public) - the same `item_key`/resource-index
      machinery ordinary items use, but never inserted into
      `ComponentSpec%var_specs`, so it is structurally unreachable from
      `self_advertise`/`add_to_states`.
- [x] 4.3 Add `graphbuilder_resolve_geometry(this)` (`GraphBuilder.F90`),
      covering both single-source cross-boundary shapes:
      - **Pull** (`GEOMETRY_FROM_CHILD`): `resolve_geometry_from_child`
        reuses `get_or_make_local_node_id()` **unmodified** - structurally
        identical to an ordinary cross-boundary `MatchConnection`.
      - **Push** (`GEOMETRY_FROM_PARENT`, the default): no existing
        machinery pulls in this direction, so a new, symmetric
        `push_geometry_to_child` injects a proxy node directly into the
        *child's* own graph (via the existing `get_child_component_graph()`
        framework-internal carve-out, REQ-GB-002), keyed by a new public
        `parent_geometry_proxy_key()`.
      - Both directions build a `GeomCharacteristic` from each side's
        `get_geom_id()` and go through `find_mismatched_characteristics`/
        the ordinary match/mismatch logic - `size(mismatched)==0` wires a
        direct dependency edge; a real mismatch is asserted as
        unreachable-given-`GeometrySpec`'s-own-exclusive-resolution
        (a defensive check, not a normal outcome for these two cases).
      - Both invoked together via `run_geometry_hook(this)`, relying on
        `initialize_advertise.F90`'s existing bottom-up recursion order
        (children complete their own `INIT_ADVERTISE`, hook included,
        before the parent's own hook runs) so that by the time a parent's
        hook runs, every relevant child's own geometry node already
        exists.
- [x] 4.4 Add `Test_GraphGeometryHook.pf`
      (`superstructure/generic/tests/`, real `OuterMetaComponent`/
      `GriddedComponentDriver` instances, not pure synthetic
      `ComponentGraph` - the hook reads real `OuterMetaComponent`
      accessors) covering:
      - a component providing its own geometry needs no resolution
        (`test_own_geometry_needs_no_resolution`);
      - a component receiving geometry from an ancestor - proxy +
        dependency edge land in the *child's* own graph
        (`test_child_receives_geometry_from_parent`);
      - a component receiving geometry from a child - proxy +
        dependency edge land in the *parent's* own graph, reusing
        `get_or_make_local_node_id` (`test_parent_receives_geometry_from_child`);
      - a genuine mismatch is reported (two `@assertExceptionRaised`
        checks matching the `_ASSERT`/`_VERIFY` chain) and does not wire
        a dependency edge (`test_mismatched_geometry_from_child_is_not_wired`);
      - the toggle itself (`test_hook_is_noop_when_graph_native_disabled`,
        listed under group 3 above).

      Test-setup note: geometries must be pre-tagged with a `GeomId`
      (via `mapl_GeomUtilities_mod%GeomSetId`, test-only) before being
      handed to `OuterMetaComponent%set_geom` - `set_geom`'s own
      fallback (mint a fresh id when none is present) has no public way
      to write that id back onto the geom's own `ESMF_Info`; only
      same-module code (`propagate_geom_to_children.F90`/
      `initialize_geom_a.F90`, via direct `geom_id` field assignment)
      can make two components agree on one id without pre-tagging.
      Documented in `Test_GraphGeometryHook.pf`'s own `make_test_geom`.

## 5. Documentation and roadmap bookkeeping

- [x] 5.1 Updated `docs/graph/spec/13-geometry-and-vertical-grids.md`'s
      status line with an "Implementation status (Phase 4e, landed)" note:
      REQ-GEO-001/002/003 satisfied for the single-source, static-geometry
      case (REQ-GEO-002a and §13.4 remain open/deferred), the
      `GeometrySpec`-already-resolves-connectivity deviation, and the new
      `graph_native_enabled()` gate.
- [x] 5.2 Updated `docs/graph/spec/20-implementation-roadmap.md` §20.4.3's
      4e entry: marked landed, documented the real deviations from the
      original plan (abandoned `VariableSpec`-based advertise approach,
      the dedicated push/pull hook design reusing existing cross-boundary
      and extension-reuse machinery, and the new global toggle).
