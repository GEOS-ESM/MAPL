## 1. RouteHandleKey type

- [x] 1.1 Create `superstructure/generic/graph/RouteHandleKey.F90`
      (`mapl_RouteHandleKey_mod`) defining the `RouteHandleKey` derived
      type with the fields in design.md D1's table: `source_geometry`/
      `destination_geometry` (`NodeId`), `regridmethod`
      (`ESMF_RegridMethod_Flag`), `srcMaskValues`/`dstMaskValues`
      (`integer, allocatable :: (:)`), `extrapmethod`
      (`ESMF_ExtrapMethod_Flag`), `extrapNumSrcPnts` (`integer`),
      `extrapDistExponent` (`real(ESMF_KIND_R4)`), `extrapNumLevels`
      (`integer, allocatable`), `normtype` (`ESMF_NormType_Flag`),
      `polemethod` (`ESMF_PoleMethod_Flag`), `regridPoleNPnts`
      (`integer, allocatable`), `linetype` (`ESMF_LineType_Flag`),
      `unmappedaction` (`ESMF_UnmappedAction_Flag`), `ignoreDegenerate`
      (`logical`).
- [x] 1.2 Add a structure-constructor-style `new_RouteHandleKey`
      function (mirroring `RoutehandleParam`'s own constructor defaults:
      `regridmethod` defaults to `ESMF_REGRIDMETHOD_BILINEAR`,
      `normtype` to `ESMF_NORMTYPE_DSTAREA`, `extrapmethod` to
      `ESMF_EXTRAPMETHOD_NONE`, `unmappedaction` to
      `ESMF_UNMAPPEDACTION_ERROR`, `ignoreDegenerate` to `.false.`,
      `linetype` to `ESMF_LINETYPE_GREAT_CIRCLE`) with `source_geometry`/
      `destination_geometry` as required, non-optional arguments and
      every other field optional.
- [x] 1.3 Add accessors for every field (`get_source_geometry`,
      `get_destination_geometry`, `get_regridmethod`, etc. - read-only,
      matching `PortBindingKey`'s own accessor-only, no-setter precedent).

## 2. Canonical rendering and equality

- [x] 2.1 Implement `to_string(this) result(key_string)` producing the
      colon-delimited rendering in design.md D2:
      `'ROUTEHANDLE:' // source%to_string() // ':' // destination%to_string()
      // ':' // <field codes...>`. Render `srcMaskValues`/`dstMaskValues`
      as a deterministic, order-preserving comma-joined integer list
      (empty-but-allocated vs. unallocated rendered distinguishably).
      Render each `ESMF_*_Flag` field via the explicit enumerant-to-code
      mapping in design.md D4, covering exactly: `ESMF_REGRIDMETHOD_
      BILINEAR/CONSERVE/CONSERVE_2ND/PATCH/NEAREST_STOD`,
      `ESMF_NORMTYPE_DSTAREA`, `ESMF_EXTRAPMETHOD_NONE`,
      `ESMF_POLEMETHOD_ALLAVG/NONE`, `ESMF_LINETYPE_GREAT_CIRCLE`,
      `ESMF_UNMAPPEDACTION_ERROR/IGNORE`. Any other enumerant value
      reaching this function MUST `_FAIL` explicitly (design.md D4) -
      add a `rc`/status output argument to `to_string()` for this.
- [x] 2.2 Implement `operator(==)` for `RouteHandleKey` by comparing
      `to_string()` output (design.md D2) - no independent field-by-field
      comparison function.
- [x] 2.3 Add `RouteHandleKey.F90` to
      `superstructure/generic/graph/CMakeLists.txt`'s source list
      (alongside `PortBindingKey.F90`/`GeomCharacteristic.F90`).

## 3. Unit tests: key construction, rendering, REQ-RH-003 distinguishing behavior

- [x] 3.1 Add `superstructure/generic/graph/tests/Test_RouteHandleKey.pf`:
      construction with required-only arguments applies the documented
      defaults (1.2); every accessor (1.3) returns the value the key was
      constructed with.
- [x] 3.2 Test REQ-RH-003 directly: two keys built for the identical
      `source_geometry`/`destination_geometry` `NodeId` pair but
      different `regridmethod` (e.g. `BILINEAR` vs. `CONSERVE`) render
      distinct `to_string()` output and compare unequal via
      `operator(==)`. Repeat for at least one other distinguishing field
      (e.g. differing `srcMaskValues`).
- [x] 3.3 Test that two keys built separately with identical arguments
      (including identical `NodeId`s) render identical `to_string()`
      output and compare equal.
- [x] 3.4 Test `to_string()`'s explicit-failure path (design.md D4): a
      key carrying an `ESMF_RegridMethod_Flag` value outside the covered
      enumerant set causes `to_string()` to report failure via `rc`, not
      a silent/garbled rendering.
- [x] 3.5 Add `Test_RouteHandleKey.pf` to
      `superstructure/generic/graph/tests/CMakeLists.txt`'s test source
      list.

## 4. Integration test: reuse via ComponentGraph's existing semantic index

- [x] 4.1 Add a test (new file or appended to
      `Test_ComponentGraph_PortBindings.pf`'s sibling coverage,
      whichever existing file already covers `resource_index` round-trips
      most directly) that: builds a synthetic `ComponentGraph` with two
      geometry-variant `StateItemNode`s (reusing existing geometry-item
      test fixtures/helpers where available), constructs a
      `RouteHandleKey` referencing their `NodeId`s, registers a third,
      synthetic `RouteHandle`-variant `StateItemNode`'s `NodeId` against
      `graph%add_resource_index(key%to_string(), node_id, rc)`, and
      confirms `graph%get_resource_index(key%to_string())` returns that
      same `NodeId` (REQ-RH-004).
- [x] 4.2 Extend the same test to confirm REQ-RH-005: looking up a
      `RouteHandleKey` that was never registered returns no match (the
      existing `get_resource_index` not-found behavior, unmodified),
      demonstrating the index only locates and never fabricates
      ownership.
- [x] 4.3 Extend the same test to confirm REQ-RH-003 end-to-end through
      the index: two distinct keys (differing only in `regridmethod`)
      for the same geometry pair, each registered against a different
      `NodeId`, both resolve independently via `get_resource_index` -
      no collision between them.

## 5. Documentation and roadmap bookkeeping

- [x] 5.1 Update `docs/graph/spec/14-route-handles.md`'s status line
      with an "Implementation status (Phase 4g, landed)" note:
      REQ-RH-002/003 satisfied by `RouteHandleKey`
      (`superstructure/generic/graph/RouteHandleKey.F90`), REQ-RH-004/005
      satisfied by reuse of `ComponentGraph`'s pre-existing
      `resource_index`/`add_resource_index`/`get_resource_index`
      mechanism (no new `ComponentGraph` API), REQ-RH-006/§14.4 remaining
      `[OPEN]`/deferred.
- [x] 5.2 Update `docs/graph/spec/20-implementation-roadmap.md` §20.4.3's
      4g entry: mark landed, note the key design choice (reuse of the
      existing generic semantic index rather than a dedicated gFTL map,
      design.md D2) and that no `RegridTransform`/`GraphBuilder.F90`
      wiring was added in this sub-change (explicit deferral, carried
      forward for whichever future change builds real regrid execution).
