## Context

See proposal.md - Why/What Changes. Relevant current state:

- `ComponentGraph`'s generic semantic-index mechanism already exists and
  is already in production use: `resource_index : StateItemMemberMap`
  (`character(:) -> NodeId`), with `add_resource_index(key, node_id, rc)`
  / `get_resource_index(key)` (`ComponentGraph.F90`). `GraphBuilder.F90`'s
  `item_key()`/`proxy_key()` and `ExtensionResolution.F90`'s own chain-
  reuse key both already build plain `character` keys and go through this
  exact mechanism (REQ-CG-001's "semantic resource indexes" bullet is
  this mechanism, confirmed by inspection — `docs/graph/spec/07-*.md`
  does not name it, but `14-route-handles.md` REQ-RH-004's own worked
  example, `RouteHandleKey -> NodeId`, is the same shape `item_key`
  already produces). REQ-RH-005 ("locate, don't own") is already true of
  this mechanism: `resource_index` maps to `NodeId` only; ownership stays
  in `ComponentGraph`'s separate `nodes : NodeIdGraphNodeMap`.
- `RouteHandleValue`'s representation (REQ-SI-002a/002b, REQ-CG-010) is
  already fully implemented: `GraphStateItem%set_route_handle`/
  `get_route_handle` (`GraphStateItem.F90`), a persistent wrapper
  `ESMF_State` holding one `ESMF_RouteHandle` member, `variant() ==
  MAPL_STATEITEM_ROUTEHANDLE`. This landed with Phase 1-2, outside any
  tracked openspec change, and is unmodified by this change.
- 4e (`horizontal-geometry-graph-state-item`, landed) gives every
  component a geometry `StateItemNode` with a real `NodeId`, reachable
  via `graph%get_resource_index(item_key(ESMF_STATEINTENT_EXPORT,
  GEOMETRY_ITEM_NAME))`. This is the natural candidate for
  `RouteHandleKey`'s "source geometry"/"destination geometry" fields —
  resolves the loose end `17-open-questions.md` (line ~404) flagged as
  "not yet checked against `RouteHandleKey`'s geometry references": the
  answer is the same structural-fact pattern 4e/4f already established
  (reference by `NodeId` into the owning `ComponentGraph`, no separate
  opaque identity token invented for this purpose), not a new mechanism.
- `infrastructure/regridder_mgr/RoutehandleParam.F90` (legacy, pre-graph)
  already enumerates exactly the settings `ESMF_FieldRegridStore`
  accepts that affect its result: `srcMaskValues`/`dstMaskValues`,
  `regridmethod`, `polemethod`, `regridPoleNPnts`, `linetype`,
  `normtype`, `extrapmethod`, `extrapNumSrcPnts`, `extrapDistExponent`,
  `extrapNumLevels`, `unmappedaction`, `ignoreDegenerate`. Its own
  `equal_to` function independently confirms these are exactly the
  fields that matter for "are two regrid requests the same." Per
  explicit project direction (same posture `vertical-grid`'s design.md
  applied to legacy `VerticalGridAspect`): this module is read as a
  reference for *which fields matter*, never called into.
- A repo-wide search of this change's starting state finds no
  `RouteHandleKey` anywhere — this is greenfield within the graph
  module, matching the roadmap's own framing of Phase 4 as "no partially-
  built code to extend."
- ESMF regrid-flag enumerants actually exercised anywhere in this repo
  today (`grep`, excluding build directories): `ESMF_REGRIDMETHOD_
  BILINEAR/CONSERVE/CONSERVE_2ND/PATCH/NEAREST_STOD`,
  `ESMF_NORMTYPE_DSTAREA`, `ESMF_EXTRAPMETHOD_NONE`,
  `ESMF_POLEMETHOD_ALLAVG/NONE`, `ESMF_LINETYPE_GREAT_CIRCLE`,
  `ESMF_UNMAPPEDACTION_ERROR`. This is a narrower set than ESMF's full
  enumeration of each flag type.

## Goals / Non-Goals

**Goals:**
- Give `RouteHandleKey` a field set that is traceably exactly REQ-RH-002's
  bullets, each one justified against `RoutehandleParam.F90`'s existing,
  in-production field (Decision D1).
- Make `RouteHandleKey` usable as a key into the *existing*
  `resource_index` mechanism with zero new `ComponentGraph` surface
  area (Decision D2).
- Make REQ-RH-003's distinguishing requirement independently testable:
  two keys differing only in one settings field must render to different
  canonical strings.
- Resolve, rather than carry forward, the open question of what
  "source geometry"/"destination geometry" concretely means at the type
  level (Decision D3).

**Non-Goals:**
- §14.4 time-dependent renewal (REQ-RH-006) — explicit deferral, per
  proposal.md.
- A real regrid-executing `Transform`/`RegridTransform` node type, or
  any `GraphBuilder.F90` wiring that constructs a `RouteHandleKey` from
  a declared connection. This change delivers the key type and its
  reuse-lookup behavior only; a future change supplies the caller, the
  same "structure now, execution later" split 4e/4f already used for
  their own `build_transform`.
- Exhaustive coverage of every ESMF enumerant for every flag family in
  `to_string()` — scoped to the enumerants in Context's last bullet, with
  an explicit, loud failure for anything else (Decision D4), not a
  silent misrepresentation.
- Any change to `GraphStateItem`'s existing `RouteHandleValue`
  representation, or to `ComponentGraph`'s existing `resource_index`
  storage/API.

## Decisions

### D1: RouteHandleKey's field set is RoutehandleParam's field set, mapped onto REQ-RH-002's bullets
`RouteHandleKey` (new module, `RouteHandleKey.F90`) carries:

| REQ-RH-002 bullet | Field(s) | Type |
|---|---|---|
| source geometry | `source_geometry` | `NodeId` |
| destination geometry | `destination_geometry` | `NodeId` |
| regridding method | `regridmethod` | `ESMF_RegridMethod_Flag` |
| masks | `srcMaskValues`, `dstMaskValues` | `integer, allocatable :: (:)` |
| extrapolation options | `extrapmethod`, `extrapNumSrcPnts`, `extrapDistExponent`, `extrapNumLevels` | mixed, mirroring `RoutehandleParam` |
| normalization options | `normtype` | `ESMF_NormType_Flag` |
| other relevant ESMF settings | `polemethod`, `regridPoleNPnts`, `linetype`, `unmappedaction`, `ignoreDegenerate` | mixed, mirroring `RoutehandleParam` |

This is a direct, field-for-field adoption of
`RoutehandleParam.F90`'s own component list (Context) minus
`srcTermProcessing` (already dead/commented out in the legacy source,
not a real `ESMF_FieldRegridStore` input today). No new field is
invented beyond what legacy already found necessary.

**Alternative considered:** derive the field set fresh from
`ESMF_FieldRegridStore`'s own argument list directly. Rejected: this
would re-derive exactly what `RoutehandleParam.F90` already encodes and
has already validated in production use; doing so independently risks
silent drift between the two (e.g. missing an argument legacy already
learned mattered).

**Alternative considered:** wrap `RoutehandleParam` itself as a
component of `RouteHandleKey` rather than re-declaring its fields.
Rejected: `RoutehandleParam` is a legacy, non-graph-neutral module
(`infrastructure/regridder_mgr/`); `superstructure/generic/graph/`
modules do not depend on `infrastructure/` modules elsewhere in this
codebase (mirrors REQ-CG-002's general graph-neutrality posture, which,
while written specifically about `OuterComponent`/`StateRegistry`,
reflects the same "graph core does not reach into legacy infrastructure
modules" principle already followed by every other graph module). A
field-for-field adoption keeps `RouteHandleKey` dependency-free of
`infrastructure/regridder_mgr` while still being traceably the same
field set.

### D2: RouteHandleKey has no dedicated gFTL map or ComponentGraph API — it is a key into the existing resource_index
`RouteHandleKey` gains a `to_string()` function producing a canonical,
deterministic `character(:)` rendering, in the same spirit as
`item_key()`/`proxy_key()`/`ExtensionResolution.F90`'s own reuse key
(Context) — a prefixed, colon-delimited concatenation:
`'ROUTEHANDLE:' // source_geometry%to_string() // ':' //
destination_geometry%to_string() // ':' // <regrid-method code> // ':'
// <mask/extrap/norm/other codes...>`. A caller registers/looks up a
`RouteHandle`'s owning `NodeId` via the *existing*
`graph%add_resource_index(key%to_string(), node_id, rc)` /
`graph%get_resource_index(key%to_string())` — unchanged signatures, no
new `ComponentGraph` method, no new gFTL `Map` instantiation (unlike
`PortBindingKey`/`PortBindingKeyPortMap`, which needed a dedicated gFTL
map because `PortBindingTable`'s own storage is not the generic
`resource_index`).

**Alternative considered:** a dedicated `RouteHandleKeyNodeIdMap` gFTL
map (mirroring `PortBindingKeyPortMap`'s pattern) with `RouteHandleKey`
as a genuine composite map key (`operator(<)`, following
`PortBindingKey`'s own "only a strict weak ordering is required, not a
semantically meaningful one" precedent). Rejected: `ComponentGraph`
already owns one generic semantic index for exactly this purpose
(Context); adding a second, parallel, type-specific index would give
`ComponentGraph` two different mechanisms doing the same job for two
different key types, with no behavioral benefit — REQ-RH-004 only asks
that *a* semantic index exist and locate by this key, not that it be
its own dedicated map. `operator(==)` is still provided on
`RouteHandleKey` directly (next bullet), for callers that want to
compare two keys without using the index at all.

`RouteHandleKey` also provides `operator(==)`, implemented by comparing
`to_string()` output — guarantees "equal key ⇒ equal rendering" matches
"equal rendering ⇒ equal key" by construction, with a single source of
truth for equality (no separate field-by-field comparison function to
keep in sync with `to_string()`, unlike legacy `RoutehandleParam`'s own
`equal_to`, which compares fields directly and independently of its
separate `make_info`).

### D3: Source/destination geometry are NodeId references into the owning ComponentGraph, not a new opaque identity token
`RouteHandleKey%source_geometry`/`destination_geometry` are plain
`NodeId` values — the `NodeId` of the relevant component's own
horizontal-geometry `StateItemNode` (4e, `GEOMETRY_ITEM_NAME`), reached
the same way any other 4e/4f consumer reaches it:
`graph%get_resource_index(item_key(ESMF_STATEINTENT_EXPORT,
GEOMETRY_ITEM_NAME))`. This mirrors `PortBindingKey`'s own precedent
(a composite key built directly from `NodeId`s already owned by the
graph, Context) rather than `GeomCharacteristic`'s precedent (a
separate opaque text token, `geom_id`, invented because `Characteristic`
instances are compared *before* a shared `ComponentGraph`/`NodeId`
necessarily exists for both sides). For `RouteHandleKey`, both
geometries are only ever known once each side's `StateItemNode` already
has a `NodeId` (by construction — a regrid is only ever requested
between two things already represented in the graph), so there is no
need for a token predating `NodeId` assignment.

**Resolves `17-open-questions.md`'s loose end** (flagged, not fully
checked, under Q11's sub-item 2's follow-on discussion): `RouteHandleKey`
does *not* need a `GeomValue`/opaque-token-style identity — ordinary
`NodeId` equality (via `to_string()`, D2) is sufficient, because
`NodeId` equality already implies "the identical geometry `StateItemNode`
in a given `ComponentGraph`," which is exactly the comparison
REQ-RH-002's "source geometry"/"destination geometry" bullets ask for.

**Cross-`ComponentGraph` case, noted not solved further here:** when the
source and destination geometry belong to different components (so,
potentially, different `ComponentGraph` instances — the same situation
4e's own design.md handles for its own ancestor/child geometry
dependency edges), `NodeId` values remain globally unique and comparable
by `to_string()` regardless of which `ComponentGraph` owns them, so
`RouteHandleKey` construction and comparison both still work unmodified.
*Which* `ComponentGraph`'s `resource_index` a given `RouteHandleKey` is
ultimately registered against (consumer's graph vs. provider's) is a
question for the future `GraphBuilder.F90` wiring this change explicitly
defers (proposal.md What Changes) — not resolved here because no such
wiring exists yet to make the choice concrete.

### D4: to_string() covers the enumerants already used in this repo today; anything else fails loudly
`to_string()` renders each `ESMF_*_Flag` field via an explicit
`if (<field> == ESMF_<FLAG>_<ENUMERANT>) then` chain covering exactly
the enumerants Context's last bullet lists (the ones this repo's own
`RoutehandleParam.F90`/other regrid call sites already exercise), each
mapped to a short, stable code string (e.g. `ESMF_REGRIDMETHOD_BILINEAR`
→ `"BILINEAR"`). An enumerant outside that covered set causes `to_string()`
to `_FAIL` explicitly rather than guess or silently drop information
from the rendering — the same "fail explicitly on an unrecognized ESMF
value" posture `RoutehandleParam%make_info` already takes for
`regridmethod` (Context: it only recognizes `BILINEAR`/`CONSERVE` and
`_FAIL`s otherwise). Extending the covered set for a new enumerant is a
mechanical, additive change to this one function, not a design change —
recorded here so a future author knows this limit is deliberate, not an
oversight.

**Alternative considered:** derive a numeric code from each flag value
reflectively (e.g. via `transfer()` on the flag's underlying integer
representation) instead of an explicit name mapping. Rejected: ESMF flag
types do not expose a documented, stable public integer accessor for
this purpose in the versions this repo builds against (confirmed by
inspection — `RoutehandleParam.F90` itself only ever compares flags via
`==`, never extracts an integer), and relying on an undocumented memory
layout would be fragile across ESMF versions. An explicit mapping is
more code but has no such hazard, and is the same choice legacy already
made for its own (narrower) `make_info`.

## Risks / Trade-offs

- [Risk] D4's enumerant coverage is a strict subset of ESMF's full
  enumeration for each flag family. → Mitigation: covers everything this
  repo currently exercises (Context); any gap surfaces as an immediate,
  loud `_FAIL` at key-construction/rendering time, not a silent
  collision between two actually-different settings, and is a one-line
  addition to fix when hit.
- [Risk] `to_string()`'s real-valued fields (`extrapDistExponent`) are
  rendered via exact-value formatted output, so two floating-point
  values that are "meant" to be the same but differ in the last bit
  would render as different keys (a missed-reuse false negative, never
  a false-positive collision). → Mitigation: this is strictly safer than
  the alternative failure mode (wrongly reusing a `RouteHandle` built
  with different extrapolation weighting); legacy `RoutehandleParam%
  equal_to` already compares this same field with exact `==`, so this is
  not a new risk this change introduces.
- [Trade-off] D3 does not solve "which `ComponentGraph` owns the
  `RouteHandleKey -> NodeId` registration" for the cross-component case —
  deliberately left to the future wiring change (proposal.md explicit
  deferral), since no real caller exists yet to make that choice
  concrete against.
- [Trade-off] No `RegridTransform`/caller is added (Non-Goals) — this
  change is unreachable from any real component configuration until a
  later change wires it in, the same posture 4e/4f's own `build_transform`
  shipped with (structure and detection now, execution later).

## Open Questions

None — D1-D4 above resolve every technical choice this change's scope
requires; REQ-RH-006/§14.4 remains an explicit, intentional deferral
(proposal.md), not an unresolved question of this change's own design.
