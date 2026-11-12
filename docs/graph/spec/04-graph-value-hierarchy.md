# 4. GraphValue Hierarchy

Status: `[SETTLED]` core hierarchy and composition rule, as originally
specified, alongside the now-resolved `[SETTLED]` `GraphStateItem` amendment
(§4.6, `17-open-questions.md` Q11 — fully resolved); `[OPEN]` composite
revision semantics detail (§4.4, cross-ref `17-open-questions.md` Q8).

**Reading note:** §4.1–§4.5 describe the original polymorphic `GraphValue`
design. §4.6 describes the adopted `GraphStateItem` amendment, which
supersedes REQ-VAL-001 for `StateItemNode`'s payload (REQ-SI-005) for the
four ESMF handle kinds it covers. §4.1–§4.5 are retained, not deleted:
`GeomValue` (§4.2a) remains the documented fallback if the `esmf_field`-
based geometry approach (§4.6.4) proves problematic in practice, and the
polymorphic `GraphValue` reading remains the reference model for anything
not yet reconciled into `GraphStateItem`.

## 4.1 Composition, not subclassing per node

**REQ-VAL-001.** `StateItemNode` MUST use composition (holding one
polymorphic `GraphValue`) rather than defining a separate `GraphNode`
subclass per payload kind. All value kinds share exactly one node type,
`StateItemNode`.

**Superseded, for the four `GraphStateItem`-covered handle kinds.** §4.6
(REQ-SI-001–006) replaces "one polymorphic `GraphValue`" with "one
concrete `GraphStateItem`" for the `Field`/`FieldBundle`/`State`/`RouteHandle`
kinds — resolved, `17-open-questions.md` Q11. REQ-VAL-001 as stated above
remains the description of the original design and applies unchanged to
any value kind outside `GraphStateItem`'s four covered kinds.

## 4.2 GraphValue hierarchy

Minimum set of `GraphValue` concrete types:

- `FieldValue`
- `FieldBundleValue`
- `StateValue`
- `GeomValue`
- `VerticalGridValue`
- `RouteHandleValue`

**Correction (recorded, not a renumbering — nothing here was previously a
numbered requirement).** Earlier drafts of this list enumerated `GridValue`,
`MeshValue`, and `LocStreamValue` as three separate concrete types, and
omitted ESMF's exchange grid (`XGrid`) entirely. Both are fixed by
`GeomValue` below, confirmed correct.

**REQ-VAL-002.** `GraphValue` (the abstract root of this hierarchy) MUST be
graph-neutral: it MUST NOT depend on `Graph`, `ComponentGraph`, or
`GraphBuilder`. It MAY depend on `NodeId` (a low-level identity type, not a
graph container) — see `05-identities.md`.

**Implementation note:** to keep REQ-VAL-002 true in practice, `NodeId` and
its siblings MUST live in a module with no dependency on `ComponentGraph` or
higher layers. This is an explicit module-layering requirement, not just an
aspiration.

## 4.2a GeomValue

**REQ-VAL-008.** `GeomValue` MUST be a concrete (non-polymorphic) type
using composition, with allocatable components for `ESMF_Grid`,
`ESMF_Mesh`, `ESMF_LocStream`, and `ESMF_XGrid`, of which **at most one MAY
be allocated** at a time. One `GeomValue` type suffices for all four
geometry kinds; there is no separate `GridValue`/`MeshValue`/
`LocStreamValue`.

**Note — structural echo with `GraphStateItem`:** this is the same
closed-variant pattern as `GraphStateItem` (§4.6, REQ-SI-002) — a fixed set of
allocatable ESMF-handle components, at most one allocated, used to enable
generic operations without `select type`. The two types serve different
layers (`GraphStateItem` for the ESMF-State-visible handles; `GeomValue` for
geometry objects that REQ-ESMF-005 says cannot be placed directly in an
ESMF State), but the repeated shape may be worth factoring into one shared
template later, the same way ID types already share the FPP template
(`05-identities.md` REQ-ID-004). Not required for v1.1; noted for future
consideration.

**Cross-reference:** under the `GraphStateItem` amendment (§4.6.4, resolved),
geometry that must be visible inside a `GraphStateItem`-bearing `StateItemNode`
is instead carried directly as an incomplete `esmf_field`, not as a
`GeomValue`. `GeomValue` remains the correct shape for the *original*
polymorphic-`GraphValue` reading (§4.1–§4.5), and is the fallback if the
`esmf_field`-based approach (§4.6.4) proves problematic in practice.

## 4.3 FieldBundleValue

**REQ-VAL-003.** `FieldBundleValue` MAY contain a map from member name to
`NodeId`, covering only members that are graph-visible.

**Clarification — cross-component access.** This map is local bookkeeping
for whichever `ComponentGraph` owns the `FieldBundleValue` (e.g., useful
for the exporting side's own extension-reuse search, REQ-EXT-005). It is
NOT shared or consulted across a `ComponentGraph` boundary — a `NodeId` is
only meaningful within the graph that minted it (`05-identities.md`
REQ-ID-005). When a `FieldBundle` is connected export-to-import
(`09-extension-reuse.md` REQ-EXT-002/003), the importing side accesses
individual members either (a) at the plain ESMF level
(`ESMF_FieldBundleGet` by name), independent of either side's graph state,
or (b) by running its own local advertise/wiring process to create its own
local `StateItemNode` for a member it needs as a first-class graph citizen
— never by reading the exporting side's member map. This map, in other
words, is not itself a graph operation for the receiving side.

**REQ-VAL-004.** An underlying `ESMF_FieldBundle` MAY contain members that
have not yet become graph nodes (e.g., not yet advertised into the graph).
The map in REQ-VAL-003 is not required to be exhaustive over the ESMF
FieldBundle's actual membership.

## 4.4 StateValue

**REQ-VAL-005.** `StateValue` MUST contain:

- one `ESMF_State`
- a map from ESMF member name → `NodeId`

**REQ-VAL-006.** A state member name is a map *key* and MUST NOT be
duplicated inside the mapped value (i.e., the `StateItemNode`/`NodeId`
pointed to does not itself store "the name it was found under" — that name
is a property of this particular membership, not of the node).

**REQ-VAL-007.** Named aliases (`ESMF_NamedAlias`) are NOT separate graph
nodes. Two different state memberships MAY map two different member names to
the *same* `StateItemNode`/`NodeId`. This directly reflects REQ-ESMF-003
(aliases share underlying data/Info) at the graph level.

## 4.5 Open: composite revision semantics

How `NodeRevision` behaves for `StateValue`/`FieldBundleValue` composites
(does the container revision advance when a member's revision advances? are
they tracked independently?) is addressed at a first-pass level in
`11-revision-and-update.md` §11.4, but is flagged `[OPEN]` — see
`17-open-questions.md` Q8.

## 4.6 Amendment: `GraphStateItem` — a concrete, non-polymorphic node payload

Status: `[SETTLED]` — adopted; interaction with §4.1–§4.5 resolved
(`17-open-questions.md` Q11, fully resolved). `VerticalGrid`'s
representation, which does not collapse into `GraphStateItem` the same way as
geometry, is settled separately as Q14 (§13.3.2,
`13-geometry-and-vertical-grids.md` REQ-GEO-009). The component count and
classification mechanism were revised again in §4.6.2/§4.6.2a (three
allocatable components, not four; two-tier kind/variant classification)
— see the note at the head of §4.6.2.

### 4.6.1 Motivation

**REQ-SI-001.** A `GraphStateItem` type is proposed to replace polymorphic
dispatch over `FieldValue`/`FieldBundleValue`/`StateValue`/`RouteHandleValue`
with a single concrete type having allocatable components, one per ESMF
handle kind. The motivation is to facilitate **generic operations** over
`GraphStateItem`s (e.g., uniform characteristic inspection, §18) without
requiring `select type` dispatch at every call site.

### 4.6.2 Structure

**Revised (this pass).** The original REQ-SI-002 gave `GraphStateItem` four
allocatable components, one of which (`esmf_route_handle`) was itself
defined (REQ-SI-002a) as a wrapper `ESMF_State`. Since the wrapper is
already physically an `ESMF_State`, a dedicated fourth component is
unnecessary: the RouteHandle case is represented by allocating the
*same* `esmf_state` component, distinguished from an ordinary nested
state by inspecting the wrapper's own sole member (see REQ-SI-002b).
This also formalizes, as a general mechanism, the informal
`Info`-tagging technique already used ad hoc for Bracket-vs-Vector
`FieldBundle`s and the `VerticalGrid`-vs-ordinary-nested-`State` case
(`17-open-questions.md` Q11 "general note", Q14; `13-geometry-and-
vertical-grids.md` §13.3.2, REQ-GEO-009) — see REQ-SI-002b.

**REQ-SI-002.** `GraphStateItem` MUST be a concrete (non-polymorphic,
non-extensible) derived type containing exactly these allocatable
components, of which **at most one MAY be allocated** at any time:

- `esmf_field` — `ESMF_Field`
- `esmf_field_bundle` — `ESMF_FieldBundle`
- `esmf_state` — `ESMF_State`

There is no separate `esmf_route_handle` component. A RouteHandle is
represented by allocating `esmf_state` in its RouteHandle-wrapper role
(REQ-SI-002a); see REQ-SI-002b for how this is distinguished from an
ordinary nested state.

**REQ-SI-002a.** `ESMF_RouteHandle` has no `ESMF_FieldEmptyReset`-style
in-place reset operation and no `Info` object (`11-revision-and-update.md`
§11.2), so a genuinely new `RouteHandle` requires destroy-then-create,
which invalidates any shallow copy of a bare handle
(`07-component-graph.md` REQ-CG-010). The RouteHandle-wrapper role of
`esmf_state` is therefore a persistent `ESMF_State` (which itself is
never destroyed/recreated) holding the current `RouteHandle` as its one
member; renewal replaces that member, not the wrapper. Consumers MUST
re-resolve the current `RouteHandle` via `ESMF_StateGet` at each use
rather than caching the raw handle across calls that might span a
renewal. See `07-component-graph.md` §7.4.1 for the full rationale,
which also covers the parallel (but simpler) Field/Geom case.

### 4.6.2a Two-tier classification: `esmf_kind()` and `variant()`

**REQ-SI-002b `[supersedes REQ-SI-003; formalizes the Q11/Q14 Info-tag
precedent]`.** `GraphStateItem` MUST expose two queries rather than one,
since a single "kind" conflates two independent questions ("which native
ESMF representation is populated" vs. "which role/use-case is this
instance of that representation playing"):

- `esmf_kind()` returns an `ESMF_StateItem_Flag` — ESMF's own native
  item-classification enumeration — reporting which of `esmf_field` /
  `esmf_field_bundle` / `esmf_state` is allocated, using the
  corresponding native values `ESMF_STATEITEM_FIELD` /
  `ESMF_STATEITEM_FIELDBUNDLE` / `ESMF_STATEITEM_STATE`, or
  `ESMF_STATEITEM_NOTFOUND` (ESMF's own "absent" sentinel, reused rather
  than inventing a bespoke invalid value) if none is allocated. **When
  `esmf_state` is allocated and playing the RouteHandle-wrapper role
  (REQ-SI-002a), `esmf_kind()` MUST report `ESMF_STATEITEM_ROUTEHANDLE`,
  not `ESMF_STATEITEM_STATE`** — determined by inspecting the wrapper's
  sole member via `ESMF_StateGet`, whose own returned item type is
  already `ESMF_STATEITEM_ROUTEHANDLE` for a RouteHandle member. This
  reuses ESMF's native classification for that one case rather than
  requiring a `GraphStateItem`-specific tag, which is exactly why no separate
  `esmf_route_handle` component is needed.
- `variant()` returns a `MAPL_StateItem_Flag` — a new, MAPL-defined,
  open-ended enumeration distinguishing roles that ESMF's own
  classification has no vocabulary for (e.g. an ordinary `FieldBundle`
  vs. a time-interpolation bracket vs. a vector quantity; an ordinary
  `Field` vs. a geometry proxy; an ordinary nested `State` vs. a
  `VerticalGrid`). `variant()` MUST return the distinguished
  `MAPL_STATEITEM_PLAIN` value when no more specific role applies (this
  is also the value for `esmf_kind() == ESMF_STATEITEM_ROUTEHANDLE`,
  since that case is already fully distinguished at the `esmf_kind()`
  tier and needs no further variant). `MAPL_StateItem_Flag` MUST be
  defined so that new variants can be added later without changing
  `GraphStateItem`'s structure (an open, extensible flag set, not a closed
  enumeration baked into `GraphStateItem` itself).

**REQ-SI-002c.** The variant reported by `variant()` MUST be backed by
`ESMF_Info` metadata attached to the allocated component's own ESMF
object (the Field's, FieldBundle's, or State's `Info`), under a reserved
MAPL-owned `Info` key — not by a `GraphStateItem`-side field, so that the tag
travels with the ESMF object itself, consistent with how
`ESMF_FIELDSTATUS_GRIDSET` already travels with a geometry-proxy Field
independent of `GraphStateItem`.

**REQ-SI-003 `[superseded by REQ-SI-002b]`.** The original single
`kind()` query (reporting which of four components was allocated) is
replaced by the two-tier `esmf_kind()`/`variant()` model above. Retained
here, marked superseded rather than deleted, for traceability — no
production code should implement a single combined `kind()` going
forward.

**REQ-SI-004.** It MUST be a defect (checked, not silently tolerated) for
more than one of the three components (REQ-SI-002) to be allocated
simultaneously.

### 4.6.3 Relationship to StateItemNode

**REQ-SI-005.** `StateItemNode`'s payload (REQ-NODE-003a) is
exactly one `GraphStateItem` — concrete, not polymorphic. This supersedes
REQ-VAL-001 for the handle kinds covered by `GraphStateItem`. See §3.1a.

### 4.6.4 Resolved: membership maps and geometry kinds

**REQ-SI-006 `[resolves 17-open-questions.md Q11 sub-item 1]`.** `GraphStateItem`
MUST carry two additional map-valued components alongside the three
handle components (REQ-SI-002), generalizing REQ-VAL-003–007's
member-name → `NodeId` maps to the `GraphStateItem` model:

- `field_bundle_members : map<string, NodeId>` — populated only when
  `esmf_kind() == ESMF_STATEITEM_FIELDBUNDLE`; empty otherwise.
- `state_members : map<string, NodeId>` — populated only when
  `esmf_kind() == ESMF_STATEITEM_STATE`; empty otherwise. Because the
  RouteHandle-wrapper role reports `esmf_kind() ==
  ESMF_STATEITEM_ROUTEHANDLE` (REQ-SI-002b), not `ESMF_STATEITEM_STATE`,
  this map is never populated for a RouteHandle-wrapper `GraphStateItem` —
  its one member is a raw `ESMF_RouteHandle`, not a graph-visible
  `NodeId`-addressable item, so it would never belong in this map
  anyway.

These MUST live on `GraphStateItem` itself, not in a side table keyed by
`NodeId` external to it. Rationale: membership is not a cross-network
concern (unlike port bindings, `10-transforms-and-ports.md` REQ-XFORM-005,
which genuinely differ per `DependencyNetwork` and were therefore put in
an external table for that reason) — one `FieldBundle`/`State`'s
membership is a single fact true everywhere it's referenced, so it belongs
on the object describing that fact, consistent with keeping `GraphStateItem` as
the one place all `StateItemNode` structure lives. Confirmed by the spec
author (`17-open-questions.md` Q11, sub-decision 1).

**Resolved.** `GraphStateItem` has no component corresponding to `GeomValue`
(§4.2a), by design rather than omission: geometry is represented via the
`esmf_field` component already in `GraphStateItem` (REQ-SI-002) — a
geometry-proxy `ESMF_Field` (REQ-GEO-003) held in
`ESMF_FIELDSTATUS_GRIDSET` (grid/mesh/locstream/xgrid associated, data
array not yet allocated), with `variant() == MAPL_STATEITEM_GEOM`
(REQ-SI-002b) distinguishing it from an ordinary data-bearing Field
(`variant() == MAPL_STATEITEM_PLAIN`). A `GeomCharacteristic`
(`18-state-item-characteristics.md` §18.2.1) is then a
`ReferenceCharacteristic` holding the `NodeId` of that ordinary
`StateItemNode` — **no bespoke geometry node kind is needed under the
`GraphStateItem` model.** `GeomValue` (§4.2a) remains the correct shape for the
*original* polymorphic-`GraphValue` reading (§4.1–§4.5), and is the
documented fallback if the `esmf_field`-based approach proves problematic
in practice.

This resolves one branch of `17-open-questions.md` Q11: under `GraphStateItem`,
`GeomValue` is not needed as a distinct node-payload kind for the
Grid/Mesh/LocStream/XGrid case specifically. The membership-map
sub-question (REQ-VAL-003–007) is resolved separately, above, as
REQ-SI-006.

**`VerticalGridValue` — resolved separately, as Q14, not the same way as
`GeomValue`.** `VerticalGrid` does NOT collapse into an `esmf_field`
(unlike Geom); it is instead represented as an `esmf_state`-kind
`GraphStateItem` (`esmf_kind() == ESMF_STATEITEM_STATE`, its `ESMF_State`
members are the physical-dimension coordinate-set Fields),
`variant() == MAPL_STATEITEM_VERTICALGRID` (REQ-SI-002b) distinguishing
it from an ordinary nested state (`MAPL_STATEITEM_PLAIN`) — see
`13-geometry-and-vertical-grids.md` REQ-GEO-009 and `17-open-questions.md`
Q14. This also requires no new `GraphStateItem` component: it uses the
existing `esmf_state` allocation, the same as any other nested-state
`GraphStateItem`, with its `state_members` map populated the same way as an
ordinary nested state (REQ-SI-006).
