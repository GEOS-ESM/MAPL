# 13. Geometry and Vertical Grids

Status: `[SETTLED]` core representation, including the corrected
`VerticalGrid` model (§13.3) and its `ESMF_State` representation (§13.3.2,
REQ-GEO-009, `17-open-questions.md` Q14); `[OPEN]` post-freeze
time-dependent renewal mechanics (§13.4, shared with `14-route-handles.md`),
exchange-component geometry (§13.1, REQ-GEO-002a, deferred).

**Implementation status (Phase 4e,
`openspec/changes/horizontal-geometry-graph-state-item`, landed):**
REQ-GEO-001/002/003 are implemented for the three single-source cases
(own/from-ancestor/from-child) and static (pre-freeze) geometry. Entirely
gated behind a new global `graph_native_enabled()` toggle
(`mapl_GraphMode_mod`, default off) — no real production run's behavior
changes. One deviation from this section's original wording, discovered
during implementation: REQ-GEO-002's three single-source cases are not
*resolved* by the new graph-native code — `GeometrySpec`/
`initialize_geom_a.F90`/`initialize_geom_b.F90` already resolve them,
entirely before `GraphBuilder.F90` ever runs. The new code (a dedicated
`GraphBuilder.F90` hook, `run_geometry_hook`, not the ordinary
`VariableSpec`-based `resolve_one` path) gives that already-resolved
outcome real graph structure — one `StateItemNode` per component plus a
cross-`ComponentGraph` dependency edge for the ancestor/child cases —
reusing the same `Characteristic`-based mismatch-detection and
extension-chain-delegation machinery `09-extension-reuse.md` already
established for `units`/vertical grid, and the same cross-boundary proxy
machinery ordinary `MatchConnection`s already use for the "from child"
pull direction (a new, symmetric helper for the "from parent" push
direction, which has no existing precedent to reuse). See that change's
design.md for the full rationale. REQ-GEO-002a and §13.4 remain deferred/
open as stated below.

## 13.1 Geometry as first-class GraphValue

**REQ-GEO-001.** Geometry MUST become first-class, via (at minimum)
`GeomValue` (`04-graph-value-hierarchy.md` §4.2a — covers `Grid`/`Mesh`/
`LocStream`/`XGrid` as one closed-variant type, correcting an earlier draft
that listed `GridValue`/`MeshValue`/`LocStreamValue` separately and omitted
`XGrid`), `VerticalGridValue`, and `RouteHandleValue`. This is intended to
replace or reduce today's special-cased geometry-inheritance logic.

**Amended (§4.6.4, §13.3.2).** Under the `GraphStateItem` amendment, geometry
that must be visible inside a `GraphStateItem`-bearing `StateItemNode` is
instead carried directly as an incomplete `esmf_field`
(`ESMF_FIELDSTATUS_GRIDSET`, no data array yet), not as a `GeomValue`; and
`VerticalGrid` is carried as an `esmf_state`-kind `GraphStateItem`, with
`variant() == MAPL_STATEITEM_VERTICALGRID` (REQ-GEO-009,
`04-graph-value-hierarchy.md` REQ-SI-002b), not as a `VerticalGridValue`. Both
`GeomValue` and `VerticalGridValue` remain correct for the original
polymorphic-`GraphValue` reading (§4.1–§4.5) and as documented fallbacks.
See `04-graph-value-hierarchy.md` §4.6.4, `13-geometry-and-vertical-grids.md`
§13.3.2, and `17-open-questions.md` Q11/Q14.

**REQ-GEO-002.** Ordinary graph connection rules (advertise → connect →
transform-if-needed, per `09-extension-reuse.md`) MUST resolve all of the
following without additional special-case code paths:

- a component providing its own geometry
- a component receiving geometry from an ancestor
- a component receiving geometry from a child
- a component using time-dependent geometry

**REQ-GEO-002a `[deferred from initial implementation]`.** Some
components (e.g. `SURF`) act as **exchange components**: they must see
geometry from their parent *and* from each of their children
simultaneously, in order to construct an `ESMF_XGrid` spanning both. This
is a distinct connectivity shape from the four bullets above (which are
each a single source of geometry) and is not yet covered by them. This
case MAY be deferred in the initial implementation, but MUST NOT be
assumed solved by REQ-GEO-002's ordinary single-source connection rules —
it needs its own explicit treatment (likely: a `GeomValue`/`esmf_field`
whose `XGrid` is itself derived from *multiple* geometry inputs, which is
a many-to-one shape ordinary Transform ports already support per
`10-transforms-and-ports.md` REQ-XFORM-002, but has not been worked
through for this specific case).

## 13.2 Geometry proxy fields

**REQ-GEO-003.** Because ESMF States cannot directly contain geometry
objects (REQ-ESMF-005), an `OuterComponent` MAY expose hidden proxy Fields
under reserved MAPL names to carry a geometry `GraphValue`'s ESMF
representation. User component states MUST remain clean of these proxies
— they are framework-internal and MUST NOT appear as ordinary advertised
items in user-facing documentation/introspection.

**Noted subtlety (probably not a major concern, flagged rather than
silently assumed away):** the proxy Field carrying a geometry has its own
`ESMF_Info` object, distinct from whatever `Info` may be attached to the
`Grid`/`Mesh`/`LocStream`/`XGrid` it holds. Any metadata scheme that
identifies *which* geometry kind or role a proxy Field represents (see the
`ESMF_Info`-based Bracket-vs-Vector disambiguation idea noted under
REQ-GEO-006/007 below) needs to pick, deliberately, whether it lives on the
Field's Info or the geometry object's own Info — these are not the same
namespace and do not automatically stay in sync.

## 13.3 Vertical grids `[revised — corrects a misunderstanding in the original REQ-GEO-004/006/007]`

**Correction, recorded:** the original text of this section modeled a
vertical grid as "one logical value with multiple coordinate *views*,
commonly edge pressures (`PLE`) and edge heights (`ZLE`)," with
`VerticalGridValue` referencing fixed `PLE`/`ZLE` members by name. This
mischaracterized the abstraction. The corrected model:

**REQ-GEO-004.** A `VerticalGrid` is an abstraction holding one or more
**coordinate sets**. Each coordinate set is itself an ordinary `ESMF_Field`
— generally a real `GraphStateItem` for some gridded component or extension,
not a synthetic framework-only object.

**REQ-GEO-004a.** Coordinate sets within a `VerticalGrid` are accessed by
**physical dimension** (e.g. `"pressure"`, `"height"`), not by a fixed pair
of names like `PLE`/`ZLE`. A simple vertical grid may have only a
`"level"` dimension. `PLE`/`ZLE`-style names, if used at all, are examples
of concrete coordinate-set identifiers under the `"pressure"`/`"height"`
dimensions respectively, not the fixed schema.

**REQ-GEO-004b.** A coordinate set's physical units are NOT stored
redundantly on the `VerticalGrid`; they are found **transitively** through
the units already carried by its associated `ESMF_Field` (i.e., ordinary
`PhysicalUnitsCharacteristic`, `18-state-item-characteristics.md` §18.2,
on that Field's `GraphStateItem`).

**REQ-GEO-005.** The `OuterComponent` MAY materialize vertical grids in a
reserved nested state, keyed by physical dimension rather than a fixed
`PLE`/`ZLE` pair, e.g.:

```
MAPL_VerticalGrids
  atmosphere_levels
    pressure   ! a coordinate-set Field, e.g. edge pressures
    height     ! a coordinate-set Field, e.g. edge heights
```

**REQ-GEO-006.** A user field's private MAPL Info metadata MAY identify its
associated vertical grid by name (a string key resolvable within
`MAPL_VerticalGrids`).

**REQ-GEO-007 `[superseded by REQ-GEO-009 — see §13.3.2]`.** The original
model here described a `VerticalGridValue` payload type referencing:

- one `StateItemNode` per available physical-dimension coordinate set
  (keyed by physical dimension, per REQ-GEO-004a — not a fixed `PLE`/`ZLE`
  pair)
- a horizontal-geometry `StateItemNode`

This is superseded (not extended) by REQ-GEO-009: there is no
`VerticalGridValue` payload type. A `VerticalGrid` is instead an
`esmf_state`-kind `GraphStateItem` whose `ESMF_State` members are the
physical-dimension coordinate-set Fields (each itself an ordinary
`GraphStateItem`/`StateItemNode`), addressed by member name = physical
dimension, with `variant() == MAPL_STATEITEM_VERTICALGRID` to identify it
as a `VerticalGrid`. A reference to
the associated horizontal geometry is carried the same way any other
cross-reference is under the `GraphStateItem` model — as a
`ReferenceCharacteristic` (`18-state-item-characteristics.md` §18.2.1) on
the relevant coordinate-set `GraphStateItem`(s), not as a bespoke field on a
`VerticalGridValue` that no longer exists.

### 13.3.1 Connecting mismatched vertical grids: dimension adaptability

**REQ-GEO-007a.** When connecting an export to an import with *different*
vertical grids, the specific coordinate system to adapt through MUST be
chosen by an explicit adaptability check, not assumed:

1. Compute the set of physical dimensions the export's `VerticalGrid` and
   the import's declared `VerticalGrid` requirement have in common.
2. If exactly **one** physical dimension overlaps, that dimension's
   coordinate set MUST be used for the adaptation.
3. If **zero** or **more than one** dimensions overlap, this MUST be
   treated as an error (no dimension: genuinely incompatible; more than
   one: ambiguous — the connection cannot silently guess).

**REQ-GEO-007b `[current scope, not a permanent restriction]`.** For now,
an import `GraphStateItem` is assumed to either (a) use the identical
`VerticalGrid` as its source (no adaptation needed), or (b) explicitly
declare a single vertical coordinate system it requires. The general case
of an import declaring multiple acceptable coordinate systems (requiring
REQ-GEO-007a's overlap logic to choose among more than one *viable*
candidate on the import side, not just the export side) is out of scope
for this pass.

### 13.3.2 Metadata scheme; VerticalGrid as an ESMF_State `[settled for VerticalGrid; open for Geom]`

In the legacy implementation, `ESMF_Info` metadata is used to disambiguate
cases where the same underlying ESMF type is used for more than one
logical purpose — e.g. a `TimeInterpolationBracket` and a vector quantity
are both represented as a plain `FieldBundle`, distinguished only by
`Info` metadata, not by type.

**REQ-GEO-009 (settled — `17-open-questions.md` Q14).**
`VerticalGrid` is represented as an `ESMF_State` — its members are the
physical-dimension coordinate-set Fields (REQ-GEO-004/004a), addressed by
member name = physical dimension. This requires **no structural change to
`GraphStateItem`**: a `VerticalGrid`-kind item allocates the existing
`esmf_state` component (`04-graph-value-hierarchy.md` REQ-SI-002) exactly
as any other nested-state `GraphStateItem` would (`esmf_kind() ==
ESMF_STATEITEM_STATE`), distinguished from an ordinary nested state via
`variant() == MAPL_STATEITEM_VERTICALGRID` — the formalized, typed form
of the `ESMF_Info`-tagging technique noted above for Bracket-vs-Vector,
per `04-graph-value-hierarchy.md` REQ-SI-002b — not via a new `GraphStateItem`
component or a bespoke `VerticalGridValue` payload type.
`VerticalGridValue` (§4.2, REQ-GEO-007) is accordingly superseded for
this purpose by "an `esmf_state`-kind `GraphStateItem` with `variant() ==
MAPL_STATEITEM_VERTICALGRID`."

**`[OPEN]`** Whether the same `Info`-tagging technique should be applied to
Geom (disambiguating geometry *roles* via `Info` metadata on the proxy
Field, REQ-GEO-003) remains open — Q14 settled the `VerticalGrid` case
specifically, not this Geom sub-question.

**REQ-GEO-008.** Vertical grids CAN themselves require extensions — e.g.,
coordinates established on horizontal geometry `H1` needing interpolation
onto `H2`. Vertical transforms MUST use these as **explicit graph
dependencies** (ordinary `TransformGraphNode` inputs/outputs per
`10-transforms-and-ports.md`), not as internally-hidden coordinate
couplers. This is a deliberate simplification vs. today's implicit
handling.

## 13.4 Time-dependent geometry `[OPEN — interacts with freeze rule]`

Time-dependent geometry causing "dependent RouteHandles to be recreated or
otherwise renewed lazily" is stated as a goal (`14-route-handles.md`
§14.4). Per `07-component-graph.md` REQ-CG-008, any such renewal after
graph freeze MUST be an in-place value/revision update on an existing node,
not new-node creation. Whether every realistic time-dependent-geometry
scenario can in fact be satisfied by in-place renewal (vs. genuinely
needing new nodes/edges, which the frozen-graph model currently forbids) is
**not yet verified against a concrete example** — flagged for follow-up
before implementation of time-dependent geometry support.

**Update:** the specific sub-question of whether an `ESMF_Field` can be
resized in place, under a stable handle/identity, when its geometry
changes, is now answered: **yes**, via `ESMF_FieldEmptyReset`. This
resolves the in-place-renewal feasibility question for the Field case
specifically. It does not resolve the same question for `RouteHandle`
recomputation (still open, §14.4), and it does not by itself address
*when* reallocation happens relative to the component code that expects
newly-sized storage — see `18-state-item-characteristics.md` §18.8 for the
eager/lazy split adopted for that timing question.
