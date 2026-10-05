## Context

See proposal.md - Why. **Revised after discovery during implementation
(see Decisions below, especially D0): the architecture described here
replaces an earlier draft of this document that assumed
`VerticalGrid`-item construction would wrap `OuterMetaComponent%
get_vertical_grid()`/legacy `ModelVerticalGrid`. That assumption was
wrong and is not used anywhere in this version.**

Relevant current state:

- `MAPL_StateItem_Flag` (`superstructure/generic/graph/MAPLStateItemFlag.F90`)
  already defines `MAPL_STATEITEM_VERTICALGRID`, and `GraphStateItem`
  (`GraphStateItem.F90`) already supports an `esmf_state`-kind item with a
  `state_members_map : name -> NodeId` (REQ-SI-006) — both landed with
  Phase 1–2, unused by any real code path before this change.
- `mapl_StateItemVariantInfo_mod%set_variant`/`get_variant` already have
  overloads for `ESMF_State` (not just `ESMF_Field`/`ESMF_FieldBundle`) —
  confirmed by inspection; no change needed there.
- **`composite-state-spec` (landed, `openspec/changes/archive/
  2026-09-18-composite-state-spec`) already does essentially all of the
  structural work this capability needs, with zero `StateRegistry`
  involvement.** A component declares a composite item via
  `VariableSpec%declare_member(name, member)` (`VariableSpec.F90:800`);
  `GraphBuilder.F90`'s `advertise_one` (line ~346) already detects a
  composite (`var_spec%get_member_names()%size() > 0`) and routes it
  through `mapl_CompositeStateMaterialization_mod%materialize_composite`
  (`CompositeStateMaterialization.F90`), which recursively builds one real
  `StateItemNode` per member (leaf or nested), with each nested level's
  `GraphStateItem` `state_members_map` populated via the existing
  `add_state_member` choke point — exactly REQ-GEO-004/004a/009's required
  shape (member name = physical dimension), already built, already
  tested, already in the ordinary `advertise_one` path every component
  goes through. **This change's structural task is reduced to: give this
  already-correct mechanism one new, optional piece of information — "tag
  this composite's materialized state with variant X instead of leaving
  it plain `STATE`."**
- `VariableSpec` already has a precedent for exactly this kind of
  addition: `callback_interface_id` (`VariableSpec.F90:185`) "marks an
  import as a callback consumer... GraphBuilder branches on this field's
  validity... mirrors `MAPL_STATEITEM_SERVICE`'s own 'mark the item, not
  the connection' precedent without overloading `itemType` itself... set
  via plain field assignment only, no `make_VariableSpec` keyword." This
  change's new `state_item_variant` field follows that precedent exactly.
- **`GraphStateItem` has no persisted characteristics map.** `build_characteristics`
  (`GraphBuilder.F90:793`) builds a `CharacteristicMap` *transiently*,
  from one `VariableSpec`'s own declared fields, used only within one
  `resolve_one` call (compared via `find_mismatched_characteristics`,
  then discarded) — it is never stored on the resulting `StateItemNode`/
  `GraphStateItem`. A persisted `characteristics` map on `GraphStateItem`
  is `18-state-item-characteristics.md` REQ-CHAR-007, explicitly
  `[SPECULATIVE]`/Phase 5, not built. This matters for D3 below.
- **`VerticalGridCharacteristic`/legacy `VerticalGridAspect`'s
  `get_coordinate_field()` must not be used.** Confirmed by tracing:
  `ModelVerticalGrid%get_coordinate_field` routes through
  `StateRegistry%extend()` (`StateRegistry_Extensions_smod.F90:191`),
  which can call `StateItemSpec%make_extension()`, allocate a new
  `ExtensionTransform`, build a real `ESMF_GridComp` coupler
  (`make_coupler`), and register a new consumer — real `StateRegistry`
  mutation, not a pure read. Per explicit project direction: **the graph
  solution must not use `StateRegistry`; legacy's aspect/extension
  machinery is at most suggestive of what capability is needed (e.g.
  `find_common_physical_dimension`, `VerticalRegridTransform`), never
  something the graph code calls into.** This is why `VerticalGridItem.F90`/
  `run_vertical_grid_hook` (the original D1/D2) are gone from this design:
  there is no safe, `StateRegistry`-free way to ask legacy `VerticalGrid`
  for a coordinate `ESMF_Field` on demand, and there doesn't need to be —
  composite declaration (above) already provides a fully graph-native
  path to the same structure.
- `VerticalGridCharacteristic` (`VerticalGridCharacteristic.F90`) is
  the correct home for REQ-GEO-007a's dimension-overlap classification
  (see "Unification" below and D3) — not a separate characteristic, as
  this document originally planned. It is already live in
  `build_characteristics` for ordinary per-Field vertical-grid identity
  matching via `var_spec%vertical_grid%get_id()` (a pure accessor on the
  legacy abstract `VerticalGrid` base type's own private integer field —
  no `StateRegistry` touched by `get_id()` itself).
- `VerticalGrid%get_supported_physical_dimensions()` is **also** a pure
  accessor (confirmed by inspection of `ModelVerticalGrid`/
  `BasicVerticalGrid` — it returns an already-held `StringVector`, no
  `StateRegistry` call) — unlike `get_coordinate_field()`. This matters
  for the Unification below: it means the ordinary-Field path can supply
  dimension information to `VerticalGridCharacteristic` just as cheaply
  as the composite path can.
- Legacy `VerticalGridAspect`/`make_transform.F90` already implements real
  dimension-overlap-based vertical regridding
  (`find_common_physical_dimension`, `VerticalRegridTransform`) — a
  first-match (not ambiguity-checked) version of REQ-GEO-007a. Per
  explicit project direction, this change must not call into it, but a
  later change building a real graph-native regrid `Transform` is
  expected to reuse its low-level numerical logic (interpolation,
  staggering, normalization) where that logic is separable from
  `StateRegistry`-coupled orchestration — not attempted in this change.

### Unification (post-implementation revision)

The first implementation of this change added a brand-new, independent
`VerticalGridMembershipCharacteristic`/`VERTICAL_GRID_MEMBERSHIP_CHARACTERISTIC_ID`,
living alongside the pre-existing `VerticalGridCharacteristic`
(following an explicit earlier instruction in this session to leave
`VerticalGridCharacteristic` untouched and add the new logic
separately). On review after implementation, this was judged
architecturally wrong, for two reasons:

1. Two separate "vertical grid" `Characteristic` kinds, one doing
   real-identity comparison and one doing name-set comparison, is
   confusing — there is no good reason a reader of this code should need
   to know both exist and which one applies where.
2. More importantly: matching by dimension-*name* overlap **alone**,
   with no identity check backing it, is a real correctness risk, not
   just a documented limitation — two components that happen to declare
   the same physical-dimension names (`"pressure"`, `"height"`) for
   genuinely unrelated vertical grids would be reported as "matching"
   and wired directly, with no mismatch ever detected.

The corrected design, described in D2/D3 below, folds the dimension-set
information directly into the existing `VerticalGridCharacteristic`:
identity-token comparison remains the authoritative exact-match check
when both sides have one (ordinary Fields), and dimension-set comparison
is only ever the *fallback* used when no identity token is available at
all (the composite case) — not a competing, independently-invoked
mechanism.



## Goals / Non-Goals

**Goals:**
- Give `VerticalGrid` real graph structure (REQ-GEO-004/004a/009) by
  teaching the *already-shipped* composite-declaration path one new,
  optional piece of information, not by adding a new construction path.
- Add REQ-GEO-007a's dimension-overlap mismatch classification to the
  **existing** `VerticalGridCharacteristic`, exercised through the
  *already-shipped* ordinary connection-resolution path (`resolve_one`),
  not a new hook and not a new `Characteristic` kind.
- Zero `StateRegistry` involvement anywhere in this change's own code.
- Zero change to legacy `VerticalGridSpec`/`VerticalGridManager`/
  `ModelVerticalGrid`/`OuterMetaComponent%get_vertical_grid()`.

**Non-Goals:**
- REQ-GEO-007b's general case (import declaring multiple acceptable
  coordinate systems) — out of scope.
- §13.4 (time-dependent renewal under freeze) — out of scope.
- A real, executing vertical-regrid `Transform` — `build_transform`
  fails explicitly (structure/detection now, execution later, same
  posture as `UnitsCharacteristic`'s siblings). A follow-up change may
  reuse legacy's low-level regrid numerics (Context) but that is not
  attempted here.
- Any reserved, framework-materialized `MAPL_VerticalGrids` nested state
  (REQ-GEO-005's "MAY materialize") — a component declares its vertical
  grid composite under whatever `short_name` it chooses; no reserved-name
  convention is introduced by this change.
- A persisted, inspectable per-item characteristics map on `GraphStateItem`
  (spec 18's REQ-CHAR-007) — explicitly Phase 5; see D4 for how the
  geometry-reference requirement is satisfied without it.
- Deriving a coordinate-set item's declared units/typekind/etc. — those
  are ordinary leaf-member declarations, already covered by existing
  `VariableSpec`/`build_characteristics` behavior, untouched by this
  change.

## Decisions

### D0: Superseded — no `OuterMetaComponent`/`ModelVerticalGrid`-based construction
The original plan (a `VerticalGridItem.F90` constructor wrapping
`OuterMetaComponent%get_vertical_grid()`/`get_coordinate_field()`, plus a
`run_vertical_grid_hook` in `GraphBuilder.F90`) is abandoned. Two
independent findings, both discovered mid-implementation (Context):
`get_coordinate_field()` is not a pure getter (it mutates `StateRegistry`
via `extend()`), and — independently — it is unnecessary, because
composite declaration (D1) already provides a `StateRegistry`-free path
to the same structure. Recorded here so this reversal is not
re-attempted without re-reading Context's trace of `extend()`/
`make_extension()`.

### D1: VerticalGrid structure comes from ordinary composite declaration, not a new constructor
A component declares its vertical grid exactly like any other composite
item: one top-level `VariableSpec` (export, chosen `short_name`), with
one `declare_member(dimension_name, coordinate_field_spec)` call per
physical dimension it supports — ordinary leaf `VariableSpec`s,
declaring their own units/typekind the normal way. `advertise_one`/
`materialize_composite` (Context) already turn this into one real
`StateItemNode` per dimension plus one grouping `esmf_state`-kind
`GraphStateItem` whose `state_members_map` is keyed by dimension name —
REQ-GEO-004/004a satisfied with **zero new production code** beyond D2's
one-line tagging addition. No new module (`VerticalGridItem.F90`) is
added.

**Alternative considered:** a dedicated constructor module building the
`esmf_state`/`state_members_map` directly (the original D1). Rejected as
unnecessary duplication once composite declaration was confirmed to
already build the identical shape — the only genuinely missing piece is
the variant tag (D2).

### D2: A new `VariableSpec%state_item_variant` field, mirroring `callback_interface_id`'s precedent
Add `type(MAPL_StateItem_Flag), allocatable :: state_item_variant` to
`VariableSpec` (`VariableSpec.F90`), unallocated by default (inert for
every existing declaration — no toggle needed, see Risks). A component
marks its vertical-grid composite by plain field assignment:
`var_spec%state_item_variant = MAPL_STATEITEM_VERTICALGRID`, exactly
`callback_interface_id`'s own "set via plain field assignment only, no
`make_VariableSpec` keyword" precedent (Context) — chosen specifically
because it generalizes beyond this one use (any future composite role
reuses the same field) rather than adding a single-purpose boolean.

`mapl_CompositeStateMaterialization_mod%materialize_composite` gains one
new step: after `ESMF_StateCreate` and before `payload%set(state, rc)`,
if `var_spec%state_item_variant` is allocated, call
`set_variant(state, var_spec%state_item_variant, rc)` — reusing the
existing `set_variant`/`get_variant` overload for `ESMF_State`
(Context) unmodified. REQ-GEO-009's `variant() == MAPL_STATEITEM_VERTICALGRID`
now holds for exactly the composites that opt in, with no change to any
other composite's behavior (default unallocated → `variant()` falls back
to plain `MAPL_STATEITEM_STATE`, `GraphStateItem.F90`'s own existing
default-case behavior, untouched).

**Alternative considered:** a polymorphic "role" class hierarchy on
`VariableSpec` (closer to legacy `ClassAspect`'s shape). Rejected (user
direction): no second composite role is anticipated yet; a flag field
reusing the already-existing `MAPL_StateItem_Flag` enum is simpler and
sufficient, matching `GraphStateItem`'s own two-tier `itemType()`/
`variant()` split one level earlier (declaration time).

### D3: REQ-GEO-007a lives inside the existing `VerticalGridCharacteristic`, not a separate kind
`VerticalGridCharacteristic` (`VerticalGridCharacteristic.F90`) gains:

- A new optional field, `dimensions : type(StringVector)` — the
  declared set of physical-dimension names — alongside its existing
  `grid_id : character(:), allocatable` identity token. Both are now
  independently optional in the constructor (`grid_id` was already an
  allocatable component; it is simply no longer a mandatory constructor
  argument). A `MAPL_STATEITEM_VERTICALGRID`-tagged composite is
  constructed with `dimensions` only (built directly from
  `var_spec%get_member_names()` — REQ-GEO-004a's "member name = physical
  dimension" means no separate dimension-set storage is needed). An
  ordinary Field's `vertical_grid` aspect is constructed with **both**
  `grid_id` (unchanged, rendered from `VerticalGrid%get_id()`) and
  `dimensions` (new — from `VerticalGrid%get_supported_physical_dimensions()`,
  confirmed pure, Context).
- `needs_extension_for(goal)`: when **both** sides have `grid_id`
  allocated, that comparison is authoritative (`this%grid_id /=
  goal%grid_id`) — byte-for-byte the pre-existing behavior for ordinary
  Field-to-Field connections. When **either** side lacks one (always
  true for the composite path, since it never has one), falls back to
  dimension-set equality (`same_set`) — the structural, name-only
  definition of "match" available when no real identity exists (Risks:
  weaker than a true grid-identity check, but this is now explicitly the
  fallback path, not a competing primary mechanism).
- `build_transform(...)`: called only on mismatch (existing
  `find_or_build_extension_chain` dispatch, `ExtensionResolution.F90`,
  unmodified) — implements the three-way REQ-GEO-007a outcome from
  `dimensions` on **both** sides, regardless of which comparison path
  (`grid_id` or dimension-set) detected the mismatch:
  - exactly one overlapping dimension → `_FAIL` naming that dimension as
    the identified (but not-yet-implemented) adaptation candidate.
  - zero overlapping dimensions → `_FAIL` with an explicit "incompatible"
    message.
  - more than one overlapping dimension → `_FAIL` with an explicit
    "ambiguous" message, naming the overlapping dimensions.
  No real transform is allocated in any case (Non-Goals) — this change
  makes the three outcomes distinguishable in the failure diagnostic,
  not working regrid execution. Because dimensions are now populated for
  the ordinary-Field path too, this classified diagnostic applies there
  as well, not only to the new composite path — a behavior change from
  the pre-existing code's one flat message, requiring one existing test
  (`test_materialize_extensions_vgrid_mismatch_unaffected`) to be updated
  to the new, more specific expected message.
- `get_signature()`: extended to append the sorted, joined dimension
  list when `dimensions` is non-empty, so two otherwise-identical-
  looking characteristics that differ only in dimensions get distinct
  `ExtensionResolution.F90` chain-reuse cache keys.

Exercised automatically: when a `MatchConnection` pairs an export
`VerticalGrid`-tagged composite against an import `VerticalGrid`-tagged
composite of the same `short_name` (or an ordinary mismatched Field
connection), `resolve_one`'s existing `build_characteristics`/
`find_mismatched_characteristics`/`find_or_build_extension_chain`
sequence (`GraphBuilder.F90:689-782`, unmodified) already runs this
`Characteristic` — no new `GraphBuilder` hook needed.

**Alternatives considered:**
- The original D3 (a standalone `classify_vertical_grid_mismatch` helper
  function, called explicitly from `GraphBuilder.F90`). Superseded for
  the same reason as the next alternative below — once the
  classification lives inside a `Characteristic`'s own `build_transform`
  (which already has the right signature shape for a three-way outcome
  via its own `_FAIL` messaging), a separate helper and a new call site
  are unneeded machinery.
- A second, independent `Characteristic` kind
  (`VerticalGridMembershipCharacteristic`/
  `VERTICAL_GRID_MEMBERSHIP_CHARACTERISTIC_ID`) living alongside
  `VerticalGridCharacteristic`. This was the first implementation of
  this change, built and fully tested, then reverted after review (see
  Context "Unification") — two separate "vertical grid" concepts was
  judged confusing, and comparing by dimension-name overlap with no
  identity-token fallback at all (the standalone kind's only
  comparison) was a real false-positive risk, not merely a documented
  one. Folding dimensions into the existing characteristic, with
  identity-token comparison as the authoritative check whenever
  available, resolves both problems.

### D4: Geometry-reference requirement is satisfied by shared `ComponentGraph` ownership, not a new per-item reference
The capability spec's original wording ("each coordinate-set item
carries a reference... inspectable from that item") assumed a persisted
per-item characteristics map, which does not exist (Context) and is
explicitly Phase 5 to build. **Revised, buildable equivalent:** a
coordinate-set item materialized by D1 is always registered into the
*same* `ComponentGraph` as that component's own horizontal-geometry item
(`graphbuilder_advertise_geometry`, Phase 4e — both always operate on
`this%get_component_graph()`). The association is therefore already
real and already queryable today, with no new code: given any
coordinate-set `NodeId`, the owning `ComponentGraph`'s existing
`get_resource_index(item_key(EXPORT, GEOMETRY_ITEM_NAME))` (Phase 4e,
public) resolves to that component's geometry item. This change adds a
test confirming that relationship holds for a vertical-grid composite's
members specifically; it does not add a new reference mechanism. The
capability spec (specs/graph/vertical-grid/spec.md) is revised to state
this accurately instead of describing a per-item characteristic that
would require spec 18.

**Trade-off, accepted:** this only identifies "which component's
geometry" a coordinate set belongs to, via graph topology — it is not a
portable, per-item token that survives being referenced from a different
`ComponentGraph` (e.g. after a cross-boundary proxy is created). Given
coordinate sets are not inherited across component boundaries the way
horizontal geometry itself is (REQ-GEO-002's own/ancestor/child cases
are specific to geometry), this is sufficient for this change's scope.
A true portable reference remains Phase 5 (spec 18) business.

## Risks / Trade-offs

- [Risk] `VerticalGridCharacteristic`'s dimension-set fallback match (D3)
  is structural (dimension-name-set equality) only — two composites
  could declare the same dimension names while meaning physically
  different grids (different levels/spacing). → Mitigation: this is
  strictly the *fallback* path, used only when no identity token exists
  at all (the composite case has none to compare); ordinary Fields keep
  their existing, stronger identity-token check unconditionally.
  Documented as a known limitation of the composite case specifically,
  not silently assumed correct.
- [Risk] No existing precedent for a `Characteristic` whose comparison
  input (dimension names) is derived from `get_member_names()` rather
  than a single declared scalar field. → Mitigation: `get_member_names()`
  is already public, already used by `advertise_one`/
  `materialize_composite` for the identical purpose; no new
  `VariableSpec` accessor needed.
- [Trade-off] No reserved `MAPL_VerticalGrids` materialization convention
  (REQ-GEO-005, Non-Goals) — two independently-developed components could
  choose colliding `short_name`s for their own vertical-grid composites
  with no framework-level namespacing. Accepted: REQ-GEO-005 is phrased
  as "MAY," not required, and ordinary short-name collision is already a
  general concern ordinary item declaration already has to manage.
- [Trade-off] D4's geometry association is topology-based (same
  `ComponentGraph`), not a portable per-item token (see D4's own
  trade-off note above).
- [Trade-off] No `graph_native_enabled()` gating (unlike Phase 4e). This
  change's two write-paths (`state_item_variant` field,
  `VerticalGridCharacteristic`'s new dimension-only construction branch
  in `build_characteristics`) are both conditioned on a field/branch
  that no pre-existing `VariableSpec` declaration sets — inert for every
  current configuration without needing an explicit toggle. Accepted as
  simpler than Phase 4e's posture, appropriate because this change,
  unlike 4e, adds no code that runs unconditionally for every component.
- [Trade-off] Populating `dimensions` on the ordinary-Field path (D3)
  changes an existing mismatch diagnostic's exact message text (from one
  flat failure to the REQ-GEO-007a-classified one). Accepted
  deliberately (user direction) as a net diagnostic improvement with no
  functional behavior change (both the old and new code fail in exactly
  the same mismatch cases) — one pre-existing test's expected string was
  updated to match.

## Open Questions

None — REQ-GEO-005/007b, §13.4, real vertical-regrid execution, and
spec 18's full hierarchy are explicit Non-Goals, and D0–D4 above
(including D3's post-implementation unification) resolve every
technical choice this change's own (revised) scope requires.
