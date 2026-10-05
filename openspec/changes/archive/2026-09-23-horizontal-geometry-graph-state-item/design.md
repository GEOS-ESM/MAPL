## Context

See proposal.md - Why. Relevant current state:

- `MAPL_StateItem_Flag` (`superstructure/generic/graph/MAPLStateItemFlag.F90`)
  already defines `MAPL_STATEITEM_GEOM` as a field-native-kind variant, and
  `mapl_StateItemVariantInfo_mod` already provides `set_variant`/`get_variant`
  on a field's own `ESMF_Info` — both landed with Phase 1–2, unused by any
  real code path before this change.
- `GraphStateItem` (`GraphStateItem.F90`) already tolerates a field-typed
  item tagged with any variant with no structural change (`state-item`
  capability, "No dedicated geometry node kind" scenario) — this change is
  a consumer of that structure, not a modification of it.
- **Geometry connectivity is already fully resolved before this change ever
  runs.** `GeometrySpec` (`superstructure/generic/specs/GeometrySpec.F90`:
  `GEOMETRY_PROVIDER`/`GEOMETRY_FROM_PARENT` [default]/`GEOMETRY_FROM_CHILD`)
  drives `initialize_geom_a.F90`/`initialize_geom_b.F90`
  (`propagate_geom_to_children.F90`), which run in `GenericPhases.F90`'s
  fixed phases 3-4 — **entirely before** `GENERIC_INIT_ADVERTISE` (phase 5),
  and hierarchy-wide (every component finishes phase 3, then every
  component finishes phase 4, before any component starts phase 5). By the
  time anything in `GraphBuilder.F90` runs, `this%get_geom()`/
  `this%get_geom_id()` already reflect the correct outcome for all three
  REQ-GEO-002 single-source cases. This was discovered mid-implementation
  and materially changed this change's own design (see D3 below) — the
  original plan assumed geometry connectivity needed to be *resolved* by
  new graph-native logic; it only needs to be *represented*.
- `OuterMetaComponent` already owns `has_geom()`/`get_geom()`/`set_geom()`
  (this change adds `get_geom_id()` alongside them, D2) and
  `get_child_component_graph()`/`get_child_component_spec()`/
  `get_child_outer_meta()` — a framework-internal carve-out (REQ-GB-002)
  `GraphBuilder.F90` already uses for ordinary cross-boundary
  `MatchConnection` resolution (`get_or_make_local_node_id`).
- `initialize_advertise.F90` recurses into every child's own
  `INIT_ADVERTISE` (hook included) **before** running the parent's own
  advertise/hook step — bottom-up completion, self last. This ordering
  guarantee is what makes the cross-boundary wiring in D3 correct without
  needing a separate synchronization mechanism.
- `VerticalGridCharacteristic` (`superstructure/generic/graph/
  VerticalGridCharacteristic.F90`) is the direct precedent this change's
  `GeomCharacteristic` follows: an opaque identity-token comparison
  (`needs_extension_for`) plus a `build_transform` that fails explicitly
  because no real vertical-regrid provider exists yet.
- `mapl_GeomId_mod`'s `GeomId` (`infrastructure/geom/GeomId.F90`, a
  monotonic-counter-backed value type) and `mapl_GeomUtilities_mod`'s
  `GeomGetId`/`GeomSetId`/`SameGeom` are the existing identity machinery
  legacy `GeomAspect` already uses. This change reuses `GeomId` as
  `GeomCharacteristic`'s opaque token but does **not** read/write it via
  the geom's own `ESMF_Info` anywhere in production code — see D2.
- Introduced mid-implementation, at the user's explicit request: a single
  global `graph_native_enabled()` toggle (`mapl_GraphMode_mod`), mirroring
  `mapl_ExtensionResolution_mod`'s existing `materialize_extensions`
  precedent, anticipating that graph-native code making real decisions
  will increasingly conflict with the legacy `OuterMetaComponent`/
  `StateRegistry` layer it runs alongside (see D6).

## Goals / Non-Goals

**Goals:**
- Make horizontal geometry a real `GraphStateItem`: one per component,
  wrapping whatever `GeometrySpec` already resolved `this%get_geom()` to.
- Give the three REQ-GEO-002 single-source relationships (own geometry,
  from ancestor, from child) real graph structure — dependency edges
  between real `StateItemNode`s, including cross-`ComponentGraph`
  boundaries — not merely per-component representation.
- Use the same `Characteristic`/mismatch-detection/extension-chain-
  delegation machinery 3c already established for `units`/`vertical_grid`
  for geometry's own mismatch case, rather than inventing a parallel
  comparison mechanism.
- Introduce the legacy/graph-native mode toggle as reusable, general
  infrastructure (not geometry-specific), while scoping this change's own
  use of it to gating the new geometry hook's execution.
- Keep every change additive and default-off: no real production run's
  behavior changes unless `set_graph_native_enabled(.true.)` is called.

**Non-Goals:**
- REQ-GEO-002a (exchange-component, multi-source `XGrid` geometry, e.g.
  `SURF`) — different connectivity shape (many-to-one, not single-source),
  explicitly deferred by the roadmap and by proposal.md.
- §13.4 (time-dependent geometry renewal under freeze) — explicitly
  deferred; this change only wires geometry at (pre-freeze) advertise time.
- A real, executing geometry regrid provider
  (`GeomCharacteristic%build_transform`). Same "structure now, real
  execution later" posture 3c shipped for `units`, and
  `VerticalGridCharacteristic` still ships for vertical grids.
- **Re-deriving geometry connectivity.** This change does not replace, and
  does not duplicate, `GeometrySpec`/`initialize_geom_a.F90`/
  `initialize_geom_b.F90`'s own resolution logic — it is purely additive
  structure on top of an already-correct answer. Making the graph-native
  representation *drive* real behavior (e.g. having `get_geom()` itself
  become graph-aware, or bypassing `initialize_geom_a`/`b` when graph-native
  mode is enabled) is explicitly out of scope for this change; the new
  toggle (D6) exists so that work has a place to attach later.
- Any change to legacy `GeomAspect`/`StateRegistry` grid-inheritance code.
- Any change to `GraphStateItem`'s own structure or its variant-tagging
  mechanism.
- A `VariableSpec`-based advertise path for the geometry item (the
  original plan, D3 below explains why it was abandoned).

## Decisions

### D1: Geometry-proxy construction is a new, narrow module, decoupled from `GeomId`
`mapl_GeomProxyField_mod` (`GeomProxyField.F90`) owns exactly:
`ESMF_FieldEmptyCreate` → `ESMF_FieldEmptySet(field, geom=geom)` (leaves
the field in `ESMF_FIELDSTATUS_GRIDSET` — grid assigned, no data array) →
`set_variant(..., MAPL_STATEITEM_GEOM)` → wrap via `GraphStateItem`'s
existing `set_field`. It does **not** touch `GeomId` at all — this is a
change from the original plan (which had this module ensure a `GeomId`
was present, mirroring `GeomAspect`). Identity is threaded separately
(D2), so the constructor's only job is wrapping the field.

**Alternative considered:** add a `GraphStateItem` constructor overload
that special-cases geometry directly. Rejected — `GraphStateItem` is
deliberately variant-agnostic; adding a geometry-aware constructor there
would violate that separation for no benefit.

### D2: Geometry identity flows through `OuterMetaComponent%get_geom_id()`, never through the geom's own `ESMF_Info`
Discovered mid-implementation: `OuterMetaComponent%set_geom`'s own
fallback (mint a fresh `GeomId` via the global counter when
`MAPL_GeomGetId` finds nothing already attached) does **not** write that
freshly-minted id back onto the geom's own `ESMF_Info` — it only stores it
in `this%geom_id` (a private field). Legacy code that needs two components
to agree on one id (`propagate_geom_to_children.F90`,
`initialize_geom_a.F90`) works around this via same-module private-field
assignment (`child_meta%geom_id = this%geom_id`), not through `set_geom`'s
own public fallback path.

Consequently, `GraphBuilder.F90`'s geometry hook reads identity via the
new `OuterMetaComponent%get_geom_id()` accessor directly at each call
site (mirroring exactly how `build_characteristics` already reads
`var_spec%vertical_grid%get_id()` for `VerticalGridCharacteristic` — an
in-memory value read, not a re-derivation from ESMF metadata) rather than
via `MAPL_GeomGetId` on the geom object. This is simpler than the
original plan and sidesteps the fallback-path gap entirely: whatever
`this%geom_id` already is (assigned by `GeometrySpec` resolution,
long before this hook runs) is authoritative.

**Consequence for testing:** a test driving `OuterMetaComponent%set_geom`
directly (bypassing `initialize_geom_a`/`b`, which have same-module access
this change's own tests do not) must pre-tag the geom's `ESMF_Info` itself
(via `mapl_GeomUtilities_mod%GeomSetId`) before calling `set_geom` on more
than one component that should agree on one id — otherwise each `set_geom`
call takes the "no tag found" fallback branch independently and mints its
own, different id. Documented in `Test_GraphGeometryHook.pf`.

### D3: No `VariableSpec`; a dedicated `GraphBuilder` hook driven directly by `GeometrySpec`/`OuterMetaComponent`
The original plan (declare the geometry proxy as an ordinary reserved-name
`VariableSpec` so it flows through `resolve_one`/`build_characteristics`
unmodified) is abandoned. Two independent findings, both discovered
mid-implementation:

1. **It doesn't work.** `initialize_advertise.F90`'s `self_advertise`
   walks `ComponentSpec%var_specs` and feeds every entry to
   `this%registry`/`StateRegistry%add_to_states`. A reserved-name
   `VariableSpec` inserted there to make `GraphBuilder`'s *own*
   `var_specs` walk see it would *also* flow through that legacy path and
   leak into real user-facing import/export states — a direct REQ-GEO-003
   violation, not a hypothetical risk.
2. **It isn't needed.** `GeometrySpec`/`initialize_geom_a.F90`/
   `initialize_geom_b.F90` already fully resolve own/from-parent/
   from-child, in phases that complete hierarchy-wide before
   `GENERIC_INIT_ADVERTISE` ever starts (Context). There is no
   "connection" left for `resolve_one`'s exact-name-match machinery to
   discover — the decision was already made. Representing it in the graph
   is a separate, additive concern from making it.

Replacement design: `GraphBuilder.F90` gains a self-contained hook,
`run_geometry_hook(this)`, calling two new private procedures:

- `graphbuilder_advertise_geometry(this)`: if `this%has_geom()`, wraps
  `this%get_geom()` via `new_geom_proxy_item` and registers it as an
  ordinary `StateItemNode`, indexed under a new reserved key
  (`item_key(EXPORT, GEOMETRY_ITEM_NAME)`, `GEOMETRY_ITEM_NAME =
  'MAPL_Geometry'`, now public) using the *same* `item_key`/resource-index
  machinery ordinary items use — but never touching `ComponentSpec%
  var_specs`, so it is structurally unreachable from
  `self_advertise`/`add_to_states` (this is what makes the original
  plan's REQ-GEO-003 problem moot here: there is no `VariableSpec` to
  leak).
- `graphbuilder_resolve_geometry(this)`: gives the *already-resolved*
  parent/child relationship (read directly from
  `this%get_component_spec()%geometry_spec%kind` and, for each child,
  `this%get_child_component_spec(name)%geometry_spec%kind`) a real
  dependency edge:
  - **Pull** (`GEOMETRY_FROM_CHILD`, `resolve_geometry_from_child`):
    reuses `get_or_make_local_node_id()` **completely unmodified** —
    structurally identical to an ordinary cross-boundary `MatchConnection`
    pulling a named child's item into the parent's own graph.
  - **Push** (`GEOMETRY_FROM_PARENT`, the default, `push_geometry_to_child`):
    no existing machinery pulls in this direction (ordinary connections
    are always resolved "one level up," never pushed down), so this is a
    new, symmetric helper: it injects a proxy node directly into the
    *child's* own graph (via the existing `get_child_component_graph()`
    carve-out, REQ-GB-002 — the same one `get_or_make_local_node_id`
    already uses for the pull direction), keyed by a new public
    `parent_geometry_proxy_key()`.
  - Both directions build a `GeomCharacteristic` (D4) from each side's
    `get_geom_id()` and run `find_mismatched_characteristics` — matching
    wires a direct dependency edge; a mismatch is asserted as
    unreachable-given-`GeometrySpec`'s-own-exclusive-resolution (a
    defensive check, not a normal outcome for these two closed-set cases
    — see D4).
- `run_geometry_hook(this)` calls `advertise_geometry` then
  `resolve_geometry`, and is itself gated behind `graph_native_enabled()`
  (D6). Invoked from `initialize_advertise.F90` right after the existing
  `run_advertise_hook(this)` call. Correctness for the push direction
  depends on `initialize_advertise.F90`'s existing bottom-up recursion
  (children complete their own `INIT_ADVERTISE`, hook included, before the
  parent's runs) — by the time a parent's hook runs, every child's own
  geometry node (if it has one) already exists, satisfying both the pull
  direction's `get_or_make_local_node_id` existence assertion and the push
  direction's own existence assertion on the child side.

### D4: `GeomCharacteristic`, built the same way `VerticalGridCharacteristic` was
Add `GEOM_CHARACTERISTIC_ID` to `mapl_CharacteristicId_mod` and
`mapl_GeomCharacteristic_mod`, structured identically to
`VerticalGridCharacteristic`: an opaque identity token (`GeomId%
get_value()` rendered to text — the token is supplied by the *caller*
at each `GraphBuilder.F90` call site via `get_geom_id()`, per D2; this
type itself does not know about `OuterMetaComponent` or `GeomId`
directly, matching `VerticalGridCharacteristic`'s own independence from
`VerticalGrid`); `needs_extension_for` reports a mismatch when tokens
differ (`class default` reports "needs extension" defensively);
`build_transform` fails explicitly (`_FAIL`) — no real horizontal-regrid
provider ships in this change, mirroring `vgrid_build_transform`'s own
body exactly.

Unlike the original plan, `GeomCharacteristic` is **not** wired into
`GraphBuilder.F90`'s `build_characteristics`/`resolve_one` (there is no
geometry `VariableSpec` for that function to read, per D3) — it is
constructed directly, inline, at each of `resolve_geometry_from_child`'s
and `push_geometry_to_child`'s own call sites, and compared via
`find_mismatched_characteristics` (reused, not reimplemented) exactly the
way `resolve_one` already does for `units`/`vertical_grid`.

**On mismatch being a defensive check, not a normal path:** because
`GeometrySpec`'s three kinds are mutually exclusive at declaration time
(a component is a provider, or names one child, or defers to its parent —
never more than one), a genuine mismatch between a component's own
`get_geom_id()` and its resolved parent's/child's `get_geom_id()` should
be structurally impossible in a correctly-functioning system. The check
exists anyway (asserted, not assumed) so a latent bug in `GeometrySpec`
resolution — or a future change that weakens that exclusivity — fails
loudly here rather than silently wiring mismatched geometry.

### D5: `graphbuilder_advertise_geometry`/`resolve_geometry` are self-contained, not general `VariableSpec` infrastructure
Both new procedures take `class(OuterMetaComponent)` directly (matching
every other `GraphBuilder.F90` entry point's own signature shape) and read
`GeometrySpec`/`get_geom()`/`get_geom_id()` directly — no new abstraction
layer, no attempt to generalize "a `VariableSpec`-free advertised item" as
a reusable concept for future non-geometry cases. If a future change needs
the same shape again, generalize then, from two real examples, rather than
speculatively now.

### D6: A global, default-off legacy/graph-native toggle (`mapl_GraphMode_mod`)
`graph_native_enabled()`/`set_graph_native_enabled()`, a module-level
`logical, save`, default `.false.` — mirrors `mapl_ExtensionResolution_mod`'s
own `materialize_extensions` precedent exactly in shape (setter, query
function, no config/env-var-driven default yet). Added at the user's
explicit request, anticipating that as graph-native code starts making
real decisions (not just building parallel, additive structure), it will
increasingly conflict with the `OuterMetaComponent`/`StateRegistry` layer
it runs alongside — this flag is the single point that will decide who
wins, one call site at a time, as each such conflict is reached.

This change's own use of it is narrow: `run_geometry_hook`'s entire body
is a no-op unless the flag is enabled. This differs from
`materialize_extensions`'s own scope (which gates only real payload
*materialization*, with chain *structure* always built unconditionally)
because geometry has no equivalent structure-vs-materialize split in this
change — there is no real allocation step separate from "build the graph
structure," so gating the whole hook is the correct-sized boundary here.

**Non-Goal, explicitly:** using this flag to make `initialize_geom_a.F90`/
`initialize_geom_b.F90` defer to the graph-native representation (e.g.
`get_geom()` itself becoming graph-aware) is real, additional work this
change does not attempt (Goals/Non-Goals). The flag exists as reusable
infrastructure for that and other future conflicts; this change is only
its first, narrowly-scoped consumer.

## Risks / Trade-offs

- [Risk] The push direction (`push_geometry_to_child`) is new code with no
  existing precedent to mirror (unlike the pull direction, which reuses
  `get_or_make_local_node_id` unmodified) — it is the one place in this
  change where a genuinely new cross-graph-boundary mechanism was written
  rather than reused. → Mitigation: kept deliberately symmetric with the
  pull direction's own shape (same key-naming convention, same
  `Characteristic`-based mismatch check, same "proxy node + dependency
  edge" structure) and covered by a dedicated test
  (`test_child_receives_geometry_from_parent`) asserting the proxy and
  edge land in the *child's* graph, not the parent's.
- [Risk] Shipping mismatch-detection with no working transform means a
  configuration that ever legitimately hits the mismatch path (should be
  unreachable given `GeometrySpec`'s own exclusivity, D4) gets a hard,
  unrecoverable failure rather than a real regrid. → Mitigation: same
  additive posture every Phase 3/4 sub-change has taken; this path is not
  reachable through any currently-modeled `GeometrySpec` configuration,
  and the failure is loud/diagnosable (two `_ASSERT`-chain exceptions), not
  silent.
- [Trade-off] `get_geom_id()`'s in-memory-only identity flow (D2) is
  simpler than the original ESMF-Info-based plan but means geometry
  identity is only comparable *through `OuterMetaComponent`* — a geom
  handle passed around independently of its owning `OuterMetaComponent`
  carries no recoverable identity via this path (it may still carry one
  via `MAPL_GeomGetId` if it happens to have been tagged by the
  `mapl_GeomManager` layer, but this change does not depend on that).
  Accepted — no current call site needs identity outside an
  `OuterMetaComponent` context.
- [Trade-off] The global toggle (D6) is currently only settable
  programmatically (tests); there is no config-driven production path to
  enable it yet. Accepted as appropriately minimal for this change's own
  scope — real activation is necessarily tied to whatever future change
  first needs graph-native behavior to actually run in production.

## Open Questions

None — REQ-GEO-002a and §13.4 are explicit Non-Goals (deferred by the
roadmap itself), and D1–D6 above resolve every technical choice this
change's own (revised) scope requires.
