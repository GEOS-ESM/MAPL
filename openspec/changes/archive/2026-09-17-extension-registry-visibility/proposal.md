## Why

Phase 3c (`extension-reuse`, archived) gave `GraphBuilder` real
extension-chain machinery for mismatched export/import pairs
(`Characteristic`/`CharacteristicMap`/`TransformGraphNode`/extension
`StateItemNode`s, `superstructure/generic/graph/ExtensionResolution.F90`)
but explicitly deferred REQ-EXT-004 (`09-extension-reuse.md`): extension
items it creates are correct graph structure — real `NodeId`s, real
dependency edges, real reuse semantics — but carry no real payload.
Per `docs/graph/spec/20-implementation-roadmap.md` §20.4.1, this is
sub-change 3c2. Without this, `GraphBuilder`'s extension resolution has
no computable effect — `UnitsConverterTransform` (3c's one real,
executing transform) already assumes its output item has a real
`ESMF_Field` to write into, and nothing today ever gives it one.

`StateRegistry` — the legacy machinery that materializes and registers
extension payloads today — is slated for eventual removal as graph
development matures. This capability's job is therefore to replicate,
natively in the graph layer, the one piece of that legacy machinery this
capability needs (real payload materialization for an extension item),
not to bridge into or depend on `StateRegistry` even read-only.
`StateRegistry`/`StateItemSpec`/`ClassAspect` remain useful only as
background reference for what a correct payload looks like.

## What Changes

- **Real materialization is gated behind a simple, global, default-off
  switch.** `GraphBuilder`'s hooks already run unconditionally in every
  real production run today (`initialize_advertise.F90`,
  `initialize_accept_transfer.F90`); through 3c this was safe because
  every graph-side operation was cheap structural bookkeeping with zero
  real ESMF allocation. This capability is the first to perform real
  ESMF work (`FieldCreate`) from inside that same unconditional call
  chain — without an explicit gate, every `units`-mismatched connection
  in every real run would have its field allocated twice (once by
  legacy, once by this capability). A module-level flag, off by default,
  guards only the real materialization step; chain structure (`NodeId`s,
  edges, reuse) is unaffected and unconditional exactly as 3c left it.
  No production code enables the flag — only this capability's own
  tests. See design.md for why this is a global internal switch, not a
  `VariableSpec` variant or a per-component config option (raised and
  resolved via domain-expert review during planning).
- Give a `units`-mismatched, plain-`Field`-typed export/import pair
  (3c's only characteristic with a real, executing `build_transform`) a
  real, computable result: when `ExtensionResolution` builds or reuses a
  chain for such a pair, it materializes a real, fully allocated
  `ESMF_Field` for the chain's final extension item using
  `mapl_FieldCreate_mod::FieldCreate` (`infrastructure/field/
  FieldCreate.F90`) — a general-purpose, `StateRegistry`-independent
  field factory already used at the same dependency tier as
  `mapl_FieldPointerUtilities_mod`, which 3c's own
  `UnitsConverterTransform` already depends on.   `typekind`/`ungridded_dims` come from the export's own `VariableSpec` —
  the same plain-field-read pattern 3c's own `build_characteristics`
  already established — and `units` comes from the import's required
  value. `geom`/`vgrid` are resolved in priority order: the export's own
  `VariableSpec%geom`/`vertical_grid` if explicitly set (the
  `HistoryCollection`-style explicit-override case), else the owning
  `OuterMetaComponent`'s existing component-wide default
  (`get_geom()`/`get_vertical_grid()` — the same fallback
  `advertise_variable.F90` already applies to every `VariableSpec`, and
  the common case per domain-expert direction, since a `VariableSpec`
  generally does not carry its own geom), else an explicit failure —
  true cross-component geom "mirroring" (legacy's
  `GeomAspect%connect_to_export`) is a distinct, substantially larger
  follow-up capability, not attempted here. This is entirely new logic
  in the graph layer; it does **not** read, link to, or depend on
  anything `StateRegistry`'s own imperative connect path independently
  creates for the same connection.
- Scope the materialization to plain-`Field`-typed items only
  (`VariableSpec%itemType == MAPL_STATEITEM_FIELD`). A `units` mismatch
  on a `Vector`-typed item (real payload would be an `ESMF_FieldBundle`)
  or on anything reachable only through a `State` payload is detected
  and reported as an explicit, distinguishable failure rather than
  silently producing an incomplete or fabricated payload — no
  `StateRegistry`-independent factory exists yet for either case, and
  building one is a substantially larger follow-up capability, not a
  small addition here.
- Discoverability of a resolved extension item is unchanged from 3c
  (REQ-EXT-005's existing `ComponentGraph` resource-index search) — this
  capability gives an already-discoverable node a real payload; it does
  not add a new discoverability mechanism.
- Any other mismatched characteristic (vertical grid, geom, typekind,
  ...) still has no real `build_transform` (unchanged 3c scope boundary)
  and therefore still fails explicitly before this capability's
  materialization step would ever run.
- **Explicitly out of scope**: real providers for non-`units`
  characteristics (unchanged follow-up, not this change); real
  materialization for a `Vector`/`FieldBundle` or `State`-reachable
  mismatch (follow-up; proper recursive `State` support is the actual
  long-term motivation for the graph work generally, not something this
  narrow capability closes); any bridge into a real `OuterComponent`'s
  ESMF import/export state (`ESMF_StateAdd`) — that is separate, later
  integration work once the graph is ready to take over the
  responsibility `StateRegistry`'s `add_to_states` currently has;
  REQ-EXT-002a (shared-data payload); removal or replacement of the
  legacy `StateRegistry%extend()`/`ExtensionFamily` path, which remains
  fully independent of this capability's own graph-native path and
  continues to run unchanged; any change to `TransformGraphNode`/
  `DependencyNetwork`/`ComponentGraph`/`GraphStateItem` public APIs from
  Phase 1–2.

## Capabilities

### New Capabilities
(none)

### Modified Capabilities
- `graph/extension-reuse`: adds two requirements — (1) real payload
  materialization is disabled unless explicitly enabled, with zero
  behavior change from 3c when disabled; (2) when enabled, a
  framework-created extension item resulting from this capability's
  chain creation for a plain-field-typed mismatch MUST be materialized
  as a real, allocated payload, natively in the graph layer, not left as
  graph-internal structure only; a mismatch on an item class this
  capability cannot materialize a payload for MUST fail explicitly
  rather than produce an incomplete one. 3c's own delta spec explicitly
  did not include either requirement (see `openspec/changes/archive/
  2026-09-16-extension-reuse/specs/graph/extension-reuse/spec.md`); this
  change adds them.

## Impact

- **Affected code**: `superstructure/generic/graph/
  ExtensionResolution.F90` (the materialization gate) and a new
  `superstructure/generic/graph/ExtensionMaterialization.F90` (the
  `FieldCreate`-based materialization procedure itself — a chain's
  final extension `StateItemNode` gets a real, materialized `ESMF_Field`
  instead of being left with an unallocated `empty_payload`, for
  plain-`Field`-typed mismatches); `superstructure/generic/
  GraphBuilder.F90` (the gate check, `VariableSpec%itemType` check, and
  geom/vgrid resolution live at the same connect-hook call site that
  already builds `CharacteristicMap`s from `VariableSpec` — `build_
  characteristics`'s own established pattern — plus an explicit failure
  path for any other item class or unresolvable geom). **No changes**
  to `superstructure/generic/registry/` (`StateRegistry`) or
  `superstructure/generic/connection/` (`SimpleConnection`/
  `Connection`). `superstructure/generic/OuterMetaComponent.F90` gets
  one small, additive accessor, `get_child_outer_meta` (mirrors the
  existing `get_child_component_spec`/`get_child_component_graph`
  framework-internal reach, REQ-GB-002) — needed because
  `has_geom()`/`get_geom()`/`get_vertical_grid()` live on the whole
  `OuterMetaComponent` object, not on `ComponentSpec`/`ComponentGraph`
  alone; no existing public API is changed. (An earlier draft of this
  proposal said "no changes to `OuterMetaComponent*`" at all — revised
  during implementation: that was stricter than necessary and did not
  anticipate this small, precedented accessor.)
- **Existing code read as background reference only, never called
  into**: `superstructure/generic/specs/StateItemSpec.F90`
  (`FieldClassAspect%create`/`allocate`, for what a correct field needs),
  `superstructure/generic/specs/VectorClassAspect.F90`/
  `StateClassAspect.F90` (for why those item classes are out of scope
  here), `superstructure/generic/registry/StateRegistry*` (for
  background on what legacy already does for the same connection,
  independent of and unaffected by this change).
- **New Fortran surface**: a module-level materialization gate (flag +
  getter/setter); a materialization procedure (`ExtensionResolution`-
  adjacent, in `superstructure/generic/graph/`) that, given a resolved
  `units`-mismatch chain for a `Field`-typed item and the gate enabled,
  calls `FieldCreate` and assigns the result to the extension
  `StateItemNode`'s payload; an explicit-failure path for any
  non-`Field` item class or unresolvable geom/vgrid. No new
  `StateRegistry`-facing type, no changes to
  `StateRegistry`/`Connection`; `OuterMetaComponent` gets one small,
  additive accessor (`get_child_outer_meta`, see Impact above), no
  change to any existing accessor. Concrete signatures left to
  design.md.
- **Tests**: assert the gate defaults off with zero behavior change from
  3c; with the gate explicitly enabled in the test, extend the existing
  extension-reuse pFUnit coverage so a `units`-mismatch, field-typed
  extension chain's final item is asserted to (a) have a real,
  correctly-shaped `ESMF_Field` after resolution, (b) produce a
  numerically correct conversion via `UnitsConverterTransform`, (c) not
  be re-materialized when reused by a second import, and (d) still fail
  loudly, unchanged, for a characteristic with no registered provider or
  for a non-`Field` item class. Where practical, extend the
  `graphbuilder_equivalence` real-configuration fixture (3c's own
  precedent) so it also asserts the graph-side field is real and
  correctly shaped when enabled (decision parity with legacy — same
  reuse/creation choice — not byte-for-byte field identity with a
  legacy-created extension, since this path no longer touches
  `StateRegistry` at all).
- **Dependencies**: builds on 3c's chain-creation machinery
  (`extension-reuse`, archived); adds a direct dependency on
  `infrastructure/field/FieldCreate.F90` (general-purpose, already an
  accepted dependency tier for this code per 3c's own use of
  `mapl_FieldPointerUtilities_mod`). Does not depend on
  `StateRegistry`/`OuterComponent`/`Connection` in any way, and does not
  depend on real `build_transform` providers for any characteristic
  beyond `units`.
- **Out of scope**: everything listed under "Explicitly out of scope"
  above.
