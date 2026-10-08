## Why

`docs/graph/spec/20-implementation-roadmap.md` §20.4.4 identifies Phase 5a
(`18-state-item-characteristics.md` §18.2–§18.8) as ready to scope now: Q11
(the `GraphStateItem` amendment this document depends on) is resolved, and
what remains are ordinary `[OPEN]` naming/mechanism points, not a blocking
precondition. Today, mismatch detection for a connection (units, vertical
grid, geometry) is handled entirely by `GraphBuilder`'s own ephemeral,
`VariableSpec`-derived `Characteristic`/`CharacteristicMap`
(`superstructure/generic/graph/Characteristic.F90`, built for the
extension-reuse sub-change, 3c) — there is no persistent, per-characteristic
model attached to a `GraphStateItem` itself, so a `GraphStateItem`'s current
mismatch-relevant metadata (units, type/kind, geometry, ...) is not
independently inspectable, nor can a characteristic be shared across
`GraphStateItem`s (needed for geometry/vertical-grid, §18.7) or safely
mutated in place with eager structural propagation to dependents (needed for
a `MAPL_SetGeom`-style call mid-`Run`, §18.8). This change gives each
`GraphStateItem` that first-class characteristic model.

## What Changes

- Add a new `StateItemCharacteristic` abstract type hierarchy
  (REQ-CHAR-001/002), branching into `ValueCharacteristic` (inline value,
  never shared) and `ReferenceCharacteristic` (holds a `NodeId` referencing
  a shared graph node) per REQ-CHAR-002a/002b, with concrete subclasses
  `PhysicalUnitsCharacteristic` and `TypeKindCharacteristic`
  (`ValueCharacteristic`) and a geometry `ReferenceCharacteristic` — named
  `GeometryCharacteristic`, not `GeomCharacteristic`, to avoid colliding
  with the existing, unrelated `mapl_GeomCharacteristic_mod` type used by
  `graph/extension-reuse` (same naming-collision discipline already applied
  to `UnitsConverterTransform` vs. legacy `ConvertUnitsTransform` — see
  design.md Decisions).
- Add a status enumeration (REQ-CHAR-003/004) with `INVALID`/`SPECIFIED`/
  `MIRRORED`/`UNCHECKED`/`DEFERRED` values, and a per-subclass stable type
  tag (REQ-CHAR-005/006) used as the map key below. Both working names
  from `18-state-item-characteristics.md` (`CharacteristicStatus`,
  `CharacteristicType`) are finalized or replaced in design.md — see
  Decisions for the exact names and the collision-avoidance reasoning for
  the latter (the existing, unrelated `CharacteristicId`,
  `graph/extension-reuse`'s own per-kind identity type, already occupies
  adjacent naming space in the same directory).
- Add `GraphStateItem.characteristics`, a sparse map from the new type tag
  to `StateItemCharacteristic` (REQ-CHAR-007/008), plus an ordering query
  that returns the sequence in which mismatching characteristics' Transforms
  should be chained, delegating to a per-kind strategy table
  (REQ-CHAR-010/011, resolving Q13's own recommendation).
- Support sharing a `ReferenceCharacteristic` across multiple
  `GraphStateItem`s by holding the same `NodeId` (REQ-CHAR-012/013/014), and
  add a dedicated, synchronous mutator entry point (REQ-CHAR-015/016/017)
  that updates a shared characteristic's value, eagerly resets its direct
  structural dependents within one `ComponentGraph` to `INVALID`, and
  advances the shared node's `NodeRevision` so lazy content-side
  propagation (REQ-CHAR-018, existing demand-driven `update()`) picks up
  the change on next use. The mutator never invokes a `MethodGraphNode`
  (REQ-CHAR-017) — restricted to pure structural resets.
- **Correction made during implementation, recorded here for the
  record**: an earlier draft of this proposal planned to add REQ-REV-011
  (a `MethodGraphNode` invocation trigger pulling bound IN/INOUT arguments
  before invoking the underlying method) as new work. While implementing,
  this was found to already exist — `docs/graph/spec/11-revision-and-
  update.md` §11.4a already specifies REQ-REV-011/REQ-REV-011a (including
  a note already cross-referencing `18-state-item-characteristics.md`'s
  REQ-CHAR-018), `openspec/specs/graph/method-graph-node/spec.md` already
  documents the matching requirement and scenarios, and
  `superstructure/generic/graph/MethodInvocation.F90`'s
  `invoke_on_default_network()` already implements it (pulls every bound
  IN/INOUT argument via `ComponentGraph%update()` before invoking, and
  only advances bound OUT/INOUT revisions on success), landed with Phase
  4b (`griddedcomponentdriver-integration-lifecycle`). This change adds
  no new code or spec delta for REQ-REV-011 — it only relies on that
  already-shipped mechanism as the content-side half of REQ-CHAR-018's
  propagation story, and adds one new test confirming the two interact
  correctly (a mid-`Run` structural reset from this change's new mutator,
  followed by `invoke_on_default_network` on a dependent method node).
- **Explicitly out of scope / deferred, per the spec's own `[OPEN]` marks**:
  the cross-`ComponentGraph` structural-dependent walk (REQ-CHAR-016's
  second bullet — "has not been fully worked out") is deferred to a
  follow-up; this change's mutator walks structural dependents within one
  `ComponentGraph` only. Real `build_transform` implementations for
  `TypeKindCharacteristic`/`GeometryCharacteristic` are not added here —
  mirrors the existing `graph/extension-reuse` precedent (`units` is the
  only characteristic with a real, executing provider so far); an
  unsupported characteristic's adaptation fails explicitly, it is never
  silently skipped.
- **Deliberately not integrated with `graph/extension-reuse`**: the existing
  `Characteristic`/`CharacteristicMap`/`CharacteristicId` family
  (`GraphBuilder`'s real connection-resolution path) is left unmodified and
  continues to operate on its own `VariableSpec`-derived data, per that
  module's own header comment declaring independence from
  `StateItemCharacteristic`. REQ-CHAR-009's "iterate all characteristics /
  build a reconciling chain" language is implemented here as a new,
  independently-testable algorithm operating on `GraphStateItem`'s own
  `characteristics` map (synthetic-node tests, no `GraphBuilder`
  involvement) — not as a rewire of `GraphBuilder`'s real connection
  resolution. Wiring real connection resolution to consult
  `GraphStateItem.characteristics` instead of (or alongside) the existing
  `VariableSpec`-derived `CharacteristicMap` is left to a follow-up; see
  design.md Decisions for the full rationale.

## Capabilities

### New Capabilities
- `graph/state-item-characteristics`: the `StateItemCharacteristic`
  hierarchy (Value/Reference split), `CharacteristicStatus`/type-tag
  identity, ordering-delegation, sharing across `GraphStateItem`s, and the
  eager-structural/lazy-content propagation mutator (REQ-CHAR-001..018,
  `18-state-item-characteristics.md` §18.2–§18.8).

### Modified Capabilities
- `graph/state-item`: `GraphStateItem` gains the sparse
  `characteristics : map<type-tag, StateItemCharacteristic>` component
  (REQ-CHAR-007/008) and the ordering-query method (REQ-CHAR-010).

## Impact

- **Affected code**: new module(s) under `superstructure/generic/graph/`
  for `StateItemCharacteristic`/`ValueCharacteristic`/
  `ReferenceCharacteristic` and its concrete subclasses, the status
  enumeration, and the type-tag identity type (exact file layout left to
  design.md/tasks.md); `GraphStateItem.F90` gains the `characteristics` map
  component and ordering-query method; a new mutator module implementing
  REQ-CHAR-016.
- **Existing code read as reference only, not modified**: `graph/
  extension-reuse`'s `Characteristic.F90`/`CharacteristicId.F90`/
  `UnitsCharacteristic.F90`/`VerticalGridCharacteristic.F90`/
  `GeomCharacteristic.F90` (deliberately independent family, per its own
  header comments) and legacy `StateItemAspect`/`AspectMap`.
  `ComponentGraph`'s `DependencyNetwork` successor/predecessor queries
  (`get_successors`, `contains_dependency`) are reused as-is for the
  single-graph structural-dependent walk. `MethodInvocation.F90`'s
  existing `invoke_on_default_network()` (already-shipped REQ-REV-011) is
  reused as-is, not modified — see "What Changes" correction above.
- **New Fortran types**: `CharacteristicStatus` (or design.md's chosen
  final name), the type-tag identity type (design.md's chosen name, not
  `CharacteristicType` verbatim — collision-avoidance), `StateItemCharacteristic`
  (abstract), `ValueCharacteristic`/`ReferenceCharacteristic` (abstract),
  `PhysicalUnitsCharacteristic`, `TypeKindCharacteristic`,
  `GeometryCharacteristic`.
- **Tests**: new pFUnit coverage for the Value/Reference split, status
  transitions, the sparse map (absent-key vs. `INVALID`, per design.md's
  resolution of that open point), ordering delegation per kind, sharing a
  `ReferenceCharacteristic` across two `GraphStateItem`s, the eager mutator
  (structural reset + revision advance, no `MethodGraphNode` invoked), and
  one new case confirming the already-shipped `invoke_on_default_network`
  (REQ-REV-011) correctly resolves a mid-`Run` structural reset from this
  change's new mutator — all exercised without any ESMF component/
  `GridComp`/`StateRegistry` involvement, matching Phase 1–2's own exit
  criterion.
- **Dependencies**: builds on `graph/state-item` (`GraphStateItem`),
  `graph/node-revision-and-update` (`NodeRevision`, demand-driven
  `update()`), `graph/method-graph-node` (`MethodGraphNode`,
  `invoke_on_default_network`'s already-shipped REQ-REV-011 pull),
  `graph/dependency-network` (successor/predecessor queries), and
  `graph/component-graph` (resource index, for a future reuse-search
  integration — not required by this change itself). No dependency on `16`
  (ordinary inout) or Q9 (compiled execution), per the roadmap.
- **Out of scope**: cross-`ComponentGraph` structural-dependent walk; real
  `build_transform` providers beyond what already exists for `units` in
  `graph/extension-reuse` (this change's characteristics are a separate,
  parallel model — see "What Changes"); rewiring `GraphBuilder`'s real
  connection resolution to consult `GraphStateItem.characteristics`;
  `16`/`18`'s own REQ-INOUT-002 general inout case; Q9 compiled execution.
