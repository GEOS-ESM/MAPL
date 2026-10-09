## Why

`docs/graph/spec/20-implementation-roadmap.md` §20.4.8's own 10c entry
("`TypeKindCharacteristic` wired into real resolution") currently reads:
"`GraphBuilder.F90`'s own `build_characteristics()` gains a
`TYPE_KIND_CHARACTERISTIC_ID` entry whenever a `VariableSpec%typekind` is
declared, mirroring the existing `units`/`vertical_grid` entries" — i.e.
a *new*, `Characteristic`-extending (`graph/extension-reuse` hierarchy)
`TypeKindCharacteristic`, inserted into `CharacteristicMap` the same way
`UnitsCharacteristic`/`VerticalGridCharacteristic` already are.

This collides with an **already-shipped** type: `mapl_TypeKindCharacteristic_mod`'s
`TypeKindCharacteristic` (`superstructure/generic/graph/TypeKindCharacteristic.F90`),
landed by the archived `state-item-characteristics` change, extending
`ValueCharacteristic` (the separate, deliberately-independent
`StateItemCharacteristic` hierarchy, `docs/graph/spec/
18-state-item-characteristics.md`). Same module name
(`mapl_TypeKindCharacteristic_mod`), same type name
(`TypeKindCharacteristic`) — Fortran's one global module/type namespace
makes 10c as currently worded a literal compile collision with existing,
shipped code, not merely a conceptual overlap.

This is the inverse of every prior instance of this same pattern. Three
times already, the *new* (`StateItemCharacteristic`) hierarchy
re-modeled something the *old* (`Characteristic`) hierarchy already had,
and each time pre-emptively renamed itself to dodge the collision
(`GeomCharacteristic` -> `GeometryCharacteristic`;
`VerticalGridCharacteristic` -> `VerticalCoordinateCharacteristic`;
`UnitsCharacteristic` -> `PhysicalUnitsCharacteristic` — each dodge
documented, with the renaming decision cited by name, in that type's own
header comment and its change's own design.md). 10c is the first case
running the other direction: the *old* hierarchy's own planned growth
would land on a name the *new* hierarchy already occupies. Left as
worded, 10c's own implementer would face the same choice point already
seen three times — rename the new old-hierarchy type to dodge (a fourth
instance of the same avoidance, deferring unification again) — with no
roadmap text currently steering that choice either way.

Reviewed while completing `phase-10a-state-materialization`
(`openspec/changes/phase-10a-state-materialization`), itself motivated
by closing the same kind of "built but never validated" gap this
roadmap section exists to track.

## What Changes

- Amend §20.4.8's own 10c entry in
  `docs/graph/spec/20-implementation-roadmap.md` to commit explicitly to
  the unification direction, not the fourth-rename direction: 10c SHALL
  give the existing, already-shipped `TypeKindCharacteristic`
  (`ValueCharacteristic`-based) a real adaptation capability (a
  `build_transform`-equivalent, mirroring `UnitsCharacteristic`'s own
  shape) and wire `GraphBuilder.F90`'s real connection resolution to
  consult it directly for a typekind mismatch — rather than
  constructing a second, `Characteristic`-extending type under the same
  or a dodged name.
- Record this as the deliberate first, narrowly-scoped pilot for
  reconciling the two parallel characteristic hierarchies
  (`graph/extension-reuse`'s `Characteristic` family and
  `graph/state-item-characteristics`'s `StateItemCharacteristic`
  family) one characteristic at a time, rather than the all-at-once
  migration every prior design.md (`state-item-characteristics` D6,
  `state-item-characteristic-subclasses` D1/D3) explicitly declined as
  too costly to attempt speculatively. 10c is the first point any
  roadmap phase is *forced* to confront the collision, by virtue of
  being the first new growth on either side that lands on an
  already-taken name - not an arbitrarily chosen moment.
- No code changes in this proposal itself - it only amends the
  still-unimplemented 10c roadmap entry before anyone implements it as
  currently worded. 10c's own future `openspec` change (when filed, per
  10a/10b's own precedent of being filed as separate, narrowly-scoped
  sub-changes) inherits this amended scope.

## Capabilities

### New Capabilities
None.

### Modified Capabilities
None - this proposal only amends a planning-record document
(`docs/graph/spec/20-implementation-roadmap.md`); it does not change any
requirement in an `openspec/specs/` capability. `.openspec.yaml` for
this change sets `skip_specs: true` accordingly, matching the existing
`phase-10-sub-sequencing`/`phase-5-sub-sequencing` precedent for
roadmap-only amendments.

## Impact

- **Affected file:** `docs/graph/spec/20-implementation-roadmap.md`
  only (10c's own entry in §20.4.8 amended; no other section's content
  changes).
- **Downstream effect:** when 10c is eventually filed as its own
  `openspec` change (after 10a/10b land), its proposal/design/tasks
  must follow this amended scope - give the existing
  `TypeKindCharacteristic` a real `build_transform`-equivalent and wire
  `GraphBuilder.F90` to it directly, rather than creating a second,
  `Characteristic`-extending type.
- **No code, build, or test impact from this change itself.** This is a
  documentation-only planning amendment, written before 10c exists as
  code.
