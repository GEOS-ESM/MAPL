## Context

Two parallel "characteristic" class hierarchies exist today:

- **`Characteristic`** (`superstructure/generic/graph/Characteristic.F90`)
  - live, consulted by `GraphBuilder.F90`'s real `resolve_one`/
    `ExtensionResolution.F90`'s `build_chain` for every real connection
    resolution. Three concrete types: `UnitsCharacteristic`,
    `VerticalGridCharacteristic`, `GeomCharacteristic`. Interface has a
    `build_transform` method - only `UnitsCharacteristic` implements it
    for real (`UnitsConverterTransform`); the other two `_FAIL`
    explicitly (no real provider yet).
- **`StateItemCharacteristic`** (`superstructure/generic/graph/
  StateItemCharacteristic.F90`, via `ValueCharacteristic`/
  `ReferenceCharacteristic`) - fully implemented and unit-tested, but
  not consulted anywhere in `GraphBuilder.F90`/`ExtensionResolution.F90`
  (confirmed by direct grep - zero references). Nine concrete types,
  including `TypeKindCharacteristic`, `PhysicalUnitsCharacteristic`,
  `GeometryCharacteristic`, `VerticalCoordinateCharacteristic`, and five
  more with no `Characteristic`-side counterpart at all. No
  `build_transform`-equivalent exists anywhere in this hierarchy by
  design (REQ-CHAR-002: "describes, does not adapt").

Every prior overlap between the two was resolved by renaming the *new*
side to dodge an existing *old* name (`Geom`->`Geometry`,
`VerticalGrid`->`VerticalCoordinate`, `Units`->`PhysicalUnits`), each
documented as a deliberate, cost-driven deferral of real unification
(`state-item-characteristics` design.md D6;
`state-item-characteristic-subclasses` design.md D1/D3). 10c, as
currently worded in the roadmap, is the first case running the other
direction - new growth on the *old* side landing on a name
(`TypeKindCharacteristic`) the *new* side already occupies - with no
existing precedent for how to resolve that specific direction of
collision.

See `proposal.md` for full motivation; this document records the
technical decision for *how* 10c should resolve it, so that decision
exists before 10c's own future `openspec` change is filed.

## Goals / Non-Goals

**Goals:**
- Pick, now, which hierarchy 10c's typekind-mismatch resolution lives
  on, so 10c's own future design.md does not have to re-derive this
  from scratch or default to a fourth rename-to-dodge.
- Frame 10c as a real, narrowly-scoped pilot for reconciling the two
  hierarchies on exactly one characteristic axis, with an explicit,
  checkable success criterion (`GraphBuilder.F90` consults the
  `StateItemCharacteristic`-hierarchy `TypeKindCharacteristic` directly,
  with no second, `Characteristic`-extending type created).

**Non-Goals:**
- Migrating `UnitsCharacteristic`, `VerticalGridCharacteristic`, or
  `GeomCharacteristic` onto the `StateItemCharacteristic` hierarchy -
  out of scope for 10c; a later phase's own decision, informed by
  whether this pilot works out.
- Retiring the `Characteristic` hierarchy, `CharacteristicMap`, or
  `CharacteristicId` - `GraphBuilder.F90`'s real resolution continues to
  key its mismatch map by `CharacteristicId`; this design only changes
  what object answers "does this axis mismatch, and what fixes it" for
  the typekind axis specifically.
- Designing the real R4<->R8 cast transform's own implementation
  details (10c's own future design.md's job) - this document only
  settles which characteristic class owns the decision to build one.

## Decisions

**10c builds on the existing `TypeKindCharacteristic`
(`ValueCharacteristic`-based), not a new `Characteristic`-extending
type.** The existing type already carries the one piece of real data
needed (`type_kind`, a plain `'R4'`/`'R8'` label) and already implements
`needs_extension_for` correctly for this axis. Building a second type
from scratch on the `Characteristic` side would duplicate that
comparison logic for no reason other than interface-shape convenience -
exactly the pattern this amendment exists to stop repeating.

**`GraphBuilder.F90`'s `build_characteristics()` gains a real adaptation
hook, not a second characteristic hierarchy per axis.** Two technical
options were considered for making a `ValueCharacteristic`-rooted type
usable where `CharacteristicMap`/`find_or_build_extension_chain`
currently expect a `Characteristic`:

1. **Add an optional `build_transform`-equivalent method to
   `StateItemCharacteristic`/`ValueCharacteristic` itself** (e.g. a
   deferred-but-overridable method, default implementation `_FAIL`s
   "no adapter", matching how `GeomCharacteristic`/
   `VerticalGridCharacteristic` already `_FAIL` on the `Characteristic`
   side for the same "detection exists, adaptation doesn't yet"
   situation). `TypeKindCharacteristic` overrides it for real
   (mirroring `UnitsCharacteristic%build_transform`'s own shape:
   construct a `Transform` subclass directly from its own stored
   `type_kind` values). `GraphBuilder.F90`'s `build_characteristics()`
   constructs a `TypeKindCharacteristic` the same way it already
   constructs `UnitsCharacteristic`/`VerticalGridCharacteristic`, but
   inserts it into a structure keyed/dispatched to accept either
   hierarchy for this one axis (a thin adapter at the `CharacteristicMap`
   insertion point, not a parallel map).
2. **Write a `Characteristic`-side adapter/wrapper that holds a
   `TypeKindCharacteristic` by composition** and forwards
   `needs_extension_for` to it, implementing only `build_transform`
   itself. Keeps `ValueCharacteristic`'s own "describes, does not
   adapt" contract completely untouched; isolates the one new
   capability (adaptation) in a wrapper type that exists solely for
   this bridge.

This document does not mandate which of the two 10c's own future
design.md should pick - both satisfy the real success criterion (no
second `TypeKindCharacteristic`, `GraphBuilder.F90` consults the
existing one) and the choice has no bearing on anything outside 10c's
own scope; 10c's own design.md, written with 10c's own actual
implementation constraints in hand, is the right place to pick between
them. What this document does settle is that **a wrapper/adapter at the
bridge point is the shape of the fix, not a second concrete
characteristic type under either name.**

**This is a pilot, evaluated before being repeated.** If 10c's own
bridge works cleanly, it is evidence for eventually applying the same
adapter-at-the-bridge approach to `Units`/`PhysicalUnits` and
`Geom`/`Geometry`/`VerticalGrid`/`VerticalCoordinate` too - but that
decision is explicitly deferred to whenever those axes next need
real-code attention, not decided here.

## Risks / Trade-offs

- **[Risk] 10c's own future design.md ignores this amendment and
  reverts to a fourth rename-to-dodge anyway** (nothing enforces
  reading this document before filing 10c as its own change) →
  **Mitigation:** the roadmap's own §20.4.8 10c entry text itself is
  amended (not just this standalone design.md) to state the
  constraint directly, so it is read by whoever next opens that
  section, not only by whoever happens to find this archived change.
- **[Risk] The adapter/wrapper approach (Decision 2) or the optional-
  method approach (Decision 1) turns out to be awkward once 10c's
  implementer is actually writing `ExtensionResolution.F90`-facing
  code** → **Mitigation:** explicitly left as an open choice for 10c's
  own design.md (see Decisions above) rather than mandated here,
  precisely because this document is written before 10c's own real
  constraints are in hand.

## Open Questions

- Which of Decision 1 vs Decision 2 (optional method on
  `StateItemCharacteristic` vs. a `Characteristic`-side wrapper) 10c
  should actually implement - deliberately left to 10c's own future
  design.md; does not affect this amendment's own scope or the roadmap
  text change.
