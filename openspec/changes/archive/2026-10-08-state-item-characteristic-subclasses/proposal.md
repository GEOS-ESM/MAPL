## Why

`docs/graph/spec/20-implementation-roadmap.md` §20.4.4 identifies sub-change
5a2 as a required-but-not-yet-filed completion of 5a
(`openspec/changes/archive/2026-10-08-state-item-characteristics`), discovered
during 5a's own code review: REQ-CHAR-001's table lists only three concrete
`StateItemCharacteristic` subclasses (`PhysicalUnitsCharacteristic`,
`TypeKindCharacteristic`, `GeometryCharacteristic`) and explicitly says so —
"almost certainly incomplete... treat the list as a starting point, not a
closed set." Legacy's own `StateItemAspect` hierarchy
(`superstructure/generic/specs/*Aspect.F90`) already has ten concrete
mismatch-detectable axes, not three. The seven not yet covered —
`VerticalGridAspect`, `AttributesAspect`, `UngriddedDimsAspect`,
`QuantityTypeAspect`, `ConservationAspect`, `NormalizationAspect`,
`StandardNameAspect` — each need a `StateItemCharacteristic` analog before
the graph-native path can claim parity with legacy's own mismatch-detection
surface. This is a parity gap against an existing legacy surface, not a
brand-new requirement, and REQ-CHAR-001/REQ-CHAR-006 already guarantee the
mechanism accommodates it: "adding a new characteristic subclass MUST NOT
require changes to `GraphStateItem` or `CharacteristicType`'s consumers
beyond registering a new `CharacteristicType` value."

## What Changes

- Add seven new concrete `StateItemCharacteristic` subclasses, one per
  remaining legacy `*Aspect`, each registered with its own new
  `StateItemCharacteristicKind` value (REQ-CHAR-005/006) — table below
  (final names decided in design.md, following the same collision-avoidance
  discipline 5a's own design.md D3 established for `GeometryCharacteristic`):

  | Legacy `*Aspect` | New `StateItemCharacteristic` | Base kind |
  |---|---|---|
  | `VerticalGridAspect` | vertical-grid reference characteristic (name TBD — collides with the existing, unrelated `graph/extension-reuse` `VerticalGridCharacteristic`) | `ReferenceCharacteristic` |
  | `AttributesAspect` | `AttributesCharacteristic` | `ValueCharacteristic` |
  | `UngriddedDimsAspect` | `UngriddedDimsCharacteristic` | `ValueCharacteristic` |
  | `QuantityTypeAspect` | `QuantityTypeCharacteristic` | `ValueCharacteristic` |
  | `ConservationAspect` | `ConservationCharacteristic` | `ValueCharacteristic` |
  | `NormalizationAspect` | `NormalizationCharacteristic` | `ValueCharacteristic` |
  | `StandardNameAspect` | `StandardNameCharacteristic` | `ValueCharacteristic` |

- Each new subclass implements only `get_kind()`/`needs_extension_for()`
  (REQ-CHAR-002's description-not-adaptation boundary, unchanged from 5a) —
  no `build_transform`/adaptation logic beyond whatever the corresponding
  legacy `*Aspect%make_transform` already does unconditionally today, which
  for all seven is either an explicit, unconditional failure
  (`ConservationAspect`) or an unconditional no-op placeholder
  (`NullTransform`, the other six) — mirroring 5a's own stance that only
  `units`/`geometry`/`typekind` have any real executing adaptation anywhere
  in this codebase yet, graph-native or legacy.
- Resolve, as a planned design decision before implementation (same
  discipline 5a's design.md followed for its own four open points): the
  final name for the vertical-grid `ReferenceCharacteristic` (collision with
  `graph/extension-reuse`'s existing `VerticalGridCharacteristic`), and
  whether legacy `AspectStatus`'s sixth value, `FROM_COMP` (absent from
  5a's `CharacteristicStatus`), needs a graph-native equivalent or is a
  legacy-only distinction with no analog needed.
- No change to `GraphStateItem`'s structure, the detection algorithm
  (`find_mismatched_state_item_characteristics`), or the ordering-table
  mechanism (`characteristic_ordering_table`) — REQ-CHAR-001/REQ-CHAR-006's
  own guarantee that a new subclass requires no such change is exercised,
  not re-litigated, by this sub-change.
- **Explicitly out of scope, same as 5a**: no `GraphBuilder` rewire (these
  remain additive, standalone types exercised by synthetic-node pFUnit
  coverage only); no real `build_transform` implementations beyond each
  legacy `*Aspect%make_transform`'s own current (mostly no-op/failing)
  behavior.

## Capabilities

### New Capabilities
(none)

### Modified Capabilities
- `graph/state-item-characteristics`: seven new concrete
  `StateItemCharacteristic` subclasses and their `StateItemCharacteristicKind`
  registrations (REQ-CHAR-001's "not exhaustive" list extended), plus the
  resolved `FROM_COMP`-analog decision. `GraphStateItem`'s own structure
  (`graph/state-item`) is unaffected — no delta needed there.

## Impact

- **Affected code**: seven new modules under
  `superstructure/generic/graph/` (one per subclass above), each registering
  one new named constant in `StateItemCharacteristicKind.F90`
  (`superstructure/generic/graph/StateItemCharacteristicKind.F90`).
  `StateItemCharacteristic.F90`, `GraphStateItem.F90`,
  `StateItemCharacteristicDetection.F90`, and `SetSharedCharacteristic.F90`
  are read as-is, not modified (REQ-CHAR-001/006's guarantee).
- **Existing code read as reference only, not modified**: the corresponding
  seven legacy `*Aspect.F90` files (`superstructure/generic/specs/`) and
  `AspectStatus.F90` (for the `FROM_COMP` decision) — mirrored for parity,
  never called into, same posture as 5a's own relationship to legacy
  `StateItemAspect`.
- **New Fortran types**: one new `StateItemCharacteristic` subclass per row
  of the table above (exact names finalized in design.md), plus seven new
  `StateItemCharacteristicKind` parameter constants.
- **Tests**: new pFUnit coverage per subclass (kind registration, Value-vs-
  Reference classification, `needs_extension_for` comparison semantics),
  exercised with synthetic nodes only, no ESMF component/`GridComp`/
  `StateRegistry` involvement — same Phase 1–2 exit-criterion posture 5a's
  own tests followed.
- **Dependencies**: builds on `graph/state-item-characteristics` (5a,
  landed) only. No dependency on `5b`/`5b2`/`5c`, per the roadmap's own
  independence note.
- **Out of scope**: `GraphBuilder` rewire; real `build_transform` adaptation
  logic beyond each legacy aspect's own current (mostly no-op/failing)
  behavior; any change to legacy `StateRegistry`/`ExtensionFamily`/
  `ClassAspect`.
