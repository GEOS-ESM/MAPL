## Context

5a (`openspec/changes/archive/2026-10-08-state-item-characteristics`) landed
`StateItemCharacteristic`/`ValueCharacteristic`/`ReferenceCharacteristic`
(`superstructure/generic/graph/StateItemCharacteristic.F90`), three concrete
subclasses (`PhysicalUnitsCharacteristic.F90`, `TypeKindCharacteristic.F90`,
`GeometryCharacteristic.F90`), `StateItemCharacteristicKind.F90` (values 1–3
registered, `INVALID`/`MOCK` reserved outside that range),
`CharacteristicStatus.F90` (five values, no `FROM_COMP`), the sparse map and
ordering-query on `GraphStateItem.F90`, and the standalone detection
algorithm (`StateItemCharacteristicDetection.F90`). All of it deliberately
independent of `graph/extension-reuse`'s own, pre-existing `Characteristic`
family (5a design.md D6) — this sub-change inherits and preserves that same
boundary; it is not revisited here.

Each concrete subclass is small and self-contained: a `get_kind()`
nopass function returning its own `StateItemCharacteristicKind` constant, and
a `needs_extension_for(this, goal)` plain value/identity comparison (no
Transform-building logic — REQ-CHAR-002's description-not-adaptation
boundary). `PhysicalUnitsCharacteristic` wraps a bare string; it does not
replicate any status-based escape hatch (e.g. an "unchecked" shortcut) —
that nuance, where it exists in legacy, lives at `GraphBuilder`'s real
connection-resolution layer, out of scope for this standalone hierarchy.
This sub-change's own seven new subclasses follow that same precedent.

For each of the seven legacy `*Aspect` types being mirrored, the relevant
value-holding fields and comparison (`matches()`) logic were inspected
directly (not reimplemented wholesale):

- `AttributesAspect` (`superstructure/generic/specs/AttributesAspect.F90`):
  one `StringVector` of attribute names; `matches()` is an asymmetric "src
  provides every name dst requires" subset check, not symmetric equality.
  `make_transform` always returns `NullTransform` (no real adaptation).
- `UngriddedDimsAspect`: one `UngriddedDims` value
  (`infrastructure/esmf/UngriddedDims.F90` — no `StateRegistry`/aspect
  coupling, already has `operator(==)`); `matches()` is plain equality.
  `make_transform` always returns `NullTransform`.
- `QuantityTypeAspect`: `quantity_type` (`MAPL_QuantityType`) and `basis`
  (`MAPL_MixingRatioBasis`) fields participate in `matches()`
  ("equal-or-either-unknown/none"); `dimensions`/`molecular_weight` do not.
  `make_transform` always returns `NullTransform`.
- `ConservationAspect`: one `ConservationMetadata` value
  (`enums/ConservationMetadata.F90` — no `StateRegistry` coupling, already
  has `operator(==)` with its own mirror-aware semantics: both-mirror
  compares equal unconditionally). `make_transform` unconditionally
  `_FAIL`s ("should not be called").
- `NormalizationAspect`: one `NormalizationMetadata` value
  (`enums/NormalizationMetadata.F90` — same no-coupling,
  already-has-`operator(==)`, mirror-aware shape as `ConservationMetadata`).
  `make_transform` always returns `NullTransform`.
- `StandardNameAspect`: one `standard_name` string; `matches()` layers
  wildcard/unchecked acceptance, asymmetric warn-vs-error severity via
  `ValidationMode`, and `pflogger` logging on top of plain equality — real
  `GraphBuilder`-adjacent production-config behavior, not a plain
  value/identity comparison. `make_transform` always returns `NullTransform`.
- `VerticalGridAspect`: the only one of the seven with a real
  (non-placeholder) `make_transform` body (`VerticalGridAspect/
  make_transform.F90`) — but the already-existing, deliberately independent
  `graph/extension-reuse` `VerticalGridCharacteristic.F90` already has a
  graph-native analog of this axis (landed with 4f,
  `vertical-grid-graph-state-item`) and, per that module's own header
  comment, also leaves `build_transform` failing explicitly (`_FAIL`) — no
  real graph-native vertical-regrid Transform exists anywhere yet, legacy's
  real logic notwithstanding.

`StateItemCharacteristicKind.F90`'s `to_string()`/`==`/`/=`/`<` and
`GraphStateItem.F90`'s `characteristic_ordering_table()` are both
module-local, closed `select case`/`if` ladders over currently-registered
kinds; neither requires a new `case` branch for a kind it has no opinion on
(`to_string()`'s `case default` returns `"UNKNOWN"`;
`characteristic_ordering_table()`'s fallback returns the mismatched input's
own order unchanged, per 5a design.md D5) — confirming REQ-CHAR-001/006's
"no structural change elsewhere" guarantee holds for this sub-change without
inspection of their internals beyond this paragraph.

## Goals / Non-Goals

**Goals:**
- Add one new concrete `StateItemCharacteristic` subclass per remaining
  legacy `*Aspect` (seven total), each correctly classified as a
  `ValueCharacteristic` or `ReferenceCharacteristic` per the proposal's
  table, holding the minimal field set needed to reproduce that legacy
  type's own `matches()` comparison as `needs_extension_for` — reusing an
  existing, already-decoupled value type (`UngriddedDims`,
  `ConservationMetadata`, `NormalizationMetadata`) directly where one
  exists, rather than re-deriving its comparison logic by hand.
- Resolve the vertical-grid reference characteristic's final name (collision
  with `graph/extension-reuse`'s existing `VerticalGridCharacteristic`) and
  the `FROM_COMP`-analog question, as concrete, final decisions, before
  implementation starts (same discipline 5a's own design.md followed for
  its four open points).
- Register seven new `StateItemCharacteristicKind` values, stable and never
  renumbered, appended after the three 5a already registered.
- Keep every new subclass exercised by synthetic-node pFUnit coverage only,
  matching 5a's own Phase 1–2 exit-criterion posture even though this change
  lands after Phase 4.

**Non-Goals:**
- Full field-for-field parity with each legacy `*Aspect`. Fields that exist
  on a legacy type but do not participate in its own `matches()` comparison
  (e.g. `QuantityTypeAspect`'s `dimensions`/`molecular_weight`) are not
  carried over — REQ-CHAR-002's job for this hierarchy is mismatch
  description via `needs_extension_for`, not full legacy-aspect-object
  replication.
- `StandardNameCharacteristic` replicating `StandardNameAspect`'s
  `ValidationMode`-driven severity grading or `pflogger` warning/error
  output. That behavior is real `GraphBuilder`/production-config
  integration, the same category of thing 5a's own `PhysicalUnitsCharacteristic`
  already declines to replicate for units' own legacy unchecked/wildcard
  handling (Context, above).
- Any `GraphBuilder` rewire, or wiring these new subclasses into real
  connection resolution. Additive, standalone types only, per 5a's own D6
  boundary.
- Real `build_transform`/adaptation logic beyond what each legacy
  `*Aspect%make_transform` already does unconditionally today (explicit
  `_FAIL` for conservation; `NullTransform` no-op placeholder for the other
  six) — no new subclass in this change gains a real adaptation path that
  does not already exist, graph-native or legacy, for its axis.
- Any change to `GraphStateItem.F90`, `StateItemCharacteristicDetection.F90`,
  or `SetSharedCharacteristic.F90`. REQ-CHAR-001/006's "no structural change
  elsewhere" guarantee is exercised by this change, not amended.
- Any change to legacy `StateRegistry`/`ExtensionFamily`/`ClassAspect` or
  the seven legacy `*Aspect.F90` files themselves.

## Decisions

### D1. Vertical-grid reference characteristic is named `VerticalCoordinateCharacteristic`
The spec's own working note (`20-implementation-roadmap.md` §20.4.4's table)
flags this name as "TBD at design time" because the obvious name,
`VerticalGridCharacteristic`, is an exact, unavoidable collision with the
existing, unrelated `graph/extension-reuse` type of that same name
(`superstructure/generic/graph/VerticalGridCharacteristic.F90`) — Fortran
module/type names share one global namespace, the same constraint already
resolved once for `GeometryCharacteristic` (5a design.md D3) and once before
that for `UnitsConverterTransform`/legacy `ConvertUnitsTransform`.
`VerticalCoordinateCharacteristic` is chosen: distinguishable at a glance,
no abbreviation collision, and consistent with D3's own precedent of
preferring a full, different word over leaning on module-path
disambiguation alone. Module: `mapl_VerticalCoordinateCharacteristic_mod`.

Shape mirrors `GeometryCharacteristic.F90` exactly (extends
`ReferenceCharacteristic`; holds only the inherited `NodeId`; constructor
takes `referenced_node_id`; `needs_extension_for` compares referenced
`NodeId` equality) — vertical-grid shares geometry's own
"sharing is nothing more than two map entries holding the same `NodeId`"
property (REQ-CHAR-012), and nothing about the *reference* half of this
axis differs from geometry's. The richer dimension-overlap comparison
`graph/extension-reuse`'s own `VerticalGridCharacteristic` already
implements (REQ-GEO-007a's three-way classification) is that deliberately
independent family's job, not this one's — same D6 boundary 5a already
drew for geometry.

**Alternative considered:** `VerticalGridReferenceCharacteristic` (keep
"Grid", disambiguate with an explicit "Reference" suffix instead of a
different root word). Rejected: longer, and the "Reference" suffix reads as
redundant next to the base class of the same name
(`ReferenceCharacteristic`) that every sibling in this family already
extends without repeating "Reference" in its own name (`Geometry`
Characteristic, not `GeometryReferenceCharacteristic`) — consistency with
that existing naming pattern favors a plain, different root word instead.

### D2. `AttributesCharacteristic` ports legacy's asymmetric "provides-the-required-set" comparison, not symmetric equality
`AttributesAspect%matches(src, dst)` checks that `src`'s attribute names
include every one of `dst`'s — an asymmetric, direction-sensitive
comparison (an export's name set must be a superset of what an import
requires), unlike every other characteristic in this hierarchy so far
(unit/typekind/geometry/the other five new types here), which all compare
symmetrically. This sub-change ports that exact asymmetric check into
`AttributesCharacteristic%needs_extension_for(this, goal)` — `needs_extension
= .not. (this's names include every name in goal's)` — rather than
simplifying to set equality, because the comparison is a pure,
already-decoupled algorithm (operates only on two `StringVector`s, no
`StateRegistry`/`VariableSpec` coupling) and preserving it is true parity,
not scope creep. `needs_extension_for`'s existing contract (this hierarchy's
one asymmetric case) is documented in the new module's own header comment,
not left implicit.

**Alternative considered:** simplify to symmetric set equality, matching
every sibling characteristic's own comparison shape. Rejected: this would
be a real behavioral regression relative to legacy for the one axis where
asymmetry is the actual documented legacy semantics (`AttributesAspect.F90`'s
own header: "we require that an export provides all attributes that an
import specifies as a shared attribute") — parity-gap closure is this
sub-change's whole purpose; simplifying away a load-bearing asymmetry would
defeat it for this one axis.

### D3. `UngriddedDimsCharacteristic`, `ConservationCharacteristic`, `NormalizationCharacteristic` wrap an existing, already-decoupled value type directly, by composition
`UngriddedDims` (`infrastructure/esmf/UngriddedDims.F90`),
`ConservationMetadata`, and `NormalizationMetadata` (`enums/
ConservationMetadata.F90`, `enums/NormalizationMetadata.F90`) each already:
(a) have no `StateRegistry`/`VariableSpec`/aspect-system coupling — they
live in `infrastructure/esmf/` and `enums/`, depending only on ESMF `Info`
and their own companion type; and (b) already define `operator(==)`/
`operator(/=)` with the exact comparison semantics each legacy Aspect's own
`matches()` uses (including `ConservationMetadata`/`NormalizationMetadata`'s
own mirror-aware "both-mirror compares equal" case). Each new
`ValueCharacteristic` subclass here stores one field of the corresponding
type and implements `needs_extension_for` as a single `/=` call — no
hand-written field-by-field comparison logic is introduced, and no
behavioral drift from legacy's own equality semantics is possible beyond
whatever already exists in the reused type's own `operator(==)`.

**Alternative considered:** re-derive each comparison by hand (store raw
component fields, write a new comparison function), matching
`PhysicalUnitsCharacteristic`'s own shape (a bare string, no composed
value type). Rejected for these three specifically: unlike units (a single
primitive string), `UngriddedDims`/`ConservationMetadata`/
`NormalizationMetadata` are themselves small value types with real,
already-correct equality operators; re-deriving the comparison by hand
would duplicate logic that already exists and is already exercised
elsewhere, for no benefit.

### D4. `QuantityTypeCharacteristic` stores only `quantity_type` and `basis`, not `dimensions`/`molecular_weight`
`QuantityTypeAspect%matches()` only inspects `quantity_type` and `basis`
("match if quantity types and basis match, or if either is unknown/none");
`dimensions` and `molecular_weight` are declared on the legacy aspect but
play no role in its own mismatch comparison. Since REQ-CHAR-002 scopes a
`StateItemCharacteristic` to mismatch description via `needs_extension_for`,
not full legacy-object replication (Goals/Non-Goals, above),
`QuantityTypeCharacteristic` stores only the two fields `matches()` itself
uses, with `needs_extension_for` porting that same
equal-or-either-unknown/none logic verbatim (both are plain `mapl_enums_api`
enum types — `MAPL_QuantityType`/`MAPL_MixingRatioBasis` — no coupling
concern).

**Alternative considered:** carry all four fields for forward-compatibility,
even though two are unused by comparison today. Rejected: REQ-CHAR-002's
own boundary is "describes the characteristic's current value/status" for
the purpose of mismatch detection and adaptation dispatch — carrying fields
with no comparison role and no consumer anywhere in this hierarchy is
speculative scope the proposal's parity-gap framing does not ask for; the
non-goal above already excludes this explicitly.

### D5. `StandardNameCharacteristic` stores a plain string; comparison drops legacy's `ValidationMode`/logging/wildcard nuance in favor of the existing `CharacteristicStatus` escape hatch
`StandardNameAspect%matches()` layers real `GraphBuilder`-adjacent
production behavior on top of plain string equality: an
`is_unchecked()`/wildcard short-circuit, asymmetric accept-with-warning
when only one side declares a name, and `ValidationMode`-gated
strict-vs-permissive severity with `pflogger` output
(`FieldDictionaryConfig`). None of that is a plain value/identity
comparison in the sense this hierarchy's other characteristics use (Context,
above) — it is config-and-logging-coupled connection-resolution behavior,
the same category of thing 5a's own `PhysicalUnitsCharacteristic` already
declines to replicate for units (Context).
`StandardNameCharacteristic` instead stores one `standard_name` string, and
`needs_extension_for` uses the base `StateItemCharacteristic`'s own
`CharacteristicStatus` (already present on every characteristic, REQ-CHAR-003)
as the "accept without comparison" escape — `this`'s or `goal`'s status
equal to `UNCHECKED` means no extension is needed, exactly matching
`UNCHECKED`'s own documented meaning ("connection allowed without a
reconciling Transform even though the characteristic is not confirmed to
match") — rather than inventing a second, type-specific unchecked concept
the way legacy's own `is_unchecked()` does. When neither side is
`UNCHECKED`, comparison is plain string equality — no warning, no
`ValidationMode`, no logging.

**Alternative considered:** replicate `ValidationMode`/logging fully.
Rejected: would require this standalone, synthetic-node-only hierarchy to
depend on `FieldDictionaryConfig`/`pflogger` — production-config and
diagnostic-output coupling of exactly the kind 5a's own design deliberately
kept out of this family (D6, 5a design.md) — for behavior
(severity-graded warnings) that is a `GraphBuilder`-integration concern, not
a plain two-value comparison. Using the already-present `CharacteristicStatus`
status field instead of inventing a parallel per-subclass concept also
keeps the "status carries the match-independent signal, value equality
carries everything else" split REQ-CHAR-003 already established, uniform
across this one case rather than special-cased.

### D6. No `FROM_COMP`-equivalent `CharacteristicStatus` value is added
Legacy `AspectStatus` has a sixth value, `ASPECT_STATUS_FROM_COMP`, absent
from 5a's `CharacteristicStatus` (REQ-CHAR-003's own `[OPEN]` note flags
this gap explicitly and requires this sub-change to decide it, not
discover it mid-implementation). Tracing every live production use
(`VariableSpec.F90`'s `make_GeomAspect`/`make_VerticalGridAspect`,
`StateItemAspect.F90`'s `is_from_component()`) shows `FROM_COMP` means
specifically "this item's geometry/vertical-grid value was not declared on
the item itself, but inherited from the owning component's own
component-wide default" — a `VariableSpec`/advertise-time resolution
distinction between "explicit on this item" vs. "defaulted from the
component," not a characteristic-value mismatch-comparison state at all.
Nothing in this hierarchy (5a or this sub-change) performs that
component-wide-default resolution — `StateItemCharacteristic` instances are
constructed already-resolved, with no equivalent of `VariableSpec`'s own
own-vs-inherited distinction to encode. `FROM_COMP` is therefore a
legacy-only, `VariableSpec`-level distinction with no graph-native analog
needed here — not a gap this sub-change leaves unresolved by omission, but
a decided "does not apply" for the reason stated.

**Alternative considered:** add a sixth `CharacteristicStatus` value
(e.g. `FROM_COMPONENT`) for forward-compatibility with a future
`GraphBuilder`-side advertise-time resolution step that might one day need
it. Rejected: 5a's own D1 already declined to grow this enum speculatively
("adding it speculatively would be exactly the kind of unvalidated enum
growth REQ-CHAR-003's own open note warns against") for a different
candidate value (`CONFLICT`); the same reasoning applies here — no concrete
scenario in this sub-change's own seven subclasses needs it, and a future
change that does can add it without changing any of this one's own code,
since `CharacteristicStatus` is a closed, independently-versioned type.

### D7. `StateItemCharacteristicKind` values 4–10 are registered in table order, immediately after the three 5a already assigned
Per REQ-CHAR-006's "never renumber" discipline (mirroring `REQ-<AREA>-<NNN>`
conventions), the seven new constants take the next available integer
values after 5a's `PHYSICAL_UNITS_CHARACTERISTIC_KIND` (1),
`TYPE_KIND_CHARACTERISTIC_KIND` (2), `GEOMETRY_CHARACTERISTIC_KIND` (3),
leaving `INVALID_CHARACTERISTIC_KIND` (-1) and the test-only
`MOCK_CHARACTERISTIC_KIND` (99) untouched:

| Value | Constant | Subclass |
|---|---|---|
| 4 | `VERTICAL_COORDINATE_CHARACTERISTIC_KIND` | `VerticalCoordinateCharacteristic` |
| 5 | `ATTRIBUTES_CHARACTERISTIC_KIND` | `AttributesCharacteristic` |
| 6 | `UNGRIDDED_DIMS_CHARACTERISTIC_KIND` | `UngriddedDimsCharacteristic` |
| 7 | `QUANTITY_TYPE_CHARACTERISTIC_KIND` | `QuantityTypeCharacteristic` |
| 8 | `CONSERVATION_CHARACTERISTIC_KIND` | `ConservationCharacteristic` |
| 9 | `NORMALIZATION_CHARACTERISTIC_KIND` | `NormalizationCharacteristic` |
| 10 | `STANDARD_NAME_CHARACTERISTIC_KIND` | `StandardNameCharacteristic` |

Assignment order within 4–10 follows the proposal's own table order
(mirroring `20-implementation-roadmap.md` §20.4.4's own listing order), not
any implementation-convenience ordering — arbitrary but stable once chosen,
consistent with REQ-CHAR-006's only real constraint (stable, never reused).

### D8. No change to `characteristic_ordering_table()`
`GraphStateItem.F90`'s existing per-variant ordering table (5a design.md D5)
has entries today only for `MAPL_STATEITEM_FIELD` and `MAPL_STATEITEM_GEOM`,
covering the three 5a kinds; every other variant, and every kind the table
has no opinion on for a variant it does cover, already falls back to
"preserve the mismatched input's own order" (D5's own stable,
non-crashing default). None of this sub-change's seven new kinds is given a
hand-tuned position in either existing table entry — doing so without a
concrete end-to-end comparison scenario driving the choice would be
exactly the kind of speculative tuning 5a design.md D5 itself already
declined ("Q13's own confidence is 'medium'... deliberately kept simple").
The fallback path is sufficient for this sub-change's own goal (each new
kind participates correctly in ordering, even if unordered relative to the
others) and requires zero new code, directly exercising REQ-CHAR-001/006's
"no structural change elsewhere" guarantee this proposal's own Decisions
section documents as the design's expected property, not a deferred task.

## Risks / Trade-offs

- **[Risk]** `AttributesCharacteristic`'s asymmetric comparison (D2) means
  `needs_extension_for(a, b)` and `needs_extension_for(b, a)` can disagree —
  a real, documented departure from every other characteristic in this
  hierarchy, which are all symmetric. **Mitigation**: documented explicitly
  in the new module's own header comment and in this design doc (D2); a
  dedicated pFUnit case exercises both call directions to make the
  asymmetry visible in test output, not just prose.
- **[Risk]** `StandardNameCharacteristic`'s simplified comparison (D5) is a
  real behavioral narrowing relative to legacy — no severity grading, no
  logging, no asymmetric empty-side acceptance. If a future `GraphBuilder`
  integration needs that nuance, it is not available from this type alone.
  **Mitigation**: explicitly scoped as a non-goal (Goals/Non-Goals) and
  recorded as a deliberate boundary (D5), matching the same boundary 5a's
  own `PhysicalUnitsCharacteristic` already accepted for a different axis —
  a future integration change inherits a known, already-precedented
  limitation, not a surprise.
- **[Risk]** Reusing `UngriddedDims`/`ConservationMetadata`/
  `NormalizationMetadata` directly (D3) means this hierarchy's comparison
  behavior for those three axes is only as correct as those types' own
  `operator(==)` — a latent bug there would surface here too.
  **Mitigation**: these types are already independently exercised by their
  own existing test suites; this sub-change's own pFUnit coverage targets
  `needs_extension_for`'s delegation to `/=`, not re-verifying the reused
  operator's own correctness.
