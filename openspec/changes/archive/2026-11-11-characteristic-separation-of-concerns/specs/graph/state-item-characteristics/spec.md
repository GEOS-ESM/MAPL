## MODIFIED Requirements

### Requirement: Transform-insertion order is delegated to a per-kind strategy
When two or more characteristics mismatch, the order in which their
reconciling Transforms are chained SHALL be determined by the
`ItemShapeCharacteristic` of the current extension-chain tip, through
`get_extension_order(goal)`. After each step the tip's mismatched kind is
replaced by the goal's value, and the order is re-queried on the new tip,
so a step that changes shape (for example BRACKET to FIELD) causes the
remaining kinds to be ordered by the new shape. No other characteristic
subclass acts as an arbiter over the others. Mismatched kinds not listed
by the shape SHALL follow the listed ones in a fixed, deterministic order.

#### Scenario: Ordering is independent of detection order
- **WHEN** two or more characteristics are found mismatched, in some
  arbitrary iteration order
- **THEN** the order in which their reconciling Transforms are chained is
  determined by the tip's shape order, not by the order mismatches
  happened to be discovered in

#### Scenario: Two different kinds may order the same pair of characteristics differently
- **WHEN** two chain tips of different shapes both mismatch on the same
  two characteristics
- **THEN** each shape's own order determines that tip's chaining order
  independently; one shape's order is not assumed to apply to the other

#### Scenario: Field order follows legacy
- **WHEN** a FIELD export mismatches an import on GEOMETRY, PHYSICAL_UNITS
  and TYPE_KIND
- **THEN** the chain order is GEOMETRY, then PHYSICAL_UNITS, then
  TYPE_KIND

#### Scenario: Bracket orders units after the shape step
- **WHEN** a BRACKET export mismatches a FIELD import on ITEM_SHAPE and
  PHYSICAL_UNITS
- **THEN** the ITEM_SHAPE step comes first, and the PHYSICAL_UNITS step
  is ordered by the FIELD order of the new tip

## ADDED Requirements

### Requirement: Subclasses never interpret status
Concrete characteristic subclasses SHALL NOT read or branch on
`CharacteristicStatus` when comparing or converting values. Status
semantics (MIRRORED, UNCHECKED, DEFERRED, INVALID) SHALL be handled only by
the base class and the descriptor (`graph/state-item-descriptor`, "Status
resolution is generic and table-driven"). The one exception is
`match_warning`, which only reports on a pair the descriptor already
resolved as a match and may read both statuses to choose its text.

#### Scenario: StandardName has no status check of its own
- **WHEN** `StandardNameCharacteristic%needs_extension_for` is called
- **THEN** both arguments are SPECIFIED, and the method compares values
  only

### Requirement: A kind present on only one side is not a mismatch
Mismatch detection SHALL compare only kinds present on both the export
(or chain tip) and the goal. Absence means the kind is not applicable to
that shape; cross-shape differences are reconciled by the ITEM_SHAPE
step.

#### Scenario: Vector bracket without standard name
- **WHEN** a VECTORBRACKET export has no STANDARD_NAME entry and a FIELD
  import has a SPECIFIED STANDARD_NAME
- **THEN** STANDARD_NAME is not reported as mismatched

### Requirement: A standard-name disagreement follows the validation mode
Two different SPECIFIED standard names SHALL need an extension only in
strict validation mode, where `build_transform` reports the pair
incompatible. In permissive mode (the default) the pair SHALL match, and
`match_warning` SHALL return a warning naming both names, unless
`suppress_standard_name_warnings` is set. An UNCHECKED export meeting a
SPECIFIED import SHALL also warn, in either mode. A FIELD and a BRACKET
carry the kind.

#### Scenario: Permissive mismatch is a match with a warning
- **WHEN** the validation mode is permissive and the export declares
  `air_temperature` while the import declares `surface_temperature`
- **THEN** the pair resolves as a match and its `match_warning` names both
  names

#### Scenario: Strict mismatch is incompatible
- **WHEN** the validation mode is strict and the two names differ
- **THEN** the pair needs an extension and `build_transform` reports
  `BUILD_TRANSFORM_INCOMPATIBLE`

#### Scenario: Suppressed warnings
- **WHEN** `suppress_standard_name_warnings` is set
- **THEN** `match_warning` returns `''` for every standard-name pair
