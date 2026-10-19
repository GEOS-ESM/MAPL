## ADDED Requirements

### Requirement: Mismatch detection covers all established StateItemCharacteristic axes, not only units, type-kind, and vertical-coordinate
When determining whether a matched export/import pair's payloads
mismatch, the system SHALL compare every characteristic axis each item
has established at advertise time — not only physical units, type-kind,
and vertical-coordinate. This includes, at minimum, the attributes,
ungridded-dimensions, quantity-type, conservation, normalization, and
standard-name axes. A pair that matches on every axis SHALL be wired
directly, with no extension chain created, exactly as an exact-match
pair is wired today. A pair that mismatches on one or more of these axes
SHALL be routed through the extension-reuse capability's existing
chain-building mechanism the same way a units or type-kind mismatch is
routed today — no separate resolution path is introduced for these
axes.

#### Scenario: Matching declarations on a newly-covered axis are wired directly
- **WHEN** a destination component's import and a matching export agree
  on every one of the attributes, ungridded-dimensions, quantity-type,
  conservation, normalization, and standard-name axes
- **THEN** the pair is wired directly, with no extension chain created,
  the same as an exact-match pair that has no characteristics mismatch
  at all

#### Scenario: A mismatch on a newly-covered axis is detected
- **WHEN** a destination component's import and a matching export
  disagree on one of the attributes, ungridded-dimensions,
  quantity-type, conservation, normalization, or standard-name axes
- **THEN** the system detects the mismatch and delegates to the
  extension-reuse capability, the same as it already does for a units or
  type-kind mismatch

#### Scenario: A mismatch on an axis with no real adaptation fails explicitly
- **WHEN** resolution attempts to build an extension chain for a
  mismatch detected on one of the attributes, ungridded-dimensions,
  quantity-type, conservation, normalization, or standard-name axes
- **THEN** the attempt fails explicitly, reporting that axis as an
  unsupported characteristic — the same outcome the system already
  produces for any other characteristic axis with no registered
  adaptation — and no dependency edge is silently created in place of
  the failed chain
