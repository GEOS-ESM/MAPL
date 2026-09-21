## ADDED Requirements

### Requirement: An import declaring an expected callback interface is resolved through the callback-wiring capability, not ordinary exact-name matching
When resolving a matched destination import whose declared item
expects to satisfy a callback interface, this capability SHALL delegate
resolution to the callback-wiring capability's flattened-namespace
matching rather than its own ordinary (exact short-name match)
resolution. This capability continues to resolve any destination import
that does not declare an expected callback interface exactly as before
- this requirement adds a new, additive branch to existing match
resolution; it does not alter the exact-match behavior any existing
(non-callback) connection already exercises.

#### Scenario: A callback-interface-declaring destination is handed to callback-wiring resolution
- **WHEN** a matched destination import's declared item expects to
  satisfy a callback interface
- **THEN** this capability's ordinary exact-name resolution does not
  attempt to resolve it, and the callback-wiring capability's
  flattened-namespace resolution is used instead

#### Scenario: A destination with no declared callback interface is unaffected
- **WHEN** a matched destination import's declared item does not expect
  to satisfy a callback interface
- **THEN** this capability resolves it exactly as it did before this
  capability existed, with no observable behavior change
