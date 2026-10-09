## ADDED Requirements

### Requirement: An import declaring inout intent is resolved through the ordinary-inout capability, not ordinary single-direction exact-name matching
When resolving a matched destination item whose declaration marks it as
an ordinary inout borrower, this capability SHALL delegate resolution to
the ordinary-inout capability rather than its own ordinary (single
forward-direction, import/export) exact-match resolution. This
capability continues to resolve any destination item that does not
declare inout intent exactly as before — this requirement adds a new,
additive branch to existing match resolution; it does not alter the
exact-match behavior any existing (non-inout) connection already
exercises.

#### Scenario: An inout-declaring destination is handed to ordinary-inout resolution
- **WHEN** a matched destination item's declaration marks it as an
  ordinary inout borrower
- **THEN** this capability's ordinary single-direction exact-name
  resolution does not attempt to resolve it, and the ordinary-inout
  capability's resolution is used instead

#### Scenario: A destination with no declared inout intent is unaffected
- **WHEN** a matched destination item's declaration does not mark it as
  an ordinary inout borrower
- **THEN** this capability resolves it exactly as it did before this
  capability existed, with no observable behavior change
