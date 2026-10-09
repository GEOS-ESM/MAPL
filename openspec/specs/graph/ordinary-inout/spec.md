# Ordinary Inout Specification

## Purpose

Lets a component declare an ordinary (non-callback) inout state item that
borrows another component's item, and wires the degenerate case where the
borrower's required payload exactly matches the owner's — no new field
storage or transform in either direction.

## Requirements

### Requirement: An item can be declared as an ordinary inout borrower of an owner item
The system SHALL support declaring a state item as an ordinary inout
borrower, identifying exactly one owner item it borrows from. A borrower
declaration without an identifiable owner SHALL be rejected rather than
silently treated as an ordinary import.

#### Scenario: A borrower declaration identifies its owner
- **WHEN** a component declares a state item as an ordinary inout
  borrower of a specific owner item
- **THEN** that borrower's declaration is retrievable afterward together
  with the identity of the owner item it borrows from

#### Scenario: A borrower declaration with no identifiable owner is rejected
- **WHEN** a state item is declared as an ordinary inout borrower but no
  owner item can be identified for it
- **THEN** the declaration is rejected and reported rather than silently
  accepted as an ordinary import

### Requirement: A borrower whose required payload exactly matches its owner's is wired with no transform in either direction
When a declared inout borrower's required payload exactly matches its
owner's (no grid, units, precision, or other characteristic mismatch),
the system SHALL wire the pairing by adding a dependency edge directly
from the owner's node to the borrower's node, and a second dependency
edge directly from the borrower's node to the owner's node, each in its
own dependency network (forward and return, respectively). The system
SHALL NOT create any new field storage or `TransformGraphNode` for
either edge.

#### Scenario: Matching borrower and owner are wired in both directions with no transform
- **WHEN** a declared inout borrower's required payload exactly matches
  its owner's
- **THEN** a forward-network dependency edge exists directly from the
  owner's node to the borrower's node, a return-network dependency edge
  exists directly from the borrower's node to the owner's node, and
  neither edge has a `TransformGraphNode` interposed

#### Scenario: Owner and borrower share one underlying payload
- **WHEN** a declared inout borrower's required payload exactly matches
  its owner's and wiring completes
- **THEN** the borrower's item and the owner's item reference the same
  underlying payload rather than independently materialized storage

### Requirement: Forward and return edges for one inout pairing are never part of the same update pass
For a direct-alias inout pairing, the system SHALL ensure the
forward-network edge (owner to borrower, read before the borrower
executes) and the return-network edge (borrower to owner, written after
the borrower executes) are never both resolved within the same update
pass, mirroring the existing non-overlapping-pass guarantee already
granted to the callback get/put pattern.

#### Scenario: Forward read precedes borrower execution
- **WHEN** an inout borrower is about to execute
- **THEN** the forward-network edge has already been resolved, making
  the owner's current value visible to the borrower before it executes

#### Scenario: Return write follows borrower execution
- **WHEN** an inout borrower has finished executing
- **THEN** the return-network edge is resolved afterward, so the owner's
  node reflects the borrower's result, and this resolution is not part
  of the same update pass as the forward-network read that preceded
  execution

### Requirement: A chained borrowing pairing is rejected
If a declared inout borrower's owner item is itself a borrower in
another declared inout pairing, the system SHALL reject the pairing
explicitly rather than attempting to resolve a chain. Recursive
(chained) borrowing is unspecified behavior reserved for a future design
addendum, same as the general (non-direct-alias) case.

#### Scenario: A borrower-of-a-borrower pairing is rejected
- **WHEN** a declared inout borrower's owner item is itself declared as
  a borrower in another inout pairing
- **THEN** the pairing is rejected and reported as unsupported, rather
  than resolved as a chain

### Requirement: A declaration combining inout borrower intent with an expected callback interface is rejected
An item declared both an ordinary inout borrower and an expected
callback interface (`graph/callback-wiring`) SHALL be rejected
explicitly rather than resolved through either capability alone. These
are two separate, mutually exclusive resolution paths; silently
resolving through one would silently ignore the other declaration.

#### Scenario: A combined declaration is rejected rather than silently resolved one way
- **WHEN** a destination item is declared both an ordinary inout borrower
  and an expected callback interface
- **THEN** the declaration is rejected and reported as unsupported,
  rather than resolved through the callback-wiring capability or the
  ordinary-inout capability alone

### Requirement: A mismatched inout pairing is rejected, not resolved through an extension chain
When a declared inout borrower's required payload does NOT exactly match
its owner's, the system SHALL reject the pairing explicitly rather than
interposing an extension chain or any other transform. Non-direct-alias
inout support is deferred to a future design addendum and MUST NOT be
attempted by this capability.

#### Scenario: A mismatched pairing is rejected rather than silently chained
- **WHEN** a declared inout borrower's required payload does not exactly
  match its owner's
- **THEN** the pairing is rejected and reported as unsupported, and no
  extension chain or transform is created for it
