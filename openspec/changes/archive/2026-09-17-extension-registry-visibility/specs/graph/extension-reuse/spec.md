## ADDED Requirements

### Requirement: Payload materialization is disabled unless explicitly enabled
Real payload materialization for a framework-created extension item is
an opt-in capability. Unless explicitly enabled, the system SHALL NOT
perform any real payload allocation for an extension item — an existing
mismatch resolution's structural result (chain creation, reuse
decisions) SHALL be completely unaffected by whether materialization is
enabled or not.

#### Scenario: Materialization disabled by default
- **WHEN** an extension chain is built or reused for a mismatched pair,
  and payload materialization has not been explicitly enabled
- **THEN** the chain's final extension item is left with an unallocated
  placeholder payload, exactly as if this capability did not exist, and
  no real ESMF resource is allocated on its behalf

### Requirement: Framework-created extension items have a real, materialized payload
When payload materialization is explicitly enabled, every
framework-created extension item the system successfully builds or
reuses for a mismatched export/import pair whose underlying item is a
plain field SHALL be materialized as a real, allocated payload, not left
as graph structure only — correct `NodeId`s and dependency edges with no
real, computable payload behind them.

#### Scenario: Resolved extension has a real payload
- **WHEN** payload materialization is enabled and an extension chain is
  built or reused for a mismatched, field-typed export/import pair whose
  mismatch has a registered, executing provider
- **THEN** the chain's final extension item has a real, allocated
  payload — not an empty or unallocated placeholder

#### Scenario: Resolved extension remains discoverable through the existing reuse search
- **WHEN** payload materialization is enabled and an extension chain's
  final item has a real payload
- **THEN** that item remains discoverable through the same reuse-search
  mechanism that locates any other extension item, without requiring a
  separate lookup mechanism

#### Scenario: Reused extension is not re-materialized
- **WHEN** payload materialization is enabled and a second import is
  wired to an already-existing extension item via reuse (no new chain
  created)
- **THEN** no duplicate payload is materialized for that already-real
  extension item

#### Scenario: A non-field item class fails explicitly rather than getting a fabricated payload
- **WHEN** payload materialization is enabled and a mismatched
  export/import pair's underlying item is not a plain field (e.g. a
  vector, bracket, or state-typed item)
- **THEN** the system reports an explicit, distinguishable failure
  identifying the unsupported item class rather than materializing an
  incomplete, incorrect, or empty payload

#### Scenario: An extension with no executing provider is still not given a payload
- **WHEN** payload materialization is enabled and a characteristic
  mismatch has no registered, executing provider (the existing
  "unregistered characteristic fails loudly" case)
- **THEN** no payload is materialized for that connection, consistent
  with the connection failing rather than partially succeeding
