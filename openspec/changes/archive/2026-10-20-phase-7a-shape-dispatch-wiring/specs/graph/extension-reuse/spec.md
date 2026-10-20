## MODIFIED Requirements

### Requirement: Framework-created extension items have a real, materialized payload
When payload materialization is explicitly enabled, every
framework-created extension item the system successfully builds or
reuses for a mismatched export/import pair whose underlying item is a
plain field, or whose mismatch is resolved entirely through the
shape-dispatch characteristic for a bracket export resolving into a
field import, SHALL be materialized as a real, allocated payload, not
left as graph structure only — correct `NodeId`s and dependency edges
with no real, computable payload behind them. This applies to every
intermediate item in a multi-step chain, not only the chain's own final
item: a chain resolving two or more mismatched characteristics SHALL
have every link's own item materialized with its own correctly-evolved
typekind, units, and geometry — the state after the
immediately-preceding link's own conversion has notionally applied —
before the next link's own conversion runs against it. A chain whose
mismatch set includes the shape-dispatch characteristic for a
bracket-to-field pairing SHALL have its final item materialized as a
field, not the export's own bracket shape. A vector-bracket or vector
export/import pair remains outside this requirement's scope (see the
"non-field, non-shape-convertible item class" scenario below) — its own
materialization is a distinct, not-yet-addressed gap.

#### Scenario: Resolved extension has a real payload
- **WHEN** payload materialization is enabled and an extension chain is
  built or reused for a mismatched, field-typed export/import pair whose
  mismatch has a registered, executing provider
- **THEN** the chain's final extension item has a real, allocated
  payload — not an empty or unallocated placeholder

#### Scenario: Every intermediate link of a multi-step chain has a real payload
- **WHEN** payload materialization is enabled and an extension chain
  resolving two or more mismatched characteristics is built for a
  field-typed export/import pair whose mismatches all have registered,
  executing providers
- **THEN** every intermediate item in the chain — not only the chain's
  own final item — has a real, allocated payload, each reflecting that
  link's own correctly-evolved typekind, units, and geometry rather than
  an unallocated placeholder or the original export's own unmodified
  payload

#### Scenario: A chain of three or more links executes end to end
- **WHEN** payload materialization is enabled and a chain resolves three
  or more mismatched characteristics, each with a registered, executing
  provider
- **THEN** every transform step in the chain can read a real, allocated
  payload from its own predecessor link and write a real, allocated
  payload to its own successor link, with no step failing because an
  intermediate link's payload was left unallocated

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

#### Scenario: A reused chain's intermediate links are not re-materialized
- **WHEN** payload materialization is enabled and a second import is
  wired to an already-existing extension chain via reuse (no new chain
  created)
- **THEN** no duplicate payload is materialized for any already-real
  item in that chain, intermediate or final

#### Scenario: A non-field item class fails explicitly rather than getting a fabricated payload
- **WHEN** payload materialization is enabled and a mismatched
  export/import pair's underlying item is not a plain field and is not
  a bracket export resolving into a field import through the
  shape-dispatch characteristic (e.g. a state-typed item, a
  vector-bracket or vector item of any kind, or a bracket item whose
  mismatch set does not include the shape-dispatch characteristic)
- **THEN** the system reports an explicit, distinguishable failure
  identifying the unsupported item class rather than materializing an
  incomplete, incorrect, or empty payload — for any link in the chain,
  intermediate or final

#### Scenario: A bracket export resolved through shape dispatch is materialized as a field
- **WHEN** payload materialization is enabled and a bracket-shaped
  export's mismatch with a field-typed import is resolved through the
  shape-dispatch characteristic
- **THEN** the chain's final extension item has a real, allocated field
  payload — not the export's own bracket (field-bundle) shape, and not
  an unallocated placeholder

#### Scenario: An extension with no executing provider is still not given a payload
- **WHEN** payload materialization is enabled and a characteristic
  mismatch has no registered, executing provider (the existing
  "unregistered characteristic fails loudly" case)
- **THEN** no payload is materialized for that connection, consistent
  with the connection failing rather than partially succeeding — and no
  later link in the chain is materialized either, since chain
  construction stops at the first unsupported characteristic
