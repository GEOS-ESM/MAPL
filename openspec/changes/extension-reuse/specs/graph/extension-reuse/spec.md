## Purpose

Bridges a mismatched export/import pair (differing in grid, units,
precision, or other declared characteristics) with a framework-created
transform chain instead of wiring the pair directly or silently coercing
the mismatch, and reuses an existing extension for a second import
needing the same variant instead of duplicating it.

## ADDED Requirements

### Requirement: Extension chain bridges a mismatched export/import pair
When an export's payload does not match what a connected import
requires, the system SHALL interpose one or more framework-created
transform steps between the export and the import rather than wiring
them together directly or coercing the mismatch silently.

#### Scenario: Single-step mismatch gets one transform
- **WHEN** an export and its connected import differ in exactly one
  declared characteristic (e.g. units)
- **THEN** the system creates one transform step and one
  framework-created extension item between the export and the import,
  and the import ends up wired to that extension item rather than to
  the original export

#### Scenario: Multi-step mismatch gets a chain
- **WHEN** an export and its connected import differ in more than one
  declared characteristic
- **THEN** the system creates a chain of transform steps and
  intermediate extension items sufficient to resolve every differing
  characteristic, with the import wired to the chain's final extension
  item

### Requirement: Import item remains distinct but shares extension payload
The import SHALL remain its own distinct item, retrievable by its own
identity, but SHALL NOT be independently materialized when it is fed by
an extension chain — its payload SHALL be the same payload as the
chain's final extension item.

#### Scenario: Import identity is preserved
- **WHEN** an import is fed through an extension chain
- **THEN** the import is still retrievable by its own identity, distinct
  from the export's identity and from any extension item's identity

#### Scenario: Import payload is not independently allocated
- **WHEN** an import is fed through an extension chain
- **THEN** the import's payload is the same underlying payload as the
  chain's final extension item, not a separately allocated copy

### Requirement: Exact match requires no extension
If an export's payload already matches what the import requires
exactly, the system SHALL NOT create any extension item or transform
step for that connection.

#### Scenario: No-op case creates nothing
- **WHEN** an export's payload matches what a connected import requires
  exactly, with no mismatch in any declared characteristic
- **THEN** no extension item or transform step is created for that
  connection, and the import ends up wired to the export directly

### Requirement: Existing extensions are reused before creating a new one
Before creating a new extension item for a given export, the system
SHALL search the extensions already created from that export for one
whose payload already matches what the new import requires, and reuse
it instead of creating a duplicate.

#### Scenario: Second importer needing the same variant reuses the first extension
- **WHEN** a second import, needing the same variant of an export as an
  already-resolved first import, is resolved
- **THEN** the second import is wired to the same extension item the
  first import uses, and no duplicate extension item or transform step
  is created

#### Scenario: Different variant still creates a new extension
- **WHEN** a second import needs a different variant of the same export
  than any extension already created from that export
- **THEN** the system creates a new extension item for the second
  import without disturbing any existing extension already wired to
  other importers

### Requirement: A characteristic mismatch with no registered extension provider fails explicitly
Building an extension step for a mismatched characteristic SHALL go
through a framework-registered provider for that characteristic. If no
provider is registered for a characteristic a mismatch was detected on,
the system SHALL report an explicit, distinguishable failure for that
connection rather than wiring the pair directly, silently ignoring the
mismatch, or reporting it the same way as an unresolved import (no
matching export at all).

#### Scenario: Unregistered characteristic fails loudly
- **WHEN** an export/import pair mismatches on a characteristic with no
  registered extension provider
- **THEN** the system reports an explicit failure identifying the
  unsupported characteristic, distinguishable from "import has no
  matching export," and does not wire the import to the export directly

#### Scenario: Registered characteristic succeeds even when others are not
- **WHEN** an export/import pair mismatches only on characteristics that
  all have registered extension providers
- **THEN** the system builds the extension chain normally, regardless of
  how many or how few characteristics currently have a registered
  provider

### Requirement: Reuse search does not change which extensions get reused
Restating the reuse search as a graph traversal SHALL NOT change, for
any configuration also resolvable by the existing extension-reuse
algorithm, which extension ends up reused versus newly created.

#### Scenario: Same reuse decision as the existing algorithm
- **WHEN** a configuration's export/import mismatches are resolved once
  by the existing (non-graph) extension-reuse algorithm and once by this
  capability's graph-based search
- **THEN** both produce the same set of extension items and the same
  import-to-extension pairings
