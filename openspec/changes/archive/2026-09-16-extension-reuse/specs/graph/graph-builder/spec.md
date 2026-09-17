## MODIFIED Requirements

### Requirement: Ordinary connections are resolved into dependency edges
For each ordinary (exact short-name match, non-wildcard, non-callback)
import/export connection declared between two components, the system
SHALL determine whether the export's payload matches what the import
requires. If it matches, the system SHALL add a dependency edge, in the
consuming component's default dependency network, directly from the
export's node to the matching import's node. If it does not match, the
system SHALL delegate to the extension-reuse capability to interpose a
transform chain rather than wiring the mismatched pair directly. This is
the real-wiring step and SHALL occur only once graph mutation is
appropriate (see design.md for the exact lifecycle point) - not merely
once activity/need has been determined.

#### Scenario: Matching export and import are wired
- **WHEN** a destination component's import has the same short name as
  a source component's export, an ordinary connection between the two
  components is declared, and the export's payload already matches what
  the import requires
- **THEN** the destination's default dependency network contains a
  dependency edge directly from the export's node to the import's node,
  with no extension chain interposed

#### Scenario: Mismatched export and import are wired through an extension chain
- **WHEN** a destination component's import has the same short name as
  a source component's export, an ordinary connection between the two
  components is declared, and the export's payload does not match what
  the import requires
- **THEN** this capability delegates to the extension-reuse capability
  rather than adding a dependency edge directly from the export's node
  to the import's node

#### Scenario: Import with no matching export is left unresolved
- **WHEN** a destination component's import has no export of the same
  short name available from the declared source
- **THEN** no dependency edge is created for that import, and the
  unresolved import is reported rather than silently ignored

#### Scenario: Non-exact-match cases are not resolved by this capability
- **WHEN** a connection requires wildcard expansion or callback-style
  binding between export and import
- **THEN** this capability does not create a dependency edge for that
  connection and does not report it as an ordinary-connection failure —
  such connections remain the responsibility of a later resolution step
