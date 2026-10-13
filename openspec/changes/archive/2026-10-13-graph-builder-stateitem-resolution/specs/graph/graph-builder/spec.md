## MODIFIED Requirements

### Requirement: Ordinary connections are resolved into dependency edges
For each ordinary (exact short-name match, non-wildcard, non-callback)
import/export connection declared between two components, the system
SHALL determine whether the export's payload matches what the import
requires, using only each state item's own already-established
identity and characteristics (set once when that item was advertised),
never by looking up a `VariableSpec` on a different component by name.
If it matches, the system SHALL add a dependency edge, in the
consuming component's default dependency network, directly from the
export's node to the matching import's node, and SHALL materialize a
real field payload on the export's node when the graph mode (see the
graph-mode capability) is `graph` or `both`. If it does not match, the
system SHALL delegate to the extension-reuse capability to interpose a
transform chain rather than wiring the mismatched pair directly, and
SHALL rely on that capability to materialize every link of the
resulting chain - intermediate as well as final - when payload
materialization is enabled, rather than materializing only the chain's
final node itself. This is the real-wiring step and SHALL occur only
once graph mutation is appropriate (see design.md for the exact
lifecycle point) - not merely once activity/need has been determined.

#### Scenario: Matching export and import are wired
- **WHEN** a destination component's import has the same short name as
  a source component's export, an ordinary connection between the two
  components is declared, and the export's payload already matches what
  the import requires
- **THEN** the destination's default dependency network contains a
  dependency edge directly from the export's node to the import's node,
  with no extension chain interposed

#### Scenario: Matching export is materialized with a real field
- **WHEN** a destination component's import has the same short name as
  a source component's export, an ordinary connection between the two
  components is declared, the export's payload already matches what the
  import requires, and the graph mode is `graph` or `both`
- **THEN** the export's node reports a real, allocated field payload,
  rather than the unallocated placeholder it reported before resolution

#### Scenario: Matching export is not materialized in registry mode
- **WHEN** a destination component's import has the same short name as
  a source component's export, an ordinary connection between the two
  components is declared, the export's payload already matches what the
  import requires, and the graph mode is `registry`
- **THEN** the export's node still reports the unallocated placeholder
  payload it reported before resolution, unchanged from today's behavior

#### Scenario: Mismatched export and import are wired through an extension chain
- **WHEN** a destination component's import has the same short name as
  a source component's export, an ordinary connection between the two
  components is declared, and the export's payload does not match what
  the import requires
- **THEN** this capability delegates to the extension-reuse capability
  rather than adding a dependency edge directly from the export's node
  to the import's node

#### Scenario: A multi-step mismatch chain's intermediate links are materialized through the same delegation
- **WHEN** a destination component's import has the same short name as
  a source component's export, an ordinary connection between the two
  components is declared, the export's payload mismatches the import on
  two or more characteristics, and payload materialization is enabled
- **THEN** this capability's delegation to the extension-reuse
  capability results in every intermediate link of the resulting chain
  having a real, allocated payload - not only the chain's own final
  node - without this capability itself needing to materialize any
  intermediate link directly

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

#### Scenario: Resolution succeeds when the originating declaration is no longer reachable
- **WHEN** an ordinary connection's destination import and source export
  were each advertised from a declaration this capability has no way to
  look up again by component name at resolution time (for example, the
  declaration belonged to a component several hierarchy levels away from
  where resolution is running)
- **THEN** resolution still correctly determines whether the two match or
  mismatch, using only the two items' own already-established identity
  and characteristics - resolution's correctness never depends on being
  able to re-reach either side's originating declaration

#### Scenario: Mismatch determination uses each item's own established characteristics
- **WHEN** this capability determines whether a matched export/import
  pair's payloads mismatch
- **THEN** that determination compares the two items' own already-
  established characteristics (set once when each item was advertised),
  not a freshly recomputed value derived from re-reading either
  component's own declaration
