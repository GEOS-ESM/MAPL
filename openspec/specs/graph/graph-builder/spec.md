# Graph Builder Specification

## Purpose

Integrates the graph-neutral `ComponentGraph`/`DependencyNetwork` core with
the rest of MAPL by turning advertised state items and ordinary
(exact-name-match) import/export connections into graph structure, so a
component's wiring becomes queryable and updateable through its local
graph instead of only through the legacy imperative coupler.

## Requirements

### Requirement: Advertising creates graph state-item nodes
For every state item advertised to a component (import, export, or
internal), the system SHALL create exactly one corresponding node in that
component's local dependency graph, retrievable afterward by the item's
identity.

#### Scenario: Advertised import is represented in the graph
- **WHEN** a component advertises an import variable
- **THEN** the component's local dependency graph contains exactly one
  node representing that import, queryable by the variable's identity

#### Scenario: Advertised export is represented in the graph
- **WHEN** a component advertises an export variable
- **THEN** the component's local dependency graph contains exactly one
  node representing that export, queryable by the variable's identity

#### Scenario: Re-advertising the same item does not duplicate its node
- **WHEN** a state item already advertised to a component is advertised
  again with unchanged identity
- **THEN** the component's local dependency graph still contains exactly
  one node for that item, not two

### Requirement: Unsatisfied-import determination happens without graph mutation
Before real connection resolution occurs, the system SHALL be able to
determine which ordinary-match imports have no matching export, for
reporting purposes, without creating any dependency edge or proxy node.
This mirrors the existing MAPL lifecycle's own two-step connection
handling: an early step that only determines which imports/exports are
active (used to decide whether an unresolved import must be propagated to
a parent component, and whether an export needs to be realized at all),
followed by a later step that performs the real wiring.

#### Scenario: Unresolved determination does not mutate the graph
- **WHEN** the system determines that an ordinary-match import has no
  matching export, before real connection resolution has run
- **THEN** the import is reported as unresolved, and no dependency edge or
  proxy node is created as a side effect of that determination

#### Scenario: Unresolved determination agrees with real resolution
- **WHEN** an ordinary connection is evaluated first for unsatisfied-import
  determination and later for real resolution, with no change to
  advertised items in between
- **THEN** both steps agree on which imports are unresolved

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

### Requirement: Resolution creates or reuses the default dependency network
Ordinary connection resolution for a component SHALL occur in that
component's default dependency network, creating it first if the
component's local graph does not already have one.

#### Scenario: First resolution creates the default network
- **WHEN** ordinary connection resolution runs for a component whose local
  graph has no dependency network yet
- **THEN** afterward the component's local graph has exactly one dependency
  network containing the resolved edges

#### Scenario: Later resolution reuses the existing default network
- **WHEN** ordinary connection resolution runs again for a component that
  already has a default dependency network
- **THEN** new edges are added to that same network rather than a second
  network being created

### Requirement: Public ports and child proxies are populated during resolution
When a component's advertised state item is declared a public port, or
when an ordinary connection crosses a parent/child boundary, the system
SHALL populate the parent's public port table and child-proxy storage
(established by the component-hierarchy capability) with the corresponding
node references.

#### Scenario: Declared public port is recorded
- **WHEN** a component advertises a state item and declares it a public
  port
- **THEN** that item's node identity is retrievable from the component's
  public port table by the port's name

#### Scenario: Child's published port gets a parent-local proxy
- **WHEN** a parent component declares an ordinary connection that
  references one of a child's published ports
- **THEN** the parent's local graph contains a proxy node standing in for
  that child port, reachable only through the parent's own graph, with no
  reference to the child's internal node or network identities

### Requirement: Resolved graph structure is validated and frozen
After ordinary connection resolution for a component completes, the
system SHALL validate the resulting dependency network and, when
validation succeeds, freeze the component's local graph so structure
cannot change afterward.

#### Scenario: Successful resolution is followed by freeze
- **WHEN** ordinary connection resolution for a component completes with
  no validation errors
- **THEN** the component's local graph reports itself frozen afterward

#### Scenario: Validation failure prevents freeze
- **WHEN** resolution would produce a dependency network that fails
  validation (for example, two producers for the same state item within
  one update pass)
- **THEN** the component's local graph is not frozen and the failure is
  reported rather than silently discarded

#### Scenario: Real resolution tolerates being invoked more than once
- **WHEN** real connection resolution for a component runs again after
  that component's local graph has already been successfully frozen by an
  earlier resolution
- **THEN** the repeat invocation succeeds as a no-op rather than failing or
  attempting to mutate the already-frozen graph

### Requirement: Ordinary resolution matches existing coupler behavior
For any configuration where every import/export connection is an ordinary
exact-name match between two components that each declare the connected
item directly (not merely visible on one of them through cross-level
propagation from a further descendant), the set of import-to-export
pairings produced by graph-based resolution SHALL be identical to the set
of pairings produced by the existing imperative coupler for that same
configuration.

#### Scenario: Real configuration produces identical pairings
- **WHEN** a real, exact-match-only MAPL configuration, in which both
  connected components declare the connected items directly, is resolved
  once by the existing imperative coupler and once by graph-based
  resolution
- **THEN** the two produce the same set of import-to-export pairings, with
  no pairing present in one result and absent from the other

#### Scenario: Propagated (cross-level) items are not yet covered
- **WHEN** a connection's real source or destination item is only visible
  on the connected component through `StateRegistry`'s cross-level
  export/import propagation from a further descendant, rather than being
  declared directly on the connected component itself
- **THEN** this capability does not claim equivalence with the existing
  imperative coupler for that connection — it is out of scope for this
  capability, not silently mismatched

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
