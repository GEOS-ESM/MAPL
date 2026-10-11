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
export's node to the matching import's node, and SHALL materialize a
real field payload on the export's node when the graph mode (see the
graph-mode capability) is `graph` or `both`. If it does not match, the
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

### Requirement: Graph-native state population populates a real ESMF state from resolved graph structure
The system SHALL provide a graph-native equivalent of the legacy
imperative coupler's own state-population step: given a component's
resolved graph structure, it SHALL be able to insert each resolved,
field-typed advertised item's real materialized field into a real
`ESMF_State`, under that item's own short name, resolving an import's
real field by following its dependency edge back to whichever node
actually holds the materialized payload (the matching export directly,
or an extension chain's final node). This population step SHALL only be
invoked as a component's real state producer when the graph mode is
`graph`; in `registry` or `both` mode, it SHALL NOT insert anything into
the real `ESMF_State` the legacy coupler or any `check()`-style assertion
observes.

When the resolved connection is between two of the invoking component's
own children (a sibling-to-sibling connection the invoking component
itself declared, but did not itself advertise either endpoint of), the
system SHALL insert each child's own resolved field into that same
child's own real `ESMF_State` - the state its own GridComp actually
observes - never into the invoking (parent) component's own state. A
child component SHALL NOT be required to have, or exercise, any
awareness of its parent for this insertion to occur; the parent alone
is responsible for reaching into each child's own state.

A sibling connection's export-side resolved payload SHALL be the same
real `ESMF_Field` the export child's own run phase writes a changed
value into, rather than an independent copy - established by aliasing,
never by duplicating, the export child's own real materialized field.
A run-phase value change on the export side SHALL therefore be
observable, through this same resolved structure, on the import side -
whether the import is wired directly to the export or through one or
more extension-chain transform nodes.

#### Scenario: An exact-match import's real field is found via its export
- **WHEN** graph-native state population runs for a component whose
  import is wired by a direct dependency edge to a matching export's
  materialized node
- **THEN** the real field inserted for that import under its own short
  name is the same field object materialized on the export's node

#### Scenario: Graph mode governs whether population touches the real state
- **WHEN** graph-native state population is invoked for a component
  whose graph mode is `registry` or `both`
- **THEN** no item is inserted into the real `ESMF_State` as a result of
  that invocation

#### Scenario: Unresolved import is not populated
- **WHEN** graph-native state population runs for a component with an
  import that has no resolved dependency edge
- **THEN** no entry is inserted into the real `ESMF_State` for that
  import, and no error is raised solely because of its absence

#### Scenario: A sibling connection's resolved field reaches each child's own state
- **WHEN** graph-native state population runs for a parent component
  that declared an ordinary connection between two of its own children,
  and that connection is resolved
- **THEN** the export child's own real `ESMF_State` contains the
  resolved export field under its own short name, and the import
  child's own real `ESMF_State` contains the resolved import field
  under its own short name - neither field is inserted into the
  parent's own real `ESMF_State`

#### Scenario: A child needs no awareness of its parent to be populated
- **WHEN** a sibling connection's resolved field is inserted into a
  child's own real `ESMF_State` by graph-native state population
- **THEN** that insertion requires no accessor, lookup, or other
  reference from the child's own graph or component state back to its
  parent

#### Scenario: A sibling connection's export-side payload is a real alias, not a copy
- **WHEN** a sibling-to-sibling connection's export-side proxy is
  resolved and the export child's own real field has already been
  realized
- **THEN** the proxy's own payload is a named alias of that same real
  `ESMF_Field` object - not a separately allocated field with equal
  attributes

#### Scenario: A run-phase value change on the export side reaches the import side
- **WHEN** the export child's own run phase writes a new value into its
  real export field, and the resolved connection's import side is then
  read (directly, or through an extension-chain transform's
  re-evaluation)
- **THEN** the value observed on the import side reflects the export
  child's own newly written value, converted by any transform the
  connection's extension chain applies

### Requirement: A sibling connection's extension chain is re-evaluated after a run phase
After a component's own run phase completes, for each of that
component's own declared children, the system SHALL re-evaluate every
dependency that child's own advertised import items have on an
extension-chain transform, using the same demand-driven update
mechanism already defined for transform re-evaluation. This
re-evaluation SHALL happen only when the graph mode is `graph`; it
SHALL NOT invoke, construct, or otherwise schedule any method node, and
SHALL NOT cause any GridComp phase to execute a second time.

This requirement covers the re-evaluation mechanism itself, not
whether a run phase's own newly-written value is connected to that
mechanism's own input - whether a changed value actually reaches a
sibling's own extension-chain transform depends on that transform's
own input node already holding the component's real, run-phase-written
field, which is a separate concern from re-evaluation timing.

#### Scenario: Re-evaluation runs after a run phase completes
- **WHEN** a component's own run phase completes while its graph mode
  is `graph`
- **THEN** the system re-evaluates every one of that component's own
  declared children's advertised import items for staleness, using the
  existing demand-driven update mechanism

#### Scenario: Re-propagation never invokes a method node or a GridComp phase
- **WHEN** a component's own run phase completes and re-evaluation
  runs for its own declared children
- **THEN** no method node is invoked, and no GridComp phase executes,
  as a result of that re-evaluation

#### Scenario: Re-propagation is a no-op outside graph mode
- **WHEN** a component's own run phase completes while its graph mode
  is `registry` or `both`
- **THEN** no re-evaluation occurs, and no sibling's own real
  `ESMF_State` is touched as a result
