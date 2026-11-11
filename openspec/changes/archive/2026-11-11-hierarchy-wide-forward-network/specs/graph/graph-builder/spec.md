## MODIFIED Requirements

### Requirement: Ordinary connections are resolved into dependency edges
For each ordinary (exact short-name match) import/export connection
declared between two components — whether declared directly as a
`SimpleConnection` or produced by expanding a `MatchConnection`'s
regex pattern against the destination's own import-port table, one
exact-name pair at a time — the system SHALL determine whether the
export's payload matches what the import requires, using only each
state item's own already-established identity and characteristics
(set once when that item was advertised), never by looking up a
`VariableSpec` on a different component by name. If it matches, the
system SHALL add a dependency edge, in the hierarchy's
forward dependency network, directly from the export's node to the
matching import's node, and SHALL materialize a real field payload on
the export's node when the graph mode (see the graph-mode capability)
is `graph` or `both`. If it does not match, the system SHALL delegate
to the extension-reuse capability to interpose a transform chain
rather than wiring the mismatched pair directly, and SHALL rely on
that capability to materialize every link of the resulting chain -
intermediate as well as final - when payload materialization is
enabled, rather than materializing only the chain's final node itself.
This is the real-wiring step and SHALL occur only once graph mutation
is appropriate (see design.md for the exact lifecycle point) - not
merely once activity/need has been determined.

A declared `SimpleConnection` is resolved by this same determination
and wiring logic as a `MatchConnection`'s own single matched pair —
there is no separate resolution path, no separate dependency-edge
construction, and no separate materialization/extension-chain
treatment for one versus the other. `MatchConnection` is the
pattern-matching generalization over `SimpleConnection`'s exact-match
primitive: expanding a regex against the destination's import-port
table reduces each match to the same exact-name resolution a declared
`SimpleConnection` already exercises directly.

#### Scenario: Matching export and import are wired
- **WHEN** a destination component's import has the same short name as
  a source component's export, an ordinary connection between the two
  components is declared, and the export's payload already matches what
  the import requires
- **THEN** the forward dependency network contains a
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

#### Scenario: A declared SimpleConnection is resolved exactly as an equivalent MatchConnection match would be
- **WHEN** a `SimpleConnection` is declared naming an exact source and
  destination, and a `MatchConnection` whose pattern matches only that
  same exact pair is declared instead, for the same destination's
  import
- **THEN** both produce the same dependency-edge-or-unresolved outcome,
  the same materialization behavior, and the same extension-chain
  delegation when the export and import mismatch - no distinguishing
  behavior exists between the two declaration forms once each resolves
  to the same single exact-name pair

#### Scenario: Non-exact-match cases are not resolved by this capability
- **WHEN** a connection requires wildcard expansion or callback-style
  binding between export and import, or is a `ReexportConnection`
  (export-to-export passthrough, not an import/export pairing)
- **THEN** this capability does not create a dependency edge for that
  connection and does not report it as an ordinary-connection failure —
  such connections remain the responsibility of a later resolution step
  or a separate capability not covered here

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

### Requirement: A sibling connection's extension chain is re-evaluated after a run phase
Before a component's own run method executes, and only when the graph
mode is `graph`, the system SHALL bring each of that component's own
advertised import items up to date, using the demand-driven update
mechanism on the forward network. This SHALL reach the import's
producer wherever the edge was declared (by the component's parent, or
by any ancestor), so an import fed through an extension chain declared
several levels up is re-evaluated as well. This re-evaluation SHALL NOT
invoke, construct, or otherwise schedule any method node, and SHALL NOT
cause any GridComp phase to execute a second time.

This requirement covers the re-evaluation mechanism itself, not
whether a run phase's own newly-written value is connected to that
mechanism's own input - whether a changed value actually reaches a
consumer's extension-chain transform depends on that transform's own
input node already holding the producing component's real,
run-phase-written field, which is a separate concern from re-evaluation
timing.

#### Scenario: Re-evaluation runs after a run phase completes
- **WHEN** a component's run phase completes, advancing its exports, and
  a consumer of one of those exports is next about to run, while the
  graph mode is `graph`
- **THEN** the consumer first re-evaluates every one of its own
  advertised import items for staleness, using the existing
  demand-driven update mechanism, so it sees the producer's new value

#### Scenario: An import connected by a grandparent is re-evaluated
- **WHEN** a connection declared on a grandparent feeds a grandchild's
  import through an extension chain, the chain's source export has
  changed, and the grandchild's run phase is about to execute
- **THEN** the chain's transform executes and the grandchild's import
  holds the converted current value before its run method starts

#### Scenario: Re-propagation never invokes a method node or a GridComp phase
- **WHEN** a component's imports are re-evaluated before its run phase
- **THEN** no method node is invoked, and no GridComp phase executes,
  as a result of that re-evaluation

#### Scenario: Re-propagation is a no-op outside graph mode
- **WHEN** a component's run phase executes while its graph mode is
  `registry` or `both`
- **THEN** no re-evaluation occurs, and no real `ESMF_State` is touched
  as a result

## REMOVED Requirements

### Requirement: Resolution creates or reuses the default dependency network
**Reason**: Components no longer have a default network of their own.
All ordinary edges go into the hierarchy's one forward network
(`graph/component-hierarchy`, "One forward network carries all ordinary
data flow").
**Migration**: Code that asked a component graph for its default network
asks for the forward network instead.
