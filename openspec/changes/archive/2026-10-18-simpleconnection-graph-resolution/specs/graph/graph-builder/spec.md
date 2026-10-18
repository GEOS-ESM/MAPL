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
system SHALL add a dependency edge, in the consuming component's
default dependency network, directly from the export's node to the
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

### Requirement: An unresolved import is mirrored into every ancestor's own import-port table, independent of any connection
Before a component's own ordinary connection resolution and
unsatisfied-import determination run, the system SHALL, for every
direct child of that component, determine which of the child's own
advertised imports have no matching export (using the existing,
unmodified unsatisfied-import determination described below, invoked on
the child's behalf) and, for each one, add it as an additional import
port in the parent's own import-port table — the same real `NodeId` the
child's own graph already registered, not a copy. This mirroring SHALL
occur for every direct child unconditionally, independent of whether
any connection declared at the parent's own level names that child.
Because the component hierarchy's own init-phase recursion already
processes every component's children before that component's own
advertise-phase work resumes, a component's own unresolved imports are
already fully mirrored into itself (including any further-propagated
grandchild imports) by the time its own parent's mirroring step runs
against it.

Whether a child's import counts as "already resolved" (and therefore
not mirrored) SHALL be determined by whether that import's node
already has a resolved producer in the graph — never by the mere
presence of a `SimpleConnection` or other declared connection naming
the import's destination by exact name, independent of whether that
connection's own source side actually resolves to a real export. A
declared connection naming a destination that has no real matching
export SHALL NOT, on that basis alone, be treated as having resolved
the import.

#### Scenario: A child's unresolved import becomes visible in the parent's own import-port table
- **WHEN** a child component has an advertised import with no matching
  export at the point the child's own advertise-phase work completes
- **THEN** the parent's own import-port table gains an entry for that
  same import, under the same real `NodeId` the child's own graph
  already registered, with no new node created

#### Scenario: Mirroring requires no connection naming the child
- **WHEN** a parent component has one or more children, and no
  connection declared anywhere in the hierarchy names any of those
  children specifically for the purpose of propagation
- **THEN** each child's own unresolved imports are still mirrored into
  the parent's own import-port table — mirroring is unconditional, not
  triggered by connection declaration

#### Scenario: An already-resolved import is not mirrored
- **WHEN** a child component's advertised import already has a matching
  export (resolved by a connection declared at or below the child's own
  level) at the point the child's own advertise-phase work completes
- **THEN** that import is not mirrored into the parent's own
  import-port table

#### Scenario: A SimpleConnection naming a destination with no real matching export does not suppress mirroring
- **WHEN** a `SimpleConnection` is declared naming a child's import as
  its destination by exact name, but the connection's declared source
  does not actually resolve to a matching export
- **THEN** that import is still treated as unresolved and is still
  mirrored into the parent's own import-port table — the mere presence
  of a connection naming the destination is not, by itself, sufficient
  to treat the import as resolved

#### Scenario: A grandchild's unresolved import reaches a grandparent through two mirroring steps, not one search
- **WHEN** a grandchild component has an unresolved import, and neither
  its parent nor its grandparent declares a connection resolving it
  before each one's own advertise-phase work completes
- **THEN** the import is mirrored first into the parent's own
  import-port table (by the parent's own mirroring step, run against
  the grandchild), and separately mirrored again from the parent's own
  import-port table into the grandparent's own import-port table (by
  the grandparent's own mirroring step, run against the parent) — two
  ordinary single-level mirroring steps, composed by the hierarchy's
  own existing recursion, not one step searching two levels deep

#### Scenario: Multiple distinct descendants bubbling the same short name are each independently resolvable
- **WHEN** two or more distinct descendant components each have their
  own unresolved import of the same short name, mirrored into the same
  ancestor's own import-port table
- **THEN** an ordinary connection declared at that ancestor resolves
  each one independently — including, when the two descendants' own
  advertised items have different characteristics, each one taking its
  own independent match-or-mismatch resolution path, with no
  requirement that distinct bubbled items of the same short name share
  one resolution outcome

#### Scenario: An ordinary connection resolves a mirrored import exactly as it would an item advertised directly
- **WHEN** an ordinary connection is declared at a component naming one
  of that component's own import ports, and that port's entry was
  populated by mirroring (above) rather than by the component's own
  direct advertisement
- **THEN** resolution proceeds identically to the case where the
  component advertised that import directly — no distinguishing
  behavior, no new resolution code path, exists for a mirrored import
  versus a directly-advertised one
