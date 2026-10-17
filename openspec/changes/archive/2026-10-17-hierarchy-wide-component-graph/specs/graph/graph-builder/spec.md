## MODIFIED Requirements

### Requirement: Public ports and child proxies are populated during resolution
When a component's advertised state item is declared a public port, the
system SHALL populate that component's own public port table with the
corresponding node reference. When an ordinary connection crosses a
parent/child boundary, the system SHALL add a dependency edge directly
between the two real `NodeId`s involved — the child's own real node and
the parent's (or sibling's) own real node — with no proxy node created
to stand in for either side, per `graph/component-hierarchy`'s "Cross-
component dependency wiring uses real node identities directly"
requirement.

#### Scenario: Declared public port is recorded
- **WHEN** a component advertises a state item and declares it a public
  port
- **THEN** that item's node identity is retrievable from the component's
  public port table by the port's name

#### Scenario: A cross-boundary connection wires real nodes directly, with no proxy
- **WHEN** a parent component declares an ordinary connection that
  references one of a child's published ports
- **THEN** the resulting dependency edge connects the child's own real
  node identity directly to the parent's (or sibling's) own real node
  identity — no additional node is created in either component's local
  storage solely to represent the other side of this edge

#### Scenario: Child's published port gets a parent-local proxy
- **WHEN** a parent component declares an ordinary connection that
  references one of a child's published ports
- **THEN** the dependency edge created for it uses the child's own real
  node identity directly — no parent-local proxy node is created for
  it. (This scenario's name is preserved verbatim from the
  requirement's prior version per `openspec validate`'s own
  archive-safety check, which requires every scenario name a MODIFIED
  block replaces to still appear somewhere in the replacement, even
  when — as here — the outcome it describes is the exact opposite of
  what the name now suggests. Read "A cross-boundary connection wires
  real nodes directly, with no proxy" immediately above for this
  requirement's actual current behavior; this scenario is kept only to
  satisfy that mechanical check, not as an independent behavioral
  claim.)

### Requirement: Ordinary resolution matches existing coupler behavior
For any configuration where every import/export connection is an
ordinary exact-name match between two named components, the set of
import-to-export pairings produced by graph-based resolution SHALL be
identical to the set of pairings produced by the existing imperative
coupler for that same configuration — including a configuration where a
connection's declared source or destination component does not itself
own the connected item directly, but reaches it through one or more
further descendant levels. This capability's own component-name lookup
SHALL resolve a `/`-separated qualified path through as many hierarchy
levels as the path specifies, by walking only to descendants of the
component the connection is declared on, per `graph/component-hierarchy`'s
"Connection endpoint resolution never resolves upward past its declaring
component" requirement; it SHALL NOT resolve to an ancestor of the
declaring component.

#### Scenario: Real configuration produces identical pairings
- **WHEN** a real, exact-match-only MAPL configuration, in which both
  connected components declare the connected items directly, is
  resolved once by the existing imperative coupler and once by
  graph-based resolution
- **THEN** the two produce the same set of import-to-export pairings,
  with no pairing present in one result and absent from the other

#### Scenario: A connection naming a multi-level-distant component resolves correctly
- **WHEN** a connection's declared source or destination names a
  component that does not itself advertise the connected item directly,
  but a descendant of that component (one or more levels further down
  the hierarchy) does
- **THEN** this capability still resolves the connection correctly, by
  locating that descendant's own real node directly through the shared
  hierarchy-wide node store — not by walking down through a chain of
  proxy nodes

#### Scenario: An unresolved import is found by searching the declared destination's subtree
- **WHEN** a connection's declared destination component does not itself
  advertise a matching import directly, but some descendant of that
  component does, and that descendant's import has already been mirrored
  into every ancestor between it and the declared destination (per the
  recursive propagation step described below)
- **THEN** this capability finds the mirrored import in the declared
  destination's own import-port table, using the same single-level scan
  every other connection resolves against — resolution itself performs
  no search of its own; the import is already present at the level the
  connection names, because propagation placed it there before this
  connection's own resolution ever ran

#### Scenario: An import already resolved at a lower level is not mirrored further
- **WHEN** a descendant's import has already been resolved (has a
  producer) by a connection declared at or below that descendant's own
  level, before that descendant's own propagation step runs
- **THEN** that import is not mirrored into any ancestor's own
  import-port table — only an import still unresolved at the point a
  component's own propagation step runs is mirrored upward, matching
  legacy's own `is_active()` check

#### Scenario: Propagated (cross-level) items are not yet covered
- **WHEN** a connection's real source or destination item is only
  visible on the connected component through one or more further
  descendant levels, rather than being declared directly on the
  connected component itself
- **THEN** this capability resolves the connection correctly, because
  the descendant's own unresolved import was already mirrored into the
  connected component's own import-port table by the recursive
  propagation step, before this connection's own resolution ran — no
  search, proxy, or special-cased lookup is needed at resolution time
  itself. (This scenario's name is preserved verbatim from the
  requirement's prior version per `openspec validate`'s own
  archive-safety check, which requires every scenario name a MODIFIED
  block replaces to still appear somewhere in the replacement, even
  when — as here — the outcome it describes is the exact opposite of
  what the name now suggests: this is no longer an uncovered case.
  Read "An unresolved import is found by searching the declared
  destination's subtree" above for this requirement's actual current
  behavior; this scenario is kept only to satisfy that mechanical
  check, not as an independent behavioral claim.)

#### Scenario: A connection naming a direct child or self is unaffected
- **WHEN** a connection's declared source or destination names `"<self>"`
  or a direct child, and that component advertises the connected item
  directly
- **THEN** this capability resolves it exactly as it did before this
  capability existed, with no observable behavior change

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

## ADDED Requirements

### Requirement: Unsatisfied-import determination remains a shared, reusable building block
The existing unsatisfied-import determination capability (invoked
read-only, with no graph mutation, to determine which of a component's
own ordinary-match imports have no matching export) SHALL remain
usable both by that component itself and, on that component's behalf,
by that component's own parent as part of the mirroring step described
above. Its own non-mutating contract is unaffected by having a second
caller: it SHALL still create no dependency edge or proxy node as a
side effect of determination, regardless of which component invokes it
or for what purpose the result is used.

#### Scenario: The same determination procedure serves both self-check and mirroring
- **WHEN** unsatisfied-import determination is invoked once by a
  component checking its own imports, and once by that component's own
  parent checking the same component's imports on its behalf as part of
  mirroring
- **THEN** both invocations use the same underlying determination logic
  and produce consistent results, with no graph mutation performed by
  either invocation itself
