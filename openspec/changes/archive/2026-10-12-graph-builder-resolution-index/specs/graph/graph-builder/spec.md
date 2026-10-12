## MODIFIED Requirements

### Requirement: Ordinary resolution matches existing coupler behavior
For any configuration where every import/export connection is an
ordinary exact-name match between two named components, the set of
import-to-export pairings produced by graph-based resolution SHALL be
identical to the set of pairings produced by the existing imperative
coupler for that same configuration — including a configuration where
a connection's declared source or destination component does not
itself own the connected item directly, but reaches it through one or
more further descendant levels. This capability's own component-name
lookup SHALL resolve a `/`-separated qualified path through as many
hierarchy levels as the path specifies, not only `"<self>"` or a
single direct child.

#### Scenario: Real configuration produces identical pairings
- **WHEN** a real, exact-match-only MAPL configuration, in which both
  connected components declare the connected items directly, is
  resolved once by the existing imperative coupler and once by
  graph-based resolution
- **THEN** the two produce the same set of import-to-export pairings,
  with no pairing present in one result and absent from the other

#### Scenario: A connection naming a multi-level-distant component resolves correctly
- **WHEN** a connection's declared source or destination names a
  component that does not itself advertise the connected item
  directly, but a descendant of that component (one or more levels
  further down the hierarchy) does
- **THEN** this capability still resolves the connection correctly,
  by walking down from the named component to the descendant that
  actually owns the item - the same outcome a single-level-only lookup
  would only achieve if the item happened to be advertised directly on
  the named component itself

#### Scenario: Propagated (cross-level) items are not yet covered
- **WHEN** a connection's real source or destination item is only
  visible on the connected component through one or more further
  descendant levels, rather than being declared directly on the
  connected component itself
- **THEN** this capability resolves the connection correctly by
  walking down to the descendant that owns the item. (This scenario's
  name is preserved verbatim from the requirement's prior version per
  `openspec validate`'s own archive-safety check, which requires every
  scenario name a MODIFIED block replaces to still appear somewhere in
  the replacement, even when - as here - the outcome it describes is
  the exact opposite of what the name now suggests. Read the scenario
  immediately above, "A connection naming a multi-level-distant
  component resolves correctly," for this requirement's actual current
  behavior; this scenario is kept only to satisfy that mechanical
  check, not as an independent behavioral claim.)

#### Scenario: A connection naming a direct child or self is unaffected
- **WHEN** a connection's declared source or destination names `"<self>"`
  or a direct child, and that component advertises the connected item
  directly
- **THEN** this capability resolves it exactly as it did before this
  capability existed, with no observable behavior change


