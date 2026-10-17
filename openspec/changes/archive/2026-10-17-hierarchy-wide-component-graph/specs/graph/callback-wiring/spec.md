## MODIFIED Requirements

### Requirement: Matched callback exports are materialized into one flat collection
Resolving a callback-interface-declaring destination against the
flattened namespace SHALL produce exactly one collection, owned by the
destination component, whose members are the resolved matching exports
— addressable individually by member identity, not merged or flattened
into a single opaque value — and which feeds the destination import as
that import's sole producer. Each member's identity SHALL be the
matching export's own real `NodeId`, used directly; no proxy node SHALL
be created to stand in for a matched export that belongs to a different
component than the destination, per `graph/component-hierarchy`'s
"Cross-component dependency wiring uses real node identities directly"
requirement.

#### Scenario: The materialized collection contains exactly the resolved matches
- **WHEN** a callback-interface-declaring destination resolves to a set
  of matching exports
- **THEN** the destination's materialized collection has exactly one
  member per resolved match, each individually addressable, and no
  additional or missing members

#### Scenario: Re-resolving the same destination does not duplicate collection members
- **WHEN** a callback-interface-declaring destination is resolved more
  than once with no change to the underlying advertised exports
- **THEN** the materialized collection still has exactly one member per
  match, not two

#### Scenario: A matched export's collection member is its own real node identity, with no proxy
- **WHEN** a matched export belongs to a component other than the
  destination component whose collection is being materialized
- **THEN** the collection's member for that match is the matched
  export's own real `NodeId`, not the identity of any intermediate node
  created to stand in for it in the destination's own local storage
