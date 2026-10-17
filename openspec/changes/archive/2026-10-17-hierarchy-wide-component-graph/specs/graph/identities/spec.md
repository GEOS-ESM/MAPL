## MODIFIED Requirements

### Requirement: NodeId generation is fresh and non-repeating
There SHALL be exactly one `NodeIdGenerator` instance for the entire
component hierarchy under a given root, owned by the hierarchy-wide
shared storage (see `graph/component-hierarchy`'s "One shared
hierarchy-wide graph store" requirement) — not one per `ComponentGraph`
instance. Every `ComponentGraph`'s `next_node_id()` SHALL delegate to
that one shared generator. `next()` SHALL return a `NodeId` distinct
from every `NodeId` previously returned, for the lifetime of that
hierarchy, so that no two components can ever be issued the same
`NodeId` — not by checking for collisions after the fact, but because
only one counter exists to draw from.

#### Scenario: Sequential distinct ids
- **WHEN** `next()` is called N times on one generator
- **THEN** all N returned `NodeId` values are pairwise distinct (`/=`)

#### Scenario: Distinct ids across different components in the same hierarchy
- **WHEN** two different components, each advertising items into the
  same hierarchy, each obtain fresh `NodeId`s for their own items
  (each through their own `ComponentGraph`'s `next_node_id()`)
- **THEN** no `NodeId` issued to one component's item is ever equal to a
  `NodeId` issued to the other component's item, because both calls
  drew from the same single shared generator

### Requirement: NodeId exhaustion is detected, not silently wrapped
When fresh `NodeId` generation cannot produce a new id because the
hierarchy-wide counter would overflow, it SHALL report exhaustion via an
optional `status` argument rather than wrapping around and silently
reusing or duplicating a prior value, regardless of which component in
the hierarchy requested the fresh id.

#### Scenario: Exhaustion reported
- **WHEN** `next()` is called while the hierarchy-wide counter is at its
  maximum representable value
- **THEN** `next()` returns `INVALID_NodeId` and, if `status` is
  present, sets it to a nonzero value

#### Scenario: No silent wraparound
- **WHEN** generation is at exhaustion
- **THEN** `next()` does not return a `NodeId` equal to any previously
  issued value anywhere in the hierarchy
