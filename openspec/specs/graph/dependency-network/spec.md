# Dependency Network Specification

## Purpose

Provides graph-neutral dependency wiring with explicit adjacency, acyclic
validation, producer checks, and a lifecycle boundary before execution begins.

## Requirements

### Requirement: Dependency uses direct adjacency, not edge objects
`DependencyNetwork` MUST represent each dependency as a source and target
`NodeId` pair and MUST NOT expose persistent addressable edge objects. It MUST
maintain predecessor and successor adjacency for every referenced node.

#### Scenario: Dependency is queryable in both directions
- **WHEN** dependency `A -> B` is added
- **THEN** `B` lists `A` as predecessor and `A` lists `B` as successor

#### Scenario: No first-class edge identity exists
- **WHEN** clients inspect or query network dependencies
- **THEN** dependencies are addressed only by source and target node identities

### Requirement: Network rejects invalid or cyclic dependencies
Adding a dependency MUST reject self-dependencies, invalid node identities,
and any edge that would make its individual network cyclic. Rejection MUST
leave existing adjacency unchanged.

#### Scenario: Cycle is rejected
- **WHEN** `A -> B` and `B -> C` exist and client adds `C -> A`
- **THEN** operation fails and all existing adjacency remains unchanged

#### Scenario: Self-dependency is rejected
- **WHEN** client adds `A -> A`
- **THEN** operation fails without mutation

### Requirement: Network exposes structural queries and mutation
The network MUST support adding and removing dependencies, checking
membership, retrieving predecessor and successor sets, checking whether a
node has either relation, testing whether an edge would create a cycle, and
clearing all dependencies while mutable.

#### Scenario: Removing existing dependency
- **WHEN** client removes existing `A -> B` from a mutable network
- **THEN** pair disappears from both adjacency directions

#### Scenario: Cycle query has no side effects
- **WHEN** client calls `would_create_cycle(A, B)`
- **THEN** result reflects current reachability and adjacency is unchanged

### Requirement: Network validation detects structural defects
`validate()` MUST detect predecessor/successor asymmetry, invalid referenced
identities, self-dependencies, cycles, and violations of the one-producer
constraint for a `StateItemNode` within one network.

#### Scenario: Valid network passes validation
- **WHEN** mutable network contains valid acyclic adjacency with at most one
  producer per state item
- **THEN** validation succeeds

#### Scenario: Multiple producers fail validation
- **WHEN** one state item has two direct producer transform nodes in one
  network
- **THEN** validation fails and requires an explicit merge or accumulation
  transform instead

### Requirement: Frozen network rejects structural mutation
`freeze()` MUST irreversibly mark the network frozen. After freezing, add,
remove, and clear operations MUST fail, while read-only queries and validation
remain available.

#### Scenario: Frozen mutation is rejected
- **WHEN** client attempts to add, remove, or clear a frozen network
- **THEN** operation fails and network structure remains unchanged

#### Scenario: Freeze is irreversible
- **WHEN** client calls `freeze()` and later attempts to resume mutation
- **THEN** no operation can make network mutable again
