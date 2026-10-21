## ADDED Requirements

### Requirement: A wildcard-typed import's declared pattern is matched against the source export namespace and fanned out into a composite collection
For every advertised import whose declared item class is `WILDCARD` (an
import declared with a pattern as its own short name, intended to match
more than one real export rather than name exactly one), the system
SHALL, once a connection has matched that import as a destination: take
the same pattern already used to find this import as a destination
(the pattern every ordinary connection resolution already matches
destination import names against), and match it again against the
connection's own source component's export namespace — using the same
pattern-matching mechanism, applied to the source side instead of the
destination side. For every export name that matches, the system SHALL
bind that export's own already-established node as a member of one
composite collection, keyed by that export's own qualified name, owned
by the wildcard import's own already-advertised node — reusing that
node's own existing identity rather than creating a separate collection
node. This resolution SHALL run once per matched wildcard import,
independent of how many exports the pattern matches.

A wildcard-typed export's own producers reachable through a descendant
level SHALL be matched using the same qualified export namespace
ordinary connection resolution and callback-wiring resolution already
rely on for cross-level visibility — no separate cross-level discovery
mechanism is introduced for wildcard resolution specifically.

#### Scenario: A pattern matching multiple exports produces one member per match
- **WHEN** a wildcard-typed import's declared pattern matches more than
  one export in the connection's own source export namespace
- **THEN** the import's own composite collection has exactly one member
  per matching export, each individually addressable by that export's
  own qualified name, and no additional or missing members

#### Scenario: A pattern matching exactly one export is resolved the same way as a multi-match
- **WHEN** a wildcard-typed import's declared pattern matches exactly
  one export in the source export namespace
- **THEN** the import's own composite collection has exactly one
  member, resolved through the same matching mechanism a multi-match
  case uses — no separate single-match code path exists

#### Scenario: Each composite member is the matched export's own real node identity, with no proxy
- **WHEN** a matched export belongs to a component other than the one
  whose wildcard import is being resolved
- **THEN** the collection's member for that match is the matched
  export's own real `NodeId`, not the identity of any intermediate node
  created to stand in for it

#### Scenario: Re-resolving the same wildcard import does not duplicate collection members
- **WHEN** wildcard resolution runs more than once for the same
  already-resolved wildcard import, with no change to the underlying
  advertised exports
- **THEN** the composite collection still has exactly one member per
  match, not two

#### Scenario: A descendant's export reachable only through a qualified name is still matched
- **WHEN** a wildcard-typed import's declared pattern matches an
  export that is visible on the connection's own source component only
  through a descendant's own qualified export name, not a direct
  export of that component itself
- **THEN** that export is still matched and included as a composite
  member, using the same qualified export namespace already available
  for this purpose

### Requirement: A wildcard-typed import with zero matching exports is reported as a resolution failure, not silently resolved
When a wildcard-typed import's declared pattern matches no export at
all in the connection's own source export namespace, the system SHALL
report that failure explicitly, rather than creating an empty composite
collection and treating the import as resolved.

#### Scenario: Zero matches is reported, not silently accepted
- **WHEN** a wildcard-typed import's declared pattern matches no export
  in the connection's own source export namespace
- **THEN** the system reports this as a resolution failure for that
  import, and does not create a composite collection with zero members
  as though resolution had succeeded

## MODIFIED Requirements

### Requirement: An expression-typed import or export is excluded from ordinary resolution, but is resolvable through expression resolution
The existing "an import with no graph-native resolution path at all is
excluded from unresolved-import propagation" behavior SHALL continue to
exclude `SERVICE`-typed imports (those remain out of scope for
graph-native resolution), but SHALL NOT exclude `EXPRESSION`-typed or
`WILDCARD`-typed items: once this capability's own expression resolution
step or wildcard resolution step (each described above or elsewhere in
this capability) has run for a given component, an `EXPRESSION`-typed or
`WILDCARD`-typed item advertised on that component has a real
graph-native resolution path and is no longer treated as unconditionally
unresolvable for propagation purposes.

#### Scenario: A wildcard-typed import remains excluded from ordinary propagation
- **WHEN** a component advertises an import whose declared item class is
  `WILDCARD`, and this capability's own wildcard resolution step has run
  for that component
- **THEN** that import is no longer treated as unconditionally excluded
  from graph-native resolution — it is resolved through wildcard
  resolution (above), not through ordinary exact-name connection
  resolution. (This scenario's name is preserved verbatim from the
  requirement's prior version per `openspec validate`'s own
  archive-safety check, which requires every scenario name a MODIFIED
  block replaces to still appear somewhere in the replacement, even
  when — as here — the outcome it describes is the exact opposite of
  what the name now suggests. Read this scenario's own THEN clause for
  this requirement's actual current behavior, matching the identical
  convention this same file's "Resolved graph structure is validated
  and frozen" and "Ordinary resolution matches existing coupler
  behavior" requirements already use elsewhere.)

#### Scenario: A service-typed import remains excluded from ordinary propagation
- **WHEN** a component advertises an import whose declared item class is
  `SERVICE`
- **THEN** that import continues to be excluded from unresolved-import
  propagation, unaffected by this capability's own expression or
  wildcard resolution steps

#### Scenario: An expression-typed export is no longer treated as having no resolution path
- **WHEN** a component advertises an export whose declared item class is
  `EXPRESSION`, and this capability's own expression resolution step has
  run for that component
- **THEN** that export is no longer treated as unconditionally excluded
  from graph-native resolution — it is resolved through expression
  resolution, not through ordinary exact-name connection resolution

#### Scenario: A wildcard-typed import is no longer treated as having no resolution path
- **WHEN** a component advertises an import whose declared item class is
  `WILDCARD`, and this capability's own wildcard resolution step has run
  for that component
- **THEN** that import is no longer treated as unconditionally excluded
  from graph-native resolution — it is resolved through wildcard
  resolution (above), not through ordinary exact-name connection
  resolution
