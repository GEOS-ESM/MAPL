## MODIFIED Requirements

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
node. Only an export whose declared item class is `FIELD` SHALL be a
member: an export of any other class (for example `FIELDBUNDLE`, `STATE`,
`VECTOR`, `BRACKET`, `VECTORBRACKET`, `EXPRESSION` or `SERVICE`) whose
name matches the pattern SHALL be skipped, not bound, and the skip SHALL
NOT be reported as a connection conflict. Each skip SHALL be logged at
debug level; when every export matching the pattern was skipped, one
warning SHALL be logged saying why the import is unresolved. The
read-only check that decides whether a wildcard import already has a
match SHALL apply the same membership rule. This resolution SHALL run once
per matched wildcard import, independent of how many exports the pattern
matches.

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
  own qualified name, and no additional or missing members (every
  matching export is a `FIELD`; see the non-field scenarios below)

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

#### Scenario: A non-field export matching the pattern is not a member
- **WHEN** a wildcard-typed import's declared pattern matches a `FIELD`
  export and also matches an export whose declared item class is not
  `FIELD` (for example a `FIELDBUNDLE`)
- **THEN** the import's own composite collection has a member only for
  the `FIELD` export, the non-field export is not bound, and the skip is
  not reported as a connection conflict

#### Scenario: A pattern matching only non-field exports is unresolved
- **WHEN** every export a wildcard-typed import's declared pattern
  matches has a declared item class other than `FIELD`
- **THEN** the import is reported as unresolved, no composite collection
  is created, one warning is logged, and the read-only already-matched
  check reports the import as unmatched as well

### Requirement: Mismatch detection covers all established StateItemCharacteristic axes, not only units, type-kind, and vertical-coordinate
When determining whether a matched export/import pair's payloads
mismatch, the system SHALL compare every characteristic axis each item
has established at advertise time — not only physical units, type-kind,
and vertical-coordinate. This includes, at minimum, the attributes,
ungridded-dimensions, quantity-type, conservation, normalization, and
standard-name axes. A pair that matches on every axis SHALL be wired
directly, with no extension chain created, exactly as an exact-match
pair is wired today. A pair that mismatches on one or more of these axes
SHALL be routed through the extension-reuse capability's existing
chain-building mechanism the same way a units or type-kind mismatch is
routed today — no separate resolution path is introduced for these
axes.

#### Scenario: Matching declarations on a newly-covered axis are wired directly
- **WHEN** a destination component's import and a matching export agree
  on every one of the attributes, ungridded-dimensions, quantity-type,
  conservation, normalization, and standard-name axes
- **THEN** the pair is wired directly, with no extension chain created,
  the same as an exact-match pair that has no characteristics mismatch
  at all

#### Scenario: A mismatch on a newly-covered axis is detected
- **WHEN** a destination component's import and a matching export
  disagree on one of the attributes, ungridded-dimensions,
  quantity-type, conservation, normalization, or standard-name axes
- **THEN** the system detects the mismatch and delegates to the
  extension-reuse capability, the same as it already does for a units or
  type-kind mismatch

#### Scenario: A mismatch on an axis with no real adaptation fails explicitly
- **WHEN** resolution attempts to build an extension chain for a
  mismatch detected on one of the attributes, ungridded-dimensions,
  quantity-type, conservation, normalization, or standard-name axes
- **THEN** the attempt fails explicitly, reporting that axis as an
  unsupported characteristic or a connection conflict — the same outcome
  the system already produces for any other characteristic axis with no
  registered adaptation — and no dependency edge is silently created in
  place of the failed chain

#### Scenario: A tolerated standard-name mismatch is wired with a warning
- **WHEN** the validation mode is permissive and a matched pair's
  standard names differ
- **THEN** the standard-name axis is not a mismatch, the pair is wired
  directly, and the disagreement is reported in `connection_warnings`

#### Scenario: Unsupported pairs are recorded for a strict freeze check
- **WHEN** a pair is left unwired because a characteristic reported
  `BUILD_TRANSFORM_UNSUPPORTED`
- **THEN** the resolving component records it, and
  `check_unsupported_pairs` fails on it in strict validation mode and
  passes in permissive mode
