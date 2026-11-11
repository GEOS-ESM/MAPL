# state-item Specification

## Purpose

Defines `GraphStateItem`, the concrete, non-polymorphic node payload type that
replaces per-value-kind subclassing for the ESMF handle kinds a
`StateItemNode` can hold, enabling generic operations without `select
type` dispatch, and its two-tier native/variant classification.

## Requirements

### Requirement: GraphStateItem is concrete with three allocatable components
`GraphStateItem` SHALL be a concrete (non-polymorphic, non-extensible) type
containing exactly three allocatable components: a field, a field
bundle, and a state. Of these three, at most one SHALL be allocated at
any time. There SHALL be no separate route-handle component.

#### Scenario: At most one component allocated
- **WHEN** a `GraphStateItem` instance has one of its three handle components
  allocated
- **THEN** the other two remain unallocated

#### Scenario: Newly created GraphStateItem has no component allocated
- **WHEN** a `GraphStateItem` is created and no handle has been assigned to it
  yet
- **THEN** all three handle components are unallocated and its
  native-classification query reports the distinguished "not found"
  value

### Requirement: Route handle is represented as a state in a wrapper role
A route handle SHALL be represented by allocating the state component in
a persistent wrapper role — a state containing exactly one route-handle
member — not by a separate, dedicated route-handle component.

#### Scenario: Route handle accessed via wrapper
- **WHEN** a `GraphStateItem` is playing the route-handle-wrapper role
- **THEN** the route handle is obtained by resolving it out of the
  state's one member, not by exposing a bare stored route-handle field,
  and the state component (not a distinct component) is what is
  allocated

#### Scenario: Route handle renewal replaces the member, not the wrapper
- **WHEN** the underlying route handle for a route-handle-wrapper
  `GraphStateItem` is renewed (destroyed and recreated)
- **THEN** the wrapper state object itself is not destroyed or
  recreated; only its route-handle member is replaced

### Requirement: Two-tier classification — native kind and variant
`GraphStateItem` SHALL expose two classification queries rather than one
combined query, since "which native representation is allocated" and
"which role/use-case this instance plays" are independent questions:

- A native-kind query, using the same classification vocabulary ESMF
  itself uses for state members (field / field-bundle / state /
  not-found), reporting purely which of the three components is
  allocated. This query SHALL NOT distinguish the route-handle-wrapper
  role from an ordinary state — both report the plain-state value,
  since "which ESMF type is allocated" and "which role this state
  plays" are the independent questions this two-tier split exists to
  separate; the route-handle-wrapper role is a variant-tier question
  (below).
- A variant query, using a new, open-ended classification vocabulary
  that mirrors the native-kind vocabulary as its vanilla default for
  each native kind (a field's default variant is the vanilla "field"
  value, a field bundle's is the vanilla "field-bundle" value, and a
  state's is the vanilla "state" value), reporting a finer-grained role
  for whichever component is allocated, for cases the native
  classification has no vocabulary for or does not itself distinguish
  (e.g., an ordinary field bundle vs. a time-interpolation bracket vs.
  a vector quantity; an ordinary field vs. a geometry proxy; an
  ordinary nested state vs. a vertical grid vs. a route-handle
  wrapper). The route-handle-wrapper role is reported at this tier —
  determined by inspecting the wrapper's own sole member's native
  classification (which is itself already the route-handle value for a
  route-handle member) — as a refinement of the plain-state value,
  exactly like the vertical-grid role; when the wrapper role applies,
  this value SHALL be reported directly, without consulting any
  attached metadata for that instance.

Client code SHALL use these queries rather than probing allocation
status of individual components directly.

#### Scenario: Native kind matches the allocated component
- **WHEN** exactly one handle component of a `GraphStateItem` is allocated
- **THEN** the native-kind query reports the value corresponding to
  that component, whether or not it is playing the route-handle-wrapper
  role

#### Scenario: Native kind reports not-found when none allocated
- **WHEN** no handle component of a `GraphStateItem` is allocated
- **THEN** the native-kind query reports the distinguished "not found"
  value

#### Scenario: Variant defaults to the vanilla value for the native kind
- **WHEN** an allocated component has no more specific role tagged on it
  and is not playing the route-handle-wrapper role
- **THEN** the variant query reports the vanilla variant value matching
  that component's native kind (e.g. the vanilla "field" value for an
  untagged field, the vanilla "field-bundle" value for an untagged
  field bundle)

#### Scenario: Variant reports route-handle for the wrapper role
- **WHEN** a `GraphStateItem`'s state component is allocated and playing the
  route-handle-wrapper role
- **THEN** the variant query reports the route-handle value, without
  consulting any attached metadata

#### Scenario: Variant is extensible without changing GraphStateItem's structure
- **WHEN** a new variant value is introduced for some future role
- **THEN** no change to `GraphStateItem`'s allocatable components or their
  count is required to support it

### Requirement: Variant is backed by ESMF metadata on the component itself
The variant reported for an allocated component SHALL be backed by
metadata attached to that component's own underlying ESMF object (the
field's, field bundle's, or state's own metadata), under a reserved,
framework-owned key — not by a field stored on `GraphStateItem` itself.

#### Scenario: Variant travels with the underlying ESMF object
- **WHEN** a `GraphStateItem`'s allocated component's underlying ESMF object
  is inspected independently of the `GraphStateItem` wrapper (e.g., copied or
  passed elsewhere at the ESMF level)
- **THEN** its variant tag remains attached and readable, because it
  lives on the ESMF object's own metadata, not on the `GraphStateItem`
  wrapper

### Requirement: Multiple allocated components is a checked defect
It SHALL be a checked, detectable defect for more than one of the three
handle components of a `GraphStateItem` to be allocated simultaneously; this
condition SHALL NOT be silently tolerated.

#### Scenario: Defect detected on invariant violation
- **WHEN** a `GraphStateItem` instance is inspected (e.g., via its native-kind
  query or an explicit validity check) while more than one of its three
  handle components is allocated
- **THEN** the violation is reported as an error/defect rather than
  silently returning an arbitrary or first-match kind

### Requirement: Membership maps are gated by native kind and role
`GraphStateItem` SHALL carry two additional map-valued components — a
field-bundle member map and a state member map — each mapping a member
name to a node identity. The field-bundle member map SHALL be populated
only when the native-kind query reports the field-bundle value; the
state member map SHALL be populated only when the native-kind query
reports the state value *and* the variant query does not report the
route-handle value. Both SHALL be empty otherwise (in particular, for
the route-handle-wrapper role — whose one member is not a graph-visible,
node-identity-addressable item — even though its native kind is the
same plain-state value as any other state).

#### Scenario: Field-bundle member map populated only for field-bundle kind
- **WHEN** a `GraphStateItem`'s native kind is field-bundle
- **THEN** its field-bundle member map MAY contain entries, and its
  state member map is empty

#### Scenario: State member map populated only for plain-state kind, excluding the wrapper role
- **WHEN** a `GraphStateItem`'s native kind is state (including a
  vertical-grid-variant state) and it is not playing the
  route-handle-wrapper role
- **THEN** its state member map MAY contain entries, and its
  field-bundle member map is empty

#### Scenario: Both maps empty for field kind
- **WHEN** a `GraphStateItem`'s native kind is field
- **THEN** both the field-bundle member map and the state member map are
  empty

#### Scenario: Both maps empty for the route-handle-wrapper role
- **WHEN** a `GraphStateItem`'s native kind is state and it is playing the
  route-handle-wrapper role
- **THEN** both the field-bundle member map and the state member map are
  empty, even though the native kind alone is indistinguishable from an
  ordinary state

### Requirement: GraphStateItem is the sole StateItemNode payload
A `StateItemNode`'s payload SHALL be exactly one `GraphStateItem` value —
concrete, not polymorphic. No separate node kind SHALL exist for
geometry values; geometry is represented via the field component of an
ordinary `GraphStateItem`, tagged with the geometry variant.

#### Scenario: StateItemNode payload is a GraphStateItem
- **WHEN** a `StateItemNode`'s payload is queried
- **THEN** the returned value is a `GraphStateItem`, and dispatch on it never
  requires `select type`

#### Scenario: No dedicated geometry node kind
- **WHEN** a geometry value needs to be represented as a graph-visible
  node
- **THEN** it is represented as an ordinary `StateItemNode` whose
  `GraphStateItem` payload has its field component allocated (holding an
  incomplete/geometry-only field) and tagged with the geometry variant,
  not as a distinct node or payload kind
