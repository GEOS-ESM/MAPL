# Callback Wiring Specification

## Purpose

Wires the callback data model (`CallbackInterface`/`CallbackStateBinding`)
into a real component hierarchy: resolves an ordinary connection whose
destination item declares an expected `CallbackInterface` against a
flattened, hierarchy-wide qualified-export namespace, materializes
matched callback states into one collection for the receiving component,
and drives per-method invocation so a bound callback method runs exactly
once per logical call, only after every argument it needs is ready.

## Requirements

### Requirement: Descendant exports are visible under a hierarchically-qualified name
For a component's own local view of available exports, every export
advertised by that component itself SHALL be visible under its own
short name, and every export advertised by any descendant component
SHALL additionally be visible under a name qualified by that
descendant's position in the hierarchy, distinguishable from the same
short name exported by a different descendant or by the component
itself.

#### Scenario: A component's own export is visible under its own name
- **WHEN** a component advertises an export
- **THEN** that export is visible in the component's own flattened
  export namespace under its own short name

#### Scenario: A descendant's export is visible under a qualified name
- **WHEN** a descendant component advertises an export
- **THEN** that export is visible in the ancestor's flattened export
  namespace under a name that distinguishes it from an export of the
  same short name advertised elsewhere in the hierarchy

#### Scenario: Two descendants exporting the same short name remain distinguishable
- **WHEN** two different descendant components each advertise an export
  under the same short name
- **THEN** both exports are visible in the ancestor's flattened
  namespace under two distinct qualified names, neither one shadowing
  the other

#### Scenario: Re-querying the namespace does not duplicate an entry
- **WHEN** the flattened export namespace is queried more than once with
  no new export advertised in between
- **THEN** each previously-visible export still appears exactly once

### Requirement: An import declaring an expected callback interface is resolved against the flattened namespace, not by exact name
An ordinarily-declared connection whose destination import declares that
it expects to satisfy a callback interface SHALL be resolved by matching
its declared source pattern against the flattened export namespace
described above, selecting every entry whose qualified name matches and
that implements the expected interface, rather than requiring an
exact-name match against a single component's own directly-declared
exports. A destination import that does not declare an expected
callback interface SHALL continue to be resolved by exact-name matching,
unaffected by this requirement.

#### Scenario: A pattern matching multiple qualified exports selects all of them
- **WHEN** a connection's destination declares an expected callback
  interface and its source pattern matches more than one entry in the
  flattened export namespace, and every match implements the expected
  interface
- **THEN** every matching export is selected, none omitted

#### Scenario: A matching export that does not implement the expected interface is rejected
- **WHEN** a source pattern matches an export that does not implement
  the destination's expected callback interface
- **THEN** that export is not selected, and the rejection is reported
  rather than silently accepted or silently dropped with no trace

#### Scenario: A pattern matching no export resolves to an empty, valid selection
- **WHEN** a destination declaring an expected callback interface has a
  source pattern that matches no entry in the flattened export namespace
- **THEN** resolution completes with an empty selection rather than
  failing, since the qualified-export namespace may still gain matching
  entries later in the advertise sequence

#### Scenario: A destination with no declared callback interface is unaffected
- **WHEN** a connection's destination import does not declare an
  expected callback interface
- **THEN** that import continues to be resolved by ordinary exact-name
  matching, with no flattened-namespace lookup performed for it

### Requirement: Matched callback exports are materialized into one flat collection
Resolving a callback-interface-declaring destination against the
flattened namespace SHALL produce exactly one collection, owned by the
destination component, whose members are the resolved matching exports -
addressable individually by member identity, not merged or flattened
into a single opaque value - and which feeds the destination import as
that import's sole producer.

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

### Requirement: A method-level binding identifies its network, its invoked method, and its argument endpoints
A callback method-level binding SHALL explicitly store: the identity of
the method node it invokes, and, for each of that method's declared
arguments, the source and target identities of that argument's data
flow, organized by the dependency network each direction operates on.

#### Scenario: Method-level binding exposes network, method node, and argument endpoints
- **WHEN** a method-level binding is constructed for a bound callback
  method
- **THEN** its invoked method node identity and each declared argument's
  source/target identities are retrievable afterward, along with the
  identity of the network each direction's endpoints belong to

### Requirement: Each callback method may have its own get and put dependency networks
For an input-and-output callback argument, the system SHALL support
constructing two independent dependency networks for one callback
method binding: a "get" network carrying data from the provider's
representation, through any needed transform, to the callback's
representation; and a "put" network carrying data the reverse direction.
The same underlying data items MAY participate in both networks, but
each network SHALL remain acyclic considered on its own, even when the
union of both networks would not be.

#### Scenario: Get and put networks are independently constructed
- **WHEN** a callback method binding declares an input-and-output
  argument
- **THEN** a get network and a put network are each constructed for
  that argument, independently queryable

#### Scenario: Each network is acyclic on its own even if their union is not
- **WHEN** a get network and a put network for the same argument share
  data items such that their combined edge set would contain a cycle
- **THEN** each network individually still passes acyclicity validation,
  considered apart from the other

### Requirement: A bound callback method is invoked exactly once, only after every required argument path is ready
Invoking a callback method through its method-level binding SHALL first
ensure every argument path across that binding's relevant get/put
network(s) has been prepared, and SHALL then invoke the bound method
node exactly once. The method SHALL NOT be invoked before every required
argument path is ready, and SHALL NOT be invoked more than once as a
side effect of preparing multiple argument paths.

#### Scenario: Invocation waits until every required argument path is ready
- **WHEN** a callback method's invocation is requested while at least
  one of its required argument paths is not yet ready
- **THEN** the underlying method node is not invoked until every
  required argument path has been prepared

#### Scenario: A ready method is invoked exactly once per invocation request
- **WHEN** a callback method's invocation is requested and every
  required argument path is already ready
- **THEN** the underlying method node is invoked exactly once, not once
  per argument path

#### Scenario: Preparing multiple argument paths does not trigger multiple invocations
- **WHEN** a callback method binding's several argument paths become
  ready as part of resolving one invocation request
- **THEN** the underlying method node is invoked exactly once for that
  request, regardless of how many argument paths needed preparation
