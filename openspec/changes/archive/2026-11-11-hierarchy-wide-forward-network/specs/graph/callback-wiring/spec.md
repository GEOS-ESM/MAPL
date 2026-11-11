## MODIFIED Requirements

### Requirement: A method-level binding identifies its network, its invoked method, and its argument endpoints
A callback method-level binding SHALL explicitly store: the identity of
the method node it invokes; for each of that method's declared
arguments, the source and target identities of that argument's data
flow in each direction; and the identity of the binding's put
(write-back) network. The get direction's edges are in the hierarchy's
forward network.

#### Scenario: Method-level binding exposes network, method node, and argument endpoints
- **WHEN** a method-level binding is constructed for a bound callback
  method
- **THEN** its invoked method node identity, each declared argument's
  source/target identities, and its put network's identity are
  retrievable afterward

### Requirement: Each callback method may have its own get and put dependency networks
For an input-and-output callback argument, the system SHALL wire the
"get" direction (from the provider's representation, through any needed
transform, to the callback's representation) into the hierarchy's
forward network, and SHALL wire the "put" direction (the reverse) into
a write-back network of the callback method's own. The same underlying
data items MAY participate in both, and each network SHALL remain
acyclic on its own even when their union would not be.

#### Scenario: Get and put networks are independently constructed
- **WHEN** a callback method binding declares an input-and-output
  argument
- **THEN** the provider-to-member edge is in the forward network, the
  member-to-provider edge is in the binding's put network, and the
  forward network has no member-to-provider edge

#### Scenario: Each network is acyclic on its own even if their union is not
- **WHEN** the get and put edges for the same argument share data items
  such that their combined edge set would contain a cycle
- **THEN** the forward network and the put network each still pass
  acyclicity validation, considered apart from the other

### Requirement: A bound callback method is invoked exactly once, only after every required argument path is ready
Invoking a callback method through its method-level binding SHALL first
bring every input and input-output argument's callback-side item up to
date through the forward network, following each provider back to its
own producer wherever that edge was declared, and SHALL then invoke the
bound method node exactly once. The method SHALL NOT be invoked before
every required argument path is ready, and SHALL NOT be invoked more
than once as a side effect of preparing multiple argument paths.

#### Scenario: Invocation waits until every required argument path is ready
- **WHEN** a callback method's invocation is requested while at least
  one of its required argument paths is not yet ready
- **THEN** the underlying method node is not invoked until every
  required argument path has been prepared

#### Scenario: A provider's upstream producer is brought up to date
- **WHEN** a callback argument's provider is itself fed by a transform
  in the forward network, declared by a component other than the one
  that wired the callback, and that transform's input has changed
- **THEN** the transform executes before the callback method is invoked,
  and the method sees the converted current value

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
