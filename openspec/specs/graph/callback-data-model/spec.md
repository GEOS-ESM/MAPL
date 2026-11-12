# Callback Data Model Specification

## Purpose

Provides the reusable callback-contract data model — a named argument
and per-method access-mode declaration set (`CallbackInterface`), a
shared cross-graph lookup registry for such contracts, and a validated
binding of one contract to a specific callback State's members and
method invocation adapters — so a callback interface can be declared
once and reused by every component that implements or invokes it.

## Requirements

### Requirement: A callback interface declares named arguments with an expected kind
A callback interface SHALL support declaring any number of named
arguments, each carrying an expected value kind (e.g. Field,
FieldBundle, State). A declared argument name SHALL be unique within one
callback interface.

#### Scenario: Argument declared with an expected kind is retrievable
- **WHEN** a callback interface declares an argument with a given
  expected kind
- **THEN** that argument's name and expected kind are retrievable
  afterward, matching what was declared

#### Scenario: Duplicate argument name is rejected
- **WHEN** a second argument is declared under a name already declared
  on the same callback interface
- **THEN** the declaration is rejected and the original declaration is
  unchanged

### Requirement: A callback interface declares methods whose argument access is independent per method
A callback interface SHALL support declaring any number of named
methods. Each method SHALL independently declare, for any subset of the
interface's own declared arguments, an access mode (input-only,
output-only, input-and-output, or unspecified) describing how that
method uses the argument. The same argument name SHALL be usable with a
different access mode in a different method of the same interface. A
declared method name SHALL be unique within one callback interface.

#### Scenario: Same argument has different access modes in different methods
- **WHEN** a callback interface declares one argument, then declares two
  methods each assigning that argument a different access mode
- **THEN** each method reports the access mode declared for it,
  independent of the other method's declared access mode for the same
  argument

#### Scenario: Duplicate method name is rejected
- **WHEN** a second method is declared under a name already declared on
  the same callback interface
- **THEN** the declaration is rejected and the original declaration is
  unchanged

#### Scenario: Method argument access for an undeclared argument is rejected
- **WHEN** a method declares access for an argument name that was never
  declared on the owning callback interface
- **THEN** the declaration is rejected

#### Scenario: Method argument access for an undeclared method is rejected
- **WHEN** access is declared for a method name that was never declared
  on the callback interface
- **THEN** the declaration is rejected

### Requirement: A callback interface's own identity is carried externally, not duplicated on the interface
A callback interface value SHALL carry no service-name field of its own;
the name under which it is looked up SHALL be owned exclusively by
whatever registers it.

#### Scenario: Interface value carries no independent service name
- **WHEN** a callback interface is constructed and later registered
  under a service name
- **THEN** querying the interface value itself for a service name is not
  a supported operation — the name is retrievable only through the
  registration mechanism that assigned it

### Requirement: Callback interfaces are shared across every component graph through one registry
Registered callback interfaces SHALL be visible to lookups performed
from any part of a MAPL application's component hierarchy, not scoped
to any single component's own local graph.

#### Scenario: Interface registered from one context is visible from another
- **WHEN** a callback interface is registered under a service name
- **THEN** a lookup for that service name succeeds regardless of which
  part of the application performs the lookup

### Requirement: Registering a callback interface assigns a unique identity and rejects a duplicate service name
Registering a callback interface under a service name SHALL assign that
interface a unique identity, retrievable afterward both by identity and
by service name. Registering a second interface under a service name
already in use SHALL be rejected, leaving the original registration
unchanged.

#### Scenario: Registered interface is retrievable by identity and by service name
- **WHEN** a callback interface is registered under a service name
- **THEN** the same interface is retrievable both by the identity
  assigned at registration and by the service name it was registered
  under

#### Scenario: Duplicate service name is rejected
- **WHEN** a second callback interface is registered under a service
  name already registered
- **THEN** the registration is rejected and the original registration's
  interface and identity are unchanged

#### Scenario: Lookup of an unregistered service name fails explicitly
- **WHEN** a lookup is performed for a service name that has never been
  registered
- **THEN** the lookup fails explicitly rather than returning an
  arbitrary or default interface

### Requirement: A callback state binding associates one callback state with a specific interface, its argument members, and its method attachments
A callback state binding SHALL explicitly store: the identity of the
callback interface it implements; the identity of the callback state it
binds to; a mapping from each bound argument name to the identity of
that argument's member within the callback state; and a mapping from
each bound method name to that method's invocation attachment.

#### Scenario: Binding stores interface identity, state identity, and both maps
- **WHEN** a callback state binding is constructed for a registered
  callback interface and a callback state identity
- **THEN** the interface identity and callback state identity are
  retrievable from the binding afterward

#### Scenario: Bound argument member is retrievable by argument name
- **WHEN** an argument declared on the bound interface is bound to a
  member identity
- **THEN** that member identity is retrievable afterward by the
  argument's name

#### Scenario: Bound method attachment is retrievable by method name
- **WHEN** a method declared on the bound interface is bound to a method
  invocation attachment
- **THEN** that attachment is retrievable afterward by the method's name

### Requirement: A callback state binding only accepts argument and method names declared on its interface
Binding an argument or a method on a callback state binding SHALL
require that name to already be declared on the interface identity the
binding was constructed with. A name outside the interface's own
declarations SHALL be rejected.

#### Scenario: Binding an argument name outside the interface is rejected
- **WHEN** a callback state binding attempts to bind an argument name
  never declared on its interface
- **THEN** the binding is rejected and no member identity is recorded
  for that name

#### Scenario: Binding a method name outside the interface is rejected
- **WHEN** a callback state binding attempts to bind a method name never
  declared on its interface
- **THEN** the binding is rejected and no method attachment is recorded
  for that name

### Requirement: A method attachment is invocable and reports a missing invocation implementation explicitly
A callback state binding's method attachment SHALL support being
invoked with the binding's current argument member identities.
Attempting to invoke a method attachment that has no underlying
invocation implementation configured SHALL fail explicitly rather than
silently doing nothing.

#### Scenario: Method attachment with a configured implementation invokes it
- **WHEN** a method attachment is bound with an underlying invocation
  implementation configured, and is then invoked
- **THEN** that underlying implementation is invoked exactly once with
  the current argument member identities

#### Scenario: Method attachment with no configured implementation fails loudly on invocation
- **WHEN** a method attachment is bound with no underlying invocation
  implementation configured, and an invocation is attempted
- **THEN** the invocation fails explicitly rather than doing nothing
  observable
