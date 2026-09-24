## Purpose

Provides a graph node type representing an explicitly invoked method —
either a GridComp initialize/run phase or an attached ESMF State
callback method — through one unified shape, so the two ESMF call
signatures are hidden behind an interchangeable invocation adapter
rather than requiring two different node kinds.

## ADDED Requirements

### Requirement: One node type covers both GridComp phases and State callback methods
A method invocation SHALL be represented by one node type regardless of
whether the underlying call is a GridComp initialize/run phase
`(importState, exportState, clock, context)` or an attached ESMF State
callback method `(callback State)`. The node's own shape (its argument
declarations and bindings) SHALL be identical in both cases; only the
attached invocation adapter differs.

#### Scenario: Node constructed for a GridComp-phase call shape
- **WHEN** a method node is constructed with a GridComp-phase invocation
  adapter attached
- **THEN** the node exposes the same argument-declaration and
  argument-binding query methods as any other method node

#### Scenario: Node constructed for a State-callback call shape
- **WHEN** a method node is constructed with a State-callback invocation
  adapter attached
- **THEN** the node exposes the same argument-declaration and
  argument-binding query methods as any other method node, with no
  observable difference in shape from the GridComp-phase case

#### Scenario: No adapter attached is not invocable
- **WHEN** a method node is invoked before any invocation adapter has
  been attached to it
- **THEN** the invocation fails loudly rather than silently doing
  nothing

### Requirement: Named argument declarations carry an access mode
A method node SHALL support declaring any number of named arguments,
each with an associated access mode describing how the method uses that
argument (input-only, output-only, input-and-output, or unspecified). An
argument declaration MAY optionally constrain its expected value kind. A
declared argument name SHALL be unique within one method node.

#### Scenario: Arguments declared with each access mode
- **WHEN** a method node declares one argument for each access mode
  (input-only, output-only, input-and-output, unspecified)
- **THEN** each declared argument's access mode is retrievable
  afterward, matching what was declared

#### Scenario: Duplicate argument name is rejected
- **WHEN** a second argument is declared under a name already declared
  on the same method node
- **THEN** the declaration is rejected and the original declaration is
  unchanged

#### Scenario: Kind-constrained argument accepts a matching binding
- **WHEN** an argument is declared with an expected value kind, and is
  later bound to a value of that same kind
- **THEN** the binding succeeds

#### Scenario: Kind-constrained argument rejects a mismatched binding
- **WHEN** an argument is declared with an expected value kind, and is
  later bound to a value of a different kind
- **THEN** the binding is rejected and the argument remains unbound

### Requirement: Argument bindings are queryable independent of adapter kind
A method node SHALL let a declared argument be bound to a concrete
value identity, and SHALL let previously-declared bindings be queried
by argument name, uniformly regardless of which invocation adapter kind
is attached. Binding an argument SHALL require that argument to have
already been declared.

#### Scenario: Binding an undeclared argument is rejected
- **WHEN** a binding is attempted for an argument name that was never
  declared on that method node
- **THEN** the binding is rejected

#### Scenario: Bound argument is retrievable by name
- **WHEN** a declared argument is bound to a value identity
- **THEN** that same value identity is retrievable afterward by the
  argument's name

### Requirement: Import and export are one conceptual argument set
A method node's argument declarations and bindings SHALL be a single,
flat set with no substructure distinguishing which physical state
(import-side or export-side) a bound argument's value originated from.
Declaring or binding an argument SHALL NOT require creating any
additional composite state object beyond what already holds the bound
value.

#### Scenario: Arguments from both sides coexist without distinction
- **WHEN** a method node declares and binds one argument whose value
  would conventionally be read from an import-side state and one whose
  value would conventionally be written to an export-side state
- **THEN** both arguments are retrievable through the same argument
  query methods, with no additional query needed to determine which
  side either one came from

### Requirement: Invocation delegates to one adapter entry point without reimplementing call semantics
Invoking a method node SHALL do nothing beyond gathering the node's
current argument declarations and bindings and passing them to its
attached invocation adapter's own single entry point. A method node
SHALL NOT itself branch on, or otherwise encode knowledge of, which
underlying call convention (GridComp phase vs. attached State method) a
given invocation adapter implements.

#### Scenario: Invocation calls exactly the adapter's entry point
- **WHEN** a method node with an attached invocation adapter is invoked
- **THEN** the adapter's single invocation entry point is called exactly
  once, with the node's current argument declarations and bindings, and
  nothing else observable happens

#### Scenario: Invocation succeeds identically regardless of adapter kind
- **WHEN** two method nodes, one with each invocation adapter kind
  attached, are each invoked with equivalent argument declarations and
  bindings
- **THEN** both invocations succeed through the same node-level
  `invoke` behavior, with any difference in outcome attributable only to
  the two adapters' own distinct implementations

### Requirement: Clock is invocation context, not a graph dependency
A method node's invocation MAY accept a clock as a plain, optional
invocation-time argument, passed through to the invocation adapter.
Clock SHALL NOT be represented as a declared argument, a value identity,
or any other graph-visible dependency.

#### Scenario: Invoking with a clock does not create a graph dependency
- **WHEN** a method node is invoked with a clock supplied
- **THEN** the clock is passed to the invocation adapter, and no
  argument declaration, binding, or other graph-visible entity is
  created or consulted to represent it

#### Scenario: Invoking without a clock succeeds
- **WHEN** a method node's invocation adapter does not require clock
  context
- **THEN** the method node may be invoked with no clock supplied at all

### Requirement: No graph node represents a component as a whole
There SHALL be no graph node type representing "the component" itself.
A component's phases and callback methods are represented only through
method nodes; a component's state items are represented only through
existing value nodes. No method-node-related type introduced by this
capability SHALL be usable as, or require, a component-level node.

#### Scenario: A component is representable using only existing node kinds
- **WHEN** a component's phases and state items are each represented as
  graph nodes
- **THEN** every one of those nodes is either a method node or a value
  node, with no additional node required to represent the component
  itself

### Requirement: Method node execution is never scheduled by demand-driven update
A method node's invocation timing SHALL be decided entirely by whatever
code invokes it. Demand-driven graph update SHALL NOT invoke a method
node as a side effect of resolving some other node's staleness.

#### Scenario: Demand-driven update does not invoke a registered method node
- **WHEN** a method node is registered in a graph alongside other nodes,
  and demand-driven update is run to resolve some other node's value
- **THEN** the method node's invocation adapter is never called as part
  of that update
