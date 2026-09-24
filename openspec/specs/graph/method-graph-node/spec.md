# Method Graph Node Specification

## Purpose

Provides a graph node type representing an explicitly invoked method —
either a GridComp initialize/run phase or an attached ESMF State
callback method — through one unified shape, so the two ESMF call
signatures are hidden behind an interchangeable invocation adapter
rather than requiring two different node kinds.

## Requirements

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

### Requirement: A driver key resolves to an owned driver without exposing driver identity as a pointer
A method node's driver key SHALL resolve, at invocation time, to a
concrete driver owned by the component the method node belongs to —
either that component's own driver or, for a driver key naming one of
its children, the corresponding child driver. Resolution SHALL NOT
require the method node, its invocation adapter, or any graph-visible
object to hold a direct reference to, or a copy of, the resolved
driver; the driver remains owned exactly where it already was before
resolution.

#### Scenario: Own-driver key resolves to the component's own driver
- **WHEN** a method node's driver key names the owning component's own
  driver
- **THEN** invocation reaches that component's own driver, and no
  graph-visible object retains a reference to it afterward

#### Scenario: Child-driver key resolves to the named child's driver
- **WHEN** a method node's driver key names one of the owning
  component's children
- **THEN** invocation reaches that child's driver, and no graph-visible
  object retains a reference to it afterward

#### Scenario: Unresolvable driver key fails loudly
- **WHEN** a method node is invoked with a driver key that names
  neither the owning component's own driver nor any of its children
- **THEN** invocation fails with an explicit error rather than
  invoking some other driver or silently doing nothing

### Requirement: GridComp phase invocation reaches the real driver's own phase entry points
Invoking a method node whose invocation adapter models a GridComp phase
SHALL result in exactly the resolved driver's own initialize, run, or
finalize behavior for that phase being exercised, using the method
node's current argument bindings as that phase's import/export
content. No separate, independently-maintained reimplementation of
GridComp phase-calling behavior SHALL be exercised as part of this
invocation path.

#### Scenario: Invoking an initialize-phase method node runs the driver's initialize behavior
- **WHEN** a method node modeling a GridComp initialize phase is
  invoked with its driver key resolved
- **THEN** the resolved driver's own initialize behavior for that phase
  executes, observably reflecting the method node's current argument
  bindings

#### Scenario: Invoking a run-phase method node runs the driver's run behavior
- **WHEN** a method node modeling a GridComp run phase is invoked with
  its driver key resolved
- **THEN** the resolved driver's own run behavior for that phase
  executes, observably reflecting the method node's current argument
  bindings

### Requirement: Default-network invocation pulls all bound imports before and advances all bound exports after
Invoking a method node on the default dependency network SHALL, as an
inseparable part of "invoke": first ensure every one of the method
node's bound input and input-output arguments is current; then perform
the invocation; then, only if invocation succeeds, mark every one of
the method node's bound output and input-output arguments as newly
current. This SHALL happen unconditionally for every default-network
invocation, regardless of which arguments the invoked phase actually
touched.

#### Scenario: Bound inputs are made current before invocation
- **WHEN** a method node with stale bound input arguments is invoked on
  the default network
- **THEN** every bound input and input-output argument is current
  before the underlying phase executes

#### Scenario: Bound outputs are advanced after successful invocation
- **WHEN** a method node is invoked on the default network and the
  underlying phase completes successfully
- **THEN** every one of the method node's bound output and
  input-output arguments is marked newly current immediately afterward

#### Scenario: Failed invocation does not advance bound outputs
- **WHEN** a method node's invocation on the default network fails
- **THEN** none of its bound output or input-output arguments are
  marked newly current as a result of that failed attempt

### Requirement: The realize/accept/realize initialization cycle terminates on defined convergence or fails explicitly
The initialization cycle that repeats realizing provided items,
accepting transferred connections, and realizing newly-qualified items
SHALL terminate either when an iteration makes no further progress, or
when a fixed iteration limit is reached. Reaching the iteration limit
without having stopped making progress SHALL be treated as an explicit
error, never as a silent partial result accepted as if it were
converged.

#### Scenario: Cycle stops once an iteration makes no further progress
- **WHEN** the initialization cycle runs and some iteration completes
  with no item newly reaching a resolved state on any characteristic
- **THEN** the cycle stops after that iteration, having made no further
  realize/accept/realize calls

#### Scenario: Cycle reaching the iteration limit without convergence fails explicitly
- **WHEN** the initialization cycle runs and every iteration up to the
  fixed limit still makes progress without ever reaching a
  no-progress iteration
- **THEN** initialization reports an explicit error rather than
  proceeding to the next lifecycle phase as if converged

#### Scenario: Cycle converging within the iteration limit proceeds normally
- **WHEN** the initialization cycle reaches a no-progress iteration
  before the fixed iteration limit is reached
- **THEN** initialization proceeds to the next lifecycle phase with no
  error
