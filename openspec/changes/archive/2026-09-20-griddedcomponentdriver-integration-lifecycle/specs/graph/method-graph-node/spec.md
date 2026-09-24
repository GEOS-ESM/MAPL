## ADDED Requirements

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
