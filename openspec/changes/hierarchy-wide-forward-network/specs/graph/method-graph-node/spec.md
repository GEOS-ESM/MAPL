## RENAMED Requirements

- FROM: `### Requirement: Default-network invocation pulls all bound imports before and advances all bound exports after`
- TO: `### Requirement: Forward-network invocation pulls all bound imports before and advances all bound exports after`

## MODIFIED Requirements

### Requirement: Forward-network invocation pulls all bound imports before and advances all bound exports after
Invoking a method node on the forward network SHALL, as an
inseparable part of "invoke": first ensure every one of the method
node's bound input and input-output arguments is current, following
each argument's producer wherever that edge was declared; then perform
the invocation; then, only if invocation succeeds, mark every one of
the method node's bound output and input-output arguments as newly
current. This SHALL happen unconditionally for every forward-network
invocation, regardless of which arguments the invoked phase actually
touched.

#### Scenario: Bound inputs are made current before invocation
- **WHEN** a method node with stale bound input arguments is invoked on
  the forward network
- **THEN** every bound input and input-output argument is current
  before the underlying phase executes

#### Scenario: Bound outputs are advanced after successful invocation
- **WHEN** a method node is invoked on the forward network and the
  underlying phase completes successfully
- **THEN** every one of the method node's bound output and
  input-output arguments is marked newly current immediately afterward

#### Scenario: Failed invocation does not advance bound outputs
- **WHEN** a method node's invocation on the forward network fails
- **THEN** none of its bound output or input-output arguments are
  marked newly current as a result of that failed attempt
