## ADDED Requirements

### Requirement: An expression-typed export's N named references are resolved into a multi-input transform chain
For every advertised state item whose declared item class is
`EXPRESSION` (an item declared with an `expression:` string referencing
one or more other variables by name — e.g. `(A + B)/C`), the system
SHALL, as part of a dedicated resolution step distinct from ordinary
pairwise connection resolution: parse the expression string to obtain
its full set of referenced variable names; resolve each referenced name
against the component's own sibling export namespace, using the same
exact-name export lookup ordinary connection resolution already uses;
build one transform node declaring exactly one named input port per
referenced variable plus one output port; bind each referenced
variable's own resolved export node to its corresponding named input
port, and bind the output port to the expression item's own
already-advertised node; and add a dependency edge from that transform
node to the expression item's own node. This resolution SHALL run once
per advertised expression item, independent of whether any ordinary
connection names that item as a destination — an expression item is
itself an export (or internal item) whose own producers are discovered
by parsing its expression string, not declared through a connection.

#### Scenario: A two-operand expression resolves both referenced variables as named input ports
- **WHEN** a component advertises an export whose expression references
  exactly two other sibling exports by name
- **THEN** the system builds one transform node declaring exactly two
  named input ports, each bound to the correctly-resolved sibling
  export's own node, and one output port bound to the expression
  item's own node

#### Scenario: A three-or-more-operand expression resolves every referenced variable, not only the first two
- **WHEN** a component advertises an export whose expression references
  three or more other sibling exports by name
- **THEN** the system builds one transform node declaring one named
  input port for every referenced variable — not only the first two —
  each correctly bound to its own resolved sibling export's node

#### Scenario: A referenced variable with no matching export is reported, not silently dropped
- **WHEN** an expression references a variable name with no matching
  sibling export
- **THEN** the system reports that reference as unresolved, consistent
  with the existing "import with no matching export is left unresolved"
  requirement for ordinary connections, and does not build a transform
  node missing that input

#### Scenario: Re-resolving the same expression item does not duplicate its transform chain
- **WHEN** expression resolution runs more than once for the same
  already-resolved expression item, with no change to its own
  advertised identity or referenced variables
- **THEN** the system does not create a second transform node or a
  second set of port bindings for that same expression item

### Requirement: Expression evaluation produces a genuine evaluated result, not a placeholder
Executing the transform node built for an expression item's own
resolved chain (above) SHALL produce a real evaluated result — the
expression's own arithmetic applied to its bound inputs' current
values — using the same expression-evaluation mechanism the legacy
(non-graph-native) `ExpressionClassAspect`/`EvalTransform` mechanism
already uses, not a reimplementation and not a stub that leaves the
output unset or copies a single input unchanged.

#### Scenario: Executing an expression's transform node evaluates its arithmetic
- **WHEN** the transform node built for an expression item is executed
  against real, allocated input field values
- **THEN** the resulting output field's values are the expression's own
  evaluated arithmetic applied to those input values, not a copy of a
  single input and not left unset

### Requirement: An expression-typed import or export is excluded from ordinary resolution, but is resolvable through expression resolution
The existing "an import with no graph-native resolution path at all is
excluded from unresolved-import propagation" behavior SHALL continue to
exclude `WILDCARD`- and `SERVICE`-typed imports (those remain out of
scope for graph-native resolution), but SHALL NOT exclude
`EXPRESSION`-typed items: once this capability's own expression
resolution step (above) has run for a given component, an
`EXPRESSION`-typed item advertised on that component has a real
graph-native resolution path and is no longer treated as unconditionally
unresolvable for propagation purposes.

#### Scenario: A wildcard-typed import remains excluded from ordinary propagation
- **WHEN** a component advertises an import whose declared item class is
  `WILDCARD`
- **THEN** that import continues to be excluded from unresolved-import
  propagation, unaffected by this capability's own expression resolution
  step

#### Scenario: A service-typed import remains excluded from ordinary propagation
- **WHEN** a component advertises an import whose declared item class is
  `SERVICE`
- **THEN** that import continues to be excluded from unresolved-import
  propagation, unaffected by this capability's own expression resolution
  step

#### Scenario: An expression-typed export is no longer treated as having no resolution path
- **WHEN** a component advertises an export whose declared item class is
  `EXPRESSION`, and this capability's own expression resolution step has
  run for that component
- **THEN** that export is no longer treated as unconditionally excluded
  from graph-native resolution — it is resolved through expression
  resolution (above), not through ordinary exact-name connection
  resolution
