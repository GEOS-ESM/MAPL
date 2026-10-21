## Why

`docs/graph/spec/20-implementation-roadmap.md` §20.4.5 (Phase 7b) identifies
the second of three legacy `ClassAspect` shape-dispatch mechanisms with no
graph-native analog: `ExpressionClassAspect` -> `FieldClassAspect` via
`EvalTransform` — an **N:1** conversion (an arbitrary number of named
variable references collapsing into one evaluated `Field`), unlike Phase
7a's 1:1 `Bracket`->`Field` time-interpolation. `GraphBuilder`'s own
`is_ordinary_resolvable_import` (`GraphBuilder_ConnectionResolution_smod.F90`)
currently excludes `MAPL_STATEITEM_EXPRESSION` from graph-native resolution
entirely, identically to `WILDCARD`/`SERVICE` — an expression-typed export
or import has no graph-native resolution path today, confirmed by
`Test_Scenarios`' three `expression*` fixtures exercising only the legacy
`GRAPH_MODE_REGISTRY` path. This change closes that gap for `Expression`
specifically, leaving `Wildcard`/`Service` (Phase 7c — pattern-based,
topology-discovery fan-in, a genuinely different problem per the roadmap)
untouched.

**Why this cannot reuse Phase 7a's mechanism directly.** 7a's
`ItemShapeCharacteristic` dispatches through `find_mismatched_
characteristics`/`build_chain` (`ExtensionResolution.F90`), whose
`StateItemCharacteristic%build_transform(this, goal, transformer, rc)`
interface is fixed-arity: exactly one `this` (established) and one `goal`
value, producing exactly one `Transform` with exactly the two ports that
loop always binds (`INPUT_PORT_NAME`/`OUTPUT_PORT_NAME`). An expression's
input count is not fixed at one — `(A + B)/C` has three — so there is no
single `StateItemCharacteristic` pair to compare and no way to express "N
named references" through that interface without changing its arity for
every other characteristic too. This matches the roadmap's own framing:
legacy's own `ExpressionClassAspect%make_transform` is already an
exception to the uniform `ClassAspect` interface (it reaches outside normal
scope via `registry%extend()` once per referenced variable), not something
7a's pairwise mechanism was ever meant to generalize to.

**Why a dedicated resolution hook, not a `resolve_one` extension.** Ordinary
connection resolution (`resolve_one`, `GraphBuilder_ConnectionResolution_
smod.F90`) is fundamentally pairwise — one destination import resolved
against one matching source export, by exact short name. An expression
export's own real producers are a set of *other* exports, named inside the
expression string itself (`parser_variables_in_expression`), discovered by
parsing text, not by any connection the user declared. This is exactly the
shape `run_geometry_hook` (Phase 4e) already established for a different
graph-wide, non-pairwise resolution step run once per component outside
`resolve_one`'s own connection loop — Expression resolution follows that
same precedent rather than overloading `resolve_one`.

**Why no composite `StateItemNode` (member) structure.** The roadmap's own
"working hypothesis" suggested modeling an expression's N inputs as
declared composite members (`VariableSpec%declare_member`/
`CompositeStateMaterialization.F90`), reusing the mechanism `4c`/`4f`
already apply to callback states and vertical grids. Investigated and
rejected for this change: `declare_member` hard-asserts `itemType ==
MAPL_STATEITEM_STATE` (an expression's itemType is `EXPRESSION`), and
`CompositeStateMaterialization`'s own `materialize_composite` immediately
allocates a real `ESMF_State` — the wrong terminal shape for an expression,
whose resolved payload must be a plain `Field` (`try_materialize_field_
extension`'s own Field-only materialization, unchanged by this proposal).
Forcing a `STATE`-shaped intermediate purely to reuse `declare_member`
would fight both of those constraints for no structural benefit: an
expression's N referenced variables are naturally already-existing,
already-advertised sibling `StateItemNode`s (another component's real
export), not sub-structure *of* the expression item itself. They are bound
directly as a new `ExpressionTransform`'s own named input ports instead —
see design.md D1.

## What Changes

- Add `MAPL_STATEITEM_EXPRESSION` as the one graph-native vocabulary
  addition this change needs (`MAPLStateItemFlag.F90`) — the ESMF-level
  flag (`infrastructure/esmf/StateItem.F90`) already exists; only the
  graph-native variant registry is missing it.
- Add a dedicated `GraphBuilder` resolution step
  (`graphbuilder_run_expression_hook`, invoked alongside `run_connect_hook`)
  that, for every advertised `EXPRESSION`-typed item in a component's own
  graph: parses its expression string via the existing
  `superstructure/state/StateArithmeticParser_mod::parser_variables_in_
  expression` (reused as-is, not reimplemented), resolves each referenced
  variable name against the component's own sibling export namespace
  (`find_export_node_id`, reused as-is), builds one new `ExpressionTransform`
  (`Transform` subclass, `superstructure/generic/graph/transforms/`)
  declaring one named input port per referenced variable plus one
  `destination_field` output port, binds every port via `graph%bind_port`
  (first real multi-input-port consumer of `PortSpec`/`PortBindingTable`),
  and wires the output to the expression item's own already-advertised
  `NodeId` via an ordinary `add_dependency` edge.
- `ExpressionTransform%compute` performs genuine expression evaluation —
  assembling its bound named inputs' `Field`s into an `ESMF_State` and
  calling the existing `StateArithmeticParser_mod::StateEval` (same
  byte-code expression engine legacy's own `EvalTransform` already calls),
  not a stub or placeholder.
- Narrow `is_ordinary_resolvable_import`'s exclusion
  (`GraphBuilder_ConnectionResolution_smod.F90`) so an `EXPRESSION`-typed
  item is no longer unconditionally treated as "no graph-native resolution
  path" — it is resolvable once this change's own hook has run for it.
  `WILDCARD`/`SERVICE` remain excluded, unaffected (Phase 7c, out of
  scope).
- Add `Test_Scenarios` coverage reusing the existing `expression`/
  `expression_match`/`expression_defer_geom` fixtures under
  `GRAPH_MODE_BOTH`/`GRAPH_MODE_GRAPH`, plus direct unit coverage for the
  new hook and `ExpressionTransform`'s own evaluation correctness.

## Capabilities

### New Capabilities
(none — this extends the existing graph-native `GraphBuilder`/`Transform`/
`PortBindingTable` architecture with one new resolution step and one new
`Transform` subclass; no new capability directory is warranted)

### Modified Capabilities
- `graph/graph-builder`: adds a new resolution step (the expression hook)
  resolving N named references per expression item, and narrows the
  "an import with no graph-native resolution path is excluded from
  propagation" requirement to no longer include `EXPRESSION`.

No other capability's own requirements change. `graph/transform-graph-node`
already specifies arbitrary-N named input/output port declarations
generically (its own "Multiple named inputs and outputs" scenario already
declares three input ports) — this change is simply that capability's
first real, production (non-test-double) multi-input-port consumer, not a
requirement change.

## Impact

- `superstructure/generic/graph/MAPLStateItemFlag.F90`: one new
  `MAPL_StateItem_Flag` constant (`MAPL_STATEITEM_EXPRESSION`).
- `superstructure/generic/graph/transforms/`: one new `Transform` subclass
  (`ExpressionTransform`), declaring a variable number of named input
  ports.
- `superstructure/generic/GraphBuilder.F90` /
  `GraphBuilder_ConnectionResolution_smod.F90` (or a new submodule,
  decided in design.md): one new resolution hook
  (`run_expression_hook`/`graphbuilder_run_expression_hook`), one narrowed
  gate (`is_ordinary_resolvable_import`).
- `superstructure/generic/OuterMetaComponent/initialize_accept_transfer.F90`
  (or wherever the new hook is invoked, decided in design.md): one new
  call site, timed alongside `run_connect_hook`.
- `superstructure/generic/tests/scenarios/`: existing `expression*`
  fixtures gain graph-native coverage (no new fixture files expected,
  unless a gap surfaces during implementation).
- `superstructure/generic/graph/tests/`: new unit tests for
  `ExpressionTransform` and the new hook, alongside
  `Test_ExtensionResolution.pf`'s existing style.
- No change to legacy `ExpressionClassAspect`/`EvalTransform`
  (`superstructure/generic/specs/`, `superstructure/generic/transforms/`)
  — those remain the `GRAPH_MODE_REGISTRY`/legacy-coupler path, unaffected.
- No change to `Wildcard`/`Service` resolution (Phase 7c, explicitly out of
  scope) or to Phase 7a's `Bracket`/`VectorBracket` mechanism (unaffected,
  already landed).
