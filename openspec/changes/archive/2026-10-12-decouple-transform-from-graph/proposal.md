## Why

`mapl_Transform_mod`'s `compute(this, rc)` takes no arguments beyond
`this`/`rc` — not even its own bound input/output values. Because of
that, every concrete `Transform` (`UnitsConverterTransform`,
`TypeKindConverterTransform`) is forced to self-serve: each stores
`class(ComponentGraph), pointer :: graph` plus both bound `NodeId`s at
construction time, purely so `compute()` can look its own inputs/outputs
up at execution time (`this%graph%get_node(this%input_node_id)`). This
in turn forces `mapl_Characteristic_mod`'s `build_transform` — the
deferred method that constructs a `Transform` from two mismatched
`Characteristic` values — to also take `class(ComponentGraph)` and both
`NodeId`s as arguments, purely to forward them into the constructor.

This is backwards relative to how MAPL's own legacy coupler already
works: a legacy `ExtensionTransform%update(this, importState,
exportState, clock, rc)` is handed its states directly by the caller on
every call — it has no equivalent self-lookup, and no `StateRegistry`
dependency of its own. The graph-side `Transform`/`Characteristic`
pair's dependence on `ComponentGraph` is not an intrinsic property of
"what a transform/characteristic is" — it exists only because
`compute()`'s signature gives a `Transform` no other way to receive its
own bound data, and `build_transform` inherited that same constraint by
association.

This dependency is also what currently prevents merging
`mapl_Characteristic_mod`'s `Characteristic`/`CharacteristicId` family
with `mapl_StateItemCharacteristic_mod`'s `StateItemCharacteristic`/
`StateItemCharacteristicKind` family (two parallel, independently
-maintained models for what is conceptually one legacy `Aspect`
responsibility) — `GraphStateItem` already must depend on
`ComponentGraph` (via `StateItemNode`), so giving `GraphStateItem` a
`Characteristic`-typed component would create a module-use cycle
through `Characteristic` → `ComponentGraph` → `StateItemNode` →
`GraphStateItem`. Removing `ComponentGraph` from `Characteristic`'s own
interface removes that cycle and is a prerequisite for any future change
that merges the two characteristic families — but this change does not
perform that merge; it only removes the obstacle.

## What Changes

- **BREAKING** (internal-only — no spec-level requirement in
  `graph/transform-graph-node` or `graph/extension-reuse` describes
  *how* a `Transform`/`Characteristic` reaches its bound values, only
  their black-box behavior; no scenario in either spec changes):
  - `Transform%compute`'s deferred interface gains two new arguments —
    the transform's own bound input and output values (one
    `GraphStateItem` per declared input/output port, keyed by port
    name) — supplied by the caller, not self-fetched. `Transform` no
    longer needs, and MUST NOT hold, any `ComponentGraph`/`NodeId`
    reference of its own.
  - `TransformGraphNode%execute()` gains the responsibility of
    resolving its own declared ports' bound `NodeId`s (via the
    existing external port-binding table it already has access to
    through `ComponentGraph`) into `GraphStateItem` values immediately
    before calling `compute()`, and passes them in.
  - `UnitsConverterTransform`/`TypeKindConverterTransform` drop their
    stored `graph`/`input_node_id`/`output_node_id` fields entirely;
    `compute()` reads its bound values from its new arguments instead
    of self-fetching them.
  - `Characteristic%build_transform`'s deferred interface drops its
    `class(ComponentGraph)`, `input_node_id`, and `output_node_id`
    arguments — it only ever forwarded them unused into whichever
    concrete `Transform` it constructed. The method becomes a pure
    value-to-strategy constructor: given `this` (the export's own
    characteristic value) and `goal` (the import's), build a
    `Transform` that knows how to convert between them, with no graph
    concept involved.
  - `mapl_ExtensionResolution_mod`'s `build_chain` updates its one call
    site of `build_transform` for the new, narrower signature; its own
    responsibility (constructing the two `NodeId`s, registering the
    `TransformGraphNode`, binding its ports) is unchanged — it already
    does this work itself, the dropped arguments were never its own
    source of truth for it.
- **Explicitly out of scope**: merging `Characteristic`/
  `StateItemCharacteristic` into one family. This change only removes
  the `ComponentGraph` dependency that makes such a merge impossible;
  it does not perform the merge itself (left to a follow-up change).
  No change to `ComponentGraph.F90`'s own public API, to
  `mapl_ExtensionResolution_mod`'s chain-building algorithm or reuse
  logic, or to `GraphBuilder.F90`'s connection-resolution callers of
  that module.

## Capabilities

### Modified Capabilities
None. `graph/transform-graph-node` and `graph/extension-reuse` both
describe observable behavior (port declarations/bindings, chain
creation/reuse, materialization) that is unaffected — this change is a
pure internal-mechanism refactor of how a `Transform` receives its own
bound data, not a change to what any transform/chain does. Per this
project's own convention (e.g. `graph-builder-stateitem-resolution`'s
own precedent for a mechanism-only correction), this would normally
warrant a spec delta restating the existing requirement's mechanism —
but no requirement in either spec currently makes any claim about
`compute()`'s or `build_transform`'s own argument-passing mechanism to
restate; both specs are already written in terms of black-box behavior
only. Hence `skip_specs: true`.

## Impact

- **Affected code**: `Transform.F90` (new `compute` interface),
  `TransformGraphNode.F90` (`execute()` resolves and passes bound
  values), `UnitsConverterTransform.F90`/`TypeKindConverterTransform.F90`
  (drop stored graph/NodeId fields, read bound values from new
  arguments), `Characteristic.F90` (new `build_transform` interface),
  `UnitsCharacteristic.F90`/`TypeKindCharacteristicAdapter.F90`/
  `VerticalGridCharacteristic.F90`/`GeomCharacteristic.F90` (drop
  graph/NodeId arguments from `build_transform`, construct their
  `Transform` without them), `ExtensionResolution.F90`'s `build_chain`
  (updates its one `build_transform` call site).
- **Existing code read, not modified**: `ComponentGraph%get_port_binding`/
  `get_node` (already public, used by `TransformGraphNode%execute()`'s
  new resolution step); `ComponentGraph_DemandDrivenUpdate.F90`'s
  `finish_transform` (already resolves bound `NodeId`s for revision
  bookkeeping immediately before calling `execute()` — this change adds
  a parallel, same-shaped resolution for values, not a new traversal).
- **New Fortran types**: none anticipated — the bound-values argument to
  `compute()` is expected to be a plain, small gFTL map (port name ->
  `GraphStateItem`) or a short array parallel to the port declaration
  order; design.md will settle the exact shape.
- **Tests**: every existing `Test_TransformGraphNode.pf`,
  `Test_UnitsConverterTransform.pf`, `Test_Characteristic.pf`,
  `Test_ExtensionResolution.pf` case must continue to pass with its
  existing expected values unchanged (same observable outcome) —
  updated only where a test constructs a `Transform`/`Characteristic`
  subclass directly and must adapt to the new constructor/interface
  shape. No new spec-level test coverage is added (no behavior changes).
- **Dependencies**: none — this change is self-contained and does not
  depend on any in-flight or planned change. It is a prerequisite for a
  future `Characteristic`/`StateItemCharacteristic` family-merge change
  and for `graph-builder-stateitem-resolution`, both of which should
  land after this one.
- **Out of scope**: merging the two characteristic families (see "What
  Changes"); any change to `GraphBuilder.F90`'s connection-resolution
  logic (`graph-builder-stateitem-resolution`'s own concern, deferred
  until after this change and the family-merge change land).
