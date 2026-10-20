## Why

`docs/graph/spec/20-implementation-roadmap.md` §20.4.5 (Phase 7a) identifies
a gap orthogonal to every characteristic Phase 12 already wired: legacy's
`ClassAspect` (`BracketClassAspect`) performs a real *shape-changing* 1:1
conversion — a time-interpolation bracket (`MAPL_STATEITEM_BRACKET`, a
2-field bundle holding adjacent time levels plus interpolation weights)
collapsing into an ordinary `Field` via `TimeInterpolateTransform` — that
the graph-native characteristic family has no analog of today.
`GraphBuilder_ConnectionResolution_smod.F90`'s own
`try_materialize_field_extension`/`materialize_extension_chain` explicitly
reject any export or import whose `get_declared_item_type()` is not
`MAPL_STATEITEM_FIELD` (`unsupported_item_class`), so a bracket-shaped
export can never resolve today, matched or mismatched — confirmed by
`Test_Scenarios` having zero fixtures exercising `BRACKET` end to end.
This change closes that specific, narrowly-scoped gap, leaving the harder
Expression (7b) and Wildcard (7c) mechanisms — explicitly different
problems, not extensions of this one — untouched.

**Scoped to `Bracket`->`Field` only, not `VectorBracket`->`Vector`.**
Unlike the Field case, a resolved `VectorBracket`->`Vector` mismatch's
final node is itself a multi-field `FieldBundle` (`MAPL_FIELDBUNDLETYPE_
VECTOR`) carrying its own `vector_basis_kind` — `ExtensionMaterialization.
F90`'s `materialize_field_extension` has no FieldBundle-shaped analog, and
`GraphStateItem` has no field to carry `vector_basis_kind` through
materialization at all. Both are real, non-trivial gaps of their own,
better scoped as a follow-on once this change establishes the
shape-dispatch characteristic/Transform pattern for the simpler,
single-`Field`-payload case. This also matches the roadmap's own
recommendation to pilot same-shape `Vector` matching (`vector_1`) before
attempting genuine shape conversion — real vector shape-conversion
materialization is harder still, and deliberately left out here.

## What Changes

- Add a new `StateItemCharacteristic` subclass (`graph/state-item-
  characteristics`) comparing each side's declared item-class/shape
  (`BRACKET` vs. `FIELD`) instead of a plain value, dispatched through the
  *same* `find_mismatched_characteristics`/`build_chain` loop
  `graph/extension-reuse` already uses for Units and TypeKind — no new
  chain-building machinery, no new `GraphBuilder` resolution path.
- Its `build_transform` succeeds only for the `BRACKET`->`FIELD` pairing,
  returning a new graph-native `Transform` (`mapl_Transform_mod`, not
  legacy's `ExtensionTransform` interface) performing the bracket-to-field
  time interpolation, reading the same `interpolation_weights`/field-count
  convention `TimeInterpolateTransform.F90` already establishes on the
  bracket `FieldBundle`'s own ESMF `Info`. Every other pairing (including
  `VECTORBRACKET`->`VECTOR`) fails explicitly, matching the existing
  "unregistered characteristic fails loudly" contract.
- `GraphBuilder_Advertising_smod.F90`'s `populate_characteristics` inserts
  this new characteristic for every advertised item (mirroring Phase 12's
  own "insert unconditionally, let `needs_extension_for` decide" pattern
  for axes with a meaningful default).
- Narrow `try_materialize_field_extension`/`materialize_extension_chain`'s
  existing `unsupported_item_class` gate (`GraphBuilder_ConnectionResolution_
  smod.F90`) so a `BRACKET`-declared export resolving through this new
  characteristic's chain into a `FIELD`-declared import is materialized
  instead of rejected — every other non-field declared item class
  (`VECTOR`, `VECTORBRACKET`, `STATE`, `WILDCARD`, `SERVICE`,
  `EXPRESSION`) keeps failing exactly as it does today.
- Add a `Test_Scenarios` fixture exercising a bracket export resolving
  into a field import end to end (today's gap), alongside unit coverage
  for the new characteristic and its `Transform`.

## Capabilities

### New Capabilities
(none — this reuses the existing graph-native characteristic/extension-
chain architecture; no new capability directory is warranted)

### Modified Capabilities
- `graph/state-item-characteristics`: adds an eighth concrete
  `StateItemCharacteristic` subclass (item-class/shape axis), with a real
  `build_transform` for the `BRACKET`->`FIELD` pairing only.
- `graph/extension-reuse`: the existing "a non-field item class fails
  explicitly rather than getting a fabricated payload" requirement is
  narrowed — a bracket export resolved through the new shape
  characteristic's chain into a field import is now materialized, rather
  than failing; every other non-field declared item class (including
  `VECTORBRACKET`/`VECTOR`) is unaffected.

## Impact

- `superstructure/generic/graph/characteristics/`: one new
  `StateItemCharacteristic` subclass file, one new
  `StateItemCharacteristicKind` registration.
- `superstructure/generic/graph/transforms/`: one new `Transform`
  subclass (bracket-to-field time interpolation).
- `superstructure/generic/GraphBuilder_Advertising_smod.F90`: one more
  unconditional characteristic insertion in `populate_characteristics`.
- `superstructure/generic/GraphBuilder_ConnectionResolution_smod.F90`:
  narrows the `unsupported_item_class` gate in
  `try_materialize_field_extension`/`materialize_extension_chain` for the
  `BRACKET`->`FIELD` pairing only.
- `superstructure/generic/tests/scenarios/`: one new fixture (bracket
  export -> field import).
- `superstructure/generic/graph/tests/`: new unit tests for the
  characteristic and its `Transform`, alongside
  `Test_ExtensionResolution.pf`'s existing style.
- No change to legacy `ClassAspect`/`BracketClassAspect`/
  `VectorBracketClassAspect` (`superstructure/generic/specs/`) — those
  remain the `GRAPH_MODE_REGISTRY`/legacy-coupler path, unaffected.
- No change to `VectorBracket`/`Vector` resolution or materialization —
  left for a follow-on change (see Why).
