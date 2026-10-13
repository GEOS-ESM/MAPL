## Why

The graph layer currently maintains two parallel, independently-defined
models for the same underlying concept legacy's coupler handles with one
(`StateItemAspect`): "a named, comparable, possibly-adaptable property
of a state item." `mapl_Characteristic_mod`'s `Characteristic`/
`CharacteristicId` family (`UnitsCharacteristic`,
`TypeKindCharacteristicAdapter`, `VerticalGridCharacteristic`,
`GeomCharacteristic`) was built for `graph/extension-reuse` (3c) to
compare `VariableSpec`-derived metadata at connection-resolution time,
and is the only one of the two with a real `build_transform` (adapt a
mismatch into a transform chain). `mapl_StateItemCharacteristic_mod`'s
`StateItemCharacteristic`/`StateItemCharacteristicKind` family
(`PhysicalUnitsCharacteristic`, `TypeKindCharacteristic`,
`VerticalCoordinateCharacteristic`, `GeometryCharacteristic`,
`AttributesCharacteristic`, `ConservationCharacteristic`,
`NormalizationCharacteristic`, `QuantityTypeCharacteristic`,
`StandardNameCharacteristic`, `UngriddedDimsCharacteristic`) was built
later for a different purpose — `GraphStateItem`'s own persistent
per-item characteristics map, REQ-CHAR-016/017/018's node-sharing and
revision-invalidation bookkeeping — and was deliberately kept
independent of the first family at the time (archived
`state-item-characteristics` change, design.md D6) specifically because
giving it `build_transform` would have required a `ComponentGraph`
reference, which would have created a module-use cycle through
`GraphStateItem` (which already must depend on `ComponentGraph` via
`StateItemNode`).

`decouple-transform-from-graph` (landed) removed exactly that obstacle:
`Characteristic%build_transform` no longer takes a `ComponentGraph`/
`NodeId` argument, so nothing in either family's own interface creates
a cycle anymore. With the blocker gone, there is no remaining reason to
maintain two parallel comparison/adaptation hierarchies for the same
concept — doing so means ten additional characteristic kinds exist with
no adaptation story at all (the `StateItemCharacteristic`-only ones:
attributes, conservation, normalization, quantity-type, standard-name,
ungridded-dims), four kinds exist twice under different names for the
sole reason of avoiding a name collision between the two families
(`UnitsCharacteristic`/`PhysicalUnitsCharacteristic`,
`VerticalGridCharacteristic`/`VerticalCoordinateCharacteristic`,
`GeomCharacteristic`/`GeometryCharacteristic`, plus
`TypeKindCharacteristicAdapter` wrapping `TypeKindCharacteristic` by
composition purely to bridge the two interfaces), and
`GraphBuilder`'s own connection-resolution path (`resolve_one`) is
blocked from reading `GraphStateItem%characteristics` directly for
exactly this reason — a separate, planned change
(`graph-builder-stateitem-resolution`) needs this merge as a
prerequisite.

## What Changes

- **BREAKING** (internal-only — no spec-level requirement names either
  family by its Fortran type name; `graph/state-item-characteristics`
  and `graph/extension-reuse` both describe only observable comparison/
  adaptation outcomes, not which internal type produces them):
  - `StateItemCharacteristic`'s abstract interface gains a third
    deferred method, `build_transform(this, goal, transformer, rc)`
    (same signature `Characteristic%build_transform` already has,
    post-`decouple-transform-from-graph` — no `ComponentGraph`/`NodeId`
    argument). `ValueCharacteristic`/`ReferenceCharacteristic` (the two
    intermediate abstract tiers) are unaffected — the new deferred
    method lands on their common ancestor, same tier `get_kind`/
    `needs_extension_for` already live on.
  - Every `StateItemCharacteristic` concrete subclass gains a
    `build_transform` implementation. For the four kinds that already
    have a `Characteristic`-side sibling with a real implementation
    (`PhysicalUnitsCharacteristic` <- `UnitsCharacteristic`'s;
     `TypeKindCharacteristic` <- `TypeKindCharacteristicAdapter`'s;
     `VerticalCoordinateCharacteristic` <-
     `VerticalGridCharacteristic`'s dimension-overlap logic;
     `GeometryCharacteristic` <- `GeomCharacteristic`'s), the
    implementation is moved over, not reinvented. The remaining six
    kinds (`AttributesCharacteristic`, `ConservationCharacteristic`,
    `NormalizationCharacteristic`, `QuantityTypeCharacteristic`,
    `StandardNameCharacteristic`, `UngriddedDimsCharacteristic`) gain a
    `build_transform` that fails explicitly (`_FAIL`) — matching
    `VerticalGridCharacteristic`'s/`GeomCharacteristic`'s own existing
    "no real provider yet" precedent exactly; this is not new behavior,
    since none of these six had any adaptation story before this
    change either.
  - `mapl_Characteristic_mod`, `mapl_CharacteristicId_mod`,
    `UnitsCharacteristic.F90`, `TypeKindCharacteristicAdapter.F90`,
    `VerticalGridCharacteristic.F90`, `GeomCharacteristic.F90` are
    removed outright (not deprecated/kept-as-dead-code) once every real
    caller has migrated to the `StateItemCharacteristic` family's
    equivalent — mirrors this project's own established precedent
    (`graph-builder-stateitem-resolution`'s own tasks.md risk note:
    "keeping removed-but-unused helpers around contradicts the change's
    own purpose").
  - `GraphBuilder.F90`/`GraphBuilder_ConnectionResolution_smod.F90`/
    `GraphBuilder_Geometry_smod.F90`/`GraphBuilder_CallbackWiring_smod.F90`
    — every production call site currently building a `CharacteristicMap`
    from `UnitsCharacteristic`/`TypeKindCharacteristicAdapter`/
    `VerticalGridCharacteristic`/`GeomCharacteristic` is updated to build
    a `StateItemCharacteristicMap` from the equivalent
    `StateItemCharacteristic` subclass instead; `find_mismatched_characteristics`
    call sites move to `find_mismatched_state_item_characteristics`
    (`mapl_StateItemCharacteristicDetection_mod`, already shipped,
    currently unused by any production code path).
  - `mapl_ExtensionResolution_mod`'s `find_or_build_extension_chain`/
    `build_chain` are updated to operate on
    `StateItemCharacteristicMap`/`StateItemCharacteristicKind` instead
    of `CharacteristicMap`/`CharacteristicId` — the chain-building
    algorithm itself (reuse-key lookup, per-link `build_transform` call,
    `TransformGraphNode` registration/port-binding) is unchanged, only
    its map/key types.
- **Explicitly out of scope**: wiring `GraphBuilder`'s connection
  resolution to read `GraphStateItem%characteristics` directly instead
  of building a fresh map from `VariableSpec` on every call — that is
  `graph-builder-stateitem-resolution`'s own concern, which depends on
  this change landing first but is not performed by it. This change
  only removes the two-family split; it does not change *when* or *from
  what source* a characteristics map gets built.

## Capabilities

### Modified Capabilities
- `graph/state-item-characteristics`: gains a new requirement — a
  concrete `StateItemCharacteristic` subclass can build a `Transform`
  resolving a mismatch against a goal of the same kind, or must fail
  explicitly if no real provider is implemented yet. This is new
  requirement content (an `ADDED` requirement in the delta spec, not a
  restatement of an existing one): no existing requirement in this
  capability's spec currently describes a `build_transform`/adaptation
  capability by name — requirement 5 ("Transform-insertion order is
  delegated to a per-kind strategy") already presupposes "reconciling
  Transforms" exist for a `StateItemCharacteristic` mismatch, but no
  requirement specifies how one is actually obtained; this change is
  what makes that presupposition concretely true for the first time.
  No change to any existing scenario's observable mismatch-detection,
  status, sharing, or propagation outcome.

`graph/extension-reuse` is not modified: every one of its requirements
and scenarios is already written in terms of observable export/import
-pair bridging behavior with no reference to either Fortran type
family by name (`CharacteristicMap`/`CharacteristicId` vs.
`StateItemCharacteristicMap`/`StateItemCharacteristicKind`) — the
underlying map/key types this capability's chain-building operates on
change, but nothing it is specified to do changes, so no delta spec is
needed for it (per the "if the implementation can change without
changing externally visible behavior, it likely does not belong in the
spec" test).

## Impact

- **Affected code**: `StateItemCharacteristic.F90` (new deferred
  method), `PhysicalUnitsCharacteristic.F90`/`TypeKindCharacteristic.F90`/
  `VerticalCoordinateCharacteristic.F90`/`GeometryCharacteristic.F90`
  (gain real `build_transform`, moved from their `Characteristic`-side
  sibling), `AttributesCharacteristic.F90`/`ConservationCharacteristic.F90`/
  `NormalizationCharacteristic.F90`/`QuantityTypeCharacteristic.F90`/
  `StandardNameCharacteristic.F90`/`UngriddedDimsCharacteristic.F90`
  (gain explicit-failure `build_transform`),
  `mapl_ExtensionResolution_mod` (`find_mismatched_characteristics`/
  `find_or_build_extension_chain`/`build_chain` re-typed),
  `GraphBuilder.F90`/`GraphBuilder_ConnectionResolution_smod.F90`/
  `GraphBuilder_Geometry_smod.F90`/`GraphBuilder_CallbackWiring_smod.F90`
  (every `CharacteristicMap`-building call site re-typed to
  `StateItemCharacteristicMap`).
- **Removed code**: `Characteristic.F90`, `CharacteristicId.F90`,
  `CharacteristicStatus.F90` (used only by the surviving family already
  — confirm no `Characteristic`-side-only use before removal),
  `UnitsCharacteristic.F90`, `TypeKindCharacteristicAdapter.F90`,
  `VerticalGridCharacteristic.F90`, `GeomCharacteristic.F90`, and their
  dedicated test files (`Test_Characteristic.pf`,
  `Test_GeomCharacteristic.pf`) once every assertion they make is
  covered by an equivalent `StateItemCharacteristic`-side test
  (`Test_StateItemCharacteristic.pf`/`Test_StateItemCharacteristicSubclasses.pf`/
  `Test_StateItemCharacteristicKind.pf`) or ported there.
- **Existing code read, not modified**: `mapl_StateItemCharacteristicDetection_mod`'s
  `find_mismatched_state_item_characteristics` (already shipped, this
  change makes it the one production detection entry point instead of
  an unused-by-production addition); `GraphStateItem%characteristics`/
  `set_characteristic`/`get_characteristics` (already shipped, unchanged
  by this merge — still populated/read exactly as `state-item
  -characteristics` left it; `graph-builder-stateitem-resolution` is
  what will later make `GraphBuilder` a real producer/consumer of it).
- **Tests**: every existing `Test_StateItemCharacteristic.pf`,
  `Test_StateItemCharacteristicDetection.pf`,
  `Test_StateItemCharacteristicKind.pf`,
  `Test_StateItemCharacteristicSubclasses.pf`,
  `Test_SetSharedCharacteristic.pf`,
  `Test_ExtensionResolution.pf` case must continue to pass with its
  existing expected values unchanged once re-typed to the surviving
  map/key types (`Test_ExtensionResolution.pf`'s own `CharacteristicMap`/
  `CharacteristicId` references migrate to
  `StateItemCharacteristicMap`/`StateItemCharacteristicKind`). New
  coverage: a `build_transform` test for each of the six previously
  -adaptation-free `StateItemCharacteristic` subclasses, confirming each
  fails explicitly (mirrors `Test_GeomCharacteristic.pf`'s own
  `test_geom_characteristic_build_transform_fails_explicitly` precedent).
- **Dependencies**: depends on `decouple-transform-from-graph` (landed)
  — this change could not proceed before that one removed
  `ComponentGraph`/`NodeId` from `build_transform`'s own interface.
  Does not depend on and is not required by `3b2-cross-component-import
  -propagation`. Is itself a prerequisite for
  `graph-builder-stateitem-resolution`, which currently cannot proceed
  past its own task 2.2/4.1 without this merge landing first.
- **Out of scope**: wiring `GraphBuilder`'s connection resolution to
  read `GraphStateItem%characteristics` directly (see "What Changes" -
  `graph-builder-stateitem-resolution`'s own concern); any change to
  `mapl_SetSharedCharacteristic_mod`'s own mutator/structural-reset
  logic (unaffected by this merge - it already operates purely on the
  `StateItemCharacteristic` family).
