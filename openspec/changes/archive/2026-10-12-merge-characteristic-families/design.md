## Context

See proposal.md - Why/What Changes. Two concrete facts shape the
approach:

- `decouple-transform-from-graph` (landed) already changed
  `Characteristic%build_transform`'s signature to
  `build_transform(this, goal, transformer, rc)` - no `ComponentGraph`/
  `NodeId` arguments. `StateItemCharacteristic` has no `build_transform`
  deferred method at all today. Adding one with this exact signature is
  therefore not a new design question for the *interface shape* - it is
  already settled by the earlier change; this change only decides which
  concrete subclasses implement it and how four of them absorb an
  existing real implementation.
- Of the ten `StateItemCharacteristic` concrete subclasses, four have a
  same-concept `Characteristic`-side sibling with a real
  `build_transform` already written and tested
  (`UnitsCharacteristic`/`PhysicalUnitsCharacteristic`,
  `TypeKindCharacteristicAdapter`/`TypeKindCharacteristic`,
  `VerticalGridCharacteristic`/`VerticalCoordinateCharacteristic`,
  `GeomCharacteristic`/`GeometryCharacteristic`). The other six
  (`AttributesCharacteristic`, `ConservationCharacteristic`,
  `NormalizationCharacteristic`, `QuantityTypeCharacteristic`,
  `StandardNameCharacteristic`, `UngriddedDimsCharacteristic`) have no
  `Characteristic`-side sibling at all - confirmed by grep, there is no
  `AttributesCharacteristic`/`ConservationCharacteristic`/etc. under
  `mapl_Characteristic_mod`'s hierarchy anywhere in the codebase. Each
  of these six's own header comment already documents the reason: no
  real adaptation exists for that axis anywhere, graph-native or
  legacy (e.g. `AttributesCharacteristic.F90`: "legacy
  AttributesAspect%make_transform always returns NullTransform").

## Goals / Non-Goals

**Goals:**
- Give every `StateItemCharacteristic` concrete subclass a
  `build_transform` implementation - four real (moved from their
  `Characteristic`-side sibling, not reinvented), six explicit-failure
  (matching the already-established "no real provider yet" pattern
  `VerticalGridCharacteristic`/`GeomCharacteristic` already use today).
- Retire `mapl_Characteristic_mod`/`mapl_CharacteristicId_mod` and the
  four `Characteristic`-only concrete types entirely once every
  production call site and test has migrated - not kept as unused dead
  code (proposal.md's own stated precedent).
- Re-type `mapl_ExtensionResolution_mod` and every `GraphBuilder*`
  production call site that builds a characteristics map onto
  `StateItemCharacteristicMap`/`StateItemCharacteristicKind` -
  `GraphStateItem%characteristics` (already shipped) becomes reachable
  by the chain-building machinery for the first time, which is the
  actual precondition `graph-builder-stateitem-resolution` needs.
- Preserve every existing `graph/extension-reuse` and `graph/state-item
  -characteristics` observable scenario exactly.

**Non-Goals:**
- Wiring `GraphBuilder`'s `resolve_one`/`advertise_one` to actually
  read/write `GraphStateItem%characteristics` instead of building an
  ephemeral map from `VariableSpec` on every call -
  `graph-builder-stateitem-resolution`'s own concern, deferred until
  after this change lands. This change only makes the *types* line up;
  it does not change *when* or *from what source* a characteristics map
  gets built at any existing call site.
- Changing `StateItemCharacteristicDetection.F90`'s own detection
  algorithm, `SetSharedCharacteristic.F90`'s mutator, or any
  `CharacteristicStatus`/sharing/propagation behavior - all already
  correct, all already exclusively on the `StateItemCharacteristic` side,
  untouched by this merge.
- Changing `mapl_ExtensionResolution_mod`'s own chain-building
  *algorithm* (reuse-key construction, per-link execution order,
  materialization gating) - only its map/key types change, mechanically.

## Decisions

**Each of the four real `build_transform` implementations moves onto
its `StateItemCharacteristic`-side counterpart with its logic preserved
verbatim; the `Characteristic`-side original is deleted in the same
task, not kept temporarily alongside it.** Concretely:
- `UnitsCharacteristic%build_transform`'s body (`select type (goal) ...
  UnitsConverterTransform(this%units, goal%units)`) moves onto
  `PhysicalUnitsCharacteristic`, reading `this%get_units()`/
  `goal%get_units()` (both already exist as accessors) in place of the
  private-component access the original had (`PhysicalUnitsCharacteristic`'s
  `units` component is private within its own module, same visibility
  rule the original had within its own - no new exposure).
- `TypeKindCharacteristicAdapter%build_transform`'s body moves onto
  `TypeKindCharacteristic`, which already stores the same plain
  'R4'/'R8' label `TypeKindCharacteristicAdapter%inner` wrapped by
  composition - the `label_to_typekind`/`typekind_to_label` helper pair
  moves with it (there is no longer a wrapping-adapter layer to own
  them separately).
- `VerticalGridCharacteristic%build_transform`'s body (dimension-overlap
  classification, `classify_dimension_overlap`/`DIMENSION_OVERLAP_*`)
  moves onto `VerticalCoordinateCharacteristic`. `VerticalCoordinateCharacteristic`
  is a `ReferenceCharacteristic` (holds a `NodeId`, no `dimensions`
  component of its own) where `VerticalGridCharacteristic` is a
  `Characteristic` holding `dimensions`/`grid_id` directly - the
  dimension-set/grid-id storage and the three accessors
  (`get_grid_id`/`has_grid_id`/`get_dimensions`) move onto
  `VerticalCoordinateCharacteristic` as new components/methods alongside
  its existing `referenced_node_id` (inherited from
  `ReferenceCharacteristic`); `needs_extension_for`'s own existing
  node-identity-equality comparison is unaffected (that comparison
  answers a different question - "is this the same shared node" - than
  `build_transform`'s own dimension-overlap classification, which
  answers "if these differ, is there a single adaptable dimension").
- `GeomCharacteristic%build_transform`'s body (always `_FAIL`s - no real
  horizontal-regrid provider exists) moves onto `GeometryCharacteristic`
  essentially unchanged (it has no real logic to adapt, only the
  `_FAIL` call and its message).

**Alternative considered:** keep each `Characteristic`-side type as a
thin private delegate held by composition inside its
`StateItemCharacteristic`-side counterpart (mirroring
`TypeKindCharacteristicAdapter`'s own existing composition-over-`TypeKindCharacteristic`
pattern, generalized to all four). Rejected: this is exactly the
pattern this change exists to eliminate (two types per concept, one
wrapping the other to bridge an interface gap) - the interface gap
(`ComponentGraph` dependency) that originally justified it is gone
post-`decouple-transform-from-graph`, so keeping the wrapper layer
would be carrying forward complexity whose only reason no longer
applies.

**The six provider-less `StateItemCharacteristic` subclasses each gain a
`build_transform` that unconditionally `_FAIL`s with a message naming
the subclass and stating no real provider exists yet - copied from
`GeomCharacteristic`'s/`VerticalGridCharacteristic`'s own existing
`_FAIL` message shape, not a new message convention.** No `select
type`/goal-type dispatch is needed in any of the six (unlike
`needs_extension_for`, which must still distinguish a genuine kind
mismatch from a legitimate same-kind comparison) - `build_transform`
fails identically regardless of `goal`'s dynamic type, since there is
no case in which it would succeed.

**`UngriddedDimsCharacteristic`/`AttributesCharacteristic`/etc.'s own
module-header "No build_transform/adaptation method exists at this
layer (REQ-CHAR-002)" comments are updated, not merely left stale** -
REQ-CHAR-002's own "describes, does not adapt" framing was correct
*before* this change (when `StateItemCharacteristic` had no
`build_transform` deferred method at all) and remains correct in
substance for these six specifically (no real adaptation exists for
any of them) - the comment is corrected to say "a `build_transform` is
required by the common interface but fails explicitly here - no real
provider exists for this axis" rather than claiming the method does
not exist, since after this change it does, on every subclass, as a
deferred-method requirement.

**`mapl_ExtensionResolution_mod`'s `find_mismatched_characteristics`
call sites move to `find_mismatched_state_item_characteristics`
(`mapl_StateItemCharacteristicDetection_mod`, already shipped); the
module's own exported name `find_mismatched_characteristics` is kept as
a re-export (thin forwarding function) rather than renamed at every
call site in the same change.** `mapl_ExtensionResolution_mod` already
exports this name publicly and `GraphBuilder.F90`/
`GraphBuilder_Geometry_smod.F90`/`GraphBuilder_CallbackWiring_smod.F90`
all call it by that name - renaming every call site to
`find_mismatched_state_item_characteristics` directly, in the same
change that also re-types every one of those call sites'
`CharacteristicMap`/`CharacteristicId` locals to
`StateItemCharacteristicMap`/`StateItemCharacteristicKind`, is strictly
more mechanical churn for zero added clarity (the function's own job
does not change) - the existing name is simply re-pointed at the
existing, already-tested detection algorithm it is semantically
identical to.

**Alternative considered:** remove `find_mismatched_characteristics`
entirely and update every call site to call
`find_mismatched_state_item_characteristics` directly. Rejected for
this change's own scope reasons above - revisit as a follow-up
naming-cleanup if desired, not required for the merge itself to be
correct or complete.

**`find_or_build_extension_chain`/`build_chain`
(`mapl_ExtensionResolution_mod`) keep their own existing names and
algorithm structure, re-typed onto `StateItemCharacteristicMap`/
`StateItemCharacteristicKind` throughout (dummy arguments, reuse-key
construction via each characteristic's own `get_signature()`-equivalent,
per-link `export_characteristic%build_transform` call).**
`StateItemCharacteristic` has no `get_signature()` method today (only
`Characteristic` does) - `chain_key`'s own reuse-key construction
(`ExtensionResolution.F90:192-206`) calls `goal%get_signature()` per
mismatched kind. This is a genuinely new requirement on the surviving
family, not a renamed existing one: `StateItemCharacteristic` gains a
`get_signature()` deferred method (same role, same per-subclass
human-readable value-distinguishing string) so the reuse-key mechanism
has something to call. Each of the ten concrete subclasses implements
it; for the four with a `Characteristic`-side sibling, the sibling's
own existing `get_signature()` body moves over unchanged (e.g.
`'units=' // this%get_units()`); the six provider-less subclasses get a
straightforward signature derived from their own stored value (e.g.
`AttributesCharacteristic`: a sorted, joined list of its own
`attribute_names`) - not speculative, since `chain_key` needs a stable,
value-distinguishing string for every kind that can appear in a
mismatched set, including these six, for the reuse-search mechanism to
work correctly on any configuration that mismatches on one of them.

**Alternative considered:** keep `get_signature()` as a
`Characteristic`-only concept and have `chain_key` fall back to
`kind%to_string()` (kind-only, not value-distinguishing) for any
`StateItemCharacteristic` with no `get_signature()`. Rejected: this
would silently break reuse-search correctness for exactly the six
previously-`Characteristic`-side-absent kinds the moment any of them
ever appears in a real mismatch (two different attribute-set mismatches
would incorrectly reuse each other's extension) - a correctness
regression hiding behind "it still compiles," not an acceptable
trade-off for this change's own "preserve every existing scenario"
goal.

## Risks / Trade-offs

- **[Risk] `VerticalCoordinateCharacteristic` gaining `dimensions`/
  `grid_id` storage changes its own shape from a pure
  `ReferenceCharacteristic` (only a `NodeId`) to one that also carries
  inline value data - a mix this hierarchy's own
  `ValueCharacteristic`/`ReferenceCharacteristic` split was designed to
  avoid** → **Mitigation:** accepted as a deliberate, narrow exception
  specific to this one subclass: the vertical-grid axis is the only one
  where `graph/extension-reuse`'s existing dimension-overlap
  classification needs value data (the dimension set) *in addition to*
  the shared-node reference `graph/state-item-characteristics`'s own
  sharing requirement needs - both requirements are real and both
  already exist independently (one per family, pre-merge); merging the
  two types necessarily merges their data. `needs_extension_for`'s own
  existing node-identity comparison and `build_transform`'s own
  dimension-overlap comparison are different operations answering
  different questions on the same instance, same as they already are
  on two separate instances today - this is not new conceptual
  complexity, only consolidated storage for complexity that already
  existed.
- **[Risk] Six new `get_signature()` implementations for the
  provider-less subclasses are new code with no pre-existing
  `Characteristic`-side reference implementation to port verbatim,
  unlike every other method this change moves** → **Mitigation:** each
  is a small, mechanical derivation from the subclass's own already
  -stored, already-tested value (e.g. join a `StringVector`, format a
  scalar) - no new comparison or adaptation logic, only a string
  rendering of data the subclass's own constructor already validated;
  task list requires a dedicated test per subclass confirming two
  equal-value instances produce identical signatures and two
  different-value instances produce different ones (sufficient for
  reuse-key correctness, the only property `chain_key` depends on).
- **[Risk] Large single-change removal of four modules
  (`Characteristic.F90`, `CharacteristicId.F90`,
  `UnitsCharacteristic.F90`, `TypeKindCharacteristicAdapter.F90`,
  `VerticalGridCharacteristic.F90`, `GeomCharacteristic.F90`) plus their
  dedicated tests, with every call site updated in the same change,
  leaves no incremental rollback point** → **Mitigation:** accepted,
  same posture `graph-builder-stateitem-resolution`'s own tasks.md
  already states for an analogous situation ("keeping removed-but
  -unused helpers around contradicts the change's own purpose... this
  project's own precedent is clean removal, not deprecation shims") -
  task ordering (add real `build_transform`/`get_signature` to the
  surviving family first, re-type every call site second, remove the
  retired modules last, only once nothing references them) keeps the
  tree buildable at every intermediate task boundary within the change,
  even without a formal rollback point between changes.
