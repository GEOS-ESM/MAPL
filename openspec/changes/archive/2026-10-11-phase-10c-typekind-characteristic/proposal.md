## Why

`docs/graph/spec/20-implementation-roadmap.md` §20.4.8's 10c entry (as
amended by the archived `phase-10c-typekind-unification` change) commits
to a specific direction but stops short of implementing it: give the
already-shipped `TypeKindCharacteristic`
(`superstructure/generic/graph/TypeKindCharacteristic.F90`,
`ValueCharacteristic`-based) a real adaptation capability, and wire
`GraphBuilder.F90`'s real connection resolution to consult it, so a
genuine precision-only mismatch (e.g. `R4` export feeding an `R8`
import) is no longer invisible to `resolve_one`'s mismatch detection.
Today `build_characteristics()` never inserts a typekind entry at all,
so `find_mismatched_characteristics` cannot see a typekind mismatch, and
the one Transform meant for this axis (`CopyTransform`,
`superstructure/generic/transforms/CopyTransform.F90`) is legacy-only
(`ExtensionTransform`-based, never built from the graph side).

This is also the first real exercise of the pilot the amendment framed:
can a `StateItemCharacteristic`-rooted type be made usable at
`ExtensionResolution.F90`'s `Characteristic`-typed insertion point
without duplicating its comparison logic in a second type. Confirmed
during research for this proposal: `ExtensionResolution.F90`'s
`build_chain` already walks an arbitrary-length `mismatched_kinds(:)`
array generically (one `Characteristic%build_transform` call per
entry) - no chain-building code change is required for a second,
independent mismatch axis to compose with the existing `units` one.

A second, previously undocumented gap surfaced while tracing the real
materialization call sites for this proposal:
`GraphBuilder.F90`'s `try_materialize_field_extension` passes
`typekind=export_var_spec%typekind` (the export's own typekind) to
`materialize_field_extension` for both the exact-match and the
mismatch-chain branches - unlike `units=import_var_spec%units`, which
correctly uses the import's own required value. For the exact-match
branch this is harmless (both sides already agree). For the
mismatch-chain branch it is a latent, never-yet-exercised bug: the
chain's final extension item is materialized with the *export's*
typekind, not the typekind the import actually declared, so even once
a real cast transform exists, the item sitting at the end of the chain
would be given the wrong real payload type/kind. No fixture mismatches
on typekind today, so this has never been exercised. This proposal is
the first point that forces fixing it.

## What Changes

- `superstructure/generic/graph/TypeKindCharacteristic.F90` gains a
  real `build_transform`-equivalent, making it directly insertable
  where `ExtensionResolution.F90`/`CharacteristicMap` expect a
  `class(Characteristic)` - no second,
  `Characteristic`-extending `TypeKindCharacteristic` is created (the
  collision the archived amendment exists to prevent). The specific
  bridge shape (optional method on `ValueCharacteristic` vs. a
  `Characteristic`-side wrapper by composition) is a design.md decision
  for this change, not pre-decided by the amendment.
- A new `CharacteristicId` value is added to
  `superstructure/generic/graph/CharacteristicId.F90` if the chosen
  bridge needs one (depends on the design.md decision above).
- A new, real precision-cast `Transform` subclass is added under
  `superstructure/generic/graph/`, mirroring
  `UnitsConverterTransform.F90`'s shape exactly (owns `graph` + two
  `NodeId`s, two named ports, `compute(this, rc)` dispatches on
  typekind via `mapl_FieldPointerUtilities_mod::assign_fptr`) -
  performing a real R4<->R8 cast. Named to avoid colliding with both
  `superstructure/generic/transforms/CopyTransform.F90` (legacy,
  `ExtensionTransform`-based) and the `18-state-item-characteristics.md`
  placeholder name `CopyTransform` itself (already flagged poor there).
- `GraphBuilder.F90`'s `build_characteristics()` gains a typekind entry,
  inserted on a basis this change's design.md must settle explicitly
  (unlike `units`, `VariableSpec%typekind` is a non-allocatable field
  with a default value, so the existing `if (allocated(...))` gating
  pattern cannot be reused verbatim).
- `GraphBuilder.F90`'s `try_materialize_field_extension` is fixed to
  pass the import's own typekind (not the export's) to
  `materialize_field_extension`, matching the existing `units=
  import_var_spec%units` precedent - closing the latent bug described
  above.
- A new, flat (`cap.yaml`+`comp_src.yaml`+`comp_dst.yaml`) fixture is
  added under `superstructure/generic/tests/graph-scenarios/`,
  mismatched on both `units` and `typekind` simultaneously, with
  hand-checkable values - isolating "chain exists, correct values once"
  from `invalidate/`'s existing `numsteps:2` runtime-re-invalidation
  fixture, which this change deliberately does not reuse or modify.
- `Test_GraphEquivalenceHarness.pf`'s `get_equivalence_fixtures()` gains
  a fourth parameter entry for the new fixture; its shared `check()`
  subroutine (or a narrowly-scoped addition to it) is extended to also
  assert the chain's intermediate materialized value(s), not just the
  final exported one - new ground beyond what the current
  `graph/equivalence-harness` spec requires.

## Capabilities

### New Capabilities
None.

### Modified Capabilities
- `graph/equivalence-harness`: the harness's payload-parity check is
  extended to cover a mismatched (extension-chain) fixture's
  intermediate/final materialized value(s), not only an exact-match
  connection's static attributes - new ground beyond the current spec's
  exact-match-only scope.

Note: `graph/extension-reuse`'s existing requirements already describe
"a registered characteristic succeeds even when others are not" and
"an unregistered characteristic fails loudly" generically, independent
of which characteristic kinds happen to have a provider - adding a real
typekind provider satisfies those existing requirements for a new kind
without changing their text, so no delta is filed for that capability.

## Impact

- **Affected code:**
  `superstructure/generic/graph/TypeKindCharacteristic.F90`,
  `superstructure/generic/graph/CharacteristicId.F90` (possibly),
  a new Transform module under `superstructure/generic/graph/`,
  `superstructure/generic/GraphBuilder.F90` (`build_characteristics()`,
  `try_materialize_field_extension`), a new fixture directory under
  `superstructure/generic/tests/graph-scenarios/`,
  `superstructure/generic/tests/Test_GraphEquivalenceHarness.pf`.
- **Not affected:** `ExtensionResolution.F90`'s `build_chain`/
  `find_mismatched_characteristics` (already generic over N
  characteristics - confirmed by direct reading, no change needed);
  `UnitsCharacteristic`/`VerticalGridCharacteristic`/`GeomCharacteristic`
  (this change does not migrate or touch them); the `invalidate/`
  legacy-scenario fixture (left as-is, not reused).
- **Downstream:** this is the first real precedent for the
  `Characteristic`/`StateItemCharacteristic` bridge pattern the archived
  amendment names as a pilot - a later phase may apply the same
  approach to `Units`/`PhysicalUnits` or `Geom`/`Geometry`/
  `VerticalGrid`/`VerticalCoordinate`, but that is explicitly out of
  scope here. Phase 10d (ordinary-phase `MethodGraphNode` construction)
  depends on this change's fixture per roadmap §20.4.8 ("Extends 10c's
  own fixture... to additionally exercise the chain re-propagating
  correctly").
