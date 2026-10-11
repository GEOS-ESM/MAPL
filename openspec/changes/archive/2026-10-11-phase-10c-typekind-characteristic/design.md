## Context

Two parallel "characteristic" hierarchies exist (see proposal.md and the
archived `phase-10c-typekind-unification` amendment for full history):

- `Characteristic` (`superstructure/generic/graph/Characteristic.F90`) -
  live, consulted directly by `GraphBuilder.F90`'s `build_characteristics()`
  / `ExtensionResolution.F90`'s `find_mismatched_characteristics`/
  `build_chain`. Three real subclasses today: `UnitsCharacteristic`
  (`build_transform` builds `UnitsConverterTransform` for real),
  `VerticalGridCharacteristic`, `GeomCharacteristic` (both `_FAIL`
  explicitly - no provider yet).
- `StateItemCharacteristic` (via `ValueCharacteristic`/
  `ReferenceCharacteristic`) - fully implemented, unit-tested, but never
  consulted by `GraphBuilder.F90`/`ExtensionResolution.F90` (confirmed by
  direct grep: zero references outside its own module/tests). Includes
  the already-shipped `TypeKindCharacteristic` (a `ValueCharacteristic`
  holding a plain `'R4'`/`'R8'` string label, correct
  `needs_extension_for`, no adaptation method anywhere in this
  hierarchy by design - REQ-CHAR-002, "describes, does not adapt").

The amendment settled that 10c builds on the existing
`TypeKindCharacteristic`, not a second `Characteristic`-extending type
of the same name (a literal module/type-name collision), and left the
bridge *shape* open between: (1) an optional adaptation method added
directly to `StateItemCharacteristic`/`ValueCharacteristic`, or (2) a
`Characteristic`-side wrapper holding a `TypeKindCharacteristic` by
composition.

`ExtensionResolution.F90`'s `build_chain` (confirmed by direct reading)
already walks an arbitrary-length `mismatched_kinds(:)` array
generically - each entry resolved by one call to that entry's own
`Characteristic%build_transform`. Composing a second mismatch axis with
the existing `units` axis therefore requires zero change to
`build_chain`/`find_mismatched_characteristics`/`find_or_build_extension_chain`,
provided the typekind entry is insertable into `CharacteristicMap` as a
`class(Characteristic)`.

A second, independent finding from this change's own research:
`GraphBuilder.F90`'s `try_materialize_field_extension`
(`GraphBuilder.F90:1190`) passes `typekind=export_var_spec%typekind`
(the export's own typekind) to `materialize_field_extension` on *both*
call sites (exact-match branch, line ~1245/1249, and mismatch-chain
branch, same subroutine) - unlike `units=import_var_spec%units`, which
correctly uses the import's required value on both. For the exact-match
branch this is harmless (no mismatch means both sides already agree).
For the mismatch-chain branch, once this change gives typekind a real
provider, it is a real bug: the chain's final extension item would be
materialized with the export's typekind, not the import's required
one - defeating the cast the chain was built to perform. No existing
fixture has ever exercised this path (typekind was never a detectable
mismatch before this change), so it has been latent and silent until
now.

## Goals / Non-Goals

**Goals:**
- Make `TypeKindCharacteristic` usable at `ExtensionResolution.F90`'s
  `Characteristic`-typed insertion point, with no second,
  `Characteristic`-extending type under the same or a dodged name.
- Give `GraphBuilder.F90`'s real resolution path a real, executing
  typekind provider: a genuine R4<->R8 cast `Transform`.
- Fix the export-vs-import typekind argument bug in
  `try_materialize_field_extension` described above, so the chain this
  change enables materializes correctly from day one.
- Land one new, flat, hand-checkable fixture mismatched on both `units`
  and `typekind` simultaneously, and extend the existing equivalence
  harness to assert the chain's intermediate value, not just the final
  one.

**Non-Goals:**
- Migrating `UnitsCharacteristic`, `VerticalGridCharacteristic`, or
  `GeomCharacteristic` onto the `StateItemCharacteristic` hierarchy -
  this change is a single-axis pilot only (amendment's own framing).
- Retiring or restructuring `CharacteristicMap`/`CharacteristicId`/
  `Characteristic` itself - `GraphBuilder.F90` continues to key its
  mismatch map by `CharacteristicId`.
- Any change to `ExtensionResolution.F90`'s `build_chain`/
  `find_mismatched_characteristics`/`find_or_build_extension_chain` -
  confirmed unnecessary; these already generalize over N characteristics.
- Reusing or modifying the `invalidate/` scenario fixture - this change
  adds an independent, new fixture instead (amendment's own explicit
  instruction).
- Runtime re-invalidation / multi-timestep re-propagation through the
  new chain - that is Phase 10d's job, which extends this change's
  fixture later, per roadmap §20.4.8.
- `materialize_extensions_enabled()`'s own gating semantics - unchanged;
  the new provider is simply available once that gate (already
  independently toggled by existing tests/fixtures) is on.

## Decisions

**D1. Bridge shape: a `Characteristic`-side wrapper by composition
(the archived amendment's "Decision 2"), not an optional method added to
`ValueCharacteristic`.**

A new type, `TypeKindCharacteristicAdapter`
(`superstructure/generic/graph/TypeKindCharacteristicAdapter.F90`,
module `mapl_TypeKindCharacteristicAdapter_mod`), extends `Characteristic`
and holds a `TypeKindCharacteristic` (the existing, unmodified
`StateItemCharacteristic`-side type) by composition:

```fortran
type, extends(Characteristic) :: TypeKindCharacteristicAdapter
   private
   type(TypeKindCharacteristic) :: inner
contains
   procedure, nopass :: get_id => adapter_get_id
   procedure :: get_signature => adapter_get_signature
   procedure :: needs_extension_for => adapter_needs_extension_for
   procedure :: build_transform => adapter_build_transform
end type TypeKindCharacteristicAdapter
```

- `get_id()` returns a new `TYPE_KIND_CHARACTERISTIC_ID` (added to
  `CharacteristicId.F90`, next free value after `GEOM_CHARACTERISTIC_ID`;
  `to_string()`'s `select case` gains a `"TYPE_KIND"` branch).
- `get_signature()` delegates to `'typekind=' // this%inner%get_type_kind()`,
  mirroring `UnitsCharacteristic%get_signature`'s `'units=' // this%units`
  shape exactly (used by `ExtensionResolution.F90`'s reuse-key `chain_key`).
- `needs_extension_for(goal)` unwraps `goal` (`select type`, expecting
  `TypeKindCharacteristicAdapter`) and delegates to
  `this%inner%needs_extension_for(goal%inner)` - reusing the existing,
  already-correct string-inequality comparison with zero duplication.
  `class default` -> `needs_extension = .true.` (matches
  `UnitsCharacteristic`'s own defensive-but-not-`_FAIL`'d default, not
  `TypeKindCharacteristic`'s own `error stop`, since `Characteristic`'s
  interface has no `rc` to fail through and the calling convention here
  is `Characteristic`'s, not `StateItemCharacteristic`'s).
- `build_transform(graph, input_node_id, output_node_id, goal, transformer, rc)`
  is the one real new logic: unwraps `goal`, constructs
  `TypeKindConverterTransform` (D2 below) from `this%inner%get_type_kind()`
  and `goal%inner%get_type_kind()`, mirroring
  `UnitsCharacteristic%build_transform`'s shape exactly (`select type` /
  `class default` -> `_FAIL`).

**Rejected alternative (amendment's "Decision 1" - optional method on
`ValueCharacteristic`):** would require adding a deferred-but-overridable
adaptation method to `StateItemCharacteristic`/`ValueCharacteristic`
itself, touching a hierarchy whose own spec (REQ-CHAR-002) currently
states flatly "describes, does not adapt" for *every* subclass, not just
`TypeKindCharacteristic` - every other `ValueCharacteristic` subclass
(`PhysicalUnitsCharacteristic`, `AttributesCharacteristic`, five more from
5a2) would inherit a default `_FAIL`-ing method it does not need, and the
`graph/state-item-characteristics` spec itself would need a new,
cross-cutting requirement to describe it. The wrapper (D1 above) keeps
that hierarchy's existing "description only" contract completely
untouched - no spec change needed for `graph/state-item-characteristics`
at all (see Capabilities update below) - and isolates the one new
capability (adaptation) in a type that exists solely for this bridge,
exactly as the amendment's own text anticipated ("a wrapper/adapter at
the bridge point is the shape of the fix, not a second concrete
characteristic type under either name").

**D2. Real cast transform: `TypeKindConverterTransform`
(`superstructure/generic/graph/TypeKindConverterTransform.F90`, module
`mapl_TypeKindConverterTransform_mod`), mirroring
`UnitsConverterTransform.F90`'s shape exactly.**

Checked for collisions first (per `UnitsConverterTransform`'s own
precedent comment): neither `TypeKindConverterTransform` nor
`mapl_TypeKindConverterTransform_mod` exists anywhere in
`superstructure/generic/transforms/` or `superstructure/generic/graph/`
today (confirmed by repo-wide grep) - no rename-to-dodge needed, unlike
`UnitsConverterTransform`'s own naming history. Deliberately not named
`CopyTransform`: that name is both already taken by the legacy
`ExtensionTransform`-based type
(`superstructure/generic/transforms/CopyTransform.F90`) and was itself
flagged as a poor placeholder in `18-state-item-characteristics.md`.

Shape, following `UnitsConverterTransform` line for line:
- `extends(Transform)`; holds `class(ComponentGraph), pointer :: graph`,
  `input_node_id`/`output_node_id` (`NodeId`), plus `src_typekind`/
  `dst_typekind` (`ESMF_TypeKind_Flag`, not the plain string label -
  `TypeKindCharacteristic%get_type_kind()` returns `'R4'`/`'R8'`, so the
  constructor converts that label to an `ESMF_TypeKind_Flag` once, at
  construction time, via a small local helper - see Risk below).
- Two named ports (`INPUT_PORT_NAME = 'source_field'`,
  `OUTPUT_PORT_NAME = 'destination_field'` - same literal names
  `UnitsConverterTransform` already uses; both transforms share the same
  two-port shape, so reusing the names is consistent, not accidental).
- `compute(this, rc)`: fetches both bound `StateItemNode`s from
  `this%graph`, gets their `ESMF_Field`s, dispatches on the pairing via
  `assign_fptr` (`mapl_FieldPointerUtilities_mod`) - four concrete cases
  (`R4->R8`, `R8->R4`; `R4->R4`/`R8->R8` cannot occur here since
  `needs_extension_for` already requires the labels to differ, but the
  `compute` body still only needs the two real cross-kind assignments,
  e.g. `x_out(:) = real(x_in(:), kind=ESMF_KIND_R8)` and the reverse) -
  no udunits2 dependency (this is a pure numeric kind cast, not a unit
  conversion).

**D3. `build_characteristics()` gains a typekind entry inserted
unconditionally, not gated by `allocated(...)`.**

`VariableSpec%typekind` (`superstructure/generic/specs/VariableSpec.F90`)
is declared `type(ESMF_TypeKind_Flag) :: typekind = ESMF_TYPEKIND_R4`
(non-allocatable, always has a value) - unlike `units`
(`character(:), allocatable`), there is no "not declared" state to gate
on. `build_characteristics()` therefore inserts a
`TypeKindCharacteristicAdapter` for every `VariableSpec` unconditionally:

```fortran
call characteristics%insert(TYPE_KIND_CHARACTERISTIC_ID, &
     TypeKindCharacteristicAdapter(var_spec%typekind))
```

(constructor converts the `ESMF_TypeKind_Flag` to the `'R4'`/`'R8'`
label `TypeKindCharacteristic`'s own constructor expects).

**Rationale / risk check:** every existing fixture/test today has both
sides of every exact-match connection defaulting to (or explicitly
agreeing on) the same typekind - confirmed by inspecting every
`graph-scenarios/`/`scenarios/` fixture's own YAML for an explicit
`typekind:` override that differs between a connection's two sides
(none found outside `invalidate/`, which this change does not touch).
`find_mismatched_characteristics` only reports a kind as mismatched when
both sides declare it *and* `needs_extension_for` returns true for a
real value difference - so inserting the entry unconditionally adds a
real (near-zero-cost) comparison to every existing exact-match
connection's resolution, but changes the resolved *result* (mismatch
count, chain presence) for none of them. Tasks.md includes an explicit
regression-check task (re-run `Test_GraphBuilder.pf`/
`Test_GraphBuilderEquivalence.pf`/`Test_Scenarios.pf` unmodified) to
confirm this holds in practice, not just by inspection.

**D4. Fix `try_materialize_field_extension`'s typekind argument.**

Both `materialize_field_extension` call sites in
`try_materialize_field_extension` change from
`typekind=export_var_spec%typekind` to `typekind=import_var_spec%typekind`,
matching the existing `units=import_var_spec%units` precedent exactly
(the materialized item's real payload must match what the *import*
requires, not what the export happened to produce). For the exact-match
branch this is a pure no-op change (both sides already agree by
definition of "no mismatch"). For the mismatch-chain branch, this is the
fix described in Context - without it, this change's own new chain would
materialize the wrong typekind at the end despite the cast transform
existing and running correctly for structural purposes.

**D5. New fixture: `graph-scenarios/units_and_typekind_mismatch/`**
(name chosen to be self-describing, avoiding `invalidate/`'s own
confusing `A`/`B`/`T` convention), following `plain_field/`'s exact
3-file (`cap.yaml`+`comp_src.yaml`+`comp_dst.yaml`), flat,
`all_unsatisfied`-connection convention:
- `comp_src.yaml`: export `T`, `units: 'hPa'`, `typekind: R4`.
- `comp_dst.yaml`: import `T`, `units: 'Pa'`, `typekind: R8`.
- A single, static (no `run:`/timestep) value on the export side,
  hand-computable through both conversion steps (e.g. export value `10.`
  hPa -> `1000.` Pa after the units step -> `1000.d0` R8 after the
  typekind step - exact value TBD at implementation time, chosen so the
  arithmetic is exact in both precisions, avoiding any rounding
  ambiguity in the hand-check).
- No `numsteps`/re-invalidation semantics - static, single-resolution
  check only (Phase 10d's own job to extend this fixture for the
  runtime-re-propagation case, per roadmap §20.4.8).

**D6. `Test_GraphEquivalenceHarness.pf` extension.**

Add a fourth `EquivalenceFixtureDescription` entry for the new fixture.

**Correction, found during implementation:** the existing `check()`
subroutine's payload-parity block does *not* already run correctly
unmodified for this fixture, contrary to this design's original
wording above. The block resolves the export's own graph-native proxy
node and compares its materialized payload against `legacy_field`
(comp_dst's own real import field, *after* legacy's imperative coupler
has run its own conversions). For the three existing exact-match
fixtures the export's payload equals the import's by definition of "no
mismatch" (and, confirmed by testing - see below - the export really is
materialized for those three). For this fixture, two things are both
true and both block the existing comparison, not just the one
originally identified:

1. `legacy_field` genuinely differs from the export's own value -
   legacy's coupler converts `hPa`/`R4` -> `Pa`/`R8` before writing
   `dst_import_state`, while the graph-native export node correctly
   stays at `hPa`/`R4` (only the chain's *final* item should be
   converted).
2. **(Found only by running the suite, not by reading the code alone.)**
   `resolve_one`'s mismatch branch (`GraphBuilder.F90`) never
   materializes the export's own node at all for a mismatched
   connection - only the exact-match branch does that unconditionally;
   the mismatch branch only materializes the chain's *final* item
   (and only when `materialize_extensions_enabled()` is true, see
   below). So `export_payload%itemType()` fails outright for this
   fixture - there is no payload to compare, not merely the wrong one.

Fix: the entire export-payload-parity block (proxy resolve through the
Task 4.4 rank check) is gated on `this%fixture_name /=
'units_and_typekind_mismatch'`, not just the typekind/units assertions
as originally corrected - matching this change's own
`graph/equivalence-harness` spec delta, which scopes "Static attributes
agree before any run phase" to an *exact-match* connection in the first
place. Preserves exact current behavior for the three existing
fixtures; skips the whole block for the new one, whose real correctness
check is the chain-final-item assertion below instead.

The chain's *final* value this change's own proposal.md refers to is
the final (post-chain) extension item's materialized payload, reachable
the same way `UnitsConverterTransform`'s own existing single-
characteristic chains already are reachable in principle, but never
asserted anywhere yet - add one narrowly-scoped new `@assert_that`-style
check in `check()`, gated on
`this%fixture_name == 'units_and_typekind_mismatch'` (the only fixture
with a real chain to inspect), comparing its materialized field's
typekind/units against the import's own declared requirements (`R8`/
`'Pa'`) - demonstrating the chain's *final* result is correct, not
merely that it was built.

**Lookup mechanism, confirmed during implementation:** no new accessor
is needed. `proxy_key(comp_name, state_intent, short_name)`, already
imported by this test file for the export side, resolves the
*import's* own local `NodeId` just as well (it is a general
proxy/local-item lookup, not export-specific -
`proxy_key('comp_dst', ESMF_STATEINTENT_IMPORT, name)`). The chain's
final item is then the import node's sole predecessor in the graph's
default `DependencyNetwork` (`cap_graph%get_network(cap_graph%
get_default_network_id())%get_predecessors(import_node_id)`) - exactly
the same `get_predecessors`-on-the-default-network pattern
`Test_GraphBuilder.pf` already uses to walk a resolved connection's own
chain (confirmed by reading that test file directly).

**Second correction, found during test execution (not just reading):**
even with the first correction above, the harness fails before reaching
either the gated legacy-vs-export comparison or the new chain-final-item
check - at `export_payload%itemType()` itself, because the export
node has no real payload at all for this fixture. Tracing
`GraphBuilder.F90`'s `resolve_one`: the exact-match branch
materializes the *export's own* node unconditionally once
`graph_mode() /= GRAPH_MODE_REGISTRY` (this is why the three existing
fixtures' export payload-parity check works at all) - but the mismatch
branch (this fixture's own path) only materializes the *chain's final
item*, and only when `mapl_ExtensionResolution_mod`'s
`materialize_extensions_enabled()` is true. That flag is a separate,
default-`.false.` global switch, today turned on only by
`Test_GraphBuilderEquivalence.pf` (`set_materialize_extensions(.true.)`
around its own init-phase loop, reset to `.false.` after) - this
harness never calls it. Without it, neither the export nor the chain
has a real field for a mismatched connection under `both` mode, and the
existing (now correctly gated) payload-parity block's own
`export_payload%itemType()` call fails outright for this fixture, not
merely compares the wrong value.

Fix: `equivalence_fixture_setup` calls
`set_materialize_extensions(.true.)` immediately before the init-phase
loop (alongside the existing `set_graph_mode(GRAPH_MODE_BOTH)` call) and
resets it to `.false.` immediately after (alongside the existing
`set_graph_mode(GRAPH_MODE_REGISTRY)` reset) - mirroring
`Test_GraphBuilderEquivalence.pf`'s own existing precedent exactly, for
every fixture (not just the new one): harmless for the three existing
exact-match fixtures, since `materialize_extensions_enabled()` is only
ever consulted on the mismatch branch, which none of those three take.

## Risks / Trade-offs

- **[Risk] `ESMF_TypeKind_Flag` <-> `'R4'`/`'R8'` string-label
  round-tripping** (needed at the `TypeKindCharacteristicAdapter`
  constructor boundary and inside `TypeKindConverterTransform`'s own
  constructor) **has no existing shared helper** -
  `TypeKindCharacteristic`'s own constructor takes the string label
  directly; `VariableSpec%typekind` and `ESMF_FieldGet`'s own
  `typekind=` argument are real `ESMF_TypeKind_Flag` values. →
  **Mitigation:** a small, local (not shared/exported beyond this
  change's own two new modules) conversion helper, following
  `UnitsConverterTransform.F90`'s own existing `ESMF_TYPEKIND_R4`/
  `ESMF_TYPEKIND_R8` comparison precedent (`units_compute`'s own
  `if (typekind == ESMF_TYPEKIND_R4) ... else if (... R8) ... else _FAIL`
  pattern) - not a new general-purpose typekind-to-string utility,
  which is out of this change's scope.
- **[Risk] The new unconditional typekind-characteristic insertion
  (D3) could, in principle, surface an undiscovered real mismatch in an
  existing fixture that was previously silently ignored** (e.g. a
  fixture whose two sides' YAML actually do declare different
  `typekind:` values today, never checked before this change existed) →
  **Mitigation:** D3's own regression-check task
  (re-run the existing scenario/equivalence suites unmodified) surfaces
  this immediately as a new test failure, not a silent behavior change;
  if one is found, fixing that fixture's own YAML (or confirming the
  mismatch is intentional and belongs in this change's own new fixture
  instead) is an explicit task, not a surprise at review time.
- **[Risk] Harness extension (D6) depends on a lookup mechanism for the
  resolved import's own bound node that may not exist as a public
  accessor today** → **Mitigation:** flagged explicitly in D6 above as
  an implementation-time check, not assumed solved; worst case, a small,
  narrowly-scoped new accessor (mirroring `proxy_key`'s own existing
  shape) is added to `GraphBuilder.F90`'s public interface, test-only
  motivated but not test-only gated (consistent with `proxy_key` itself,
  already used by this exact test file today).
- **[Risk] This is the first real precedent for the
  `Characteristic`/`StateItemCharacteristic` bridge pattern** - a design
  choice here that turns out awkward in practice could bias a later
  phase's own decision for `Units`/`Geom`/`VerticalGrid` unification →
  **Mitigation:** explicitly out of scope to decide now (Non-Goals); this
  change's own review is the right point to record whether the wrapper
  approach felt clean, informing (not binding) that later decision.

## Open Questions

None - the one real open point left by the archived amendment (which
bridge shape to pick) is resolved by D1 above; the remaining
implementation-level unknowns (exact lookup accessor for D6, exact
hand-checkable numeric values for D5) are deferred to tasks.md/
implementation as ordinary task-level detail, not design-level
decisions that would change the specs or approach.
