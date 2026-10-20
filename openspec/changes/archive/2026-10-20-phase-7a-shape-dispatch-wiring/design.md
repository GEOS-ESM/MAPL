## Context

`populate_characteristics` (`GraphBuilder_Advertising_smod.F90`) already
sets every advertised item's `GraphStateItem%declared_item_type` directly
from `var_spec%itemType` (line `call payload%set_declared_item_type(var_spec
%itemType)`) — this is how `try_materialize_field_extension`/
`materialize_extension_chain`'s existing `unsupported_item_class` gate
reads "is this a plain field" today. `find_mismatched_characteristics`/
`build_chain` (`ExtensionResolution.F90`) know nothing about
`declared_item_type` at all; they only ever compare entries in a
`StateItemCharacteristicMap`, uniformly, through `StateItemCharacteristic`'s
own deferred `needs_extension_for`/`build_transform` — confirmed by direct
reading (no `declared_item_type` reference anywhere in that module). This
is exactly the roadmap's own point (proposal.md - Why / §20.4.5): shape is
visible to the *gate*, but invisible to *detection/chain-building* —
making shape a `StateItemCharacteristic` closes that gap through the
existing, uniform mechanism rather than a parallel one.

`BracketClassAspect%connect_to_export` (`superstructure/generic/specs/
BracketClassAspect.F90`) unconditionally `_FAIL`s "cannot be an import" —
legacy itself never treats a bracket as a valid import shape. This
change's own scope (bracket export resolving a mismatch against a field
import) follows that same asymmetry; no code path here ever needs to
support a bracket-shaped import.

`try_materialize_field_extension`/`materialize_extension_chain`
(`GraphBuilder_ConnectionResolution_smod.F90`) only ever build a plain
`ESMF_Field` via `materialize_field_extension`/`FieldCreate`
(`ExtensionMaterialization.F90`) — there is no FieldBundle-shaped
materialization path anywhere in graph-native code today. This is the
concrete reason `VectorBracket`->`Vector` is out of scope (proposal.md -
Why): its own final node is a multi-field `FieldBundle`
(`MAPL_FIELDBUNDLETYPE_VECTOR`), not a `Field`, and `GraphStateItem` has
no field to carry `vector_basis_kind` through that materialization even
if the FieldBundle-building code existed.

`resolve_one` (`GraphBuilder_ConnectionResolution_smod.F90`) has two
"give this node a real payload" call sites for the mismatch branch: one
(lines ~422-438) unconditionally materializes the chain's own INPUT node
(the export's own node) using `export_item` for both the export and
import arguments of `try_materialize_field_extension`, and one (via
`materialize_extension_chain`, lines ~448-466) materializes every link
up to and including the chain's own FINAL node. Both currently gate on
`export_item%get_declared_item_type() == MAPL_STATEITEM_FIELD` and fail
otherwise. For a genuine `Bracket`->`Field` mismatch, the first call site
is asking "give the bracket its own real bracket payload" — something
`materialize_field_extension`/`FieldCreate` cannot do (Field-only) and
that this change does not add — while the second is asking "give the
chain's final node a real FIELD payload using the IMPORT's own values",
which `materialize_field_extension` already does correctly today and
needs no new capability, only a loosened gate.

## Goals / Non-Goals

**Goals:**
- Make a `Bracket`-shaped export's mismatch against a `Field`-shaped
  import visible to `find_mismatched_characteristics`, through a new
  `StateItemCharacteristic` subclass dispatched by the existing
  `build_chain` loop — no new chain-building machinery.
- Give that mismatch a real `build_transform` (unlike Phase 12's six
  detection-only axes): a genuine time-interpolation `Transform`
  collapsing the bracket's held time levels into the import's single
  field — reusing `TimeInterpolateTransform.F90`'s own established
  `interpolation_weights`/field-count convention on the bracket's ESMF
  `Info`, not reinventing it.
- Let the resulting chain's FINAL node (the field-shaped side) be
  materialized through the *existing*, unmodified
  `materialize_field_extension`/`FieldCreate` path, once the gate
  checking the chain's export side is loosened to admit `BRACKET`.

**Non-Goals:**
- No `VectorBracket`->`Vector` support of any kind (detection,
  `build_transform`, or materialization) — see proposal.md - Why. The new
  characteristic's `needs_extension_for` still reports a mismatch for
  that pairing (so a genuine mismatch is visible, not silently ignored),
  but its `build_transform` fails explicitly for it, exactly like every
  other axis's own "no real provider" case.
- No new mechanism to materialize a bracket export's own real,
  from-scratch bracket `FieldBundle` payload. A bracket export's real
  payload is expected to already exist before `GraphBuilder` resolution
  runs — via legacy `GRAPH_MODE_REGISTRY`-backed advertising (the
  production case, consistent with the roadmap's own observation that no
  real fixture exercises this yet) or, for this change's own test
  coverage, via direct test setup (`node%set_payload`-style construction,
  the same idiom `Test_ExtensionResolution.pf`'s own synthetic nodes
  already use) — never through `try_materialize_field_extension`'s own
  `FieldCreate` path, which remains Field-only.
- No change to `find_mismatched_characteristics`/`build_chain`
  (`ExtensionResolution.F90`) — confirmed already-generic over any
  `StateItemCharacteristic` subclass; this change adds a subclass, not a
  code path.
- No change to legacy `ClassAspect`/`BracketClassAspect` or the
  `GRAPH_MODE_REGISTRY` resolution path — unaffected, per proposal.md -
  Impact.

## Decisions

**D1 (amended during implementation — see tasks.md 4.2's own completion note): the insert into `populate_characteristics` is gated to the four "ordinary", shape-comparable item types (`FIELD`/`BRACKET`/`VECTOR`/`VECTORBRACKET`), not unconditional across every `ESMF_StateItem_Flag` value.**
Running the full `MAPL.generic` test suite surfaced a real regression: a
truly unconditional insert made `ItemShapeCharacteristic` participate in
`find_mismatched_characteristics` for item types that are resolved
through an entirely separate mechanism outside `build_chain` altogether
— concretely, `MAPL_STATEITEM_EXPRESSION` (`ExpressionClassAspect%
make_transform` -> `EvalTransform`, legacy `StateRegistry`-based). An
`EXPRESSION`-typed export connected to a `FIELD` import is a real,
previously-working, production connection pattern; `ItemShapeCharacteristic
%needs_extension_for` (D2) correctly flags `EXPRESSION /= FIELD` as a
mismatch, and `build_transform` (D3) correctly `_FAIL`s it (no provider
for that pairing exists or should exist), but the net effect — turning a
working connection into `unsupported_characteristic` — is a regression,
not a newly-surfaced pre-existing bug. `MAPL_STATEITEM_WILDCARD`/
`MAPL_STATEITEM_SERVICE`/`MAPL_STATEITEM_STATE`/`MAPL_STATEITEM_
FIELDBUNDLE` are the same class of risk (each already has its own
non-graph-native resolution path — see `is_ordinary_resolvable_import`'s
identical enumeration, `GraphBuilder_ConnectionResolution_smod.F90` task
6.8) even though no current fixture happens to exercise one of them
reaching this exact code path today.

Fix: `populate_characteristics` inserts `ItemShapeCharacteristic` only
when `var_spec%itemType` is one of the four ordinary types this axis is
actually scoped to (proposal.md's own `Bracket`->`Field` focus plus the
`Vector`/`VectorBracket` pairing D2/Non-Goals explicitly wants reported
as a mismatch). `declared_item_type` itself (`set_declared_item_type`)
is unaffected — still set unconditionally, exactly as before this
change; only the new characteristic's insert is gated. This does not
reopen the "declared_item_type vs. characteristics map" independence
concern the original D1 alternative-considered paragraph raises:
`populate_characteristics` already reads `var_spec%itemType` directly
(the same source `set_declared_item_type` reads), so gating the insert
on that same value is a decision made once, at advertise time, by the
one procedure that already owns both — not a new dependency from
`ExtensionResolution.F90` back onto `declared_item_type`.

**D1 (original): A new `ItemShapeCharacteristic` value characteristic, holding its own copy of the declared item class — not a reinterpretation of the existing `declared_item_type` field.**
`ItemShapeCharacteristic` (`graph/state-item-characteristics`,
`ITEM_SHAPE_CHARACTERISTIC_KIND`) stores a plain `type(ESMF_StateItem_
Flag)` value (`BRACKET`/`FIELD`/`VECTOR`/`VECTORBRACKET`/...), populated
in `populate_characteristics` from the same `var_spec%itemType` value
`set_declared_item_type` already captures. This is a deliberate, narrow
duplication (both `GraphStateItem%declared_item_type` and this
characteristic's own private field end up holding the same value) rather
than teaching `find_mismatched_characteristics`/`build_chain` a
`declared_item_type`-specific comparison path outside the characteristics
map. The duplication is harmless within this change (both are set once,
unconditionally, from the same source, in the same procedure) and is the
direct cost of the roadmap's own explicit design intent: "walked by the
*same* `build_chain` loop, uniformly with Units" (§20.4.5) — a bespoke,
non-uniform mismatch check for this one axis would be exactly the kind of
asymmetric special-casing that sentence rules out.

Alternative considered: have `find_mismatched_characteristics`
additionally compare `declared_item_type` directly, with no new
characteristic at all. Rejected — `declared_item_type` is a
`GraphStateItem` field, not a `StateItemCharacteristicMap` entry;
teaching `ExtensionResolution.F90` (deliberately independent of
`GraphStateItem`, per its own module header) to reach into it would
violate that independence and still leave `build_chain`'s per-mismatch
`build_transform` dispatch with nothing to call for this axis, since
`build_transform` is a `StateItemCharacteristic` method.

**D2: `needs_extension_for` compares the two sides' item classes for inequality — any mismatch is reported, including ones this change's own `build_transform` cannot handle.**
Matches `find_mismatched_characteristics`' own existing "mismatch
detection and conversion-support are separate concerns" split (a kind can
be detected as mismatched and then separately fail to build a
`Transform` — e.g. `AttributesCharacteristic` always mismatches-or-not
correctly but never builds a real `Transform`). `VectorBracket` vs.
`Vector` is therefore a real, reported mismatch; it simply has no
`Transform` (D3), so `build_chain` reports `unsupported_characteristic`
for it exactly as it already does for the six Phase-12 axes.

**D3: `build_transform` succeeds only for `BRACKET` (this) -> `FIELD` (goal) — a `select case`/`if` pairing check, not a generic "different shapes, so interpolate" rule.**
Every other pairing — `VECTORBRACKET`->`VECTOR`, `FIELD`->`VECTOR`, any
pairing touching `STATE`, or `BRACKET`->anything-other-than-`FIELD` —
`_FAIL`s explicitly with a message naming the unsupported pairing,
mirroring `PhysicalUnitsCharacteristic%units_build_transform`'s own
`class default` -> `_FAIL` precedent for an unrecognized goal type. This
is what keeps `VectorBracket`->`Vector` correctly unsupported (D2/Non-
Goals) without a separate gate anywhere else.

**D4: `BracketTimeInterpolationTransform` is a new, independent graph-native `Transform` — not a wrapper around legacy's `TimeInterpolateTransform`.**
Mirrors `UnitsConverterTransform`'s own precedent exactly (module header:
"Deliberately independent of legacy's `ExtensionTransform` interface...
Reuses... read as a faithful behavioral reference... never called into").
`TimeInterpolateTransform` (`superstructure/generic/transforms/
TimeInterpolateTransform.F90`) is legacy's own `ExtensionTransform`
subclass (`initialize`/`update(importState, exportState, clock, rc)`,
ESMF-`State`-shaped arguments) — architecturally incompatible with
`Transform`'s own `compute(inputs, outputs: PortNameGraphStateItemMap,
rc)` contract, and its own r4/r8 interpolation subroutines are private to
that module. The new `Transform` re-implements the same small
weights-times-member arithmetic directly (`TimeInterpolateTransform.F90`
`run_r4`/`run_r8`, read as the faithful behavioral reference, never
called into), via the same `mapl_field_bundle_api`
`MAPL_FieldBundleGet(bundle, fieldList=..., interpolation_weights=...)`
call `TimeInterpolateTransform.F90` already uses — not legacy's
`StateRegistry`/`ExtensionFamily` machinery. Declares its ports under the
same hardcoded names every chain-built `Transform` must use
(`INPUT_PORT_NAME = 'source_field'`, `OUTPUT_PORT_NAME =
'destination_field'`, `ExtensionResolution.F90`'s own `bind_port` calls
in `build_chain`) even though its input port semantically holds a field
*bundle* — reads it via `inputs%at(INPUT_PORT_NAME)%get_field_bundle()`
instead of `%get_field()`; the port name is a chain-wiring convention,
not a type claim.

Alternative considered: extract `TimeInterpolateTransform.F90`'s own
`run_r4`/`run_r8`/`run_r4_vector`/`run_r8_vector` into a shared, legacy-
independent utility module both the legacy and new `Transform` call into.
Rejected for this change — a bigger refactor of working legacy code for a
"lowest urgency" (roadmap's own words), zero-production-fixture gap; the
small amount of duplicated arithmetic (the same trade-off
`UnitsConverterTransform` already accepted relative to legacy's
`ConvertUnitsTransform`) is proportionate. Revisit only if/when
`VectorBracket`->`Vector` (explicitly deferred) needs the vector variant
too and the duplication would otherwise triple.

**D5: Loosen the EXPORT-side `unsupported_item_class` gate in `try_materialize_field_extension`/`materialize_extension_chain` to admit `BRACKET` alongside `FIELD` — the IMPORT-side check is unchanged.**
Both procedures' own existing `if (.not. (export_item%get_declared_item_type()
== MAPL_STATEITEM_FIELD))` check gains an `.or. ... == MAPL_STATEITEM_BRACKET`
arm; their own `import_item` check is untouched (an import is still
required to be a plain field — consistent with `BracketClassAspect`
itself never supporting import, Context). This is the one gate both
procedures' mismatch branch depends on; no other code in either
procedure references `declared_item_type` or otherwise assumes `FIELD`.
`merged_item_for_link` (`materialize_extension_chain`'s own per-link
typekind/units resolver) gains one new enumerated no-op branch for
`ITEM_SHAPE_CHARACTERISTIC_KIND`, mirroring the existing vertical-
coordinate/Phase-12 branches exactly: this axis has nothing for that
helper to merge (the materialized node's *shape* is always a `Field`,
decided by `materialize_field_extension` itself always calling
`FieldCreate`, not by anything `merged_item_for_link` computes) — the
branch exists only so a genuine `BRACKET`->`FIELD` mismatch fails with
`build_chain`'s own correct `unsupported_characteristic` message in any
future case where it does not reach a real `Transform`, rather than this
helper's own unrelated "unrecognized `StateItemCharacteristicKind`"
assertion.

**D6: `resolve_one`'s own separate "materialize the chain's own INPUT node" call site is skipped, not failed, when the export's declared item type is not `FIELD`.**
That one call site (`GraphBuilder_ConnectionResolution_smod.F90`
mismatch branch, ~line 422) exists to give a plain-field export a real
payload when nothing else already has — Context's "D6" problem: today,
reaching this call site with a `BRACKET` export would hit the EXACT SAME
gate D5 loosens, except here `export_item` is passed as BOTH the export
*and* import argument, so loosening the export-side check alone would
make this call reach the IMPORT-side check next — which D5 deliberately
leaves `FIELD`-only, so it would still fail, now with the same
`unsupported_item_class` outcome, abandoning the whole connection
(`_RETURN(_SUCCESS)` before the chain's own real wiring/materialization
ever runs). Rather than loosening the import-side check here too (which
would claim this call CAN materialize a real bracket payload — it
cannot; `materialize_field_extension` only ever builds `Field`s, Non-
Goals), this call site gains an explicit guard: skip entirely (no call,
no failure, continue to the chain's own real materialization below) when
`export_item%get_declared_item_type()` is not `MAPL_STATEITEM_FIELD`.
This is the one new piece of control flow this change adds beyond the
gate loosening in D5 — without it, the proposal's own end-to-end
scenario (a bracket export resolving a mismatch into a field import)
cannot succeed at all, regardless of what D1-D5 add.

Alternative considered: give this call site `export_item` as the export
argument and a synthetic FIELD-typed stand-in as the import argument, so
it "succeeds" trivially. Rejected — fabricates a value with no real
meaning (there is no real field-shaped version of the bracket's own
INPUT node; the bracket IS the input), and risks silently overwriting the
export's own already-real (or deliberately still-unallocated) payload
with an unrelated placeholder field — exactly the "fabricated payload"
failure mode `graph/extension-reuse`'s own spec already rules out for
every other unsupported case.

**D7: Test coverage demonstrates chain STRUCTURE and the Transform's own correctness directly; it does not add a new way to materialize a bracket's own real payload end-to-end through `GraphBuilder`.**
The new `Test_Scenarios` fixture (proposal.md - What Changes) exercises
`find_mismatched_characteristics`/`build_chain`/the loosened gate (D5)
producing correct graph structure (chain nodes, edges,
`unsupported_item_class` no longer firing) for a bracket export -> field
import mismatch, with real payload materialization exercised either (a)
off (structure-only, matching the majority of existing scenario
fixtures' own convention), or (b) on, with the export's own bracket
`FieldBundle` payload pre-seeded directly onto its `StateItemNode` by the
test/fixture setup itself (not through any `GraphBuilder` call) so the
final node's own `materialize_field_extension` call (already unmodified,
D5/Context) has a real input to read from if the fixture also exercises
`graph%update()`/`Transform%compute()`. `BracketTimeInterpolationTransform`'s
own correctness (real time-interpolation arithmetic, D4) is covered by a
direct unit test constructing a bracket `FieldBundle` and a `Field` and
calling `compute()` against them, independent of `GraphBuilder` entirely
— the same "exercise the `Transform` directly" idiom
`Test_ExtensionResolution.pf`'s own `FakeCharacteristic` tests already
use for chain-building, and no different in kind from how a real
`UnitsConverterTransform` test would be written.

## Risks / Trade-offs

- **[Risk] D1's duplication (the same declared-item-class value held in
  both `GraphStateItem%declared_item_type` and the new characteristic)
  could drift if a future change updates one without the other.** →
  Mitigation: both are set from the identical `var_spec%itemType`
  expression, in the same procedure (`populate_characteristics`),
  immediately adjacent to each other — a future reader changing one is
  very likely to see the other in the same diff context. Not eliminated,
  but the same class of risk Phase 12's own `VERTICAL_COORDINATE_
  CHARACTERISTIC_KIND` branch already accepted for a different axis.
- **[Risk] D6's skip-not-fail guard is easy to read as "this item is
  fully handled" when it is really "this item's own real payload is
  silently assumed to already exist."** → Mitigation: documented
  explicitly in both the code comment at that call site and this
  design's own D6/Non-Goals — a future `VectorBracket`->`Vector` or
  general bracket-materialization follow-on change will need to revisit
  this exact spot, not rediscover it.
- **[Trade-off] No regression risk to any existing Field-only
  connection** — D5's gate change is strictly additive (`.or.`), D6's
  guard only activates for a non-`FIELD` export (today: BRACKET), and
  D3/D4 only ever run for a genuine `BRACKET`->`FIELD` mismatch pairing a
  `find_mismatched_characteristics` call would not have reported before
  this change existed (the new characteristic is the only thing that can
  produce it). Every existing Units/TypeKind/VerticalCoordinate/Phase-12
  mismatch path is untouched.

## Migration Plan

No data migration or rollback concerns — purely additive detection/
conversion logic plus two narrowly-scoped gate changes, not a change to
any existing stored format, wire protocol, or restart file. A build
containing this change is not required to run in any particular order
relative to other in-flight graph changes. Rollback, if needed, is a
plain revert — no forward-only state is created.
