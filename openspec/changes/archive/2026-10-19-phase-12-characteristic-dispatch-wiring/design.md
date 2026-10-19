## Context

`GraphBuilder_Advertising_smod.F90`'s `populate_characteristics` runs once
per advertised item and writes directly into that item's own
`GraphStateItem%characteristics` sparse map (`set_characteristic`), via a
pointer obtained through `get_payload_ptr` — never a copy-out/copy-back
round trip (a documented gFTL2 deep-copy hazard under gfortran
`-fcheck=all` for composite nodes). All seven `StateItemCharacteristic`
subclasses already exist, tested, from 5a2
(`openspec/changes/archive/2026-10-08-state-item-characteristic-subclasses`).
`find_mismatched_characteristics`/`build_chain`
(`superstructure/generic/graph/ExtensionResolution.F90`) already iterate
the characteristics map/`mismatched_kinds(:)` generically — confirmed by
direct reading for this change, and previously confirmed by the
phase-10c-typekind-characteristic change. See proposal.md - Why for the
vertical-coordinate-already-wired correction and the
no-`VariableSpec`-field gap this design addresses for three of the six
axes.

`merged_item_for_link` (`GraphBuilder_ConnectionResolution_smod.F90`)
computes, for one link of a multi-step extension chain, which of two
axes (units, typekind) that link's materialized value should take from
the import side versus the export side — it is the one place today that
must recognize every `StateItemCharacteristicKind` that can appear in a
`mismatched_kinds(:)` array, `_FAIL`ing on any kind it does not
recognize. Adding a new detectable kind without updating this function's
`select`/`case` would turn a real mismatch on that kind into an
unconditional, unrelated-sounding assertion failure the first time one
occurs — not a defect in `merged_item_for_link`'s own logic, but a
structural consequence of how it is written (enumerate every known kind,
assert on anything else). This design's `merged_item_for_link` branches
are therefore not optional cleanup; they are required for the six new
kinds to fail with the *intended* "unsupported characteristic" message
(from `build_chain`, surfaced before `merged_item_for_link` is ever
reached for that pair - see Decision D3) rather than an unrelated
internal-consistency assertion.

## Goals / Non-Goals

**Goals:**
- Make a genuine production mismatch on six specific axes (attributes,
  ungridded-dims, quantity-type, conservation, normalization,
  standard-name) visible to existing mismatch detection, using the
  existing, already-tested comparison logic in each `*Characteristic`
  subclass.
- Give `QuantityTypeCharacteristic`/`ConservationCharacteristic`/
  `NormalizationCharacteristic` a real, settable `VariableSpec` field to
  read from, so their detection is genuine rather than permanently
  comparing two defaults.
- Preserve the existing "unsupported characteristic fails loudly, no
  silent pass-through" contract for every one of the six axes, since
  none has a real `build_transform` adaptation.

**Non-Goals:**
- No real `Transform`/adaptation logic for any of the six axes — 5a2's
  own design already established that none of their legacy
  `*Aspect%make_transform` counterparts (except units/typekind/geom) do
  real work either; this phase is detection-only, matching
  §20.4.12's own explicit framing.
- No `ComponentSpecParser`/YAML declaration surface for the three new
  `VariableSpec` fields (`quantity_type`/`conservation`/`normalization`)
  — mirrors `callback_interface_id`'s existing "plain field assignment
  only" precedent. A future change may add parser support; not needed
  for detection-wiring to be genuine (a test can set these fields
  directly, the same way existing tests exercise `callback_interface_id`
  programmatically).
- No re-verification or re-wiring of `VerticalCoordinateCharacteristic`
  (already wired; see proposal.md's correction).
- No change to `ExtensionResolution.F90` (confirmed already-generic, no
  code change needed for either detection or chain-building).

## Decisions

**D1: Gating pattern per axis — three different shapes, matched to each field's own "unset" representation, not a single uniform rule.**
`populate_characteristics` already has two gating precedents: `if
(allocated(var_spec%units))` for an allocatable field with no default
(units truly may be "not declared"), and the unconditional insert used
for `typekind` (non-allocatable, always has a default value, and that
default is itself meaningful — phase-10c's own D3). This change follows
whichever precedent matches each field's actual Fortran declaration,
rather than forcing one rule onto all six:
- `StandardNameCharacteristic` ← `var_spec%standard_name`: allocatable,
  no default → `allocated()` gate (units precedent).
- `AttributesCharacteristic` ← `var_spec%attributes`: non-allocatable
  `StringVector`, default empty → unconditional insert, gated only on
  `%size() > 0` to avoid inserting a characteristic that would never
  mismatch (an empty attribute set is `AttributesCharacteristic`'s own
  vacuously-true case per its `needs_extension_for`'s "every name in
  goal's set is present in this's set" logic — inserting it is harmless
  either way, but skipping the empty case avoids a no-op map entry on
  the overwhelmingly common case of a field with no declared shared
  attributes).
- `UngriddedDimsCharacteristic` ← `var_spec%ungridded_dims`:
  non-allocatable, default "no ungridded dims" → unconditional insert
  (typekind precedent; the default is itself a meaningful, comparable
  value, exactly as `MAPL_TYPEKIND_R4`'s default is).
- `QuantityTypeCharacteristic`/`ConservationCharacteristic`/
  `NormalizationCharacteristic` ← the three new `VariableSpec` fields
  (D2): unconditional insert (typekind precedent) — each field's default
  (`QUANTITY_UNKNOWN`, default `ConservationMetadata`, default
  `NormalizationMetadata`) is itself a valid, already-tested "matches
  anything" or structurally-compared value per that characteristic's own
  `needs_extension_for`.

Alternative considered: gate all six uniformly on "is this the
subclass's own default value" to minimize map entries. Rejected —
requires each subclass to expose an `is_default()` query that does not
exist today and would need to be added to all seven subclasses
(including the already-landed three) for consistency; not worth the
surface-area increase for what is at most one extra sparse-map entry per
item, already how `typekind`/`vertical_stagger` behave today.

**D2: Three new `VariableSpec` fields, not reuse of an existing aspect-adjacent type.**
Add `quantity_type` (`type(MAPL_QuantityType) :: quantity_type =
QUANTITY_UNKNOWN`), `conservation` (`type(ConservationMetadata) ::
conservation`), `normalization` (`type(NormalizationMetadata) ::
normalization`) as new `VariableSpec` fields, declared and defaulted the
same way `typekind` is today (non-allocatable, in-line default,
`mapl_enums_api`/`mapl_ConservationMetadata_mod`/
`mapl_NormalizationMetadata_mod` types already used elsewhere in this
same file). Set via plain field assignment only — no `make_VariableSpec`
keyword argument, no `_SET_OPTIONAL` entry — mirroring
`callback_interface_id`'s and `state_item_variant`'s own precedent
exactly (both documented in `VariableSpec.F90`'s own header comments as
"mark the item... plain field assignment only, no make_VariableSpec
keyword").

Alternative considered: thread these through `make_VariableSpec` as new
optional keyword arguments, matching `units`/`standard_name`'s own
pattern. Rejected for this change — `make_VariableSpec` already has a
long keyword list, and no caller (YAML parser, SetServices-convenience
entry point) exists yet to supply these three values from outside a
test; adding unused keyword plumbing ahead of a real caller is the same
premature-surface problem `callback_interface_id`'s own precedent was
written to avoid. Revisit if/when a YAML declaration surface for these
axes is proposed.

**D3: `merged_item_for_link` gains six enumerated no-op branches, mirroring the existing `VERTICAL_COORDINATE_CHARACTERISTIC_KIND` branch exactly — not a `class default`/fallthrough.**
Each new branch is a comment-only `else if` arm (no code, matching the
vertical-coordinate precedent's own "nothing to merge here... enumerated
explicitly rather than falling into the `_FAIL` default below"), because
none of the six has a real `build_transform`. Reaching
`merged_item_for_link` for one of these kinds in practice would require
`build_chain` to have already called that kind's `build_transform`
successfully for every entry in `mismatched_kinds(1:i)` — which never
happens, since all six `_FAIL` unconditionally. The six branches exist
so that if one of these axes ever does gain a real adaptation in a later
phase (mirroring units/typekind's own history), the enumeration failure
mode (`merged_item_for_link`'s own internal `_FAIL` on an unrecognized
kind) does not fire for a kind that is, by then, legitimately
mismatched and chain-built — the same forward-compatibility reasoning
already applied to vertical-coordinate.

Alternative considered: change the `else` branch's `_FAIL` to a silent
no-op `case default`, covering every future kind automatically.
Rejected — this is exactly the "silent pass-through" failure mode
`20.4.12`'s own scope text explicitly warns against; an unrecognized
kind reaching this function should remain a loud, specific assertion
failure naming the kind, not a silent default.

**D4: Fixture scope — one new multi-axis-mismatch fixture, not six single-axis fixtures.**
Add one new fixture under `superstructure/generic/tests/
graph-scenarios/` mismatching on at least two of the six newly-wired
axes simultaneously (e.g. standard-name and quantity-type), following
`units_and_typekind_mismatch/`'s exact flat
(`cap.yaml`+`comp_src.yaml`+`comp_dst.yaml`) convention, confirming (a)
detection fires for a non-units/typekind axis and (b) the resulting
chain-build attempt fails explicitly with the correct
unsupported-characteristic reason string, rather than silently
succeeding or hitting an unrelated assertion. Pure matched-value
coverage for all six axes (the "no false positive" direction) is added
as pFUnit cases directly against `populate_characteristics`'s real call
path (via `Test_GraphBuilder.pf` or a new, narrowly-scoped test file),
not as additional YAML fixtures — a YAML fixture is the right tool for
exercising the full init-phase/resolution pipeline end-to-end, not for
enumerating six independent assertions that two matching `VariableSpec`
values map to a non-mismatching characteristic pair.

Alternative considered: six separate single-axis-mismatch fixtures, one
per axis, mirroring `units_and_typekind_mismatch`'s own precedent for
each individual axis. Rejected as disproportionate — unlike units/
typekind (which needed their own fixture because they drive a real,
value-checkable chain-materialization outcome), these six axes'
chain-build attempt fails identically and immediately regardless of
which axis triggered it (D3); one fixture demonstrating that failure
path, plus direct unit coverage of the detection-only (non-chain) cases,
covers the same ground without six nearly-identical fixture directories.

## Risks / Trade-offs

- **[Risk] Adding six unconditional/near-unconditional characteristic
  insertions could surface a mismatch in an existing, previously-passing
  real fixture or test component that nobody noticed because detection
  for these axes didn't exist.** → Mitigation: run the full
  `MAPL.generic.*` ctest label set (per `MAPL.generic.scenarios`'s own
  ~30-fixture real-YAML regression suite) before considering this change
  complete, matching phase-10c's own regression-check discipline
  (tasks.md 7.1-7.3 there). A newly-surfaced mismatch in an existing
  fixture is itself useful signal (a previously-silent real
  inconsistency), not necessarily a defect in this change — but must be
  triaged, not merged over, if it occurs.
- **[Risk] `AttributesCharacteristic`'s asymmetric comparison
  (export-provides-superset-of-import-requires) means insertion order
  side (`this` vs. `goal`) matters at the `find_mismatched_characteristics`
  call site.** → Mitigation: `find_mismatched_characteristics`
  (`ExtensionResolution.F90:124`) is already called with
  `export_map`/`import_map` in that fixed order at both of
  `GraphBuilder_ConnectionResolution_smod.F90`'s call sites (confirmed by
  reading, not changed by this design) — `needs_extension_for` is always
  invoked as `export_characteristic%needs_extension_for(import_characteristic)`,
  matching `this`=export/`goal`=import, the direction
  `AttributesCharacteristic`'s own module header already documents as
  required. No new risk introduced; flagged here only because it is the
  one axis where getting the call order wrong would silently produce the
  opposite-of-intended result rather than an obvious failure.
- **[Trade-off] Three new always-allocated `VariableSpec` fields (D2)
  grow `VariableSpec`'s own size slightly for every item, including the
  overwhelming majority that will never set them.** → Accepted: this
  mirrors `typekind`'s own existing cost/benefit (a small, fixed-size
  value type with a cheap default), and `VariableSpec` already carries
  several such always-present fields (`ungridded_dims`, `attributes`,
  `dependencies`).

## Migration Plan

No data migration or rollback concerns — this is additive detection
logic plus three new struct fields with safe defaults, not a change to
any existing stored format, wire protocol, or restart file. A build
containing this change is not required to be run in any particular
order relative to other in-flight graph changes (proposal.md Impact:
independent of 3b2, Phase 7, Phase 13). Rollback, if needed, is a plain
revert — no forward-only state is created.
