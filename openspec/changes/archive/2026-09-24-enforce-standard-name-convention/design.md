## Context

See `proposal.md - Why` for the observed gap. Three existing pieces of
machinery this design builds on or must reconcile with:

1. **`UnitsAspect` is the proven precedent for "must agree, or wildcard/unknown
   escapes, stored field-wide."** `UnitsAspect` (`superstructure/generic/specs/UnitsAspect.F90`)
   is its own `StateItemAspect` (own `AspectId`, `UNITS_ASPECT_ID`), not a
   member field buried inside `FieldClassAspect`. Its `matches(src, dst)`
   (lines 98-117) is invoked by the generic connection-resolution machinery
   (`AspectMap`/`StateItemSpec`) *before* two endpoints are actually wired
   together, and enforces exactly the semantics #5413 wants for
   `standard_name`: equal, or either side `"<unknown>"`/unchecked, else
   reject. Its storage (`FieldInfo.F90:155-156`, `namespace_ // KEY_UNITS`) is
   a single unaliased `ESMF_Info` key - one value per physical field, visible
   identically from every `ESMF_NamedAlias` of it - and `connect_to_export`
   (`UnitsAspect.F90:137-152`) simply copies the (already-validated-equal)
   export value onto the import side; there is no separate inheritance
   mechanism because there is only one value to inherit.
2. **`standard_name` today lives inside `FieldClassAspect` (`CLASS_ASPECT_ID`)
   as inert cargo, not a matched aspect.** `FieldClassAspect%matches_a`
   (`FieldClassAspect_smod.F90:9-23`) only checks that `dst` is a
   `FieldClassAspect`/`WildcardClassAspect` - it never looks at
   `src%standard_name`. The recent `generic/field-name-propagation` change
   gave `standard_name` the same per-`ESMF_NamedAlias`, one-directional-
   inheritance treatment as `long_name` (`FieldClassAspect.F90:305-380`,
   `FieldInfo.F90:159-172`). That treatment made sense when nothing required
   the two sides to agree; once agreement is enforced (this change), per-
   alias storage for `standard_name` no longer serves a purpose - see
   Decision 1 below.
3. **`FieldDictionaryConfig`/`ValidationMode` already exist but are wired to
   nothing.** `enums/ValidationMode.F90` and
   `infrastructure/field_dictionary/FieldDictionaryConfig.F90` define exactly
   the strict/permissive switch and the required/exempt item-type split this
   issue asks for (`is_exempt`: `SERVICE`, `SERVICE_PROVIDER`,
   `SERVICE_SUBSCRIBER`, `FIELDBUNDLE`, `STATE`, `WILDCARD`, `EXPRESSION` are
   exempt; `FIELD`, `VECTOR`, `BRACKET`, `VECTORBRACKET` are required -
   `FieldDictionaryConfig.F90:83-98`), but no caller constructs a
   `FieldDictionaryConfig` from a real `cap.yaml`, and no code path consults
   `is_exempt`/`get_validation_mode` outside `Test_FieldDictionary.pf`. This
   design treats that required/exempt split as an already-settled decision
   and wires it up rather than re-deriving it. Separately,
   `MaplFramework.F90:initialize_field_dictionary` (lines 1188-1215) already
   reads a `field_dictionary` key from `cap.yaml`'s `mapl_hconfig`, but today
   it is only ever a bare path *string*, not the `mapl/field_dictionary: {path:
   ..., validation_mode: ...}` mapping `FieldDictionaryConfig::new_from_hconfig`
   expects - this design must reconcile the two entry points (see Decision 4).
4. **`FieldBundleClassAspect`, `BracketClassAspect`, and `VectorBracketClassAspect`
   already wrap/delegate to `FieldClassAspect` for `get_aspect_order`/
   `get_mandatory_aspect_ids`** (`BracketClassAspect.F90:139-146`,
   `VectorClassAspect.F90:104-113` delegate to
   `FieldClassAspect%get_mandatory_aspect_ids`/`get_aspect_order` via a
   placeholder/first-component instance). `FieldBundleClassAspect` and
   `StateClassAspect` have their own, separate aspect-id lists (both already
   include `UNITS_ASPECT_ID` today, `FieldBundleClassAspect.F90:94`,
   `StateClassAspect.F90:86`, despite `FieldDictionaryConfig` marking those
   two item types exempt from *dictionary* validation - "exempt from
   dictionary lookup" and "participates in units/typekind/etc. matching" are
   independent axes today, and this design keeps them independent).

## Goals / Non-Goals

**Goals:**
- Enforce `standard_name` agreement between a connected Import and Export
  (with a wildcard escape on the Import side) using the existing
  `AspectMap`/`matches()` connection-resolution machinery, not a bespoke
  post-hoc check.
- Make `FieldDictionary`-driven `long_name`/`units` defaulting from
  `standard_name` unconditional (no more `use_field_dictionary` opt-in) for
  the required item types (`FIELD`, `VECTOR`, `BRACKET`, `VECTORBRACKET`) -
  except a Vector's own compound-encoded `standard_name`, which is not a
  dictionary lookup key (see Decision 6 and Non-Goals).
- Enforce `standard_name` agreement per-component for Vector `VarSpec`s
  (component 1 against component 1, component 2 against component 2), since
  Vector's `standard_name` is a compound `"(name1,name2)"` encoding of two
  independent CF names, not one shared scalar value like `units` (see
  Decision 6).
- Wire the already-defined `ValidationMode`/`FieldDictionaryConfig` into an
  actual `cap.yaml` entry point and into the new matching/defaulting logic,
  without breaking today's bare-string `field_dictionary:` key.
- Simplify `standard_name` storage back to a single, unaliased, field-wide
  value (dropping the per-alias mechanism added for it by
  `generic/field-name-propagation`), since enforcement guarantees there is
  only one value to store.

**Non-Goals:**
- Enforcing/validating a `standard_name` on `FieldBundle`, `State`,
  `Wildcard`, `Expression`, or `Service` item types - these remain exempt per
  the already-established `FieldDictionaryConfig%is_exempt` list.
- The extdata-yaml-supplements-Export-standard_name idea from the issue
  ("or we require extdata yaml files to supplement data files") - explicitly
  deferred by the issue itself; this design only adds a warning (never an
  error) when an Export declares no `standard_name`.
- Changing `long_name`'s existing per-alias/inheritance model
  (`generic/field-name-propagation`) - only `standard_name`'s storage model
  changes.
- Updating GEOS's separate `FieldDictionary` repo/content - out of scope for
  a MAPL-only change; MAPL's own tests get a small synthetic dictionary
  sufficient to run in strict mode (see Migration Plan).
- Extending `FieldDictionary`-driven `long_name`/`units` defaulting to a
  Vector's compound-encoded `standard_name`. `long_name`/`units` are single
  values shared by both vector components, but the two components generally
  need *different* dictionary entries (e.g. `eastward_wind` vs.
  `northward_wind`), so there is no single well-defined entry to default a
  shared value from. This matches pre-existing behavior: compound-encoded
  names were already excluded from dictionary lookup before this change
  (`VariableSpec.F90:293-299`'s `index(standard_name, '(') == 0` guard).
- **Canonical `standard_name` transformation when a Vector is rotated
  between basis kinds** (e.g. CF's `eastward_wind`/`northward_wind` for
  `MAPL_VECTOR_BASIS_KIND_NS` vs. `x_wind`/`y_wind` for
  `MAPL_VECTOR_BASIS_KIND_GRID` - CF's "Guidelines for Construction of CF
  Standard Names", section "Component", confirms these are genuinely
  distinct standard names for the same physical component under different
  bases). Investigation found **no existing rotation-aware connection
  machinery to hook a canonical rename into**: `VectorClassAspect%matches()`
  (`VectorClassAspect.F90:115-127`) does not inspect `basis_kind` at all
  today - an NS-basis export already silently "matches" a GRID-basis import
  with a `NullTransform` (no rotation, no error), independent of this
  change. The only real NS↔GRID rotation math lives in
  `Regridder%regrid_vector` (`infrastructure/regridder_mgr/Regridder.F90:139-197`),
  reached through the separate regridder-manager path, not through the
  generic-state `matches()`/`make_transform()` machinery this change
  extends. Existing MAPL tests/scenarios already use the same
  `eastward_wind`/`northward_wind`-flavored compound name under both `NS`
  and `GRID` `vector_basis_kind` (`Test_VectorBasisKind.pf:158,180`) - i.e.
  today's codebase does not follow CF's letter here either. Fixing this
  properly requires (a) making `VectorClassAspect` basis-kind-aware in its
  own right (a pre-existing, independent gap) and (b) a canonical
  eastward/northward↔x/y name-mapping table wired into whatever transform
  performs the rotation - both substantial, separable pieces of work. This
  change's `standard_name` agreement check therefore compares each Vector
  component's *declared* value literally (Decision 6); a follow-up issue
  should track basis-aware canonicalization.

## Decisions

### Decision 1: Extract `standard_name` into its own `StandardNameAspect`, modeled directly on `UnitsAspect`

**Chosen.** Create `mapl_StandardNameAspect_mod`
(`superstructure/generic/specs/StandardNameAspect.F90`), a new
`StateItemAspect` subtype with its own `STANDARD_NAME_ASPECT_ID`, mirroring
`UnitsAspect`'s shape:
- `matches(src, dst)`: equal, or either side is a wildcard/unchecked, else
  fail. Reports the two conflicting values so the caller can decide
  error-vs-warning (see Decision 3).
- `connect_to_export`: unconditionally adopts the export's (already-matched)
  value - no per-alias inheritance needed.
- `update_from_payload`/`update_payload`: read/write a single, unaliased
  `ESMF_Info` key (new `FieldInfo.F90` helper, parallel to the existing
  `units` case at `FieldInfo.F90:155-156`/334-337, *not* the per-alias
  `standard_name` path added by `generic/field-name-propagation`, which this
  change removes).
- Add `STANDARD_NAME_ASPECT_ID` to `FieldClassAspect%get_aspect_order`/
  `get_mandatory_aspect_ids` (`FieldClassAspect.F90:129-169`, alongside
  `UNITS_ASPECT_ID`). Because `VectorClassAspect` and `BracketClassAspect`
  already delegate these two methods to a `FieldClassAspect`
  placeholder/component (`VectorClassAspect.F90:104-113`,
  `BracketClassAspect.F90:139-146`), **Vector and Bracket automatically pick
  up the new aspect with no changes to those two files** - matching the
  required-type list already defined in `FieldDictionaryConfig.F90:83-84`.
  `VectorBracketClassAspect` follows the same delegation pattern and is
  expected to need no direct change either (confirm during implementation).
  `FieldBundleClassAspect`/`StateClassAspect` are **not** given
  `STANDARD_NAME_ASPECT_ID` (matching their exemption).
- Remove `standard_name` as a member of `FieldClassAspect` entirely
  (constructor arg, `get_standard_name`/`get_long_name`'s `standard_name`
  half, the `mirror_name(this%standard_name, ...)` call in
  `connect_to_export`, and its slice of the `add_to_state` persistence call)
  - it becomes purely `StandardNameAspect`'s concern, the same way `units` is
  purely `UnitsAspect`'s concern and not also a `FieldClassAspect` member.

**Rejected - keep `standard_name` inside `FieldClassAspect`, just change its
storage key and bolt on an equality check in `connect_to_export`.** This was
the smaller-diff option, but `FieldClassAspect%matches_a`
(`FieldClassAspect_smod.F90`) runs *during* the `AspectMap`'s connection
candidate resolution, before any endpoint's `connect_to_export` executes;
`matches_a` today is a pure type check with no access to a
"reject-and-report" contract for content mismatches (unlike `UnitsAspect%matches`,
which is exactly that contract, already used by the framework to decide
"can these two endpoints even be considered connected"). Bolting an
after-the-fact check into `connect_to_export` would (a) run after a
connection has already been committed to, meaning failure has to unwind more
state than a `matches()` rejection, and (b) duplicate machinery
(`AspectMap`/candidate scoring) that already exists specifically to answer
"do these two aspects match" for `UnitsAspect`, `TypekindAspect`, `GeomAspect`,
etc. Extracting a sibling aspect is more code (~1 new small file) but reuses
the proven connection-time rejection path instead of inventing a second one.

### Decision 2: `standard_name` storage becomes unaliased/field-wide; `FieldInfo.F90`'s per-alias helper drops it

`field_info_set_internal`/`field_info_get_internal`
(`FieldInfo.F90:66-192`/`194+`) currently require `named_alias_id` whenever
`standard_name` *or* `long_name` is present, and namespace both under
`alias_namespace_`. Split this: `long_name` keeps the existing
alias-namespaced overload untouched; `standard_name` moves to a plain
`namespace_ // KEY_STANDARD_NAME` key (no `named_alias_id`), written/read by
`StandardNameAspect`'s `update_payload`/`update_from_payload` exactly the way
`UnitsAspect` writes/reads `units`. `KEY_STANDARD_NAME`
(`utils/MAPL_ESMF_InfoKeys.F90:68`) is unchanged; only which namespace it is
written under changes.

**Rejected - keep `standard_name` alias-namespaced but always mirror it to
match at connect time.** Would leave dead complexity: once agreement is
enforced, every alias's per-alias slot holds the identical value by
construction, so the per-alias namespace buys nothing over a single shared
key, while still costing the `named_alias_id` plumbing and a place for the
two copies to (bug-)drift apart.

### Decision 3: Reporting a mismatch is `matches()`'s job; severity (error vs. warning) is decided by the caller using `FieldDictionaryConfig`

`StateItemAspect%matches` returns a plain boolean today (see `UnitsAspect%matches`,
`GeomAspect%matches`, etc.) - there is no existing channel for a `matches()`
failure to carry "this should only be a warning." Rather than change the
`matches()` interface (which would ripple through every aspect and every
caller of `AspectMap`'s matching logic), `StandardNameAspect%matches` keeps
returning `.false.` on disagreement (so the framework's normal "no compatible
export found for this import" error path fires) **only when the active
`ValidationMode` is `STRICT`**. In `PERMISSIVE` mode, `matches` returns
`.true.` on a `standard_name` disagreement (connection proceeds, adopting the
export's value per the unconditional `connect_to_export` copy) but first logs
a warning identifying both values. `StandardNameAspect` consults the module-
level `FieldDictionaryConfig` singleton (Decision 4) to read the active mode;
it does not receive it as an argument, matching how `VariableSpec.F90`
already reaches `get_field_dictionary()` as a singleton rather than threading
it through call sites (`VariableSpec.F90:301,411`).

The same `ValidationMode`-gated pattern applies to the "unknown
`standard_name`" (not found in `FieldDictionary`) case in
`VariableSpec%apply_field_dictionary_defaults_`: `STRICT` -> `_FAIL`,
`PERMISSIVE` -> the existing `logger%warning` path
(`VariableSpec.F90:310-319`) stays as today's behavior, just no longer gated
behind `use_field_dictionary`.

**Rejected - add a `severity`/`allow_mismatch` output argument to `matches()`.**
Touches every `StateItemAspect` subtype's interface for a need only one
aspect has; reading the mode from the existing singleton is a strictly
smaller change.

### Decision 4: Promote `FieldDictionaryConfig` to a module-level singleton, loaded alongside the dictionary itself; extend (don't replace) the existing `field_dictionary:` cap.yaml key

`MaplFramework.F90:initialize_field_dictionary` (1188-1215) currently accepts
`field_dictionary: <path-string>`. Change it to accept **either**:
- a bare string (today's form - unchanged meaning: that path, permissive
  mode, for backward compatibility with existing `cap.yaml` files), or
- a mapping (`field_dictionary: {path: ..., validation_mode: ...}`), parsed
  via the already-written `FieldDictionaryConfig(node)` constructor
  (`FieldDictionaryConfig.F90:56-79`).

Detect which form is present via `ESMF_HConfigIsDefined`/`ESMF_HConfigIsMap`-
style introspection before calling `ESMF_HConfigAsString`. Store the result
in a new package-level singleton in `FieldDictionaryConfig.F90` (`the_field_dictionary_config`,
`get_field_dictionary_config()`), set once during
`initialize_field_dictionary`, mirroring the existing
`the_field_dictionary`/`get_field_dictionary()` singleton in
`FieldDictionary.F90:63,346-349`. `StandardNameAspect` and
`VariableSpec%apply_field_dictionary_defaults_` both read the mode/exemption
through this singleton.

**Rejected - two independent `cap.yaml` keys (`field_dictionary` for the
path, a new key for the mode).** `FieldDictionaryConfig`'s constructor and
tests (`Test_FieldDictionary.pf:444-516`) already assume one `mapl/field_dictionary`
mapping carries both `path` and `validation_mode`; splitting them would mean
rewriting already-tested, already-merged code instead of reusing it.

### Decision 5: `long_name`/`units` defaulting from the dictionary becomes unconditional; `use_field_dictionary` flag is retired

`VariableSpec%apply_field_dictionary_defaults_`
(`VariableSpec.F90:274-322`) and the regrid-method defaulting
(`VariableSpec.F90:370-420`) are today gated by an explicit
`use_field_dictionary=.true.` argument to `make_VariableSpec`
(`VariableSpec.F90:142,215,259-261`). Per the proposal, this becomes
unconditional whenever a required-item-type `VarSpec` declares a
`standard_name`: call `apply_field_dictionary_defaults_` unconditionally
(subject to `FieldDictionaryConfig%is_exempt(item_type)`), and remove the
`use_field_dictionary` argument (and its call sites) once no longer
referenced. `Test_FieldDictIntegration.pf`'s `test_no_fd_lookup_when_flag_absent`
scenario is retired/rewritten accordingly (see `tasks.md`).

### Decision 6: VectorClassAspect gets its own embedded pair of StandardNameAspect instances; Bracket/VectorBracket need no such change

Investigation while implementing this design surfaced a structural fact
Decision 1 did not originally account for: **`standard_name` is not
uniform across `ClassAspect` subtypes the way `units`/`typekind` are.**

- **`BracketClassAspect`/`VectorBracketClassAspect`** are both, structurally,
  "N repeated copies of one `FieldClassAspect` template" (a time-bracket of
  the *same* physical quantity, or, for `VectorBracketClassAspect`, one
  bracketed *component* of what the caller composes into a vector via two
  separate `VECTORBRACKET` `VarSpec` declarations -
  `VariableSpec.F90`'s `VECTORBRACKET` case passes `this%standard_name`
  *unsplit* to `VectorBracketClassAspect`, unlike the `VECTOR` case below).
  A single shared `standard_name` is exactly right for these, and Decision
  1's original claim holds unmodified: both already delegate
  `get_aspect_order`/`get_mandatory_aspect_ids` to a `FieldClassAspect`
  placeholder, so they pick up `STANDARD_NAME_ASPECT_ID` automatically with
  **no changes to `BracketClassAspect.F90`/`VectorBracketClassAspect.F90`**
  beyond fixing their embedded `FieldClassAspect(standard_name, long_name,
  fill_value)` constructor calls for FieldClassAspect's new (shorter)
  signature (Decision 1).
- **`VectorClassAspect`** (the plain 2-component `MAPL_STATEITEM_VECTOR`
  case) is different: its `standard_name` is the compound
  `"(name1,name2)"` encoding, split by `VariableSpec.F90:324-345`
  (`split_name`) into two genuinely different CF names, one per physical
  component (e.g. `eastward_wind` vs. `northward_wind`), which are then
  passed to the *two separate* `FieldClassAspect` values that make up
  `VectorClassAspect%component_specs(2)`. A single outer
  `StandardNameAspect` built from the raw compound string would either
  compare `"(a,b)"` as one opaque token (losing the ability to give a
  useful diagnostic naming *which* component disagrees) or require
  `StandardNameAspect` itself to become compound-aware (leaking a
  Vector-specific concern into a type that also serves plain `Field`
  and `FieldDictionary` lookups).

  **Chosen:** add `type(StandardNameAspect) :: standard_name_aspects(2)` to
  `VectorClassAspect` (mirroring the existing `component_specs(2)` array),
  built in `VariableSpec.F90`'s `VECTOR` case from the same `std_name_1`/
  `std_name_2` already produced by `split_name`. `VectorClassAspect%matches`
  is extended to additionally check
  `src%standard_name_aspects(i)%matches(dst%standard_name_aspects(i))` for
  `i = 1, 2` (beyond its existing dynamic-type check), and
  `VectorClassAspect%connect_to_export` similarly forwards to both
  components' `connect_to_export`. `VectorClassAspect%get_aspect_order`/
  `get_mandatory_aspect_ids` (which otherwise delegate wholesale to
  `component_specs(1)`, per Decision 1's original text) are overridden to
  filter `STANDARD_NAME_ASPECT_ID` out of the delegated list, since
  agreement is now checked directly inside `VectorClassAspect%matches`
  rather than through a separate outer aspect-map slot. `VariableSpec`
  still builds and inserts an outer, unconsulted `STANDARD_NAME_ASPECT_ID`
  entry for a Vector `VarSpec` (harmless dead cargo, same pattern already
  tolerated for e.g. `WildcardClassAspect`'s unconsulted `UNITS_ASPECT_ID`
  entry) rather than special-casing `VariableSpec%make_aspects` by item
  type.

  **Rejected - make `StandardNameAspect` itself compound-aware (detect
  `"("`, split, compare pairwise internally).** Would let a single outer
  aspect instance serve both Field and Vector, but spreads Vector-specific
  string-encoding knowledge into a type whose other consumer
  (`FieldDictionary` lookup key resolution) already explicitly excludes
  compound values (`VariableSpec.F90:293-299`) - i.e. `StandardNameAspect`
  would need to know about compound encoding for matching purposes while
  simultaneously refusing to use it for dictionary purposes. Keeping the
  compound-splitting logic entirely inside `VectorClassAspect` (which
  already owns `split_name`'s call site and the resulting two
  `FieldClassAspect` components) is more consistent with where that
  knowledge already lives.

## Risks / Trade-offs

- **[Risk]** Making dictionary defaulting unconditional means any existing
  component that already declares a `standard_name` MAPL's dictionary
  doesn't yet recognize will newly trigger a diagnostic (warning in
  permissive mode, error in strict mode) where today it triggered nothing.
  -> **Mitigation**: permissive mode is the default (`FieldDictionaryConfig::new_default`,
  already `VALIDATION_MODE_PERMISSIVE`), so existing configurations degrade
  to warnings, not failures, until an operator opts into `STRICT`.
- **[Risk]** Removing `standard_name` from `FieldClassAspect` and its
  per-alias `FieldInfo` path could break any code that reads
  `this%standard_name` directly on a `FieldClassAspect`, or that calls
  `MAPL_FieldGet(field, standard_name=...)` expecting the
  `generic/field-name-propagation` per-alias semantics.
  -> **Mitigation**: grep all internal readers of `FieldClassAspect%standard_name`
  and all `MAPL_FieldGet(..., standard_name=`/`MAPL_FieldSet(..., standard_name=`
  call sites before merging (same audit style as the field-name-propagation
  change's task 6.1); `MAPL_FieldGet`'s public signature is unchanged; only
  its *semantics* for `standard_name` change (single shared value instead of
  per-alias), which is the intended, documented outcome of this change.
- **[Risk]** `Test_FieldDictIntegration.pf` and the `names_1` scenario
  (`superstructure/generic/tests/scenarios/names_1`) were written against
  the per-alias/opt-in behavior this change replaces for `standard_name`.
  -> **Mitigation**: update both as part of this change (`tasks.md`); a
  `standard_name`-disagreement case is exactly the kind of new scenario this
  issue is asking to exist and did not exist before (per the investigation:
  "no existing test exercises mismatched standard_names").
- **[Trade-off]** Internal MAPL tests need their own small, synthetic
  `FieldDictionary` YAML complete enough to run in `STRICT` mode (per the
  issue: "MAPL internal tests need to maintain a synthetic field
  dictionary"), separate from and smaller than GEOS's real
  `geos_field_dictionary.yaml`. This is accepted scope, not deferred - it is
  the only way to exercise `STRICT` mode in MAPL's own CI before GEOS's
  dictionary is complete.

## Migration Plan

No data migration; this is in-memory/config-only behavior. Suggested
implementation order (full detail in `tasks.md`):
1. Add `StandardNameAspect` (new file, modeled on `UnitsAspect`) and its
   `FieldInfo.F90` unaliased get/set helpers.
2. Wire `STANDARD_NAME_ASPECT_ID` into `FieldClassAspect`'s aspect-order/
   mandatory lists; remove `standard_name` from `FieldClassAspect` itself
   and from the per-alias `FieldInfo.F90` path.
3. Promote `FieldDictionaryConfig` to a singleton; extend
   `MaplFramework.F90:initialize_field_dictionary` to accept the mapping
   form of `field_dictionary:` alongside the existing bare-string form.
4. Make `VariableSpec`'s dictionary defaulting unconditional (subject to
   `is_exempt`); retire `use_field_dictionary`.
5. Update/add test coverage: `Test_FieldDictIntegration.pf`,
   `names_1` scenario, a new mismatch scenario, and a small synthetic
   `FieldDictionary` YAML sufficient for `STRICT`-mode CI.
6. Regression-audit `MAPL_FieldGet`/`MAPL_FieldSet(..., standard_name=`
   call sites (Risk above) and run `MAPL.generic.scenarios`/`MAPL.generic.core`.

Rollback is a plain revert; no persisted state or schema is affected. A
run can also roll back its own exposure by setting
`validation_mode: permissive` (or omitting the mapping form entirely) in its
`cap.yaml` without any code change.

## Open Questions

- Exact wording/format of the connection-mismatch error/warning message
  (e.g., whether to include both component names, not just field names) -
  deferred to implementation; does not affect the spec, approach, or task
  breakdown.
- Whether `VectorBracketClassAspect` truly needs zero direct changes (per
  Decision 1's delegation argument) or has some divergence from
  `BracketClassAspect`'s delegation pattern - to be confirmed by reading the
  file during implementation; if it diverges, the fix is the same one-line
  addition already planned for `FieldClassAspect`, not a design change.
