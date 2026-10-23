## Why

MAPL lets any component declare `standard_name`, `long_name`, and `units` on an
import/export/internal `VarSpec`, but today nothing checks that a connected
Import and Export agree on `standard_name`, and the `FieldDictionary` (which
already exists and knows the canonical `long_name`/`units` for a given
`standard_name`) is only ever consulted when a caller opts in explicitly via
`use_field_dictionary=.true.` on `make_VariableSpec`
(`superstructure/generic/specs/VariableSpec.F90:274-322`). Two components can
therefore declare the same short name with conflicting `standard_name`s (or
none at all) and MAPL will silently connect them anyway
(`FieldClassAspect_smod.F90:matches_a` never inspects `standard_name`), and a
component that forgets to declare `long_name`/`units` gets no help from the
dictionary unless it remembers the opt-in flag. GEOS-ESM/MAPL#5413 asks MAPL
to enforce the standard name convention: agreement is required where declared,
missing descriptive metadata is filled in from the dictionary, and a runtime
switch is needed to relax the new enforcement while GEOS's own
`FieldDictionary` (a separate repo, updated on its own schedule) is still
catching up to the convention. Internal MAPL tests need only a small synthetic
dictionary and can run in strict mode immediately.

A related change already lets each connection endpoint retain its own
`long_name`/`standard_name` in a per-`ESMF_NamedAlias` namespace
(`generic/field-name-propagation`, `superstructure/generic/specs/FieldClassAspect.F90`).
Requiring `standard_name` to agree across every endpoint of a connection makes
that per-alias divergence for `standard_name` moot - once agreement is
enforced, there is at most one value for `standard_name` to store, so it can
move to the same simple, unaliased, field-wide storage `UnitsAspect` already
uses instead of the per-alias namespace + one-directional inheritance
mechanism. `long_name`'s existing per-alias/inheritance behavior already
satisfies the issue's requirement 3 ("import without a `long_name` defaults to
its export's `long_name`") and is unaffected by this change.

## What Changes

- **Standard name agreement is enforced at connection time.** When an Import
  and its connected Export both declare a `standard_name`, it is an error if
  they disagree. An Import MAY declare a wildcard `standard_name` (matches
  any Export). An Export that declares no `standard_name` triggers a warning
  (not an error) rather than blocking the connection; requiring/validating
  Export-side `standard_name` against extdata-supplied fields that have no
  YAML spec is explicitly deferred (see Non-Goals in `design.md`).
- **`standard_name` storage moves off the per-alias model.** Because
  agreement is now enforced rather than merely inherited, `FieldClassAspect`
  stores `standard_name` as a single field-wide value (like `UnitsAspect`)
  instead of a per-`ESMF_NamedAlias` value with one-directional inheritance.
  `long_name` is unaffected and keeps its existing per-alias/inheritance
  behavior from `generic/field-name-propagation`.
- **Field-dictionary defaulting of `long_name`/`units` becomes automatic**
  whenever a `VarSpec` (Field or Vector) declares a `standard_name`, instead
  of requiring the caller to opt in with `use_field_dictionary=.true.`. An
  explicitly-declared `long_name`/`units` on the `VarSpec` always overrides
  the dictionary value (already true of the existing opt-in code path,
  `VariableSpec.F90:274-322`).
- **A runtime validation-mode switch controls how violations are handled**
  while GEOS's `FieldDictionary` is being extended to full coverage:
  `VALIDATION_MODE_STRICT` (errors on disagreement/unknown `standard_name`)
  vs. `VALIDATION_MODE_PERMISSIVE` (logs a warning instead of erroring). This
  wires up the already-present-but-unused `mapl_ValidationMode_mod`
  (`enums/ValidationMode.F90`) and `FieldDictionaryConfig`
  (`infrastructure/field_dictionary/FieldDictionaryConfig.F90`) into the
  actual enforcement path (today neither is consulted by any production code
  path - see `design.md` Context). Item types already identified as exempt
  from dictionary validation (`FieldDictionaryConfig%is_exempt`: bundles,
  states, wildcards, expressions, services) remain exempt.
- **BREAKING (internal only):** a component pair that today connects despite
  disagreeing `standard_name`s will fail to connect once strict mode is the
  effective default for a run (permissive mode, available as the transition
  switch, preserves today's connect-anyway behavior with a warning).

## Capabilities

### New Capabilities
- `generic/standard-name-enforcement`: connection-time agreement checking
  between an Import's and Export's declared `standard_name` (with a wildcard
  escape hatch on the Import side and a warning-only path for an
  undeclared Export `standard_name`), automatic `long_name`/`units`
  defaulting from the `FieldDictionary` for any `VarSpec` (Field or Vector)
  that declares a `standard_name`, and a runtime `ValidationMode`
  (strict/permissive) switch that governs whether a violation of the above is
  an error or a warning.

### Modified Capabilities
- `generic/field-name-propagation`: the existing per-connection-endpoint,
  one-directional-inheritance requirements for `standard_name` are replaced by
  a single, unaliased, field-wide value (consistent with the new enforcement
  in `generic/standard-name-enforcement`, which guarantees all endpoints of a
  connection already agree). The corresponding requirements for `long_name`
  are unchanged.

## Impact

- `superstructure/generic/specs/FieldClassAspect.F90`: remove per-alias
  `standard_name` storage/inheritance (`connect_to_export`'s `mirror`
  call for `standard_name`); store `standard_name` field-wide, alongside
  `UnitsAspect`-style storage.
- `infrastructure/field/FieldInfo.F90`, `FieldGet.F90`, `FieldSet.F90`: drop
  the per-alias `standard_name` get/set path added by
  `generic/field-name-propagation`; keep the per-alias path for `long_name`.
- `superstructure/generic/specs/VariableSpec.F90`: make
  `apply_field_dictionary_defaults_` unconditional on `standard_name` being
  present (no longer gated by `use_field_dictionary`); extend to Vector
  `VarSpec`s.
- `superstructure/generic/specs/FieldClassAspect_smod.F90` (and the
  `matches_a`-style methods of any other `ClassAspect` that carries a
  `standard_name`, e.g. `VectorClassAspect`): add the agreement check at
  connection-match time.
- `infrastructure/field_dictionary/FieldDictionaryConfig.F90`,
  `enums/ValidationMode.F90`: wire the existing `ValidationMode`/
  `FieldDictionaryConfig` into `OuterMetaComponent`/cap.yaml parsing so a run
  can select strict vs. permissive; today nothing constructs
  `FieldDictionaryConfig` from a real `cap.yaml`.
- Test fixtures: MAPL's own internal tests need a small synthetic
  `FieldDictionary` YAML (`superstructure/generic/tests/scenarios/...`) that
  is complete enough to run in strict mode; GEOS's separate `FieldDictionary`
  repo is out of scope for this change and is expected to still need
  permissive mode until it catches up.
