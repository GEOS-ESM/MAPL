## Why

`superstructure/generic` couples an Export and its connected Import by making the
Import's `ESMF_Field` an `ESMF_NamedAlias` of the Export's field. Aliases share a
single underlying `ESMF_Info` host, so today `standard_name`/`long_name` are stored
unaliased (one shared slot per physical field). Only the Export side's `allocate()`
step ever writes that slot, so any `standard_name`/`long_name` independently declared
on the Import side (or on a re-export several hops away) is silently discarded, and
querying the field's metadata from *any* connection point returns the Export's value
regardless of which state you fetched the field from. This was caught by a new
`Test_Scenarios` check (`superstructure/generic/tests/scenarios/names_1`) added to
validate this metadata, which fails against current behavior.

This matters because `standard_name`/`long_name` are pure descriptive metadata (unlike
units, typekind, or grid) with no compatibility enforcement forcing the two sides to
agree - each side is allowed to declare its own value in yaml today, but the
declaration is silently dropped for every side except the original Export.

## What Changes

- Each connection endpoint (Export, Import, and any intermediate coupler/transform
  hop) will persist its own `standard_name`/`long_name` in a per-alias namespace of
  the shared `ESMF_Info`, keyed by the `ESMF_NamedAlias` id already assigned to that
  placement - the same pattern already used in this codebase for `restart_mode`.
- When a connection endpoint does not declare its own `standard_name`/`long_name`,
  it inherits the value from its connection predecessor (the Export/source it
  connects to, or the prior hop in a transform chain), instead of falling back to a
  hardcoded `'unknown'`. An endpoint's own explicitly-declared value always takes
  precedence over the inherited one. Propagation is one-directional (downstream
  only): an Import's declared name never overwrites its Export's.
- `MAPL_FieldGet(field, standard_name=, long_name=)` will resolve the value for the
  specific alias represented by the `field` handle passed in, rather than a single
  field-wide value. No signature change to `MAPL_FieldGet`/`MAPL_FieldSet` is
  required; callers already hold the correct alias handle because it always came
  from a specific `ESMF_State`.
- The `FieldClassAspect` default for an unassigned `standard_name`/`long_name`
  changes from the literal string `'unknown'` to "unassigned" (no forced value),
  so presence can be tested and propagation decided unambiguously. `'unknown'`
  remains only as the last-resort fallback when nothing in the chain ever assigned
  a value.
- Scope for this change is the scalar `ESMF_Field` case (`FieldClassAspect`) only.
  `FieldBundleClassAspect`, `StateClassAspect`, `VectorClassAspect`,
  `BracketClassAspect`, and `VectorBracketClassAspect` are not touched here; they
  already only forward `standard_name` (not `long_name`) and are call out as
  follow-up work.

## Capabilities

### New Capabilities
- `generic/field-name-propagation`: per-connection-endpoint `standard_name`/
  `long_name` metadata on `ESMF_Field` state items, including inheritance from a
  connection's predecessor when an endpoint leaves the metadata unassigned.

### Modified Capabilities
(none - no existing capability spec describes field metadata behavior today)

## Impact

- `superstructure/generic/specs/FieldClassAspect.F90`: constructor default,
  `allocate()`, `add_to_state()`, `connect_to_export()`.
- `infrastructure/field/FieldInfo.F90`: new per-alias set/get helpers for
  `standard_name`/`long_name` (generalizing the existing `restart_mode` pattern).
- `infrastructure/field/FieldGet.F90` (and `mapl_FieldGet` callers indirectly,
  e.g. `infrastructure/geom_io/SharedIO.F90`): read path resolves per-alias.
- `superstructure/generic/tests/Test_Scenarios.pf` and
  `superstructure/generic/tests/scenarios/names_1/`: existing check procedures and
  scenario already added; scenario will gain additional cases for the inheritance
  and transform-chain paths.
- No public API signature changes; `MAPL_FieldGet`/`MAPL_FieldSet` call sites are
  unaffected.
