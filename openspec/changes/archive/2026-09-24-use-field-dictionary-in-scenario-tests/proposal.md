## Why

`mapl_FieldDictionary_mod`'s singleton dictionary is empty for every pFUnit
test binary in MAPL today. `MaplFramework%initialize` (invoked once per
process via the pFUnit `extra_initialize_` hook) always runs with an empty
`mapl_hconfig` - no cap.yaml is ever read - so `initialize_field_dictionary`
always falls to its "no `field_dictionary:` key" branch, looks for
`geos_field_dictionary.yaml` in the working directory, never finds it, and
leaves the dictionary empty for the whole process. As a result, none of
MAPL's own scenario tests (or any other pFUnit test) has ever exercised
`VariableSpec`'s dictionary-driven `long_name`/`units` defaulting, or the
`standard_name`-vs-dictionary check added by `generic/standard-name-enforcement`
(GEOS-ESM/MAPL#5413) - the mechanism is built and unit-tested in isolation,
but never proven against MAPL's own realistic scenario fixtures the way
GEOS's own configs will exercise it.

There is already a checked-in, correctly-schemad fixture for this -
`superstructure/generic/tests/field_dictionary_test.yaml` - including a
section explicitly commented "entries required by non-field-dictionary unit
tests... so the mandatory dictionary lookup in make_VariableSpec succeeds
without modifying those tests." Nothing has ever actually loaded it: it is
copied into the test build directory but no `load_field_dictionary` call
anywhere in the tree references it by name.

Separately, the scenario fixtures' own `standard_name`/`units`/`long_name`
values are inconsistent in ways a shared dictionary would surface: the same
literal `standard_name` string is declared with different `units` in
different scenarios that share a test process (e.g. `invalidate`'s
`Temperature` is `km` in one child and `m` in the other; `B2 standard name`/
`I_B1 standard name` is `barn` in `3d_specs`/`precision_extension*` but `m`
in `ungridded_dims`/`names_1`), and some scenarios (`statistics`,
`statistics_real`) use human-sentence text ("Surface Temperature") as a
`standard_name` where CF requires a `snake_case` identifier
(`surface_temperature`). None of this matters while the dictionary is empty;
it will matter the moment it is not, and it would need cleaning up before
any future move toward `STRICT` `ValidationMode` for these tests could be
considered.

## What Changes

- Activate the existing `superstructure/generic/tests/field_dictionary_test.yaml`
  fixture by wiring it into the `superstructure/generic/tests/` pFUnit
  binaries' build/working directory as the default-path
  `geos_field_dictionary.yaml` `initialize_field_dictionary` already looks
  for - no MAPL library code changes, only a build-configuration addition.
  `ValidationMode` remains `PERMISSIVE` (the default) for the existing
  `MAPL.generic.scenarios` binary. (Revised during implementation: the nine
  scenarios converged by the batches below were verified clean under
  `STRICT` and split into a new, dedicated `MAPL.generic.scenarios.strict`
  executable - see `design.md` Decision 6 - rather than this change ending
  with zero tests actually exercising `STRICT` mode.)
- Extend that fixture with entries for an initial, explicitly-scoped batch
  of scenario fixtures (not all of them - see Non-Goals in `design.md`):
  - The `I_A1`/`E_A1`/`Z_A1`/`I_B1`/`E_B1`/`Z_B1`-family `standard_name`s
    reused verbatim across `scenario_1`, `scenario_2`,
    `scenario_reexport_twice`, `propagate_geom`, and `memory_checkpoint` -
    highest leverage for the least churn, since one consistent set of
    dictionary entries covers five scenario directories at once.
  - `statistics`/`statistics_real`, converged from human-sentence
    `standard_name`s ("Surface Temperature") to proper CF `snake_case` form
    (`surface_temperature`, etc.), which is also a straightforward
    correctness fix independent of the dictionary.
  - `vertical_regridding_2`/`vertical_regridding_3`, which already use
    largely CF-shaped names (`air_pressure`, `height`) and need only
    matching dictionary entries added.
- Where a migrated scenario did not previously declare `long_name`, remove
  it from the YAML (rather than duplicating the dictionary's value) so the
  scenario genuinely exercises dictionary-driven defaulting, and update that
  scenario's `expectations.yaml` to assert the dictionary-supplied value.
- Resolve the two concrete cross-scenario `standard_name`/`units`
  inconsistencies identified above (`invalidate`; the `3d_specs`/
  `precision_extension*` vs. `ungridded_dims`/`names_1` `B2`/`I_B1`
  collision) by renaming to keep the colliding literal strings distinct -
  these scenarios are not part of the migrated batch and were never meant
  to share an identity; per user direction, none of these ad hoc names or
  units are treated as fixed.
- Explicitly NOT changing: any scenario's `units`/`standard_name` where the
  scenario's own test intent depends on the current value (for example, a
  deliberate cross-unit mismatch used to exercise a unit-conversion
  coupler) - only incidental/placeholder values are touched.

## Capabilities

No new or modified capabilities: this is a test-fixture and build-configuration
change with no change to any MAPL library requirement or observable runtime
behavior (`generic/standard-name-enforcement`'s and
`generic/field-name-propagation`'s existing requirements are unchanged - this
change only makes MAPL's own test suite exercise them against a real,
non-empty dictionary for the first time). `.openspec.yaml` sets
`skip_specs: true` accordingly.

## Impact

- `superstructure/generic/tests/field_dictionary_test.yaml`: extended with
  the batch's `standard_name` entries; existing `test_*`-prefixed and
  non-scenario entries (already relied on by `Test_Aspects.pf`,
  `Test_VectorBasisKind.pf`, `Test_FieldDictIntegration.pf`) are left as-is.
- `superstructure/generic/tests/CMakeLists.txt`: one additional
  `configure_file`-style copy step so the fixture also lands as
  `geos_field_dictionary.yaml` in the shared test working directory.
- Scenario fixtures and their `expectations.yaml` under
  `superstructure/generic/tests/scenarios/`: `scenario_1`, `scenario_2`,
  `scenario_reexport_twice`, `propagate_geom`, `memory_checkpoint`,
  `statistics`, `statistics_real`, `vertical_regridding_2`,
  `vertical_regridding_3`, `invalidate`, `3d_specs`,
  `precision_extension`/`precision_extension_3d`, `ungridded_dims`, `names_1`
  (rename-only touches on the last five, no dictionary entries needed for
  them).
- Since `superstructure/generic/tests/` builds six separate pFUnit
  executables sharing one working directory
  (`MAPL.generic.scenarios/.transforms/.vertical/.aspects/.components/.core`),
  activating the fixture affects all six, not only
  `MAPL.generic.scenarios` - expected to be a net positive (it is already
  the fixture `Test_Aspects.pf`/`Test_VectorBasisKind.pf`/
  `Test_FieldDictIntegration.pf` were seemingly prepared to use) but must be
  verified by a full regression run, not assumed.
- `gridcomps/configurable/tests/` (a separate binary/working directory) and
  `tests/MAPL3G_Component_Testing_Framework/` (standalone Cap-driver
  processes, not pFUnit) are out of scope for this change - see Non-Goals
  in `design.md`.
- New: `superstructure/generic/tests/Test_ScenariosStrict.pf` (the nine
  migrated scenarios' checks, moved from `Test_Scenarios.pf`) and a new
  `MAPL.generic.scenarios.strict` pFUnit executable/ctest target
  (`superstructure/generic/tests/CMakeLists.txt`), running under `STRICT`
  `ValidationMode` via a new `Initialize_strict()` entry point in the
  existing `mapl_pFUnit_Initialize_mod` (`pfunit/MAPL_Initialize.F90`); no
  other pFUnit binary is affected. `Test_MemoryCheckpoint.pf` moved wholesale
  into the new target (it only covers `memory_checkpoint`, one of the nine).
  Fixed two genuine, pre-existing defects the split's strict-mode
  verification surfaced that permissive mode had always masked: three
  mismatched Import/Export `standard_name` pairs in the `I_A1`/`E_A1`/`Z_A1`
  family, and a hardcoded `standard_name='<unknown>'` literal in
  `ProtoStatGridComp.F90` - see `design.md`'s post-implementation notes.
