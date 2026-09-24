## 1. Activate the fixture

- [x] 1.1 In `superstructure/generic/tests/field_dictionary_test.yaml`, add
      entries for the `I_A1`/`E_A1`/`Z_A1`/`I_B1`/`E_B1`/`Z_B1` family
      (canonical_units matching their current, already-consistent-within-this-family
      declared units; long_name following the existing `<name> long name`
      convention already used by `names_1`). Leave all existing `test_*`
      and non-scenario entries (`air_temperature`, `air_pressure PLE`,
      `air_pressure PL`, `A`) untouched.
- [x] 1.2 Add entries for `statistics`/`statistics_real`'s fields using the
      new CF `snake_case` names decided in task 4.1 (not the current
      "Surface Temperature"-style text). `statistics`'s `T` field reuses
      the pre-existing `air_temperature` entry rather than a new one.
- [x] 1.3 Add entries for `vertical_regridding_2`/`vertical_regridding_3`.
      (Plan revised during implementation: rather than keeping the
      distinctly-suffixed names as separate dictionary entries, the
      scenarios' own names were consolidated onto plain `air_pressure`/
      `air_temperature` - see task 5.1 - so only `air_pressure` and
      `height` are new entries here; `air_temperature` reuses the
      pre-existing entry.)
- [x] 1.4 In `superstructure/generic/tests/CMakeLists.txt`, add a second
      `configure_file(field_dictionary_test.yaml geos_field_dictionary.yaml
      COPYONLY)` next to the existing `field_dictionary_test.yaml` copy step
      (design.md Decision 2), so the fixture also lands under the default
      path `initialize_field_dictionary` looks for.
- [x] 1.5 Built `superstructure/generic/tests/` and ran its full ctest
      suite (`MAPL.generic.scenarios`, `.transforms`, `.vertical`,
      `.aspects`, `.components`, `.core`) with only tasks 1.1-1.4 applied
      (no scenario YAML changes yet). All 6 binaries passed. Captured full
      verbose output (`--verbose`) for task 2's review. This run surfaced
      two real bugs (not data-consistency issues) in the prior
      `enforce-standard-name-convention` change's Vector handling, fixed
      as part of this change with the user's explicit approval (outside
      this change's original "no library code changes" goal, but blocking
      a clean/meaningful signal for task 2's review):
      1. `VariableSpec.F90`'s `MAPL_STATEITEM_VECTOR` case defaulted a
         Vector's two split component names to the literal `'unknown'`
         instead of leaving them unallocated when no `standard_name` was
         declared at all, so they were treated as real, specified names
         instead of wildcard/unchecked - causing spurious mismatch
         warnings for every such Vector (e.g. the statistics gridcomp's
         internal accumulators).
      2. Fixing (1) then exposed a genuine crash in
         `StandardNameAspect%connect_to_export` (unconditional assignment
         from a potentially-unallocated `export_%standard_name`, an
         "ALLOCATABLE ... is not currently allocated" runtime error) -
         fixed to only copy the export's value when allocated, otherwise
         leaving the import's own value/state untouched.
      Re-verified full `ctest --test-dir nag` (all 74 tests, not just the 6
      `superstructure/generic/tests/` binaries) after both fixes: only the
      7 pre-existing/environmental failures remain.

## 2. Reconcile collisions and unintended fallout

- [x] 2.1 Reviewed task 1.5's verbose output for every "not found in field
      dictionary" warning. All are from non-migrated scenarios/tests
      (`3d_specs`, `precision_extension*`, `ungridded_dims`, `expression*`,
      `history_*`, `service_*`, `regrid*`, `vertical_alignment_*`, etc.) -
      expected, no action needed. None of the three target batches showed
      this warning once fully migrated (task 3-5).
- [x] 2.2 Reviewed task 1.5's output for new test failures: none (all 6
      binaries passed both before and after the batch migrations). The two
      bugs found (task 1.5) were crashes/spurious-warnings discovered by
      reading the log carefully, not `ctest` failures - `MAPL.generic.scenarios`
      passed even before those fixes; they were fixed anyway because they
      directly undermined the "clean, meaningful warning log" goal this
      task's review depends on.
- [x] 2.3 (Resolved by verification, not renaming - design.md Decision 4
      revised) Read `invalidate/A.yaml`, `B.yaml`, `description.md`,
      `expectations.yaml` in full. Confirmed: both sides already declare
      the identical `standard_name: 'Temperature'` (no disagreement to
      fix); the `km`/`m` units difference is the scenario's deliberate,
      documented purpose (a units-conversion coupler test), not an
      incidental placeholder. `'Temperature'` is not added to the
      dictionary by this change, and both sides declare `units` explicitly,
      so dictionary activation has no effect on this scenario beyond one
      harmless permissive-mode "not found" warning per side. No changes
      made to `invalidate`.
- [x] 2.4 (Resolved by verification, not renaming - design.md Decision 4
      revised) Read `3d_specs/A.yaml`/`B.yaml` and
      `ungridded_dims/A.yaml`/`B.yaml` in full. Confirmed the reused
      `'B2 standard name'`/`'I_B1 standard name'` strings are copy-paste-derived
      placeholders across unrelated, unconnected scenario directories -
      cosmetic overlap only. Neither string is part of any of this change's
      three migration batches, so there is no dictionary entry for either
      to accidentally match against - no functional risk. No changes made
      to `3d_specs`, `precision_extension`, `precision_extension_3d`, or
      `ungridded_dims` (all remain explicitly out of scope per design.md
      Non-Goals). (A broader check found the batch 1 family's literal
      strings are *also* reused by several other out-of-scope scenarios -
      `history_wildcard`, `history_1`, `service_with_geom`,
      `service_with_options`, `service_service` - verified harmless: none
      check `long_name` against a conflicting value, and `service_service`
      already independently declares the identical `'Z_A1 long name'`
      explicitly, confirming this is the pre-existing convention, not a
      new collision.)

## 3. Migrate the structural I_A1/E_A1/Z_A1 family

- [x] 3.1 Read `scenario_1`, `scenario_2`, `scenario_reexport_twice`,
      `propagate_geom`, and `memory_checkpoint`'s YAML, `parent.yaml`
      (connections), and `expectations.yaml` in full. Confirmed
      `scenario_2`/`scenario_reexport_twice`'s differing units
      (`meter`/`barn`/`1`) are incidental, not deliberate: in both, the
      fields using different units are never actually connected to each
      other (or the connection they do have already uses matching units on
      both sides), and neither scenario's `expectations.yaml` checks
      `units`/`standard_name` text at all.
- [x] 3.2 Unified `scenario_2`/`scenario_reexport_twice`'s units to `'m'`
      (matching `scenario_1`/`propagate_geom`/`memory_checkpoint`/`names_1`);
      no scenario declared an explicit `long_name` on these fields except
      `names_1` (already matches the dictionary, left untouched). Added a
      one-line comment to each touched YAML file noting the
      `field_dictionary_test.yaml` backing.
- [x] 3.3 Added `long_name` assertions to `scenario_1`, `scenario_2`, and
      `propagate_geom`'s `expectations.yaml` for every `status: complete`
      field in the family (the generic per-field checker already supports
      `long_name:`/`standard_name:` keys via `add_params`, confirmed in
      `Test_Scenarios.pf:112-113` - "essentially free"). Not added to
      `scenario_reexport_twice` (every field there stays `status: gridset`,
      never `complete`, so `long_name` is never meaningful to check per the
      existing "only defined once allocation-complete" rule) or
      `memory_checkpoint` (`Test_MemoryCheckpoint.pf` has no
      name-based checks of any kind).
- [x] 3.4 Re-ran `MAPL.generic.scenarios`: 100% pass, including the new
      `long_name` assertions.

## 4. Migrate statistics / statistics_real to CF-form standard_names

- [x] 4.1 Applied: `"Surface Temperature"` -> `surface_temperature`,
      `"Surface Pressure"` -> `surface_air_pressure`, `"Specific Humidity"`
      -> `specific_humidity`, `"Sea Level Pressure"` -> `air_pressure_at_sea_level`.
      `statistics`'s own `'Temperature'` -> `air_temperature` (reusing the
      pre-existing dictionary entry). Vector name: kept
      `'(eastward_wind,northward_wind) horizontal velocity'` (more
      CF-correct than `vector_1`'s bare `'(eastward,northward) ...'`, since
      `eastward_wind`/`northward_wind` are real CF standard names and
      `eastward`/`northward` alone are not) rather than reconciling with
      `vector_1` - touching `vector_1` would expand scope beyond this
      change's stated Impact list. Did fix a real bug in the process: the
      original text had a space after the comma
      (`'(..._wind, north..._wind) ...'`), which `split_name` parses
      literally, leaving the second component's name with a leading space
      (`' northward_wind horizontal velocity'`) - removed the space so the
      two connected endpoints' compound names parse to identical
      per-component values.
- [x] 4.2 Added `surface_temperature`, `surface_air_pressure`,
      `specific_humidity`, `air_pressure_at_sea_level` dictionary entries
      (task 1.2). No entry added or needed for the vector name - a
      Vector's compound-encoded `standard_name` is exempt from
      `FieldDictionary` lookup by design (`generic/standard-name-enforcement`).
- [x] 4.3 Updated `statistics/A.yaml` and `statistics_real/A.yaml` (export
      declarations) and `statistics_real/collection_1.yaml` (the vector
      imports' matching compound names) to the new names; added
      `long_name` assertions to `statistics`/`statistics_real`'s
      `expectations.yaml` for the `T`/`TS`/`PS` scalar fields at the
      points where `expectations.yaml` already asserts `status: complete`.
- [x] 4.4 Re-ran `MAPL.generic.scenarios`: 100% pass. Verified via targeted
      log inspection that the vector per-component mismatch warnings
      previously present (leading-space artifact) are gone, and no new
      "standard_name convention violation" appears between
      `statistics_real/A.yaml`'s exports and `collection_1.yaml`'s imports.

## 5. Migrate vertical_regridding_2 / vertical_regridding_3

- [x] 5.1 Confirmed via `parent.yaml`/`AGCM.yaml`'s `connections:` that
      `vertical_regridding_2`'s `PLE`(export)/`I_B`(import) and
      `vertical_regridding_3`'s `PLE`/`I_C` and `T_DYN`/`T_PHYS` pairs were
      pre-existing, real (not incidental) standard_name disagreements
      between connected endpoints - a latent gap from the prior
      `enforce-standard-name-convention` change, unrelated to and
      predating this change's dictionary activation. Resolved by
      consolidating the suffixed synthetic names
      (`air_pressure_ple_edge`/`air_pressure_c_center`/`air_pressure_dyn_center`,
      `temperature_dyn_center`/`temperature_phys_center` - suffixes that
      only distinguished which component's copy of the field you were
      looking at, not a real difference in physical quantity) onto plain
      `air_pressure`/`air_temperature`, and removing the `I_B`/`I_D`/`I_C`/
      `T_PHYS` imports' own now-redundant/mismatched standard_name
      declarations entirely (they now correctly report as
      wildcard/unchecked, matching whatever their connected export
      declares).
- [x] 5.2 No redundant `long_name` existed to remove (none of these fields
      declared one before). `expectations.yaml` for both scenarios checks
      only `status`/`typekind`/`rank`/`value`/`vertical_profile`, never
      `standard_name`/`long_name` text, so no assertions were added (would
      not be "essentially free" - would require new checker wiring this
      task did not need to add).
- [x] 5.3 Re-ran `MAPL.generic.scenarios`: 100% pass. Verified via targeted
      log inspection that the three pre-existing "standard_name convention
      violation" warnings between these scenarios' connected endpoints
      (found in task 5.1) are gone. No descoping was needed.

## 6. Regression pass

- [x] 6.1 Forced a truly clean rebuild (touched every `.F90` source, full
      `cmake --build nag -j8` + `--target build-tests`, zero errors) and
      ran the complete `ctest --test-dir nag` (all 74 tests across the
      whole repo, not only `superstructure/generic/tests/`). Result: 67/74
      pass; the 7 failures (`ll-ll`, `cs-cs`, `cs-ll`, `ll-cs`,
      `MAPL3G_Comp_Test_case02/11/23`) are exactly the pre-existing/
      environmental ones (missing datasets, stale platform libraries)
      already confirmed present on the unmodified base branch during the
      prior `enforce-standard-name-convention` change - no new failures.
- [x] 6.2 Updated `CHANGELOG.md` under `[Unreleased]`: a `### Changed`
      entry describing the activated dictionary and the three migrated
      batches, and additions to the existing `### Fixed` section for the
      two bugs found and fixed in task 1.5.

## 7. Verify strict-mode readiness and split into a dedicated executable

- [x] 7.1 Forced `ValidationMode=STRICT` for the whole (unsplit)
      `MAPL.generic.scenarios` binary as a diagnostic (temporary patch to
      `pfunit/MAPL_Initialize.F90`, reverted after). Of the nine migrated
      scenarios, four (`scenario_reexport_twice`, `memory_checkpoint`,
      `statistics_real`, `vertical_regridding_2`) passed strict-clean
      immediately; five did not, for reasons unrelated to dictionary-entry
      coverage.
- [x] 7.2 Fixed `superstructure/generic/tests/gridcomps/ProtoStatGridComp.F90`:
      removed a hardcoded `standard_name='<unknown>'` literal (the
      `StandardNameAspect` "not set" sentinel, passed as if it were a real
      value) from two `mapl_GridCompAddSpec` calls - unblocked `statistics`.
- [x] 7.3 Fixed `scenario_1`, `scenario_2`, `propagate_geom`: each connects
      `E_A1` (Export) to `I_B1` (Import), but `I_B1` declared its own
      independent placeholder `standard_name` instead of the value it
      actually receives. Changed `I_B1`'s `standard_name` to
      `'E_A1 standard name'` in all three `child_B.yaml`s, with matching
      `expectations.yaml` `long_name` updates (dictionary-driven long_name
      now correctly follows the connection's resolved identity).
- [x] 7.4 Root-caused `vertical_regridding_3`'s remaining failure to a
      genuine framework bug in `VerticalGridAspect%make_transform`'s
      coordinate-field lookup (inherited the payload field's
      `StandardNameAspect`, not just its `UnitsAspect`) - not a test-data
      issue, and not specific to this change's scenarios. Per user
      direction, fixed and amended directly into the unpushed
      `enforce-standard-name-convention` (#5413) commit rather than as part
      of this change - see that change's own record. Re-verified all nine
      scenarios (not just the previously-passing four) strict-clean after
      the fix.
- [x] 7.5 Split the nine verified-clean scenarios into a new
      `MAPL.generic.scenarios.strict` pFUnit executable (design.md
      Decision 6): new `Test_ScenariosStrict.pf` (trimmed copy of
      `Test_Scenarios.pf`'s parameter list), `Test_MemoryCheckpoint.pf`
      moved wholesale, new `Initialize_strict()` entry point in
      `mapl_pFUnit_Initialize_mod` (`pfunit/MAPL_Initialize.F90`), new
      `add_pfunit_ctest` target in
      `superstructure/generic/tests/CMakeLists.txt`. Removed the nine
      scenarios' entries from `Test_Scenarios.pf` (moved, not duplicated).
- [x] 7.6 Verified: both `MAPL.generic.scenarios` and
      `MAPL.generic.scenarios.strict` pass cleanly via `ctest`. Positive
      control: temporarily renamed the `air_pressure` dictionary entry and
      confirmed `MAPL.generic.scenarios.strict` fails (20/84) while
      `MAPL.generic.scenarios` is unaffected - confirms `STRICT` is
      genuinely active, not silently still permissive. Reverted the
      sabotage and re-confirmed both pass. Full clean rebuild + full
      `ctest` (75 tests, one more than before from the new target): same 8
      pre-existing/environmental failures as baseline, no new failures.
