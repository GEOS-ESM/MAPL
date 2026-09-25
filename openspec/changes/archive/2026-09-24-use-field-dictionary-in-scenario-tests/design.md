## Context

See `proposal.md - Why` for the observed gap. Four facts about the existing
test infrastructure drive this design:

1. **The dictionary singleton is process-wide, loaded once, for every pFUnit
   binary.** `MaplFramework%initialize` runs via the pFUnit
   `extra_initialize_` hook (`mapl_pfunit_initialize_mod_MP_initialize`)
   exactly once per test executable, before any `@test` runs, with an
   `ESMF_HConfigCreate(content='{}')` empty `mapl_hconfig`
   (`mapl/MaplFramework.F90:255-259`). There is no per-scenario or per-`.pf`-file
   re-initialization hook. Whatever `initialize_field_dictionary`
   (`mapl/MaplFramework.F90:1198-1247`) resolves at that one call is what
   every test in that binary gets for its entire run.
2. **`superstructure/generic/tests/` compiles six separate pFUnit
   executables that all share one CMake `WORKING_DIRECTORY`**
   (`${CMAKE_CURRENT_BINARY_DIR}`): `MAPL.generic.scenarios` (bundles
   `Test_Scenarios.pf` - all 33 scenario directories - plus
   `Test_MemoryCheckpoint.pf`), `.transforms`, `.vertical`, `.aspects`,
   `.components`, `.core` (bundles `Test_FieldDictionary.pf`/
   `Test_FieldDictIntegration.pf`, which manage their own throwaway
   dictionaries via explicit `load_field_dictionary` calls per test -
   `superstructure/generic/tests/Test_FieldDictIntegration.pf:62-77` etc.).
   A file planted in that shared directory is visible to all six processes'
   independent `initialize_field_dictionary` calls.
3. **`superstructure/generic/tests/field_dictionary_test.yaml` already
   exists, in the right schema, partially anticipating this exact need.**
   Its own header comment (lines 170-175) says entries were added "so the
   mandatory dictionary lookup in `make_VariableSpec` succeeds without
   modifying" tests like `Test_Aspects.pf`, `Test_VectorBasisKind.pf`, and
   `Test_FieldDictIntegration.pf` (all of which do use
   `'air_temperature'`/similar names from this file today). It is copied to
   the build directory (`superstructure/generic/tests/CMakeLists.txt:16-18`,
   `configure_file(field_dictionary_test.yaml field_dictionary_test.yaml
   COPYONLY)`) but under its own name, which nothing ever
   `load_field_dictionary`s - the "mandatory lookup" it anticipated
   (`generic/standard-name-enforcement`, GEOS-ESM/MAPL#5413) has since
   landed, but the fixture was never wired up to actually satisfy it.
4. **Scenario fixtures already mostly declare explicit `units`, and rarely
   assert `long_name`.** Spot-checked `scenario_2/expectations.yaml` (its
   `I_A1`/`E_A1` deliberately differ - `meter` vs. `barn` - with no
   `units`/`standard_name`/`long_name` check at all in that file) and
   `expression/expectations.yaml` (checks `long_name` only for the
   `expr(1)`/`I`/`expr2(1)`/`I2` chain, never for the plain `A`/`B`/`C`
   fields). Since `VariableSpec%apply_field_dictionary_defaults_` only fills
   `units`/`long_name` when the `VarSpec` does not already declare them
   (`superstructure/generic/specs/VariableSpec.F90:274-322`, unchanged by
   this design), activating the dictionary cannot silently change a
   scenario's *units* behavior anywhere `units:` is already explicit (the
   overwhelming majority of cases) - the only new effect is `long_name`
   appearing where it previously did not, and a warning where a declared
   `standard_name` has no dictionary entry (permissive mode: warning, not
   failure).

## Goals / Non-Goals

**Goals:**
- Make `superstructure/generic/tests/`'s pFUnit binaries actually load a
  real, non-empty `FieldDictionary`, using the fixture already prepared for
  this, via the existing (already-shipped) default-path fallback mechanism -
  no MAPL library code changes.
- Converge three concrete, bounded batches of scenario fixtures
  (structural `I_A1`/`E_A1`/`Z_A1` family; `statistics`/`statistics_real`;
  `vertical_regridding_2`/`_3`) onto dictionary-backed `standard_name`s, so
  at least some of MAPL's own scenario tests genuinely exercise
  dictionary-driven `long_name` defaulting end-to-end.
- Eliminate the two identified same-process `standard_name`/`units`
  collisions so the shared dictionary's presence does not leave landmines
  for whoever migrates the next batch.
- Leave a clear, low-friction path for future increments (more scenario
  batches, eventually `STRICT` mode) without committing to them now.

**Non-Goals:**
- Flipping `ValidationMode` to `STRICT` for the *existing* `MAPL.generic.scenarios`
  binary as a whole. That would require *every* `standard_name` used anywhere
  in that shared process to be either dictionary-covered or on an exempt item
  type - a much larger undertaking than converging three batches. (Revised
  during implementation - see Decision 6: this was resolved for the
  nine already-converged scenarios by splitting them into a *second*,
  dedicated executable rather than waiting for full coverage of the
  original one.)
- Migrating all 33 scenario directories. The remaining ones
  (`expression*`, `history_*`, `service_*`, `export_dependency`,
  `vertical_alignment_*`, `regrid`/`regrid_r8`, `3d_specs`,
  `precision_extension*`, `ungridded_dims`) are left with their current
  placeholder `standard_name`s (only the two renames needed to remove
  collisions are made to the last three) - candidates for future increments.
- `gridcomps/configurable/tests/` (its own binary, own `WORKING_DIRECTORY`,
  own `FieldDictionary.yml` which is actually the *unrelated* NUOPC field
  dictionary schema, not `mapl_FieldDictionary_mod`'s) and
  `tests/MAPL3G_Component_Testing_Framework/` (30 standalone Cap-driver
  processes, not pFUnit, each capable of taking a real `field_dictionary:`
  cap.yaml key already, but not exercised here).
- Changing any scenario's `units` where the scenario's test intent depends
  on a specific (or specifically mismatched) value.

## Decisions

### Decision 1: Reuse and extend `field_dictionary_test.yaml`; do not create a new fixture

It already has the right module's schema (`mapl_FieldDictionary_mod`'s flat
`standard_name: {canonical_units, long_name, aliases, ...}` map, not the
unrelated NUOPC `field_dictionary: {entries: [...]}` schema used by
`scenarios/FieldDictionary.yml`), and other tests
(`Test_Aspects.pf`, `Test_VectorBasisKind.pf`, `Test_FieldDictIntegration.pf`)
already assume some of its entries (e.g. `air_temperature`) exist. Creating
a second, competing fixture would fragment an already-half-adopted
convention for no benefit.

### Decision 2: Activate it via the existing default-path fallback, not a new explicit `load_field_dictionary` call

`initialize_field_dictionary`'s "no `field_dictionary:` key" branch already
looks for `geos_field_dictionary.yaml` in the working directory
(`mapl/MaplFramework.F90:1221-1228`, from `generic/standard-name-enforcement`).
Add a second `configure_file(field_dictionary_test.yaml
geos_field_dictionary.yaml COPYONLY)` line next to the existing one in
`superstructure/generic/tests/CMakeLists.txt`, so the SAME fixture content
lands under both names in the shared build directory. This activates all
six binaries' already-shipped fallback mechanism with zero framework code
changes.

**Rejected - add an explicit `load_field_dictionary(...)` call inside
`Test_Scenarios.pf`'s existing setup** (which already calls
`NUOPC_FieldDictionarySetup` for the unrelated NUOPC dictionary,
`Test_Scenarios.pf:186`). This was the first idea considered, on the theory
that it would scope the change to scenario tests only. It does not actually
achieve narrower scope: `Test_Scenarios.pf` and `Test_MemoryCheckpoint.pf`
already share one process/one singleton (`MAPL.generic.scenarios`), so an
explicit call there is no more scoped than the default-path fallback for
that binary - and it would leave `MAPL.generic.aspects`/`.core`'s existing
reliance on this fixture (`air_temperature`, etc., Decision 1) still
unaddressed, contradicting the fixture's own stated purpose. The
default-path fallback is simpler (one CMake line, no new code) and finishes
what was evidently already started.

### Decision 3: Roll out as "activate, then reconcile per batch" - do not attempt to pre-resolve every scenario's interaction with the dictionary on paper

Given Decision 2, all 33 scenario directories in `MAPL.generic.scenarios`
(plus everything in `.transforms`/`.vertical`/`.aspects`/`.components`/`.core`)
begin consulting the same dictionary the moment it is activated, whether or
not their `standard_name`s were part of the intentionally-migrated batches.
Per the Context (point 4), the realistic exposure is low - explicit `units`
already shields almost everything, and `long_name` is rarely asserted - but
this is confirmed empirically, not assumed:
1. Activate the fixture (Decision 2) with only the pre-existing entries
   plus the three target batches added.
2. Run the full `superstructure/generic/tests/` ctest suite and read every
   new `standard_name`/`long_name`-related warning or failure it produces.
3. For each: if it is a permissive-mode "not found in dictionary" warning
   on a non-migrated scenario, no action needed (expected, harmless,
   tracked as an implicit inventory of what a future batch could migrate).
   If a previously-passing assertion now fails because a `long_name`/`units`
   default unexpectedly applied, resolve it by either accepting and
   updating the assertion (if the new value is correct/harmless) or by
   renaming the colliding `standard_name` (if the match was accidental).

### Decision 4 (revised during implementation): Neither of the two originally-identified "collisions" needs a rename

The original proposal/design flagged two same-literal-string reuses as
collisions to resolve by renaming. Reading the actual scenario files during
implementation showed both are functionally harmless once the dictionary's
content is scoped to only the three declared batches, so no rename is made
for either:

- **`invalidate`'s `Temperature` (`km` vs. `m`)**: `A.yaml`/`B.yaml` already
  declare the *same* `standard_name: 'Temperature'` on both sides - they
  already agree, so there is no standard_name disagreement at all. The `km`
  vs. `m` units difference is not incidental: `invalidate/description.md`
  states the scenario deliberately "use[s] units and precision change as
  the two couplers", and `expectations.yaml` explicitly comments
  `# km --> m`. Since both sides declare `units` explicitly, `apply_field_dictionary_defaults_`
  never consults the dictionary's `canonical_units` for either (Context
  point 4); `'Temperature'` is not added to the dictionary by this change,
  so the only new effect is one harmless "not found" warning per side.
  Renaming would touch a scenario whose entire purpose is the specific
  behavior a rename would obscure, for no functional benefit.
- **`3d_specs`/`precision_extension*` vs. `ungridded_dims`/`names_1`'s reuse
  of `B2 standard name`/`I_B1 standard name`**: confirmed via
  `3d_specs/A.yaml`/`B.yaml` and `ungridded_dims/A.yaml`/`B.yaml` - clear
  copy-paste-derived placeholder text reused across unrelated, unconnected
  scenario directories. Since neither string is part of any of this
  change's three migration batches (none of the four directories are
  touched otherwise), there is no risk of an unintended dictionary hit -
  the "collision" is cosmetic only (two unrelated scenarios happen to share
  a placeholder string), not a functional risk introduced by activating the
  dictionary. Renaming would touch four directories `Non-Goals` already
  scopes out of this change, for a purely cosmetic reason.

Both were verified (not assumed) by reading every file in each pair and
confirmed harmless; see `tasks.md` 2.3/2.4 for the verification record.

### Decision 5: Batch 2 (`statistics`/`statistics_real`) also fixes a CF-format violation, independent of the dictionary

`standard_name: "Surface Temperature"` (capitalized, space-separated) is not
a valid CF standard name regardless of dictionary activation - CF standard
names are lowercase `snake_case` identifiers. Converging this batch to
`surface_temperature`/etc. is a correctness improvement on its own, made
concrete and low-risk to do now because verifying it means adding one
dictionary entry and re-running the scenario, rather than a speculative
drive-by rename.

### Decision 6 (added post-implementation): Split into `MAPL.generic.scenarios` (permissive) and `MAPL.generic.scenarios.strict` (STRICT), rather than waiting for full coverage

`ValidationMode` is a process-wide singleton (`FieldDictionaryConfig`, set
once per pFUnit binary at `MAPL_initialize` time - see Context above), so it
cannot vary per-scenario within one executable: flipping the existing
`MAPL.generic.scenarios` binary to `STRICT` would fail every one of the ~20
still-unmigrated scenarios at once, which is exactly why the original
Non-Goal deferred this to "someday". Rather than wait for a future increment
to migrate all remaining scenarios first, the nine scenarios verified clean
under `STRICT` (`scenario_1`, `scenario_2`, `scenario_reexport_twice`,
`propagate_geom`, `memory_checkpoint`, `statistics`, `statistics_real`,
`vertical_regridding_2`, `vertical_regridding_3`) were moved into a second,
dedicated pFUnit executable, `MAPL.generic.scenarios.strict`
(`superstructure/generic/tests/Test_ScenariosStrict.pf`, plus
`Test_MemoryCheckpoint.pf` which moved wholesale since it only covers
`memory_checkpoint`), sharing the same `field_dictionary_test.yaml`/
`geos_field_dictionary.yaml` fixture and working directory as
`MAPL.generic.scenarios` but forcing `STRICT` mode via a new
`Initialize_strict()` entry point (`pfunit/MAPL_Initialize.F90`, same module
as the existing `Initialize()`, selected per-executable via
`add_pfunit_ctest`'s `EXTRA_INITIALIZE`/`EXTRA_USE` - no new module, no
per-binary command-line plumbing needed since `add_pfunit_ctest` does not
expose a way to pass literal CLI arguments per target). The remaining ~20
scenarios stay in `MAPL.generic.scenarios` under the default `PERMISSIVE`
mode, migrating to `MAPL.generic.scenarios.strict` as future increments
converge them, exactly as originally envisioned - only the mechanism for
"some strict, some not" changed from "wait for 100% coverage" to "split now,
grow the strict side incrementally."

Verifying this split surfaced a genuine framework bug, unrelated to any
scenario's own data: `VerticalGridAspect%make_transform`'s vertical-regrid
coordinate-field lookup (`superstructure/generic/specs/VerticalGridAspect/make_transform.F90`)
inherited the *payload* field's `StandardNameAspect` wholesale (only
`UnitsAspect` was correctly overridden), so looking up the coordinate field
(e.g. `PLE`, `air_pressure`) spuriously compared it against the payload's
own unrelated `standard_name` (e.g. `air_temperature`) - fatal under
`STRICT`, silently wrong (a warning) under `PERMISSIVE`. This is a defect in
`enforce-standard-name-convention` (GEOS-ESM/MAPL#5413) itself, not in this
change's scenario data, and - since that change's own commit had not yet
been pushed/reviewed - was fixed by amending directly into it rather than as
part of this change; see that change's own record and the `CHANGELOG.md`
`### Fixed` entry for the fix itself.

## Risks / Trade-offs

- **[Risk]** Activating the fixture for all six `superstructure/generic/tests/`
  binaries (not just `MAPL.generic.scenarios`) could surface unrelated
  fallout in `.transforms`/`.vertical`/`.components` that this design's
  Context research did not specifically sample. -> **Mitigation**: Decision
  3's "activate, then run the full suite and read the diff" step covers all
  six binaries, not only the ones with named scenario batches; task list
  includes running the complete `superstructure/generic/tests/` ctest
  targets, not only `MAPL.generic.scenarios`.
- **[Risk]** Removing an explicit `long_name:` from a migrated scenario's
  YAML and relying on the dictionary changes what a *diff* of that scenario
  looks like to a future reader who does not know to check the dictionary
  fixture too. -> **Mitigation**: add a one-line comment in each migrated
  scenario YAML noting the `long_name` now comes from
  `field_dictionary_test.yaml`, mirroring the existing convention of
  explanatory comments already used in `names_1`/`expression`'s YAML.
- **[Trade-off]** This change deliberately leaves ~20 scenario directories
  un-migrated. Their `standard_name`s will keep warning (permissively) once
  the dictionary is active. This is accepted scope per the proposal ("not
  all-or-nothing") - the warnings are a visible, low-cost inventory of
  future migration candidates, not a regression.

## Migration Plan

No production code changes; nothing to roll out beyond the test suite
itself. Rollback is a plain revert of the CMake line and fixture/scenario
YAML edits - no runtime or persisted state is affected. Suggested order:
1. Extend `field_dictionary_test.yaml` with the three batches' entries.
2. Add the `geos_field_dictionary.yaml` copy step to
   `superstructure/generic/tests/CMakeLists.txt`.
3. Build and run the full `superstructure/generic/tests/` ctest suite;
   catalog new warnings/failures (Decision 3).
4. Resolve the two known collisions (Decision 4) plus any newly-discovered
   ones from step 3.
5. Migrate each of the three batches one at a time (structural family,
   then `statistics`/`statistics_real`, then `vertical_regridding_2`/`_3`),
   re-running the affected scenario(s) after each.
6. Full regression pass (`ctest --test-dir <build>`) and CHANGELOG entry.

## Implementation Notes (added post-implementation)

Two things surfaced during `tasks.md` execution that revised this design
beyond what was planned above:

- **Batch 3 (`vertical_regridding_2`/`_3`) needed real fixes, not just new
  dictionary entries.** Reading the actual scenario files (design.md's
  original text only inferred these from a prior research pass, not a
  full read) showed three of this batch's connections already had
  disagreeing `standard_name`s between their Import and Export - a
  pre-existing gap from `generic/standard-name-enforcement`, unrelated to
  and predating this change. Resolved by consolidating the distinctly
  per-component-suffixed synthetic names
  (`air_pressure_ple_edge`/`air_pressure_c_center`/`air_pressure_dyn_center`,
  `temperature_dyn_center`/`temperature_phys_center`) onto plain
  `air_pressure`/`air_temperature` and removing the mismatched imports'
  own standard_name declarations (making them correctly wildcard/unchecked)
  - see `tasks.md` 5.1 for the full record. This reduced Batch 3's new
  dictionary entries from six planned (one per suffixed name) to two
  (`air_pressure`, `height`; `air_temperature` reused the pre-existing
  entry).
- **Two real library bugs were found and fixed, with the user's explicit
  approval, despite this change's "no MAPL library code changes" Goal.**
  Activating the dictionary (task 1.5) and reading the resulting warning
  log carefully - something no prior change had actually done - surfaced:
  (1) `VariableSpec.F90`'s `MAPL_STATEITEM_VECTOR` case defaulted an
  undeclared Vector's two split component names to the literal `'unknown'`
  instead of leaving them unallocated, so `StandardNameAspect` treated them
  as real, specified names (only its own `'<unknown>'` sentinel means
  wildcard) - producing spurious mismatch warnings for every such Vector
  (e.g. the statistics gridcomp's internal accumulators) connecting to any
  vector with a real name; (2) fixing (1) then exposed a genuine runtime
  crash in `StandardNameAspect%connect_to_export` (an unconditional
  assignment from a potentially-unallocated `export_%standard_name`),
  never previously reachable because bug (1) always gave it an allocated
  (if wrong) value to copy. Both are fixed; see `tasks.md` 1.5 and the
  `CHANGELOG.md` `### Fixed` entry for details. This was surfaced to and
  approved by the user as a deliberate, documented exception to the "no
  library code changes" Goal, rather than absorbed silently - see the
  session's questions on this point.
- **The nine migrated scenarios were split into a dedicated `STRICT`
  executable rather than left waiting for a future increment.** See
  Decision 6. Verifying the split with a real strict-mode run (rather than
  assuming the batches were clean because they passed under the default
  permissive mode) found two additional, real defects the earlier batches'
  permissive-mode passes had not - and could not have - surfaced: (1) three
  connected Import/Export pairs in the `I_A1`/`E_A1`/`Z_A1` family
  (`scenario_1`, `scenario_2`, `propagate_geom`'s `E_A1`→`I_B1` connection)
  declared different literal `standard_name`s on each end - fixed by making
  the Import side match its connected Export's value, with a matching
  `expectations.yaml` update; (2) `superstructure/generic/tests/gridcomps/ProtoStatGridComp.F90`
  hardcoded `standard_name='<unknown>'` (`StandardNameAspect`'s own
  "not set" sentinel) as if it were a real value - fixed by omitting the
  argument entirely. Both are genuine pre-existing defects that permissive
  mode's warn-and-continue behavior had always masked, not artifacts of the
  split itself.

## Open Questions

None remaining. The exact set of scenarios chosen for a *future* increment
beyond this change's nine now-`STRICT` scenarios is intentionally left
open - per the proposal, this is expected to continue incrementally
(migrate a batch in `MAPL.generic.scenarios`, verify it strict-clean, move
it to `MAPL.generic.scenarios.strict`), and does not affect this change's
approach or task breakdown.
