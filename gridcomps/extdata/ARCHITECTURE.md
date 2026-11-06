# MAPL extdata gridded component

**Audience:** coding agents and developers working on MAPL external-data input.
**Describes:** the MAPL3 line, branch `develop`.
**Verified against:** `MAPL` @ `1a5a5acf` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `56ce4e8`, GEOSgcm `release/MAPL-v3`): key `MAPL`, checked out at `src/Shared/@MAPL`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

Up: [gridcomps layer](../ARCHITECTURE.md), [repository architecture](../../docs/ARCHITECTURE.md),
[AGENTS.md](../../AGENTS.md). Sibling: [cap](../cap/ARCHITECTURE.md).
Spec: [coordinate tolerance](../../openspec/specs/extdata/file-collection-coordinate-tolerance/spec.md).

## 1. Purpose

ExtData supplies inputs that no component computes: for each field another
component imports and the cap connects to extdata, extdata reads the data from
files (through the parallel-I/O layer), interpolates in time between records,
regrids, and fills its export. What to read is given by a YAML rule file.
Exports are declared from that YAML in `setServices`.

## 2. Place in the hierarchy

- Child of the cap: `mapl: children: EXTDATA` with
  `setServices: sharedObj: libMAPL.extdata` and a `config_file`
  (`gridcomps/cap/tests/basic_captest/extdata.yaml`). The cap runs it first each step
  when `run_extdata` is true and connects `src=extdata, dst=root`
  (`gridcomps/cap/CapGridComp.F90`).
- Extdata has no children.

## 3. Entry points

All in `ExtDataGridComp.F90`, module `mapl_ExtDataGridComp_mod`:
- `setServices`: registers initialize `GENERIC::INIT_REALIZE_PROVIDED`
  (`realize_provided`), run `run`, finalize (`finalize_extdata`); reads the
  component `hconfig`; declares export specs (`add_var_specs` in
  `ExtDataGridComp_private.F90`); attaches private state `ExtData`.
- `realize_provided`: once only (`has_run_mod_advert`). Takes the *active* items
  of the export state (those another component requested, allocation status
  active: `get_active_items`), builds `ExtDataConfig` from the YAML for the
  current time (`new_ExtDataConfig_from_yaml`), and for each active item checks
  a rule exists (assert `no rule for extdata item`), creates one
  `PrimaryExport` per rule (`make_PrimaryExport`), and completes the export
  spec (`complete_export_spec`). An item with more than one rule gets
  `name<rule_sep>j` internal names; a rule is picked by time (`get_item_index`).
- `run`: for each active item: select the rule for the current time, call
  `update_export_spec` if the rule changed; skip constants; ask
  `pointer_update%check_update` whether to update; `update_my_bracket`
  (choose records, compute weights); collect reads into one `ExtDataReader`;
  `read_items` once; then `handle_fractional_regrid`.
- `finalize_extdata`: if `log_files_read` is set, writes a YAML file listing
  `run_start`, `run_end`, `files_read` to that path; otherwise nothing.

## 4. State

- Exports only; no imports. Declared by `MAPL_GridCompAddSpec` in
  `add_var_specs` from `Exports` (type from `get_maplitem_type`, with bracket
  size, `fill_value=0.0`), constants (`collection: /dev/null`, an
  expression item) and `Derived` (`function:` expression, units `<unknown>`).
  Nested files named in `subconfigs` are read recursively.
- Private state `ExtDataGridComp`: `PrimaryExportVector` of rules, index
  vectors, active item names, last-used rule per item, files-read set.
- Per-item time data is held in a `DataSetBracket` of `DataSetNode`s, chosen
  by a file selector (`NonClimDataSetFileSelector`, `ClimDataSetFileSelector`,
  both of `AbstractDataSetFileSelector`).
- Restarts: none.

## 5. Configuration that changes structure

Top-level YAML keys read by `ExtDataConfig.F90`: `subconfigs` (list of
files), `Collections`, `Samplings`, `Exports`, `Derived`, `debug`,
`log_files_read`. Keys read in the rule, collection, sample and derived
classes (named by the source files `ExtDataRule`, `ExtDataCollection`,
`ExtDataSample`, `ExtDataDerived`): `collection`, `variable`, `sample`,
`regrid`, `linear_transformation`, `starting` (for a list of rules),
`update_frequency`, `update_offset`, `update_reference_time`, `source_time`,
`exact`, `time_interpolation`, `extrapolation`, `fail_on_missing_file`,
`valid_range`, `enable_vertical_regrid`, `vertical_alignment`, `function`,
`template`, `freq`, `coordinate_tolerance`.
A map value of an `Exports` key is one rule; a sequence value is a time-ordered
list of rules (sorted by `starting`, `sort_rules_by_start`). `coordinate_tolerance`
defaults to a nonzero value and can be set to 0 (`ExtDataCollection.F90`,
`openspec/specs/extdata/.../spec.md`).

## 6. Dependencies

Uses `MAPL`, `ESMF::ESMF`, `pflogger`, gFTL2 containers (`StringVector`,
`StringSet`, maps). File I/O goes through the `ExtDataReader` type
(`ExtDataFileReader.F90`, `add_item`/`read_items`, with a client collection
id). Extdata has no dependency on history.

## 7. Known traps

- Exports are created for every name in the YAML, but only those requested by
  other components (active) get rules built; a requested item without a rule
  fails at `realize_provided`.
- A rule is chosen by simulation time; rule changes call `update_export_spec`
  during `run`.
- Exports are field bundles (a bracket of time records), not plain fields; the
  weights are set on the bundle (`set_weights`).
- `realize_provided` is guarded to run once.

## 8. Decisions

### Rules built at realize time, from active items only
- Pattern: `realize_provided` builds `PrimaryExport`s only for exports that another component has made active (`ExtDataGridComp.F90`, `get_active_items`, `realize_provided`). [Verified]
- Likely reason: unrequested inputs need no file access or rule. [Inferred]

### Declaring exports from YAML in `setServices`
- Pattern: `setServices` parses the YAML to add export specs, so exports exist before connections are made (`add_var_specs`). [Verified]
- Likely reason: the cap's `ConnectAll` needs the export names at connection time. [Inferred]

## 9. Observations (noted, not fixed)

- Tests: pFUnit target `MAPL.extdata.tests` (`extdata/tests`: `Test_ExtDataNodeBracket.pf`,
  `Test_ExtDataCollection.pf`, `Test_ExtDataSample.pf`, `Test_ExtDataPointerUpdate.pf`;
  `Test_NonClimDataSetFileSelector.pf` and `Test_DataSetNode.pf` are added only
  when `LOCAL_REGRESSION_DATA_DIR` is a directory). The `LABELS "REGRESSION"`
  line is commented out, so the test has no ctest label and is not selected
  by `-L ESSENTIAL`.
- Companion tools: `extdata/extdata_dryrun_check.py` (documented in
  `extdata/extdata_dryrun_check.md`; installed to `bin`) predicts the files a run
  needs; its `--verify_files_read` mode compares against the file list that
  `log_files_read` makes `finalize_extdata` write (per its own document).
  `tests/generate_extdatadriver_input.py` is in the top-level `tests/` directory.
- The spec directory holds one capability so far
  (`file-collection-coordinate-tolerance`).
