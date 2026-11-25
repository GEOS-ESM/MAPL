# gridcomps/ExtData: architecture

**Audience:** coding agents and developers working on the legacy (resource-file) ExtData gridded component in MAPL.
**Describes:** the MAPL2 line, branch `release/v2`.
**Verified against:** `MAPL` @ `aea13cf6` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `0f2e0fb`): key `MAPL`, checked out at `src/Shared/@MAPL`; this directory is `gridcomps/ExtData/`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

## Purpose
The original ExtData: fills model imports from files, configured by an ESMF
resource file (`ExtData.rc`) with a `PrimaryExports` and a `DerivedExports`
table. The component GEOSgcm runs is the YAML-driven one in
[`../ExtData2G/ARCHITECTURE.md`](../ExtData2G/ARCHITECTURE.md); the two share
the role (provider of last resort for root imports), the Cap hand-off of the
export list and the update/bracket/interpolate idea.

## Build and selection
- Target `MAPL.ExtData` (`CMakeLists.txt`), always built (`gridcomps/CMakeLists.txt`
  adds it unconditionally). Depends on `MAPL.shared`, `MAPL.constants`,
  `MAPL.base`, `MAPL.generic`, `MAPL.pfio`, `MAPL.state_utils`,
  `MAPL.griddedio`, `MAPL_cfio_r4`.
- Sources: `ExtDataGridCompMod.F90` (module `MAPL_ExtDataGridCompMod`, about
  4.6k lines), `ExtData_IOBundleMod.F90`, `ExtData_IOBundleVectorMod.F90`.
  With Intel Release builds `ExtDataGridCompMod.F90` is compiled at the lower
  optimisation level (comment: 192 s at O3, 12 s at O1).
- Used when Cap's `USE_EXTDATA2G:` is false (code default). Cap then adds
  child `EXTDATA` with `ExtData1G_SetServices`. Cap `use`s the module
  regardless of the option. Cap creates the child and runs it each step the
  same way as the 2G component (`MAPL_CapGridComp`).

## Configuration: `ExtData.rc`
Cap reads `EXTDATA_CF:` (default `ExtData.rc`), adds `NX`, `NY`, `CF_EXTDATA`,
`EXPID`, `RUN_DT`. The component re-reads the file named by `CF_EXTDATA` and
scans for two table blocks, each ended by a `%%` line (`ExtDataGridCompMod.F90`):
- `PrimaryExports%%`: one line per import with whitespace-separated columns,
  per the header comment of the shipped examples: import name, units, `Clim`
  (Y/N), regrid-method flag, refresh time template, offset factor, scale
  factor, variable on file, file template (also `/dev/null` for a constant).
  Multiple lines for one name give time-ranged entries.
- `DerivedExports%%`: per line, import name, expression, refresh template;
  the expression is evaluated with the same mask and arithmetic helpers
  (`MAPL_ExtDataEvaluateMask`, `CalcDerivedField`).
Run-time sequence mirrors the 2G one (`CheckUpdate`, bracket times on file
`UpdateBracketTime`, read through pFIO i-clients, `MAPL_ExtDataInterpField`,
vertical interpolation, derived fields), but the file/time logic is inside the
one large module rather than split into handler classes.

## Does GEOSgcm still ship legacy files?
- Not for the main run: `gcm_run.j` writes an empty `ExtData.rc` ("Keep this
  due to MAPL expectation of an ExtData.rc file, but it is not actually used
  for anything"), and `CAP.rc.tmpl` sets `USE_EXTDATA2G: .TRUE.`.
- `src/Applications/@CPLFCST_Etc/RC/` holds `PrimaryExports%%` files
  (`BC_`, `CO_`, `NI_`, `OC_`, `SU_GridComp_ExtData-clim.rc`).
  *Unverified:* whether any job template uses them; they are not referenced
  from `@GEOSgcm_App` by name.
- MAPL's own `Tests/ExtData_Testing_Framework/test_cases/*/ExtData.rc` use the
  table format.

## Existing documents
None in this directory. The user documents in `../ExtData2G/` (`README.md`,
`ExtDataGridComp.md`) describe the YAML component, not this one.

## Observations
- `../ExtData2G/README.md` says Cap instantiates "ExtData" without saying
  which generation; the code picks by `USE_EXTDATA2G`.
- Both directories define the I/O bundle files under the same file names with
  different module names.
- Two ExtData implementations are compiled in a default build; only one is
  selected at run time.
