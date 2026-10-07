# Dynamic mask lost in ESMF_Info round trip

Date: 2026-10-07

## Bug
ExtData `regrid:` methods that rely on an ESMF dynamic mask (e.g. `FRACTION`,
`VOTE`, `*_MONOTONIC`, and the `missing_value` mask used by
BILINEAR/CONSERVE/PATCH/NEAREST_STOD) silently ran with no mask.

Observed in `/home/bmauer/validation/fractional_regridding_mapl3`
(ExtData `regrid: FRACTION;5`, History output): the `fraction` mask routine was
never called and the output was raw land-type codes (min -5.000004) instead of
fractions in [0,1].

## Root cause
`EsmfRegridderParam` (infrastructure/regridder_mgr/EsmfRegridder.F90) carries a
`dyn_mask`, but ExtData hands the param to the field bundle as `ESMF_Info`:

- `gridcomps/extdata/PrimaryExport.F90` builds the param (with mask) and calls `make_info`.
- `EsmfRegridderParam%make_info` serialized only `routehandle_param`.
- `superstructure/generic/specs/GeomAspect.F90` rebuilds the param via
  `make_EsmfRegridderParam(info)` -> `make_regridder_param_from_info`, which
  called `EsmfRegridderParam(rh_param)` with no mask (and so `termorder = FREE`
  rather than `SRCSEQ`).
- Result: `regrid_field` saw no mask and called `ESMF_FieldRegrid` without
  `dynamicMask`.

## Fix (infrastructure/regridder_mgr/EsmfRegridder.F90)
- `make_info` now also stores, when a mask is present: mask type,
  `handleAllElements`, kind (r4/r8), src mask value, optional dst mask value
  (keys `DynMaskType`, `DynMaskHandleAll`, `DynMaskKind`, `DynMaskSrcValue`,
  `DynMaskDstValue`; values kept as R8).
- `make_regridder_param_from_info` reads them back, rebuilds the `DynamicMask`
  with the r4 or r8 constructor and passes it to `EsmfRegridderParam(rh_param, dyn_mask=...)`,
  which also restores `termorder = SRCSEQ`.

## Test
`infrastructure/regridder_mgr/tests/Test_EsmfRegridderParam.pf` (added to the
tests CMakeLists): round-trips params through `make_info` /
`make_EsmfRegridderParam` and checks equality for FRACTION (r4, r8), VOTE,
CONSERVE/BILINEAR monotonic, BILINEAR/CONSERVE missing_value; that mask types
stay distinguishable; and that a param with no mask stays maskless. Several of
these fail without the fix.

## Verification
- Validation case: with the fix the `fraction` routine runs and regrid output
  is in [0, 0.39] (was -5 .. 58).
- Full `ctest`: 77/77 pass.
- Running GEOS.x from the build needs `LD_LIBRARY_PATH` to include
  `install-debug/lib` and `build-debug/gridcomps/componentDriverGridComp`
  (libMAPL.componentDriverGridComp.so is not installed by `make install`).

## Scope / impact
- Affected: every ExtData regrid method that uses a mask; all ran unmasked.
- Not affected (by code reading): History (param passed in-process through
  VariableSpec -> GeomAspect, no Info round trip) and FieldBundleRead.
- Behavior change: ExtData regrids that previously ignored their mask now apply
  it, so outputs/baselines for those cases may change (correctly).

## Known related gap (not addressed)
`RoutehandleParam` `make_info` / `make_rh_param_from_info` only support
bilinear and conserve; PATCH and NEAREST_STOD `_FAIL` if serialized.

## Remaining
- Add CHANGELOG entry; commit per github-workflow skill.
- Optionally verify the History claim with a History regrid run.
