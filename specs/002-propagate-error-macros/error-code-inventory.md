# Error Code Inventory: Remaining Macro Migration

Raw scan from foundation issue #5324 found approximately 314 macro references across
MAPL-owned source/test files. This inventory is authoritative for issue #5328.

## Review Checklist

For every site record:

- Failure condition and category
- Recovery/action
- Return-code behavior
- Existing code and meaning
- Required context type and fields
- Canonical group or explicit rejection
- Migration status

## Batch Status

| Subsystem | Sites | Status | Essential result |
|---|---:|---|---|
| `base/` | 92 migrated sites recorded below | mixed | `nag`: Essential 65/65 passed |
| `mp_utils/` | 7 migrated sites recorded below; remaining sites legacy | mixed | `nag-stack`: Essential 65/65 passed |
| `infrastructure/` | 49 migrated sites recorded below; remaining sites legacy | mixed | `nag-stack`: affected Essential tests passed |
| `superstructure/` | 1 migrated site recorded below | mixed | `nag`: Essential 65/65 passed |
| `gridcomps/` | TBD | legacy | pending |
| `tests/` | TBD | legacy | pending |

No new codes or non-string context types are approved by this initial inventory.

## Approved Site Records

| Source | Macro | Canonical code | Category | Context | Return behavior | Status |
|---|---|---:|---|---|---|---|
| `superstructure/generic/RestartHandler.F90:91` | `_ASSERT` -> `_ASSERT_CODE_CTX` | `MAPL_FILE_NOT_FOUND` (1) | io | `filename` string | Existing assertion failure/`rc` behavior preserved | verified; Essential 65/65 passed |
| `base/FileMetadataUtilities.F90:85,105,123,141,161,181,201,221` | `_ASSERT` -> `_ASSERT_CODE_CTX` | `MAPL_LOOKUP_FAILURE` (22) | lookup | `variable=...` string | Existing assertion return path preserved | verified; Essential 65/65 passed |
| `base/SimpleBundleMod.F90:349,370,374,619,629` | `_FAIL` -> generated code forms | `MAPL_UNSUPPORTED_TYPE` (5) / `MAPL_LOOKUP_FAILURE` (22) | validation/lookup | field and bundle strings | Existing early-return behavior preserved | verified; Essential 65/65 passed |
| `base/NCIO.F90:263,372,388,408,426,449,470,481,488,2858,2941` | `_FAIL` -> generated code forms | `MAPL_LOOKUP_FAILURE` (22) | lookup | variable/field context | Existing early-return behavior preserved | verified; Essential 65/65 passed |
| `base/FileMetadataUtilities.F90:144,164,184,204,224,342,352,362,383,442,486,498,513,528,538,548` | `_ASSERT`/`_FAIL` -> generated code forms | `MAPL_LOOKUP_FAILURE` (22), `MAPL_UNSUPPORTED_TYPE` (5), `MAPL_VALUE_NOT_SUPPORTED` (6) | metadata lookup/validation | attribute string where available | Existing early-return behavior preserved | verified; Essential 65/65 passed |
| `base/FileIOShared.F90:216,219,222,225,228,231,234,280,321,338,648,649,695,696` | `_ASSERT`/`_FAIL` -> generated code forms | `MAPL_ARGUMENT_INVALID` (24) / `MAPL_UNSUPPORTED_TYPE` (5) | validation | catalog code identifies invalid shape/type | Existing early-return behavior preserved | verified; Essential 65/65 passed |
| `base/MAPL_LocStreamMod.F90:1010,1156,1247,1458,1463,1514,1519,1734,1740` | `_ASSERT` -> `_ASSERT_CODE_CTX` | `MAPL_OBJECT_NOT_INITIALIZED` (23) | lifecycle | `LocStream` or `LocStream tiling` | Initialization/tiling precondition preserved | verified; Essential 65/65 passed |
| `base/SunOrbit.F90:1016,2906,3006,3106` | `_ASSERT` -> `_ASSERT_CODE_CTX` | `MAPL_OBJECT_NOT_INITIALIZED` (23) | lifecycle | `SunOrbit` | Creation precondition preserved | verified; Essential 65/65 passed |
| `mp_utils/Partition.F90:55,56,58,64` | `_ASSERT_CODE_CTX` | `MAPL_ARGUMENT_INVALID` (24) | validation | numeric argument context | Existing validation behavior preserved | verified; Essential 65/65 passed |
| `superstructure/generic/UserSetServices.F90:161` | `_ASSERT_CODE_CTX` | `MAPL_VALUE_NOT_SUPPORTED` (6) | validation | DSO name | Existing validation behavior preserved | verified; Essential 65/65 passed |
| `superstructure/generic/vertical/FixedLevelsVerticalGrid.F90:106` | `_ASSERT_CODE_CTX` | `MAPL_CONFIGURATION_INVALID` (25) | configuration | physical dimension | Existing configuration behavior preserved | verified; Essential 65/65 passed |
| `base/NCIO.F90:1105,1106,1175,1176,1406,1407,1476,1477,1658,1659,1742,1743,1988,1989,2072,2073,2730,2745,3900,3921` | `_ASSERT` -> `_ASSERT_CODE_CTX` | `MAPL_INTERNAL_INVARIANT_FAILURE` (26) | state | invariant name | Existing internal consistency checks preserved | verified; Essential 65/65 passed |
| `base/SimpleBundleMod.F90:220,676,693,730,759` | `_ASSERT` -> `_ASSERT_CODE_CTX` | `MAPL_INTERNAL_INVARIANT_FAILURE` (26) | state | invariant name | Existing internal consistency checks preserved | verified; Essential 65/65 passed |
| `mp_utils/ArrayReductions.F90:90,175` | `_ASSERT` -> `_ASSERT_CODE` | `MAPL_ARGUMENT_INVALID` (24) | validation | none | Existing argument validation behavior preserved | verified; Essential 65/65 passed |
| `mp_utils/LocalDisplacementEnsemble.F90:176` | `_FAIL` -> `_FAIL_CODE` | `MAPL_VALUE_NOT_SUPPORTED` (6) | validation | none | Existing unsupported-operation return behavior preserved | verified; Essential 65/65 passed |
| `mp_utils/SimpleCommSplitter.F90:156,160` | `_ASSERT` -> `_ASSERT_CODE` | `MAPL_ARGUMENT_INVALID` (24) | validation | none | Existing argument validation behavior preserved | verified; Essential 65/65 passed |
| `infrastructure/vertical/vertical/VerticalConservativeMap.F90:47,48,63` | `_ASSERT` -> `_ASSERT_CODE` | `MAPL_ARGUMENT_INVALID` (24) | validation | none | Existing layer validation behavior preserved | verified; vertical-grid Essential passed |
| `infrastructure/vertical/vertical/VerticalLinearMap.F90:48,49,50` | `_ASSERT` -> `_ASSERT_CODE` | `MAPL_ARGUMENT_INVALID` (24) | validation | none | Existing interpolation precondition behavior preserved | verified; vertical-grid Essential passed |
| `infrastructure/vertical/vertical/VerticalCoordinate.F90:124,131,180,182,232,243` | `_ASSERT`/`_FAIL` -> generated code forms | `MAPL_LOOKUP_FAILURE` (22), `MAPL_VALUE_NOT_SUPPORTED` (6), `MAPL_UNSUPPORTED_TYPE` (5) | metadata validation | coordinate/attribute context | Existing coordinate parsing return behavior preserved | verified; vertical-grid Essential passed |
| `infrastructure/vertical/vertical_grid/VerticalGridManager.F90:92,112,131,153,156,170,174,177,182,211,239,248,271,281,304,312,328,351,374` | `_ASSERT`/`_FAIL` -> generated code forms | `MAPL_INTERNAL_INVARIANT_FAILURE` (26), `MAPL_LOOKUP_FAILURE` (22), `MAPL_CONFIGURATION_INVALID` (25) | registry/configuration | factory/grid context retained where legacy tests require it | Existing manager failure and return behavior preserved | verified; vertical-grid Essential passed |
| `infrastructure/field_bundle/FieldBundleSet.F90:104,144` | `_FAIL` -> `_FAIL_CODE` | `MAPL_VALUE_NOT_SUPPORTED` (6) | validation | none | Existing bundle validation behavior preserved | verified; field-bundle Essential passed |
| `infrastructure/field_bundle/FieldBundleGet.F90:153` | `_FAIL` -> `_FAIL_CODE` | `MAPL_VALUE_NOT_SUPPORTED` (6) | validation | none | Existing unsupported geometry behavior preserved | verified; field-bundle Essential passed |
| `infrastructure/geom/CoordinateAxis/get_coordinates_dim.F90:22,30` | `_ASSERT`/`_FAIL` -> generated code forms | `MAPL_LOOKUP_FAILURE` (22), `MAPL_UNSUPPORTED_TYPE` (5) | metadata validation | none | Existing coordinate lookup behavior preserved | verified; geom Essential passed |
| `infrastructure/geom/CoordinateAxis/get_dim_name.F90:54` | `_ASSERT` -> `_ASSERT_CODE` | `MAPL_LOOKUP_FAILURE` (22) | lookup | none | Existing duplicate-match behavior preserved | verified; geom Essential passed |
| `infrastructure/geom/Mesh/MeshGeomSpec_smod.F90:87,107` | `_FAIL` -> `_FAIL_CODE` | `MAPL_VALUE_NOT_SUPPORTED` (6), `MAPL_ARGUMENT_INVALID` (24) | configuration | none | Existing mesh configuration behavior preserved | verified; geom Essential passed |
| `infrastructure/regridder_mgr/RoutehandleParam.F90:304,326` | `_FAIL` -> `_FAIL_CODE` | `MAPL_VALUE_NOT_SUPPORTED` (6) | validation | none | Existing regrid-method validation behavior preserved | verified; regridder Essential passed |
| `infrastructure/regridder_mgr/EsmfRegridder.F90:232` | `_FAIL` -> `_FAIL_CODE` | `MAPL_VALUE_NOT_SUPPORTED` (6) | validation | none | Existing geometry validation behavior preserved | verified; regridder Essential passed |
| `infrastructure/regridder_mgr/Regridder.F90:80,88,89,130,158,159` | `_ASSERT` -> `_ASSERT_CODE` | `MAPL_ARGUMENT_INVALID` (24) | validation | none | Existing bundle/vector validation behavior preserved | verified; regridder Essential passed |

### Merge Review: Restart File Missing

- Failure condition: requested restart file does not exist after bootstrap skip handling.
- Recovery/action: caller must provide readable restart file or use bootstrap mode.
- Return behavior: existing assertion return path remains unchanged.
- Required context: restart `filename`; catalog template owns diagnostic prose.
- Rejected near-matches: generic file I/O failures remain separate until their recovery and status semantics are reviewed.

### Merge Review: Metadata Variable Lookup

- Failure condition: requested metadata variable pointer is not associated.
- Recovery/action: caller must request an available variable or correct input metadata.
- Return behavior: assertion exits same procedure path; context now comes from `var_name`.
- Required context: variable name string; catalog template owns diagnostic prose.
- Rejected near-matches: attribute retrieval failures remain separate because they occur after variable lookup and have different recovery actions.

## Validation Evidence

- NAG build: `cmake --build nag -j 8` passed.
- Targeted tests: `MAPL.utils.tests` and `MAPL.error_code_generator` passed.
- Essential gate: `ctest --test-dir nag -L ESSENTIAL --output-on-failure` passed 65/65.
- Infrastructure vertical batch: `module load nag-stack/default`; full build passed; rebuilt `MAPL.vertical_grid.tests`; targeted test passed.
- Infrastructure field-bundle/geometry batch: `module load nag-stack/default`; full build passed; `MAPL.field_bundle.tests` and `MAPL.geom.tests` passed.
- Infrastructure regridder batch: `module load nag-stack/default`; full build passed; `MAPL.regridder_mgr.tests` passed.
- Generic consolidation: lookup, lifecycle, argument, configuration, and file-not-found groups use canonical codes; former specific entries remain deprecated aliases.
- Internal invariant consolidation: repeated NCIO/SimpleBundle count and bounds checks use canonical code 26; scientific and shape-specific groups remain separate for review.
- mp_utils batch: `module load nag-stack/default`; `cmake --build nag -j 8` and `ctest --test-dir nag -L ESSENTIAL --output-on-failure` passed; Essential 65/65.
- Infrastructure vertical batch: `module load nag-stack/default`; full build passed; rebuilt `MAPL.vertical_grid.tests`; targeted test passed.
