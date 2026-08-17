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
| `infrastructure/` | 70 migrated sites recorded below; remaining sites legacy | mixed | `nag-stack`: full Essential pending final gate |
| `superstructure/` | 8 migrated sites recorded below; remaining sites legacy | mixed | `nag-stack`: affected tests passed |
| `gridcomps/` | 54 migrated sites recorded below; remaining sites legacy | mixed | `nag-stack`: affected tests passed where available |
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
| `gridcomps/statistics/StatisticsGridComp.F90:108,204,309` | `_FAIL` -> `_FAIL_CODE` | `MAPL_VALUE_NOT_SUPPORTED` (6) | validation | none | Existing statistics configuration behavior preserved | verified; statistics Essential passed |
| `gridcomps/statistics/TimeVariance.F90:232` | `_FAIL` -> `_FAIL_CODE` | `MAPL_VALUE_NOT_SUPPORTED` (6) | validation | none | Existing variance algorithm behavior preserved | verified; statistics Essential passed |
| `gridcomps/history/HistoryUtilities.F90:53,55,59` | `_ASSERT` -> `_ASSERT_CODE` | `MAPL_ARGUMENT_INVALID` (24), `MAPL_LOOKUP_FAILURE` (22) | configuration validation | none | Existing history item parsing behavior preserved | verified; history Essential passed |
| `gridcomps/history/HistoryCollectionGridComp_private.F90:106,113,149` | `_ASSERT`/`_FAIL` -> generated code forms | `MAPL_VALUE_NOT_SUPPORTED` (6), `MAPL_ARGUMENT_INVALID` (24) | validation | none | Existing history bundle and alias validation behavior preserved | verified; history Essential passed |
| `gridcomps/extdata/ExtDataConfig.F90:81,85,113,141,156` | `_ASSERT`/`_FAIL` -> generated code forms | `MAPL_ARGUMENT_INVALID` (24), `MAPL_LOOKUP_FAILURE` (22), `MAPL_VALUE_NOT_SUPPORTED` (6) | configuration validation | none | Existing ExtData configuration behavior preserved | verified; extdata Essential passed |
| `gridcomps/extdata/ExtDataCollection.F90:46,71,100,127` | `_ASSERT`/`_FAIL` -> generated code forms | `MAPL_ARGUMENT_INVALID` (24), `MAPL_VALUE_NOT_SUPPORTED` (6) | configuration validation | none | Existing collection parsing behavior preserved | verified; extdata Essential passed |
| `gridcomps/extdata/ExtDataRule.F90:61,67` | `_ASSERT` -> `_ASSERT_CODE` | `MAPL_ARGUMENT_INVALID` (24) | configuration validation | none | Existing export-rule validation behavior preserved | verified; extdata Essential passed |
| `gridcomps/orbit/MAPL_OrbGridCompMod.F90:93,359,371` | `_ASSERT`/`_FAIL` -> generated code forms | `MAPL_ARGUMENT_INVALID` (24), `MAPL_VALUE_NOT_SUPPORTED` (6), `MAPL_INTERNAL_INVARIANT_FAILURE` (26) | configuration/validation | none | Existing orbit setup and sizing behavior preserved | verified; full NAG build passed; no dedicated orbit test registered |
| `gridcomps/componentDriverGridComp/componentDriverGridComp.F90:188,295` | `_FAIL`/`_ASSERT` -> generated code forms | `MAPL_ARGUMENT_INVALID` (24) | configuration validation | none | Existing run-mode and expression validation behavior preserved | verified; component-driver tests passed where registered |
| `gridcomps/componentDriverGridComp/time_support.F90:106` | `_FAIL` -> `_FAIL_CODE` | `MAPL_VALUE_NOT_SUPPORTED` (6) | validation | none | Existing time-unit validation behavior preserved | verified; full NAG build passed |
| `gridcomps/extdata/ClimDataSetFileSelector.F90:51,66,85,91,92,93,94` | `_ASSERT` -> `_ASSERT_CODE` | `MAPL_ARGUMENT_INVALID` (24) | validation | none | Existing climatology range validation behavior preserved | verified; extdata Essential passed |
| `gridcomps/extdata/NonClimDataSetFileSelector.F90:49,59,87` | `_ASSERT` -> `_ASSERT_CODE` | `MAPL_ARGUMENT_INVALID` (24) | validation | none | Existing persistence/range validation behavior preserved | verified; extdata Essential passed |
| `gridcomps/extdata/AbstractDataSetFileSelector.F90:84` | `_FAIL` -> `_FAIL_CODE` | `MAPL_LOOKUP_FAILURE` (22) | lookup | none | Existing file search failure behavior preserved | verified; extdata Essential passed |
| `gridcomps/extdata/PrimaryExport.F90:88,175,229,342` | `_ASSERT`/`_FAIL` -> generated code forms | `MAPL_ARGUMENT_INVALID` (24), `MAPL_VALUE_NOT_SUPPORTED` (6) | validation | none | Existing export specification behavior preserved | verified; extdata Essential passed |
| `gridcomps/extdata/DataSetNode.F90:197` | `_ASSERT` -> `_ASSERT_CODE` | `MAPL_ARGUMENT_INVALID` (24) | validation | none | Existing node-side precondition preserved | verified; extdata Essential passed |
| `gridcomps/extdata/DataSetBracket.F90:61` | `_FAIL` -> `_FAIL_CODE` | `MAPL_ARGUMENT_INVALID` (24) | validation | none | Existing bracket-side validation preserved | verified; extdata Essential passed |
| `gridcomps/FakeParent/FakeParentGridComp.F90:68` | `_ASSERT` -> `_ASSERT_CODE` | `MAPL_CONFIGURATION_INVALID` (25) | configuration | none | Existing FakeParent configuration behavior preserved | verified; cap tests passed |
| `gridcomps/configurable/ConfigurableGridComp.F90:73` | `_ASSERT` -> `_ASSERT_CODE` | `MAPL_ARGUMENT_INVALID` (24) | validation | none | Existing vertical-profile shape validation preserved | verified; cap tests passed |
| `gridcomps/history/HistoryGridComp_private.F90:81,84,112,140` | `_ASSERT` -> `_ASSERT_CODE` | `MAPL_CONFIGURATION_INVALID` (25), `MAPL_ARGUMENT_INVALID` (24) | configuration validation | none | Existing history configuration behavior preserved | verified; history tests passed |
| `gridcomps/history/HistoryCollectionGridComp_private.F90:266,270` | `_ASSERT` -> `_ASSERT_CODE` | `MAPL_ARGUMENT_INVALID` (24) | validation | none | Existing per-variable override behavior preserved | verified; history tests passed |
| `infrastructure/field/FieldUtilities.F90:57,83,113,140,183,204,211,239` | `_ASSERT`/`_FAIL` -> generated code forms | `MAPL_ARGUMENT_INVALID` (24), `MAPL_UNSUPPORTED_TYPE` (5) | field validation | none | Existing field operation behavior preserved | verified; field tests passed |
| `infrastructure/field/FieldCreate.F90:192,222,223,343,359,361` | `_ASSERT` -> `_ASSERT_CODE` | `MAPL_OBJECT_NOT_INITIALIZED` (23), `MAPL_ARGUMENT_INVALID` (24) | lifecycle/validation | none | Existing field creation behavior preserved | verified; field tests passed |
| `infrastructure/field/FieldDelta.F90:254,346` | `_ASSERT` -> `_ASSERT_CODE` | `MAPL_OBJECT_NOT_INITIALIZED` (23), `MAPL_VALUE_NOT_SUPPORTED` (6) | lifecycle/validation | none | Existing field reallocation behavior preserved | verified; full Essential passed |
| `infrastructure/esmf/ESMF_Utilities.F90:165,192,210` | `_ASSERT`/`_FAIL` -> generated code forms | `MAPL_UNSUPPORTED_TYPE` (5), `MAPL_ARGUMENT_INVALID` (24) | state validation | none | Existing state intent behavior preserved | verified; full Essential passed |
| `infrastructure/esmf/FieldPointerUtilities.F90:227,265,304,342,380,489,932,952,972,992,997,1098` | `_FAIL` -> generated code forms | `MAPL_UNSUPPORTED_TYPE` (5), `MAPL_VALUE_NOT_SUPPORTED` (6) | type/rank validation | none | Existing pointer utility behavior preserved | verified; full Essential passed |
| `superstructure/generic/ComponentSpecParser/parse_var_specs.F90:215,246,279` | `_ASSERT`/`_FAIL` -> generated code forms | `MAPL_ARGUMENT_INVALID` (24), `MAPL_VALUE_NOT_SUPPORTED` (6) | configuration validation | none | Existing variable-spec parsing behavior preserved | verified; generic tests passed |
| `superstructure/generic/ComponentSpecParser/parse_geometry_spec.F90:66,79,83` | `_ASSERT`/`_FAIL` -> generated code forms | `MAPL_ARGUMENT_INVALID` (24), `MAPL_CONFIGURATION_INVALID` (25) | configuration validation | none | Existing geometry parsing behavior preserved | verified; generic tests passed |
| `superstructure/generic/ComponentSpecParser/to_itemtype.F90:48` | `_FAIL` -> `_FAIL_CODE` | `MAPL_VALUE_NOT_SUPPORTED` (6) | type validation | none | Existing unknown-subclass behavior preserved | verified; generic tests passed |
| `superstructure/state/StateMasking.F90:44,55,61,148` | `_ASSERT`/`_FAIL` -> generated code forms | `MAPL_ARGUMENT_INVALID` (24), `MAPL_VALUE_NOT_SUPPORTED` (6) | mask validation | none | Existing state-mask behavior preserved | verified; state tests passed |

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
- Gridcomps statistics batch: `module load nag-stack/default`; full build passed; `MAPL.statistics.tests` passed.
- Gridcomps history batch: `module load nag-stack/default`; full build passed; `MAPL.history.tests` passed.
- Gridcomps ExtData batch: `module load nag-stack/default`; full build passed; `MAPL.extdata.tests` passed.
- Gridcomps component-driver batch: `module load nag-stack/default`; full build passed; `basic_captest` and `parent_child_captest` passed.
- Infrastructure field/ESMF batch: `module load nag-stack/default`; full build passed; full Essential 65/65 passed.
- Superstructure parser/state batch: `module load nag-stack/default`; full build passed; targeted generic/state tests passed; full Essential 65/65 passed.
- Final validation checkpoint: `module load nag-stack/default`; full build passed; `ctest --test-dir nag -L ESSENTIAL --output-on-failure` passed 65/65.
- Generic consolidation: lookup, lifecycle, argument, configuration, and file-not-found groups use canonical codes; former specific entries remain deprecated aliases.
- Internal invariant consolidation: repeated NCIO/SimpleBundle count and bounds checks use canonical code 26; scientific and shape-specific groups remain separate for review.
- mp_utils batch: `module load nag-stack/default`; `cmake --build nag -j 8` and `ctest --test-dir nag -L ESSENTIAL --output-on-failure` passed; Essential 65/65.
- Infrastructure vertical batch: `module load nag-stack/default`; full build passed; rebuilt `MAPL.vertical_grid.tests`; targeted test passed.
