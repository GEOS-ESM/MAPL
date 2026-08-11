# MAPL3 Derived Type Constructors That Can Return an Exception

Derived types whose constructors accept an `rc` integer argument with `intent(out)` or `intent(inout)`.
A constructor is identified by a named `interface` block or `generic` statement mapping the type name to the implementing procedure.

---

## `gridcomps/ExtData/`

| Derived Type | Constructor | File |
|---|---|---|
| `ExtDataCollection` | `new_ExtDataCollection` | `gridcomps/ExtData/ExtDataCollection.F90` |
| `ExtDataDerived` | `new_ExtDataDerived` | `gridcomps/ExtData/ExtDataDerived.F90` |
| `ExtDataRule` | `new_ExtDataRule` | `gridcomps/ExtData/ExtDataRule.F90` |
| `ExtDataSample` | `new_ExtDataSample` | `gridcomps/ExtData/ExtDataSample.F90` |
| `PrimaryExport` | `new_PrimaryExport` | `gridcomps/ExtData/PrimaryExport.F90` |

## `gridcomps/statistics/`

| Derived Type | Constructor | File |
|---|---|---|
| `TimeAccumulate` | `new_TimeAccumulate` | `gridcomps/statistics/TimeAccumulate.F90` |
| `TimeAverage` | `new_TimeAverage` | `gridcomps/statistics/TimeAverage.F90` |
| `TimeMax` | `new_TimeMax` | `gridcomps/statistics/TimeMax.F90` |
| `TimeMin` | `new_TimeMin` | `gridcomps/statistics/TimeMin.F90` |

## `infrastructure/esmf/`

| Derived Type | Constructor | File |
|---|---|---|
| `SimpleAlarm` | `construct_simple_alarm` | `infrastructure/esmf/alarm/SimpleAlarm.F90` |

## `infrastructure/field_dictionary/`

| Derived Type | Constructor | File |
|---|---|---|
| `FieldDictionary` | `new_from_yaml` | `infrastructure/field_dictionary/FieldDictionary.F90` |
| `FieldDictionaryConfig` | `new_from_hconfig` | `infrastructure/field_dictionary/FieldDictionaryConfig.F90` |

## `infrastructure/fields/`

| Derived Type | Constructor | File |
|---|---|---|
| `StateMask` | `new_StateMask` | `infrastructure/fields/state/StateMasking.F90` |

## `infrastructure/geom/`

| Derived Type | Constructor | File |
|---|---|---|
| `EASEGeomSpec` | `new_EASEGeomSpec` | `infrastructure/geom/geom/EASE/EASEGeomSpec.F90` |
| `MeshGeomSpec` | `new_MeshGeomSpec_from_file` | `infrastructure/geom/geom/Mesh/MeshGeomSpec.F90` |

## `mp_utils/`

| Derived Type | Constructor | File |
|---|---|---|
| `CommGroupDescription` | `new_CommGroupDescription` | `mp_utils/CommGroupDescription.F90` |
| `CsvProfileReporter` | `new_CsvProfileReporter` | `mp_utils/profiler/reporting/CsvProfileReporter.F90` |
| `ProfileReporter` | `new_ProfileReporter_config` | `mp_utils/profiler/reporting/ProfileReporter.F90` |

## `pfio/`

| Derived Type | Constructor | File |
|---|---|---|
| `Attribute` | `new_Attribute_0d` | `pfio/Attribute.F90` |
| `Attribute` | `new_Attribute_1d` | `pfio/Attribute.F90` |
| `ClientManager` | `new_ClientManager` | `pfio/ClientManager.F90` |
| `CoordinateVariable` | `new_CoordinateVariable` | `pfio/CoordinateVariable.F90` |
| `DirectoryService` | `new_DirectoryService` | `pfio/DirectoryService.F90` |
| `IntArray` | `new_IntArray_1d` | `pfio/IntArray.F90` |
| `IntArray` | `new_IntArray_1d_size` | `pfio/IntArray.F90` |
| `LocalMemReference` | `new_LocalMemReference` | `pfio/LocalMemReference.F90` |
| `LocalMemReference` | `new_LocalMemReference_0d` | `pfio/LocalMemReference.F90` |
| `LocalMemReference` | `new_LocalMemReference_1d` | `pfio/LocalMemReference.F90` |
| `LocalMemReference` | `new_LocalMemReference_2d` | `pfio/LocalMemReference.F90` |
| `LocalMemReference` | `new_LocalMemReference_3d` | `pfio/LocalMemReference.F90` |
| `LocalMemReference` | `new_LocalMemReference_4d` | `pfio/LocalMemReference.F90` |
| `LocalMemReference` | `new_LocalMemReference_5d` | `pfio/LocalMemReference.F90` |
| `ModifyMetadataMessage` | `new_ModifyMetadataMessage` | `pfio/ModifyMetadataMessage.F90` |
| `MpiServer` | `new_MpiServer` | `pfio/MpiServer.F90` |
| `MpiSocket` | `new_MpiSocket` | `pfio/MpiSocket.F90` |
| `MultiCommServer` | `new_MultiCommServer` | `pfio/MultiCommServer.F90` |
| `MultiGroupServer` | `new_MultiGroupServer` | `pfio/MultiGroupServer.F90` |
| `MultiLayerServer` | `new_MultiLayerServer` | `pfio/MultiLayerServer.F90` |
| `RDMAReference` | `new_RDMAReference` | `pfio/RDMAReference.F90` |
| `ReplaceMetadataMessage` | `new_ReplaceMetadataMessage` | `pfio/ReplaceMetadataMessage.F90` |
| `ServerThread` | `new_ServerThread` | `pfio/ServerThread.F90` |
| `ShmemReference` | `new_ShmemReference` | `pfio/ShmemReference.F90` |
| `UnlimitedEntity` | `new_UnlimitedEntity_0d` | `pfio/UnlimitedEntity.F90` |
| `UnlimitedEntity` | `new_UnlimitedEntity_1d` | `pfio/UnlimitedEntity.F90` |
| `UnlimitedEntity` | `new_UnlimitedEntity_2d` | `pfio/UnlimitedEntity.F90` |
| `UnlimitedEntity` | `new_UnlimitedEntity_3d` | `pfio/UnlimitedEntity.F90` |
| `UnlimitedEntity` | `new_UnlimitedEntity_4d` | `pfio/UnlimitedEntity.F90` |
| `UnlimitedEntity` | `new_UnlimitedEntity_5d` | `pfio/UnlimitedEntity.F90` |
| `Variable` | `new_Variable` | `pfio/Variable.F90` |

## `utils/`

| Derived Type | Constructor | File |
|---|---|---|
| `ISO8601Date` | `construct_ISO8601Date` | `utils/MAPL_ISO8601_DateTime.F90` |
| `ISO8601DateTime` | `construct_ISO8601DateTime` | `utils/MAPL_ISO8601_DateTime.F90` |
| `ISO8601Duration` | `construct_ISO8601Duration` | `utils/MAPL_ISO8601_DateTime.F90` |
| `ISO8601Time` | `construct_ISO8601Time` | `utils/MAPL_ISO8601_DateTime.F90` |

---

**Total: 53 constructors** across 23 source files.
