esma_set_this (OVERRIDE MAPL.fields)

# Fields infrastructure layer (Tier 3 in MAPL v3 restructuring).
# Consolidates: field/, field_bundle/, state/.

set (srcs
    # From field/
    field/API.F90
    field/FieldUtils.F90
    field/FieldBLAS.F90
    field/FieldUtilities.F90
    field/FieldUnaryFunctions.F90
    field/FieldBinaryOperations.F90
    field/FieldUnits.F90
    field/FieldCondensedArray.F90
    field/FieldCondensedArray_private.F90
    field/FieldFillDefault.F90
    field/FieldFill.F90
    field/FieldCreate.F90
    field/FieldDelta.F90
    field/FieldGet.F90
    field/FieldSet.F90
    field/FieldInfo.F90
    field/StateItemAllocation.F90
    field/RestartModes.F90
    # From field_bundle/
    field_bundle/API.F90
    field_bundle/FieldBundleType_Flag.F90
    field_bundle/FieldBundleGet.F90
    field_bundle/FieldBundleGetByIndex.F90
    field_bundle/FieldBundleSet.F90
    field_bundle/FieldBundleInfo.F90
    field_bundle/FieldBundleDelta.F90
    field_bundle/FieldBundleCreate.F90
    field_bundle/FieldBundleCopy.F90
    field_bundle/FieldBundleDestroy.F90
    field_bundle/FieldBundleGetPointer.F90
    field_bundle/FieldBundleMatch.F90
    # From state/
    state/API.F90
    state/StateGet.F90
    state/StateSet.F90
    state/StateGetPointer.F90
    state/StateGetGeom.F90
    state/StateAddMethod.F90
    state/StateUtils.F90
    state/StateArithmeticParser.F90
    state/StateMasking.F90
    state/StateFilter.F90
    state/StateDestroy.F90
)

list (APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}")

if (BUILD_WITH_PFLOGGER)
    find_package (PFLOGGER REQUIRED)
endif ()
find_package (GFTL REQUIRED)

esma_add_library (${this}
    SRCS ${srcs}
    DEPENDENCIES MAPL.vertical_grid MAPL.shared MAPL.esmf MAPL.geom
                 PFLOGGER::pflogger ESMF::ESMF udunits2f GFTL::gftl-v2
    TYPE SHARED
)

target_include_directories (${this} PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/field>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/state>
)

# Backward-compatibility aliases
foreach (alias MAPL.field MAPL.field_bundle MAPL.state)
    add_library (${alias} INTERFACE)
    target_link_libraries (${alias} INTERFACE MAPL.fields)
    install (TARGETS ${alias} EXPORT MAPL-targets)
endforeach ()

if (PFUNIT_FOUND)
    add_subdirectory (tests)
    add_subdirectory (../field_bundle/tests field_bundle_tests)
    add_subdirectory (../state/tests state_tests)
endif ()
