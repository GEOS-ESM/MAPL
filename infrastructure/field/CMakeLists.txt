esma_set_this (OVERRIDE MAPL.field)

# Fields infrastructure layer (Tier 3 in MAPL v3 restructuring).
# Consolidates: field/, field_bundle/, state/.

set (srcs
    # From field/
    API.F90
    FieldUtils.F90
    FieldBLAS.F90
    FieldUtilities.F90
    FieldUnaryFunctions.F90
    FieldBinaryOperations.F90
    FieldUnits.F90
    FieldCondensedArray.F90
    FieldCondensedArray_private.F90
    FieldFillDefault.F90
    FieldFill.F90
    FieldCreate.F90
    FieldDelta.F90
    FieldGet.F90
    FieldSet.F90
    FieldInfo.F90
    RestartModes.F90
    Internal_field.F90
    Export_field.F90
    # From field_bundle/
    ../field_bundle/API.F90
    ../field_bundle/FieldBundleGet.F90
    ../field_bundle/FieldBundleGetByIndex.F90
    ../field_bundle/FieldBundleSet.F90
    ../field_bundle/FieldBundleInfo.F90
    ../field_bundle/FieldBundleDelta.F90
    ../field_bundle/FieldBundleCreate.F90
    ../field_bundle/FieldBundleCopy.F90
    ../field_bundle/FieldBundleDestroy.F90
    ../field_bundle/FieldBundleGetPointer.F90
    ../field_bundle/FieldBundleMatch.F90
    ../field_bundle/Internal_field_bundle.F90
    # From state/
    ../state/API.F90
    ../state/StateGet.F90
    ../state/StateSet.F90
    ../state/StateGetPointer.F90
    ../state/StateGetGeom.F90
    ../state/StateAddMethod.F90
    ../state/StateUtils.F90
    ../state/StateArithmeticParser.F90
    ../state/StateMasking.F90
    ../state/StateFilter.F90
    ../state/StateDestroy.F90
    ../state/Internal_state.F90
)

list (APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}")

if (BUILD_WITH_PFLOGGER)
    find_package (PFLOGGER REQUIRED)
endif ()
find_package (GFTL REQUIRED)

esma_add_library (${this}
    SRCS ${srcs}
    DEPENDENCIES MAPL.enums MAPL.vertical_grid MAPL.shared MAPL.esmf MAPL.geom
                 PFLOGGER::pflogger ESMF::ESMF udunits2f GFTL::gftl-v2
    TYPE SHARED
)

target_include_directories (${this} PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../state>
)

# Backward-compatibility aliases
foreach (alias MAPL.fields MAPL.field_bundle MAPL.state)
    add_library (${alias} INTERFACE)
    target_link_libraries (${alias} INTERFACE MAPL.field)
    install (TARGETS ${alias} EXPORT MAPL-targets)
endforeach ()

if (PFUNIT_FOUND)
    add_subdirectory (tests)
    add_subdirectory (../field_bundle/tests field_bundle_tests)
    add_subdirectory (../state/tests state_tests)
endif ()
