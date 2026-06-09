esma_set_this (OVERRIDE MAPL.field)

set (srcs
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
    API.F90
)

list (APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}")

if (BUILD_WITH_PFLOGGER)
    find_package (PFLOGGER REQUIRED)
endif ()
find_package (GFTL REQUIRED)

esma_add_library (${this}
    SRCS ${srcs}
    DEPENDENCIES MAPL.enums MAPL.vertical_grid MAPL.utils MAPL.esmf MAPL.geom
                 PFLOGGER::pflogger ESMF::ESMF udunits2f GFTL::gftl-v2
    TYPE SHARED
)

target_include_directories (${this} PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
)

# Backward-compatibility alias
add_library (MAPL.fields INTERFACE)
target_link_libraries (MAPL.fields INTERFACE MAPL.field)
install (TARGETS MAPL.fields EXPORT MAPL-targets)

if (PFUNIT_FOUND)
    add_subdirectory (tests)
endif ()
