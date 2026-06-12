esma_set_this (OVERRIDE MAPL.state)

set (srcs
    StateGet.F90
    StateSet.F90
    StateGetPointer.F90
    StateGetGeom.F90
    StateAddMethod.F90
    StateUtils.F90
    StateArithmeticParser.F90
    StateMasking.F90
    StateFilter.F90
    StateDestroy.F90
    API.F90
)

esma_add_library (${this}
    SRCS ${srcs}
    DEPENDENCIES MAPL.field_bundle MAPL.utils MAPL.esmf MAPL.geom
                 PFLOGGER::pflogger ESMF::ESMF GFTL::gftl-v2
    TYPE SHARED
)

target_include_directories (${this} PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
)

if (PFUNIT_FOUND)
    add_subdirectory (tests EXCLUDE_FROM_ALL)
endif ()
