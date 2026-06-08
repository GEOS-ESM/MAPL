esma_set_this (OVERRIDE MAPL.field_bundle)

set (srcs
    FieldBundleGet.F90
    FieldBundleGetByIndex.F90
    FieldBundleSet.F90
    FieldBundleInfo.F90
    FieldBundleDelta.F90
    FieldBundleCreate.F90
    FieldBundleCopy.F90
    FieldBundleDestroy.F90
    FieldBundleFilter.F90
    FieldBundleGetPointer.F90
    FieldBundleMatch.F90
    API.F90
)

esma_add_library (${this}
    SRCS ${srcs}
    DEPENDENCIES MAPL.field MAPL.utils MAPL.esmf
                 PFLOGGER::pflogger ESMF::ESMF GFTL::gftl-v2
    TYPE SHARED
)

if (PFUNIT_FOUND)
    add_subdirectory (tests)
endif ()
