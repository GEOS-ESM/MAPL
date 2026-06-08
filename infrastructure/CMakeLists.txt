esma_set_this (OVERRIDE MAPL.infrastructure)

# MAPL.infrastructure is the single consolidated library for all of
# infrastructure/. It is being built up incrementally; subdirectories
# are absorbed here as their circular dependencies are resolved.
# See GitHub issue #5014.

# --- field_dictionary (absorbed from infrastructure/field_dictionary/) ---
set (srcs
    API.F90
    field_dictionary/FieldDictionaryItem.F90
    field_dictionary/FieldDictionaryItemMap.F90
    field_dictionary/FieldDictionary.F90
    field_dictionary/FieldDictionaryConfig.F90
)

if (BUILD_WITH_PFLOGGER)
    find_package (PFLOGGER REQUIRED)
endif ()
find_package (GFTL_SHARED REQUIRED)

add_subdirectory (esmf)
add_subdirectory (field)
add_subdirectory (field_bundle)
add_subdirectory (geom)
add_subdirectory (regridder_mgr)
add_subdirectory (geom_io)
add_subdirectory (vertical)

esma_add_library (${this}
    SRCS ${srcs}
    DEPENDENCIES MAPL.esmf MAPL.field MAPL.field_bundle MAPL.geom MAPL.regridder_mgr MAPL.GeomIO MAPL.vertical PFLOGGER::pflogger GFTL_SHARED::gftl-shared-v2 ESMF::ESMF
    TYPE SHARED
)

# Backward-compatibility aliases
add_library (MAPL.field_dictionary ALIAS MAPL.infrastructure)
