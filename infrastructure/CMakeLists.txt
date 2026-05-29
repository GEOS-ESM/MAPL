esma_set_this (OVERRIDE MAPL.infrastructure)

# MAPL.infrastructure is the single consolidated library for all of
# infrastructure/. It is being built up incrementally; subdirectories
# are absorbed here as their circular dependencies are resolved.
# See GitHub issue #5014.

# --- field_dictionary (absorbed from infrastructure/field_dictionary/) ---
set (srcs
    field_dictionary/FieldDictionaryItem.F90
    field_dictionary/FieldDictionaryItemMap.F90
    field_dictionary/FieldDictionary.F90
    field_dictionary/FieldDictionaryConfig.F90
)

if (BUILD_WITH_PFLOGGER)
    find_package (PFLOGGER REQUIRED)
endif ()
find_package (GFTL_SHARED REQUIRED)

esma_add_library (${this}
    SRCS ${srcs}
    DEPENDENCIES MAPL.shared MAPL.esmf PFLOGGER::pflogger GFTL_SHARED::gftl-shared-v2 ESMF::ESMF
    TYPE SHARED
)

# Backward-compatibility alias
add_library (MAPL.field_dictionary ALIAS MAPL.infrastructure)
