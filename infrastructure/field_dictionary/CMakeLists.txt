esma_set_this (OVERRIDE MAPL.field_dictionary)

# Field dictionary layer (Tier 3 in MAPL v3 restructuring).
# Separated from generic3g/ to allow future migration to a NUOPC layer.

set (srcs
    FieldDictionaryItem.F90
    FieldDictionaryItemMap.F90
    FieldDictionary.F90
    FieldDictionaryConfig.F90
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

