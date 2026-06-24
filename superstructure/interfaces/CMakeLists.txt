esma_set_this (OVERRIDE MAPL.ESMF_Interfaces)
# ESMF abstract interface definitions used across the superstructure.
# Kept separate from MAPL.esmf (infrastructure) to avoid circular dependencies
# with MAPL.generic.

set (srcs
    ESMF_Interfaces.F90
    API.F90
)

esma_add_library (${this}
    SRCS ${srcs}
    DEPENDENCIES ESMF::ESMF
    TYPE SHARED
)

target_include_directories (${this} PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
)
