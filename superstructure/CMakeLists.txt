esma_set_this (OVERRIDE MAPL.superstructure)
# Superstructure layer (Tier 4 in MAPL v3 restructuring).
# Consolidates: component/, generic3g/, state/.

set (srcs
    API.F90
  )

add_subdirectory (interfaces)
add_subdirectory (state)
add_subdirectory (component)
add_subdirectory (generic)

esma_add_library (${this}
    SRCS ${srcs}
    DEPENDENCIES MAPL.ESMF_Interfaces MAPL.state MAPL.component MAPL.generic PFLOGGER::pflogger GFTL_SHARED::gftl-shared-v2 ESMF::ESMF
    TYPE SHARED
)
