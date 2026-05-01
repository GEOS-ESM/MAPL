esma_set_this (OVERRIDE MAPL.base3g)

set (srcs
  SimulationTime.F90
)

esma_add_library (${this}
  SRCS ${srcs}
  DEPENDENCIES ESMF::ESMF PFLOGGER::pflogger GFTL_SHARED::gftl-shared-v2
  TYPE SHARED
)
