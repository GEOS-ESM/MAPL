esma_set_this (OVERRIDE MAPL.base3g)

set (srcs
  SimulationTime.F90
  MAPL_Comms.F90
  MAPL_SatVapor.F90
  StringTemplate.F90
  API.F90
)

esma_add_library (${this}
  SRCS ${srcs}
  DEPENDENCIES ESMF::ESMF PFLOGGER::pflogger GFTL_SHARED::gftl-shared-v2 MAPL.shared MAPL.geom MPI::MPI_Fortran
  TYPE SHARED
)

target_compile_definitions (${this} PRIVATE MAPL_MODE)

target_include_directories (${this} PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)
