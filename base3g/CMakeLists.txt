esma_set_this (OVERRIDE MAPL.base3g)

set (srcs
  SimulationTime.F90
  Comms.F90
  SatVapor.F90
  StringTemplate.F90
  LocalDisplacementEnsemble.F90
  MAPL_MemUtils.F90
  FileIO.F90
  TimeInterpolation.F90
  MAPL_sun_uc.F90
  API.F90
)

esma_add_library (${this}
  SRCS ${srcs}
  DEPENDENCIES ESMF::ESMF PFLOGGER::pflogger GFTL_SHARED::gftl-shared-v2 MAPL.shared MAPL.geom MPI::MPI_Fortran NetCDF::NetCDF_Fortran
  TYPE SHARED
)

target_compile_definitions (${this} PRIVATE MAPL_MODE)

target_include_directories (${this} PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)
