esma_set_this ()

# Common dependencies always required
set (mapl_dependencies
  MAPL.base
  MAPL.pfio
  MAPL_cfio_r4
  MAPL.griddedio
  MAPL.field_utils
  MAPL.state_utils
  MAPL.vertical
  MAPL.oomph
  ESMF::ESMF
  NetCDF::NetCDF_Fortran
  MPI::MPI_Fortran
  $<$<BOOL:${BUILD_WITH_FLAP}>:FLAP::FLAP>
  )

list (APPEND mapl_dependencies
  MAPL.generic
  MAPL.gridcomps
  MAPL.orbit
  MAPL.python_bridge
  ${EXTDATA_TARGET}
  )

esma_add_library (${this}
  SRCS MAPL.F90
  DEPENDENCIES ${mapl_dependencies}
  TYPE ${MAPL_LIBRARY_TYPE}
  )

target_compile_definitions (${this} PRIVATE $<$<BOOL:${USE_EXTDATA2G}>:BUILD_WITH_EXTDATA2G>)

target_include_directories (${this} PUBLIC
          $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
