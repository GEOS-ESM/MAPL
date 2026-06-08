esma_set_this(OVERRIDE MAPL)

set (srcs
  MAPL.F90
  MaplFramework.F90
  )

esma_add_library (${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL.superstructure MAPL.infrastructure MAPL.pfio MAPL.field MAPL.state MAPL.python_bridge MAPL.base MAPL.regridder_mgr MAPL.esmf MAPL.mp_utils MAPL.GeomIO ${EXTDATA_TARGET}
               ESMF::ESMF NetCDF::NetCDF_Fortran MPI::MPI_Fortran PFLOGGER::pflogger
  TYPE SHARED
  )

target_compile_definitions (${this} PRIVATE $<$<BOOL:${USE_EXTDATA2G}>:BUILD_WITH_EXTDATA2G>)

target_include_directories (${this} PUBLIC
          $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

