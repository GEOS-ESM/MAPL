esma_set_this(OVERRIDE MAPL2)

esma_add_library (${this}
  SRCS MAPL.F90
  DEPENDENCIES MAPL MAPL.base MAPL.generic MAPL.generic3g MAPL.pfio MAPL.gridcomps MAPL.orbit MAPL.field ${EXTDATA_TARGET}
               ESMF::ESMF NetCDF::NetCDF_Fortran MPI::MPI_Fortran
  TYPE SHARED
  )

target_compile_definitions (${this} PRIVATE $<$<BOOL:${USE_EXTDATA2G}>:BUILD_WITH_EXTDATA2G>)

target_include_directories (${this} PUBLIC
          $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
