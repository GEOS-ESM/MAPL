esma_set_this()


esma_add_library (${this}
  SRCS MAPL.F90 mapl3g.F90
  DEPENDENCIES MAPL.base MAPL.generic MAPL.generic3g MAPL.cap3g MAPL.pfio MAPL_cfio_r4 MAPL.gridcomps MAPL.orbit MAPL.griddedio MAPL.field_utils ${EXTDATA_TARGET}
               ESMF::ESMF NetCDF::NetCDF_Fortran MPI::MPI_Fortran
               $<$<BOOL:${BUILD_WITH_FLAP}>:FLAP::FLAP>
  TYPE ${MAPL_LIBRARY_TYPE}
  )

target_compile_definitions (${this} PRIVATE $<$<BOOL:${USE_EXTDATA2G}>:BUILD_WITH_EXTDATA2G>)

target_include_directories (${this} PUBLIC
          $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

ecbuild_add_executable(TARGET GEOS.x SOURCES GEOS.F90 DEPENDS MAPL.generic3g MAPL.cap3g ESMF::ESMF)
target_link_libraries(GEOS.x PRIVATE ${this})
