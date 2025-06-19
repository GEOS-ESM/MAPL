esma_set_this()

set (srcs
  mapl3g.F90
  MaplFramework.F90
  )

esma_add_library (${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL.generic3g MAPL.pfio MAPL.cap3g MAPL.griddedio MAPL.field ${EXTDATA_TARGET}
               ESMF::ESMF NetCDF::NetCDF_Fortran MPI::MPI_Fortran PFLOGGER::pflogger
  TYPE SHARED
  )

target_compile_definitions (${this} PRIVATE $<$<BOOL:${USE_EXTDATA2G}>:BUILD_WITH_EXTDATA2G>)

target_include_directories (${this} PUBLIC
          $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

ecbuild_add_executable(TARGET GEOS.x SOURCES GEOS.F90 DEPENDS MAPL.generic3g MAPL.cap3g ESMF::ESMF)
target_link_libraries(GEOS.x PRIVATE ${this})

# GEOS.x is needed for 'make tests'
add_dependencies(build-tests GEOS.x)
