esma_set_this(OVERRIDE MAPL.orbit)

set(srcs MAPL_OrbGridCompMod.F90)

esma_add_library(${this} SRCS ${srcs} DEPENDENCIES MAPL.shared MAPL.constants MAPL.base MAPL.generic TYPE ${MAPL_LIBRARY_TYPE})
target_link_libraries(
  ${this}
  PUBLIC
    GFTL::gftl
    GFTL_SHARED::gftl-shared
    ESMF::ESMF
    NetCDF::NetCDF_Fortran
  PRIVATE
    MPI::MPI_Fortran
    OpenMP::OpenMP_Fortran
)
target_include_directories(${this} PUBLIC $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

set_target_properties(
  ${this}
  PROPERTIES
    Fortran_MODULE_DIRECTORY
      ${include_${this}}
)
