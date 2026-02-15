esma_set_this (OVERRIDE MAPL.componentDriverGridComp)
find_package (MPI REQUIRED)

set (srcs
 ComponentDriverGridComp.F90
 time_support.F90
)

esma_add_library(${this}
   SRCS ${srcs}
   DEPENDENCIES mapl3g MAPL TYPE SHARED)

ecbuild_add_executable(TARGET MAPL_Component_Driver.x SOURCES MAPL_Component_Driver.F90 DriverCap.F90 DEPENDS ${this} MAPL mapl3g ESMF::ESMF)
target_link_libraries (MAPL_Component_Driver.x PRIVATE MAPL mapl3g MAPL.cap3g MPI::MPI_Fortran ESMF::ESMF OpenMP::OpenMP_Fortran)
target_include_directories (MAPL_Component_Driver.x PRIVATE $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
add_dependencies(build-tests MAPL_Component_Driver.x)
