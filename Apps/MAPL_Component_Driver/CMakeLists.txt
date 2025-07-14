
find_package (MPI REQUIRED)

ecbuild_add_executable(TARGET MAPL_Component_Driver.x SOURCES MAPL_Component_Driver.F90 DriverCap.F90 DriverCapGridComp.F90 RootGridComp.F90 time_support.F90 DEPENDS MAPL mapl3g ESMF::ESMF)
target_link_libraries (MAPL_Component_Driver.x PRIVATE MAPL mapl3g MPI::MPI_Fortran ESMF::ESMF OpenMP::OpenMP_Fortran)
target_include_directories (MAPL_Component_Driver.x PRIVATE $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
