esma_set_this (OVERRIDE root_gridcomp)
find_package (MPI REQUIRED)

esma_add_library(${this}
  SRCS RootGridComp.F90
  DEPENDENCIES mapl3g MAPL
  TYPE SHARED)

ecbuild_add_executable(TARGET MAPL_Component_Driver.x SOURCES MAPL_Component_Driver.F90 DriverCap.F90 DriverCapGridComp.F90 time_support.F90 DEPENDS MAPL mapl3g ESMF::ESMF)
target_link_libraries(MAPL_Component_Driver.x PRIVATE ${this})
target_include_directories (MAPL_Component_Driver.x PRIVATE $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
