esma_set_this (OVERRIDE MAPL.generic)

set (srcs
  SurrogateFrameworkComponent.F90
  AbstractComponent.F90

  MaplGeneric.F90
  MAPL_Generic_Stubs.F90

  # OpenMP support
  OpenMP_Support.F90
  )

if (BUILD_WITH_PFLOGGER)
  find_package(PFLOGGER REQUIRED)
endif ()
find_package(GFTL REQUIRED)
find_package(GFTL_SHARED REQUIRED)

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL.shared MAPL.profiler MAPL.base
               PFLOGGER::pflogger GFTL_SHARED::gftl-shared-v2 GFTL_SHARED::gftl-shared-v1  GFTL::gftl-v2 GFTL::gftl-v1
  TYPE SHARED
  )
target_include_directories (${this} PUBLIC
  $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
target_link_libraries (${this} PUBLIC ESMF::ESMF NetCDF::NetCDF_Fortran
  PRIVATE OpenMP::OpenMP_Fortran)

if (PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()
