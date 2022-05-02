esma_set_this (OVERRIDE MAPL.generic3g)

set(srcs
  ESMF_Interfaces.F90
  UserSetServices.F90
  MethodPhasesMap.F90

  ChildComponent.F90
  ChildComponent_run_smod.F90
  ChildComponentMap.F90
  GenericCouplerComponent.F90
  CouplerComponentVector.F90

  InnerMetaComponent.F90
  OuterMetaComponent.F90
  OuterMetaComponent_setservices_smod.F90
  GenericGridComp.F90

  # ComponentSpecBuilder.F90
  )

list (APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}")

find_package (MPI REQUIRED)
find_package (GFTL REQUIRED)
find_package (GFTL_SHARED REQUIRED)
find_package (YAFYAML REQUIRED)
find_package (PFLOGGER REQUIRED)

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL.shared MAPL.profiler MAPL.base YAFYAML::yafyaml PFLOGGER::pflogger GFTL_SHARED::gftl-shared-v2 GFTL::gftl-v2
  TYPE ${MAPL_LIBRARY_TYPE}
  )

target_include_directories (${this} PUBLIC
  $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
target_link_libraries (${this} PUBLIC esmf NetCDF::NetCDF_Fortran)

if (PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()

