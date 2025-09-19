esma_set_this (OVERRIDE MAPL.regridder_mgr)

set(srcs
  regridder_mgr.F90

  RoutehandleParam.F90
  RoutehandleSpec.F90
  RoutehandleSpecVector.F90
  RoutehandleVector.F90
  DynamicMask.F90
  RoutehandleManager.F90

  RegridderParam.F90
  RegridderSpec.F90
  RegridderSpecVector.F90

  Regridder.F90
  RegridderVector.F90
  NullRegridder.F90
  EsmfRegridder.F90

  RegridderFactory.F90
  EsmfRegridderFactory.F90
  RegridderFactoryVector.F90
  RegridderManager.F90
  RegridderMethods.F90
#HorzFluxRegridder.F90

  )

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL.geom MAPL.field_bundle MAPL.shared GFTL::gftl-v2
  TYPE SHARED
  )

target_include_directories (${this} PUBLIC
  $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
target_link_libraries (${this} PUBLIC ESMF::ESMF)

if (PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()

