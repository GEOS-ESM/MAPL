esma_set_this (OVERRIDE MAPL.generic)

set (srcs
  # Abstract interfaces
  SurrogateFrameworkComponent.F90
  AbstractComponent.F90
  AbstractFrameworkComponent.F90
  AbstractComposite.F90

  StringCompositeMap.F90
  ConcreteComposite.F90
  CompositeComponent.F90

  BaseComponent.F90
  MaplComponent.F90
  DecoratorComponent.F90
  StubComponent.F90

  # Specs
  VarSpecType.F90
  VarSpec.F90
  VarSpecPtr.F90
  VarConnPoint.F90
  VarConnType.F90
  VariableSpecification.F90
  VarSpecMiscMod.F90
  VarSpecVector.F90
  VarConnPoint.F90
  VarConnType.F90
  VarConnVector.F90
  VarConn.F90
  StateSpecification.F90
  ComponentSpecification.F90
  MAPL_ServiceConnectionItemVector.F90
  MAPL_ProvidedServiceItemVector.F90
  MAPL_RequestedServiceItemVector.F90
  MAPL_ServiceServicesTypes.F90
  MAPL_ServiceServices.F90

  BaseFrameworkComponent.F90
  MaplGenericComponent.F90

  GenericCplComp.F90

  MaplGeneric.F90

  MAPL_Generic.F90

  )
if (BUILD_WITH_PFLOGGER)
  find_package(PFLOGGER REQUIRED)
endif ()
find_package(GFTL REQUIRED)
find_package(GFTL_SHARED REQUIRED)

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL.oomph MAPL.shared MAPL.profiler MAPL.base
               PFLOGGER::pflogger GFTL_SHARED::gftl-shared-v2 GFTL_SHARED::gftl-shared-v1 GFTL::gftl
  TYPE ${MAPL_LIBRARY_TYPE}
  )
target_include_directories (${this} PUBLIC
  $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
target_link_libraries (${this} PUBLIC esmf NetCDF::NetCDF_Fortran)

if (PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()

add_executable(demo driver.F90 UserComponent.F90)
target_link_libraries(demo PRIVATE ${this})
set_target_properties(demo PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})
