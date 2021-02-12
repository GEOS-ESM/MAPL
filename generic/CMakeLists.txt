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

  VariableSpecification.F90
  VarSpecVector.F90
  MAPL_VarSpecMod.F90
  StateSpecification.F90
  ComponentSpecification.F90
  
  BaseFrameworkComponent.F90
  MaplGenericComponent.F90

  MaplGrid.F90
  
  MaplGeneric.F90
  )

find_package(PFLOGGER REQUIRED)
find_package(GFTL REQUIRED)
find_package(GFTL_SHARED REQUIRED)

esma_add_library(${this} SRCS ${srcs} DEPENDENCIES MAPL.shared pflogger gftl-shared gftl)
target_include_directories (${this} PUBLIC
  $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
target_include_directories (${this} PUBLIC ${INC_ESMF} ${INC_NETCDF})
target_link_libraries (${this} PUBLIC ${ESMF_LIBRARIES} ${MPI_Fortran_LIBRARIES})

if (PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()

add_executable(demo driver.F90 UserComponent.F90)
target_link_libraries(demo PRIVATE ${this})
