esma_set_this (OVERRIDE MAPL.generic3g)

set(srcs
  ESMF_Subset.F90
  Generic3g.F90

  FieldDictionaryItem.F90
  FieldDictionaryItemMap.F90
  FieldDictionary.F90

  GenericGrid.F90

  ComponentSpecParser.F90
  ComponentBuilder.F90

  ESMF_Interfaces.F90
  UserSetServices.F90
  MethodPhasesMap.F90

  ChildComponent.F90
  ChildComponent_run_smod.F90
  ChildComponentMap.F90
#  GenericCouplerComponent.F90
#  CouplerComponentVector.F90

  MultiState.F90
  InnerMetaComponent.F90
  OuterMetaComponent.F90
  OuterMetaComponent_setservices_smod.F90
  OuterMetaComponent_addChild_smod.F90
  GenericPhases.F90
  GenericGridComp.F90

  MAPL_Generic.F90
  Validation.F90

  VerticalGeom.F90

  # ComponentSpecBuilder.F90

  ESMF_Utilities.F90
  )
# Workaround for strict NAG Fortran with ESMF implicit interface for private state.
#set_property( SOURCE InnerMetaComponent.F90 OuterMetaComponent.F90
#  PROPERTY COMPILE_FLAGS ${MISMATCH})

list (APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}")

find_package (MPI REQUIRED)
find_package (GFTL REQUIRED)
find_package (GFTL_SHARED REQUIRED)
find_package (YAFYAML REQUIRED)
if (BUILD_WITH_PFLOGGER)
  find_package (PFLOGGER REQUIRED)
endif ()

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL.regridder_mgr MAPL.geom_mgr MAPL.shared MAPL.profiler MAPL.base YAFYAML::yafyaml PFLOGGER::pflogger GFTL_SHARED::gftl-shared-v2 GFTL::gftl-v2
  TYPE ${MAPL_LIBRARY_TYPE}
  )
add_subdirectory(specs)
add_subdirectory(registry)
add_subdirectory(connection)
add_subdirectory(actions)

target_include_directories (${this} PUBLIC
  $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
target_link_libraries (${this} PUBLIC MAPL.field_utils esmf NetCDF::NetCDF_Fortran)

# Workaround for ifort 2010.10.0 Explicit deep copy in some containers
# result in odd error in GEOM_Manager.  This is not sufficient on its
# own, and some constructors had to be converted to
# subroutines. (ComponentSpecParser.F90)
  target_compile_definitions(${this} PRIVATE "__gftl_map_use_default_assignment")
  target_compile_definitions(${this} PRIVATE "__gftl_set_use_default_assignment")


if (PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()

