esma_set_this (OVERRIDE MAPL.generic3g)

set(srcs
  ESMF_Subset.F90
  Generic3g.F90

  FieldDictionaryItem.F90
  FieldDictionaryItemMap.F90
  FieldDictionary.F90

  GenericGrid.F90

  ComponentSpecParser.F90

  ESMF_Interfaces.F90
  UserSetServices.F90
  MethodPhasesMap.F90

  ComponentDriver.F90
  ComponentDriverVector.F90
  GriddedComponentDriver.F90
  GriddedComponentDriver_smod.F90
  GriddedComponentDriverMap.F90

  MultiState.F90
  InnerMetaComponent.F90
  OuterMetaComponent.F90
  OuterMetaComponent_smod.F90
  GenericPhases.F90
  GenericGridComp.F90

  MAPL_Generic.F90
  MAPL3_Deprecated.F90
  Validation.F90

  VerticalGeom.F90

  # ComponentSpecBuilder.F90

  ESMF_Utilities.F90
  ESMF_HConfigUtilities.F90
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
  DEPENDENCIES MAPL.regridder_mgr MAPL.geom_mgr MAPL.shared MAPL.profiler MAPL.base MAPL.hconfig_utils YAFYAML::yafyaml PFLOGGER::pflogger GFTL_SHARED::gftl-shared-v2 GFTL::gftl-v2
  TYPE SHARED
  )
add_subdirectory(specs)
add_subdirectory(registry)
add_subdirectory(connection)
add_subdirectory(actions)
add_subdirectory(couplers)
add_subdirectory(ComponentSpecParser)
add_subdirectory(ESMF_HConfigUtilities)

target_include_directories (${this} PUBLIC
  $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
target_link_libraries (${this} PUBLIC udunits2f MAPL.field_utils ESMF::ESMF NetCDF::NetCDF_Fortran)

if (PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()

