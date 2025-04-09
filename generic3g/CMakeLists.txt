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
  ComponentDriverPtrVector.F90
  GriddedComponentDriver.F90
  GriddedComponentDriverMap.F90

  MultiState.F90
  InnerMetaComponent.F90
  OuterMetaComponent.F90
  GenericPhases.F90
  GenericGridComp.F90

  MAPL_Generic.F90
  MAPL3_Deprecated.F90
  Validation.F90

  # ComponentSpecBuilder.F90

  ESMF_Utilities.F90

  ESMF_HConfigUtilities.F90
  RestartHandler.F90
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
  DEPENDENCIES MAPL.regridder_mgr MAPL.geom_mgr MAPL.GeomIO MAPL.esmf_utils MAPL.field MAPL.state MAPL.shared MAPL.profiler MAPL.base MAPL.hconfig_utils
  ESMF::ESMF NetCDF::NetCDF_Fortran udunits2f PFLOGGER::pflogger GFTL_SHARED::gftl-shared-v2 GFTL::gftl-v2 MAPL.esmf_utils
  TYPE SHARED
  )

add_subdirectory(specs)
add_subdirectory(registry)
add_subdirectory(connection)
add_subdirectory(transforms)
add_subdirectory(couplers)
add_subdirectory(vertical)

esma_add_fortran_submodules(
  TARGET MAPL.generic3g
  SUBDIRECTORY OuterMetaComponent
  SOURCES SetServices.F90 add_child_by_spec.F90 new_outer_meta.F90 init_meta.F90
          get_child_by_name.F90 run_child_by_name.F90 run_children.F90
          get_outer_meta_from_outer_gc.F90 attach_outer_meta.F90 free_outer_meta.F90
          get_phases.F90 set_hconfig.F90 get_hconfig.F90 has_geom.F90 get_geom.F90
          initialize_advertise.F90
	  initialize_modify_advertised.F90 initialize_modify_advertised2.F90
          initialize_realize.F90 initialize_read_restart.F90 recurse.F90
	  apply_to_children_custom.F90
          initialize_user.F90 run_custom.F90 run_user.F90
	  initialize_set_clock.F90 run_clock_advance.F90
	  initialize_geom_a.F90 initialize_geom_b.F90
          write_restart.F90 get_name.F90 get_gridcomp.F90
          set_geom.F90 set_vertical_grid.F90 get_vertical_grid.F90 get_registry.F90
          get_component_spec.F90 get_internal_state.F90 get_lgr.F90
          get_user_gc_driver.F90 connect_all.F90 set_entry_point.F90
          finalize.F90)

esma_add_fortran_submodules(
  TARGET MAPL.generic3g
  SUBDIRECTORY ComponentSpecParser
  SOURCES parse_child.F90 parse_children.F90 parse_connections.F90
          parse_var_specs.F90 parse_geometry_spec.F90 parse_component_spec.F90
          parse_setservices.F90 parse_timespec.F90)

esma_add_fortran_submodules(
  TARGET MAPL.generic3g
  SUBDIRECTORY ESMF_HConfigUtilities
  SOURCES MAPL_HConfigMatch.F90
          write_hconfig.F90)

esma_add_fortran_submodules(
  TARGET MAPL.generic3g
  SUBDIRECTORY GriddedComponentDriver
  SOURCES initialize.F90 run.F90 finalize.F90 get_states.F90
          get_clock.F90 set_clock.F90 run_export_couplers.F90
          run_import_couplers.F90 clock_advance.F90
          get_gridcomp.F90 get_name.F90 add_export_coupler.F90
          add_import_coupler.F90 write_restart.F90)

target_include_directories (${this} PUBLIC
  $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

if(ESMF_HCONFIGSET_HAS_INTENT_INOUT)
  target_compile_definitions(${this} PRIVATE ESMF_HCONFIGSET_HAS_INTENT_INOUT)
endif()

if (PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()
