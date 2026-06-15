esma_set_this (OVERRIDE MAPL.generic)

set(srcs
  # FieldDictionary* moved to infrastructure/field_dictionary/
  API.F90

  GenericGrid.F90

  ComponentSpecParser.F90

  UserSetServices.F90
  MethodPhasesMap.F90

  InnerMetaComponent.F90
  OuterMetaComponent.F90
  GenericGridComp.F90

  MAPL_Generic.F90
  MAPL3_Deprecated.F90

  # Validation.F90 moved to utils/

  # ComponentSpecBuilder.F90

  # Moved to infrastructure/esmf/
  # ESMF_HConfigUtilities.F90
  RestartHandler.F90

  RunEntryPoint.F90
  EntryPointVector.F90
  OpenMP_Support.F90

  # Umbrella module
  API.F90
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
  DEPENDENCIES MAPL.ESMF_Interfaces MAPL.enums MAPL.regridder_mgr MAPL.geom MAPL.vertical_grid MAPL.GeomIO
    MAPL.esmf MAPL.field MAPL.state MAPL.utils
    MAPL.vertical MAPL.component MAPL.field_bundle MAPL.esmf MAPL.utils
    MAPL.infrastructure
  ESMF::ESMF NetCDF::NetCDF_Fortran udunits2f PFLOGGER::pflogger GFTL_SHARED::gftl-shared-v2 GFTL::gftl-v2
  TYPE SHARED
  )

add_subdirectory(specs)
add_subdirectory(registry)
add_subdirectory(connection)
add_subdirectory(transforms)
add_subdirectory(couplers)
add_subdirectory(vertical)

esma_add_fortran_submodules(
  TARGET MAPL.generic
  SUBDIRECTORY OuterMetaComponent
  SOURCES SetServices.F90 add_child_by_spec.F90 new_outer_meta.F90 init_meta.F90
          get_num_children.F90 get_child_name.F90 get_child_by_name.F90
          run_child_by_name.F90 run_children.F90
          get_outer_meta_from_outer_gc.F90 attach_outer_meta.F90 free_outer_meta.F90
          get_phases.F90 set_hconfig.F90 get_hconfig.F90 has_geom.F90 get_geom.F90
	        advertise_variable.F90 initialize_advertise.F90 initialize_modify_advertised.F90
          initialize_realize.F90 initialize_read_restart.F90 recurse.F90
	        apply_to_children_custom.F90
          initialize_user.F90 run_custom.F90 run_user.F90
	        initialize_set_clock.F90 run_clock_advance.F90
	        initialize_geom_a.F90 initialize_geom_b.F90
          write_restart.F90 get_name.F90 get_gridcomp.F90
          set_geom.F90 set_vertical_grid.F90 get_vertical_grid.F90 get_registry.F90
          get_component_spec.F90 get_internal_state.F90 get_logger.F90
          get_user_gc_driver.F90 connect_all.F90 set_entry_point.F90
          finalize.F90 start_timer.F90 stop_timer.F90 get_checkpoint_dir.F90)

esma_add_fortran_submodules(
  TARGET MAPL.generic
  SUBDIRECTORY ComponentSpecParser
  SOURCES parse_child.F90 parse_children.F90 parse_connections.F90
          parse_var_specs.F90 parse_geometry_spec.F90 parse_component_spec.F90
          parse_setservices.F90 parse_timespec.F90
	  to_itemtype.F90)

target_include_directories (${this} PUBLIC
  $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)


if (PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()
