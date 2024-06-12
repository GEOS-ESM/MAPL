target_sources(MAPL.generic3g PRIVATE

     SetServices.F90
     add_child_by_name.F90
     new_outer_meta.F90
     init_meta.F90

     get_child_by_name.F90
     run_child_by_name.F90
     run_children.F90
     get_outer_meta_from_outer_gc.F90
     attach_outer_meta.F90
     free_outer_meta.F90

     get_phases.F90
     set_hconfig.F90
     get_hconfig.F90
     get_geom.F90

     initialize_advertise_geom.F90
     initialize_realize_geom.F90
     initialize_advertise.F90
     initialize_post_advertise.F90
     initialize_realize.F90

     recurse.F90
     apply_to_children_custom.F90
     initialize_user.F90
     run_custom.F90
     run_user.F90
     run_clock_advance.F90

     read_restart.F90
     write_restart.F90
     get_name.F90
     get_gridcomp.F90
     set_geom.F90
     set_vertical_geom.F90
     get_registry.F90

     get_component_spec.F90
     get_internal_state.F90
     get_lgr.F90
     get_user_gc_driver.F90
     connect_all.F90
     set_entry_point.F90
)

mapl_add_fortran_submodules(TARGET A SUBDIRECTORY A SRCS finalize.F90)
