target_sources(MAPL.geom_mgr PRIVATE

  get_geom_manager.F90
  new_GeomManager.F90
  initialize.F90
  add_factory.F90
  delete_mapl_geom.F90
  get_mapl_geom_from_spec.F90
  add_mapl_geom.F90
  find_factory.F90
  make_geom_spec_from_metadata.F90
  make_geom_spec_from_hconfig.F90
  make_mapl_geom_from_spec.F90
)
