target_sources(MAPL.geom_mgr PRIVATE

  new_GeomManager_GeomManager.F90
  initialize_GeomManager.F90
  add_factory_GeomManager.F90
  delete_mapl_geom_GeomManager.F90
  get_mapl_geom_from_hconfig_GeomManager.F90
  get_mapl_geom_from_metadata_GeomManager.F90
  get_mapl_geom_from_id_GeomManager.F90
  get_mapl_geom_from_spec_GeomManager.F90
  add_mapl_geom_GeomManager.F90
  make_geom_spec_from_metadata_GeomManager.F90
  make_geom_spec_from_hconfig_GeomManager.F90
  make_mapl_geom_from_spec_GeomManager.F90
  get_geom_from_id_GeomManager.F90
)
