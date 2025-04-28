target_sources(MAPL.geom PRIVATE

  create_fields.F90
  destroy_fields.F90
  get_unit_vector.F90
  grid_get_centers.F90
  grid_get_coords_1d.F90
  grid_get_coords_2d.F90
  grid_get_corners.F90
  latlon2xyz.F90
  MAPL_GeomGetCoords.F90
  mid_pt_sphere.F90
  new_GridVectorBasis.F90
  new_NS_Basis.F90
  xyz2latlon.F90
)
