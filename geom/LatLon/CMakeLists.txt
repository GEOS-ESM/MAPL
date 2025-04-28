target_sources(MAPL.geom PRIVATE

  LonAxis.F90
  LatAxis.F90
  LatLonDecomposition.F90
  LatLonGeomSpec.F90
  LatLonGeomFactory.F90

)

esma_add_fortran_submodules(
  TARGET MAPL.geom
  SUBDIRECTORY LatLonDecomposition
  SOURCES get_subset.F90 get_idx_range.F90
          get_lon_subset.F90 get_lat_subset.F90
          make_LatLonDecomposition_current_vm.F90
          make_LatLonDecomposition_vm.F90 equal_to.F90)

esma_add_fortran_submodules(
  TARGET MAPL.geom
  SUBDIRECTORY LatLonGeomFactory
  SOURCES make_geom.F90 typesafe_make_geom.F90 create_basic_grid.F90
          fill_coordinates.F90 make_gridded_dims.F90
          make_file_metadata.F90 typesafe_make_file_metadata.F90)

esma_add_fortran_submodules(
  TARGET MAPL.geom
  SUBDIRECTORY LatLonGeomSpec
  SOURCES equal_to.F90 make_decomposition.F90 
          make_distribution.F90 supports_hconfig.F90
          make_LatLonGeomSpec_from_hconfig.F90
          supports_metadata.F90
          make_LatLonGeomSpec_from_metadata.F90)

esma_add_fortran_submodules(
  TARGET MAPL.geom
  SUBDIRECTORY LatAxis
  SOURCES supports_hconfig.F90
          supports_metadata.F90 make_LatAxis_from_hconfig.F90 
          make_lataxis_from_metadata.F90 get_lat_range.F90 get_lat_corners.F90 
          fix_bad_pole.F90)

esma_add_fortran_submodules(
  TARGET MAPL.geom
  SUBDIRECTORY LonAxis
  SOURCES get_lon_range.F90 make_LonAxis_from_metadata.F90
          supports_hconfig.F90 get_lon_corners.F90 make_LonAxis_from_hconfig.F90
          supports_metadata.F90)
