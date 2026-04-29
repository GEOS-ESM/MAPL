target_sources(MAPL.geom PRIVATE

  EASEConversion.F90
  EASECoords.F90
  EASEDecomposition.F90
  EASEGeomSpec.F90
  EASEGeomFactory.F90

)

esma_add_fortran_submodules(
  TARGET MAPL.geom
  SUBDIRECTORY EASEGeomSpec
  SOURCES equal_to.F90
          supports_hconfig.F90
          supports_metadata.F90
          make_EASEGeomSpec_from_hconfig.F90
          make_EASEGeomSpec_from_metadata.F90
          get_horz_ij_index.F90)

esma_add_fortran_submodules(
  TARGET MAPL.geom
  SUBDIRECTORY EASEGeomFactory
  SOURCES make_geom.F90 typesafe_make_geom.F90
          create_basic_grid.F90 fill_coordinates.F90
          make_gridded_dims.F90 make_variable_attributes.F90
          make_file_metadata.F90 typesafe_make_file_metadata.F90)
