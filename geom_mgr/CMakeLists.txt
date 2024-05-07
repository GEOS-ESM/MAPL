esma_set_this (OVERRIDE MAPL.geom_mgr)

set(srcs
  geom_mgr.F90 # package
  GeomUtilities.F90

  GeomSpec.F90
  NullGeomSpec.F90
  MaplGeom.F90
  MaplGeom/new_MaplGeom_MaplGeom.F90
  MaplGeom/set_id_MaplGeom.F90
  MaplGeom/get_spec_MaplGeom.F90
  MaplGeom/get_geom_MaplGeom.F90
  MaplGeom/get_factory_MaplGeom.F90
  MaplGeom/get_file_metadata_MaplGeom.F90
  MaplGeom/get_gridded_dims_MaplGeom.F90
  MaplGeom/get_basis_MaplGeom.F90
  #MaplGeom_smod.F90

  GeomFactory.F90

  CoordinateAxis.F90
  #CoordinateAxis_smod.F90
  CoordinateAxis/new_CoordinateAxis_CoordinateAxis.F90
  CoordinateAxis/equal_to_CoordinateAxis.F90
  CoordinateAxis/not_equal_to_CoordinateAxis.F90
  CoordinateAxis/get_extent_CoordinateAxis.F90
  CoordinateAxis/get_centers_CoordinateAxis.F90
  CoordinateAxis/get_corners_CoordinateAxis.F90
  CoordinateAxis/is_periodic_CoordinateAxis.F90
  CoordinateAxis/get_dim_name_CoordinateAxis.F90
  CoordinateAxis/get_coordinates_dim_CoordinateAxis.F90

  latlon/LonAxis.F90
  latlon/LonAxis_smod.F90
  latlon/LatAxis.F90
  latlon/LatAxis_smod.F90
  latlon/LatLonDecomposition.F90
  latlon/LatLonDecomposition_smod.F90
  latlon/LatLonGeomSpec.F90
  latlon/LatLonGeomSpec_smod.F90
  latlon/LatLonGeomFactory.F90
  latlon/LatLonGeomFactory_smod.F90

  GeomManager.F90
  GeomManager/new_GeomManager_GeomManager.F90
  GeomManager/initialize_GeomManager.F90
  GeomManager/add_factory_GeomManager.F90
  GeomManager/delete_mapl_geom_GeomManager.F90
  GeomManager/get_mapl_geom_from_hconfig_GeomManager.F90
  GeomManager/get_mapl_geom_from_metadata_GeomManager.F90
  GeomManager/get_mapl_geom_from_id_GeomManager.F90
  GeomManager/get_mapl_geom_from_spec_GeomManager.F90
  GeomManager/add_mapl_geom_GeomManager.F90
  GeomManager/make_geom_spec_from_metadata_GeomManager.F90
  GeomManager/make_geom_spec_from_hconfig_GeomManager.F90
  GeomManager/make_mapl_geom_from_spec_GeomManager.F90
  GeomManager/get_geom_from_id_GeomManager.F90
  #GeomManager_smod.F90

# gFTL containers
  GeomFactoryVector.F90
  GeomSpecVector.F90
  IntegerMaplGeomMap.F90

  VectorBasis.F90
  VectorBasis/create_fields_VectorBasis.F90
  VectorBasis/destroy_fields_VectorBasis.F90
  VectorBasis/get_unit_vector_VectorBasis.F90
  VectorBasis/grid_get_centers_VectorBasis.F90
  VectorBasis/grid_get_coords_1d_VectorBasis.F90
  VectorBasis/grid_get_coords_2d_VectorBasis.F90
  VectorBasis/grid_get_corners_VectorBasis.F90
  VectorBasis/latlon2xyz_VectorBasis.F90
  VectorBasis/MAPL_GeomGetCoords_VectorBasis.F90
  VectorBasis/mid_pt_sphere_VectorBasis.F90
  VectorBasis/new_GridVectorBasis_VectorBasis.F90
  VectorBasis/new_NS_Basis_VectorBasis.F90
  VectorBasis/xyz2latlon_VectorBasis.F90
  #VectorBasis_smod.F90
  )

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL.pfio MAPL.base MAPL.shared MAPL.field_utils MAPL.hconfig_utils GFTL::gftl-v2
  TYPE ${MAPL_LIBRARY_TYPE}
  )

target_include_directories (${this} PUBLIC
  $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
target_link_libraries (${this} PUBLIC ESMF::ESMF)

 if (PFUNIT_FOUND)
   add_subdirectory(tests EXCLUDE_FROM_ALL)
 endif ()

