! Internal umbrella for the MAPL infrastructure/geom layer.
! Aggregates leaf modules from geom/ and its subdirectories for use by other MAPL subdirectories.
module mapl_geom_internal

   ! Core geom modules
   use mapl_GeomUtilities_mod
   use mapl_GeomSpec_mod
   use mapl_NullGeomSpec_mod
   use mapl_MaplGeom_mod
   use mapl_GeomFactory_mod
   use mapl_CoordinateAxis_mod
   use mapl_GeomManager_mod
   use mapl_GeomAccessors_mod
   use mapl_GridAccessors_mod
   use mapl_GridGetGlobal_mod
   use mapl_GridComms_mod
   use mapl_Subgrid_mod
   use mapl_VectorBasis_mod

   ! gFTL containers
   use mapl_GeomFactoryVector_mod
   use mapl_GeomSpecVector_mod
   use mapl_IntegerMaplGeomMap_mod

   ! XY
   use mapl_XYGeomSpec_mod
   use mapl_XYGeomFactory_mod

   ! CubedSphere
   use mapl_CubedSphereGeomSpec_mod
   use mapl_CubedSphereGeomFactory_mod
   use mapl_CubedSphereDecomposition_mod

   ! LatLon
   use mapl_LatLonGeomSpec_mod
   use mapl_LatLonGeomFactory_mod
   use mapl_LatLonDecomposition_mod
   use mapl_LatAxis_mod
   use mapl_LonAxis_mod

   ! EASE
   use mapl_EASEGeomSpec_mod
   use mapl_EASEGeomFactory_mod
   use mapl_EASEDecomposition_mod
   use mapl_EASECoords_mod
   use mapl_EASEConversion_mod

   ! LocStream
   use mapl_LocStreamGeomSpec_mod
   use mapl_LocStreamGeomFactory_mod
   use mapl_LocStreamDecomposition_mod

   ! Mesh (core modules only; raster_to_mesh is optional)
   use mapl_MeshGeomSpec_mod
   use mapl_MeshGeomFactory_mod
   use mapl_MeshDecomposition_mod

   implicit none

end module mapl_geom_internal
