! Export umbrella for the MAPL infrastructure/geom layer.
! Public API exposed to external consumers.
module mapl_geom_api

   use mapl_MaplGeom_mod, only: mapl_MaplGeom => MaplGeom
   use mapl_GeomSpec_mod, only: mapl_GeomSpec => GeomSpec
   use mapl_GeomManager_mod, only: mapl_GeomManager => GeomManager
   use mapl_GeomManager_mod, only: mapl_get_geom_manager => get_geom_manager
   use mapl_GeomManager_mod, only: mapl_get_mapl_geom => get_mapl_geom
   use mapl_GeomUtilities_mod, only: mapl_SameGeom => SameGeom, mapl_GeomGetId => GeomGetId
   use mapl_GeomAccessors_mod, only: mapl_GeomGet => GeomGet
   use mapl_GeomAccessors_mod, only: mapl_GeomGetHorzIJIndex => GeomGetHorzIJIndex
   use mapl_GeomAccessors_mod, only: mapl_GridGetHorzIJIndex => GridGetHorzIJIndex
   use mapl_GridAccessors_mod, only: mapl_GridGet => GridGet
   use mapl_GridAccessors_mod, only: mapl_GridGetCoordinates => GridGetCoordinates
   use mapl_GridAccessors_mod, only: mapl_GridHasDE => grid_has_DE
   use mapl_GridGetGlobal_mod, only: mapl_GridGetGlobalCellCountPerDim => GridGetGlobalCellCountPerDim
   use mapl_GridComms_mod, only: MAPL_CollectiveGather3D => mapl_CollectiveGather3D
   use mapl_GridComms_mod, only: MAPL_CollectiveScatter3D => mapl_CollectiveScatter3D
   use mapl_Subgrid_mod, only: mapl_Interval => Interval
   use mapl_Subgrid_mod, only: mapl_make_subgrids => make_subgrids
   use mapl_Subgrid_mod, only: mapl_find_bounds => find_bounds
   use mapl_CubedSphereGeomSpec_mod, only: mapl_CubedSphereGeomSpec => CubedSphereGeomSpec
   use mapl_CubedSphereGeomSpec_mod, only: mapl_make_CubedSphereGeomSpec => make_CubedSphereGeomSpec
   use mapl_CubedSphereDecomposition_mod, only: mapl_CubedSphereDecomposition => CubedSphereDecomposition
   use mapl_CubedSphereDecomposition_mod, only: mapl_make_CubedSphereDecomposition => make_CubedSphereDecomposition

   implicit none
   private

   ! Geom types and manager
   public :: mapl_MaplGeom
   public :: mapl_GeomSpec
   public :: mapl_GeomManager
   public :: mapl_get_geom_manager
   public :: mapl_get_mapl_geom

   ! Geom utilities
   public :: mapl_SameGeom
   public :: mapl_GeomGetId
   public :: mapl_GeomGet
   public :: mapl_GeomGetHorzIJIndex
   public :: mapl_GridGetHorzIJIndex
   public :: mapl_GridGet
   public :: mapl_GridGetCoordinates
   public :: mapl_GridHasDE
   public :: mapl_GridGetGlobalCellCountPerDim

   ! Collective comms
   public :: MAPL_CollectiveGather3D
   public :: MAPL_CollectiveScatter3D

   ! Subgrid
   public :: mapl_Interval
   public :: mapl_make_subgrids

   ! CubedSphere geom specs
   public :: mapl_CubedSphereGeomSpec
   public :: mapl_make_CubedSphereGeomSpec
   public :: mapl_CubedSphereDecomposition
   public :: mapl_make_CubedSphereDecomposition

end module mapl_geom_api
