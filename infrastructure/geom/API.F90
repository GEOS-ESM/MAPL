! Export umbrella for the MAPL infrastructure/geom layer.
! Public API exposed to external consumers.
module mapl_geom_api

   use mapl_MaplGeom_mod, only: MaplGeom
   use mapl_GeomSpec_mod, only: GeomSpec
   use mapl_GeomManager_mod, only: GeomManager, geom_manager, get_geom_manager, get_mapl_geom
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
   use mapl_CubedSphereGeomSpec_mod, only: CubedSphereGeomSpec, make_CubedSphereGeomSpec
   use mapl_CubedSphereDecomposition_mod, only: CubedSphereDecomposition, make_CubedSphereDecomposition

   ! need to delete
   use mapl_Subgrid_mod, only: Interval
   use mapl_Subgrid_mod, only: find_bounds

   implicit none
   private

   ! Geom types and manager
   public :: MaplGeom
   public :: GeomSpec
   public :: GeomManager
   public :: geom_manager
   public :: get_geom_manager
   public :: get_mapl_geom

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
   public :: CubedSphereGeomSpec
   public :: make_CubedSphereGeomSpec
   public :: CubedSphereDecomposition
   public :: make_CubedSphereDecomposition


   ! Delete later
   public :: Interval
   public :: find_bounds
end module mapl_geom_api
