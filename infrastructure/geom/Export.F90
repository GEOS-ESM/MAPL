! Export umbrella for the MAPL infrastructure/geom layer.
! Public API exposed to external consumers.
module mapl_geom_export

   use ESMF, only: ESMF_Grid, ESMF_Geom, ESMF_KIND_R4
   use mapl_KeywordEnforcer_mod
   use mapl_MaplGeom_mod, only: MaplGeom
   use mapl_GeomSpec_mod, only: GeomSpec
   use mapl_GeomManager_mod, only: GeomManager, geom_manager, get_geom_manager, get_mapl_geom
   use mapl_GeomUtilities_mod, only: mapl_SameGeom, mapl_GeomGetId
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
   use mapl_XYGeomSpec_mod, only: XYGeomSpec, make_XYGeomSpec, XY_COORD_STANDARD, XY_COORD_ABI
   use mapl_XYGeomFactory_mod, only: XYGeomFactory
   use mapl_CubedSphereGeomSpec_mod, only: CubedSphereGeomSpec, make_CubedSphereGeomSpec
   use mapl_CubedSphereDecomposition_mod, only: CubedSphereDecomposition, make_CubedSphereDecomposition

   implicit none
   private

   ! Available to users

   ! Used internally by MAPL

end module mapl_geom_export
