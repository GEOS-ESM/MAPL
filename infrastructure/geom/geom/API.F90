module mapl3g_Geom_API

   use mapl_KeywordEnforcer
   use mapl3g_MaplGeom, only: MaplGeom
   use mapl3g_GeomSpec, only: GeomSpec
   use mapl3g_GeomManager, only: GeomManager, geom_manager, get_geom_manager, get_mapl_geom
   use mapl3g_GeomUtilities, only: mapl_SameGeom, mapl_GeomGetId
   use mapl3g_GeomGet, only: mapl_GeomGet => GeomGet
   use mapl3g_GridGet, only: mapl_GridGet => GridGet, mapl_GridGetCoordinates => GridGetCoordinates, &
        mapl_GridHasDE => grid_has_DE
   use mapl3g_GridGetHorzIJIndex, only: mapl_GridGetHorzIJIndex => GridGetHorzIJIndex
   use mapl3g_GridGetGlobal, only: mapl_GridGetGlobalCellCountPerDim => GridGetGlobalCellCountPerDim
   use mapl3g_GeomGetHorzIJIndex, only: mapl_GeomGetHorzIJIndex => GeomGetHorzIJIndex
   use mapl3g_Subgrid, only: mapl_Interval => Interval, mapl_make_subgrids => make_subgrids
   use mapl3g_XYGeomSpec,    only: XYGeomSpec, make_XYGeomSpec, XY_COORD_STANDARD, XY_COORD_ABI
   use mapl3g_XYGeomFactory, only: XYGeomFactory
   use mapl3g_CubedSphereGeomSpec, only: CubedSphereGeomSpec, make_CubedSphereGeomSpec
   use mapl3g_CubedSphereDecomposition, only: CubedSphereDecomposition, make_CubedSphereDecomposition
   use esmf, only: ESMF_Grid, ESMF_Geom, ESMF_KIND_R4

   implicit none(type,external)

   private

   ! Available to users
   public :: mapl_GeomGet
   public :: mapl_GridGet
   public :: mapl_GridGetCoordinates
   public :: mapl_GridGetHorzIJIndex, mapl_GeomGetHorzIJIndex
   public :: mapl_GridGetGlobalCellCountPerDim
   public :: mapl_GridHasDE

   ! Used internally by MAPL
   ! Users shouldn't need these
   public :: MaplGeom
   public :: mapl_SameGeom, mapl_GeomGetId
   public :: GeomManager, geom_manager, get_geom_manager, get_mapl_geom
   public :: GeomSpec
   public :: mapl_Interval, mapl_make_subgrids
   public :: XYGeomSpec, make_XYGeomSpec, XY_COORD_STANDARD, XY_COORD_ABI
   public :: XYGeomFactory
   public :: CubedSphereGeomSpec, make_CubedSphereGeomSpec
   public :: CubedSphereDecomposition, make_CubedSphereDecomposition

end module mapl3g_Geom_API
