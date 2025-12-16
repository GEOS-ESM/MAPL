module mapl3g_Geom_API

   use mapl_KeywordEnforcer
   use mapl3g_MaplGeom, only: MaplGeom
   use mapl3g_GeomSpec, only: GeomSpec
   use mapl3g_GeomManager, only: GeomManager, geom_manager, get_geom_manager, get_mapl_geom
   use mapl3g_GeomUtilities, only: mapl_SameGeom, mapl_GeomGetId
   use mapl3g_GridGet, only: mapl_GridGet => GridGet, mapl_GridGetCoordinates => GridGetCoordinates
   use esmf, only: ESMF_Grid, ESMF_Geom, ESMF_KIND_R4

   implicit none(type,external)

   private

   ! Available to users
   public :: mapl_GridGet
   public :: mapl_GridGetCoordinates

   ! Used internally by MAPL
   ! Users shouldn't need these
   public :: MaplGeom
   public :: mapl_SameGeom, mapl_GeomGetId
   public :: GeomManager, geom_manager, get_geom_manager, get_mapl_geom
   public :: GeomSpec

end module mapl3g_Geom_API
