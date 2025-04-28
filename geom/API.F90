module mapl3g_Geom_API

   use mapl_KeywordEnforcer
   use mapl3g_MaplGeom, only: MaplGeom
   use mapl3g_GeomSpec, only: GeomSpec
   use mapl3g_GeomManager, only: GeomManager, geom_manager, get_geom_manager
   use mapl3g_GeomUtilities, only: MAPL_SameGeom, MAPL_GeomGetId
   use esmf, only: ESMF_Grid, ESMF_KIND_R4, ESMF_KIND_R8

   implicit none

   private

   ! Available to users
   public :: MAPL_GridGet

   ! Used internally by MAPL
   ! Users shouldn't need these
   public :: MaplGeom
   public :: MAPL_SameGeom, MAPL_GeomGetId
   public :: GeomManager, geom_manager, get_geom_manager
   public :: GeomSpec

   interface MAPL_GridGet
      procedure ::  grid_get
   end interface MAPL_GridGet

   interface
      module subroutine grid_get(grid, unusable, im, jm, latitudes, longitudes, rc)
         type(ESMF_Grid), intent(in) :: grid
         class(KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(out) :: im
         integer, optional, intent(out) :: jm
         real(kind=ESMF_KIND_R4), optional, pointer, intent(out) :: latitudes(:,:)
         real(kind=ESMF_KIND_R4), optional, pointer, intent(out) :: longitudes(:,:)
         integer, optional, intent(out) :: rc
      end subroutine grid_get
   end interface

end module mapl3g_Geom_API
