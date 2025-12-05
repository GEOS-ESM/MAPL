module mapl3g_Geom_API

   use mapl_KeywordEnforcer
   use mapl3g_MaplGeom, only: MaplGeom
   use mapl3g_GeomSpec, only: GeomSpec
   use mapl3g_GeomManager, only: GeomManager, geom_manager, get_geom_manager, get_mapl_geom
   use mapl3g_GeomUtilities, only: MAPL_SameGeom, MAPL_GeomGetId
   use esmf, only: ESMF_Grid, ESMF_Geom, ESMF_KIND_R4

   implicit none(type,external)

   private

   ! Available to users
   public :: MAPL_GridGet
   public :: MAPL_GeomGet

   ! Used internally by MAPL
   ! Users shouldn't need these
   public :: MaplGeom
   public :: MAPL_SameGeom, MAPL_GeomGetId
   public :: GeomManager, geom_manager, get_geom_manager, get_mapl_geom
   public :: GeomSpec

   interface MAPL_GridGet
      procedure :: grid_get
   end interface MAPL_GridGet

   interface MAPL_GeomGet
      procedure :: geom_get
   end interface MAPL_GeomGet

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

      module subroutine geom_get(geom, topology, rc)
         type(ESMF_Geom), intent(in) :: geom
         integer, allocatable, optional, intent(out) :: topology(:)
         integer, optional, intent(out) :: rc
      end subroutine geom_get

   end interface

end module mapl3g_Geom_API
