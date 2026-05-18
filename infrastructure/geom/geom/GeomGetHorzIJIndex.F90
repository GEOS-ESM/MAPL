#include "MAPL.h"

module mapl3g_GeomGetHorzIJIndex

   use ESMF, only: ESMF_Geom, ESMF_KIND_R4, ESMF_KIND_R8
   use mapl3g_MaplGeom, only: MaplGeom
   use mapl3g_GeomSpec, only: GeomSpec
   use mapl3g_GeomManager, only: get_mapl_geom
   use MAPL_ErrorHandling, only: MAPL_Verify, MAPL_Return

   implicit none(type, external)
   private

   public :: GeomGetHorzIJIndex

   interface GeomGetHorzIJIndex
      module procedure get_horz_ij_index_r4
      module procedure get_horz_ij_index_r8
   end interface GeomGetHorzIJIndex

contains

   subroutine get_horz_ij_index_r4(geom, lon, lat, ii, jj, rc)
      type(ESMF_Geom), intent(in) :: geom
      real(kind=ESMF_KIND_R4), intent(in) :: lon(:)
      real(kind=ESMF_KIND_R4), intent(in) :: lat(:)
      integer, allocatable, intent(out) :: ii(:)
      integer, allocatable, intent(out) :: jj(:)
      integer, optional, intent(out) :: rc

      type(MaplGeom), pointer :: mapl_geom
      class(GeomSpec), allocatable :: spec
      integer :: status

      mapl_geom => get_mapl_geom(geom, _RC)
      spec = mapl_geom%get_spec()

      call spec%get_horz_ij_index(lon=lon, lat=lat, ii=ii, jj=jj, _RC)

      _RETURN(_SUCCESS)
   end subroutine get_horz_ij_index_r4

   subroutine get_horz_ij_index_r8(geom, lon, lat, ii, jj, rc)
      type(ESMF_Geom), intent(in) :: geom
      real(kind=ESMF_KIND_R8), intent(in) :: lon(:)
      real(kind=ESMF_KIND_R8), intent(in) :: lat(:)
      integer, allocatable, intent(out) :: ii(:)
      integer, allocatable, intent(out) :: jj(:)
      integer, optional, intent(out) :: rc

      type(MaplGeom), pointer :: mapl_geom
      class(GeomSpec), allocatable :: spec
      integer :: status

      mapl_geom => get_mapl_geom(geom, _RC)
      spec = mapl_geom%get_spec()

      call spec%get_horz_ij_index(lon=lon, lat=lat, ii=ii, jj=jj, _RC)

      _RETURN(_SUCCESS)
   end subroutine get_horz_ij_index_r8

end module mapl3g_GeomGetHorzIJIndex
