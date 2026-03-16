#include "MAPL.h"

module mapl3g_GeomGetHorzIJIndex

   use ESMF, only: ESMF_Geom, ESMF_KIND_R8
   use mapl3g_MaplGeom, only: MaplGeom
   use mapl3g_GeomSpec, only: GeomSpec
   use mapl3g_GeomManager, only: get_mapl_geom
   use MAPL_ErrorHandling, only: MAPL_Verify, MAPL_Return

   implicit none (type, external)
   private

   public :: GeomGetHorzIJIndex

   interface GeomGetHorzIJIndex
      module procedure get_horz_ij_index
   end interface GeomGetHorzIJIndex

contains

   subroutine get_horz_ij_index(geom, npts, ii, jj, lon, lat, lonR8, latR8, rc)
      type(ESMF_Geom), intent(in) :: geom
      integer, intent(in) :: npts
      integer, intent(out) :: ii(npts)
      integer, intent(out) :: jj(npts)
      real, optional, intent(in) :: lon(npts)
      real, optional, intent(in) :: lat(npts)
      real(kind=ESMF_KIND_R8), optional, intent(in) :: lonR8(npts)
      real(kind=ESMF_KIND_R8), optional, intent(in) :: latR8(npts)
      integer, optional, intent(out) :: rc

      type(MaplGeom), pointer :: mapl_geom
      class(GeomSpec), allocatable :: spec
      integer :: status

      mapl_geom => get_mapl_geom(geom, _RC)
      spec = mapl_geom%get_spec()

      call spec%get_horz_ij_index(npts, ii, jj, lon=lon, lat=lat, lonR8=lonR8, latR8=latR8, _RC)

      _RETURN(_SUCCESS)
    end subroutine get_horz_ij_index

end module mapl3g_GeomGetHorzIJIndex
