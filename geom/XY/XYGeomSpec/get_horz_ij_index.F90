#include "MAPL.h"

submodule (mapl3g_XYGeomSpec) get_horz_ij_index_smod
   use mapl_ErrorHandlingMod
   implicit none (type, external)

contains

   ! XY grids are arbitrary-projection grids; there is no analytic
   ! formula to invert (lon,lat) -> (i,j).  Return an error.
   module subroutine get_horz_ij_index_r4(this, lon, lat, ii, jj, rc)
      class(XYGeomSpec), intent(in) :: this
      real(R4), intent(in) :: lon(:)
      real(R4), intent(in) :: lat(:)
      integer, allocatable, intent(out) :: ii(:)
      integer, allocatable, intent(out) :: jj(:)
      integer, optional, intent(out) :: rc

      _FAIL('get_horz_ij_index not supported for XYGeomSpec')
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(lon)
      _UNUSED_DUMMY(lat)
      _UNUSED_DUMMY(ii)
      _UNUSED_DUMMY(jj)
   end subroutine get_horz_ij_index_r4

   module subroutine get_horz_ij_index_r8(this, lon, lat, ii, jj, rc)
      class(XYGeomSpec), intent(in) :: this
      real(R8), intent(in) :: lon(:)
      real(R8), intent(in) :: lat(:)
      integer, allocatable, intent(out) :: ii(:)
      integer, allocatable, intent(out) :: jj(:)
      integer, optional, intent(out) :: rc

      _FAIL('get_horz_ij_index not supported for XYGeomSpec')
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(lon)
      _UNUSED_DUMMY(lat)
      _UNUSED_DUMMY(ii)
      _UNUSED_DUMMY(jj)
   end subroutine get_horz_ij_index_r8

end submodule get_horz_ij_index_smod
