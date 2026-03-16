#include "MAPL_ErrLog.h"

! NullGeomSpec is used to return a concrete object fore failing
! factory methods that return GeomSpec objects.
module mapl3g_NullGeomSpec
   use mapl3g_GeomSpec
   use esmf, only: ESMF_KIND_R8
   use mapl_ErrorHandling
   implicit none(type,external)
   private

   public :: NULL_GEOM_SPEC

   type, extends(GeomSpec) :: NullGeomSpec
   contains
      procedure :: equal_to
      procedure :: get_horz_ij_index
   end type NullGeomSpec

   type(NullGeomSpec), protected :: NULL_GEOM_SPEC

contains

   logical function equal_to(a, b)
      class(NullGeomSpec), intent(in) :: a
      class(GeomSpec), intent(in) :: b
      equal_to = .false.
      _UNUSED_DUMMY(a)
      _UNUSED_DUMMY(b)
   end function equal_to

   subroutine get_horz_ij_index(this, ii, jj, lon, lat, lonR8, latR8, rc)
      class(NullGeomSpec), intent(in) :: this
      integer, allocatable, intent(out) :: ii(:)
      integer, allocatable, intent(out) :: jj(:)
      real, optional, intent(in) :: lon(:)
      real, optional, intent(in) :: lat(:)
      real(kind=ESMF_KIND_R8), optional, intent(in) :: lonR8(:)
      real(kind=ESMF_KIND_R8), optional, intent(in) :: latR8(:)
      integer, optional, intent(out) :: rc

      integer :: status

      allocate(ii(1), jj(1), source=-1)

      _FAIL('get_horz_ij_index is not supported for NullGeomSpec')

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(lon)
      _UNUSED_DUMMY(lat)
      _UNUSED_DUMMY(lonR8)
      _UNUSED_DUMMY(latR8)
   end subroutine get_horz_ij_index

end module mapl3g_NullGeomSpec
