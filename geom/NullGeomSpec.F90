#include "MAPL.h"

! NullGeomSpec is used to return a concrete object fore failing
! factory methods that return GeomSpec objects.
module mapl3g_NullGeomSpec

   use mapl3g_GeomSpec
   use esmf, only: ESMF_KIND_R4, ESMF_KIND_R8
   use mapl_ErrorHandling

   implicit none(type, external)
   private

   public :: NULL_GEOM_SPEC

   type, extends(GeomSpec) :: NullGeomSpec
   contains
      procedure :: equal_to
      procedure :: get_horz_ij_index_r4
      procedure :: get_horz_ij_index_r8
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

   subroutine get_horz_ij_index_r4(this, lon, lat, ii, jj, rc)
      class(NullGeomSpec), intent(in) :: this
      real(kind=ESMF_KIND_R4), intent(in) :: lon(:)
      real(kind=ESMF_KIND_R4), intent(in) :: lat(:)
      integer, allocatable, intent(out) :: ii(:)
      integer, allocatable, intent(out) :: jj(:)
      integer, optional, intent(out) :: rc

      integer :: status

      allocate(ii(1), jj(1), source=-1)

      _FAIL('get_horz_ij_index is not supported for NullGeomSpec')

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(lon)
      _UNUSED_DUMMY(lat)
   end subroutine get_horz_ij_index_r4

   subroutine get_horz_ij_index_r8(this, lon, lat, ii, jj, rc)
      class(NullGeomSpec), intent(in) :: this
      real(kind=ESMF_KIND_R8), intent(in) :: lon(:)
      real(kind=ESMF_KIND_R8), intent(in) :: lat(:)
      integer, allocatable, intent(out) :: ii(:)
      integer, allocatable, intent(out) :: jj(:)
      integer, optional, intent(out) :: rc

      integer :: status

      allocate(ii(1), jj(1), source=-1)

      _FAIL('get_horz_ij_index is not supported for NullGeomSpec')

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(lon)
      _UNUSED_DUMMY(lat)
   end subroutine get_horz_ij_index_r8

end module mapl3g_NullGeomSpec
