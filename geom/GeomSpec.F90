#include "MAPL.h"

module mapl3g_GeomSpec

   use ESMF, only: ESMF_KIND_R4, ESMF_KIND_R8

   implicit none(type, external)
   private

   public :: GeomSpec

   type, abstract :: GeomSpec
      private
      character(:), allocatable :: name
   contains
      procedure(I_equal_to), deferred :: equal_to
      procedure(I_get_horz_ij_index_r4), deferred :: get_horz_ij_index_r4
      procedure(I_get_horz_ij_index_r8), deferred :: get_horz_ij_index_r8
      generic :: get_horz_ij_index => get_horz_ij_index_r4, get_horz_ij_index_r8
      generic :: operator(==) => equal_to

      procedure, non_overridable :: set_name
      procedure, non_overridable :: get_name
      procedure, non_overridable :: has_name
   end type GeomSpec

   abstract interface

      logical function I_equal_to(a, b)
         import GeomSpec
         class(GeomSpec), intent(in) :: a
         class(GeomSpec), intent(in) :: b
      end function I_equal_to

      subroutine I_get_horz_ij_index_r4(this, lon, lat, ii, jj, rc)
         import GeomSpec, ESMF_KIND_R4
         class(GeomSpec), intent(in) :: this
         real(kind=ESMF_KIND_R4), intent(in) :: lon(:)
         real(kind=ESMF_KIND_R4), intent(in) :: lat(:)
         integer, allocatable, intent(out) :: ii(:)
         integer, allocatable, intent(out) :: jj(:)
         integer, optional, intent(out) :: rc
      end subroutine I_get_horz_ij_index_r4

      subroutine I_get_horz_ij_index_r8(this, lon, lat, ii, jj, rc)
         import GeomSpec, ESMF_KIND_R8
         class(GeomSpec), intent(in) :: this
         real(kind=ESMF_KIND_R8), intent(in) :: lon(:)
         real(kind=ESMF_KIND_R8), intent(in) :: lat(:)
         integer, allocatable, intent(out) :: ii(:)
         integer, allocatable, intent(out) :: jj(:)
         integer, optional, intent(out) :: rc
      end subroutine I_get_horz_ij_index_r8
   end interface

contains

   subroutine set_name(this, name)
      class(GeomSpec), intent(inout) :: this
      character(*), intent(in) :: name
      this%name = name
   end subroutine set_name

   function get_name(this) result(name)
      class(GeomSpec), intent(in) :: this
      character(:), allocatable :: name
      name = ""
      if (allocated(this%name)) then
         name = this%name
      end if
   end function get_name

   logical function has_name(this)
      class(GeomSpec), intent(in) :: this
      has_name = allocated(this%name)
   end function has_name

end module mapl3g_GeomSpec
