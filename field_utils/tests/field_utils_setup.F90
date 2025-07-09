#include "MAPL_Generic.h"

module field_utils_setup

   use ESMF
   use funit
   use MAPL_ExceptionHandling

   implicit none

   interface mk_field
      module procedure mk_field_r4_2d
      module procedure mk_field_r8_2d
   end interface mk_field

   interface initialize_array
      module procedure initialize_array_r4
      module procedure initialize_array_r8
   end interface

   integer :: i
   type(ESMF_Index_Flag), parameter :: INDEX_FLAG_DEFAULT = ESMF_INDEX_DELOCAL
!   integer, parameter :: REG_DECOMP_DEFAULT(*) = [2, 2] !wdb delete
!   integer, parameter :: MAX_INDEX_DEFAULT(*) = [2, 2] !wdb delete
!   integer, parameter :: MIN_INDEX_DEFAULT(*) = [1, 1] !wdb delete
!   integer, parameter :: DIMR4_DEFAULT(*) = [4, 4] !wdb delete
!   integer, parameter :: DIMR8_DEFAULT(*) = [4, 4] !wdb delete
!   integer, parameter :: SIZE_R4 = 16 !wdb delete
!   integer, parameter :: SIZE_R8 = 16 !wdb delete
   real, parameter :: undef = 42.0

   real(kind=ESMF_KIND_R4), parameter :: R4_ARRAY_DEFAULT(*,*) = reshape([(i, i = 1, 4)], [2,2])
   real(kind=ESMF_KIND_R8), parameter :: R8_ARRAY_DEFAULT(*,*) = reshape([(i, i = 1, 4)], [2,2])

   type(ESMF_Field) :: XR4
   type(ESMF_Field) :: XR8
   type(ESMF_Field) :: YR4
   type(ESMF_Field) :: YR8
   type(ESMF_Field) :: XR4_3D
   type(ESMF_Field) :: XR8_3D
   type(ESMF_Field) :: YR4_3D
   type(ESMF_Field) :: YR8_3D

contains

   ! MAKE GRID FOR FIELDS
   function mk_grid(grid_name, rc) result(grid)
      character(len=*), intent(in) :: grid_name
      integer, optional, intent(out) :: rc

      type(ESMF_Grid) :: grid

      integer :: status

      grid = ESMF_GridCreateNoPeriDim(countsPerDeDim1=[2,2], countsPerDeDim2=[2,2], indexflag=INDEX_FLAG_DEFAULT, name = grid_name, _rc)

      _return(_success)
   end function mk_grid

   function mk_field_r4_ungrid(name, ungriddedLBound, ungriddedUBound, rc) result(field)
      character(len=*), intent(in) :: name
      integer, optional, intent(in) :: ungriddedLBound(:)
      integer, optional, intent(in) :: ungriddedUBound(:)
      integer, optional, intent(out) :: rc

      type(ESMF_Field) :: field

      integer :: status

      field = mk_field_common(tk = ESMF_TYPEKIND_R4, name = name, ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, _rc)

      _return(_success)
   end function mk_field_r4_ungrid

   function mk_field_r4_2d(farray, name, rc) result(field)
      real(kind=ESMF_KIND_R4), dimension(:,:), target, intent(in) :: farray
      character(len=*), intent(in) :: name
      integer, optional, intent(out) :: rc

      type(ESMF_Field) :: field
      real(kind=ESMF_KIND_R4), dimension(:,:), pointer :: ptr

      integer :: status

      field = mk_field_common(tk = ESMF_TYPEKIND_R4, name = name, _rc)
      call ESMF_FieldGet(field, farrayPtr = ptr, _rc)
      
      ptr = farray

      _return(_success)
   end function mk_field_r4_2d

   function mk_field_r8_2d(farray, name, rc) result(field)
      real(kind=ESMF_KIND_R8), dimension(:,:), target, intent(in) :: farray
      character(len=*), intent(in) :: name
      integer, optional, intent(out) :: rc

      type(ESMF_Field) :: field
      real(kind=ESMF_KIND_R8), dimension(:,:), pointer :: ptr

      integer :: status

      field = mk_field_common(tk = ESMF_TYPEKIND_R8, name = name, _rc)
      call ESMF_FieldGet(field, farrayPtr = ptr, _rc)
      ptr = farray

      _return(_success)
   end function mk_field_r8_2d

   function mk_field_common(tk, name, ungriddedLBound, ungriddedUBound, rc) result(field)
      type(ESMF_TypeKind_Flag), intent(in) :: tk
      character(len=*), intent(in) :: name
      integer, optional, intent(in) :: ungriddedLBound(:)
      integer, optional, intent(in) :: ungriddedUBound(:)
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: GRID_SUFFIX = '_grid'
      character(len=*), parameter :: FIELD_SUFFIX = '_field'

      type(ESMF_Field) :: field
      type(ESMF_Grid) :: grid
      integer :: status
      
      grid = mk_grid(grid_name = name // GRID_SUFFIX, _rc)
      field = ESMF_FieldCreate(grid, typekind = tk, name = name // FIELD_SUFFIX, ungriddedLBound = ungriddedLBound, ungriddedUBound = ungriddedUBound, _rc)

      _return(_success)
   end function mk_field_common

   elemental function are_almost_equal(x, y) result(almost_equal)
      real(kind=ESMF_KIND_R4), parameter :: EPS = epsilon(real(1.0, kind=ESMF_KIND_R4))
      real(kind=ESMF_KIND_R8), intent(in) :: x
      real(kind=ESMF_KIND_R4), intent(in) :: y
      logical :: almost_equal

      if(y==0) then
         almost_equal = (x==0)
      else
         almost_equal = (abs(x-y)/abs(y) < EPS)
      end if

   end function are_almost_equal

   subroutine initialize_array_R4(x, xmin, xrange)
      real(ESMF_KIND_R4), intent(inout) :: x(:,:)
      real(ESMF_KIND_R4), intent(in) :: xmin
      real(ESMF_KIND_R4), intent(in) :: xrange
      integer :: rc

      _assert(xrange > 0, 'Range for random numbers must be positive.')
      call random_number(x)
      x = xrange * x + xmin

   end subroutine initialize_array_R4

   subroutine initialize_array_R8(x, xmin, xrange)
      real(ESMF_KIND_R8), intent(inout) :: x(:,:)
      real(ESMF_KIND_R8), intent(in) :: xmin
      real(ESMF_KIND_R8), intent(in) :: xrange
      integer :: rc

      _assert(xrange > 0, 'Range for random numbers must be positive.')
      call random_number(x)
      x = xrange * x + xmin

   end subroutine initialize_array_R8

   function mk_r4field(r4array, field_name, rc) result(r4field)
      type(ESMF_Field) :: r4field
      real(kind=ESMF_KIND_R4), intent(in) :: r4array(:,:)
      character(len=*), intent(in) :: field_name
      integer, optional, intent(out) :: rc

      integer :: status

      r4field = mk_field(r4array, name = field_name, _rc)

      _return(_success)

   end function mk_r4field

   function mk_r8field(r8array, field_name, rc) result(r8field)
      type(ESMF_Field) :: r8field
      real(kind=ESMF_KIND_R8), intent(in) :: r8array(:,:)
      character(len=*), intent(in) :: field_name
      integer, optional, intent(out) :: rc

      integer :: status

      r8field = mk_field(r8array, name = field_name, _rc)

      _return(_success)

   end function mk_r8field

   function mk_r4ungrid_field(field_name, lbound, ubound, rc) result(r4field)
      type(ESMF_Field) :: r4field
      character(len=*), intent(in) :: field_name
      integer, intent(in) :: lbound
      integer, intent(in) :: ubound
      integer, optional, intent(out) :: rc

      integer :: status

      r4field = mk_field_r4_ungrid(name = field_name, ungriddedLBound=[lbound],ungriddedUBound=[ubound],_rc)

      _return(_success)

   end function mk_r4ungrid_field

end module field_utils_setup
