#include "MAPL_Generic.h"

module geom_setup

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
   integer, parameter :: REG_DECOMP_DEFAULT(*) = [2, 2]
   integer, parameter :: MAX_INDEX_DEFAULT(*) = [4, 4]
   integer, parameter :: MIN_INDEX_DEFAULT(*) = [1, 1]
   integer, parameter :: DIMR4_DEFAULT(*) = [4, 4]
   integer, parameter :: DIMR8_DEFAULT(*) = [4, 4]
   integer, parameter :: SIZE_R4 = 16
   integer, parameter :: SIZE_R8 = 16
   real, parameter :: undef = 42.0

   real(kind=ESMF_KIND_R4), parameter :: R4_ARRAY_DEFAULT(*,*) = reshape([(i, i = 1, SIZE_R4)], DIMR4_DEFAULT)
   real(kind=ESMF_KIND_R8), parameter :: R8_ARRAY_DEFAULT(*,*) = reshape([(i, i = 1, SIZE_R8)], DIMR8_DEFAULT) 
   
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
   function mk_grid(regDecomp, minIndex, maxIndex, indexflag, grid_name, rc) result(grid)
      integer, dimension(:), intent(in) :: regDecomp
      integer, dimension(:), intent(in) :: minIndex
      integer, dimension(:), intent(in) :: maxIndex
      type(ESMF_Index_Flag), intent(in) :: indexflag
      character(len=*), intent(in) :: grid_name
      integer, optional, intent(out) :: rc

      type(ESMF_Grid) :: grid

      integer :: status

      grid = ESMF_GridCreateNoPeriDim(regDecomp = regDecomp, maxIndex = maxIndex, minIndex = minIndex, indexflag = indexflag, name = grid_name, _RC)

      _RETURN(_SUCCESS)
   end function mk_grid

   function mk_field_r4_ungrid(regDecomp, minIndex, maxIndex, indexflag, name, ungriddedLBound, ungriddedUBound, rc) result(field)
      integer, dimension(:), intent(in) :: regDecomp
      integer, dimension(:), intent(in) :: minIndex
      integer, dimension(:), intent(in) :: maxIndex
      type(ESMF_Index_Flag), intent(in) :: indexflag
      character(len=*), intent(in) :: name
      integer, optional, intent(in) :: ungriddedLBound(:)
      integer, optional, intent(in) :: ungriddedUBound(:)
      integer, optional, intent(out) :: rc

      type(ESMF_Field) :: field

      integer :: status

      field = mk_field_common(tk = ESMF_TYPEKIND_R4, regDecomp=regDecomp, minIndex=minIndex, maxIndex=maxIndex, indexflag = indexflag, name = name, ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, _RC)

      _RETURN(_SUCCESS)
   end function mk_field_r4_ungrid
   
   function mk_field_r4_2d(farray, regDecomp, minIndex, maxIndex, indexflag, name, rc) result(field)
      real(kind=ESMF_KIND_R4), dimension(:,:), target, intent(in) :: farray
      integer, dimension(:), intent(in) :: regDecomp
      integer, dimension(:), intent(in) :: minIndex
      integer, dimension(:), intent(in) :: maxIndex
      type(ESMF_Index_Flag), intent(in) :: indexflag
      character(len=*), intent(in) :: name
      integer, optional, intent(out) :: rc

      type(ESMF_Field) :: field
      real(kind=ESMF_KIND_R4), dimension(:,:), pointer :: ptr

      integer :: status

      field = mk_field_common(tk = ESMF_TYPEKIND_R4, regDecomp=regDecomp, minIndex=minIndex, maxIndex=maxIndex, indexflag = indexflag, name = name, _RC)
      call ESMF_FieldGet(field, farrayPtr = ptr, _RC)
      ptr => farray

      _RETURN(_SUCCESS)
   end function mk_field_r4_2d

   function mk_field_r8_2d(farray, regDecomp, minIndex, maxIndex, indexflag, name, rc) result(field)
      real(kind=ESMF_KIND_R8), dimension(:,:), target, intent(in) :: farray
      integer, dimension(:), intent(in) :: regDecomp
      integer, dimension(:), intent(in) :: minIndex
      integer, dimension(:), intent(in) :: maxIndex
      type(ESMF_Index_Flag), intent(in) :: indexflag
      character(len=*), intent(in) :: name
      integer, optional, intent(out) :: rc

      type(ESMF_Field) :: field
      real(kind=ESMF_KIND_R8), dimension(:,:), pointer :: ptr

      integer :: status

      field = mk_field_common(tk = ESMF_TYPEKIND_R8, regDecomp=regDecomp, minIndex=minIndex, maxIndex=maxIndex, indexflag = indexflag, name = name, _RC)
      call ESMF_FieldGet(field, farrayPtr = ptr, _RC)
      ptr => farray

      _RETURN(_SUCCESS)
   end function mk_field_r8_2d

   function mk_field_common(tk, regDecomp, minIndex, maxIndex, indexflag, name, ungriddedLBound, ungriddedUBound, rc) result(field)
      type(ESMF_TypeKind_Flag), intent(in) :: tk
      integer, dimension(:), intent(in) :: regDecomp
      integer, dimension(:), intent(in) :: minIndex
      integer, dimension(:), intent(in) :: maxIndex
      type(ESMF_Index_Flag), intent(in) :: indexflag
      character(len=*), intent(in) :: name
      integer, optional, intent(in) :: ungriddedLBound(:)
      integer, optional, intent(in) :: ungriddedUBound(:)
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: GRID_SUFFIX = '_grid'
      character(len=*), parameter :: FIELD_SUFFIX = '_field'
      
      type(ESMF_Field) :: field
      type(ESMF_Grid) :: grid
      integer :: status

      grid = mk_grid(regDecomp=regDecomp, minIndex=minIndex, maxIndex=maxIndex, indexflag = indexflag, grid_name = name // GRID_SUFFIX, _RC)
      field = ESMF_FieldCreate(grid, typekind = tk, name = name // FIELD_SUFFIX, ungriddedLBound = ungriddedLBound, ungriddedUBound = ungriddedUBound, _RC)

      _RETURN(_SUCCESS)
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

      _ASSERT(xrange > 0, 'Range for random numbers must be positive.')
      call random_number(x)
      x = xrange * x + xmin

   end subroutine initialize_array_R4

   subroutine initialize_array_R8(x, xmin, xrange)
      real(ESMF_KIND_R8), intent(inout) :: x(:,:)
      real(ESMF_KIND_R8), intent(in) :: xmin
      real(ESMF_KIND_R8), intent(in) :: xrange
      integer :: rc

      _ASSERT(xrange > 0, 'Range for random numbers must be positive.')
      call random_number(x)
      x = xrange * x + xmin

   end subroutine initialize_array_R8

end module geom_setup
