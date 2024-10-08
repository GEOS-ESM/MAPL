#include "MAPL_Generic.h"
module mapl3g_FieldCondensedArray
   use, intrinsic :: iso_c_binding, only: c_ptr, c_f_pointer
   use mapl3g_FieldCondensedArray_private, only: ARRAY_RANK, get_fptr_shape_private
   use mapl3g_FieldDimensionInfo, only: get_vertical_dim_spec_name
   use MAPL_FieldPointerUtilities, only: FieldGetLocalElementCount, assign_fptr
   use MAPL_ExceptionHandling
   use ESMF, only: ESMF_Field, ESMF_FieldGet
   use ESMF, only: ESMF_KIND_R4, ESMF_KIND_R8, ESMF_KIND_I8

   implicit none
   private
   public :: assign_fptr_condensed_array

   interface assign_fptr_condensed_array
      module procedure :: assign_fptr_condensed_array_r4
      module procedure :: assign_fptr_condensed_array_r8
   end interface assign_fptr_condensed_array

contains

   subroutine assign_fptr_condensed_array_r4(x, fptr, rc)
      type(ESMF_Field), intent(inout) :: x
      real(kind=ESMF_KIND_R4), pointer, intent(out) :: fptr(:,:,:)
      integer, optional, intent(out) :: rc
      integer(ESMF_KIND_I8) :: fp_shape(ARRAY_RANK)
      integer :: status

      fp_shape = get_fptr_shape(x, _RC)
      call assign_fptr(x, fp_shape, fptr, _RC)
      _RETURN(_SUCCESS)

   end subroutine assign_fptr_condensed_array_r4

   subroutine assign_fptr_condensed_array_r8(x, fptr, rc)
      type(ESMF_Field), intent(inout) :: x
      real(kind=ESMF_KIND_R8), pointer, intent(out) :: fptr(:,:,:)
      integer, optional, intent(out) :: rc
      integer(ESMF_KIND_I8) :: fp_shape(ARRAY_RANK)
      integer :: status

      fp_shape = get_fptr_shape(x, _RC)
      call assign_fptr(x, fp_shape, fptr, _RC)
      _RETURN(_SUCCESS)

   end subroutine assign_fptr_condensed_array_r8

   function get_fptr_shape(f, rc) result(fptr_shape)
      integer :: fptr_shape(ARRAY_RANK)
      type(ESMF_Field), intent(inout) :: f
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: rank
      integer, allocatable :: gridToFieldMap(:)
      integer, allocatable :: localElementCount(:)
      logical :: has_vertical
      character(len=:), allocatable :: spec_name
      character(len=*), parameter :: VERTICAL_DIM_NONE_NAME = 'VERTICAL_DIM_NONE'
      integer :: dimCount

      call ESMF_FieldGet(f, dimCount=dimCount, rank=rank, _RC)
      _ASSERT(.not. rank < 0, 'rank cannot be negative.')
      _ASSERT(.not. dimCount < 0, 'dimCount cannot be negative.')
      allocate(localElementCount(rank))
      allocate(gridToFieldMap(dimCount))
      call ESMF_FieldGet(f, gridToFieldMap=gridToFieldMap, _RC) 
      !  Due to an ESMF bug, getting the localElementCount must use the module function.
      !  See FieldGetLocalElementCount (specific function) comments.
      localElementCount = FieldGetLocalElementCount(f, _RC)
      spec_name = get_vertical_dim_spec_name(f, _RC)
      has_vertical = spec_name /= VERTICAL_DIM_NONE_NAME
      fptr_shape = get_fptr_shape_private(gridToFieldMap, localElementCount, has_vertical, _RC)

   end function get_fptr_shape

end module mapl3g_FieldCondensedArray
