#include "MAPL.h"
module mapl_FieldCondensedArray_mod
   use mapl_FieldCondensedArray_private_mod, only: ARRAY_RANK, get_fptr_shape_private
   use mapl_FieldCondensedArray_private_mod, only: SLICE3D_ARRAY_RANK, get_fptr_shape_slice3d_private
   use mapl_FieldPointerUtilities_mod, only: FieldGetLocalElementCount, assign_fptr
   use mapl_VerticalStaggerLoc_mod
   use mapl_ErrorHandling_mod
   use mapl_FieldGet_mod
   use ESMF, only: ESMF_Field, ESMF_FieldGet
   use ESMF, only: ESMF_KIND_R4, ESMF_KIND_R8, ESMF_KIND_I8
   use, intrinsic :: iso_c_binding, only: c_ptr, c_f_pointer
   implicit none(type, external)
   private

   public :: assign_fptr_condensed_array
   public :: condensed_slice_rank

   interface assign_fptr_condensed_array
      module procedure :: assign_fptr_condensed_array_r4
      module procedure :: assign_fptr_condensed_array_r8
      module procedure :: assign_fptr_condensed_array_slice3d_r4
      module procedure :: assign_fptr_condensed_array_slice3d_r8
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

   subroutine assign_fptr_condensed_array_slice3d_r4(x, fptr, rc)
      type(ESMF_Field), intent(inout) :: x
      real(kind=ESMF_KIND_R4), pointer, intent(out) :: fptr(:,:,:,:)
      integer, optional, intent(out) :: rc
      integer(ESMF_KIND_I8) :: fp_shape(SLICE3D_ARRAY_RANK)
      integer :: status

      fp_shape = get_fptr_shape_slice3d(x, _RC)
      call assign_fptr(x, fp_shape, fptr, _RC)
      _RETURN(_SUCCESS)

   end subroutine assign_fptr_condensed_array_slice3d_r4

   subroutine assign_fptr_condensed_array_slice3d_r8(x, fptr, rc)
      type(ESMF_Field), intent(inout) :: x
      real(kind=ESMF_KIND_R8), pointer, intent(out) :: fptr(:,:,:,:)
      integer, optional, intent(out) :: rc
      integer(ESMF_KIND_I8) :: fp_shape(SLICE3D_ARRAY_RANK)
      integer :: status

      fp_shape = get_fptr_shape_slice3d(x, _RC)
      call assign_fptr(x, fp_shape, fptr, _RC)
      _RETURN(_SUCCESS)

   end subroutine assign_fptr_condensed_array_slice3d_r8

   ! Return the rank of the "slice" produced by the condensed-array
   ! machinery for this field: 3 when the field has exactly three
   ! non-ungridded (grid + vertical) dimensions (so a 3D slice is natural),
   ! and 2 otherwise (the default collapsed horizontal x vertical slice).
   function condensed_slice_rank(f, rc) result(slice_rank)
      integer :: slice_rank
      type(ESMF_Field), intent(inout) :: f
      integer, optional, intent(out) :: rc
      integer :: status
      integer, allocatable :: gridToFieldMap(:)
      integer, allocatable :: localElementCount(:)
      logical :: has_vertical
      integer :: n_spatial

      call get_field_layout(f, gridToFieldMap, localElementCount, has_vertical, _RC)
      n_spatial = count(gridToFieldMap /= 0)
      if (has_vertical) n_spatial = n_spatial + 1
      if (n_spatial == 3) then
         slice_rank = 3
      else
         slice_rank = 2
      end if

      _RETURN(_SUCCESS)
   end function condensed_slice_rank

   function get_fptr_shape(f, rc) result(fptr_shape)
      integer :: fptr_shape(ARRAY_RANK)
      type(ESMF_Field), intent(inout) :: f
      integer, optional, intent(out) :: rc
      integer :: status
      integer, allocatable :: gridToFieldMap(:)
      integer, allocatable :: localElementCount(:)
      logical :: has_vertical

      call get_field_layout(f, gridToFieldMap, localElementCount, has_vertical, _RC)
      fptr_shape = get_fptr_shape_private(gridToFieldMap, localElementCount, has_vertical, _RC)

      _RETURN(_SUCCESS)
   end function get_fptr_shape

   function get_fptr_shape_slice3d(f, rc) result(fptr_shape)
      integer :: fptr_shape(SLICE3D_ARRAY_RANK)
      type(ESMF_Field), intent(inout) :: f
      integer, optional, intent(out) :: rc
      integer :: status
      integer, allocatable :: gridToFieldMap(:)
      integer, allocatable :: localElementCount(:)
      logical :: has_vertical

      call get_field_layout(f, gridToFieldMap, localElementCount, has_vertical, _RC)
      fptr_shape = get_fptr_shape_slice3d_private(gridToFieldMap, localElementCount, has_vertical, _RC)

      _RETURN(_SUCCESS)
   end function get_fptr_shape_slice3d

   ! Extract the field-layout metadata shared by the condensed-array shape
   ! helpers: the grid-to-field map, the local element count, and whether
   ! the field carries a vertical dimension.
   subroutine get_field_layout(f, gridToFieldMap, localElementCount, has_vertical, rc)
      type(ESMF_Field), intent(inout) :: f
      integer, allocatable, intent(out) :: gridToFieldMap(:)
      integer, allocatable, intent(out) :: localElementCount(:)
      logical, intent(out) :: has_vertical
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: rank
      integer :: geomDimCount
      type(VerticalStaggerLoc) :: vert_staggerloc

      call ESMF_FieldGet(f, geomDimCount=geomDimCount, rank=rank, _RC)
      _ASSERT(.not. rank < 0, 'rank cannot be negative.')
      _ASSERT(.not. geomDimCount < 0, 'geomDimCount cannot be negative.')
      allocate(localElementCount(rank))
      allocate(gridToFieldMap(geomDimCount))
      call ESMF_FieldGet(f, gridToFieldMap=gridToFieldMap, _RC)
      !  Due to an ESMF bug, getting the localElementCount must use the module function.
      !  See FieldGetLocalElementCount (specific function) comments.
      localElementCount = FieldGetLocalElementCount(f, _RC)
      call FieldGet(f, vert_staggerloc=vert_staggerloc, _RC)
      has_vertical = (vert_staggerloc /= VERTICAL_STAGGER_NONE)

      _RETURN(_SUCCESS)
   end subroutine get_field_layout


end module mapl_FieldCondensedArray_mod
