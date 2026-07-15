#include "MAPL.h"
#include "unused_dummy.H"

! Support for service providers that would like to handle fields with
! ungridded (and/or vertical) dimensions in the natural fashion, i.e. as
! a collection of "slices".
!
! An arbitrary-rank field is collapsed into a condensed array using the
! machinery in FieldCondensedArray.F90.  Two condensed layouts are
! supported, and the appropriate one is selected automatically from the
! field's structure:
!
!   * 2D slices (the default) - the field is condensed into a rank-3
!     array of shape (horizontal, vertical, ungridded).  A slice is the
!     (horizontal, vertical) plane for a fixed ungridded index.
!   * 3D slices - when the field has exactly three non-ungridded (grid +
!     vertical) dimensions (for example a 4D field whose fourth dimension
!     is the ungridded dimension), the field is condensed into a rank-4
!     array of shape (slice1, slice2, slice3, ungridded).  A slice is the
!     leading 3D block for a fixed ungridded index.
!
! Iterating over the ungridded dimensions of arbitrary rank therefore
! reduces to a single loop bound in both cases.
!
! Three entry points are provided:
!   * FieldGetPointerToSlice - return a typed (R4 or R8) pointer to a
!     single slice, for callers that manage their own iteration.  The
!     rank of the supplied pointer (2D or 3D) selects the slice layout.
!   * FieldGetSliceAsField   - return a new ESMF_Field whose data is a
!     reference to the k-th slice of the original field's condensed array.
!     The slice field has all attributes of the original except it carries
!     no ungridded dimensions (one fewer dimension).
!   * FieldApplyUserRoutine  - apply a user routine to every slice of a
!     field.  The slice is presented as an ESMF_Field (created by
!     FieldGetSliceAsField) so the user routine has access to the full
!     field metadata alongside the data.
module mapl_FieldApplyUserRoutine_mod

   use ESMF, only: ESMF_Field, ESMF_FieldGet, ESMF_FieldCreate
   use ESMF, only: ESMF_FieldDestroy
   use ESMF, only: ESMF_FieldStatus_Flag, ESMF_FIELDSTATUS_COMPLETE
   use ESMF, only: ESMF_TypeKind_Flag, ESMF_TYPEKIND_R4, ESMF_TYPEKIND_R8
   use ESMF, only: ESMF_KIND_R4, ESMF_KIND_R8
   use ESMF, only: ESMF_Grid
   use ESMF, only: ESMF_Info, ESMF_InfoGetFromHost, ESMF_InfoUpdate
   use ESMF, only: ESMF_DATACOPY_REFERENCE
   use ESMF, only: operator(==)
   use mapl_FieldCondensedArray_mod, only: assign_fptr_condensed_array
   use mapl_FieldCondensedArray_mod, only: condensed_slice_rank
   use mapl_FieldGet_mod, only: FieldGet
   use mapl_FieldInfo_mod, only: FieldInfoSetInternal
   use mapl_FieldPointerUtilities_mod, only: FieldGetLocalElementCount
   use mapl_UngriddedDims_mod, only: UngriddedDims
   use mapl_VerticalStaggerLoc_mod, only: VerticalStaggerLoc, VERTICAL_STAGGER_NONE
   use mapl_KeywordEnforcer_mod, only: KeywordEnforcer
   use mapl_ErrorHandling_mod
   use, intrinsic :: iso_c_binding, only: c_ptr, c_f_pointer, c_loc

   implicit none(type, external)
   private

   public :: FieldGetPointerToSlice
   public :: FieldGetSliceAsField
   public :: FieldApplyUserRoutine
   public :: I_FieldSliceRoutine

   ! Return a pointer to a single slice of a field.  Overloaded for R4 and
   ! R8 and for 2D and 3D slices; the rank of the supplied pointer selects
   ! the slice layout.  A field with no ungridded dimensions has exactly
   ! one slice; a genuinely 2D field yields a 2D slice with a trivial
   ! (size 1) vertical extent.
   interface FieldGetPointerToSlice
      module procedure get_slice_r4
      module procedure get_slice_r8
      module procedure get_slice3d_r4
      module procedure get_slice3d_r8
   end interface FieldGetPointerToSlice

   ! The user routine applied to each slice.  The slice is presented as an
   ! ESMF_Field that references the data of the original field for the given
   ! ungridded index; the field carries all metadata of the original except
   ! it has no ungridded dimensions.  It is intent(inout) so that the user
   ! routine can modify the field data in place.
   abstract interface
      subroutine I_FieldSliceRoutine(slice_field, rc)
         use ESMF, only: ESMF_Field
         type(ESMF_Field), intent(inout) :: slice_field
         integer, optional, intent(out) :: rc
      end subroutine I_FieldSliceRoutine
   end interface

contains

   ! Create a new ESMF_Field whose data is a pointer (reference) to the
   ! k-th unit slice of the collapsed ungridded dimension of 'field'.
   ! The returned field has all attributes and properties of the original
   ! except that it carries no ungridded dimensions (one fewer dimension).
   ! The caller must destroy the returned field when it is no longer needed;
   ! however the underlying data must NOT be freed by the caller because
   ! it belongs to the original field.
   function FieldGetSliceAsField(field, slice_index, rc) result(slice_field)
      type(ESMF_Field) :: slice_field
      type(ESMF_Field), intent(inout) :: field
      integer, intent(in) :: slice_index
      integer, optional, intent(out) :: rc

      ! Condensed-array views of the field data
      real(kind=ESMF_KIND_R4), pointer :: condensed_r4(:,:,:)
      real(kind=ESMF_KIND_R8), pointer :: condensed_r8(:,:,:)
      real(kind=ESMF_KIND_R4), pointer :: condensed3d_r4(:,:,:,:)
      real(kind=ESMF_KIND_R8), pointer :: condensed3d_r8(:,:,:,:)

      ! Typed pointers to the reinterpreted slice (correct shape for the new field)
      real(kind=ESMF_KIND_R4), pointer :: slice_r4_2d(:,:)
      real(kind=ESMF_KIND_R4), pointer :: slice_r4_3d(:,:,:)
      real(kind=ESMF_KIND_R8), pointer :: slice_r8_2d(:,:)
      real(kind=ESMF_KIND_R8), pointer :: slice_r8_3d(:,:,:)

      ! Field metadata
      type(ESMF_TypeKind_Flag) :: typekind
      type(ESMF_Grid) :: grid
      type(ESMF_Info) :: field_info, slice_info
      integer :: status, slc_rank
      integer :: field_rank, geomDimCount
      integer, allocatable :: gridToFieldMap(:), localElementCount(:)
      integer, allocatable :: grid_dims(:)
      integer :: vert_dim, num_levels
      logical :: has_vertical
      type(VerticalStaggerLoc) :: vert_staggerloc
      type(c_ptr) :: cptr
      integer :: shape2(2), shape3(3)

      ! Retrieve scalar metadata from the field
      call ESMF_FieldGet(field, typekind=typekind, grid=grid, &
           geomDimCount=geomDimCount, rank=field_rank, _RC)
      allocate(localElementCount(field_rank), gridToFieldMap(geomDimCount))
      call ESMF_FieldGet(field, gridToFieldMap=gridToFieldMap, _RC)
      localElementCount = FieldGetLocalElementCount(field, _RC)
      call FieldGet(field, vert_staggerloc=vert_staggerloc, _RC)
      has_vertical = (vert_staggerloc /= VERTICAL_STAGGER_NONE)

      ! Identify the grid-dimension indices in the field array
      grid_dims = pack(gridToFieldMap, gridToFieldMap /= 0)
      vert_dim = 0
      if (size(grid_dims) > 0) vert_dim = maxval(grid_dims)
      if (has_vertical) vert_dim = vert_dim + 1
      num_levels = 0
      if (has_vertical) num_levels = localElementCount(vert_dim)

      slc_rank = condensed_slice_rank(field, _RC)

      ! Build the grid-dimension shape used to reinterpret the condensed slice
      ! (only needed for the 2D condensed case where horizontal dims are collapsed).
      ! For a standard 2D horizontal grid: grid_dims = [1,2], so
      !   shape2 = [lec(1), lec(2)]   (lon x lat)
      !   shape3 = [lec(1), lec(2), num_levels]
      if (size(grid_dims) == 2) then
         shape2 = [localElementCount(grid_dims(1)), localElementCount(grid_dims(2))]
         shape3 = [localElementCount(grid_dims(1)), localElementCount(grid_dims(2)), num_levels]
      end if

      if (typekind == ESMF_TYPEKIND_R4) then
         if (slc_rank == 3) then
            ! Condensed3D layout: (d1, d2, d3, N_ungrid) where d1*d2*d3 are the
            ! three spatial dims (lon, lat, vert).  The slice is already shaped
            ! correctly for a rank-3 field.
            call assign_fptr_condensed_array(field, condensed3d_r4, _RC)
            _ASSERT(slice_index >= 1 .and. slice_index <= size(condensed3d_r4, 4), 'slice_index out of range.')
            cptr = c_loc(condensed3d_r4(1, 1, 1, slice_index))
            call c_f_pointer(cptr, slice_r4_3d, &
                 [size(condensed3d_r4,1), size(condensed3d_r4,2), size(condensed3d_r4,3)])
            slice_field = ESMF_FieldCreate(grid, slice_r4_3d, &
                 gridToFieldMap=gridToFieldMap, &
                 ungriddedLBound=[1], ungriddedUBound=[size(condensed3d_r4,3)], &
                 datacopyflag=ESMF_DATACOPY_REFERENCE, _RC)
         else if (has_vertical) then
            ! 2D condensed layout with vertical: (horiz_collapsed, vert, N_ungrid).
            ! Reinterpret the (horiz_collapsed, vert) slice as (lon, lat, vert).
            call assign_fptr_condensed_array(field, condensed_r4, _RC)
            _ASSERT(slice_index >= 1 .and. slice_index <= size(condensed_r4, 3), 'slice_index out of range.')
            cptr = c_loc(condensed_r4(1, 1, slice_index))
            call c_f_pointer(cptr, slice_r4_3d, shape3)
            slice_field = ESMF_FieldCreate(grid, slice_r4_3d, &
                 gridToFieldMap=gridToFieldMap, &
                 ungriddedLBound=[1], ungriddedUBound=[num_levels], &
                 datacopyflag=ESMF_DATACOPY_REFERENCE, _RC)
         else
            ! 2D condensed layout without vertical: (horiz_collapsed, 1, N_ungrid).
            ! Reinterpret the (horiz_collapsed, 1) slice as (lon, lat).
            call assign_fptr_condensed_array(field, condensed_r4, _RC)
            _ASSERT(slice_index >= 1 .and. slice_index <= size(condensed_r4, 3), 'slice_index out of range.')
            cptr = c_loc(condensed_r4(1, 1, slice_index))
            call c_f_pointer(cptr, slice_r4_2d, shape2)
            slice_field = ESMF_FieldCreate(grid, slice_r4_2d, &
                 gridToFieldMap=gridToFieldMap, &
                 datacopyflag=ESMF_DATACOPY_REFERENCE, _RC)
         end if
      else if (typekind == ESMF_TYPEKIND_R8) then
         if (slc_rank == 3) then
            call assign_fptr_condensed_array(field, condensed3d_r8, _RC)
            _ASSERT(slice_index >= 1 .and. slice_index <= size(condensed3d_r8, 4), 'slice_index out of range.')
            cptr = c_loc(condensed3d_r8(1, 1, 1, slice_index))
            call c_f_pointer(cptr, slice_r8_3d, &
                 [size(condensed3d_r8,1), size(condensed3d_r8,2), size(condensed3d_r8,3)])
            slice_field = ESMF_FieldCreate(grid, slice_r8_3d, &
                 gridToFieldMap=gridToFieldMap, &
                 ungriddedLBound=[1], ungriddedUBound=[size(condensed3d_r8,3)], &
                 datacopyflag=ESMF_DATACOPY_REFERENCE, _RC)
         else if (has_vertical) then
            call assign_fptr_condensed_array(field, condensed_r8, _RC)
            _ASSERT(slice_index >= 1 .and. slice_index <= size(condensed_r8, 3), 'slice_index out of range.')
            cptr = c_loc(condensed_r8(1, 1, slice_index))
            call c_f_pointer(cptr, slice_r8_3d, shape3)
            slice_field = ESMF_FieldCreate(grid, slice_r8_3d, &
                 gridToFieldMap=gridToFieldMap, &
                 ungriddedLBound=[1], ungriddedUBound=[num_levels], &
                 datacopyflag=ESMF_DATACOPY_REFERENCE, _RC)
         else
            call assign_fptr_condensed_array(field, condensed_r8, _RC)
            _ASSERT(slice_index >= 1 .and. slice_index <= size(condensed_r8, 3), 'slice_index out of range.')
            cptr = c_loc(condensed_r8(1, 1, slice_index))
            call c_f_pointer(cptr, slice_r8_2d, shape2)
            slice_field = ESMF_FieldCreate(grid, slice_r8_2d, &
                 gridToFieldMap=gridToFieldMap, &
                 datacopyflag=ESMF_DATACOPY_REFERENCE, _RC)
         end if
      else
         _FAIL('Unsupported typekind for FieldGetSliceAsField (expected R4 or R8).')
      end if

      ! Copy all ESMF_Info metadata from the original field to the slice field,
      ! then clear the ungridded_dims entry so the slice field correctly declares
      ! that it carries no ungridded dimensions.
      call ESMF_InfoGetFromHost(field, field_info, _RC)
      call ESMF_InfoGetFromHost(slice_field, slice_info, _RC)
      call ESMF_InfoUpdate(slice_info, field_info, recursive=.true., _RC)
      call FieldInfoSetInternal(slice_info, ungridded_dims=UngriddedDims(), _RC)

      _RETURN(_SUCCESS)
   end function FieldGetSliceAsField

   subroutine get_slice_r4(field, slice_index, ptr, rc)
      type(ESMF_Field), intent(inout) :: field
      integer, intent(in) :: slice_index
      real(kind=ESMF_KIND_R4), pointer, intent(out) :: ptr(:,:)
      integer, optional, intent(out) :: rc

      real(kind=ESMF_KIND_R4), pointer :: condensed(:,:,:)
      integer :: status

      nullify(ptr)
      call assign_fptr_condensed_array(field, condensed, _RC)
      _ASSERT(slice_index >= 1 .and. slice_index <= size(condensed, 3), 'slice_index out of range.')
      ptr => condensed(:,:,slice_index)

      _RETURN(_SUCCESS)
   end subroutine get_slice_r4

   subroutine get_slice_r8(field, slice_index, ptr, rc)
      type(ESMF_Field), intent(inout) :: field
      integer, intent(in) :: slice_index
      real(kind=ESMF_KIND_R8), pointer, intent(out) :: ptr(:,:)
      integer, optional, intent(out) :: rc

      real(kind=ESMF_KIND_R8), pointer :: condensed(:,:,:)
      integer :: status

      nullify(ptr)
      call assign_fptr_condensed_array(field, condensed, _RC)
      _ASSERT(slice_index >= 1 .and. slice_index <= size(condensed, 3), 'slice_index out of range.')
      ptr => condensed(:,:,slice_index)

      _RETURN(_SUCCESS)
   end subroutine get_slice_r8

   subroutine get_slice3d_r4(field, slice_index, ptr, rc)
      type(ESMF_Field), intent(inout) :: field
      integer, intent(in) :: slice_index
      real(kind=ESMF_KIND_R4), pointer, intent(out) :: ptr(:,:,:)
      integer, optional, intent(out) :: rc

      real(kind=ESMF_KIND_R4), pointer :: condensed(:,:,:,:)
      integer :: status

      nullify(ptr)
      call assign_fptr_condensed_array(field, condensed, _RC)
      _ASSERT(slice_index >= 1 .and. slice_index <= size(condensed, 4), 'slice_index out of range.')
      ptr => condensed(:,:,:,slice_index)

      _RETURN(_SUCCESS)
   end subroutine get_slice3d_r4

   subroutine get_slice3d_r8(field, slice_index, ptr, rc)
      type(ESMF_Field), intent(inout) :: field
      integer, intent(in) :: slice_index
      real(kind=ESMF_KIND_R8), pointer, intent(out) :: ptr(:,:,:)
      integer, optional, intent(out) :: rc

      real(kind=ESMF_KIND_R8), pointer :: condensed(:,:,:,:)
      integer :: status

      nullify(ptr)
      call assign_fptr_condensed_array(field, condensed, _RC)
      _ASSERT(slice_index >= 1 .and. slice_index <= size(condensed, 4), 'slice_index out of range.')
      ptr => condensed(:,:,:,slice_index)

      _RETURN(_SUCCESS)
   end subroutine get_slice3d_r8

   subroutine FieldApplyUserRoutine(field, userRoutine, unusable, userrc, rc)
      type(ESMF_Field), intent(inout) :: field
      procedure(I_FieldSliceRoutine) :: userRoutine
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: userrc
      integer, optional, intent(out) :: rc

      real(kind=ESMF_KIND_R4), pointer :: condensed_r4(:,:,:)
      real(kind=ESMF_KIND_R8), pointer :: condensed_r8(:,:,:)
      real(kind=ESMF_KIND_R4), pointer :: condensed3d_r4(:,:,:,:)
      real(kind=ESMF_KIND_R8), pointer :: condensed3d_r8(:,:,:,:)
      type(ESMF_FieldStatus_Flag) :: field_status
      type(ESMF_TypeKind_Flag) :: typekind
      type(ESMF_Field) :: slice_field
      integer :: k, n_slices, slice_rank, status, user_status

      if (present(userrc)) userrc = 0
      user_status = 0

      call ESMF_FieldGet(field, status=field_status, _RC)
      _RETURN_UNLESS(field_status == ESMF_FIELDSTATUS_COMPLETE)

      call ESMF_FieldGet(field, typekind=typekind, _RC)
      slice_rank = condensed_slice_rank(field, _RC)

      if (typekind == ESMF_TYPEKIND_R4) then
         if (slice_rank == 3) then
            call assign_fptr_condensed_array(field, condensed3d_r4, _RC)
            n_slices = size(condensed3d_r4, 4)
         else
            call assign_fptr_condensed_array(field, condensed_r4, _RC)
            n_slices = size(condensed_r4, 3)
         end if
      else if (typekind == ESMF_TYPEKIND_R8) then
         if (slice_rank == 3) then
            call assign_fptr_condensed_array(field, condensed3d_r8, _RC)
            n_slices = size(condensed3d_r8, 4)
         else
            call assign_fptr_condensed_array(field, condensed_r8, _RC)
            n_slices = size(condensed_r8, 3)
         end if
      else
         _FAIL('Unsupported typekind for FieldApplyUserRoutine (expected R4 or R8).')
      end if

      do k = 1, n_slices
         slice_field = FieldGetSliceAsField(field, k, _RC)
         user_status = 0
         call userRoutine(slice_field, rc=user_status)
         call ESMF_FieldDestroy(slice_field, noGarbage=.true., _RC)
         if (user_status /= 0) exit
      end do

      if (present(userrc)) userrc = user_status

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine FieldApplyUserRoutine

end module mapl_FieldApplyUserRoutine_mod
