#include "MAPL.h"
! Support for service providers that would like to handle fields with
! a collection of "slices".
!
! The field's native array (obtained via ESMF_FieldGet farrayPtr) is used
! directly throughout this module.  ESMF lays out ungridded dimensions as
! the trailing dimension(s) of the native array, so slicing is a simple
! Fortran array section on the last index.  The following native layouts
! are supported:
!
!   * No ungridded dimension - native array is rank-2 (horizontal only)
!     or rank-3 (horizontal + vertical).  There is exactly one slice,
!     which is the whole field.
!
!   * 1 ungridded dimension, no vertical - native array is rank-3:
!     (horiz_x, horiz_y, ungrid1).  A slice is the (horiz_x, horiz_y)
!     plane at a fixed ungridded index.
!
!   * 1 ungridded dimension, with vertical - native array is rank-4:
!     (horiz_x, horiz_y, vertical, ungrid1).  A slice is the
!     (horiz_x, horiz_y, vertical) block at a fixed ungridded index.
!
!   * 2 ungridded dimensions, no vertical - native array is rank-4:
!     (horiz_x, horiz_y, ungrid1, ungrid2).  A flat slice_index is
!     converted to a (i3, i4) pair in column-major order.
!
!   ungrid_count > MAX_UNGRIDDED_DIMS is not currently supported and is
!   caught by an explicit _ASSERT in both FieldSliceToField and
!   FieldApplyUserRoutine.
!
!   The vertical-vs-second-ungridded-dim ambiguity (both produce a
!   rank-4 native array with ESMF ungrid_count==2) is resolved by
!   condensed_slice_rank: slice_rank==3 means vertical is present and
!   the last dim is the sole ungridded dim; slice_rank==2 means no
!   vertical and both trailing dims are ungridded.
!
! Three public entry points are provided:
!   * FieldSliceToField       - given a field and a unit ungridded-slice
!     index, return a new ESMF_Field whose data is a pointer into the
!     corresponding slice of the original field.  The returned field has
!     all attributes of the original except ungridded-dimension metadata,
!     and is one rank lower (no ungridded dim).  If the original field
!     has no ungridded dimension the returned field is equivalent to the
!     original.  The caller is responsible for destroying the returned
!     field with ESMF_FieldDestroy.
!   * FieldGetPointerToSlice  - return a typed (R4 or R8) Fortran pointer
!     to a single slice, for callers that manage their own iteration.
!     When the original field has no ungridded dimension, slice_index=1
!     is accepted and the full field array is returned.  The rank of the
!     supplied pointer (2D or 3D) selects the slice layout.
!   * FieldApplyUserRoutine   - apply a user routine to every slice of a
!     field.  For each ungridded slice index, FieldSliceToField is called
!     to produce a transient ESMF_Field representing that slice; this
!     field is passed to the user routine and destroyed afterward.
module mapl_FieldApplyUserRoutine_mod

   use ESMF, only: ESMF_Field, ESMF_FieldGet, ESMF_FieldCreate, ESMF_FieldDestroy
   use ESMF, only: ESMF_FieldStatus_Flag, ESMF_FIELDSTATUS_COMPLETE
   use ESMF, only: ESMF_KIND_R4, ESMF_KIND_R8
   use ESMF, only: ESMF_TypeKind_Flag
   use ESMF, only: ESMF_TYPEKIND_R4, ESMF_TYPEKIND_R8
   use ESMF, only: ESMF_MAXSTR
   use ESMF, only: ESMF_Geom
   use ESMF, only: ESMF_DATACOPY_REFERENCE
   use ESMF, only: ESMF_AttributeCopy, ESMF_AttributeRemove
   use ESMF, only: operator(==)
   use mapl_FieldCondensedArray_mod, only: condensed_slice_rank
   use mapl_KeywordEnforcer_mod, only: KeywordEnforcer
   use mapl_ErrorHandling_mod

   implicit none(type, external)
   private

   public :: FieldSliceToField
   public :: FieldGetPointerToSlice
   public :: FieldApplyUserRoutine
   public :: I_FieldSliceRoutine

   integer, parameter :: MAX_UNGRIDDED_DIMS = 2

   interface FieldGetPointerToSlice
      module procedure get_slice_r4
      module procedure get_slice_r8
      module procedure get_slice3d_r4
      module procedure get_slice3d_r8
   end interface FieldGetPointerToSlice

   ! The user routine applied to each slice.  The slice is presented as
   ! a proper ESMF_Field (with pointer association into the parent field's
   ! data) so that the full ESMF field API is available inside the routine.
   abstract interface
      subroutine I_FieldSliceRoutine(field, rc)
         import ESMF_Field
         type(ESMF_Field), intent(inout) :: field
         integer, optional, intent(out) :: rc
      end subroutine I_FieldSliceRoutine
   end interface

contains

   ! --------------------------------------------------------------------------
   ! FieldSliceToField
   !
   ! Given an ESMF_Field and a unit ungridded-slice index, construct and
   ! return a new ESMF_Field whose internal data pointer references the
   ! corresponding slice of the original field's native array.  The
   ! returned field:
   !   - shares geometry (geom) with the original
   !   - has the same typekind
   !   - has no ungridded dimension (rank is one lower than the original)
   !   - carries a copy of the original field's ESMF attributes, minus
   !     ungridded-dimension metadata (see TODO below)
   !   - must be destroyed by the caller via ESMF_FieldDestroy
   !
   ! If the original field has no ungridded dimension, slice_index must
   ! be 1 and the returned field wraps the full native array.
   !
   ! The vertical-vs-second-ungridded ambiguity is resolved via
   ! condensed_slice_rank: slice_rank==3 → vertical present, last dim
   ! is the sole ungridded dim; slice_rank==2 → no vertical, both
   ! trailing rank-4 dims are ungridded.
   !
   ! ungrid_count > MAX_UNGRIDDED_DIMS triggers an assertion failure.
   ! --------------------------------------------------------------------------
   function FieldSliceToField(field, slice_index, rc) result(slice_field)
      type(ESMF_Field), intent(inout) :: field
      integer,          intent(in)    :: slice_index
      integer, optional, intent(out)  :: rc
      type(ESMF_Field)                :: slice_field

      type(ESMF_TypeKind_Flag)   :: typekind
      type(ESMF_Geom)            :: geom
      character(len=ESMF_MAXSTR) :: field_name
      integer :: ungrid_count, slice_rank, status
      integer :: i3, i4   ! column-major index pair for ungrid_count==2, no vertical

      ! Native array pointers
      real(kind=ESMF_KIND_R4), pointer :: fptr2d_r4(:,:)
      real(kind=ESMF_KIND_R8), pointer :: fptr2d_r8(:,:)
      real(kind=ESMF_KIND_R4), pointer :: fptr3d_r4(:,:,:)
      real(kind=ESMF_KIND_R8), pointer :: fptr3d_r8(:,:,:)
      real(kind=ESMF_KIND_R4), pointer :: fptr4d_r4(:,:,:,:)
      real(kind=ESMF_KIND_R8), pointer :: fptr4d_r8(:,:,:,:)

      ! Slice pointers (ungridded dim(s) removed)
      real(kind=ESMF_KIND_R4), pointer :: slice_r4(:,:)
      real(kind=ESMF_KIND_R8), pointer :: slice_r8(:,:)
      real(kind=ESMF_KIND_R4), pointer :: slice3d_r4(:,:,:)
      real(kind=ESMF_KIND_R8), pointer :: slice3d_r8(:,:,:)

      call ESMF_FieldGet(field, geom=geom, typekind=typekind, &
           name=field_name, _RC)
      call ESMF_FieldGet(field, ungriddedDimCount=ungrid_count, _RC)

      _ASSERT(ungrid_count <= MAX_UNGRIDDED_DIMS, 'FieldSliceToField: ungrid_count > MAX_UNGRIDDED_DIMS is not supported.')

      ! condensed_slice_rank returns 2 (no vertical) or 3 (vertical present).
      ! This is the only use of mapl_FieldCondensedArray_mod in this module;
      ! it resolves the ambiguity between:
      !   (horiz_x, horiz_y, vertical, ungrid1)  → slice_rank==3
      !   (horiz_x, horiz_y, ungrid1,  ungrid2)  → slice_rank==2
      ! both of which are rank-4 native arrays with ungrid_count==2 from ESMF.
      slice_rank = condensed_slice_rank(field, _RC)

      if (typekind == ESMF_TYPEKIND_R4) then

         if (ungrid_count == 0) then
            ! No ungridded dim: wrap the full native array directly.
            _ASSERT(slice_index == 1, 'slice_index must be 1 for a field with no ungridded dimension.')
            nullify(fptr3d_r4)
            call ESMF_FieldGet(field, farrayPtr=fptr3d_r4, rc=status)
            if (status == 0 .and. associated(fptr3d_r4)) then
               ! horizontal + vertical
               slice_field = ESMF_FieldCreate(geom, farrayPtr=fptr3d_r4, &
                    datacopyflag=ESMF_DATACOPY_REFERENCE, name=field_name, _RC)
            else
               ! horizontal only
               call ESMF_FieldGet(field, farrayPtr=fptr2d_r4, _RC)
               slice_field = ESMF_FieldCreate(geom, farrayPtr=fptr2d_r4, &
                    datacopyflag=ESMF_DATACOPY_REFERENCE, name=field_name, _RC)
            end if

         else if (slice_rank == 3) then
            ! Vertical present: layout (horiz_x, horiz_y, vertical, ungrid1).
            ! Last dim is the sole ungridded dim regardless of ungrid_count.
            call ESMF_FieldGet(field, farrayPtr=fptr4d_r4, _RC)
            slice3d_r4 => fptr4d_r4(:,:,:,slice_index)
            slice_field = ESMF_FieldCreate(geom, farrayPtr=slice3d_r4, &
                 datacopyflag=ESMF_DATACOPY_REFERENCE, name=field_name, _RC)

         else
            ! No vertical (slice_rank==2).
            if (ungrid_count == 1) then
               ! Layout: (horiz_x, horiz_y, ungrid1)
               call ESMF_FieldGet(field, farrayPtr=fptr3d_r4, _RC)
               slice_r4 => fptr3d_r4(:,:,slice_index)
               slice_field = ESMF_FieldCreate(geom, farrayPtr=slice_r4, &
                    datacopyflag=ESMF_DATACOPY_REFERENCE, name=field_name, _RC)
            else
               ! Layout: (horiz_x, horiz_y, ungrid1, ungrid2)
               ! Convert flat slice_index to (i3, i4) in column-major order.
               call ESMF_FieldGet(field, farrayPtr=fptr4d_r4, _RC)
               i3 = mod(slice_index-1, size(fptr4d_r4, 3)) + 1
               i4 = (slice_index-1) / size(fptr4d_r4, 3) + 1
               slice_r4 => fptr4d_r4(:,:,i3,i4)
               slice_field = ESMF_FieldCreate(geom, farrayPtr=slice_r4, &
                    datacopyflag=ESMF_DATACOPY_REFERENCE, name=field_name, _RC)
            end if
         end if

      else if (typekind == ESMF_TYPEKIND_R8) then

         if (ungrid_count == 0) then
            _ASSERT(slice_index == 1, 'slice_index must be 1 for a field with no ungridded dimension.')
            nullify(fptr3d_r8)
            call ESMF_FieldGet(field, farrayPtr=fptr3d_r8, rc=status)
            if (status == 0 .and. associated(fptr3d_r8)) then
               slice_field = ESMF_FieldCreate(geom, farrayPtr=fptr3d_r8, &
                    datacopyflag=ESMF_DATACOPY_REFERENCE, name=field_name, _RC)
            else
               call ESMF_FieldGet(field, farrayPtr=fptr2d_r8, _RC)
               slice_field = ESMF_FieldCreate(geom, farrayPtr=fptr2d_r8, &
                    datacopyflag=ESMF_DATACOPY_REFERENCE, name=field_name, _RC)
            end if

         else if (slice_rank == 3) then
            ! Vertical present: layout (horiz_x, horiz_y, vertical, ungrid1).
            call ESMF_FieldGet(field, farrayPtr=fptr4d_r8, _RC)
            slice3d_r8 => fptr4d_r8(:,:,:,slice_index)
            slice_field = ESMF_FieldCreate(geom, farrayPtr=slice3d_r8, &
                 datacopyflag=ESMF_DATACOPY_REFERENCE, name=field_name, _RC)

         else
            ! No vertical (slice_rank==2).
            if (ungrid_count == 1) then
               call ESMF_FieldGet(field, farrayPtr=fptr3d_r8, _RC)
               slice_r8 => fptr3d_r8(:,:,slice_index)
               slice_field = ESMF_FieldCreate(geom, farrayPtr=slice_r8, &
                    datacopyflag=ESMF_DATACOPY_REFERENCE, name=field_name, _RC)
            else
               call ESMF_FieldGet(field, farrayPtr=fptr4d_r8, _RC)
               i3 = mod(slice_index-1, size(fptr4d_r8, 3)) + 1
               i4 = (slice_index-1) / size(fptr4d_r8, 3) + 1
               slice_r8 => fptr4d_r8(:,:,i3,i4)
               slice_field = ESMF_FieldCreate(geom, farrayPtr=slice_r8, &
                    datacopyflag=ESMF_DATACOPY_REFERENCE, name=field_name, _RC)
            end if
         end if

      else
         _FAIL('FieldSliceToField: unsupported typekind (expected R4 or R8).')
      end if

      ! Copy all ESMF attributes from the original field to the slice
      call ESMF_AttributeCopy(field, slice_field, _RC)

      ! TODO Remove ungridded-dimension-specific metadata so the
      ! slice field does not misrepresent its structure.

      _RETURN(_SUCCESS)
   end function FieldSliceToField

   ! --------------------------------------------------------------------------
   ! FieldGetPointerToSlice
   !
   ! Return a typed Fortran pointer directly into the native field array
   ! at the requested ungridded slice index.  The rank of the supplied
   ! pointer (2D or 3D) selects the slice layout.  When the field has no
   ! ungridded dimension, slice_index must be 1 and the full array is
   ! returned.
   ! --------------------------------------------------------------------------

   subroutine get_slice_r4(field, slice_index, ptr, rc)
      type(ESMF_Field), intent(inout) :: field
      integer, intent(in) :: slice_index
      real(kind=ESMF_KIND_R4), pointer, intent(out) :: ptr(:,:)
      integer, optional, intent(out) :: rc

      real(kind=ESMF_KIND_R4), pointer :: fptr3d(:,:,:)
      real(kind=ESMF_KIND_R4), pointer :: fptr2d(:,:)
      integer :: ungrid_count, status

      nullify(ptr)
      call ESMF_FieldGet(field, ungriddedDimCount=ungrid_count, _RC)
      _ASSERT(ungrid_count <= MAX_UNGRIDDED_DIMS, 'FieldGetPointerToSlice: ungrid_count > MAX_UNGRIDDED_DIMS is not supported.')
      if (ungrid_count == 0) then
         _ASSERT(slice_index == 1, 'slice_index must be 1 for a field with no ungridded dimension.')
         call ESMF_FieldGet(field, farrayPtr=fptr2d, _RC)
         ptr => fptr2d
      else
         call ESMF_FieldGet(field, farrayPtr=fptr3d, _RC)
         ptr => fptr3d(:,:,slice_index)
      end if
      _RETURN(_SUCCESS)
   end subroutine get_slice_r4

   subroutine get_slice_r8(field, slice_index, ptr, rc)
      type(ESMF_Field), intent(inout) :: field
      integer, intent(in) :: slice_index
      real(kind=ESMF_KIND_R8), pointer, intent(out) :: ptr(:,:)
      integer, optional, intent(out) :: rc

      real(kind=ESMF_KIND_R8), pointer :: fptr3d(:,:,:)
      real(kind=ESMF_KIND_R8), pointer :: fptr2d(:,:)
      integer :: ungrid_count, status

      nullify(ptr)
      call ESMF_FieldGet(field, ungriddedDimCount=ungrid_count, _RC)
      _ASSERT(ungrid_count <= MAX_UNGRIDDED_DIMS, 'FieldGetPointerToSlice: ungrid_count > MAX_UNGRIDDED_DIMS is not supported.')
      if (ungrid_count == 0) then
         _ASSERT(slice_index == 1, 'slice_index must be 1 for a field with no ungridded dimension.')
         call ESMF_FieldGet(field, farrayPtr=fptr2d, _RC)
         ptr => fptr2d
      else
         call ESMF_FieldGet(field, farrayPtr=fptr3d, _RC)
         ptr => fptr3d(:,:,slice_index)
      end if
      _RETURN(_SUCCESS)
   end subroutine get_slice_r8

   subroutine get_slice3d_r4(field, slice_index, ptr, rc)
      type(ESMF_Field), intent(inout) :: field
      integer, intent(in) :: slice_index
      real(kind=ESMF_KIND_R4), pointer, intent(out) :: ptr(:,:,:)
      integer, optional, intent(out) :: rc

      real(kind=ESMF_KIND_R4), pointer :: fptr4d(:,:,:,:)
      real(kind=ESMF_KIND_R4), pointer :: fptr3d(:,:,:)
      integer :: ungrid_count, status

      nullify(ptr)
      call ESMF_FieldGet(field, ungriddedDimCount=ungrid_count, _RC)
      _ASSERT(ungrid_count <= MAX_UNGRIDDED_DIMS, 'FieldGetPointerToSlice: ungrid_count > MAX_UNGRIDDED_DIMS is not supported.')
      if (ungrid_count == 0) then
         _ASSERT(slice_index == 1, 'slice_index must be 1 for a field with no ungridded dimension.')
         call ESMF_FieldGet(field, farrayPtr=fptr3d, _RC)
         ptr => fptr3d
      else
         call ESMF_FieldGet(field, farrayPtr=fptr4d, _RC)
         ptr => fptr4d(:,:,:,slice_index)
      end if
      _RETURN(_SUCCESS)
   end subroutine get_slice3d_r4

   subroutine get_slice3d_r8(field, slice_index, ptr, rc)
      type(ESMF_Field), intent(inout) :: field
      integer, intent(in) :: slice_index
      real(kind=ESMF_KIND_R8), pointer, intent(out) :: ptr(:,:,:)
      integer, optional, intent(out) :: rc

      real(kind=ESMF_KIND_R8), pointer :: fptr4d(:,:,:,:)
      real(kind=ESMF_KIND_R8), pointer :: fptr3d(:,:,:)
      integer :: ungrid_count, status

      nullify(ptr)
      call ESMF_FieldGet(field, ungriddedDimCount=ungrid_count, _RC)
      _ASSERT(ungrid_count <= MAX_UNGRIDDED_DIMS, 'FieldGetPointerToSlice: ungrid_count > MAX_UNGRIDDED_DIMS is not supported.')
      if (ungrid_count == 0) then
         _ASSERT(slice_index == 1, 'slice_index must be 1 for a field with no ungridded dimension.')
         call ESMF_FieldGet(field, farrayPtr=fptr3d, _RC)
         ptr => fptr3d
      else
         call ESMF_FieldGet(field, farrayPtr=fptr4d, _RC)
         ptr => fptr4d(:,:,:,slice_index)
      end if
      _RETURN(_SUCCESS)
   end subroutine get_slice3d_r8

   ! --------------------------------------------------------------------------
   ! FieldApplyUserRoutine
   !
   ! Apply a user routine to every ungridded slice of a field.  For each
   ! slice index k, FieldSliceToField is called to create a transient
   ! ESMF_Field that wraps that slice; this field is passed to userRoutine
   ! and then destroyed.  The number of slices is derived from the actual
   ! last dimension(s) of the native array, disambiguated by
   ! condensed_slice_rank to correctly handle the vertical-vs-second-
   ! ungridded-dim ambiguity.
   !
   ! ungrid_count > MAX_UNGRIDDED_DIMS is caught by an assertion before
   ! the slice loop.
   ! --------------------------------------------------------------------------
   subroutine FieldApplyUserRoutine(field, userRoutine, unusable, userrc, rc)
      type(ESMF_Field), intent(inout) :: field
      procedure(I_FieldSliceRoutine) :: userRoutine
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: userrc
      integer, optional, intent(out) :: rc

      type(ESMF_Field) :: slice_field
      type(ESMF_FieldStatus_Flag) :: field_status
      type(ESMF_TypeKind_Flag) :: typekind
      integer :: k, n_slices, status, user_status
      integer :: ungrid_count, slice_rank

      ! Native array pointers used only to determine n_slices.
      real(kind=ESMF_KIND_R4), pointer :: fptr3d_r4(:,:,:)
      real(kind=ESMF_KIND_R8), pointer :: fptr3d_r8(:,:,:)
      real(kind=ESMF_KIND_R4), pointer :: fptr4d_r4(:,:,:,:)
      real(kind=ESMF_KIND_R8), pointer :: fptr4d_r8(:,:,:,:)

      if (present(userrc)) userrc = 0
      user_status = 0

      call ESMF_FieldGet(field, status=field_status, _RC)
      _RETURN_UNLESS(field_status == ESMF_FIELDSTATUS_COMPLETE)

      call ESMF_FieldGet(field, typekind=typekind, &
           ungriddedDimCount=ungrid_count, _RC)

      _ASSERT(ungrid_count <= MAX_UNGRIDDED_DIMS, 'FieldApplyUserRoutine: ungrid_count > MAX_UNGRIDDED_DIMS is not supported.')

      if (ungrid_count == 0) then
         n_slices = 1
      else
         ! Use condensed_slice_rank to resolve vertical-vs-second-ungridded
         ! ambiguity before computing n_slices from native array dimensions.
         slice_rank = condensed_slice_rank(field, _RC)

         if (typekind == ESMF_TYPEKIND_R4) then
            if (slice_rank == 3) then
               ! Layout: (horiz_x, horiz_y, vertical, ungrid1)
               ! Last dim is the sole ungridded dim.
               call ESMF_FieldGet(field, farrayPtr=fptr4d_r4, _RC)
               n_slices = size(fptr4d_r4, 4)
            else if (ungrid_count == 1) then
               ! Layout: (horiz_x, horiz_y, ungrid1)
               call ESMF_FieldGet(field, farrayPtr=fptr3d_r4, _RC)
               n_slices = size(fptr3d_r4, 3)
            else
               ! Layout: (horiz_x, horiz_y, ungrid1, ungrid2)
               call ESMF_FieldGet(field, farrayPtr=fptr4d_r4, _RC)
               n_slices = size(fptr4d_r4, 3) * size(fptr4d_r4, 4)
            end if
         else if (typekind == ESMF_TYPEKIND_R8) then
            if (slice_rank == 3) then
               ! Layout: (horiz_x, horiz_y, vertical, ungrid1)
               call ESMF_FieldGet(field, farrayPtr=fptr4d_r8, _RC)
               n_slices = size(fptr4d_r8, 4)
            else if (ungrid_count == 1) then
               ! Layout: (horiz_x, horiz_y, ungrid1)
               call ESMF_FieldGet(field, farrayPtr=fptr3d_r8, _RC)
               n_slices = size(fptr3d_r8, 3)
            else
               ! Layout: (horiz_x, horiz_y, ungrid1, ungrid2)
               call ESMF_FieldGet(field, farrayPtr=fptr4d_r8, _RC)
               n_slices = size(fptr4d_r8, 3) * size(fptr4d_r8, 4)
            end if
         else
            _FAIL('FieldApplyUserRoutine: unsupported typekind (expected R4 or R8).')
         end if
      end if

      do k = 1, n_slices
         user_status = 0
         ! Build a transient ESMF_Field pointing into slice k of the parent.
         slice_field = FieldSliceToField(field, k, _RC)
         call userRoutine(slice_field, rc=user_status)
         call ESMF_FieldDestroy(slice_field, _RC)
         if (user_status /= 0) exit
      end do

      if (present(userrc)) userrc = user_status

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine FieldApplyUserRoutine

end module mapl_FieldApplyUserRoutine_mod
