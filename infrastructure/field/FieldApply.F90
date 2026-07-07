#include "MAPL.h"
#include "unused_dummy.H"

! Support for service providers that would like to handle fields with
! ungridded (and/or vertical) dimensions in the natural fashion, i.e. as
! a collection of 2D "slices".
!
! An arbitrary-rank field is collapsed into a condensed rank-3 array of
! shape (horizontal, vertical, ungridded) using the machinery in
! FieldCondensedArray.F90.  A "slice" is then simply the (horizontal,
! vertical) plane for a fixed ungridded index, so iterating over the
! ungridded dimensions of arbitrary rank reduces to a single loop bound.
!
! Two entry points are provided:
!   * FieldGetPointerToSlice - return a typed (R4 or R8) pointer to a
!     single slice, for callers that manage their own iteration.
!   * FieldApplyUserRoutine  - apply a user routine to every slice of a
!     field.  The slice is passed as an unlimited-polymorphic pointer so
!     that a single interface handles both R4 and R8 fields; the user
!     routine recovers the concrete kind with SELECT TYPE.
module mapl_FieldApply_mod

   use ESMF, only: ESMF_Field, ESMF_FieldGet
   use ESMF, only: ESMF_FieldStatus_Flag, ESMF_FIELDSTATUS_COMPLETE
   use ESMF, only: ESMF_TypeKind_Flag, ESMF_TYPEKIND_R4, ESMF_TYPEKIND_R8
   use ESMF, only: ESMF_KIND_R4, ESMF_KIND_R8
   use mapl_FieldCondensedArray_mod, only: assign_fptr_condensed_array
   use mapl_KeywordEnforcer_mod, only: KeywordEnforcer
   use mapl_ErrorHandling_mod

   implicit none(type, external)
   private

   public :: FieldGetPointerToSlice
   public :: FieldApplyUserRoutine
   public :: I_FieldSliceRoutine

   ! Return a pointer to a single 2D (horizontal x vertical) slice of a
   ! field.  Overloaded for R4 and R8.  A field with no ungridded
   ! dimensions has exactly one slice; a genuinely 2D field yields a
   ! slice with a trivial (size 1) vertical extent.
   interface FieldGetPointerToSlice
      module procedure get_slice_r4
      module procedure get_slice_r8
   end interface FieldGetPointerToSlice

   ! The user routine applied to each slice.  The slice is unlimited
   ! polymorphic so that one interface serves both R4 and R8 fields.
   abstract interface
      subroutine I_FieldSliceRoutine(slice, rc)
         class(*), pointer, intent(in) :: slice(:,:)
         integer, optional, intent(out) :: rc
      end subroutine I_FieldSliceRoutine
   end interface

contains

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

   subroutine FieldApplyUserRoutine(field, userRoutine, unusable, userrc, rc)
      type(ESMF_Field), intent(inout) :: field
      procedure(I_FieldSliceRoutine) :: userRoutine
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: userrc
      integer, optional, intent(out) :: rc

      real(kind=ESMF_KIND_R4), pointer :: condensed_r4(:,:,:)
      real(kind=ESMF_KIND_R8), pointer :: condensed_r8(:,:,:)
      class(*), pointer :: slice(:,:)
      type(ESMF_FieldStatus_Flag) :: field_status
      type(ESMF_TypeKind_Flag) :: typekind
      integer :: k, n_slices, status, user_status

      if (present(userrc)) userrc = 0
      user_status = 0

      call ESMF_FieldGet(field, status=field_status, _RC)
      _RETURN_UNLESS(field_status == ESMF_FIELDSTATUS_COMPLETE)

      call ESMF_FieldGet(field, typekind=typekind, _RC)

      if (typekind == ESMF_TYPEKIND_R4) then
         call assign_fptr_condensed_array(field, condensed_r4, _RC)
         n_slices = size(condensed_r4, 3)
         do k = 1, n_slices
            slice => condensed_r4(:,:,k)
            user_status = 0
            call userRoutine(slice, rc=user_status)
            if (user_status /= 0) exit
         end do
      else if (typekind == ESMF_TYPEKIND_R8) then
         call assign_fptr_condensed_array(field, condensed_r8, _RC)
         n_slices = size(condensed_r8, 3)
         do k = 1, n_slices
            slice => condensed_r8(:,:,k)
            user_status = 0
            call userRoutine(slice, rc=user_status)
            if (user_status /= 0) exit
         end do
      else
         _FAIL('Unsupported typekind for FieldApplyUserRoutine (expected R4 or R8).')
      end if

      if (present(userrc)) userrc = user_status

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine FieldApplyUserRoutine

end module mapl_FieldApply_mod
