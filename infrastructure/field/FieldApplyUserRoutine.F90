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
! Two entry points are provided:
!   * FieldGetPointerToSlice - return a typed (R4 or R8) pointer to a
!     single slice, for callers that manage their own iteration.  The
!     rank of the supplied pointer (2D or 3D) selects the slice layout.
!   * FieldApplyUserRoutine  - apply a user routine to every slice of a
!     field.  The slice is passed as an unlimited-polymorphic,
!     assumed-rank actual argument so that a single interface handles
!     both R4 and R8 fields and both 2D and 3D slices; the user routine
!     recovers the concrete rank with SELECT RANK and the concrete kind
!     with SELECT TYPE.
module mapl_FieldApplyUserRoutine_mod

   use ESMF, only: ESMF_Field, ESMF_FieldGet
   use ESMF, only: ESMF_FieldStatus_Flag, ESMF_FIELDSTATUS_COMPLETE
   use ESMF, only: ESMF_TypeKind_Flag, ESMF_TYPEKIND_R4, ESMF_TYPEKIND_R8
   use ESMF, only: ESMF_KIND_R4, ESMF_KIND_R8
   use ESMF, only: operator(==)
   use mapl_FieldCondensedArray_mod, only: assign_fptr_condensed_array
   use mapl_FieldCondensedArray_mod, only: condensed_slice_rank
   use mapl_KeywordEnforcer_mod, only: KeywordEnforcer
   use mapl_ErrorHandling_mod

   implicit none(type, external)
   private

   public :: FieldGetPointerToSlice
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

   ! The user routine applied to each slice.  The slice is an unlimited
   ! polymorphic, assumed-rank argument so that one interface serves both
   ! R4 and R8 fields and both 2D and 3D slices.  It is intent(inout) so
   ! that the user routine can modify the field data in place.
   abstract interface
      subroutine I_FieldSliceRoutine(slice, rc)
         class(*), intent(inout) :: slice(..)
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
            do k = 1, n_slices
               user_status = 0
               call userRoutine(condensed3d_r4(:,:,:,k), rc=user_status)
               if (user_status /= 0) exit
            end do
         else
            call assign_fptr_condensed_array(field, condensed_r4, _RC)
            n_slices = size(condensed_r4, 3)
            do k = 1, n_slices
               user_status = 0
               call userRoutine(condensed_r4(:,:,k), rc=user_status)
               if (user_status /= 0) exit
            end do
         end if
      else if (typekind == ESMF_TYPEKIND_R8) then
         if (slice_rank == 3) then
            call assign_fptr_condensed_array(field, condensed3d_r8, _RC)
            n_slices = size(condensed3d_r8, 4)
            do k = 1, n_slices
               user_status = 0
               call userRoutine(condensed3d_r8(:,:,:,k), rc=user_status)
               if (user_status /= 0) exit
            end do
         else
            call assign_fptr_condensed_array(field, condensed_r8, _RC)
            n_slices = size(condensed_r8, 3)
            do k = 1, n_slices
               user_status = 0
               call userRoutine(condensed_r8(:,:,k), rc=user_status)
               if (user_status /= 0) exit
            end do
         end if
      else
         _FAIL('Unsupported typekind for FieldApplyUserRoutine (expected R4 or R8).')
      end if

      if (present(userrc)) userrc = user_status

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine FieldApplyUserRoutine

end module mapl_FieldApplyUserRoutine_mod
