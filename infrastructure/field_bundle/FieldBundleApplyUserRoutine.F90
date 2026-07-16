#include "MAPL.h"
#include "unused_dummy.H"

! Apply a user routine to each ungridded slice of every field in a
! FieldBundle.
!
! This is the bundle-level driver requested by service providers that
! want to handle fields with ungridded (and/or vertical) dimensions as a
! collection of slices.  Each field is delegated to FieldApplyUserRoutine
! (see infrastructure/field/FieldApplyUserRoutine.F90), which performs
! the per-slice iteration.
!
! For each slice, FieldApplyUserRoutine calls FieldSliceToField to
! construct a transient ESMF_Field whose data is a pointer into the
! corresponding ungridded slice of the parent field.  The user routine
! therefore always receives a proper ESMF_Field (with no ungridded
! dimension) and may use the full ESMF field API.
module mapl_FieldBundleApplyUserRoutine_mod

   use ESMF, only: ESMF_FieldBundle, ESMF_FieldBundleGet
   use ESMF, only: ESMF_Field
   use mapl_FieldApplyUserRoutine_mod, only: FieldApplyUserRoutine
   use mapl_FieldApplyUserRoutine_mod, only: I_FieldSliceRoutine
   use mapl_KeywordEnforcer_mod, only: KeywordEnforcer
   use mapl_ErrorHandling_mod

   implicit none(type, external)
   private

   public :: FieldBundleApplyUserRoutine

contains

   subroutine FieldBundleApplyUserRoutine(bundle, userRoutine, unusable, userrc, rc)
      type(ESMF_FieldBundle), intent(inout) :: bundle
      procedure(I_FieldSliceRoutine) :: userRoutine
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: userrc
      integer, optional, intent(out) :: rc

      type(ESMF_Field) :: field
      integer :: i, field_count, status, user_status

      if (present(userrc)) userrc = 0
      user_status = 0

      call ESMF_FieldBundleGet(bundle, fieldCount=field_count, _RC)
      do i = 1, field_count
         ! ESMF 5 reorders items; retrieve by position index to be safe.
         call ESMF_FieldBundleGet(bundle, i, field, _RC)
         user_status = 0
         call FieldApplyUserRoutine(field, userRoutine, userrc=user_status, _RC)
         if (user_status /= 0) exit
      end do

      if (present(userrc)) userrc = user_status

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine FieldBundleApplyUserRoutine

end module mapl_FieldBundleApplyUserRoutine_mod
