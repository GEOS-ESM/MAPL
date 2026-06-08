#include "MAPL.h"
#include "unused_dummy.H"

module mapl_FieldBundleDestroy_mod
   use esmf
   use mapl_ErrorHandling_mod
   use mapl_KeywordEnforcer_mod
   use mapl_FieldUtils, only : FieldsDestroy
   use mapl_FieldBundleGet_mod, only: MAPL_FieldBundleGet => FieldBundleGet

   implicit none(type, external)

   private
   public :: MAPL_FieldBundleDestroy

   interface MAPL_FieldBundleDestroy
      procedure :: destroy_bundle
   end interface MAPL_FieldBundleDestroy

contains

   subroutine destroy_bundle(bundle, unusable, destroy_contents, rc)
      type(ESMF_FieldBundle), intent(inout) :: bundle
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(in) :: destroy_contents
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_Field), allocatable :: fieldList(:)
      character(len=ESMF_MAXSTR) :: name
      logical :: destroying_contents

      destroying_contents = .FALSE.
      if (present(destroy_contents)) destroying_contents = destroy_contents
      if (destroying_contents) then
         call MAPL_FieldBundleGet(bundle, fieldList=fieldList, _RC)
         call FieldsDestroy(fieldList, _RC)
      end if
      call ESMF_FieldBundleGet(bundle, name=name, _RC)
      call ESMF_FieldBundleDestroy(bundle, _RC)
      call ESMF_FieldBundleValidate(bundle, rc=status)
      _ASSERT(status /= ESMF_SUCCESS, 'Bundle "' // trim(name) // '" was not destroyed.')
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine destroy_bundle

end module mapl_FieldBundleDestroy_mod
