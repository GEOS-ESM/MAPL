#include "MAPL.h"
#include "unused_dummy.H"

module mapl3g_FieldBundleDestroy
   use esmf
   use MAPL_ExceptionHandling
   use mapl_KeywordEnforcer
   use MAPL_FieldUtils, only : FieldsDestroy

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
      if(present(destroy_contents)) destroying_contents = destroy_contents
      if(destroying_contents) then
         call remove_bundle_fields(bundle, fieldList, _RC)
         call FieldsDestroy(fieldList, _RC)
      end if
      call ESMF_FieldBundleGet(bundle, name=name, _RC)
      call ESMF_FieldBundleDestroy(bundle, _RC)
      call ESMF_FieldBundleValidate(bundle, rc=status)
      _ASSERT(status /= ESMF_SUCCESS, 'Bundle "' // trim(name) // '" was not destroyed.')
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine destroy_bundle

   subroutine remove_bundle_fields(bundle, fields, rc)
      type(ESMF_FieldBundle), intent(inout) :: bundle
      type(ESMF_Field), allocatable, intent(inout) :: fields(:)
      integer, optional, intent(out) :: rc
      integer :: status, fieldCount
      character(len=ESMF_MAXSTR), allocatable :: fieldNameList(:)

      call ESMF_FieldBundleGet(bundle, fieldCount=fieldCount, _RC)
      allocate(fields(fieldCount))
      allocate(fieldNameList(fieldCount))
      call ESMF_FieldBundleGet(bundle, fieldList=fields, fieldNameList=fieldNameList, _RC)
      call ESMF_FieldBundleRemove(bundle, fieldNameList=fieldNameList, _RC)
      call ESMF_FieldBundleGet(bundle, fieldCount=fieldCount, _RC)
      _ASSERT(fieldCount == 0, 'Some fields were not removed.')
      _RETURN(_SUCCESS)

   end subroutine remove_bundle_fields

end module mapl3g_FieldBundleDestroy
