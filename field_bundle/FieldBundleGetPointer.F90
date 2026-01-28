#include "MAPL.h"
#include "unused_dummy.H"

module mapl3g_FieldBundleGetPointer

   use ESMF
   use MAPL_ErrorHandling
   use, intrinsic :: iso_fortran_env, only: real32, real64

   implicit none(type,external)
   private

   public :: FieldBundleGetPointerToData

   interface FieldBundleGetPointerToData
      module procedure FieldBundleGetPointerToDataByIndex2
      module procedure FieldBundleGetPointerToDataByIndex3
      module procedure FieldBundleGetPointerToDataByName2
      module procedure FieldBundleGetPointerToDataByName3
   end interface FieldBundleGetPointerToData

contains

   subroutine FieldBundleGetPointerToDataByIndex2(bundle, index, ptr, rc)
      type(ESMF_FieldBundle), intent(inout) :: bundle !ALT: intent(in)
      integer, intent(in) :: index
      real, pointer, intent(inout) :: ptr(:,:)
      integer, optional, intent(out):: rc

      type(ESMF_Field) :: field
      type(ESMF_FieldStatus_Flag) :: field_status
      integer :: status

      ! ESMF 5 reorders items, be careful!

      call ESMF_FieldBundleGet(bundle, index, field, _RC)
      call ESMF_FieldGet(field, status=field_status, _RC)
      if (field_status == ESMF_FIELDSTATUS_COMPLETE) then
         call ESMF_FieldGet(field, 0, ptr, _RC)
      else
         nullify(ptr)
      end if

      _RETURN(_SUCCESS)
   end subroutine FieldBundleGetPointerToDataByIndex2

   subroutine FieldBundleGetPointerToDataByIndex3(bundle, index, ptr, rc)
      type(ESMF_FieldBundle), intent(inout) :: bundle !ALT: intent(in)
      integer, intent(in) :: index
      real, pointer, intent(inout) :: ptr(:,:,:)
      integer, optional, intent(out):: rc

      type(ESMF_Field) :: field
      type(ESMF_FieldStatus_Flag) :: field_status
      integer :: status

      ! ESMF 5 reorders items, be careful!

      call ESMF_FieldBundleGet(bundle, index, field, _RC)
      call ESMF_FieldGet(field, status=field_status, _RC)
      if (field_status == ESMF_FIELDSTATUS_COMPLETE) then
         call ESMF_FieldGet(field, 0, ptr, _RC)
      else
         nullify(ptr)
      end if

      _RETURN(_SUCCESS)
   end subroutine FieldBundleGetPointerToDataByIndex3

   subroutine FieldBundleGetPointerToDataByName2(bundle, name, ptr, rc)
      type(ESMF_FieldBundle), intent(inout) :: bundle !ALT: intent(in)
      character(len=*), intent(in) :: name
      real, pointer, intent(inout) :: ptr(:,:)
      integer, optional, intent(out):: rc

      type(ESMF_field) :: field
      type(ESMF_FieldStatus_Flag) :: field_status
      integer :: status

      call ESMF_FieldBundleGet(bundle, name, field=field, _RC)
      call ESMF_FieldGet(field, status=field_status, _RC)
      if (field_status == ESMF_FIELDSTATUS_COMPLETE) then
         call ESMF_FieldGet(field, 0, ptr, _RC)
      else
         nullify(ptr)
      end if

     _RETURN(_SUCCESS)
   end subroutine FieldBundleGetPointerToDataByName2

   subroutine FieldBundleGetPointerToDataByName3(BUNDLE,NAME,PTR,RC)
      type(ESMF_FieldBundle), intent(inout) :: bundle !ALT: intent(in)
      character(len=*), intent(in) :: name
      real, pointer, intent(inout) :: ptr(:,:,:)
      integer, optional, intent(out):: rc

      type(ESMF_Field) :: field
      type(ESMF_FieldStatus_Flag) :: field_status
      integer :: status

      call ESMF_FieldBundleGet(bundle, name, field=field, _RC)
      call ESMF_FieldGet(field, status=field_status, _RC)
      if (field_status == ESMF_FIELDSTATUS_COMPLETE) then
         call ESMF_FieldGet(field, 0, ptr, _RC)
      else
         nullify(ptr)
      end if

     _RETURN(_SUCCESS)
   end subroutine FieldBundleGetPointerToDataByName3

end module mapl3g_FieldBundleGetPointer
