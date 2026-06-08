#include "MAPL.h"
#include "unused_dummy.H"

module mapl_FieldBundleGetPointer_mod

   use ESMF
   use mapl_ErrorHandling_mod
   use, intrinsic :: iso_fortran_env, only: REAL64

   implicit none(type,external)
   private

   public :: FieldBundleGetPointerToData

   interface FieldBundleGetPointerToData
      module procedure FieldBundleGetPointerToDataByIndex2
      module procedure FieldBundleGetPointerToDataByIndex3
      module procedure FieldBundleGetPointerToDataByName2
      module procedure FieldBundleGetPointerToDataByName3
      module procedure FieldBundleGetPointerToR8DataByIndex2
      module procedure FieldBundleGetPointerToR8DataByIndex3
      module procedure FieldBundleGetPointerToR8DataByName2
      module procedure FieldBundleGetPointerToR8DataByName3
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
      nullify(ptr)
      if (field_status == ESMF_FIELDSTATUS_COMPLETE) then
         call ESMF_FieldGet(field, 0, ptr, _RC)
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
      nullify(ptr)
      if (field_status == ESMF_FIELDSTATUS_COMPLETE) then
         call ESMF_FieldGet(field, 0, ptr, _RC)
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
      nullify(ptr)
      if (field_status == ESMF_FIELDSTATUS_COMPLETE) then
         call ESMF_FieldGet(field, 0, ptr, _RC)
      end if

     _RETURN(_SUCCESS)
   end subroutine FieldBundleGetPointerToDataByName2

   subroutine FieldBundleGetPointerToDataByName3(bundle, name, ptr, rc)
      type(ESMF_FieldBundle), intent(inout) :: bundle !ALT: intent(in)
      character(len=*), intent(in) :: name
      real, pointer, intent(inout) :: ptr(:,:,:)
      integer, optional, intent(out):: rc

      type(ESMF_Field) :: field
      type(ESMF_FieldStatus_Flag) :: field_status
      integer :: status

      call ESMF_FieldBundleGet(bundle, name, field=field, _RC)
      call ESMF_FieldGet(field, status=field_status, _RC)
      nullify(ptr)
      if (field_status == ESMF_FIELDSTATUS_COMPLETE) then
         call ESMF_FieldGet(field, 0, ptr, _RC)
      end if

     _RETURN(_SUCCESS)
   end subroutine FieldBundleGetPointerToDataByName3

   subroutine FieldBundleGetPointerToR8DataByIndex2(bundle, index, ptr, rc)
      type(ESMF_FieldBundle), intent(inout) :: bundle !ALT: intent(in)
      integer, intent(in) :: index
      real(REAL64), pointer, intent(inout) :: ptr(:,:)
      integer, optional, intent(out):: rc

      type(ESMF_Field) :: field
      type(ESMF_FieldStatus_Flag) :: field_status
      integer :: status

      ! ESMF 5 reorders items, be careful!

      call ESMF_FieldBundleGet(bundle, index, field, _RC)
      call ESMF_FieldGet(field, status=field_status, _RC)
      nullify(ptr)
      if (field_status == ESMF_FIELDSTATUS_COMPLETE) then
         call ESMF_FieldGet(field, 0, ptr, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine FieldBundleGetPointerToR8DataByIndex2

   subroutine FieldBundleGetPointerToR8DataByIndex3(bundle, index, ptr, rc)
      type(ESMF_FieldBundle), intent(inout) :: bundle !ALT: intent(in)
      integer, intent(in) :: index
      real(REAL64), pointer, intent(inout) :: ptr(:,:,:)
      integer, optional, intent(out):: rc

      type(ESMF_Field) :: field
      type(ESMF_FieldStatus_Flag) :: field_status
      integer :: status

      ! ESMF 5 reorders items, be careful!

      call ESMF_FieldBundleGet(bundle, index, field, _RC)
      call ESMF_FieldGet(field, status=field_status, _RC)
      nullify(ptr)
      if (field_status == ESMF_FIELDSTATUS_COMPLETE) then
         call ESMF_FieldGet(field, 0, ptr, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine FieldBundleGetPointerToR8DataByIndex3

   subroutine FieldBundleGetPointerToR8DataByName2(bundle, name, ptr, rc)
      type(ESMF_FieldBundle), intent(inout) :: bundle !ALT: intent(in)
      character(len=*), intent(in) :: name
      real(REAL64), pointer, intent(inout) :: ptr(:,:)
      integer, optional, intent(out):: rc

      type(ESMF_Field) :: field
      type(ESMF_FieldStatus_Flag) :: field_status
      integer :: status

      call ESMF_FieldBundleGet(bundle, name, field=field, _RC)
      call ESMF_FieldGet(field, status=field_status, _RC)
      nullify(ptr)
      if (field_status == ESMF_FIELDSTATUS_COMPLETE) then
         call ESMF_FieldGet(field, 0, ptr, _RC)
      end if

     _RETURN(_SUCCESS)
   end subroutine FieldBundleGetPointerToR8DataByName2

   subroutine FieldBundleGetPointerToR8DataByName3(bundle, name, ptr, rc)
      type(ESMF_FieldBundle), intent(inout) :: bundle !ALT: intent(in)
      character(len=*), intent(in) :: name
      real(REAL64), pointer, intent(inout) :: ptr(:,:,:)
      integer, optional, intent(out):: rc

      type(ESMF_Field) :: field
      type(ESMF_FieldStatus_Flag) :: field_status
      integer :: status

      call ESMF_FieldBundleGet(bundle, name, field=field, _RC)
      call ESMF_FieldGet(field, status=field_status, _RC)
      nullify(ptr)
      if (field_status == ESMF_FIELDSTATUS_COMPLETE) then
         call ESMF_FieldGet(field, 0, ptr, _RC)
      end if

     _RETURN(_SUCCESS)
   end subroutine FieldBundleGetPointerToR8DataByName3

end module mapl_FieldBundleGetPointer_mod
