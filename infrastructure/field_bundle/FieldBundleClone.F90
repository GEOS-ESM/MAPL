#include "MAPL.h"

module mapl_FieldBundleClone_mod
   use mapl_FieldUtils, only: FieldClone
   use mapl_FieldBundleCreate_mod, only: FieldBundleCreate
   use mapl_ErrorHandling_mod
   use mapl_KeywordEnforcer_mod
   use esmf
   implicit none(type, external)
   private
   public :: FieldBundleClone

   interface FieldBundleClone
      module procedure :: clone_bundle
   end interface

contains

   ! Create bundle_out as a structural (metadata + independently
   ! allocated memory, no data) clone of bundle_in: each field in
   ! bundle_out is produced by FieldClone() from the corresponding
   ! field in bundle_in. Field data values are NOT copied.
   subroutine clone_bundle(bundle_in, bundle_out, unusable, name, rc)
      type(ESMF_FieldBundle), intent(inout) :: bundle_in
      type(ESMF_FieldBundle), intent(out) :: bundle_out
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: name
      integer, optional, intent(out) :: rc

      type(ESMF_Field), allocatable :: fields_in(:)
      type(ESMF_Field) :: field_out
      character(len=ESMF_MAXSTR) :: field_name
      integer :: field_count, i, status

      bundle_out = FieldBundleCreate(name=name, _RC)

      call ESMF_FieldBundleGet(bundle_in, fieldCount=field_count, _RC)
      if (field_count == 0) then
         _RETURN(_SUCCESS)
      end if

      allocate(fields_in(field_count), _STAT)
      call ESMF_FieldBundleGet(bundle_in, itemorderflag=ESMF_ITEMORDER_ABC, fieldList=fields_in, _RC)

      do i = 1, field_count
         ! Preserve the original field name on the clone (FieldClone
         ! appends a "_clone" suffix by default) so downstream
         ! name-matching (e.g. MAPL_FieldBundleCopy) still lines up.
         call ESMF_FieldGet(fields_in(i), name=field_name, _RC)
         call FieldClone(fields_in(i), field_out, name=trim(field_name), _RC)
         call ESMF_FieldBundleAdd(bundle_out, [field_out], _RC)
      end do

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine clone_bundle

end module mapl_FieldBundleClone_mod
