#include "MAPL.h"
#include "unused_dummy.H"

module mapl3g_FieldBundleCopy
   use MAPL_FieldUtils, only: FieldCopy
   use MAPL_ExceptionHandling
   use MAPL_KeywordEnforcer
!   use esmf, only: ESMF_Field, ESMF_FieldBundle, ESMF_MAXSTR
   use esmf
   implicit none(type, external)
   private
   public :: FieldBundleCopy

   interface FieldBundleCopy
      module procedure :: copy_bundle
   end interface

contains

   subroutine copy_bundle(bundle_in, bundle_out, unusable, ignore_names, rc)
      type(ESMF_FieldBundle), intent(inout) :: bundle_in, bundle_out
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(in) :: ignore_names
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: field_count, n, i
      character(len=ESMF_MAXSTR), allocatable :: names_in(:), names_out(:)
      type(ESMF_Field), allocatable :: fields_in(:), fields_out(:)
      logical :: check_names

      call ESMF_FieldBundleGet(bundle_in, fieldCount=field_count, _RC)
      call ESMF_FieldBundleGet(bundle_out, fieldCount=n, _RC)
      _ASSERT(field_count==n, 'The fieldCount values do not match.')

      check_names = .TRUE.
      if(present(ignore_names)) check_names = .not. ignore_names

      if(check_names) then
         allocate(names_in(n))
         allocate(names_out(n))
         call ESMF_FieldBundleGet(bundle_in, itemorderflag=ESMF_ITEMORDER_ABC, fieldNameList=names_in, _RC)
         call ESMF_FieldBundleGet(bundle_out, itemorderflag=ESMF_ITEMORDER_ABC, fieldNameList=names_out, _RC)
         _ASSERT(all(names_in == names_out), 'The field names do not match.')
      end if            

      allocate(fields_in(n))
      allocate(fields_out(n))
      call ESMF_FieldBundleGet(bundle_in, itemorderflag=ESMF_ITEMORDER_ABC, fieldList=fields_in, _RC)
      call ESMF_FieldBundleGet(bundle_out, itemorderflag=ESMF_ITEMORDER_ABC, fieldList=fields_out, _RC)
      do i = 1, n
         call FieldCopy(fields_in(i), fields_out(i), _RC)
      end do

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine copy_bundle

end module mapl3g_FieldBundleCopy
