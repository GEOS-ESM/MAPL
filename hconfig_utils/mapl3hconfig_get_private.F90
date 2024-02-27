#include "MAPL_ErrLog.h"
module mapl3hconfig_get_private
   use :: esmf, only: ESMF_HConfig, ESMF_HConfigIsDefined
   use :: esmf, only: ESMF_KIND_I4, ESMF_KIND_I8
   use :: esmf, only: ESMF_KIND_R4, ESMF_KIND_R8
   use hconfig_value_mod
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling

   implicit none

   public :: get_value

   interface get_value
      module procedure :: get_value_scalar
   end interface get_value

contains
      
   logical function HConfig_Keystring_found(hconfig, keystring, rc) result(found)
      type(ESMF_HConfig), intent(inout) :: hconfig
      character(len=*), intent(in) :: keystring
      integer, optional, intent(out) :: rc
      integer :: status
      
      found = ESMF_HConfigIsDefined(hconfig, keyString=keystring, rc=status)
      _VERIFY(status)
   
      _RETURN(_SUCCESS)
   end function HConfig_Keystring_found

   subroutine get_value_scalar(hconfig, keystring, value, unusable, found, default, equals_default, typestring, valuestring, rc)
      type(ESMF_HConfig), intent(inout) :: hconfig
      character(len=*), intent(in) :: keystring
      class(*), intent(inout) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: found
      class(*), optional, intent(in) :: default
      logical, optional, intent(out) :: equals_default
      character(len=:), allocatable, optional, intent(inout) :: typestring
      character(len=:), allocatable, optional, intent(inout) :: valuestring
      integer, intent(out) :: rc

      integer :: status
      class(HConfigValue), allocatable :: hconfig_value
      logical :: keystring_found

      if(present(default)) then
         _ASSERT(same_type_as(value, default), 'value and default are different types.')
      else
         _ASSERT(.not. (present(equals_default)),  'equals_default requires default')
      end if
      keystring_found = HConfig_Keystring_found(hconfig, keystring, rc=status)
      _VERIFY(status)

      _RETURN_UNLESS(keystring_found .or. present(default))

      select type(value)
      type is (integer(kind=ESMF_KIND_I4))
         hconfig_value = HConfigValueI4(value, default)
      type is (integer(kind=ESMF_KIND_I8))
         hconfig_value = HConfigValueI8(value, default)
      type is (real(kind=ESMF_KIND_R4))
         hconfig_value = HConfigValueR4(value, default)
      type is (real(kind=ESMF_KIND_R8))
         hconfig_value = HConfigValueR8(value, default)
      type is (logical)
         hconfig_value = HConfigValueLogical(value, default)
      type is (character(len=*))
         hconfig_value = HConfigValueString(value, default)
      class default
         _FAIL('Unsupported type for conversion')
      end select

      if(keystring_found) then
         hconfig_value%hconfig_ = hconfig
         hconfig_value%keystring_ = keystring
         call hconfig_value%set_from_hconfig()
         status = hconfig_value%last_status_
         _ASSERT(status == 0, 'Error setting value from ESMF_HConfig')
         hconfig_value%value_equals_default_ = hconfig_value%value_equals_default()
      else
         call hconfig_value%set_from_default()
         hconfig_value%value_equals_default_ = .TRUE.
      end if

      if(present(valuestring)) then
         call hconfig_value%get_valuestring(valuestring)
         status = hconfig_value%last_status_
         write(*, *) 'status == ', status
         _ASSERT(status == 0, 'Error getting valuestring')
      end if

      if(present(typestring)) typestring = hconfig_value%typestring_
      if(present(equals_default)) equals_default = hconfig_value%value_equals_default_
      if(present(found)) found = keystring_found

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine get_value_scalar

end module mapl3hconfig_get_private
