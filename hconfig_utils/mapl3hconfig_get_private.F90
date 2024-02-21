#include "MAPL_ErrLog.h"
module mapl3hconfig_get_private
   use :: esmf, only: ESMF_HConfig, ESMF_HConfigIsDefined
   use :: esmf, only: ESMF_KIND_I4, ESMF_KIND_I8
   use :: esmf, only: ESMF_KIND_R4, ESMF_KIND_R8
   use hconfig_value
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

   subroutine get_value_scalar(hconfig, keystring, value, found, unusable, default, equals_default, typestring, valuestring, rc)
      type(ESMF_HConfig), intent(inout) :: hconfig
      character(len=*), intent(in) :: keystring
      class(*), intent(inout) :: value
      logical, intent(out) :: found
      class(KeywordEnforcer), optional, intent(in) :: unusable
      class(*), optional, intent(inout) :: default
      logical, optional, intent(out) :: equals_default
      character(len=:), allocatable, optional, intent(inout) :: valuestring
      integer, intent(out) :: rc

      integer :: status
      integer :: ios
      character(len=:), allocatable :: typestring_
      class(HConfigValue) :: hconfig_value
      character(len=MAXSTR) :: fmt_

      if(present(default)) then
         _ASSERT(same_type_as(value, default))
      else
         _ASSERT(.not. (present(equals_default)),  'equals_default requires default')
      end if
      found = HConfig_Keystring_found(hconfig, keystring, rc=status)
      _VERIFY(status)

      _RETURN_UNLESS(found .or. present(default))

      select type(value)
      type is integer(kind=ESMF_KIND_I4)
         hconfig_value = HConfigValueI4(default)
      type is integer(kind=ESMF_KIND_I8)
         hconfig_value = HConfigValueI8(default)
      type is real(kind=ESMF_KIND_R4)
         hconfig_value = HConfigValueR4(default)
      type is real(kind=ESMF_KIND_R8)
         hconfig_value = HConfigValueR8(default)
      type is logical
         hconfig_value = HConfigValueLogical(default)
      type is character(len=*)
         hconfig_value = HConfigValueString(default)
      class default
         _FAIL('Unsupported type for conversion')
      end select

      if(found) then
         hconfig_value%hconfig_ = hconfig
         hconfig_value%keystring_ = keystring
         call hconfig_value%set_from_hconfig()
         status = this%last_status_
         _ASSERT(status == 0, 'Error setting value from ESMF_HConfig')
         this%value_equals_default_ = this%value_equals_default()
      else
         call hconfig_value%set_from_default()
         this%value_equals_default_ = .TRUE.
      end if

      if(present(valuestring)) then
         valuestring = this%get_valuestring(valuestring)
         status = this%last_status_
         _ASSERT(status == 0, 'Error getting valuestring')
      end if

      if(present(typestring)) typestring = hconfig_value%typestring_

      if(present(equals_default)) equals_default = hconfig_value%value_equals_default_
      value = get_value(hconfig_value)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine get_value_scalar

end module mapl3hconfig_get_private
