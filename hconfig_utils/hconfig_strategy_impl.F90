#include "MAPL_Generic.h"
module hconfig_value_impl

   use hconfig_value_base
   use mapl_ErrorHandling
   use esmf

   implicit none

   private
   public :: HConfigValue, HConfigValueImpl, MAXSTRLEN

   type, abstract, extends(HConfig_Value) :: HConfigValueImpl
      type(ESMF_HConfig) :: hconfig_
      character(len=:), allocatable :: typestring_ = ''
      character(len=:), allocatable :: valuestring_ = ''
      logical :: value_is_set_ = .FALSE.
      logical :: value_equals_default_ = .FALSE.
      logical :: keystring_found = .FALSE.
      integer :: last_status_ = 0
   contains
      public
      procedure :: value_equals_default
      procedure :: value_is_set
      procedure :: typestring
      procedure :: valuestring
      procedure :: set_common_fields
      procedure :: found
      procedure, private :: has_default 
      procedure, private :: set_value
   end type HConfigValueImpl

   integer, parameter :: MAXSTRLEN = 80

contains
   
   subroutine set_value(this, rc)
      class(HConfigValue), intent(in) :: this
      integer, optional, intent(out) :: rc
   logical function found(this)
      class(HConfigValue), intent(in) :: this
      found = this%keystring_found
   end function found

   logical function value_is_set(this)
      class(HConfigValue), intent(in) :: this
      value_is_set = this%value_is_set_
   end function value_is_set

   logical function value_equals_default(this)
      class(HConfigValue), intent(in) :: this
      value_equals_default = this%value_equals_default_
   end function value_equals_default

   logical function has_default(this)
      class(HConfigValue), intent(in) :: this
      has_default = allocated(this%default_)
   end function has_default

   function typestring(this) result(typestring)
      class(HConfigValue), intent(in) :: this
      character(len=:), allocatable :: typestring
      typestring = this%typestring_
   end function typestring
   
   function valuestring(this) result(valuestring)
      class(HConfigValue), intent(in) :: this
      character(len=:), allocatable :: valuestring
      valuestring = this%valuestring_
   end function valuestring

   subroutine set_common_fields
      if(keystring_found) then
         call this%set_from_hconfig(_RC)
         if(has_default) this%value_equals_default_ = this%check_value_equals_default()
      else if(has_default) then
         call this%set_to_default()
         this%value_equals_default_ = .TRUE.
      end if
      this%value_is_set_ = .TRUE.
      call this%set_valuestring(this%valuestring_, _RC)
         
   end subroutine set_common_fields

end module hconfig_value_impl
