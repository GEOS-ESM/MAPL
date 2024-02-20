#include "MAPL_Generic.h"
module hconfig_value_i4

   use hconfig_value_impl
   use esmf

   implicit none

   public :: HConfigValueI4

   type, extends(HConfigValue) :: HConfigValueI4
      integer(kind=ESMF_KIND_I4), pointer :: value => null()
      integer(kind=ESMF_KIND_I4), allocatable :: default_
   contains
      private
      procedure :: set_valuestring
      procedure :: set_to_hconfig
      procedure :: set_from_default
      procedure :: check_value_equals_default
   end type HConfigValueI4

   interface HConfigValueI4
      module procedure :: construct_hconfig_value_i4
   end interface HConfigValueI4

contains

   function construct_hconfig_value_i4(default) result(hcv)
      type(HConfigValueI4) :: hcv
      class(*), optional, intent(in) :: default
      
      if(present(default)) then
         select type (default)
         type is (integer(kind=ESMF_KIND_I4))
            this%default_ = default
         end select type
      end if

   end function construct_hconfig_value_i4

   subroutine set_valuestring(this, string, rc) 
      class(HConfigValue), intent(inout) :: this
      character(len=*), intent(out) :: string
      integer, intent(out) :: rc 
      write(string, fmt='(I12)', iostat=rc) this%value
   end subroutine set_valuestring

   subroutine set_to_hconfig(this, rc)
      class(HConfigValue), intent(inout) :: this
      integer, intent(out) :: rc
      integer :: status
      value = ESMF_HConfigAsI4(this%hconfig_, keyString=this%keystring_, _RC)
   end subroutine set_to_hconfig

   logical function check_value_equals_default(this)
      class(HConfigValue), intent(in) :: this
      check_value_equals_default = (this%value == this%default_)
   end function check_value_equals_default

   subroutine set_from_default(this)
      class(HConfigValue), intent(inout) :: this
      this%value = this%default_
   end subroutine set_from_default

end module hconfig_value_i4
