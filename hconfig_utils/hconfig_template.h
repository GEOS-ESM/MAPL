#include "hconfig_macros.h"
   use hconfig_value_base
   implicit none

   private
   public :: DTYPE

   type, extends(HConfigValue) :: DTYPE
     MTYPE, pointer :: value_ptr
     MTYPE, allocatable :: default_
   contains
     procedure :: set_from_hconfig
     procedure :: set_from_default
     procedure :: value_equals_default
     procedure :: get_valuestring
   end type DTYPE

   interface DTYPE
     module procedure :: construct_hconfig
   end interface DTYPE

contains

   function construct_hconfig(value, default) result(this)
      type(DTYPE) :: this
      VTYPE, target :: value
      class(*), optional, intent(in) :: default
      this%value_ptr => value
      this%has_default_ = present(default)
      if(this%has_default_) then
         select type(default)
         type is(VTYPE)
            this%default_ = default
         end select
      end if
      this%typestring_ = TYPESTR
   end function construct_hconfig

   logical function value_equals_default(this) result(lval)
      class(DTYPE), intent(in) :: this
      lval = this%has_default_
      if(lval) lval = (this%value_ptr RELOPR this%default_)
   end function value_equals_default

   subroutine set_from_hconfig(this)
      class(DTYPE), intent(inout) :: this
      integer :: status
      this%value_ptr = ESMF_HCONFIG_AS(this%hconfig_, keyString=this%keystring_, rc=status)
      this%last_status_ = status
   end subroutine set_from_hconfig

   subroutine set_from_default(this)
      class(DTYPE), intent(inout) :: this
      this%value_ptr = this%default_
   end subroutine set_from_default

   subroutine get_valuestring(this, string)
      character(len=*), parameter :: FMT = TFMT
      class(DTYPE), intent(inout) :: this
      character(len=:), allocatable, intent(out) :: string
      integer :: ios
      character(len=32) :: raw
      write(raw, fmt=FMT, iostat=ios) this%value_ptr
      this%last_status_ = ios
      if(ios == 0) string = trim(adjustl(raw))
   end subroutine get_valuestring
