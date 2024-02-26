#include "hconfig_procedure_template.h"

   use hconfig_value_base
   implicit none

   type, extends(HConfigValue) :: DTYPE
     VTYPE, pointer :: value_ptr
     VTYPE, allocatable :: default_
   contains
     module procedure :: set_from_hconfig => SET_HCONFIG_
     module procedure :: set_from_default => SET_DEF_
     module procedure :: value_equals_default => VAL_EQ_DEF_
     module procedure :: get_valuestring => GET_VALSTRING_
   end type DTYPE

   interface DTYPE
     module procedure :: CONSTRUCT_HCONFIGVAL_
   end interface DTYPE

contains

   function CONSTRUCT_HCONFIGVAL_(value, default) result(this)
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
   end function CONSTRUCT_HCONFIGVAL_

   logical function VAL_EQ_DEF_(this) result(lval)
      class(DTYPE), intent(in) :: this
      lval = this%has_default_
      if(lval) lval = (this%value_ptr == this%default_)
   end function VAL_EQ_DEF_

   subroutine SET_HCONFIG_(this)
      class(DTYPE), intent(inout) :: this
      integer :: status
      this%value_ptr = HCONFIG_AS_(this)
      this%last_status_ = status
   end subroutine SET_HCONFIG_

   subroutine SET_DEF_(this)
      class(DTYPE), intent(inout) :: this
      this%value_ptr = this%default_
   end subroutine SET_DEF_

   subroutine GET_VALSTRING_(this, string)
      character(len=*), parameter :: FMT = TFMT
      class(DTYPE), intent(inout) :: this
      character(len=:), allocatable, intent(out) :: string
      integer :: ios
      character(len=32) :: raw
      write(raw, fmt=FMT, iostat=ios) this%value_ptr
      this%last_status_ = ios
      if(ios == 0) string = trim(adjustl(raw))
   end subroutine GET_VALSTRING_

