#include "hconfig_macros.h"

   use hconfig_value_base
   implicit none

   private
   public :: DTYPE

   type, extends(HConfigValue) :: DTYPE
#if defined IS_ARRAY
     MTYPE, pointer :: value_ptr(:)
     MTYPE, allocatable :: default_(:)
#else
     MTYPE, pointer :: value_ptr
     MTYPE, allocatable :: default_
#endif
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
#if defined IS_ARRAY
      VTYPE, target :: value(:)
      class(*), optional, intent(in) :: default(:)
#else
      VTYPE, target :: value
      class(*), optional, intent(in) :: default
#endif
      this%value_ptr => value
      this%has_default_ = present(default)
      this%last_status_ = 0
      if(this%has_default_) then
         select type(default)
         type is(VTYPE)
            this%default_ = default
#if defined IS_STRING
            this%last_status_ = merge(0, -1, len(default) == len(value))
#endif
         end select
      end if
      this%typestring_ = TYPESTR
            
   end function construct_hconfig

   logical function value_equals_default(this) result(lval)
      class(DTYPE), intent(in) :: this
      lval = this%has_default_
      if(lval) lval = PROPFCT(this%value_ptr, this%default_)
   end function value_equals_default

   subroutine set_from_hconfig(this)
      class(DTYPE), intent(inout) :: this
      integer :: status
#if defined USE_STRLEN
      integer :: strlen
      strlen = len(this%value_ptr)
      this%value_ptr = ESMF_HCONFIG_AS(this%hconfig_, stringLen=strlen, keyString=this%keystring_, rc=status)
#else
      this%value_ptr = ESMF_HCONFIG_AS(this%hconfig_, keyString=this%keystring_, rc=status)
#endif
      this%last_status_ = status
   end subroutine set_from_hconfig

   subroutine set_from_default(this)
      class(DTYPE), intent(inout) :: this
      this%value_ptr = this%default_
   end subroutine set_from_default

   subroutine get_valuestring(this, string)
      class(DTYPE), intent(inout) :: this
      character(len=:), allocatable, intent(out) :: string
      character(len=MAXSTRLEN) :: raw
      integer :: ios
#if defined IS_ARRAY
      character(len=*), parameter :: DELIMITER = ' '
      integer :: i

      WRITE_STATEMENT(raw, ios, this%value_ptr(1))
#else
      WRITE_STATEMENT(raw, ios, this%value_ptr)
#endif
      if(ios /= 0) return
      string = trim(adjustl(raw))
#if defined IS_ARRAY
      do i = 2, SZFCT(this%value_ptr)
         WRITE_STATEMENT(raw, ios, this%value_ptr(i))
         if(ios /= 0) return
         string = string // DELIMITER // trim(adjustl(raw))
      end do
#endif

   end subroutine get_valuestring
