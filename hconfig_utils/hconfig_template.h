!# vim:ft=fortran
#include "hconfig_macros.h"
#if defined GROUPSTR
#undef GROUPSTR
#endif
#define GROUPSTR(S) '(' // S // ')'

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
      if(lval) lval = PROPFCT(this%value_ptr, this%default_)
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
      character(len=*), parameter :: DELIMITER = ' '
      integer :: ios, sz = 0
      character(len=MAXSTRLEN) :: raw
      character(len=:), allocatable :: fmt_

      sz = SZFCT(this%value_ptr)
      fmt_ = make_format_string(FMT, sz)
      WRITE_STATEMENT(raw, fmt_, ios, this%value_ptr)
      if(ios /= 0) return
      string = trim(adjustl(raw))

   end subroutine get_valuestring

   function make_format_string(format_string, n, delimiter)
      character(len=:), allocatable :: make_format_string
      character(len=*), intent(in) :: format_string
      integer, intent(in) :: n
      character(len=*), optional, intent(in) :: delimiter
      character(len=:), allocatable :: delimiter_
      character(len=:), allocatable :: raw
      character(len=32) :: reps

      if((n < 0) .or. (len_trim(format_string) == 0)) then
         make_format_string = ''
         return
      end if

      raw = trim(adjustl(format_string))
      if(n < 2) then
         make_format_string = GROUPSTR(raw)
         return
      end if

      if(present(delimiter)) then
         delimiter_ = '"' // delimiter // '", '
      else
         delimiter_ = '1X, '
      end if

      write(reps, fmt='(I32)') n-1
      make_format_string = GROUPSTR(raw//', '//trim(adjustl(reps))//GROUPSTR(delimiter_//raw))

   end function make_format_string

