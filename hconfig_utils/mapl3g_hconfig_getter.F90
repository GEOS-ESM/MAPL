!wdb fixme deleteme typestring could be templated and formatstring
#include "MAPL_ErrLog.h"
module mapl3g_hconfig_getter
   use :: pflogger, only: logger_t => logger
   use :: esmf, MAXSTRLEN => ESMF_MAXSTR
   use mapl_ErrorHandling
   implicit none
   public :: HConfigGetter

   type :: HConfigGetter
      type(ESMF_HConfig) :: hconfig
      character(len=:), allocatable :: label
      character(len=:), allocatable :: typestring
      character(len=:), allocatable :: formatstring
      type(logger_t), pointer :: logger => null()
      logical :: found = .FALSE.
      logical :: value_equals_default = .FALSE.
   contains
      generic :: set_value => set_value_i4!, set_value_i4_seq, set_value_string !wdb IMPLEMENT
      procedure :: set_value_i4
!      procedure :: set_value_i4_seq !wdb IMPLEMENT
!      procedure :: set_value_string !wdb IMPLEMENT
      generic :: log_message => log_message_i4!, log_message_i4_seq, log_message_string !wdb IMPLEMENT
      procedure :: log_message_i4
!      procedure :: log_message_i4_seq !wdb IMPLEMENT
!      procedure :: log_message_string  !wdb IMPLEMENT
      procedure :: log_resource_message
      procedure :: do_log
   end type HConfigGetter

   interface HConfigGetter
      module procedure :: construct_hconfig_getter
!      module procedure :: construct_hconfig_getter_i4 !wdb IMPLEMENT
   end interface HConfigGetter

   character(len=*), parameter :: DEFAULT_FORMAT_STRING = '(G0)'
   character(len=*), parameter :: DEFAULT_VALUE_TAG = ' (default)'
   character(len=*), parameter :: EMPTY_STRING = ''

   interface handle_default
      procedure :: handle_default_i4 
!      procedure :: handle_default_i4_seq !wdb IMPLEMENT
!      procedure :: handle_default_string !wdb IMPLEMENT
   end interface handle_default

contains

   type(HConfigGetter) function construct_hconfig_getter(hconfig, label, logger) result(instance)
      type(ESMF_HConfig), intent(in) :: hconfig
      character(len=*), intent(in) :: label
      type(logger_t), optional, target, intent(in) :: logger

      instance%hconfig = hconfig
      instance%label = label
      instance%typestring = EMPTY_STRING
      instance%formatstring = DEFAULT_FORMAT_STRING
      if(present(logger)) instance%logger => logger

   end function construct_hconfig_getter

   logical function do_log(this)
      class(HConfigGetter), intent(in) :: this
      do_log = associated(this%logger)
   end function do_log

   !wdb fixme deleteme pass in typestring
   subroutine log_resource_message(this, message, rc)
      class(HConfigGetter), intent(inout) :: this
      character(len=*), intent(in) :: message
      integer, optional, intent(out) :: rc
      integer :: status

      if(.not. this%do_log()) return
      call this%logger%info(this%typestring //' '// this%label //' = '// message) !wdb fixme deleteme Does pflogger have rc codes?
      _RETURN(_SUCCESS)

   end subroutine log_resource_message

!    template
   subroutine set_value_i4(this, value, default, rc)
      class(HConfigGetter), intent(inout) :: this
      integer(kind=ESMF_KIND_I4), intent(out) :: value !wdb fixme deleteme could be macro!wdb can template (VALTYPEOUT)
      class(*), optional, intent(in) :: default !wdb fixme deleteme could be macro!wdb can template (VALTYPEIN)
      integer, optional,intent(out) :: rc
      integer :: status

      this%typestring = 'I4'!wdb fixme deleteme could be macro

      if(this%found) then
         value = ESMF_HConfigAsI4(this%hconfig, keyString=this%label, _RC)!wdb fixme deleteme could be macro
         ! Do not set value to default. Compare only.
      end if
      if(present(default)) then
         call handle_default(default, value, this%value_equals_default, compare_only=this%found, _RC)
      end if
      _RETURN_UNLESS(this%do_log())
      call this%log_message(value, _RC)
      _RETURN(_SUCCESS)
      
   end subroutine set_value_i4

   !template - macros for equal operator
   subroutine handle_default_i4(default, value, are_equal, compare_only, rc)
      integer(kind=ESMF_KIND_I4), intent(inout) :: value!wdb fixme deleteme could be macro
      class(*), intent(in) :: default
      logical, intent(out) :: are_equal
      logical, intent(in) :: compare_only
      integer, optional, intent(out) :: rc
      integer :: status

      select type(default)
      type is (integer(kind=ESMF_KIND_I4))!wdb fixme deleteme could be macro
         if(compare_only) then
            ! Compare only
            are_equal = (value == default)
            return
         end if
         ! Therefore compare_only is .FALSE.
         value = default
         ! So are_equal must be equal.
         are_equal = .TRUE.
      class default
         _FAIL('type unrecognized')
      end select
      _RETURN(_SUCCESS)
   end subroutine handle_default_i4

   !wdb everything could be included with template - 2nd procedure for arrays with macro selector
   subroutine log_message_i4(this, value, rc, valuestring_out)
      integer(kind=ESMF_KIND_I4), intent(in) :: value!wdb fixme deleteme could be macro !wdb can template (VALTYPEIN)
      class(HConfigGetter), intent(inout) :: this
      integer, intent(out) :: rc
      character(len=:), allocatable :: valuestring
      character(len=:), allocatable, optional, intent(out) :: valuestring_out
      integer :: status

      allocate(character(len=MAXSTRLEN) :: valuestring) !wdb fixme deleteme specific to type
      write(valuestring, fmt=this%formatstring, iostat=status) value !wdb fixme deleteme specific to type
      _ASSERT(status == 0, 'Error writing valuestring')
      valuestring = trim(valuestring) !wdb fixme deleteme refactor?
      if(this%value_equals_default) valuestring = valuestring // DEFAULT_VALUE_TAG
      !wdb fixme deleteme pass in typestring from macro
      call this%log_resource_message(valuestring, _RC)
      if(present(valuestring_out)) valuestring_out = valuestring
      _RETURN(_SUCCESS)
   end subroutine log_message_i4
      
end module mapl3g_hconfig_getter

!    template
!   type(HConfigGetter) function construct_hconfig_getter_i4(hconfig, value, label, logger) result(instance)
!      type(ESMF_HConfig), intent(in) :: hconfig
!      integer(kind=ESMF_KIND_I4), intent(in) :: value !wdb fixme deleteme could be macro
!      character(len=*), intent(in) :: label
!      type(logger_t), optional, target, intent(inout) :: logger
!
!      instance = HConfigGetter(hconfig, label, logger)
!      instance%typestring = 'I4' !wdb fixme deleteme could be macro
!
!   end function construct_hconfig_getter_i4

!   !wdb everything could be included with template
!   subroutine initialize_hconfig_getter_i4(this, value)
!      type(HConfigGetter), intent(inout) :: this
!      integer(kind=ESMF_KIND_I4), intent(in) :: value !wdb can template (VALTYPEIN)
!      this%typestring = 'I4' !wdb can template (TYPESTR)
!   end subroutine initialize_hconfig_getter_i4
!
!   !wdb everything could be included with template
!   subroutine initialize_hconfig_getter_string(this, value)
!      type(HConfigGetter), intent(inout) :: this
!      character(len=*) , intent(in) :: value !wdb can template (VALTYPEIN)
!      this%typestring = 'CH' !wdb can template (TYPESTR)
!   end subroutine initialize_hconfig_getter_i4

!   !wdb everything could be included with template
!   subroutine get_value_i4(this, value, default, rc)
!      type(HConfigGetter), intent(inout) :: this
!      integer(kind=ESMF_KIND_I4), intent(out) :: value !wdb can template (VALTYPEOUT)
!      integer(kind=ESMF_KIND_I4), optional, intent(in) :: default !wdb can template (VALTYPEIN)
!      integer, optional, intent(out) :: rc
!      integer :: status
!      logical :: value_equals_default
!
!      value = ESMF_HConfigAsI4 (this%hconfig, keyString=this%label, asOkay=this%found, _RC) !wdb can template (ESMF_HCONFIG_AS)
!      value_equals_default = this%found .and. merge(value == default, .FALSE., present(default)) 
!      value = merge(value, default, this%found)
!      _RETURN_UNLESS(this%do_log)
!      call this%set_valuestring(value, _RC)
!      
!   end subroutine get_value_i4
