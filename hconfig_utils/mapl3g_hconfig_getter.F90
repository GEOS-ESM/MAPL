#include "MAPL_ErrLog.h"
module mapl3g_hconfig_getter

   use :: pflogger, only: logger_t => logger
   use :: esmf, MAXSTRLEN => ESMF_MAXSTR
   use mapl_ErrorHandling

   implicit none
   public :: HConfigGetter
   public :: get_value

   type :: HConfigGetter
      type(ESMF_HConfig) :: hconfig
      character(len=:), allocatable :: label
      logical :: found = .FALSE.
      character(len=:), allocatable :: typestring
      character(len=:), allocatable :: valuestring
      logical :: value_equals_default = .FALSE.
   end type HConfigGetter

   interface get_value
      module procedure :: get_value_i4
   end interface get_value

   character(len=*), parameter :: DEFAULT_FORMAT_STRING = '(G0)'
   character(len=*), parameter :: EMPTY_STRING = ''

   interface HConfigGetter
      module procedure :: construct
   end interface HConfigGetter

contains
   
   type(HConfigGetter) function construct(hconfig, label, found)
      type(ESMF_HConfig), intent(in) :: hconfig
      character(len=*), intent(in) :: label
      logical, intent(in) :: found

      construct%hconfig=hconfig
      construct%label=label
      construct%found=found
      construct%typestring=EMPTY_STRING
      construct%valuestring=EMPTY_STRING

   end function construct

   subroutine get_value_i4(getter, value, default, rc)
      integer(kind=ESMF_KIND_I4), intent(out) :: value !macro VALTYPE
      character(len=*), parameter :: fmt_ = DEFAULT_FORMAT_STRING !macro FMTSTR
      integer(kind=ESMF_KIND_I4) :: default_ !macro VALTYPE
      type(HConfigGetter), intent(inout) :: getter
      class(*), optional, intent(in) :: default
      integer, optional,intent(out) :: rc
      integer :: status = 0
      character(len=MAXSTRLEN) :: buffer

      getter%typestring = 'TYPESTRING_'
      default_ = -huge(1)
      if (present(default)) then
         select type(default)
         type is (integer(kind=ESMF_KIND_I4)) !macro TYPE_
            default_ = default
            value = default_
         class default
            _FAIL('Illegal type provided for default value for label <'//getter%label//'>')
         end select
      end if

      if (getter%found) then
         value = ESMF_HConfigAsI4(getter%hconfig, keyString=getter%label, _RC) !macro ESMF_HCONFIG_AS
      end if

      getter%value_equals_default = (value == default_)
      write(buffer, fmt=fmt_, iostat=status) value
      _VERIFY(status)
      getter%valuestring = trim(buffer)

      _RETURN(_SUCCESS)

   end subroutine get_value_i4

end module mapl3g_hconfig_getter

!   subroutine make_valuestring_i4(this, value, rc)
!      integer(kind=ESMF_KIND_I4), intent(in) :: value
!      class(HConfigGetter), intent(inout) :: this
!      integer, intent(out) :: rc
!      integer :: status = 0
!
!      allocate(character(len=MAXSTRLEN) :: this%valuestring)
!      write(this%valuestring, fmt=this%formatstring, iostat=status) value
!      _ASSERT(status == 0, 'Error writing valuestring')
!      this%valuestring = trim(this%valuestring) !wdb fixme deleteme refactor?
!      _RETURN(_SUCCESS)
!
!   end subroutine make_valuestring_i4
!
!   subroutine log_message_i4(this, value, rc, valuestring_out)
!      integer(kind=ESMF_KIND_I4), intent(in) :: value
!      class(HConfigGetter), intent(inout) :: this
!      integer, intent(out) :: rc
!      character(len=:), allocatable :: valuestring
!      character(len=:), allocatable, optional, intent(out) :: valuestring_out
!      integer :: status
!
!      allocate(character(len=MAXSTRLEN) :: valuestring) !wdb fixme deleteme specific to type
!      write(valuestring, fmt=this%formatstring, iostat=status) value !wdb fixme deleteme specific to type
!      _ASSERT(status == 0, 'Error writing valuestring')
!      valuestring = trim(valuestring) !wdb fixme deleteme refactor?
!      if(this%value_equals_default) valuestring = valuestring // DEFAULT_VALUE_TAG
!      !wdb fixme deleteme pass in typestring from macro
!      call this%log_resource_message(valuestring, _RC)
!      if(present(valuestring_out)) valuestring_out = valuestring
!      _RETURN(_SUCCESS)
!   end subroutine log_message_i4
!      
!      _ASSERT(this%found .or. present(default), 'Default must be present if label is not found.')
!      _ASSERT(present(label) .or. present(default), 'Default must be present if label is not found.')
!      this%typestring = 'I4'!wdb fixme deleteme could be macro
!      block
!         this%value_equals_default = present(default)
!         if(this%found) then
!            value = ESMF_HConfigAsI4(this%hconfig, keyString=this%label, _RC)
!         end if
!         if(.not. present(default)) exit
!
!         select type(default)
!         type is (integer(kind=ESMF_KIND_R4))
!            if(found) then
!               this%value_equals_default = value==default
!               exit
!            end if
!            value = default
!         class default
!            _FAIL('type mismatch')
!         end select
!      end block
!
!      call this%make_valuestring(value)
!
!      valueset = .FALSE.
!      block
!         if(present(label)) then
!            value = ESMF_HConfigAsI4(hconfig, keyString=label, _RC)
!         end if
!         if(.not. present(default)) exit
!
!         select type(default)
!         type is (integer(kind=ESMF_KIND_R4))
!            if(present(label)) then
!               equals_default_ = value==default
!               exit
!            end if
!            value = default
!            equals_default_ = .TRUE.
!         class default
!            _FAIL('type mismatch')
!         end select
!      end block
!
!      call this%make_valuestring(value)
!      if(present(equals_default)) equals_default = equals_default_
!      _RETURN(_SUCCESS)
!    template
!   type(HConfigGetter) function construct_hconfig_getter_i4(hconfig, value, abel, logger) result(instance)
!      type(ESMF_HConfig), intent(in) :: hconfig
!      integer(kind=ESMF_KIND_I4), intent(in) :: value !wdb fixme deleteme could be macro
!      character(len=*), intent(in) :: label
!      type(logger_t), optional, target, intent(inout) :: logger
!
!      instance = HConfigGetter(hconfig, label, logger)
!      instance%typestring = 'I4' !wdb fixme deleteme could be macro
!
!   end function construct_hconfig_getter_i4
!
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
!
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
!   !wdb fixme deleteme pass in typestring
!   subroutine log_resource_message(this, message, rc)
!      class(HConfigGetter), intent(inout) :: this
!      character(len=*), intent(in) :: message
!      integer, optional, intent(out) :: rc
!      integer :: status
!
!      if(.not. this%do_log()) return
!      call this%logger%info(this%typestring //' '// this%label //' = '// message) !wdb fixme deleteme Does pflogger have rc codes?
!      _RETURN(_SUCCESS)
!
!   end subroutine log_resource_message
!
!    template
!   subroutine get_value_i4(this, value, default, rc)
!  subroutine get_value_i4(hconfig, value, valueset, label, default, equals_default, rc)
!      class(HConfigGetter), intent(in) :: hconfig
!      character(len=*), optional, intent(in) :: label
!      logical, optional, intent(out) :: equals_default
!type(HConfigGetter) function construct_hconfig_getter(hconfig, label, found) result(instance)
!   type(ESMF_HConfig), intent(in) :: hconfig
!   character(len=*), intent(in) :: label
!   logical, intent(in) :: found
!
!   instance%hconfig = hconfig
!   instance%label = label
!   instance%found = found
!   instance%valuestring = EMPTY_STRING
!
!subroutine get_value_i4(this, value, default, _RC)
