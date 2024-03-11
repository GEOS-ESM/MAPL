#include "MAPL_ErrLog.h"
module mapl3g_hconfig_get_private
!   use hconfig_value_mod !wdb fixme deleteme 
   use mapl3g_hconfig_getter, only: HConfigGetter
   use :: esmf, only: ESMF_HConfig, ESMF_HConfigIsDefined
   use :: esmf, only: ESMF_KIND_I4!, ESMF_KIND_I8
!   use :: esmf, only: ESMF_KIND_R4, ESMF_KIND_R8
   use :: pflogger, only: logger_t => logger
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling

   implicit none
   private
   public :: get_value

   interface get_value
      module procedure :: get_scalar
!      module procedure :: get_value_array
!      module procedure :: get_scalar_getter
   end interface get_value

contains

   !template
   subroutine get_scalar(hconfig, value, label, unusable, default, valueset, logger, rc)
      class(*), intent(inout) :: value !wdb could add array case with macro DIM_=dimension(:), allocatable for array; DIM_= for scalar
      type(ESMF_HConfig), intent(in) :: hconfig
      character(len=*), intent(in) :: label
      class(KeywordEnforcer), optional, intent(in) :: unusable
      class(*), optional, intent(in) :: default !wdb could add array case with macro DIM_=dimension(:) for array; DIM_= for scalar
      logical, optional, intent(out) :: valueset
      class(Logger_t), optional, target, intent(inout) :: logger
      integer, optional, intent(out) :: rc
      integer :: status
      type(HConfigGetter) :: getter

! wdb default value for valueset
      getter = HConfigGetter(hconfig, label, logger)
      getter%found = ESMF_HConfigIsDefined(getter%hconfig, keyString=getter%label, _RC)
!      getter%found = keystring_found(this%hconfig, this%label, _RC)
      if(present(valueset)) valueset = getter%found
      _RETURN_UNLESS(getter%found .or. present(default))

      select type(value)
      type is (integer(ESMF_KIND_I4))
         call getter%set_value(value, default, _RC)
!      type is (character(len=*)) !wdb fixme deleteme implement
!         call getter%set_value(value, default, _RC)
      class default
!         _FAIL('Something wicked this way comes...') !wdb fixme deleteme add something better
      end select
      
      if(present(valueset)) valueset = .TRUE. !wdb fixme  may be able to move this up.
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine get_scalar

end module mapl3g_hconfig_get_private
!   subroutine get_scalar(hconfig, value, getter, unusable, default, rc)
!      type(ESMF_HConfig), intent(in) :: hconfig
!      character(len=*), intent(in) :: label
!      class(*), intent(inout) :: value
!      class(HConfigGetter), intent(inout) :: getter
!      class(KeywordEnforcer), optional, intent(in) :: unusable
!      class(*), optional, intent(in) :: default
!      integer, intent(out) :: rc
!      integer :: status
!
!      class(HConfigValue), allocatable :: hconfig_value
!      logical :: found_
!
!      found_ = keystring_found(hconfig, label, rc=status)
!      if(present(default)) then
!         _ASSERT(same_type_as(value, default), 'value and default are different types.')
!      else
!         _ASSERT(found_ .or. present(found), '"' // trim(label) // '" not found.')
!         _ASSERT(.not. (present(equals_default)),  'equals_default requires default')
!      end if
!      _VERIFY(status)
!
!      _RETURN_UNLESS(found_ .or. present(default))
!
!      getter = HConfigGetter(label, logger)
!      call getter%initialize_getter(value)
!      call getter%set_value(value, default)
!      call getter%log_message()
!      select type(value)
!      type is (integer(kind=ESMF_KIND_I4))
!         hconfig_value = HConfigValueI4(value, default)
!      type is (integer(kind=ESMF_KIND_I8))
!         hconfig_value = HConfigValueI8(value, default)
!      type is (real(kind=ESMF_KIND_R4))
!         hconfig_value = HConfigValueR4(value, default)
!      type is (real(kind=ESMF_KIND_R8))
!         hconfig_value = HConfigValueR8(value, default)
!      type is (logical)
!         hconfig_value = HConfigValueLogical(value, default)
!      type is (character(len=*))
!         hconfig_value = HConfigValueString(value, default)
!      class default
!         _FAIL('Unsupported type for conversion')
!      end select
!      _ASSERT(hconfig_value%last_status_ == 0, 'Error constructing hconfig_value object')
!
!      if(found_) then
!         hconfig_value%hconfig_ = hconfig
!         hconfig_value%label_ = label
!         call hconfig_value%set_from_hconfig()
!         status = hconfig_value%last_status_
!         _ASSERT(status == 0, 'Error setting value from ESMF_HConfig')
!         hconfig_value%value_equals_default_ = hconfig_value%value_equals_default()
!      else
!         call hconfig_value%set_from_default()
!         hconfig_value%value_equals_default_ = .TRUE.
!      end if
!
!      if(present(valuestring)) then
!         call hconfig_value%get_valuestring(valuestring)
!         status = hconfig_value%last_status_
!         _ASSERT(status == 0, 'Error getting valuestring')
!      end if
!
!      if(present(typestring)) typestring = hconfig_value%typestring_
!      if(present(equals_default)) equals_default = hconfig_value%value_equals_default_
!      if(present(found)) found = found_
!
!      _RETURN(_SUCCESS)
!      _UNUSED_DUMMY(unusable)
!
!   end subroutine get_scalar

!   subroutine get_value_array(hconfig, label, value, unusable, found, default, equals_default, typestring, valuestring, rc)
!      type(ESMF_HConfig), intent(in) :: hconfig
!      character(len=*), intent(in) :: label
!      class(*), intent(inout) :: value(:)
!      class(KeywordEnforcer), optional, intent(in) :: unusable
!      logical, optional, intent(out) :: found
!      class(*), optional, intent(in) :: default(:)
!      logical, optional, intent(out) :: equals_default
!      character(len=:), allocatable, optional, intent(inout) :: typestring
!      character(len=:), allocatable, optional, intent(inout) :: valuestring
!      integer, intent(out) :: rc
!
!      integer :: status
!      class(HConfigValue), allocatable :: hconfig_value
!      logical :: found_
!
!      found_ = keystring_found(hconfig, label, rc=status)
!      if(present(default)) then
!         _ASSERT(same_type_as(value, default), 'value and default are different types.')
!         _ASSERT(size(value) == size(default), 'value and default are different sizes.')
!      else
!         _ASSERT(found_ .or. present(found), '"' // trim(label) // '" not found.')
!         _ASSERT(.not. (present(equals_default)),  'equals_default requires default')
!      end if
!      _VERIFY(status)
!
!      _RETURN_UNLESS(found_ .or. present(default))
!
!      select type(value)
!      type is (integer(kind=ESMF_KIND_I4))
!         hconfig_value = HConfigValueI4Seq(value, default)
!      type is (integer(kind=ESMF_KIND_I8))
!         hconfig_value = HConfigValueI8Seq(value, default)
!      type is (real(kind=ESMF_KIND_R4))
!         hconfig_value = HConfigValueR4Seq(value, default)
!      type is (real(kind=ESMF_KIND_R8))
!         hconfig_value = HConfigValueR8Seq(value, default)
!      type is (logical)
!         hconfig_value = HConfigValueLogicalSeq(value, default)
!      type is (character(len=*))
!         _FAIL('Unsupported type for conversion')
!      class default
!         _FAIL('Unsupported type for conversion')
!      end select
!      _ASSERT(hconfig_value%last_status_ == 0, 'Error constructing hconfig_value object')
!
!      if(found_) then
!         hconfig_value%hconfig_ = hconfig
!         hconfig_value%keystring_ = label
!         call hconfig_value%set_from_hconfig()
!         status = hconfig_value%last_status_
!         _ASSERT(status == 0, 'Error setting value from ESMF_HConfig')
!         hconfig_value%value_equals_default_ = hconfig_value%value_equals_default()
!      else
!         call hconfig_value%set_from_default()
!         hconfig_value%value_equals_default_ = .TRUE.
!      end if
!
!      if(present(valuestring)) then
!         call hconfig_value%get_valuestring(valuestring)
!         status = hconfig_value%last_status_
!         _ASSERT(status == 0, 'Error getting valuestring')
!      end if
!
!      if(present(typestring)) typestring = hconfig_value%typestring_
!      if(present(equals_default)) equals_default = hconfig_value%value_equals_default_
!      if(present(found)) found = found_
!
!      _RETURN(_SUCCESS)
!      _UNUSED_DUMMY(unusable)
!
!   end subroutine get_value_array
!subroutine get_scalar(hconfig, value, label, unusable, default, valueset, logger, rc)
!   type(ESMF_HConfig), intent(in) :: hconfig
!   class(*), intent(inout) :: value !wdb could add array case with macro DIM_=dimension(:), allocatable for array; DIM_= for scalar
!   character(len=*), intent(in) :: label
!   class(KeywordEnforcer), optional, intent(in) :: unusable
!   class(*), optional, intent(in) :: default !wdb could add array case with macro DIM_=dimension(:) for array; DIM_= for scalar
!   logical, optional, intent(out) :: valueset
!   class(Logger_t), optional, target, intent(inout) :: logger
!   integer, optional, intent(out) :: rc
!   type(HConfigGetter) :: getter
!   integer :: status
!
!   getter = HConfigGetter(hconfig, label, logger)
!   call get_value(getter, value, default=default, valueset=valueset, _RC)
!   if(present(valueset)) valueset = getter%found
!   
!   _RETURN(_SUCCESS)
!   _UNUSED_DUMMY(unusable)
!
!end subroutine get_scalar
