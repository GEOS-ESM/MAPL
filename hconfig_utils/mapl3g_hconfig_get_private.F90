#include "MAPL_ErrLog.h"
module mapl3g_hconfig_get_private
   use :: mapl3g_hconfig_getter, only: HConfigGetter, get_value
   use :: esmf, only: ESMF_HConfig, ESMF_HConfigIsDefined, ESMF_KIND_I4
   use :: pflogger, only: logger_t => logger
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling

   implicit none
   private
   public :: get_value

   interface get_value
      module procedure :: get_scalar
   end interface get_value

contains

   subroutine get_scalar(hconfig, value, label, unusable, default, valueset, logger, rc)
      class(*), intent(inout) :: value
      type(ESMF_HConfig), intent(in) :: hconfig
      character(len=*), intent(in) :: label
      class(KeywordEnforcer), optional, intent(in) :: unusable
      class(*), optional, intent(in) :: default
      logical, optional, intent(out) :: valueset
      class(Logger_t), optional, target, intent(inout) :: logger
      integer, optional, intent(out) :: rc
      integer :: status = _FAILURE
      type(HConfigGetter) :: getter
      type(logger_t), pointer :: logger_
      logical :: found = .FALSE.

      if(present(valueset)) valueset = .FALSE.
      if(.not. present(valueset)) status = _FAILURE
      logger_ => null()
      if(present(logger)) logger_ => logger

      found = ESMF_HConfigIsDefined(hconfig, keyString=label, _RC)
      getter = HConfigGetter(hconfig, label, found)
      _RETURN_UNLESS(found .or. present(default)))

      select type(value)
      type is (integer(ESMF_KIND_I4))
         call get_value(getter, value, default, _RC)
      class default
         _FAIL('Unsupported type provided for label <'//getter%label//'>')
      end select
      
      if(present(logger)) then
         call logger_%info(getter%typestring //' '// label //' = '// getter%valuestring)
      end if

      if(present(valueset)) valueset = .TRUE.
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine get_scalar

end module mapl3g_hconfig_get_private
