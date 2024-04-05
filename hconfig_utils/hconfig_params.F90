#include "MAPL_ErrLog.h"
module mapl3g_hconfig_params

   use :: esmf, only: ESMF_HConfig
   use :: pflogger, only: logger_t => logger
   use mapl_ErrorHandling

   implicit none
   private

   public :: HConfigParams

   type :: HConfigParams
      type(ESMF_HConfig) :: hconfig
      character(len=:), allocatable :: label
      logical :: check_value_set = .FALSE.
      logical :: value_set = .FALSE.
      class(Logger_t), pointer :: logger => null()
   contains
      procedure :: log_message
      procedure :: has_logger
   end type HConfigParams

   interface HConfigParams
      module procedure :: construct_hconfig_params
   end interface HConfigParams

contains

   function construct_hconfig_params(hconfig, label, check_value_set, logger) result(params)
      type(HConfigParams) :: params
      type(ESMF_HConfig), intent(in) :: hconfig
      character(len=*), intent(in) :: label
      logical, optional, intent(in):: check_value_set
      class(Logger_t), pointer, optional, intent(in) :: logger

      params%hconfig = hconfig
      params%label = label
      if(present(check_value_set)) params%check_value_set = check_value_set
      if(present(logger)) params%logger => logger

   end function construct_hconfig_params

   logical function has_logger(this)
      class(HConfigParams), intent(in) :: this

      has_logger = associated(this%logger) 

   end function has_logger

   subroutine log_message(this, typestring, valuestring, rc)
      class(HConfigParams), intent(in) :: this
      character(len=*), intent(in) :: typestring
      character(len=*), intent(in) :: valuestring
      integer, optional, intent(out) :: rc
      integer :: status
      character(len=:), allocatable :: message

      _ASSERT(this%has_logger(), 'There is no logger.')
      message = typestring //' '// this%label //' = '// valuestring
      call this%logger%info(message)
      _RETURN(_SUCCESS)

   end subroutine log_message

end module mapl3g_hconfig_params
