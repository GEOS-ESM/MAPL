#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) initialize_user_smod

   use mapl_enums_api, only: MAPL_GENERIC_INIT_USER, MAPL_GENERIC_COUPLER_INITIALIZE
   use mapl_ComponentDriver_mod
   use mapl_ComponentDriverPtrVector_mod
   use mapl_ErrorHandling_mod
   use pflogger, only: logger_t => logger

   implicit none(type,external)

contains

   module recursive subroutine initialize_user(this, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_USER'
      type(ComponentDriverPtrVector) :: import_Couplers
      type(ComponentDriverPtr) :: drvr
      class(logger_t), pointer :: logger
      integer :: i, status

      call recurse(this, phase_idx=MAPL_GENERIC_INIT_USER, _RC)

      import_couplers = this%registry%get_import_couplers()
      do i = 1, import_couplers%size()
         drvr = import_couplers%of(i)
          call drvr%ptr%initialize(phase_idx=MAPL_GENERIC_COUPLER_INITIALIZE, _RC)
      end do

      logger => this%get_logger()
      call logger%info("Initialize:: starting...")
      call this%start_timer("Initialize", _RC)
      call this%run_custom(ESMF_METHOD_INITIALIZE, PHASE_NAME, _RC)
      call this%stop_timer("Initialize", _RC)
      call logger%info("Initialize:: ...completed")

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine initialize_user

end submodule initialize_user_smod
