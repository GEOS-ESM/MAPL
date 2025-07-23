#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) initialize_user_smod
   use mapl3g_GenericPhases
   use mapl3g_ComponentDriver
   use mapl3g_ComponentDriverPtrVector
   use mapl3g_CouplerPhases, only: GENERIC_COUPLER_INITIALIZE
   use mapl_ErrorHandling
   implicit none

contains

   module recursive subroutine initialize_user(this, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_USER'
      type(ComponentDriverPtrVector) :: import_Couplers
      type(ComponentDriverPtr) :: drvr
      integer :: i

      import_couplers = this%registry%get_import_couplers()
      do i = 1, import_couplers%size()
         drvr = import_couplers%of(i)
         call drvr%ptr%initialize(phase_idx=GENERIC_COUPLER_INITIALIZE, _RC)
      end do

      call this%run_custom(ESMF_METHOD_INITIALIZE, PHASE_NAME, _RC)
      call recurse(this, phase_idx=GENERIC_INIT_USER, _RC)

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine initialize_user

end submodule initialize_user_smod
