#include "MAPL_Generic.h"

submodule (mapl3g_OuterMetaComponent) initialize_realize_smod
   use mapl3g_ComponentDriverPtrVector
   use mapl3g_CouplerMetaComponent, only: GENERIC_COUPLER_INITIALIZE
   IMPLICIT none

contains

   module recursive subroutine initialize_realize(this, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_REALIZE'
      type(ComponentDriverPtrVector) :: export_Couplers
      type(ComponentDriverPtr) :: drvr
      integer :: i

      call this%run_custom(ESMF_METHOD_INITIALIZE, PHASE_NAME, _RC)
      call recurse(this, phase_idx=GENERIC_INIT_REALIZE, _RC)
      call this%registry%allocate(_RC)

      export_couplers = this%registry%get_export_couplers()
      do i = 1, export_couplers%size()
         drvr = export_couplers%of(i)
         call drvr%ptr%initialize(phase_idx=GENERIC_COUPLER_INITIALIZE, _RC)
      end do

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine initialize_realize

end submodule initialize_realize_smod
