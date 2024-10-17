#include "MAPL_ErrLog.h"

submodule(mapl3g_GriddedComponentDriver) run_import_couplers_smod

   use mapl_ErrorHandling
   use mapl3g_OuterMetaComponent
   use mapl3g_MethodPhasesMapUtils
   use mapl3g_CouplerPhases, only: GENERIC_COUPLER_INVALIDATE, GENERIC_COUPLER_UPDATE

   implicit none

contains

   recursive module subroutine run_import_couplers(this, rc)
      class(GriddedComponentDriver), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentDriverVectorIterator) :: iter
      class(ComponentDriver), pointer :: driver

      associate (e => this%import_couplers%ftn_end() )
        iter = this%import_couplers%ftn_begin()
        do while (iter /= e)
           call iter%next()
           driver => iter%of()
           call driver%run(phase_idx=GENERIC_COUPLER_UPDATE, _RC)
        end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine run_import_couplers

end submodule run_import_couplers_smod
