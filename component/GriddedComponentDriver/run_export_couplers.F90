#include "MAPL.h"

submodule(mapl3g_GriddedComponentDriver) run_export_couplers_smod
   use mapl3g_CouplerPhases
   use mapl_ErrorHandling
   implicit none(type,external)

contains

   recursive module subroutine run_export_couplers(this, unusable, phase_idx, rc)
      class(GriddedComponentDriver), intent(inout) :: this
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(in) :: phase_idx
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentDriverVectorIterator) :: iter
      class(ComponentDriver), pointer :: driver

      associate (e => this%export_couplers%ftn_end() )
        iter = this%export_couplers%ftn_begin()
        do while (iter /= e)
           call iter%next()
           driver => iter%of()
           call driver%run(phase_idx=GENERIC_COUPLER_INVALIDATE, _RC)
        end do
      end associate

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine run_export_couplers

end submodule run_export_couplers_smod
