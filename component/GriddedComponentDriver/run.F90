#include "MAPL.h"

submodule(mapl3g_GriddedComponentDriver) run_smod
   use mapl_ErrorHandling
   implicit none(type,external)

contains

   module recursive subroutine run(this, unusable, phase_idx, rc)
      class(GriddedComponentDriver), intent(inout) :: this
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(in) :: phase_idx
      integer, optional, intent(out) :: rc

      integer :: status, user_status

      _ASSERT(present(phase_idx), 'until made not optional')
      
      associate ( &
           importState => this%states%importState, &
           exportState => this%states%exportState)

        call ESMF_GridCompRun(this%gridcomp, &
             importState=importState, &
             exportState=exportState, &
             clock=this%clock, &
             phase=phase_idx, _USERRC)
      end associate

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine run

end submodule run_smod
