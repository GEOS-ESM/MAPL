#include "MAPL_ErrLog.h"

submodule(mapl3g_GriddedComponentDriver) read_restart_smod
   use :: mapl_ErrorHandling
   use :: mapl3g_OuterMetaComponent
   use :: mapl3g_MethodPhasesMapUtils
   implicit none (type, external)

contains

   module recursive subroutine read_restart(this, unusable, phase_idx, rc)
      class(GriddedComponentDriver), intent(inout) :: this
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(in) :: phase_idx
      integer, optional, intent(out) :: rc

      integer :: status, user_status

      associate ( &
           importState => this%states%importState, &
           exportState => this%states%exportState)

        call ESMF_GridCompReadRestart(this%gridcomp, &
             importState=importState, exportState=exportState, clock=this%clock, &
             phase=phase_idx, _USERRC)

      end associate

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine read_restart

end submodule read_restart_smod
