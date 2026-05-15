#include "MAPL.h"

submodule(mapl3g_GriddedComponentDriver) finalize_smod
   use mapl_ErrorHandling
   implicit none(type,external)

contains

   module recursive subroutine finalize(this, unusable, phase_idx, rc)
      class(GriddedComponentDriver), target, intent(inout) :: this
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(in) :: phase_idx
      integer, optional, intent(out) :: rc

      integer :: status, user_status

      associate ( &
           importState => this%states%importState, &
           exportState => this%states%exportState)

        call ESMF_GridCompFinalize(this%gridcomp, &
             importState=importState, exportState=exportState, clock=this%clock, &
             phase=phase_idx, _USERRC)

      end associate

      call ESMF_GridCompDestroy(this%gridcomp, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine finalize

end submodule finalize_smod
