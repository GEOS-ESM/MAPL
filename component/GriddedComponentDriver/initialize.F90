#include "MAPL.h"

submodule(mapl3g_GriddedComponentDriver) initialize_smod
   use mapl_ErrorHandling
   implicit none(type,external)

contains


   recursive module subroutine initialize(this, unusable, phase_idx, rc)
      class(GriddedComponentDriver), target, intent(inout) :: this
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(in) :: phase_idx
      integer, optional, intent(out) :: rc

      integer :: status, user_status

      associate ( &
           importState => this%states%importState, &
           exportState => this%states%exportState)

        call ESMF_GridCompInitialize(this%gridcomp, &
             importState=importState, exportState=exportState, clock=this%clock, &
             phase=phase_idx, _USERRC)

      end associate

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine initialize

end submodule initialize_smod
