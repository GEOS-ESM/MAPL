#include "MAPL_ErrLog.h"

submodule(mapl3g_ComponentDriver) ComponentDriver_run_smod
   use :: mapl_ErrorHandling
   use :: mapl3g_OuterMetaComponent
   use :: mapl3g_MethodPhasesMapUtils
   use :: mapl_KeywordEnforcer
   implicit none

contains

   module recursive subroutine run_self(this, clock, unusable, phase_idx, rc)
      class(ComponentDriver), intent(inout) :: this
      type(ESMF_Clock), intent(inout) :: clock
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: phase_idx
      integer, optional, intent(out) :: rc

      integer :: status, userRC

      associate ( &
           importState => this%states%importState, &
           exportState => this%states%exportState)

        call ESMF_GridCompRun(this%gridcomp, &
             importState=importState, &
             exportState=exportState, &
             clock=clock, &
             phase=phase_idx, userRC=userRC, _RC)
        _VERIFY(userRC)
      end associate

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine run_self

   recursive module subroutine initialize_self(this, clock, unusable, phase_idx, rc)
      class(ComponentDriver), intent(inout) :: this
      type(ESMF_Clock), intent(inout) :: clock
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: phase_idx
      integer, optional, intent(out) :: rc

      integer :: status, userRC

      associate ( &
           importState => this%states%importState, &
           exportState => this%states%exportState)

        call ESMF_GridCompInitialize(this%gridcomp, &
             importState=importState, exportState=exportState, clock=clock, &
             phase=phase_idx, userRC=userRC, _RC)
        _VERIFY(userRC)

      end associate

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine initialize_self

   module recursive subroutine finalize_self(this, clock, unusable, phase_idx, rc)
      class(ComponentDriver), intent(inout) :: this
      type(ESMF_Clock), intent(inout) :: clock
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: phase_idx
      integer, optional, intent(out) :: rc

      integer :: status, userRC

      associate ( &
           importState => this%states%importState, &
           exportState => this%states%exportState)

        call ESMF_GridCompFinalize(this%gridcomp, &
             importState=importState, exportState=exportState, clock=clock, &
             phase=phase_idx, userRC=userRC, _RC)
        _VERIFY(userRC)
      end associate

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine finalize_self

   module function get_states(this) result(states)
      type(MultiState) :: states
      class(ComponentDriver), intent(in) :: this

      states = this%states
   end function get_states

end submodule ComponentDriver_run_smod
