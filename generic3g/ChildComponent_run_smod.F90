#include "MAPL_ErrLog.h"

submodule(mapl3g_ChildComponent) ChildComponent_run_smod
   use :: mapl_ErrorHandling
   use :: mapl3g_OuterMetaComponent
   use :: mapl_KeywordEnforcer
   implicit none

contains

   module subroutine run_self(this, clock, unusable, phase_name, rc)
      use mapl3g_OuterMetaComponent, only: get_outer_meta
      use mapl3g_OuterMetaComponent, only: OuterMetaComponent
      class(ChildComponent), intent(inout) :: this
      type(ESMF_Clock), intent(inout) :: clock
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: phase_name
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      outer_meta => get_outer_meta(this%gridcomp, _RC)

      call outer_meta%run( &
           importState=this%import_state, exportState=this%export_state, &
           clock=clock, phase_name=phase_name, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine run_self

end submodule ChildComponent_run_smod
