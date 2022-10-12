#include "MAPL_ErrLog.h"

submodule(mapl3g_ChildComponent) ChildComponent_run_smod
   use :: mapl_ErrorHandling
   use :: mapl3g_OuterMetaComponent
   use :: mapl3g_MethodPhasesMapUtils
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

      integer :: status, userRC
      integer :: phase
      type(OuterMetaComponent), pointer :: outer_meta

      outer_meta => get_outer_meta(this%gridcomp, _RC)
      phase = get_phase_index(outer_meta%get_phases(ESMF_METHOD_RUN), phase_name=phase_name, _RC)

      call ESMF_GridCompRun(this%gridcomp, &
           importState=this%import_state, exportState=this%export_state, clock=clock, &
           phase=phase, userRC=userRC, _RC)
      _VERIFY(userRC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine run_self

   module subroutine initialize_self(this, clock, unusable, phase_name, rc)
      use mapl3g_OuterMetaComponent, only: get_outer_meta
      use mapl3g_OuterMetaComponent, only: OuterMetaComponent
      use mapl3g_GenericGridComp
      class(ChildComponent), intent(inout) :: this
      type(ESMF_Clock), intent(inout) :: clock
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: phase_name
      integer, optional, intent(out) :: rc

      integer :: status, userRC
      integer :: phase
      type(OuterMetaComponent), pointer :: outer_meta

      outer_meta => get_outer_meta(this%gridcomp, _RC)
      phase = get_phase_index(outer_meta%get_phases(ESMF_METHOD_INITIALIZE), phase_name=phase_name, _RC)

      call ESMF_GridCompInitialize(this%gridcomp, &
           importState=this%import_state, exportState=this%export_state, clock=clock, &
           phase=phase, userRC=userRC, _RC)
      _VERIFY(userRC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine initialize_self

   module subroutine finalize_self(this, clock, unusable, phase_name, rc)
      use mapl3g_OuterMetaComponent, only: get_outer_meta
      use mapl3g_OuterMetaComponent, only: OuterMetaComponent
      class(ChildComponent), intent(inout) :: this
      type(ESMF_Clock), intent(inout) :: clock
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: phase_name
      integer, optional, intent(out) :: rc

      integer :: status, userRC
      integer :: phase
      type(OuterMetaComponent), pointer :: outer_meta

      outer_meta => get_outer_meta(this%gridcomp, _RC)
      phase = get_phase_index(outer_meta%get_phases(ESMF_METHOD_FINALIZE), phase_name=phase_name, _RC)

      call ESMF_GridCompFinalize(this%gridcomp, &
           importState=this%import_state, exportState=this%export_state, clock=clock, &
           phase=phase, userRC=userRC, _RC)
      _VERIFY(userRC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine finalize_self

end submodule ChildComponent_run_smod
