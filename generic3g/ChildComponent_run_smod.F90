#include "MAPL_ErrLog.h"

submodule(mapl3g_ChildComponent) ChildComponent_run_smod
   use :: mapl_ErrorHandling
   use :: mapl3g_OuterMetaComponent
   use :: mapl3g_MethodPhasesMapUtils
   use :: mapl_KeywordEnforcer
   implicit none

contains

   module subroutine run_self(this, clock, unusable, phase_idx, rc)
      use mapl3g_OuterMetaComponent, only: get_outer_meta
      use mapl3g_OuterMetaComponent, only: OuterMetaComponent
      class(ChildComponent), intent(inout) :: this
      type(ESMF_Clock), intent(inout) :: clock
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: phase_idx
      integer, optional, intent(out) :: rc

      integer :: status, userRC
      integer :: phase
      type(OuterMetaComponent), pointer :: outer_meta

      outer_meta => get_outer_meta(this%gridcomp, _RC)

      call ESMF_GridCompRun(this%gridcomp, &
           importState=this%import_state, exportState=this%export_state, clock=clock, &
           phase=phase_idx, userRC=userRC, _RC)
      _VERIFY(userRC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine run_self

   module subroutine initialize_self(this, clock, unusable, phase_idx, rc)
      use mapl3g_OuterMetaComponent, only: get_outer_meta
      use mapl3g_OuterMetaComponent, only: OuterMetaComponent
      use mapl3g_GenericGridComp
      class(ChildComponent), intent(inout) :: this
      type(ESMF_Clock), intent(inout) :: clock
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: phase_idx
      integer, optional, intent(out) :: rc

      integer :: status, userRC
      type(OuterMetaComponent), pointer :: outer_meta

      outer_meta => get_outer_meta(this%gridcomp, _RC)

      call ESMF_GridCompInitialize(this%gridcomp, &
           importState=this%import_state, exportState=this%export_state, clock=clock, &
           phase=phase_idx, userRC=userRC, _RC)
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

   module function get_state_string_intent(this, state_intent, rc) result(state)
      type(ESMF_State) :: state
      class(ChildComponent), intent(inout) :: this
      character(*), intent(in) :: state_intent
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      select case (state_intent)
      case ('import')
         state = this%import_state
      case ('export')
         state = this%export_state
      case ('internal')
         outer_meta => get_outer_meta(this%gridcomp, _RC)
         state = outer_meta%get_internal_state()
      case default
         _FAIL('Unsupported state intent: <'//state_intent//'>.')
      end select

      _RETURN(_SUCCESS)
   end function get_state_string_intent

   module function get_state_esmf_intent(this, state_intent, rc) result(state)
      use mapl3g_VirtualConnectionPt, only: ESMF_STATEINTENT_INTERNAL
      type(ESMF_State) :: state
      class(ChildComponent), intent(inout) :: this
      type(ESMF_StateIntent_Flag), intent(in) :: state_intent
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: string_intent

      if (state_intent == ESMF_STATEINTENT_IMPORT) then
         string_intent = 'import'
      else if (state_intent == ESMF_STATEINTENT_EXPORT) then
         string_intent = 'export'
      else if (state_intent == ESMF_STATEINTENT_INTERNAL) then
         string_intent = 'internal'
      else
         string_intent = '<unknown>'
      end if

      state = this%get_state(string_intent, _RC)
      
      _RETURN(_SUCCESS)
   end function get_state_esmf_intent

end submodule ChildComponent_run_smod
