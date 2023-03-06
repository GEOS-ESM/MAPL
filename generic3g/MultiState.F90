#include "MAPL_ErrLog.h"

module mapl3g_MultiState
   use esmf
   use mapl3g_VirtualConnectionPt ! for ESMF_STATEINTENT_INTERNAL until ESMF supports
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   implicit none
   private

   public :: MultiState
   type :: MultiState
      type(ESMF_State) :: internalState
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
   contains
      procedure :: get_state_by_string_intent
      procedure :: get_state_by_esmf_intent
      generic :: get_state => get_state_by_string_intent
      generic :: get_state => get_state_by_esmf_intent
   end type MultiState

   interface MultiState
      procedure newMultiState_user
   end interface MultiState

contains

   function newMultiState_user(unusable, importState, exportState, internalState) result(multi_state)
      type(MultiState) :: multi_state
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_State), optional, intent(in) :: importState
      type(ESMF_State), optional, intent(in) :: exportState
      type(ESMF_State), optional, intent(in) :: internalState

      if (present(importState)) multi_state%importState = importState
      if (present(exportState)) multi_state%exportState = exportState
      if (present(internalState)) multi_state%internalState = internalState

   end function newMultiState_user


   subroutine get_state_by_string_intent(this, state, state_intent, rc)
      class(MultiState), intent(in) :: this
      type(ESMF_State), intent(out) :: state
      character(*), intent(in) :: state_intent
      integer, optional, intent(out) :: rc

      integer :: status

      select case (state_intent)
      case ('import')
         state = this%importState
      case ('export')
         state = this%exportState
      case ('internal')
         state = this%internalState
      case default
         _FAIL('Unsupported state intent: <'//state_intent//'>.')
      end select

      call ESMF_StateValidate(state, _RC)
      
      _RETURN(_SUCCESS)
   end subroutine get_state_by_string_intent

   subroutine get_state_by_esmf_intent(this, state, state_intent, rc)
      class(MultiState), intent(in) :: this
      type(ESMF_State), intent(out) :: state
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

      call this%get_state(state, string_intent, _RC)
      
      _RETURN(_SUCCESS)
   end subroutine get_state_by_esmf_intent

end module mapl3g_MultiState
