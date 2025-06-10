#include "MAPL_ErrLog.h"

module mapl3g_MultiState
   use mapl3g_ESMF_Utilities, only: write(formatted)
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use esmf
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

      procedure :: write_multistate
      generic :: write(formatted) => write_multistate

      procedure :: destroy
   end type MultiState

   interface MultiState
      procedure new_MultiState_user
   end interface MultiState

contains

   function new_MultiState_user(unusable, importState, exportState, internalState) result(multi_state)
      type(MultiState) :: multi_state
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_State), optional, intent(in) :: importState
      type(ESMF_State), optional, intent(in) :: exportState
      type(ESMF_State), optional, intent(in) :: internalState

      multi_state%importState = get_state('import', importState)
      multi_state%exportState = get_state('export', exportState)
      multi_state%internalState = get_state('internal', internalState)

      _UNUSED_DUMMY(unusable)
   contains

      function get_state(name, state) result(new_state)
         type(ESMF_State) :: new_state
         character(*), intent(in) :: name
         type(ESMF_State), optional, intent(in) :: state

         if (present(state)) then
            new_state = state
            return
         end if

         new_state = ESMF_StateCreate(name=name)

      end function get_state

   end function new_MultiState_user


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

   subroutine write_multistate(this, unit, iotype, v_list, iostat, iomsg)
      class(MultiState), intent(in) :: this
      integer, intent(in)         :: unit
      character(*), intent(in)    :: iotype
      integer, intent(in)         :: v_list (:)
      integer, intent(out)        :: iostat
      character(*), intent(inout) :: iomsg

#ifndef __GFORTRAN__
      write(unit,*, iostat=iostat, iomsg=iomsg) 'IMPORT:', this%importState
      write(unit,*, iostat=iostat, iomsg=iomsg) 'EXPORT:', this%exportState
#endif

      _UNUSED_DUMMY(iotype)
      _UNUSED_DUMMY(v_list)
   end subroutine write_multistate

   subroutine destroy(this, rc)
      class(MultiState), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call ESMF_StateDestroy(this%importState, _RC)
      call ESMF_StateDestroy(this%exportState, _RC)
      call ESMF_StateDestroy(this%internalState, _RC)

      _RETURN(_SUCCESS)
   end subroutine destroy

 end module mapl3g_MultiState
