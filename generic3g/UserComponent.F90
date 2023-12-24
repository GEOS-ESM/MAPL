#include "MAPL_Generic.h"

! A user component bundles a user gridcomp with the various arguments
! to its methods.  This allows a parent/host component to readily
! manage these as a single entity, thereby reducing code complexity.

module mapl3g_UserComponent
   use mapl3g_MultiState
   use mapl3g_UserSetServices
   use mapl3g_MethodPhasesMap
   use mapl3g_InnerMetaComponent
   use mapl3g_ESMF_Interfaces, only: I_Run
   use mapl_ErrorHandling
   use mapl_KeywordEnforcerMod
   use gftl2_StringVector
   use esmf
   
   implicit none
   private

   public :: UserComponent

   type :: UserComponent
      private
      type(ESMF_GridComp) :: gridcomp
      type(MultiState) :: states
   contains

      procedure :: initialize
      procedure :: run
      procedure :: finalize

      ! Accessors
      procedure :: get_gridcomp
      procedure :: get_states
      procedure :: get_name
   end type UserComponent

   interface UserComponent
      procedure :: new_UserComponent
   end interface UserComponent

contains

   function new_UserComponent(gridcomp) result(user_component)
      type(UserComponent) :: user_component
      type(ESMF_GridComp), intent(in) :: gridcomp

      user_component%gridcomp = gridcomp

      ! Technically ESMF_StateCreate can fail which violates the unspoken rule that
      ! constructors cannot "fail".  The probability of this seems small,
      ! and a workaround can wait for that to be happen.  (TLC Dec 2023)
      associate ( &
        importState => ESMF_StateCreate(stateIntent=ESMF_STATEINTENT_IMPORT), &
        exportState => ESMF_StateCreate(stateIntent=ESMF_STATEINTENT_EXPORT), &
        internalState => ESMF_StateCreate(stateIntent=ESMF_STATEINTENT_INTERNAL))

        user_component%states = MultiState(importState=importState, exportState=exportState, internalState=internalState)
      end associate
      
   end function new_UserComponent


   recursive subroutine initialize(this, clock, phase_idx, rc)
      class(UserComponent), intent(inout) :: this
      type(ESMF_Clock), intent(inout) :: clock
      integer, intent(in) :: phase_idx
      integer, intent(out) :: rc

      integer :: status
      integer :: userrc

        associate ( &
             importState => this%states%importState, &
             exportState => this%states%exportState)
          
          call ESMF_GridCompInitialize(this%gridcomp, &
               importState=importState, exportState=exportState, &
               clock=clock, phase=phase_idx, userRC=userrc, _RC)
          _VERIFY(userRC)
        end associate

      _RETURN(_SUCCESS)
     end subroutine initialize

   recursive subroutine run(this, clock, phase_idx, rc)
      class(UserComponent), intent(inout) :: this
      type(ESMF_Clock), intent(inout) :: clock
      integer, optional, intent(in) :: phase_idx
      integer, intent(out) :: rc

      integer :: status
      integer :: userrc
        associate ( &
             importState => this%states%importState, &
             exportState => this%states%exportState)
          call ESMF_GridCompRun(this%gridcomp, &
               importState=importState, exportState=exportState, &
               clock=clock, phase=phase_idx, userrc=userrc, _RC)
          _VERIFY(userRC)

        end associate

      _RETURN(_SUCCESS)
   end subroutine run

   recursive subroutine finalize(this, clock, phase, rc)
      class(UserComponent), intent(inout) :: this
      type(ESMF_Clock), intent(inout) :: clock
      integer, optional, intent(in) :: phase
      integer, intent(out) :: rc

      integer :: status
      integer :: userrc

      associate ( &
           importState => this%states%importState, &
           exportState => this%states%exportState)
        call ESMF_GridCompFinalize(this%gridcomp, &
             importState=importState, exportState=exportState, &
             clock=clock, phase=phase, userrc=userrc, _RC)
        _VERIFY(userRC)

      end associate

      _RETURN(_SUCCESS)
   end subroutine finalize

   ! Accessors
   
   function get_gridcomp(this) result(gridcomp)
      type(ESMF_GridComp) :: gridcomp
      class(UserComponent), intent(in) :: this

      gridcomp = this%gridcomp
   end function get_gridcomp

   function get_states(this) result(states)
      type(MultiState) :: states
      class(UserComponent), intent(in) :: this

      states = this%states
   end function get_states

   function get_name(this, rc) result(name)
      character(:), allocatable :: name
      class(UserComponent), intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: buffer

      call ESMF_GridCompGet(this%gridcomp, name=buffer, _RC)
      name = trim(buffer)

      _RETURN(ESMF_SUCCESS)
   end function get_name

end module mapl3g_UserComponent
