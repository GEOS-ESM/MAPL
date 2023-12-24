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

      procedure, private :: initialize_self
      procedure :: run_self
      procedure :: finalize_self

      generic :: initialize => initialize_self
      generic :: run => run_self
      generic :: finalize => finalize_self

      ! Accessors
      procedure :: get_gridcomp
      procedure :: get_states
      procedure :: get_name
   end type UserComponent

   interface UserComponent
      procedure :: new_UserComponent
   end interface UserComponent

   interface

      module recursive subroutine initialize_self(this, clock, unusable, phase_idx, rc)
         use :: MaplShared, only: KeywordEnforcer
         class(UserComponent), intent(inout) :: this
         type(ESMF_Clock), intent(inout) :: clock
         class(KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(in) :: phase_idx
         integer, optional, intent(out) :: rc
      end subroutine initialize_self


      module subroutine run_self(this, clock, unusable, phase_idx, rc)
         use :: MaplShared, only: KeywordEnforcer
         class(UserComponent), intent(inout) :: this
         type(ESMF_Clock), intent(inout) :: clock
         class(KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(in) :: phase_idx
         integer, optional, intent(out) :: rc
      end subroutine run_self

      module subroutine finalize_self(this, clock, unusable, phase_idx, rc)
         use :: MaplShared, only: KeywordEnforcer
         class(UserComponent), intent(inout) :: this
         type(ESMF_Clock), intent(inout) :: clock
         class(KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(in) :: phase_idx
         integer, optional, intent(out) :: rc
      end subroutine finalize_self


      module function get_states(this) result(states)
         use mapl3g_MultiState
         type(MultiState) :: states
         class(UserComponent), intent(in) :: this
      end function get_states

   end interface
contains

   function new_UserComponent(gridcomp, states) result(user_component)
      type(UserComponent) :: user_component
      type(ESMF_GridComp), intent(in) :: gridcomp
      type(MultiState) :: states

      user_component%gridcomp = gridcomp
      ! Technically ESMF_StateCreate can fail which violates the unspoken rule that
      ! constructors cannot "fail".  The probability of this seems small,
      ! and a workaround can wait for that to be happen.  (TLC Dec 2023)
      user_component%states = states

      
   end function new_UserComponent


   ! Accessors
   
   function get_gridcomp(this) result(gridcomp)
      type(ESMF_GridComp) :: gridcomp
      class(UserComponent), intent(in) :: this

      gridcomp = this%gridcomp
   end function get_gridcomp


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
