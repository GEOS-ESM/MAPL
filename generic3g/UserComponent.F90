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
      class(AbstractUserSetServices), allocatable :: setservices_
      type(MethodPhasesMap), public :: phases_map
   contains
      procedure :: setservices
      procedure :: set_entry_point

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

   function new_UserComponent(gridcomp, setservices) result(user_component)
      type(UserComponent) :: user_component
      type(ESMF_GridComp), intent(in) :: gridcomp
      class(AbstractUserSetServices), intent(in) :: setservices

      user_component%gridcomp = gridcomp
      user_component%setservices_ = setservices

      ! Technically ESMF_StateCreate can fail which violates the unspoken rule that
      ! constructors cannot "fail".  The probability of this seems small,
      ! and a workaround can wait for that to be happen.  (TLC Dec 2023)
      associate ( &
        importState => ESMF_StateCreate(stateIntent=ESMF_STATEINTENT_IMPORT), &
        exportState => ESMF_StateCreate(stateIntent=ESMF_STATEINTENT_EXPORT), &
        internalState => ESMF_StateCreate(stateIntent=ESMF_STATEINTENT_INTERNAL))

        user_component%states = MultiState(importState=importState, exportState=exportState, internalState=internalState)
      end associate
      
      call initialize_phases_map(user_component%phases_map)

   end function new_UserComponent

   ! `host_gridcomp` is the MAPL generic gridcomp that wraps the user
   ! component.
   subroutine setservices(this, host_gridcomp, rc)
      class(UserComponent), intent(inout) :: this
      type(ESMF_Gridcomp), intent(in) :: host_gridcomp
      integer, optional, intent(out) :: rc

      integer :: status

      call attach_inner_meta(this%gridcomp, host_gridcomp, _RC)
      call this%setservices_%run(this%gridcomp, _RC)

      _RETURN(_SUCCESS)
   end subroutine setservices


   recursive subroutine initialize(this, clock, phase_name, rc)
      class(UserComponent), intent(inout) :: this
      type(ESMF_Clock), intent(inout) :: clock
      character(*), optional, intent(in) :: phase_name
      integer, intent(out) :: rc

      integer :: status
      integer :: userrc
      integer :: phase
      type(StringVector), pointer :: init_phases
      logical :: found

      _ASSERT(present(phase_name), 'phase_name is mandatory')

      init_phases => this%phases_map%at(ESMF_METHOD_INITIALIZE, _RC)
      associate (phase => get_phase_index(init_phases, phase_name=phase_name, found=found))
        _RETURN_UNLESS(found)

        associate ( &
             importState => this%states%importState, &
             exportState => this%states%exportState)
          
          call ESMF_GridCompInitialize(this%gridcomp, &
               importState=importState, exportState=exportState, &
               clock=clock, phase=phase, userRC=userrc, _RC)
          _VERIFY(userRC)
        end associate

      end associate

      _RETURN(_SUCCESS)
     end subroutine initialize

   recursive subroutine run(this, clock, phase_name, rc)
      class(UserComponent), intent(inout) :: this
      type(ESMF_Clock), intent(inout) :: clock
      character(*), optional, intent(in) :: phase_name
      integer, intent(out) :: rc

      integer :: status
      integer :: userrc
      logical :: found

      associate(phase_idx => get_phase_index(this%phases_map%of(ESMF_METHOD_RUN), phase_name=phase_name, found=found) )
        _ASSERT(found, "run phase: <"//phase_name//"> not found.")
        
        associate ( &
             importState => this%states%importState, &
             exportState => this%states%exportState)
          call ESMF_GridCompRun(this%gridcomp, &
               importState=importState, exportState=exportState, &
               clock=clock, phase=phase_idx, userrc=userrc, _RC)
          _VERIFY(userRC)

        end associate
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

   subroutine set_entry_point(this, method_flag, userProcedure, unusable, phase_name, rc)
      class(UserComponent), intent(inout) :: this
      type(ESMF_Method_Flag), intent(in) :: method_flag
      procedure(I_Run) :: userProcedure
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: phase_name
      integer, optional, intent(out) ::rc

      integer :: status
      character(:), allocatable :: phase_name_
      type(ESMF_GridComp) :: user_gridcomp
      logical :: found

      if (present(phase_name)) then
         phase_name_ = phase_name
      else
         phase_name_ = get_default_phase_name(method_flag)
      end if

      call add_phase(this%phases_map, method_flag=method_flag, phase_name=phase_name_, _RC)

      associate (phase_idx => get_phase_index(this%phases_map%of(method_flag), phase_name=phase_name_, found=found))
        _ASSERT(found, "run phase: <"//phase_name_//"> not found.")
        call ESMF_GridCompSetEntryPoint(this%gridcomp, method_flag, userProcedure, phase=phase_idx, _RC)
      end associate

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine set_entry_point


end module mapl3g_UserComponent
