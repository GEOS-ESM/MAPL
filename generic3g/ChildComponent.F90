module mapl3g_ChildComponent
   use mapl3g_MultiState
   use :: esmf
   implicit none
   private

   public :: ChildComponent

   type :: ChildComponent
      private
      type(ESMF_GridComp) :: gridcomp
      type(MultiState) :: states
   contains
      procedure, private :: run_self
      procedure, private :: initialize_self
      procedure, private :: finalize_self
      generic :: run => run_self
      generic :: initialize => initialize_self
      generic :: finalize => finalize_self

      procedure :: get_states
      procedure :: get_outer_gridcomp

   end type ChildComponent

   interface ChildComponent
      module procedure new_ChildComponent
   end interface ChildComponent

   interface
      ! run_self() is implemented in submodule to avoid circular dependency
      ! on OuterMetaComponent.
      module subroutine run_self(this, clock, unusable, phase_idx, rc)
         use :: MaplShared, only: KeywordEnforcer
         class(ChildComponent), intent(inout) :: this
         type(ESMF_Clock), intent(inout) :: clock
         class(KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(in) :: phase_idx
         integer, optional, intent(out) :: rc
      end subroutine

      module subroutine initialize_self(this, clock, unusable, phase_idx, rc)
         use :: MaplShared, only: KeywordEnforcer
         class(ChildComponent), intent(inout) :: this
         type(ESMF_Clock), intent(inout) :: clock
         class(KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(in) :: phase_idx
         integer, optional, intent(out) :: rc
      end subroutine initialize_self

      module subroutine finalize_self(this, clock, unusable, phase_name, rc)
         use :: MaplShared, only: KeywordEnforcer
         class(ChildComponent), intent(inout) :: this
         type(ESMF_Clock), intent(inout) :: clock
         class(KeywordEnforcer), optional, intent(in) :: unusable
         character(len=*), optional, intent(in) :: phase_name
         integer, optional, intent(out) :: rc
      end subroutine finalize_self

      module function get_states(this) result(states)
         use mapl3g_MultiState
         type(MultiState) :: states
         class(ChildComponent), intent(in) :: this
      end function get_states

   end interface

contains

   function new_ChildComponent(gridcomp, multi_state) result(child)
      type(ChildComponent) :: child
      type(ESMF_GridComp), intent(in) :: gridcomp
      type(MultiState), intent(in) :: multi_state

      child%gridcomp = gridcomp
      child%states = multi_state

   end function new_ChildComponent

   function get_outer_gridcomp(this) result(gridcomp)
      use esmf, only: ESMF_GridComp
      type(ESMF_GridComp) :: gridcomp
      class(ChildComponent), intent(in) :: this
      gridcomp = this%gridcomp
   end function get_outer_gridcomp

end module mapl3g_ChildComponent
