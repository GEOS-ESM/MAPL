module mapl3g_ComponentHandler
   use mapl3g_MultiState
   use :: esmf
   implicit none
   private

   public :: ComponentHandler

   type :: ComponentHandler
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
      procedure :: get_gridcomp

   end type ComponentHandler

   interface ComponentHandler
      module procedure new_ComponentHandler
   end interface ComponentHandler

   interface

      module recursive subroutine initialize_self(this, clock, unusable, phase_idx, rc)
         use :: MaplShared, only: KeywordEnforcer
         class(ComponentHandler), intent(inout) :: this
         type(ESMF_Clock), intent(inout) :: clock
         class(KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(in) :: phase_idx
         integer, optional, intent(out) :: rc
      end subroutine initialize_self

      ! run_self() is implemented in submodule to avoid circular dependency
      ! on OuterMetaComponent.
      module subroutine run_self(this, clock, unusable, phase_idx, rc)
         use :: MaplShared, only: KeywordEnforcer
         class(ComponentHandler), intent(inout) :: this
         type(ESMF_Clock), intent(inout) :: clock
         class(KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(in) :: phase_idx
         integer, optional, intent(out) :: rc
      end subroutine

      module subroutine finalize_self(this, clock, unusable, phase_idx, rc)
         use :: MaplShared, only: KeywordEnforcer
         class(ComponentHandler), intent(inout) :: this
         type(ESMF_Clock), intent(inout) :: clock
         class(KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(in) :: phase_idx
         integer, optional, intent(out) :: rc
      end subroutine finalize_self

      module function get_states(this) result(states)
         use mapl3g_MultiState
         type(MultiState) :: states
         class(ComponentHandler), intent(in) :: this
      end function get_states

   end interface

contains

   function new_ComponentHandler(gridcomp, states) result(child)
      type(ComponentHandler) :: child
      type(ESMF_GridComp), intent(in) :: gridcomp
      type(MultiState), intent(in) :: states

      child%gridcomp = gridcomp
      child%states = states

   end function new_ComponentHandler

   function get_gridcomp(this) result(gridcomp)
      use esmf, only: ESMF_GridComp
      type(ESMF_GridComp) :: gridcomp
      class(ComponentHandler), intent(in) :: this
      gridcomp = this%gridcomp
   end function get_gridcomp

end module mapl3g_ComponentHandler
