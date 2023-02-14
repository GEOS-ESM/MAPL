module mapl3g_ChildComponent
   use :: esmf
   implicit none
   private

   public :: ChildComponent

   type :: ChildComponent
      private
      type(ESMF_GridComp) :: gridcomp
      type(ESMF_State)    :: import_state
      type(ESMF_State)    :: export_state
   contains
      procedure, private :: run_self
      procedure, private :: initialize_self
      procedure, private :: finalize_self
      generic :: run => run_self
      generic :: initialize => initialize_self
      generic :: finalize => finalize_self

      procedure :: get_state_string_intent
      procedure :: get_state_esmf_intent
      generic :: get_state => get_state_string_intent
      generic :: get_state => get_state_esmf_intent

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

      module function get_state_string_intent(this, state_intent, rc) result(state)
         use esmf, only: ESMF_State
         type(ESMF_State) :: state
         class(ChildComponent), intent(inout) :: this
         character(*), intent(in) :: state_intent
         integer, optional, intent(out) :: rc
      end function

      module function get_state_esmf_intent(this, state_intent, rc) result(state)
         use esmf, only: ESMF_State, ESMF_StateIntent_Flag
         type(ESMF_State) :: state
         class(ChildComponent), intent(inout) :: this
         type(ESMF_StateIntent_Flag), intent(in) :: state_intent
         integer, optional, intent(out) :: rc
      end function

   end interface

contains

   function new_ChildComponent(gridcomp) result(child)
      type(ChildComponent) :: child
      type(ESMF_GridComp), intent(in) :: gridcomp

      child%gridcomp = gridcomp
      child%import_state = ESMF_StateCreate()
      child%export_state = ESMF_StateCreate()
      
   end function new_ChildComponent

   function get_outer_gridcomp(this) result(gridcomp)
      use esmf, only: ESMF_GridComp
      type(ESMF_GridComp) :: gridcomp
      class(ChildComponent), intent(in) :: this
      gridcomp = this%gridcomp
   end function get_outer_gridcomp

end module mapl3g_ChildComponent
