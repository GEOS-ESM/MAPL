module mapl3g_ChildComponent
   use :: esmf
   implicit none
   private

   public :: ChildComponent

   ! This is a _struct_ not a class: components are intentionally
   ! PUBLIC
   type :: ChildComponent
      type(ESMF_GridComp) :: gridcomp
      type(ESMF_State)    :: import_state
      type(ESMF_State)    :: export_state
      type(ESMF_State)    :: internal_state
!!$      type(CouplerComponentVector) :: couplers
   contains
      procedure, private :: run_self
      procedure, private :: initialize_self
      procedure, private :: finalize_self
      generic :: run => run_self
      generic :: initialize => initialize_self
      generic :: finalize => finalize_self

   end type ChildComponent

   interface ChildComponent
      module procedure new_ChildComponent
   end interface ChildComponent

   interface
      ! run_self() is implemented in submodule to avoid circular dependency
      ! on OuterMetaComponent.
      module subroutine run_self(this, clock, unusable, phase_name, rc)
         use :: MaplShared, only: KeywordEnforcer
         class(ChildComponent), intent(inout) :: this
         type(ESMF_Clock), intent(inout) :: clock
         class(KeywordEnforcer), optional, intent(in) :: unusable
         character(len=*), optional, intent(in) :: phase_name
         integer, optional, intent(out) :: rc
      end subroutine

      module subroutine initialize_self(this, clock, unusable, phase_name, rc)
         use :: MaplShared, only: KeywordEnforcer
         class(ChildComponent), intent(inout) :: this
         type(ESMF_Clock), intent(inout) :: clock
         class(KeywordEnforcer), optional, intent(in) :: unusable
         character(len=*), optional, intent(in) :: phase_name
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

   end interface

contains

   function new_ChildComponent(gridcomp) result(child)
      type(ChildComponent) :: child
      type(ESMF_GridComp), intent(in) :: gridcomp

      child%gridcomp = gridcomp
      
   end function new_ChildComponent

end module mapl3g_ChildComponent
