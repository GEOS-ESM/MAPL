module mapl3g_ChildComponent
   use :: esmf, only: ESMF_GridComp
   use :: esmf, only: ESMF_State
   use :: esmf, only: ESMF_Clock
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
   contains
      procedure, private :: run_self
      generic :: run => run_self
   end type ChildComponent

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
   end interface

end module mapl3g_ChildComponent
