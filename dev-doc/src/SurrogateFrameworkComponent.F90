module mapl_SurrogateFrameworkComponent
   implicit none
   private

   public :: SurrogateFrameworkComponent
   
   type, abstract :: SurrogateFrameworkComponent
   contains
      procedure(i_Run), deferred :: initialize
      procedure(i_Run), deferred :: run
      procedure(i_Run), deferred :: finalize
      procedure(i_RunChild), deferred :: run_child
   end type SurrogateFrameworkComponent

   abstract interface

      subroutine i_Run(this, clock, phase, unusable, rc)
         use mapl_KeywordEnforcerMod
         use ESMF
         import SurrogateFrameworkComponent
         class(SurrogateFrameworkComponent), intent(inout) :: this
         type(ESMF_Clock), intent(inout) :: clock
         character(*), intent(in) :: phase
         class(KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc
      end subroutine i_Run

      subroutine i_RunChild(this, name, clock, phase, unusable, rc)
         use mapl_KeywordEnforcerMod
         use ESMF
         import SurrogateFrameworkComponent
         class(SurrogateFrameworkComponent), intent(inout) :: this
         character(*), intent(in) :: name
         type(ESMF_Clock), intent(inout) :: clock
         character(*), intent(in) :: phase
         class(KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc
      end subroutine i_RunChild

   end interface


end module mapl_SurrogateFrameworkComponent
