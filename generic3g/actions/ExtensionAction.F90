module mapl3g_ExtensionAction
   implicit none
   private

   public :: ExtensionAction

   type, abstract :: ExtensionAction
   contains
      procedure(I_run_extension), deferred :: run_old
      procedure(I_Run), deferred :: run_new
      generic :: run => run_old, run_new
      procedure(I_run), deferred :: initialize
   end type ExtensionAction


   abstract interface
      subroutine I_run_extension(this, rc)
         import ExtensionAction
         class(ExtensionAction), intent(inout) :: this
         integer, optional, intent(out) :: rc
      end subroutine I_run_extension

      subroutine I_run(this, importState, exportState, clock, rc)
         use ESMF
         import ExtensionAction
         class(ExtensionAction), intent(inout) :: this
         type(ESMF_State) :: importState
         type(ESMF_State) :: exportState
         type(ESMF_Clock) :: clock
         integer, optional, intent(out) :: rc
      end subroutine I_run
   end interface

end module mapl3g_ExtensionAction
