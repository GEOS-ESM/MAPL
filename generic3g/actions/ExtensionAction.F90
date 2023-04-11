module mapl3g_ExtensionAction
   implicit none
   private

   public :: ExtensionAction

   type, abstract :: ExtensionAction
   contains
      procedure(I_run), deferred :: run
   end type ExtensionAction


   abstract interface
      subroutine I_run(this, rc)
         import ExtensionAction
         class(ExtensionAction), intent(inout) :: this
         integer, optional, intent(out) :: rc
      end subroutine I_run
   end interface

end module mapl3g_ExtensionAction


