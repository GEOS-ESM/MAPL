#include "MAPL_Generic.h"
module mapl3g_ExtensionAction
   use mapl_ErrorHandling
   use ESMF
   implicit none
   private

   public :: ExtensionAction

   type, abstract :: ExtensionAction
   contains
      procedure(I_run), deferred :: initialize
      procedure(I_run), deferred :: run
   end type ExtensionAction


   abstract interface
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
