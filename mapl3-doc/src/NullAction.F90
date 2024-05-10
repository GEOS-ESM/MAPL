#include "MAPL_Generic.h"

! A NullAction object is just used so that a function that returns an
! ExtensionAction can allocate its return value in the presenc of
! error conditions.

module mapl3g_NullAction
   use mapl3g_ExtensionAction
   use mapl_ErrorHandling
   implicit none
   private

   public :: NullAction

   type, extends(ExtensionAction) :: NullAction
   contains
      procedure :: run
   end type NullAction

   interface NullAction
      procedure new_NullAction
   end interface

contains

   function new_NullAction() result(action)
      type(NullAction) :: action
   end function new_NullAction

   subroutine run(this, rc)
      class(NullAction), intent(inout) :: this
      integer, optional, intent(out) :: rc
      _FAIL('This procedure should not be called.')
   end subroutine run

end module mapl3g_NullAction
