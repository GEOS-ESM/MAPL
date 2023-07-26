#include "MAPL_Generic.h"

module mapl3g_SequenceAction
   use mapl3g_ExtensionAction
   use mapl3g_ActionVector
   use mapl_ErrorHandling
   implicit none
   private

   public :: SequenceAction

   type, extends(ExtensionAction) :: SequenceAction
      type(ActionVector) :: actions
   contains
      procedure :: run
   end type SequenceAction

contains

   subroutine run(this, rc)
      class(SequenceAction), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i
      class(ExtensionAction), pointer :: action
      
      do i = 1, this%actions%size()
         action => this%actions%of(i)

         call action%run(_RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine run

end module mapl3g_SequenceAction
