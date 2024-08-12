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
      procedure :: initialize
      procedure :: run_old
      procedure :: run_new
   end type SequenceAction

contains

   subroutine initialize(this, importState, exportState, clock, rc)
      use esmf
      class(SequenceAction), intent(inout) :: this
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, optional, intent(out) :: rc
      _FAIL('Not implemented')
   end subroutine initialize

subroutine run_old(this, rc)
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
   end subroutine run_old

   subroutine run_new(this, importState, exportState, clock, rc)
      use esmf
      class(SequenceAction), intent(inout) :: this
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, optional, intent(out) :: rc
      _FAIL('Not implemented')
   end subroutine run_new

end module mapl3g_SequenceAction
