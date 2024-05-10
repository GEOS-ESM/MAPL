#include "MAPL_Generic.h"

module mapl3g_BundleAction
   use mapl3g_ExtensionAction
   use mapl3g_ActionVector
   use mapl_ErrorHandling
   implicit none
   private

   public :: BundleAction

   type, extends(ExtensionAction) :: BundleAction
      private
      type(ActionVector) :: actions
   contains
      procedure :: run
      procedure :: add_action
   end type BundleAction

   interface BundleAction
      procedure new_BundleAction
   end interface BundleAction

contains

   function new_BundleAction() result(action)
      type(BundleAction) :: action
      action%actions = ActionVector()
   end function new_BundleAction

   subroutine run(this, rc)
      class(BundleAction), intent(inout) :: this
      integer, optional, intent(out) :: rc
      
      integer :: i
      integer :: status
      class(ExtensionAction), pointer :: action
      
      do i = 1, this%actions%size()
         action => this%actions%of(i)
         call action%run(_RC)
      end do
      
      _RETURN(_SUCCESS)
   end subroutine run
   
   subroutine add_action(this, action)
      class(BundleAction), intent(inout) :: this
      class(ExtensionAction), intent(in) :: action

      call this%actions%push_back(action)
   end subroutine add_action
   
end module mapl3g_BundleAction
