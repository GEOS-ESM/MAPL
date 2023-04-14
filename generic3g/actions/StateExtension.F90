#include "MAPL_Generic.h"

module mapl3g_StateExtension
   use mapl3g_ExtensionAction
!!$   use mapl3g_ActualConnectionPt
   use mapl_ErrorHandling
   implicit none
   private

   public :: StateExtension
   
   type StateExtension
!!$      type(ActualConnectionPt) :: src_actual_pt
!!$      type(ActualConnectionPt) :: dst_actual_pt
      class(ExtensionAction), allocatable :: action
   contains
      procedure :: run
   end type StateExtension

   interface StateExtension
      module procedure new_StateExtension
   end interface StateExtension

contains

   function new_StateExtension(action) result(extension)
      type(StateExtension) :: extension
      class(ExtensionAction), intent(in) :: action

      extension%action = action
   end function new_StateExtension

   subroutine run(this, rc)
      class(StateExtension), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      call this%action%run(_RC)

      _RETURN(_SUCCESS)
   end subroutine run

   
end module mapl3g_StateExtension
