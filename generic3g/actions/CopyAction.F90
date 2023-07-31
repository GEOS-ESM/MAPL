#include "MAPL_Generic.h"

! A copy might be between different kinds and precisions, so is really
! a converter.  But ... what is a better name.
module mapl3g_CopyAction
   use mapl3g_ExtensionAction
   use mapl_ErrorHandling
   use esmf
   use MAPL_FieldUtils
   implicit none

   type, extends(ExtensionAction) :: CopyAction
      private
      type(ESMF_Field) :: f_in, f_out
   contains
      procedure :: run
   end type CopyAction

   interface CopyAction
      module procedure new_CopyAction
   end interface CopyAction

contains

   function new_CopyAction(f_in, f_out) result(action)
      type(CopyAction) :: action
      type(ESMF_Field), intent(in) :: f_in
      type(ESMF_Field), intent(in) :: f_out

      action%f_in = f_in
      action%f_out = f_out
   end function new_CopyAction

   subroutine run(this, rc)
      class(CopyAction), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call FieldCopy(this%f_in, this%f_out, _RC)

      _RETURN(_SUCCESS)
   end subroutine run

end module mapl3g_CopyAction
