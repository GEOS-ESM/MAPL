#include "MAPL_Generic.h"

! A copy might be between different kinds and precisions, so is really
! a converter.  But ... what is a better name.
module mapl3g_CopyAction
   use mapl3g_ExtensionAction
   use mapl_ErrorHandling
   use esmf
   implicit none

   type, extends(ExtensionAction) :: CopyAction
      type(ESMF_Field) :: f_in, f_out
   contains
      procedure :: run
   end type CopyAction

contains

   subroutine run(this, rc)
      class(CopyAction), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R4), pointer :: x_in(:,:)
      real(kind=ESMF_KIND_R8), pointer :: x_out(:,:)

      call ESMF_FieldGet(this%f_in, farrayPtr=x_in, _RC)
      call ESMF_FieldGet(this%f_out, farrayPtr=x_out, _RC)

      x_out = x_in
      
      _RETURN(_SUCCESS)
   end subroutine run

end module mapl3g_CopyAction
