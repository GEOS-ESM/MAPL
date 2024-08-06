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
      type(ESMF_TypeKind_Flag) :: src_typekind
      type(ESMF_TypeKind_Flag) :: dst_typekind
      type(ESMF_Field) :: f_in, f_out
   contains
      procedure :: run
   end type CopyAction

   interface CopyAction
      module procedure new_CopyAction
      module procedure new_CopyAction2
   end interface CopyAction

contains

   function new_CopyAction(f_in, f_out) result(action)
      type(CopyAction) :: action
      type(ESMF_Field), intent(in) :: f_in
      type(ESMF_Field), intent(in) :: f_out

      action%f_in = f_in
      action%f_out = f_out
   end function new_CopyAction

   ! We don't really need to know the typekind as the low level conversion routines
   ! will accept whatever is handed. So these arguments are more to preserve
   ! a consistent form for constructions across Action subclasses.
   function new_CopyAction2(src_typekind, dst_typekind) result(action)
      type(CopyAction) :: action
      type(ESMF_Typekind_Flag), intent(in) :: src_typekind
      type(ESMF_Typekind_Flag), intent(in) :: dst_typekind

      action%src_typekind = src_typekind
      action%dst_typekind = dst_typekind

   end function new_CopyAction2

   subroutine run(this, rc)
      class(CopyAction), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call FieldCopy(this%f_in, this%f_out, _RC)

      _RETURN(_SUCCESS)
   end subroutine run

end module mapl3g_CopyAction
