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
      procedure :: initialize
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

   subroutine initialize(this, importState, exportState, clock, rc)
      use esmf
      class(CopyAction), intent(inout) :: this
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, optional, intent(out) :: rc

      ! No-op

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
   end subroutine initialize

   subroutine run(this, importState, exportState, clock, rc)
      use esmf
      class(CopyAction), intent(inout) :: this
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Field) :: f_in, f_out

      call ESMF_StateGet(importState, itemName='import[1]', field=f_in, _RC)
      call ESMF_StateGet(exportState, itemName='export[1]', field=f_out, _RC)

      call FieldCopy(f_in, f_out, _RC)

      _RETURN(_SUCCESS)
   end subroutine run


end module mapl3g_CopyAction
