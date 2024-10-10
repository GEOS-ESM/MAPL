#include "MAPL_Generic.h"

module mapl3g_VerticalDimSpecRegridAction

   use mapl_ErrorHandling
   use mapl3g_ExtensionAction
   use mapl3g_VerticalDimSpec
   use esmf

   implicit none

   type, extends(ExtensionAction) :: VerticalDimSpecRegridAction
      private
      type(VerticalDimSpec) :: src_vdimspec
      type(VerticalDimSpec) :: dst_vdimspec
   contains
      procedure :: initialize
      procedure :: run
   end type VerticalDimSpecRegridAction

   interface VerticalDimSpecRegridAction
      module procedure new_VerticalDimSpecRegridAction
   end interface VerticalDimSpecRegridAction

contains

   function new_VerticalDimSpecRegridAction(src_vdimspec, dst_vdimspec) result(action)
      type(VerticalDimSpecRegridAction) :: action
      type(VerticalDimSpec), intent(in) :: src_vdimspec
      type(VerticalDimSpec), intent(in) :: dst_vdimspec

      action%src_vdimspec = src_vdimspec
      action%dst_vdimspec = dst_vdimspec
   end function new_VerticalDimSpecRegridAction

   subroutine initialize(this, importState, exportState, clock, rc)
      use esmf
      class(VerticalDimSpecRegridAction), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock      
      integer, optional, intent(out) :: rc

      print *, "VerticalDimSpecRegridAction::initialize"
      ! No-op
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
   end subroutine initialize

   subroutine run(this, importState, exportState, clock, rc)
      use esmf
      class(VerticalDimSpecRegridAction), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock      
      integer, optional, intent(out) :: rc

      ! integer :: status
      ! type(ESMF_Field) :: f_in, f_out

      ! call ESMF_StateGet(importState, itemName='import[1]', field=f_in, _RC)
      ! call ESMF_StateGet(exportState, itemName='export[1]', field=f_out, _RC)

      ! call FieldCopy(f_in, f_out, _RC)

      _RETURN(_SUCCESS)
   end subroutine run

end module mapl3g_VerticalDimSpecRegridAction
