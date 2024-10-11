#include "MAPL_Generic.h"

module mapl3g_VerticalDimSpecRegridAction

   use mapl_ErrorHandling
   use mapl3g_ExtensionAction
   use mapl3g_VerticalDimSpec
   use MAPL_FieldUtils, only: assign_fptr
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

      integer :: top, bottom, status
      type(ESMF_Field) :: f_in, f_out
      real(kind=ESMF_KIND_R4), pointer :: x4_in(:,:,:), x4_out(:,:,:)

      call ESMF_StateGet(importState, itemName="import[1]", field=f_in, _RC)
      call ESMF_StateGet(exportState, itemName="export[1]", field=f_out, _RC)

      call ESMF_FieldGet(f_in, fArrayPtr=x4_in, _RC)
      call ESMF_FieldGet(f_out, fArrayPtr=x4_out, _RC)

      ! Compute edge average
      top = lbound(x4_in, 3)
      bottom = ubound(x4_in, 3)
      x4_out = 0.5 * (x4_in(:, :, top+1:bottom) + x4_in(:, :, top:bottom-1))

      _RETURN(_SUCCESS)
   end subroutine run

end module mapl3g_VerticalDimSpecRegridAction
