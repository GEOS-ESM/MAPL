#include "MAPL.h"

! An ExtendTransform is essentially a noop in which an export aquires
! a different aspect value.   This should only be usef for cases where
! an export "mirrors" the aspect of the source.

! The primary use case is for Expressions which do not have their own
! geometry, and instead evaluate on the geomentry of the import spec.
! Since this geometry can vary for multiple imports, we need distinct
! extensions for each.


module mapl3g_ExtendTransform
   use mapl3g_TransformId
   use mapl3g_ExtensionTransform
   use mapl_ErrorHandling
   implicit none
   private

   public :: ExtendTransform

   type, extends(ExtensionTransform) :: ExtendTransform
   contains
      procedure :: initialize
      procedure :: update
      procedure :: get_transformId
   end type ExtendTransform

   interface ExtendTransform
      procedure new_ExtendTransform
   end interface

contains

   function new_ExtendTransform() result(transform)
      type(ExtendTransform) :: transform
   end function new_ExtendTransform

   subroutine initialize(this, importState, exportState, clock, rc)
      use esmf
      class(ExtendTransform), intent(inout) :: this
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, optional, intent(out) :: rc

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
   end subroutine initialize

   subroutine update(this, importState, exportState, clock, rc)
      use esmf
      class(ExtendTransform), intent(inout) :: this
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, optional, intent(out) :: rc

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
  end subroutine update

   function get_transformId(this) result(id)
      type(TransformId) :: id
      class(ExtendTransform), intent(in) :: this

      id = EXTEND_TRANSFORM_ID
   end function get_transformId

end module mapl3g_ExtendTransform
