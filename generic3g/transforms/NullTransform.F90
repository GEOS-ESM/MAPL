#include "MAPL_Generic.h"

! A NullTransform object is just used so that a function that returns an
! ExtensionTransform can allocate its return value in the presence of
! error conditions.

module mapl3g_NullTransform
   use mapl3g_ExtensionTransform
   use mapl_ErrorHandling
   implicit none
   private

   public :: NullTransform

   type, extends(ExtensionTransform) :: NullTransform
   contains
      procedure :: initialize
      procedure :: update
   end type NullTransform

   interface NullTransform
      procedure new_NullTransform
   end interface

contains

   function new_NullTransform() result(transform)
      type(NullTransform) :: transform
   end function new_NullTransform

   subroutine initialize(this, importState, exportState, clock, rc)
      use esmf
      class(NullTransform), intent(inout) :: this
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, optional, intent(out) :: rc
      _FAIL('This procedure should not be called.')
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
   end subroutine initialize

   subroutine update(this, importState, exportState, clock, rc)
      use esmf
      class(NullTransform), intent(inout) :: this
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, optional, intent(out) :: rc
      _FAIL('This procedure should not be called.')
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
  end subroutine update

end module mapl3g_NullTransform
