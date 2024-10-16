#include "MAPL_Generic.h"

! A NullAction object is just used so that a function that returns an
! ExtensionAction can allocate its return value in the presence of
! error conditions.

module mapl3g_NullAction
   use mapl3g_ExtensionAction
   use mapl_ErrorHandling
   implicit none (type, external)
   private

   public :: NullAction

   type, extends(ExtensionAction) :: NullAction
   contains
      procedure :: initialize
      procedure :: run
   end type NullAction

   interface NullAction
      procedure new_NullAction
   end interface

contains

   function new_NullAction() result(action)
      type(NullAction) :: action
   end function new_NullAction

   subroutine initialize(this, importState, exportState, clock, rc)
      use esmf
      class(NullAction), intent(inout) :: this
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

   subroutine run(this, importState, exportState, clock, rc)
      use esmf
      class(NullAction), intent(inout) :: this
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, optional, intent(out) :: rc
      _FAIL('This procedure should not be called.')
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
  end subroutine run

end module mapl3g_NullAction
