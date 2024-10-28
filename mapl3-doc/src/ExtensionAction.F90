#include "MAPL_Generic.h"
module mapl3g_ExtensionAction
   use mapl_ErrorHandling
   use ESMF
   implicit none
   private

   public :: ExtensionAction

   type, abstract :: ExtensionAction
   contains
      procedure(I_run), deferred :: initialize
      procedure(I_run), deferred :: update
      procedure :: invalidate
   end type ExtensionAction


   abstract interface
      subroutine I_run(this, importState, exportState, clock, rc)
         use ESMF
         import ExtensionAction
         class(ExtensionAction), intent(inout) :: this
         type(ESMF_State) :: importState
         type(ESMF_State) :: exportState
         type(ESMF_Clock) :: clock
         integer, optional, intent(out) :: rc
      end subroutine I_run
   end interface

contains

   ! This is a default no-op implementation of invalidate.
   ! Types derived from ExtensionAction should overload it
   ! as needed.
   subroutine invalidate(this, importState, exportState, clock, rc)
      use ESMF
      class(ExtensionAction), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, optional, intent(out) :: rc
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
   end subroutine invalidate

end module mapl3g_ExtensionAction
