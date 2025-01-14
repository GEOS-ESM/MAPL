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
      procedure :: runs_invalidate
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

   ! This is a default logical function that always return .FALSE.
   ! to determine if invalidate should run. Subclasses that run invalidate
   ! (override the invalidate subroutine nontrivially) need to implement
   ! a nontrivial override of this function.
   logical function run_invalidate(this)
      import ExtensionAction
      class(ExtensionAction), intent(in) :: this
      run_invalidate = .FALSE.
   end function run_invalidate

end module mapl3g_ExtensionAction
