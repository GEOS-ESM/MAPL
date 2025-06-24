#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module MAPL_StubComponent
   use ESMF
   use mapl_MaplComponent
   use mapl_KeywordEnforcerMod
   use mapl_ErrorHandlingMod

   implicit none
   private

   public :: StubComponent

   type, extends(MaplComponent) :: StubComponent
      private
      
   contains
      procedure :: initialize
      procedure :: run
      procedure :: finalize
   end type StubComponent


contains

   subroutine initialize(this, import_state, export_state, clock, phase, unusable, rc)
      class(StubComponent), intent(inout) :: this
      type(ESMF_State), intent(inout) :: import_state
      type(ESMF_State), intent(inout) :: export_state
      type(ESMF_Clock), intent(inout) :: clock
      character(*), intent(in) :: phase
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      __UNUSED_DUMMY(this)
      __UNUSED_DUMMY(import_state)
      __UNUSED_DUMMY(export_state)
      __UNUSED_DUMMY(clock)
      __UNUSED_DUMMY(phase)
      __UNUSED_DUMMY(unusable)
      __RETURN(__FAILURE)

   end subroutine initialize

   subroutine run(this, import_state, export_state, clock, phase, unusable, rc)
      class(StubComponent), intent(inout) :: this
      type(ESMF_State), intent(inout) :: import_state
      type(ESMF_State), intent(inout) :: export_state
      type(ESMF_Clock), intent(inout) :: clock
      character(*), intent(in) :: phase
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      __UNUSED_DUMMY(this)
      __UNUSED_DUMMY(import_state)
      __UNUSED_DUMMY(export_state)
      __UNUSED_DUMMY(clock)
      __UNUSED_DUMMY(phase)
      __UNUSED_DUMMY(unusable)
      __RETURN(__FAILURE)
   end subroutine run
   
   subroutine finalize(this, import_state, export_state, clock, phase, unusable, rc)
      class(StubComponent), intent(inout) :: this
      type(ESMF_State), intent(inout) :: import_state
      type(ESMF_State), intent(inout) :: export_state
      type(ESMF_Clock), intent(inout) :: clock
      character(*), intent(in) :: phase
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      __UNUSED_DUMMY(this)
      __UNUSED_DUMMY(import_state)
      __UNUSED_DUMMY(export_state)
      __UNUSED_DUMMY(clock)
      __UNUSED_DUMMY(phase)
      __UNUSED_DUMMY(unusable)
      __RETURN(__FAILURE)
   end subroutine finalize

end module MAPL_StubComponent
   
