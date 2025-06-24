#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"

module UserComponent_mod
   use ESMF
   use mapl_keywordenforcermod
   use mapl_MaplComponent
   use mapl_ErrorHandlingMod
   implicit none
   private

   public :: UserComponent

   type, extends(MaplComponent) :: UserComponent
   contains
      procedure :: initialize => stub
      procedure :: run => stub
      procedure :: finalize => stub
   end type UserComponent

contains

   subroutine stub(this, import_state, export_state, clock, phase, unusable, rc)
      class(UserComponent), intent(inout) :: this
      type(ESMF_State), intent(inout) :: import_state
      type(ESMF_State), intent(inout) :: export_state
      type(ESMF_Clock), intent(inout) :: clock
      character(*), intent(in) :: phase
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      __UNUSED_DUMMY(import_state)
      __UNUSED_DUMMY(export_state)
      __UNUSED_DUMMY(clock)
      __UNUSED_DUMMY(phase)
      __UNUSED_DUMMY(unusable)
      
      print*,this%get_name(), ' says "Hi there."'
      __RETURN(__SUCCESS)
   end subroutine stub
   

   
end module UserComponent_mod
