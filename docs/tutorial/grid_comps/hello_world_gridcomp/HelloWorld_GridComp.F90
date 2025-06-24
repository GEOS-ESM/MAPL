#include "MAPL_Generic.h"
#include "MAPL_Exceptions.h"
module HelloWorld_GridComp

  use ESMF
  use MAPL

  implicit none
  private

  public setservices

  contains

  subroutine setservices(gc,rc)

     type(ESMF_GridComp), intent(inout)  :: gc
     integer, optional :: rc

     integer :: status

     call MAPL_GridCompSetEntryPoint ( gc, ESMF_METHOD_INITIALIZE,  my_initialize, _rc)
     call MAPL_GridCompSetEntryPoint ( gc, ESMF_METHOD_RUN,  my_run, _rc)
     call MAPL_GenericSetServices(gc, _rc)
     _return(_success)

  end subroutine setservices


  subroutine my_initialize(gc, import, export, clock, rc)
     type(ESMF_GridComp), intent(inout) :: gc
     type(ESMF_State), intent(inout) :: import
     type(ESMF_State), intent(inout) :: export
     type(ESMF_Clock), intent(inout) :: clock
     integer, intent(out), optional :: rc

     integer :: status

     call MAPL_GridCreate(gc, _rc)
     call MAPL_GenericInitialize(gc, import, export, clock, _rc)

     _return(_success)

  end subroutine my_initialize


  subroutine my_run(gc, import, export, clock, rc)
     type(ESMF_GridComp), intent(inout) :: gc
     type(ESMF_State), intent(inout) :: import
     type(ESMF_State), intent(inout) :: export
     type(ESMF_Clock), intent(inout) :: clock
     integer, intent(out), optional :: rc

     type(ESMF_Time) :: current_time
     integer :: status

     call ESMF_ClockGet(clock,currTime=current_time,_rc)
     write(*,*)
     write(*,*)
     write(*,*)"Hello World, I say the time is:"
     call ESMF_TimePrint(current_time,options='string',_rc)

     _return(_success)

     _unused_dummy(gc)
     _unused_dummy(import)
     _unused_dummy(export)

  end subroutine my_run

end module HelloWorld_GridComp

subroutine SetServices(gc, rc)
   use ESMF
   use HelloWorld_GridComp, only : mySetservices=>SetServices
   type(ESMF_GridComp) :: gc
   integer, intent(out) :: rc
   call mySetServices(gc, rc=rc)
end subroutine
