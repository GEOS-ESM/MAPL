#include "MAPL_Generic.h"
#include "MAPL_Exceptions.h"
module AAA_GridComp

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

     call MAPL_GridCompSetEntryPoint ( gc, ESMF_METHOD_INITIALIZE,  my_initialize, __RC)
     call MAPL_GridCompSetEntryPoint ( gc, ESMF_METHOD_RUN,  my_run, __RC)

     call MAPL_AddExportSpec(gc,short_name='field1', long_name='NA',units='NA', &
                                 dims = MAPL_DimsHorzOnly, &
                                 vlocation = MAPL_VLocationNone, __RC)


     call MAPL_GenericSetServices(gc, __RC)
     __RETURN(__SUCCESS)

  end subroutine setservices


  subroutine my_initialize(gc, import, export, clock, rc)
     type(ESMF_GridComp), intent(inout) :: gc
     type(ESMF_State), intent(inout) :: import
     type(ESMF_State), intent(inout) :: export
     type(ESMF_Clock), intent(inout) :: clock
     integer, intent(out), optional :: rc

     integer :: status

     call MAPL_GenericInitialize(gc, import, export, clock, __RC)

     __RETURN(__SUCCESS)

  end subroutine my_initialize


  subroutine my_run(gc, import, export, clock, rc)
     type(ESMF_GridComp), intent(inout) :: gc
     type(ESMF_State), intent(inout) :: import
     type(ESMF_State), intent(inout) :: export
     type(ESMF_Clock), intent(inout) :: clock
     integer, intent(out), optional :: rc

     real, pointer :: ptr_2d(:,:)
     type(ESMF_Time) :: current_time, start_time
     type(ESMF_TimeInterval) :: time_interval
     real(ESMF_KIND_R8) :: relative_time
     integer :: status

     call MAPL_GetPointer(export,ptr_2d,'field1',__RC)
     call ESMF_ClockGet(clock,currTime=current_time,startTime=start_time,__RC)
     time_interval = current_time - start_time
     call ESMF_TimeIntervalGet(time_interval,h_r8=relative_time,__RC)
     if (associated(ptr_2d)) ptr_2d = relative_time

     __RETURN(__SUCCESS)

     __UNUSED_DUMMY(gc)
     __UNUSED_DUMMY(import)

  end subroutine my_run

end module AAA_GridComp

subroutine SetServices(gc, rc)
   use ESMF
   use AAA_GridComp, only : mySetservices=>SetServices
   type(ESMF_GridComp) :: gc
   integer, intent(out) :: rc
   call mySetServices(gc, rc=rc)
end subroutine
