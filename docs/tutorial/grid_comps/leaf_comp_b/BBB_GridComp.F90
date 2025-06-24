#include "MAPL_Generic.h"
#include "MAPL_Exceptions.h"
module BBB_GridComp

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

     call MAPL_AddImportSpec(gc,short_name='field1', long_name='NA',units='NA', &
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
     integer :: status

     call MAPL_GetPointer(import,ptr_2d,'field1',__RC)
     write(*,*)"BBB import 1 maxval: ",maxval(ptr_2d)

     __RETURN(__SUCCESS)

     __UNUSED_DUMMY(gc)
     __UNUSED_DUMMY(export)
     __UNUSED_DUMMY(clock)

  end subroutine my_run

end module BBB_GridComp

subroutine SetServices(gc, rc)
   use ESMF
   use BBB_GridComp, only : mySetservices=>SetServices
   type(ESMF_GridComp) :: gc
   integer, intent(out) :: rc
   call mySetServices(gc, rc=rc)
end subroutine
