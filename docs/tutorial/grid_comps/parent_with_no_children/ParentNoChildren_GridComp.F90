#include "MAPL_Generic.h"
#include "MAPL_Exceptions.h"
module ParentNoChild_GridComp

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

     call MAPL_AddExportSpec(gc,short_name='output1', long_name='NA',units='NA', &
                                 dims = MAPL_DimsHorzOnly, &
                                 vlocation = MAPL_VLocationNone, __RC)
     call MAPL_AddExportSpec(gc,short_name='output2', long_name='NA',units='NA', &
                                 dims = MAPL_DimsHorzVert, &
                                 vlocation = MAPL_VLocationCenter, __RC)



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

     call MAPL_GridCreate(gc, __RC)
     call MAPL_GenericInitialize(gc, import, export, clock, __RC)

     __RETURN(__SUCCESS)

  end subroutine my_initialize


  subroutine my_run(gc, import, export, clock, rc)
     type(ESMF_GridComp), intent(inout) :: gc
     type(ESMF_State), intent(inout) :: import
     type(ESMF_State), intent(inout) :: export
     type(ESMF_Clock), intent(inout) :: clock
     integer, intent(out), optional :: rc

     real, pointer :: ptr_2d(:,:), ptr_3d(:,:,:)
     type (MAPL_MetaComp),      pointer  :: MAPL
     real :: my_constant
     integer :: status

     call MAPL_GetObjectFromGC ( GC, MAPL, __RC)
     call MAPL_GetResource(MAPL, my_constant, Label="my_value:", default=17.0,__RC)
     call MAPL_GetPointer(export,ptr_2d,'output1',__RC)
     if (associated(ptr_2d)) ptr_2d = my_constant
     call MAPL_GetPointer(export,ptr_3d,'output2',__RC)
     if (associated(ptr_3d)) ptr_3d = my_constant

     __RETURN(__SUCCESS)

     __UNUSED_DUMMY(import)
     __UNUSED_DUMMY(clock)

  end subroutine my_run

end module ParentNoChild_GridComp

subroutine SetServices(gc, rc)
   use ESMF
   use ParentNoChild_GridComp, only : mySetservices=>SetServices
   type(ESMF_GridComp) :: gc
   integer, intent(out) :: rc
   call mySetServices(gc, rc=rc)
end subroutine
