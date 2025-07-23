#include "MAPL.h"
#include "MAPL_Exceptions.h"
module ParentTwoSiblings_GridComp

  use ESMF
  use MAPL

  implicit none
  private

  public setservices

  integer :: child1
  integer :: child2

  contains

  subroutine setservices(gc,rc)

     type(ESMF_GridComp), intent(inout)  :: gc
     integer, optional :: rc

     integer :: status
     type(MAPL_MetaComp), pointer :: MAPL
     character(len=80) :: my_child1_name, my_child1_so
     character(len=80) :: my_child2_name, my_child2_so

     call MAPL_GetObjectFromGC ( GC, MAPL, _RC)
     call MAPL_GetResource(MAPL, my_child1_name, Label="my_child1_name:",_RC)
     call MAPL_GetResource(MAPL, my_child1_so, Label="my_child1_so:",_RC)
     call MAPL_GetResource(MAPL, my_child2_name, Label="my_child2_name:",_RC)
     call MAPL_GetResource(MAPL, my_child2_so, Label="my_child2_so:",_RC)

     call MAPL_GridCompSetEntryPoint ( gc, ESMF_METHOD_INITIALIZE,  my_initialize, _RC)
     call MAPL_GridCompSetEntryPoint ( gc, ESMF_METHOD_RUN,  my_run, _RC)

     call MAPL_AddExportSpec(gc,short_name='output1', long_name='NA',units='NA', &
                                 dims = MAPL_DimsHorzOnly, &
                                 vlocation = MAPL_VLocationNone, _RC)
 
     child1 = MAPL_AddChild(gc, my_child1_name, "setservices_", sharedObj=my_child1_so, _RC)     
     child2 = MAPL_AddChild(gc, my_child2_name, "setservices_", sharedObj=my_child2_so, _RC)     
     call MAPL_AddConnectivity(gc, &
          short_name = ["field1"],  &
          src_id = child1, &
          dst_id = child2, &
          _RC)
          
     call MAPL_GenericSetServices(gc, _RC)
     _RETURN(_SUCCESS)

  end subroutine setservices


  subroutine my_initialize(gc, import, export, clock, rc)
     type(ESMF_GridComp), intent(inout) :: gc
     type(ESMF_State), intent(inout) :: import
     type(ESMF_State), intent(inout) :: export
     type(ESMF_Clock), intent(inout) :: clock
     integer, intent(out), optional :: rc

     integer :: status

     call MAPL_GridCreate(gc, _RC)
     call MAPL_GenericInitialize(gc, import, export, clock, _RC)

     _RETURN(_SUCCESS)

  end subroutine my_initialize

    
  subroutine my_run(gc, import, export, clock, rc)
     type(ESMF_GridComp), intent(inout) :: gc
     type(ESMF_State), intent(inout) :: import
     type(ESMF_State), intent(inout) :: export
     type(ESMF_Clock), intent(inout) :: clock
     integer, intent(out), optional :: rc

     type(MAPL_MetaComp), pointer :: MAPL
     real, pointer :: ptr_2d(:,:)
     real :: my_constant
     integer :: status

     call MAPL_GetObjectFromGC ( GC, MAPL, _RC)
     call MAPL_GetResource(MAPL, my_constant, Label="my_value:", default=17.0,_RC)
     call MAPL_GetPointer(export,ptr_2d,'output1',_RC)
     if (associated(ptr_2d)) ptr_2d = my_constant

     call MAPL_GenericRunChildren(gc, import, export, clock, _RC)

     _RETURN(_SUCCESS)

  end subroutine my_run

end module ParentTwoSiblings_GridComp

subroutine SetServices(gc, rc)
   use ESMF
   use ParentTwoSiblings_GridComp, only : mySetservices=>SetServices
   type(ESMF_GridComp) :: gc
   integer, intent(out) :: rc
   call mySetServices(gc, rc=rc)
end subroutine  
