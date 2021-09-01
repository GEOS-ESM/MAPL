#include "MAPL_Generic.h"

module AEIO_TransferGridComp
   use MAPL_ExceptionHandling
   use ESMF
   
   implicit none
   private

   public set_services

contains

   subroutine set_services(gc,rc)
      type(ESMF_GridComp) :: gc
      integer, intent(out) :: rc
      integer :: status

      call ESMF_GridCompSetEntryPoint(gc, ESMF_METHOD_INITIALIZE, userRoutine = create_field, phase=1, rc = status)
      _VERIFY(status)
      call ESMF_GridCompSetEntryPoint(gc, ESMF_METHOD_INITIALIZE, userRoutine = create_distGrid, phase=2, rc = status)
      _VERIFY(status)
      call ESMF_GridCompSetEntryPoint(gc, ESMF_METHOD_RUN, userRoutine = run_gc, rc = status)
      _VERIFY(status)
      call ESMF_GridCompSetEntryPoint(gc, ESMF_METHOD_FINALIZE, userRoutine = finalize_gc, rc = status)
      _VERIFY(status)
      _RETURN(ESMF_SUCCESS)
   end subroutine

  subroutine create_field(gc, import_state, export_state, clock, rc)
    type(ESMF_GridComp) :: gc
    type(ESMF_State) :: import_state, export_state
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc
    integer :: status
    type(ESMF_Field) :: field
    field=ESMF_FieldEmptyCreate(name="empty",__RC__)
    call ESMF_StateAdd(import_state,[field],__RC__)
    _RETURN(_SUCCESS)
  end subroutine 

  subroutine create_distGrid(gc, import_state, export_state, clock, rc)
    type(ESMF_GridComp) :: gc
    type(ESMF_State) :: import_state, export_state
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc
    integer :: status
    type(ESMF_Field) :: field
    type(ESMF_DistGrid) :: dg1,dg2
    type(ESMF_Grid) :: grid,new_grid
    type(ESMF_VM) :: vm
   
    call ESMF_VMGetCurrent(vm,__RC__)
    call ESMF_StateGet(import_state,"empty",field,__RC__)
    call ESMF_FieldGet(field,grid=grid,__RC__)
    call ESMF_GridGet(grid,distGrid=dg1,__RC__)
    dg2 = ESMF_DistGridCreate(dg1,balanceFlag=.true.,__RC__)
 
    new_grid = ESMF_GridEmptyCreate(vm=vm,__RC__)
    call ESMF_GridSet(new_grid,distGrid=dg2,vm=vm,__RC__)
    call ESMF_FieldEmptySet(field,grid=new_grid,vm=vm,__RC__)
    _RETURN(_SUCCESS)
  end subroutine 

  subroutine run_gc(gc, import_state, export_state, clock, rc)
    type(ESMF_GridComp) :: gc
    type(ESMF_State) :: import_state, export_state
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    _RETURN(_SUCCESS)
  end subroutine 

  subroutine finalize_gc(gc, import_state, export_state, clock, rc)
    type(ESMF_GridComp) :: gc
    type(ESMF_State) :: import_state, export_state
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    _RETURN(_SUCCESS)
  end subroutine 

end module AEIO_TransferGridComp
