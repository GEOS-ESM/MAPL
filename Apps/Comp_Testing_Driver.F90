#include "MAPL_Generic.h"

program comp_testing_driver
  use ESMF
  use NetCDF
  use ESMFL_Mod
  use MAPL
  use MPI
  use MAPL_GenericMod
  use MAPL_BaseMod
  use MAPL_CapGridCompMod
  use MAPL_TimeDataMod
  use MAPL_GridManagerMod

  implicit none

  call main()

  CONTAINS
    
    subroutine main()
      integer :: status, compStatus, rc, myPET, nPET
      type(ESMF_VM) :: vm
      character(len=ESMF_MAXSTR) :: filename, compName
      type(ESMF_Config) :: config
      class(BaseProfiler), pointer :: t_p

      ! initialize
      call ESMF_Initialize(logKindFlag=ESMF_LOGKIND_MULTI, vm=vm, _RC)
      call ESMF_VMGet(vm, localPET=myPET, petCount=nPET, _RC)
      call MAPL_Initialize(_RC)
      call ESMF_CalendarSetDefault(ESMF_CALKIND_GREGORIAN, _RC) ! need in order to set time
      t_p => get_global_time_profiler()
      call t_p%start('Comp_Testing_Driver.x')

      ! get rc filename and components to run
      call get_command_argument(1, filename)
      config = ESMF_ConfigCreate(_RC)
      call ESMF_ConfigLoadFile(config, filename, _RC)
      call ESMF_ConfigFindLabel(config, label="COMPONENT_TO_RUN:", _RC)
      
      call ESMF_ConfigGetAttribute(config, value=compName, _RC)
      compStatus = 0
      do while (compStatus == 0)
         call driver_component(filename, compName, _RC)
         call ESMF_ConfigGetAttribute(config, value=compName, rc=compStatus)
      end do

      ! finalize
      call t_p%stop('Comp_Testing_Driver.x')
      call MAPL_Finalize(_RC)
      call ESMF_Finalize (_RC)
  end subroutine main

  subroutine driver_component(filename, compName, rc)
    character(len=*), intent(in) :: filename, compName
    integer, intent(out) :: rc
    integer :: status, root_id, userRC, RUN_DT, i, ncid
    character(len=ESMF_MAXSTR) :: time, startTime, sharedObj, exportCheckpoint, variable
    type(ESMF_Clock) :: clock
    type(ESMF_TimeInterval) :: timeInterval
    type(ESMF_GridComp) :: temp_GC, GC
    type(ESMF_State) :: import, export
    type(ESMF_Config) :: config
    type(ESMF_Time) :: esmf_startTime
    type(ESMF_Grid) :: grid
    type (MAPL_MetaComp), pointer :: maplobj
    type(ESMF_Field) :: field
   
    config = ESMF_ConfigCreate(_RC)
    call ESMF_ConfigLoadFile(config, filename, _RC)

    ! any additional attributes
    call ESMF_ConfigGetAttribute(config, value=startTime, label="START_TIME:", _RC)
    call ESMF_ConfigGetAttribute(config, value=time, label="TIME:", _RC)
    call ESMF_ConfigGetAttribute(config, value=RUN_DT, label="RUN_DT:", _RC)
    
    ! Create a clock, set current time to required time consistent with checkpoints used 
    call ESMF_TimeIntervalSet(timeInterval, s=RUN_DT, _RC)
    startTime = trim(startTime)
    esmf_startTime = parse_time_string(startTime, _RC)
    clock = ESMF_ClockCreate(timeInterval, esmf_startTime, _RC)
    
    ! Create a grid (since the MAPL components do not do this other than GCM) we must do it here, 
    ! note for the vision of using a grid that is only a subset of columns we will have to see if we can 
    ! use an existing grid factory, or might require new factory
    grid=grid_manager%make_grid(config, _RC)

    ! Add component to be tested as the “child” via MAPL_AddChild
    temp_GC = ESMF_GridCompCreate(name=compName, _RC)
    maplobj => null()
    call MAPL_InternalStateCreate(temp_GC, maplobj, _RC)
    call MAPL_InternalStateRetrieve(temp_GC, maplobj, _RC)
    call MAPL_Set(maplobj, CF=config, _RC)

    ! get DSO from component name
    sharedObj = "libGEOS"//ESMF_UtilStringLowerCase(trim(compName))//"_GridComp.so"
    root_id = MAPL_AddChild(maplobj, grid=grid, name=compName, userRoutine="setservices_", sharedObj=sharedObj, _RC) 
    
    ! Set grid in child
    GC = maplobj%get_child_gridcomp(root_id)
    import = maplobj%get_child_import_state(root_id)
    export = maplobj%get_child_export_state(root_id)
    call ESMF_GridCompSet(GC, grid=grid, _RC)
    
    ! Will probably have to do something to force the right exports to get allocated when we run 
    ! genericinitialize, one idea is to use the checkpoint itself, examine, what variables are in
    ! the checkpoint for the export and ensure those variables are allocated in the genericinitialize, somehow...

    exportCheckpoint = ESMF_UtilStringLowerCase(trim(compName))//"_export_checkpoint"
    status = nf90_open(exportCheckpoint, nf90_nowrite, ncid)
    _VERIFY(status)
    status = nf90_inquire_variable(ncid, 1, variable)
    _VERIFY(status)

    i = 2
    do while (status == 0)
       ! field = ESMF_FieldEmptyCreate(name=variable, _RC) ! attributes don't exist with empty field that MAPL_AllocateCoupling needs
       !call ESMF_StateGet(export, variable, field, _RC)   ! field doesn't exist in child export state
       ! perhaps:
       ! 1. get data values of lat, lon from checkpoint 
       ! 2. make a grid based on those coords
       ! 3. create a field based on that grid
       ! but would it still be missing attributes?
       print*, i
       call MAPL_AllocateCoupling(field, _RC)
       status = nf90_inquire_variable(ncid, i, variable)
       i = i + 1
    end do
        
    !call ESMF_GridCompInitialize(GC, importState=import, exportState=export, clock=clock, userRC=userRC, _RC) 
    !call ESMF_GridCompRun(GC, importState=import, exportState=export, clock=clock, userRC=userRC, _RC)
    !call ESMF_GridCompFinalize(GC, importState=import, exportState=export, clock=clock, userRC=userRC, _RC) 

    _RETURN(_SUCCESS)
  end subroutine driver_component

end program comp_testing_driver
