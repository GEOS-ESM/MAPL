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
      call ESMF_ConfigFindLabel(config, label="COMPONENT_TO_RECORD:", _RC)
      
      call ESMF_ConfigGetAttribute(config, value=compName, _RC)
      !compStatus = 0
      !do while (compStatus == 0)
         call driver_component(filename, compName, _RC)
         !call ESMF_ConfigGetAttribute(config, value=compName, rc=compStatus)
      !end do

      ! finalize
      call t_p%stop('Comp_Testing_Driver.x')
      call MAPL_Finalize(_RC)
      call ESMF_Finalize (_RC)
  end subroutine main

  subroutine driver_component(filename, compName, rc)
    character(len=*), intent(in) :: filename, compName
    integer, intent(out) :: rc
    integer :: status, root_id, userRC, RUN_DT, i, ncid, varid, tsteps, NX, NY
    character(len=ESMF_MAXSTR) :: time, startTime, sharedObj, exportCheckpoint, variable, restartFile
    type(ESMF_Clock) :: clock
    type(ESMF_TimeInterval) :: timeInterval
    type(ESMF_GridComp) :: temp_GC, GC
    type(ESMF_State) :: import, export
    type(ESMF_Config) :: config
    type(ESMF_Time), allocatable :: esmf_startTime(:)
    type(ESMF_Grid) :: grid
    type(MAPL_MetaComp), pointer :: maplobj
    type(ESMF_Field) :: field, lons_field, lats_field
    real(kind=ESMF_KIND_R8), pointer :: lons_field_ptr(:,:), lats_field_ptr(:,:), grid_lons(:,:), grid_lats(:,:)
    type(NetCDF4_fileFormatter) :: formatter
    type(FileMetadata) :: basic_metadata
    type(FileMetadataUtils) :: metadata
    logical :: subset
   
    config = ESMF_ConfigCreate(_RC)
    call ESMF_ConfigLoadFile(config, filename, _RC)
    call ESMF_ConfigGetAttribute(config, value=RUN_DT, label="RUN_DT:", _RC)
    call ESMF_ConfigGetAttribute(config, value=restartFile, label="RESTART_FILE:", _RC)
    
    ! Create a clock, set current time to required time consistent with checkpoints used 
    call formatter%open(restartFile, pFIO_Read, _RC)
    call ESMF_TimeIntervalSet(timeInterval, s=RUN_DT, _RC)
    basic_metadata=formatter%read(_RC)
    call metadata%create(basic_metadata,trim(restartFile))
    call metadata%get_time_info(timeVector=esmf_startTime,_RC)
    clock = ESMF_ClockCreate(timeInterval, esmf_startTime(1), _RC)
    call formatter%close(_RC)
    
    grid=grid_manager%make_grid(config, _RC)

    temp_GC = ESMF_GridCompCreate(name=compName, _RC)
    maplobj => null()
    call MAPL_InternalStateCreate(temp_GC, maplobj, _RC)
    call MAPL_InternalStateRetrieve(temp_GC, maplobj, _RC)
    call MAPL_Set(maplobj, CF=config, _RC)

    call ESMF_ConfigFindLabel(config, label="LIBRARY_FILE:", _RC)
    call ESMF_ConfigGetAttribute(config, value=sharedObj, _RC)
    !attrStatus = 0
    !do while (attrStatus == 0)
    !   call ESMF_ConfigGetAttribute(config, value=sharedObj, rc=attrStatus)
    !end do

    root_id = MAPL_AddChild(maplobj, name=compName, userRoutine="setservices_", sharedObj=sharedObj, _RC) 
    
    GC = maplobj%get_child_gridcomp(root_id)
    import = maplobj%get_child_import_state(root_id)
    export = maplobj%get_child_export_state(root_id)

    call ESMF_ConfigGetAttribute(config, value=subset, label="SUBSET:", default=.false., _RC)
    call ESMF_ConfigGetAttribute(config, value=NX, label = "NX:", _RC)
    call ESMF_ConfigGetAttribute(config, value=NY, label = "NX:", _RC)
    if (subset .and. NX*NY == 1) then
       call formatter%open(restartFile, pFIO_Read, _RC)
       call ESMF_GridGetCoord(grid, coordDim=1, farrayPtr=lons_field_ptr, _RC)
       call ESMF_GridGetCoord(grid, coordDim=2, farrayPtr=lats_field_ptr, _RC)
       call formatter%get_var("lons", lons_field_ptr)
       call formatter%get_var("lats", lats_field_ptr)
       call ESMF_GridCompSet(GC, grid=grid, _RC)
       call formatter%close(_RC)
    end if
    
    call ESMF_GridCompInitialize(GC, importState=import, exportState=export, clock=clock, userRC=userRC, _RC) 

    call ESMF_ConfigFindLabel(config, label="EXPORT_CHECKPOINT:", _RC)
    call ESMF_ConfigGetAttribute(config, value=exportCheckpoint, _RC)

    !attrStatus = 0
    !do while (attrStatus == 0)
    !   call ESMF_ConfigGetAttribute(config, value=exportCheckpoint, rc=attrStatus)
    !end do

    status = nf90_open(exportCheckpoint, nf90_nowrite, ncid)
    _VERIFY(status)
    status = nf90_inquire_variable(ncid, 1, variable)
    _VERIFY(status)

    i = 2
    do while (status == 0)
       if (trim(variable) == "lon" .or. trim(variable) == "lat" .or. trim(variable) == "lev" .or. trim(variable) == "time") then
          status = nf90_inquire_variable(ncid, i, variable)
          i = i + 1
          cycle
       end if

       call ESMF_StateGet(export, variable, field, _RC)
       call MAPL_AllocateCoupling(field, _RC)
       status = nf90_inquire_variable(ncid, i, variable)
       i = i + 1
    end do

    call ESMF_GridCompRun(GC, importState=import, exportState=export, clock=clock, userRC=userRC, _RC)
    call ESMF_GridCompFinalize(GC, importState=import, exportState=export, clock=clock, userRC=userRC, _RC) 

    _RETURN(_SUCCESS)
  end subroutine driver_component

end program comp_testing_driver
