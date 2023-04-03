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
      integer :: status, rc, myPET, nPET
      type(ESMF_VM) :: vm
      character(len=ESMF_MAXSTR) :: filename, compName
      type(ESMF_Config) :: config
      class(BaseProfiler), pointer :: t_p

      ! initialize
      call ESMF_Initialize(logKindFlag=ESMF_LOGKIND_NONE, vm=vm, _RC)
      call ESMF_VMGet(vm, localPET=myPET, petCount=nPET, _RC)
      call MAPL_Initialize(_RC)
      call ESMF_CalendarSetDefault(ESMF_CALKIND_GREGORIAN, _RC)
      t_p => get_global_time_profiler()
      call t_p%start('Comp_Testing_Driver.x')

      ! get rc filename and component to run
      call get_command_argument(1, filename)
      config = ESMF_ConfigCreate(_RC)
      call ESMF_ConfigLoadFile(config, filename, _RC)
      call ESMF_ConfigGetAttribute(config, value=compName, label="COMPONENT_TO_RECORD:", _RC)
      
      call driver_component(filename, compName, _RC)

      ! finalize
      call t_p%stop('Comp_Testing_Driver.x')
      call MAPL_Finalize(_RC)
      call ESMF_Finalize (_RC)
  end subroutine main

  subroutine driver_component(filename, compName, rc)
    character(len=*), intent(in) :: filename, compName
    integer, intent(out) :: rc
    integer :: status, root_id, userRC, RUN_DT, i, j, ncid, varid, NX, NY, itemCount, fieldCount, phase
    character(len=ESMF_MAXSTR) :: sharedObj, exportCheckpoint, restartFile
    type(ESMF_Clock) :: clock
    type(ESMF_TimeInterval) :: timeInterval
    type(ESMF_GridComp) :: temp_GC, GC
    type(ESMF_State) :: import, export
    type(ESMF_Config) :: config
    type(ESMF_Time), allocatable :: esmf_startTime(:)
    type(ESMF_Grid) :: grid
    type(ESMF_Field) :: field
    type(MAPL_MetaComp), pointer :: maplobj
    real(kind=ESMF_KIND_R8), pointer :: lons_field_ptr(:,:), lats_field_ptr(:,:)
    type(NetCDF4_fileFormatter) :: formatter
    type(FileMetadata) :: basic_metadata
    type(FileMetadataUtils) :: metadata
    logical :: subset
    character(len=ESMF_MAXSTR), allocatable :: itemNameList(:), fieldNameList(:)
    type(ESMF_StateItem_Flag):: itemType
    type(ESMF_FieldBundle) :: fieldBundle
    type(ESMF_Field), allocatable :: fieldList(:)
    
    config = ESMF_ConfigCreate(_RC)
    call ESMF_ConfigLoadFile(config, filename, _RC)
    call ESMF_ConfigGetAttribute(config, value=RUN_DT, label="RUN_DT:", _RC)
    call ESMF_ConfigGetAttribute(config, value=restartFile, label="RESTART_FILE:", _RC)
    call ESMF_ConfigGetAttribute(config, label="EXPORT_CHECKPOINT:", value=exportCheckpoint, _RC)
    call ESMF_ConfigGetAttribute(config, label = "LIBRARY_FILE:", value=sharedObj, _RC)
    call ESMF_ConfigGetAttribute(config, value=phase, label="PHASE:", default=1, _RC)
    
    ! Create a clock, set current time to required time consistent with checkpoints used 
    call formatter%open(restartFile, pFIO_Read, _RC)
    call ESMF_TimeIntervalSet(timeInterval, s=RUN_DT, _RC)
    basic_metadata=formatter%read(_RC)
    call metadata%create(basic_metadata,trim(restartFile))
    call metadata%get_time_info(timeVector=esmf_startTime,_RC)
    clock = ESMF_ClockCreate(timeInterval, esmf_startTime(1), _RC)
    call formatter%close(_RC)

    ! create MAPL_MetaComp object, add child
    grid=grid_manager%make_grid(config, _RC)

    temp_GC = ESMF_GridCompCreate(name=compName, _RC)
    maplobj => null()
    call MAPL_InternalStateCreate(temp_GC, maplobj, _RC)
    call MAPL_InternalStateRetrieve(temp_GC, maplobj, _RC)
    call MAPL_Set(maplobj, CF=config, _RC)

    root_id = MAPL_AddChild(maplobj, grid=grid, name=compName, userRoutine="setservices_", sharedObj=sharedObj, _RC) 
    
    GC = maplobj%get_child_gridcomp(root_id)
    import = maplobj%get_child_import_state(root_id)
    export = maplobj%get_child_export_state(root_id)

    ! if subsetting, get appropriate lons and lats
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
    else
       call ESMF_GridCompSet(GC, grid=grid, _RC)
    end if
 

    call ESMF_GridCompInitialize(GC, importState=import, exportState=export, clock=clock, userRC=userRC, _RC) 

    ! allocate fields from export before file
    status = nf90_open(exportCheckpoint, nf90_nowrite, ncid)
    _VERIFY(status)

    call ESMF_StateGet(export, itemCount = itemCount, _RC)
    allocate(itemNameList(itemCount))
    call ESMF_StateGet(export, itemNameList = itemNameList, _RC)
    do i = 1, itemCount
       call ESMF_StateGet(export, itemName=itemNameList(i), itemType=itemType, _RC)
       if (itemType == ESMF_STATEITEM_FIELD) then
          call ESMF_StateGet(export, itemNameList(i), field, _RC)
          status = nf90_inq_varid(ncid, itemNameList(i), varid)
          if (status == 0) then
             call MAPL_AllocateCoupling(field, _RC)
          end if
       else if (itemType == ESMF_STATEITEM_FIELDBUNDLE) then
          call ESMF_StateGet(export, itemNameList(i), fieldBundle, _RC)
          call ESMF_FieldBundleGet(fieldBundle, fieldCount=fieldCount, _RC)
          allocate(fieldList(fieldCount))
          allocate(fieldNameList(fieldCount))
          call ESMF_FieldBundleGet(fieldBundle, fieldList=fieldList, fieldNameList=fieldNameList, _RC)
          do j = 1, fieldCount
             status = nf90_inq_varid(ncid, fieldNameList(j), varid)
             if (status == 0) then
                call MAPL_AllocateCoupling(fieldList(j), _RC)
             end if
          end do
       end if
    end do
    
    call ESMF_GridCompRun(GC, importState=import, exportState=export, clock=clock, phase=phase, userRC=userRC, _RC)

    call ESMF_GridCompFinalize(GC, importState=import, exportState=export, clock=clock, userRC=userRC, _RC) 

    _RETURN(_SUCCESS)
  end subroutine driver_component

end program comp_testing_driver
