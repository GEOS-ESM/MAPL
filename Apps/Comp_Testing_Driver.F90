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

  contains
    
    subroutine main()
      integer :: status, rc, local_PET, n_PET
      type(ESMF_VM) :: vm
      character(len=ESMF_MAXSTR) :: filename, comp_name
      type(ESMF_Config) :: config
      class(BaseProfiler), pointer :: t_p

      ! initialize
      call ESMF_Initialize(logKindFlag=ESMF_LOGKIND_NONE, vm=vm, _RC)
      call ESMF_VMGet(vm, localPET=local_PET, petCount=n_PET, _RC)
      call MAPL_Initialize(_RC)
      call ESMF_CalendarSetDefault(ESMF_CALKIND_GREGORIAN, _RC)
      t_p => get_global_time_profiler()
      call t_p%start('Comp_Testing_Driver.x')

      ! get rc filename and component to run
      call get_command_argument(1, filename)
      config = ESMF_ConfigCreate(_RC)
      call ESMF_ConfigLoadFile(config, filename, _RC)
      call ESMF_ConfigGetAttribute(config, value=comp_name, label="COMPONENT_TO_RECORD:", _RC)
      
      call driver_component(filename, comp_name, _RC)

      ! finalize
      call t_p%stop('Comp_Testing_Driver.x')
      call MAPL_Finalize(_RC)
      call ESMF_Finalize (_RC)
  end subroutine main

  subroutine driver_component(filename, comp_name, rc)
    character(len=*), intent(in) :: filename, comp_name
    integer, intent(out) :: rc
    integer :: status, root_id, user_RC, RUN_DT, i, j, nc_id, var_id
    integer :: NX, NY, item_count, field_count, phase
    character(len=ESMF_MAXSTR) :: shared_obj, export_checkpoint, restart_file
    type(ESMF_Clock) :: clock
    type(ESMF_TimeInterval) :: time_interval
    type(ESMF_GridComp) :: temp_GC, GC
    type(ESMF_State) :: import, export
    type(ESMF_Config) :: config
    type(ESMF_Time), allocatable :: start_time(:)
    type(ESMF_Grid) :: grid
    type(ESMF_Field) :: field
    type(MAPL_MetaComp), pointer :: mapl_obj
    real(kind=ESMF_KIND_R8), pointer :: lons_field_ptr(:,:), lats_field_ptr(:,:)
    type(NetCDF4_fileFormatter) :: formatter
    type(FileMetadata) :: basic_metadata
    type(FileMetadataUtils) :: metadata
    logical :: subset
    character(len=ESMF_MAXSTR), allocatable :: item_name_list(:), field_name_list(:)
    type(ESMF_StateItem_Flag):: item_type
    type(ESMF_FieldBundle) :: field_bundle
    type(ESMF_Field), allocatable :: field_list(:)
    
    config = ESMF_ConfigCreate(_RC)
    call ESMF_ConfigLoadFile(config, filename, _RC)
    call ESMF_ConfigGetAttribute(config, value=RUN_DT, label="RUN_DT:", _RC)
    call ESMF_ConfigGetAttribute(config, value=restart_file, label="RESTART_FILE:", _RC)
    call ESMF_ConfigGetAttribute(config, label="EXPORT_CHECKPOINT:", value=export_checkpoint, _RC)
    call ESMF_ConfigGetAttribute(config, label = "LIBRARY_FILE:", value=shared_obj, _RC)
    call ESMF_ConfigGetAttribute(config, value=phase, label="PHASE:", default=1, _RC)
    
    ! Create a clock, set current time to required time consistent with checkpoints used 
    call formatter%open(restart_file, pFIO_Read, _RC)
    call ESMF_TimeIntervalSet(time_interval, s=RUN_DT, _RC)
    basic_metadata=formatter%read(_RC)
    call metadata%create(basic_metadata,trim(restart_file))
    call metadata%get_time_info(timeVector=start_time,_RC)
    clock = ESMF_ClockCreate(time_interval, start_time(1), _RC)
    call formatter%close(_RC)

    ! create MAPL_MetaComp object, add child
    grid=grid_manager%make_grid(config, _RC)

    temp_GC = ESMF_GridCompCreate(name=comp_name, _RC)
    mapl_obj => null()
    call MAPL_InternalStateCreate(temp_GC, mapl_obj, _RC)
    call MAPL_InternalStateRetrieve(temp_GC, mapl_obj, _RC)
    call MAPL_Set(mapl_obj, CF=config, _RC)

    root_id = MAPL_AddChild(mapl_obj, grid=grid, name=comp_name, userRoutine="setservices_", sharedObj=shared_obj, _RC) 
    
    GC = mapl_obj%get_child_gridcomp(root_id)
    import = mapl_obj%get_child_import_state(root_id)
    export = mapl_obj%get_child_export_state(root_id)

    ! if subsetting, get appropriate lons and lats
    call ESMF_ConfigGetAttribute(config, value=subset, label="SUBSET:", default=.false., _RC)
    call ESMF_ConfigGetAttribute(config, value=NX, label = "NX:", _RC)
    call ESMF_ConfigGetAttribute(config, value=NY, label = "NX:", _RC)
    if (subset .and. NX*NY == 1) then
       call formatter%open(restart_file, pFIO_Read, _RC)
       call ESMF_GridGetCoord(grid, coordDim=1, farrayPtr=lons_field_ptr, _RC)
       call ESMF_GridGetCoord(grid, coordDim=2, farrayPtr=lats_field_ptr, _RC)
       call formatter%get_var("lons", lons_field_ptr)
       call formatter%get_var("lats", lats_field_ptr)
       call ESMF_GridCompSet(GC, grid=grid, _RC)
       call formatter%close(_RC)
    else
       call ESMF_GridCompSet(GC, grid=grid, _RC)
    end if
 

    call ESMF_GridCompInitialize(GC, importState=import, exportState=export, clock=clock, userRC=user_RC, _RC) 

    ! allocate fields from export before file
    status = nf90_open(export_checkpoint, nf90_nowrite, nc_id)
    _VERIFY(status)

    call ESMF_StateGet(export, itemCount = item_count, _RC)
    allocate(item_name_list(item_count))
    call ESMF_StateGet(export, itemNameList = item_name_list, _RC)
    do i = 1, item_count
       call ESMF_StateGet(export, itemName=item_name_list(i), itemType=item_type, _RC)
       if (item_type == ESMF_STATEITEM_FIELD) then
          call ESMF_StateGet(export, item_name_list(i), field, _RC)
          status = nf90_inq_varid(nc_id, item_name_list(i), var_id)
          if (status == 0) then
             call MAPL_AllocateCoupling(field, _RC)
          end if
       else if (item_type == ESMF_STATEITEM_FIELDBUNDLE) then
          call ESMF_StateGet(export, item_name_list(i), field_bundle, _RC)
          call ESMF_FieldBundleGet(field_bundle, fieldCount=field_count, _RC)
          allocate(field_list(field_count))
          allocate(field_name_list(field_count))
          call ESMF_FieldBundleGet(field_bundle, fieldList=field_list, fieldNameList=field_name_list, _RC)
          do j = 1, field_count
             status = nf90_inq_varid(nc_id, field_name_list(j), var_id)
             if (status == 0) then
                call MAPL_AllocateCoupling(field_list(j), _RC)
             end if
          end do
       end if
    end do
    
    call ESMF_GridCompRun(GC, importState=import, exportState=export, clock=clock, phase=phase, userRC=user_RC, _RC)

    call ESMF_GridCompFinalize(GC, importState=import, exportState=export, clock=clock, userRC=user_RC, _RC) 

    _RETURN(_SUCCESS)
  end subroutine driver_component

end program comp_testing_driver
