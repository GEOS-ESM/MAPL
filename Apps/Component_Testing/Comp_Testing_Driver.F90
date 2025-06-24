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
      character(len=ESMF_MAXSTR) :: filename
      type(ESMF_Config) :: config
      class(BaseProfiler), pointer :: t_p

      ! initialize
      call ESMF_Initialize(logKindFlag=ESMF_LOGKIND_NONE, vm=vm, _rc)
      call ESMF_VMGet(vm, localPET=local_PET, petCount=n_PET, _rc)
      call MAPL_Initialize(_rc)
      call ESMF_CalendarSetDefault(ESMF_CALKIND_GREGORIAN, _rc)
      t_p => get_global_time_profiler()
      call t_p%start('Comp_Testing_Driver.x')

      ! get rc filename and component to run
      call get_command_argument(1, filename)
      config = ESMF_ConfigCreate(_rc)
      call ESMF_ConfigLoadFile(config, filename, _rc)
      call run_component_driver(filename, _rc)

      ! finalize
      call t_p%stop('Comp_Testing_Driver.x')
      call MAPL_Finalize(_rc)
      call ESMF_Finalize (_rc)
  end subroutine main

  subroutine run_component_driver(filename, rc)
    character(len=*), intent(in) :: filename
    integer, intent(out) :: rc
    integer :: status, root_id, user_RC, RUN_DT
    integer :: NX, NY, phase
    character(len=ESMF_MAXSTR) :: comp_name, shared_obj, restart_file
    type(ESMF_Clock) :: clock
    type(ESMF_TimeInterval) :: time_interval
    type(ESMF_GridComp) :: temp_GC, GC
    type(ESMF_State) :: import, export
    type(ESMF_Config) :: config
    type(ESMF_Time), allocatable :: start_time(:)
    type(ESMF_Grid) :: grid
    type(MAPL_MetaComp), pointer :: mapl_obj
    real(kind=ESMF_KIND_R8), pointer :: lons_field_ptr(:,:), lats_field_ptr(:,:)
    type(NetCDF4_fileFormatter) :: formatter
    type(FileMetadata) :: basic_metadata
    type(FileMetadataUtils) :: metadata
    logical :: subset

    ! get attributes from config file
    config = ESMF_ConfigCreate(_rc)
    call ESMF_ConfigLoadFile(config, filename, _rc)
    call get_config_attributes(config, comp_name, RUN_DT, restart_file, shared_obj, phase, subset, NX, NY, _rc)

    ! create a clock, set current time to required time consistent with checkpoints used
    call formatter%open(restart_file, pFIO_Read, _rc)
    call ESMF_TimeIntervalSet(time_interval, s=RUN_DT, _rc)
    basic_metadata=formatter%read(_rc)
    call metadata%create(basic_metadata,trim(restart_file))
    call metadata%get_time_info(timeVector=start_time,_rc)
    clock = ESMF_ClockCreate(time_interval, start_time(1), _rc)
    call formatter%close(_rc)

    ! create MAPL_MetaComp object, add child
    grid=grid_manager%make_grid(config, _rc)

    temp_GC = ESMF_GridCompCreate(name=comp_name, _rc)
    mapl_obj => null()
    call MAPL_InternalStateCreate(temp_GC, mapl_obj, _rc)
    call MAPL_InternalStateRetrieve(temp_GC, mapl_obj, _rc)
    call MAPL_Set(mapl_obj, CF=config, _rc)

    root_id = MAPL_AddChild(mapl_obj, grid=grid, name=comp_name, userRoutine="setservices_", sharedObj=shared_obj, _rc)

    GC = mapl_obj%get_child_gridcomp(root_id)
    import = mapl_obj%get_child_import_state(root_id)
    export = mapl_obj%get_child_export_state(root_id)

    ! if subsetting, get appropriate lons and lats
    if (subset .and. NX*NY == 1) then
       call formatter%open(restart_file, pFIO_Read, _rc)
       call ESMF_GridGetCoord(grid, coordDim=1, farrayPtr=lons_field_ptr, _rc)
       call ESMF_GridGetCoord(grid, coordDim=2, farrayPtr=lats_field_ptr, _rc)
       call formatter%get_var("lons", lons_field_ptr)
       call formatter%get_var("lats", lats_field_ptr)
       call ESMF_GridCompSet(GC, grid=grid, _rc)
       call formatter%close(_rc)
    else
       call ESMF_GridCompSet(GC, grid=grid, _rc)
    end if


    call ESMF_GridCompInitialize(GC, importState=import, exportState=export, clock=clock, userRC=user_RC, _rc)

    call ESMF_GridCompRun(GC, importState=import, exportState=export, clock=clock, phase=phase, userRC=user_RC, _rc)

    call ESMF_GridCompFinalize(GC, importState=import, exportState=export, clock=clock, userRC=user_RC, _rc)

    _return(_success)
  end subroutine run_component_driver

  subroutine get_config_attributes(config, comp_name, RUN_DT, restart_file, shared_obj, phase, subset, NX, NY, rc)
    type(ESMF_Config), intent(inout) :: config
    character(len=ESMF_MAXSTR), intent(inout) :: comp_name, shared_obj, restart_file
    integer, intent(inout) :: NX, NY, phase, RUN_DT
    logical, intent(inout) :: subset
    integer, intent(out) :: rc
    integer :: status

    call ESMF_ConfigGetAttribute(config, value=comp_name, label="COMPONENT_TO_RECORD:", _rc)
    call ESMF_ConfigGetAttribute(config, value=RUN_DT, label="RUN_DT:", _rc)
    call ESMF_ConfigGetAttribute(config, value=restart_file, label="RESTART_FILE:", _rc)
    call ESMF_ConfigGetAttribute(config, value=shared_obj, label = "LIBRARY_FILE:", _rc)
    call ESMF_ConfigGetAttribute(config, value=phase, label="PHASE:", default=1, _rc)
    call ESMF_ConfigGetAttribute(config, value=subset, label="SUBSET:", default=.false., _rc)
    call ESMF_ConfigGetAttribute(config, value=NX, label = "NX:", _rc)
    call ESMF_ConfigGetAttribute(config, value=NY, label = "NX:", _rc)

    _return(_success)
  end subroutine get_config_attributes

end program comp_testing_driver
