#include "MAPL_Generic.h"
#include "unused_dummy.H"

module MAPL_CapGridCompMod
  use ESMF
  use MAPL_ExceptionHandling
  use MAPL_BaseMod
  use MAPL_Constants
  use MAPL_Profiler, only: BaseProfiler, get_global_time_profiler, get_global_memory_profiler
  use MAPL_ProfMod
  use MAPL_MemUtilsMod
  use MAPL_IOMod
  use MAPL_CommsMod
  use MAPL_GenericMod
  use MAPL_LocStreamMod
  use ESMFL_Mod
  use MAPL_ShmemMod
  use MAPL_HistoryGridCompMod, only : Hist_SetServices => SetServices
  use MAPL_HistoryGridCompMod, only : HISTORY_ExchangeListWrap
#if defined(BUILD_WITH_EXTDATA2G)
  use MAPL_ExtDataGridComp2G, only : ExtData2G_SetServices => SetServices
#endif
  use MAPL_ExtDataGridCompMod, only : ExtData1G_SetServices => SetServices
  use MAPL_ConfigMod
  use MAPL_DirPathMod
  use MAPL_KeywordEnforcerMod
  use MAPL_ExternalGridFactoryMod
  use MAPL_GridManagerMod
  use pFIO
  use gFTL_StringVector
  use pflogger, only: logging, Logger
  use MAPL_TimeUtilsMod, only: is_valid_time, is_valid_date
  use MAPL_ExternalGCStorage
#ifdef BUILD_WITH_PFLOGGER
  use mapl_SimulationTime, only: set_reference_clock
#endif
  use mpi

  use iso_fortran_env

  implicit none
  private

  character(*), parameter :: internal_cap_name = "InternalCapGridComp"

  public :: MAPL_CapGridComp, MAPL_CapGridCompCreate, MAPL_CapGridComp_Wrapper

  type :: ThroughputTimers
     real(kind=real64) :: loop_start_timer
     real(kind=REAL64) :: start_run_timer
     real(kind=REAL64) :: start_timer
  end type

  type :: MAPL_CapGridComp
     private
     type (ESMF_GridComp)          :: gc
     procedure(), pointer, nopass  :: root_set_services => null()
     character(len=:), allocatable :: root_dso
     character(len=:), allocatable :: final_file, name, cap_rc_file
     character(len=:), allocatable :: root_name
     integer :: nsteps, heartbeat_dt, perpetual_year, perpetual_month, perpetual_day
     logical :: amiroot, started_loop_timer
     logical :: lperp = .false.
     integer :: extdata_id, history_id, root_id, printspec
     type(ESMF_Clock) :: clock, clock_hist
     type(ESMF_Config) :: cf_ext, cf_root, cf_hist, config
     type(ESMF_GridComp), allocatable :: gcs(:)
     type(ESMF_State), public :: import_state, export_state
     type(ESMF_State), allocatable :: child_imports(:), child_exports(:)
     type(ESMF_VM) :: vm
     type(ESMF_Time) :: cap_restart_time
     type(ESMF_Alarm), allocatable :: alarm_list(:)
     type(ESMF_Time),  allocatable :: AlarmRingTime(:)
     logical,          allocatable :: ringingState(:)
     logical :: compute_throughput
     integer :: n_run_phases
     type (ThroughputTimers) :: starts
     integer :: step_counter
   contains
     procedure :: set_services
     procedure :: initialize
     procedure :: initialize_extdata
     procedure :: initialize_history
     procedure :: run
     procedure :: step
     procedure :: finalize
     procedure :: get_model_duration
     procedure :: get_am_i_root
     procedure :: get_heartbeat_dt
     procedure :: get_current_time
     procedure :: rewind_clock
     procedure :: record_state
     procedure :: refresh_state
     procedure :: destroy_state
     procedure :: get_field_from_import
     procedure :: get_field_from_internal
     procedure :: set_grid
     procedure :: inject_external_grid
     procedure :: set_clock
     procedure :: set_step_counter
     procedure :: increment_step_counter
     procedure :: get_step_counter
  end type MAPL_CapGridComp

  type :: MAPL_CapGridComp_Wrapper
     type(MAPL_CapGridComp), pointer :: ptr => null()
  end type MAPL_CapGridComp_Wrapper

  character(len=*), parameter :: Iam = __FILE__

contains


   subroutine MAPL_CapGridCompCreate(cap, cap_rc, name, final_file, unusable, n_run_phases, root_set_services, root_dso,  rc)
      use mapl_StubComponent
    type(MAPL_CapGridComp), intent(out), target :: cap
    character(*), intent(in) :: cap_rc, name
    character(len=*), optional, intent(in) :: final_file
    class(KeywordEnforcer), optional, intent(in) :: unusable
    procedure(), optional :: root_set_services
    character(len=*), optional, intent(in) :: root_dso
    integer, optional, intent(in)  :: n_run_phases
    integer, optional, intent(out) :: rc

    type(MAPL_CapGridComp_Wrapper) :: cap_wrapper
    type(MAPL_MetaComp), pointer :: meta => null()
    integer :: status
    character(*), parameter :: cap_name = "CAP"
    type(StubComponent) :: stub_component

    _UNUSED_DUMMY(unusable)

    cap%cap_rc_file = cap_rc
    if (present(root_set_services)) cap%root_set_services => root_set_services
    if (present(root_dso)) cap%root_dso = root_dso
    if (present(root_dso) .and. present(root_set_services)) then
       _FAIL("can only specify a setservice pointer or a dso to use")
    end if
    if (present(final_file)) then
       allocate(cap%final_file, source=final_file)
    end if
    cap%n_run_phases = 1
    if (present(n_run_phases)) cap%n_run_phases = n_run_phases

    cap%config = ESMF_ConfigCreate(_RC)
    call ESMF_ConfigLoadFile(cap%config, cap%cap_rc_file,_RC)

    allocate(cap%name, source=name)
    cap%gc = ESMF_GridCompCreate(name=cap_name, config=cap%config, _RC)

    meta => null()
    call MAPL_InternalStateCreate(cap%gc, meta, _RC)
    call MAPL_Set(meta, CF=cap%config, _RC)


    call MAPL_Set(meta, name=cap_name, component=stub_component, _RC)

    cap_wrapper%ptr => cap
    call ESMF_UserCompSetInternalState(cap%gc, internal_cap_name, cap_wrapper, status)
    _VERIFY(status)


    _RETURN(_SUCCESS)

  end subroutine MAPL_CapGridCompCreate


  subroutine initialize_gc(gc, import_state, export_state, clock, rc)
    type(ESMF_GridComp) :: gc
    type(ESMF_State) :: import_state, export_state
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    type(ESMF_GridComp)                   :: GCMGC
    type (ESMF_VM)                        :: gcmVM
    integer :: comm
    integer                      :: N,NSTEPS
    integer                      :: NPES

    integer :: corespernode

    logical :: amIRoot_
    character(len=ESMF_MAXSTR)   :: enableTimers
    character(len=ESMF_MAXSTR)   :: enableMemUtils
    integer                      :: MemUtilsMode
    integer                               :: useShmem

    integer :: status

    type (t_extdata_state), pointer       :: ExtData_internal_state => null()
    type (extdata_wrap)                   :: wrap


    character(len=ESMF_MAXSTR )  :: timerModeStr
    type(ESMF_TimeInterval)      :: Frequency
    character(len=ESMF_MAXSTR)   :: ROOT_NAME

    ! Misc locals
    !------------
    character(len=ESMF_MAXSTR)   :: EXPID
    character(len=ESMF_MAXSTR)   :: EXPDSC

    integer                      :: RUN_DT
    integer                               :: snglcol
    character(len=ESMF_MAXSTR)            :: replayMode
    integer :: nx
    integer :: ny

    integer                      :: HEARTBEAT_DT
    type(ESMF_Alarm)             :: PERPETUAL
    character(len=ESMF_MAXSTR)   :: clockname
    character(len=ESMF_MAXSTR)   :: HIST_CF, ROOT_CF, EXTDATA_CF
    character(len=ESMF_MAXSTR )           :: DYCORE
    character(len=ESMF_MAXPATHLEN) :: user_dirpath,tempString
    logical                      :: tend,foundPath
    logical                      :: cap_clock_is_present


    type (MAPL_MetaComp), pointer :: maplobj, root_obj
    character(len=ESMF_MAXSTR)         :: sharedObj
    type (ESMF_GridComp), pointer :: root_gc
    procedure(), pointer :: root_set_services
    type(MAPL_CapGridComp), pointer :: cap
    class(BaseProfiler), pointer :: t_p
    class(Logger), pointer :: lgr
    type(ESMF_Clock) :: cap_clock
    logical :: use_extdata2g

    _UNUSED_DUMMY(import_state)
    _UNUSED_DUMMY(export_state)
    _UNUSED_DUMMY(clock)

    cap => get_CapGridComp_from_gc(gc, _RC)
    call MAPL_InternalStateRetrieve(gc, maplobj, _RC)

    t_p => get_global_time_profiler()

    call ESMF_GridCompGet(gc, vm = cap%vm, _RC)
    call ESMF_VMGet(cap%vm, petcount = NPES, mpiCommunicator = comm, _RC)

    AmIRoot_ = MAPL_Am_I_Root(cap%vm)

    call MAPL_GetNodeInfo(comm = comm, _RC)

    AmIRoot_ = MAPL_Am_I_Root(cap%vm)

    cap%AmIRoot = AmIRoot_

    !  CAP's MAPL MetaComp
    !---------------------

    ! Note the call to GetLogger must be _after_ the call to MAPL_Set().
    ! That call establishes the name of this component which is used in
    ! retrieving this component's logger.
    call MAPL_GetLogger(gc, lgr, _RC)

    ! Check if user wants to use node shared memory (default is no)
    !--------------------------------------------------------------
    call MAPL_GetResource(MAPLOBJ, useShmem,  label = 'USE_SHMEM:',  default = 0, rc = status)
    if (useShmem /= 0) then
       call MAPL_InitializeShmem (_RC)
    end if

    ! Check if a valid clock was provided externally
    !-----------------------------------------------

    call ESMF_GridCompGet(gc, clockIsPresent=cap_clock_is_present, _RC)

    if (cap_clock_is_present) then
        call ESMF_GridCompGet(gc, clock=cap_clock, _RC)
        call ESMF_ClockValidate(cap_clock, _RC)
        cap%clock = ESMF_ClockCreate(cap_clock, _RC)
        ! NOTE: We assume the MAPL components will only advance by
        ! one time step when driven with an external clock.
        !---------------------------------------------------------
        cap%nsteps = 1
        cap%compute_throughput = .false.
    else
    !  Create Clock. This is a private routine that sets the start and
    !   end times and the time interval of the clock from the configuration.
    !   The start time is temporarily set to 1 interval before the time in the
    !   configuration. Once the Alarms are set in intialize, the clock will
    !   be advanced to guarantee it and its alarms are in the same state as they
    !   were after the last advance before the previous Finalize.
    !---------------------------------------------------------------------------

        call MAPL_ClockInit(MAPLOBJ, cap%clock, nsteps, _RC)
        cap%nsteps = nsteps
        cap%compute_throughput = .true.
    end if

#ifdef BUILD_WITH_PFLOGGER
    call set_reference_clock(cap%clock)
#endif

    call ESMF_ClockGet(cap%clock,currTime=cap%cap_restart_time,_RC)

    cap%clock_hist = ESMF_ClockCreate(cap%clock, _RC)  ! Create copy for HISTORY

    CoresPerNode = MAPL_CoresPerNodeGet(comm,_RC)

    ! We check resource for CoresPerNode (no longer needed to be in CAP.rc)
    ! If it is set in the resource, we issue an warning if the
    ! value does not agree with the detected CoresPerNode

    call ESMF_ConfigGetAttribute(cap%config, value = n, Label = "CoresPerNode:", rc = status)
    if (status == ESMF_SUCCESS) then
       if (CoresPerNode /= n) then
          call lgr%warning("CoresPerNode set (%i0), but does NOT match detected value (%i0)", CoresPerNode, n)
       end if
    end if

    call ESMF_VMGet(cap%vm, petcount=npes, mpicommunicator=comm, _RC)
     _ASSERT(CoresPerNode <= npes, 'something impossible happened')

    if (cap_clock_is_present) then
       call ESMF_ClockGet(cap%clock, timeStep=frequency, _RC)
       call ESMF_TimeIntervalGet(frequency, s=heartbeat_dt, _RC)
    else
       call ESMF_ConfigGetAttribute(cap%config, value = heartbeat_dt, Label = "HEARTBEAT_DT:", _RC)
       call ESMF_TimeIntervalSet(frequency, s = heartbeat_dt, _RC)
    end if

    cap%heartbeat_dt = heartbeat_dt


    perpetual = ESMF_AlarmCreate(clock = cap%clock_hist, name = 'PERPETUAL', ringinterval = frequency, sticky = .false., _RC)
    call ESMF_AlarmRingerOff(perpetual, _RC)

    ! Set CLOCK for AGCM if not externally provided
    ! ---------------------------------------------

    if (.not.cap_clock_is_present) then
       call MAPL_GetResource(MAPLOBJ, cap%perpetual_year, label='PERPETUAL_YEAR:',  default = -999, _RC)
       call MAPL_GetResource(MAPLOBJ, cap%perpetual_month, label='PERPETUAL_MONTH:', default = -999, _RC)
       call MAPL_GetResource(MAPLOBJ, cap%perpetual_day, label='PERPETUAL_DAY:',   default = -999, _RC)

       cap%lperp = ((cap%perpetual_day /= -999) .or. (cap%perpetual_month /= -999) .or. (cap%perpetual_year  /= -999))

       if (cap%perpetual_day /= -999) then
          _ASSERT(cap%perpetual_month /= -999, 'Must specify a value for PERPETUAL_MONTH in cap.')
          _ASSERT(cap%perpetual_year  /= -999, 'Must specify a value for PERPETUAL_YEAR in cap.')
       endif

       if (cap%lperp) then
          if (cap%perpetual_year  /= -999) call lgr%info('Using Perpetual  Year: %i0', cap%perpetual_year)
          if (cap%perpetual_month /= -999) call lgr%info('Using Perpetual Month: %i0', cap%perpetual_month)
          if (cap%perpetual_day   /= -999) call lgr%info('Using Perpetual   Day: %i0', cap%perpetual_day)

          call ESMF_ClockGet(cap%clock, name = clockname, rc = status)
          clockname = trim(clockname) // '_PERPETUAL'
          call ESMF_Clockset(cap%clock, name = clockname, rc = status)

          call ESMF_ClockGet(cap%clock_hist, name = clockname, rc = status)
          clockname = trim(clockname) // '_PERPETUAL'
          call ESMF_Clockset(cap%clock_hist, name = clockname, rc = status)

          call Perpetual_Clock(cap, _RC)
       endif
    endif

    !  Get configurable info to create HIST
    !  and the ROOT of the computational hierarchy
    !---------------------------------------------

    !BOR

    ! !RESOURCE_ITEM: string :: Name of ROOT's config file
    call MAPL_GetResource(MAPLOBJ, ROOT_CF, "ROOT_CF:", default = "ROOT.rc", _RC)

    ! !RESOURCE_ITEM: string :: Name to assign to the ROOT component
    call MAPL_GetResource(MAPLOBJ, ROOT_NAME, "ROOT_NAME:", default = "ROOT", _RC)
    cap%root_name = trim(ROOT_NAME)

    ! !RESOURCE_ITEM: string :: Name of HISTORY's config file
    call MAPL_GetResource(MAPLOBJ, HIST_CF, "HIST_CF:", default = "HIST.rc", _RC)

    ! !RESOURCE_ITEM: string :: Name of ExtData's config file
    call MAPL_GetResource(MAPLOBJ, EXTDATA_CF, "EXTDATA_CF:", default = 'ExtData.rc', _RC)

    ! !RESOURCE_ITEM: string :: Control Timers
    call MAPL_GetResource(MAPLOBJ, enableTimers, "MAPL_ENABLE_TIMERS:", default = 'NO', _RC)

    ! !RESOURCE_ITEM: string :: Control Memory Diagnostic Utility
    call MAPL_GetResource(MAPLOBJ, enableMemUtils, "MAPL_ENABLE_MEMUTILS:", default='NO', _RC)
    call MAPL_GetResource(MAPLOBJ, MemUtilsMode, "MAPL_MEMUTILS_MODE:", default = MAPL_MemUtilsModeBase, _RC)
    !EOR
    enableTimers = ESMF_UtilStringUpperCase(enableTimers, _RC)
    call MAPL_GetResource(maplobj,use_extdata2g,"USE_EXTDATA2G:",default=.false.,_RC)

    if (enableTimers /= 'YES') then
       call MAPL_ProfDisable(_RC)
    else
       call MAPL_GetResource(MAPLOBJ, timerModeStr, "MAPL_TIMER_MODE:", &
            default='MINMAX', _RC )

       timerModeStr = ESMF_UtilStringUpperCase(timerModeStr, _RC)

    end if
    cap%started_loop_timer=.false.

    enableMemUtils = ESMF_UtilStringUpperCase(enableMemUtils, _RC)

    if (enableMemUtils /= 'YES') then
       call MAPL_MemUtilsDisable( _RC )
    else
       call MAPL_MemUtilsInit( mode=MemUtilsMode, _RC )
    end if

    call MAPL_GetResource( MAPLOBJ, cap%printSpec, label='PRINTSPEC:', default = 0, _RC )

    call dirpaths%append(".",_RC)
    call ESMF_ConfigFindLabel(cap%config,Label='USER_DIRPATH:',isPresent=foundPath,_RC)
    if (foundPath) then
       tend=.false.
       do while (.not.tend)
          call ESMF_ConfigGetAttribute(cap%config,value=user_dirpath,default='',_RC)
          if (tempstring /= '') then
             call dirpaths%append(user_dirpath,_RC)
          end if
          call ESMF_ConfigNextLine(cap%config,tableEnd=tend,_RC)
       enddo
    end if

    ! Handle RUN_DT in ROOT_CF
    !-------------------------

    cap%cf_root = ESMF_ConfigCreate(_RC )
    call ESMF_ConfigLoadFile(cap%cf_root, ROOT_CF, _RC )

    call ESMF_ConfigGetAttribute(cap%cf_root, value=RUN_DT, Label="RUN_DT:", rc=status)
    if (STATUS == ESMF_SUCCESS) then
       if (heartbeat_dt /= run_dt) then
          call lgr%error('inconsistent values of HEARTBEAT_DT (%g0) and root RUN_DT (%g0)', heartbeat_dt, run_dt)
          _FAIL('inconsistent values of HEARTBEAT_DT and RUN_DT')
       end if
    else
       call MAPL_ConfigSetAttribute(cap%cf_root, value=heartbeat_dt, Label="RUN_DT:", _RC)
    endif

    ! Add EXPID and EXPDSC from HISTORY.rc to AGCM.rc
    !------------------------------------------------
    cap%cf_hist = ESMF_ConfigCreate(_RC )
    call ESMF_ConfigLoadFile(cap%cf_hist, HIST_CF, _RC )

    call MAPL_ConfigSetAttribute(cap%cf_hist, value=HIST_CF, Label="HIST_CF:", _RC)

    call ESMF_ConfigGetAttribute(cap%cf_hist, value=EXPID,  Label="EXPID:", default='',  _RC)
    call ESMF_ConfigGetAttribute(cap%cf_hist, value=EXPDSC, Label="EXPDSC:", default='', _RC)

    call MAPL_ConfigSetAttribute(cap%cf_hist, value=heartbeat_dt, Label="RUN_DT:", _RC)

    call MAPL_ConfigSetAttribute(cap%cf_root, value=EXPID,  Label="EXPID:",  _RC)
    call MAPL_ConfigSetAttribute(cap%cf_root, value=EXPDSC, Label="EXPDSC:", _RC)

    call ESMF_ConfigGetAttribute(cap%cf_root, value = NX, Label="NX:", _RC)
    call ESMF_ConfigGetAttribute(cap%cf_root, value = NY, Label="NY:", _RC)
    call MAPL_ConfigSetAttribute(cap%cf_hist, value=NX,  Label="NX:",  _RC)
    call MAPL_ConfigSetAttribute(cap%cf_hist, value=NY,  Label="NY:",  _RC)

    ! Add CoresPerNode from CAP.rc to HISTORY.rc and AGCM.rc
    !-------------------------------------------------------
    call MAPL_ConfigSetAttribute(cap%cf_root, value=CoresPerNode,  Label="CoresPerNode:",  _RC)
    call MAPL_ConfigSetAttribute(cap%cf_hist, value=CoresPerNode,  Label="CoresPerNode:",  _RC)

    ! Add a SINGLE_COLUMN flag in HISTORY.rc based on DYCORE value(from AGCM.rc)
    !---------------------------------------------------------------------------
    call ESMF_ConfigGetAttribute(cap%cf_root, value=DYCORE,  Label="DYCORE:",  default = 'FV3', _RC)
    if (DYCORE == 'DATMO') then
       snglcol = 1
       call MAPL_ConfigSetAttribute(cap%cf_hist, value=snglcol,  Label="SINGLE_COLUMN:",  _RC)
    end if

    ! Detect if this a regular replay in the AGCM.rc
    ! ----------------------------------------------
    call ESMF_ConfigGetAttribute(cap%cf_root, value=ReplayMode, Label="REPLAY_MODE:", default="NoReplay", _RC)


    ! Register the children with MAPL
    !--------------------------------

    !  Create Root child
    !-------------------
    call MAPL_Set(MAPLOBJ, CF=CAP%CF_ROOT, _RC)

    root_set_services => cap%root_set_services

    call t_p%start('SetService')
    if (.not.allocated(cap%root_dso)) then
       cap%root_id = MAPL_AddChild(MAPLOBJ, name = root_name, SS = root_set_services, _RC)
    else
       sharedObj = trim(cap%root_dso)
       cap%root_id = MAPL_AddChild(MAPLOBJ, root_name, 'setservices_', sharedObj=sharedObj, _RC)
    end if
    root_gc => maplobj%get_child_gridcomp(cap%root_id)
    call MAPL_GetObjectFromGC(root_gc, root_obj, _RC)
    _ASSERT(cap%n_run_phases <= SIZE(root_obj%phase_run),"n_run_phases in cap_gc should not exceed n_run_phases in root")

    !  Create History child
    !----------------------

    call MAPL_Set(MAPLOBJ, CF=CAP%CF_HIST, _RC)

    cap%history_id = MAPL_AddChild( MAPLOBJ, name = 'HIST', SS = HIST_SetServices, _RC)


    !  Create ExtData child
    !----------------------
    cap%cf_ext = ESMF_ConfigCreate(_RC )
    call ESMF_ConfigLoadFile(cap%cf_ext, EXTDATA_CF, _RC )

    call ESMF_ConfigGetAttribute(cap%cf_ext, value=RUN_DT, Label="RUN_DT:", rc=status)
    if (STATUS == ESMF_SUCCESS) then
       if (heartbeat_dt /= run_dt) then
          call lgr%error('inconsistent values of HEARTBEAT_DT (%g0) and ExtData RUN_DT (%g0)', heartbeat_dt, run_dt)
          _FAIL('inconsistent values of HEARTBEAT_DT and RUN_DT')
       end if
    else
       call MAPL_ConfigSetAttribute(cap%cf_ext, value=heartbeat_dt, Label="RUN_DT:", _RC)
    endif

    call MAPL_Set(MAPLOBJ, CF=CAP%CF_EXT, _RC)

    if (use_extdata2g) then
#if defined(BUILD_WITH_EXTDATA2G)
       cap%extdata_id = MAPL_AddChild (MAPLOBJ, name = 'EXTDATA', SS = ExtData2G_SetServices, _RC)
#else
       call lgr%error('ExtData2G requested but not built')
       _FAIL('ExtData2G requested but not built')
#endif
    else
       cap%extdata_id = MAPL_AddChild (MAPLOBJ, name = 'EXTDATA', SS = ExtData1G_SetServices, _RC)
    end if
    call t_p%stop('SetService')

    ! Add NX and NY from AGCM.rc to ExtData.rc as well as name of ExtData rc file
    call ESMF_ConfigGetAttribute(cap%cf_root, value = NX, Label="NX:", _RC)
    call ESMF_ConfigGetAttribute(cap%cf_root, value = NY, Label="NY:", _RC)
    call MAPL_ConfigSetAttribute(cap%cf_ext, value=NX,  Label="NX:",  _RC)
    call MAPL_ConfigSetAttribute(cap%cf_ext, value=NY,  Label="NY:",  _RC)
    call MAPL_ConfigSetAttribute(cap%cf_ext, value=EXTDATA_CF,  Label="CF_EXTDATA:",  _RC)
    call MAPL_ConfigSetAttribute(cap%cf_ext, value=EXPID,  Label="EXPID:",  _RC)

    !  Query MAPL for the the children's for GCS, IMPORTS, EXPORTS
    !-------------------------------------------------------------

    call MAPL_Get(MAPLOBJ, childrens_gridcomps = cap%gcs, &
         childrens_import_states = cap%child_imports, childrens_export_states = cap%child_exports, _RC)


    !  Inject grid to root child if grid has been set externally
    !-----------------------------------------------------------

    call cap%inject_external_grid(_RC)

    ! Run as usual unless PRINTSPEC> 0 as set in CAP.rc. If set then
    ! model will not run completely and instead it will simply run MAPL_SetServices
    ! and print out the IM/EX specs. This step uses MAPL_StatePrintSpecCSV found
    ! in MAPL_Generic.F90.


    if (cap%printSpec>0) then
       call MAPL_StatePrintSpecCSV(cap%gcs(cap%root_id), cap%printspec, _RC)
       call ESMF_VMBarrier(cap%vm, _RC)
    else
       !  Initialize the Computational Hierarchy
       !----------------------------------------

       call t_p%start('Initialize')
       call ESMF_GridCompInitialize(cap%gcs(cap%root_id), importState = cap%child_imports(cap%root_id), &
            exportState = cap%child_exports(cap%root_id), clock = cap%clock, userRC = status)
       _VERIFY(status)

       call cap%initialize_history(_RC)

       call cap%initialize_extdata(root_gc,_RC)

       ! Finally check is this is a regular replay
       ! If so stuff gc and input state for ExtData in GCM internal state
       ! -----------------------------------------------------------------
       if (trim(replayMode)=="Regular") then
          call MAPL_GCGet(CAP%GCS(cap%root_id),"GCM",gcmGC,_RC)
          call ESMF_GridCompGet(gcmGC,vm=gcmVM,_RC)
          _ASSERT(cap%vm==gcmVM,'CAP and GCM should agree on their VMs.')
          call ESMF_UserCompGetInternalState(gcmGC,'ExtData_state',wrap,status)
          _VERIFY(STATUS)
          ExtData_internal_state => wrap%ptr
          ExtData_internal_state%gc = CAP%GCS(cap%extdata_id)
          ExtData_internal_state%expState = CAP%CHILD_EXPORTS(cap%extdata_id)
       end if
       call t_p%stop('Initialize')
    end if


    _RETURN(ESMF_SUCCESS)
  end subroutine initialize_gc


  subroutine initialize_history(cap, rc)
    class(MAPL_CapGridComp), intent(inout) :: cap
    integer, optional, intent(out) :: rc
    integer :: status
    type(HISTORY_ExchangeListWrap) :: lswrap
    integer(kind=INT64), pointer   :: LSADDR(:) => null()

    if (present(rc)) rc = ESMF_SUCCESS
    ! All the EXPORTS of the Hierachy are made IMPORTS of History
    !------------------------------------------------------------
    call ESMF_StateAdd(cap%child_imports(cap%history_id), [cap%child_exports(cap%root_id)], _RC)

    allocate(lswrap%ptr, _STAT)
    call ESMF_UserCompSetInternalState(cap%gcs(cap%history_id), 'MAPL_LocStreamList', &
         lswrap, STATUS)
    _VERIFY(STATUS)
    call MAPL_GetAllExchangeGrids(CAP%GCS(cap%root_id), LSADDR, _RC)
    lswrap%ptr%LSADDR_PTR => LSADDR

    ! Initialize the History
    !------------------------

    call ESMF_GridCompInitialize (CAP%GCS(cap%history_id), importState=CAP%CHILD_IMPORTS(cap%history_id), &
         exportState=CAP%CHILD_EXPORTS(cap%history_id), clock=CAP%CLOCK_HIST, userRC=STATUS )
    _VERIFY(STATUS)

    _RETURN(ESMF_SUCCESS)
  end subroutine initialize_history


  subroutine initialize_extdata(cap , root_gc, rc)
    class(MAPL_CapGridComp), intent(inout) :: cap
    type (ESMF_GridComp), intent(inout), pointer :: root_gc
    integer, optional, intent(out) :: rc
    integer :: item_count, status
    type (ESMF_StateItem_Flag), pointer   :: item_types(:)
    character(len=ESMF_MAXSTR ), pointer  :: item_names(:)
    type(ESMF_Field) :: field
    type(ESMF_FieldBundle) :: bundle
    type(StringVector) :: cap_imports_vec, cap_exports_vec, extdata_imports_vec
    type(StringVectorIterator) :: iter
    integer :: i
    type(ESMF_State) :: state, root_imports, component_state
    character(len=:), allocatable :: component_name, field_name

    ! Prepare EXPORTS for ExtData
    ! ---------------------------
    cap_imports_vec = get_vec_from_config(cap%config, "CAP_IMPORTS", _RC)
    cap_exports_vec = get_vec_from_config(cap%config, "CAP_EXPORTS", _RC)
    extdata_imports_vec = get_vec_from_config(cap%config, "EXTDATA_IMPORTS")

    cap%import_state = ESMF_StateCreate(name = "Cap_Imports", stateintent = ESMF_STATEINTENT_IMPORT)
    cap%export_state = ESMF_StateCreate(name = "Cap_Exports", stateintent = ESMF_STATEINTENT_EXPORT)

    if (cap_exports_vec%size() /= 0) then
       iter = cap_exports_vec%begin()
       do while(iter /= cap_exports_vec%end())
          component_name = iter%get()
          component_name = trim(component_name(index(component_name, ",")+1:))
          field_name = iter%get()
          field_name = trim(field_name(1:index(field_name, ",")-1))
          call MAPL_ExportStateGet([cap%child_exports(cap%root_id)], component_name, &
               component_state, status)
          _VERIFY(status)
          call ESMF_StateGet(component_state, trim(field_name), field, _RC)
          call MAPL_StateAdd(cap%export_state, field, _RC)
          call iter%next()
       end do
    end if

    if (extdata_imports_vec%size() /= 0) then
       iter = extdata_imports_vec%begin()
       do while(iter /= extdata_imports_vec%end())
          component_name = iter%get()
          component_name = trim(component_name(index(component_name, ",")+1:))

          field_name = iter%get()
          field_name = trim(field_name(1:index(field_name, ",")-1))

          call MAPL_ExportStateGet([cap%child_exports(cap%root_id)], component_name, &
               component_state, _RC)
          call ESMF_StateGet(component_state, trim(field_name), field, _RC)
          call MAPL_StateAdd(cap%child_imports(cap%extdata_id), field, _RC)
          call iter%next()
       end do
    end if

    call ESMF_StateGet(cap%child_imports(cap%root_id), itemcount = item_count, _RC)
    allocate(item_names(item_count), _STAT)
    allocate(item_types(item_count), _STAT)

    call ESMF_StateGet(cap%child_imports(cap%root_id), itemnamelist = item_names, &
         itemtypelist = item_types, _RC)

    root_imports = cap%child_imports(cap%root_id)
    do i = 1, item_count
       if (vector_contains_str(cap_imports_vec, item_names(i))) then
          state = cap%import_state
       else
          state = cap%child_exports(cap%extdata_id)
       end if
       if (item_types(i) == ESMF_StateItem_Field) then
          call ESMF_StateGet(root_imports, item_names(i), field, _RC)
          call MAPL_AddAttributeToFields(root_gc,trim(item_names(i)),'RESTART',MAPL_RestartSkip,_RC)
          call MAPL_StateAdd(state, field, _RC)
       else if (item_types(i) == ESMF_StateItem_FieldBundle) then
          call ESMF_StateGet(root_imports, item_names(i), bundle, _RC)
          call MAPL_StateAdd(state, bundle, _RC)
       end if
    end do

    deallocate(item_types)
    deallocate(item_names)

    ! Initialize the ExtData
    !------------------------

    call ESMF_GridCompInitialize (cap%gcs(cap%extdata_id), importState = cap%child_imports(cap%extdata_id), &
         exportState = cap%child_exports(cap%extdata_id), &
         clock = cap%clock, userRc = status)
    _VERIFY(status)

    _RETURN(ESMF_SUCCESS)

  end subroutine initialize_extdata


  subroutine run_gc(gc, import, export, clock, rc)
    !ARGUMENTS:
    type(ESMF_GridComp) :: GC     ! Gridded component
    type(ESMF_State) :: import ! Import state
    type(ESMF_State) :: export ! Export state
    type(ESMF_Clock) :: clock  ! The clock
    integer, intent(out) :: RC     ! Error code:

    integer :: status, phase
    class (BaseProfiler), pointer :: t_p

    _UNUSED_DUMMY(import)
    _UNUSED_DUMMY(export)
    _UNUSED_DUMMY(clock)

    t_p => get_global_time_profiler()
    call t_p%start('Run')

    call ESMF_GridCompGet( gc, currentPhase=phase, _RC )
    VERIFY_(status)

    call run_MAPL_GridComp(gc, phase=phase, _RC)
    _VERIFY(status)

    call t_p%stop('Run')

    _RETURN(ESMF_SUCCESS)

  end subroutine run_gc


  subroutine finalize_gc(gc, import_state, export_state, clock, rc)
    type(ESMF_GridComp) :: gc
    type(ESMF_State) :: import_state, export_state
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    integer :: status

    type(MAPL_CapGridComp), pointer :: cap
    type(MAPL_MetaComp), pointer :: maplobj
    class (BaseProfiler), pointer :: t_p

    _UNUSED_DUMMY(import_state)
    _UNUSED_DUMMY(export_state)
    _UNUSED_DUMMY(clock)

    cap => get_CapGridComp_from_gc(gc, _RC)
    call MAPL_GetObjectFromGC(gc, maplobj, _RC)

    t_p => get_global_time_profiler()
    call t_p%start('Finalize')

    if (.not. cap%printspec > 0) then

       call ESMF_GridCompFinalize(cap%gcs(cap%root_id), importstate = cap%child_imports(cap%root_id), &
            exportstate=cap%child_exports(cap%root_id), clock = cap%clock, userrc = status)
       _VERIFY(status)

       call ESMF_GridCompFinalize(cap%gcs(cap%history_id), importstate = cap%child_imports(cap%history_id), &
            exportstate = cap%child_exports(cap%history_id), clock = cap%clock_hist, userrc = status)
       _VERIFY(status)

       call ESMF_GridCompFinalize(cap%gcs(cap%extdata_id), importstate = cap%child_imports(cap%extdata_id), &
            exportstate = cap%child_exports(cap%extdata_id), clock = cap%clock, userrc = status)
       _VERIFY(status)


       call CAP_Finalize(CAP%CLOCK_HIST, "cap_restart", _RC)

       call ESMF_ConfigDestroy(cap%cf_ext, _RC)
       call ESMF_ConfigDestroy(cap%cf_hist, _RC)
       call ESMF_ConfigDestroy(cap%cf_root, _RC)
       call ESMF_ConfigDestroy(cap%config, _RC)

       call MAPL_FinalizeShmem(_RC)

       ! Write EGRESS file
       !------------------
       call ESMF_VMBarrier(cap%vm)

       if(allocated(cap%final_file)) then
          if (cap%AmIRoot) then
             close(99)
             open (99,file=cap%final_file,form='formatted')
             close(99)
          end if
       end if
    end if

    call t_p%stop('Finalize')

    _RETURN(ESMF_SUCCESS)
  end subroutine finalize_gc


  subroutine set_services_gc(gc, rc)
    type (ESMF_GridComp) :: gc
    integer, intent(out) :: rc

    integer :: status, phase
    type(MAPL_CapGridComp), pointer :: cap

    cap => get_CapGridComp_from_gc(gc, _RC)
    call ESMF_GridCompSetEntryPoint(gc, ESMF_METHOD_INITIALIZE, userRoutine = initialize_gc, _RC)

    do phase = 1, cap%n_run_phases
       call ESMF_GridCompSetEntryPoint(gc, ESMF_METHOD_RUN, userRoutine = run_gc, _RC)
    enddo

    call ESMF_GridCompSetEntryPoint(gc, ESMF_METHOD_FINALIZE, userRoutine = finalize_gc, _RC)
    _RETURN(ESMF_SUCCESS)

  end subroutine set_services_gc


  subroutine set_services(this, rc)
    class(MAPL_CapGridComp), intent(inout) :: this
    integer, optional, intent(out) :: rc
    integer :: status

    call ESMF_GridCompSetServices(this%gc, set_services_gc, _RC)
    _RETURN(ESMF_SUCCESS)
  end subroutine set_services


  subroutine initialize(this, rc)
    class(MAPL_CapGridComp), intent(inout) :: this
    integer, optional, intent(out) :: rc

    integer :: status

    call ESMF_GridCompInitialize(this%gc, userRC=status)
    _VERIFY(status)
    _RETURN(ESMF_SUCCESS)
  end subroutine initialize


  subroutine run(this, phase, rc)
    class(MAPL_CapGridComp), intent(inout) :: this
    integer, optional, intent(in) :: phase
    integer, optional, intent(out) :: rc

    integer :: status
    integer :: userrc, phase_

    phase_ = 1
    if (present(phase)) phase_ = phase

    call ESMF_GridCompRun(this%gc, phase=phase_, userrc=userrc, _RC)
    _VERIFY(userrc)
    _RETURN(ESMF_SUCCESS)

  end subroutine run

  subroutine finalize(this, rc)
    class(MAPL_CapGridComp), intent(inout) :: this
    integer, optional, intent(out) :: rc

    integer :: status

    call ESMF_GridCompFinalize(this%gc, _RC)
    _RETURN(ESMF_SUCCESS)
  end subroutine finalize

  function get_model_duration(this, rc) result (duration)
    class (MAPL_CapGridComp) :: this
    integer, optional, intent(out) :: rc


    integer :: duration

    duration = this%nsteps * this%heartbeat_dt

    _RETURN(ESMF_SUCCESS)

  end function get_model_duration


  function get_am_i_root(this, rc) result (amiroot)
    class (MAPL_CapGridComp) :: this
    integer, optional, intent(out) :: rc


    logical :: amiroot

    amiroot = this%amiroot

    _RETURN(ESMF_SUCCESS)

  end function get_am_i_root


  function get_heartbeat_dt(this, rc) result (heartbeatdt)
    class (MAPL_CapGridComp) :: this
    integer :: heartbeatdt
    integer, optional, intent(out) :: rc

    heartbeatdt = this%heartbeat_dt

    _RETURN(ESMF_SUCCESS)

  end function get_heartbeat_dt

  function get_current_time(this, rc) result (current_time)
    class (MAPL_CapGridComp) :: this
    type(ESMF_Time) :: current_time
    integer, optional, intent(out) :: rc
    integer :: status
    call ESMF_ClockGet(this%clock,currTime=current_time,_RC)

    _RETURN(ESMF_SUCCESS)

  end function get_current_time

  subroutine set_step_counter(this, n)
    class (MAPL_CapGridComp), intent(inout) :: this
    integer, intent(in) :: n

    this%step_counter = n
  end subroutine set_step_counter

  subroutine increment_step_counter(this)
    class (MAPL_CapGridComp), intent(inout) :: this

    this%step_counter = this%step_counter + 1
  end subroutine increment_step_counter

  function get_step_counter(this) result (step_counter)
    class (MAPL_CapGridComp), intent(in) :: this
    integer :: step_counter

    step_counter = this%step_counter
  end function get_step_counter

  function get_CapGridComp_from_gc(gc, rc) result(cap)
    type(ESMF_GridComp), intent(inout) :: gc
    integer, optional, intent(out) :: rc
    type(MAPL_CapGridComp), pointer :: cap

    type(MAPL_CapGridComp_Wrapper) :: cap_wrapper
    integer :: status

    call ESMF_UserCompGetInternalState(gc, internal_cap_name, cap_wrapper, status)
    _VERIFY(status)

    cap => cap_wrapper%ptr
    _RETURN(_SUCCESS)
  end function get_CapGridComp_from_gc



  function get_vec_from_config(config, key, rc) result(vec)
    type(StringVector) :: vec
    type(ESMF_Config), intent(inout) :: config
    character(len=*), intent(in) :: key
    integer, intent(out), optional :: rc
    logical :: present, tableEnd
    integer :: status
    character(len=ESMF_MAXSTR) :: value

    call ESMF_ConfigFindLabel(config, key//":", isPresent = present, _RC)

    if (present) then
       do
          call ESMF_ConfigNextLine(config, tableEnd=tableEnd, _RC)
          if (tableEnd) exit
          call ESMF_ConfigGetAttribute(config, value, _RC)
          call vec%push_back(trim(value))
       end do
    end if
    _RETURN(_SUCCESS)

  end function get_vec_from_config


  logical function vector_contains_str(vector, string)
    type(StringVector), intent(in) :: vector
    character(len=*), intent(in) :: string
    type(StringVectorIterator) :: iter

    iter = vector%begin()

    vector_contains_str = .false.

    if (vector%size() /= 0) then
       do while (iter /= vector%end())
          if (trim(string) == iter%get()) then
             vector_contains_str = .true.
             return
          end if
          call iter%next()
       end do
    end if

  end function vector_contains_str


  subroutine run_MAPL_GridComp(gc, phase, rc)
    type (ESMF_Gridcomp) :: gc
    integer, optional, intent(in)  :: phase
    integer, optional, intent(out) :: rc

    integer :: n, status, phase_
    logical :: done

    type(MAPL_CapGridComp), pointer :: cap
    type (MAPL_MetaComp), pointer :: MAPLOBJ
    procedure(), pointer :: root_set_services

    cap => get_CapGridComp_from_gc(gc, _RC)
    call MAPL_GetObjectFromGC(gc, maplobj, _RC)

    phase_ = 1
    if (present(phase)) phase_ = phase

    if (.not. cap%printspec > 0) then

       ! Time Loop starts by checking for Segment Ending Time
       !-----------------------------------------------------
       if (cap%compute_throughput) then
          call ESMF_VMBarrier(cap%vm,_RC)
          cap%starts%loop_start_timer = MPI_WTime()
          cap%started_loop_timer = .true.
       end if

       call cap%set_step_counter(0)

       TIME_LOOP: do n = 1, cap%nsteps

          call cap%increment_step_counter()

          call MAPL_MemUtilsWrite(cap%vm, 'MAPL_Cap:TimeLoop', _RC)

          if (.not.cap%lperp) then
             done = ESMF_ClockIsStopTime(cap%clock_hist, _RC)
             if (done) exit
          endif

          call cap%step(phase=phase_, _RC)

          ! Reset loop average timer to get a better
          ! estimate of true run time left by ignoring
          ! initialization costs in the averageing.
          !-------------------------------------------
          if (n == 1 .and. cap%compute_throughput) then
             call ESMF_VMBarrier(cap%vm,_RC)
             cap%starts%loop_start_timer = MPI_WTime()
          endif

       enddo TIME_LOOP ! end of time loop

    end if

    _RETURN(ESMF_SUCCESS)
  end subroutine run_MAPL_GridComp


  subroutine step(this, unusable, phase, rc)
    class(MAPL_CapGridComp), intent(inout) :: this
    class(KeywordEnforcer), optional, intent(in   ) :: unusable
    integer, optional, intent(in) :: phase
    integer, optional, intent(out) :: rc

    integer :: status, phase_
    real(kind=REAL64) :: END_RUN_TIMER, END_TIMER

    _UNUSED_DUMMY(unusable)
    phase_ = 1
    if (present(phase)) phase_ = phase

    call ESMF_GridCompGet(this%gc, vm = this%vm)

    ! Run the ExtData Component
    ! --------------------------
    if (phase_ == 1) then

      call first_phase(_RC)

    endif ! phase_ == 1
    ! Run the Gridded Component
    ! --------------------------
    call ESMF_GridCompRun(this%gcs(this%root_id), importstate = this%child_imports(this%root_id), &
         exportstate = this%child_exports(this%root_id), &
         clock = this%clock, phase=phase_, userrc = status)
    _VERIFY(status)
    ! Advance the Clock and run History and Record
    ! ---------------------------------------------------
    if (phase_ == this%n_run_phases) then

       call last_phase(_RC)

    endif !phase_ == last

    _RETURN(ESMF_SUCCESS)

  contains

     subroutine first_phase(rc)
        integer, optional, intent(out) :: rc
        integer :: status

        if (this%compute_throughput) then
           if (.not.this%started_loop_timer) then
              this%starts%loop_start_timer = MPI_WTime()
              this%started_loop_timer=.true.
           end if
           this%starts%start_timer = MPI_Wtime()
        end if

        call ESMF_GridCompRun(this%gcs(this%extdata_id), importState = this%child_imports(this%extdata_id), &
             exportState = this%child_exports(this%extdata_id), &
             clock = this%clock, userrc = status)
         _VERIFY(status)
        ! Call Record for intermediate checkpoint (if desired)
        ! ------------------------------------------------------
        call ESMF_GridCompWriteRestart(this%gcs(this%root_id), importstate = this%child_imports(this%root_id), &
             exportstate = this%child_exports(this%root_id), &
             clock = this%clock_hist, userrc = status)
        _VERIFY(status)

        call ESMF_GridCompWriteRestart(this%gcs(this%history_id), importstate = this%child_imports(this%history_id), &
             exportstate = this%child_exports(this%history_id), &
             clock = this%clock_hist, userrc = status)
        _VERIFY(status)

        if (this%compute_throughput) then
           call ESMF_VMBarrier(this%vm,_RC)
           this%starts%start_run_timer = MPI_WTime()
        end if

        _RETURN(_SUCCESS)

     end subroutine

     subroutine last_phase(rc)
        integer, optional, intent(out) :: rc
        integer :: status

        if (this%compute_throughput) then
           call ESMF_VMBarrier(this%vm,_RC)
           end_run_timer = MPI_WTime()
        end if

        call ESMF_ClockAdvance(this%clock, _RC)
        call ESMF_ClockAdvance(this%clock_hist, _RC)

        ! Update Perpetual Clock
        ! ----------------------
        if (this%lperp) then
           call Perpetual_Clock(this, status)
           _VERIFY(status)
        end if

        call ESMF_GridCompRun(this%gcs(this%history_id), importstate=this%child_imports(this%history_id), &
             exportstate = this%child_exports(this%history_id), &
             clock = this%clock_hist, userrc = status)
        _VERIFY(status)
        ! Estimate throughput times
        ! ---------------------------
        if (this%compute_throughput) then
           call print_throughput(_RC)
        end if

        _RETURN(_SUCCESS)

     end subroutine

     subroutine print_throughput(rc)
        integer, optional, intent(out) :: rc
        integer :: status, n

        real(kind=REAL64) ::  TIME_REMAINING
        real(kind=REAL64) ::  LOOP_THROUGHPUT
        real(kind=REAL64) ::  INST_THROUGHPUT
        real(kind=REAL64) ::  RUN_THROUGHPUT
        real              :: mem_total, mem_commit, mem_committed_percent
        real              :: mem_used, mem_used_percent
        type(ESMF_Time)   :: currTime
        type(ESMF_TimeInterval) :: delt
        integer                 :: AGCM_YY, AGCM_MM, AGCM_DD, AGCM_H, AGCM_M, AGCM_S
        integer                 :: HRS_R, MIN_R, SEC_R

        character(len=8)  :: wallclock_date
        character(len=10) :: wallclock_time
        character(len=5)  :: wallclock_zone
        integer           :: wallclock_values(8)


        call ESMF_ClockGet(this%clock, CurrTime = currTime, _RC)
        call ESMF_TimeGet(CurrTime, YY = AGCM_YY, &
            MM = AGCM_MM, &
            DD = AGCM_DD, &
            H  = AGCM_H , &
            M  = AGCM_M , &
            S  = AGCM_S, _RC)
        delt=currTime-this%cap_restart_time
        ! Call system clock to estimate throughput simulated Days/Day
        call ESMF_VMBarrier( this%vm, _RC )
        END_TIMER = MPI_Wtime()
        n=this%get_step_counter()
        !GridCompRun Timer [Inst]
        RUN_THROUGHPUT  = REAL(  this%HEARTBEAT_DT,kind=REAL64)/(END_RUN_TIMER-this%starts%start_run_timer)
        ! Time loop throughput [Inst]
        INST_THROUGHPUT = REAL(  this%HEARTBEAT_DT,kind=REAL64)/(END_TIMER-this%starts%start_timer)
        ! Time loop throughput [Avg]
        LOOP_THROUGHPUT = REAL(n*this%HEARTBEAT_DT,kind=REAL64)/(END_TIMER-this%starts%loop_start_timer)
        ! Estimate time remaining (seconds)
        TIME_REMAINING = REAL((this%nsteps-n)*this%HEARTBEAT_DT,kind=REAL64)/LOOP_THROUGHPUT
        HRS_R = FLOOR(TIME_REMAINING/3600.0)
        MIN_R = FLOOR(TIME_REMAINING/60.0  -   60.0*HRS_R)
        SEC_R = FLOOR(TIME_REMAINING       - 3600.0*HRS_R - 60.0*MIN_R)
        ! Reset Inst timer
        this%starts%start_timer = END_TIMER
        ! Get percent of used memory
        call MAPL_MemUsed ( mem_total, mem_used, mem_used_percent, _RC )
        ! Get percent of committed memory
        call MAPL_MemCommited ( mem_total, mem_commit, mem_committed_percent, _RC )

        if( mapl_am_I_Root(this%vm) ) then
            call DATE_AND_TIME(wallclock_date, &
                               wallclock_time, &
                               wallclock_zone, &
                               wallclock_values)
             write(6,1000) this%root_name, AGCM_YY,AGCM_MM,AGCM_DD,AGCM_H,AGCM_M,AGCM_S,&
                           LOOP_THROUGHPUT,INST_THROUGHPUT,RUN_THROUGHPUT,HRS_R,MIN_R,SEC_R,&
                           mem_committed_percent,mem_used_percent,wallclock_date,wallclock_values(5),wallclock_values(6)
        endif
    1000 format(1x,'AGCM Date: ',i4.4,'/',i2.2,'/',i2.2,2x,'Time: ',i2.2,':',i2.2,':',i2.2, &
                2x,'Throughput(days/day)[Avg Tot Run]: ',f12.1,1x,f12.1,1x,f12.1,2x,'TimeRemaining(Est) ',i3.3,':',i2.2,':',i2.2,2x, &
                f5.1,'% : ',f5.1,'% Mem Comm:Used ; ',A8,' ',i2.2,':'i2.2)

        _RETURN(_SUCCESS)

     end subroutine

  end subroutine step

  subroutine record_state(this, rc)
    class(MAPL_CapGridComp), intent(inout) :: this
    integer, intent(out) :: rc
    integer :: status
    type(MAPL_MetaComp), pointer :: maplobj

    integer :: nalarms,i

    call MAPL_GetObjectFromGC(this%gcs(this%root_id),maplobj,_RC)
    call MAPL_GenericStateSave(this%gcs(this%root_id),this%child_imports(this%root_id), &
           this%child_exports(this%root_id),this%clock,_RC)

    call ESMF_ClockGet(this%clock,alarmCount=nalarms,_RC)

    allocate(this%alarm_list(nalarms),this%ringingState(nalarms),this%alarmRingTime(nalarms),_STAT)
    call ESMF_ClockGetAlarmList(this%clock, alarmListFlag=ESMF_ALARMLIST_ALL, &
         alarmList=this%alarm_list, _RC)
    do i = 1, nalarms
       call ESMF_AlarmGet(this%alarm_list(I), ringTime=this%alarmRingTime(I), ringing=this%ringingState(I), _RC)
       VERIFY_(STATUS)
    end do

    _RETURN(_SUCCESS)

  end subroutine record_state

  subroutine refresh_state(this, rc)
    class(MAPL_CapGridComp), intent(inout) :: this
    integer, intent(out) :: rc
    integer :: status

    integer :: i
    call MAPL_GenericStateRestore(this%gcs(this%root_id),this%child_imports(this%root_id), &
             this%child_exports(this%root_id),this%clock,_RC)
    DO I = 1, size(this%alarm_list)
       call ESMF_AlarmSet(this%alarm_list(I), ringTime=this%alarmRingTime(I), ringing=this%ringingState(I), _RC)
    END DO

    _RETURN(_SUCCESS)

  end subroutine refresh_state

  subroutine get_field_from_import(this,field_name,state_name,field,rc)
    class(MAPL_CapGridComp), intent(inout) :: this
    character(len=*), intent(in) :: field_name
    character(len=*), intent(in) :: state_name
    type(ESMF_Field), intent(inout) :: field
    integer, intent(out) :: rc
    integer :: status

    type(ESMF_State) :: state

    call MAPL_ImportStateGet(this%gcs(this%root_id),this%child_imports(this%root_id),&
         state_name,state,_RC)
    call ESMF_StateGet(state,trim(field_name),field,_RC)
    _RETURN(_SUCCESS)

  end subroutine get_field_from_import

  subroutine get_field_from_internal(this,field_name,state_name,field,rc)
    class(MAPL_CapGridComp), intent(inout) :: this
    character(len=*), intent(in) :: field_name
    character(len=*), intent(in) :: state_name
    type(ESMF_field), intent(inout) :: field
    integer, intent(out) :: rc
    integer :: status

    type(ESMF_State) :: state

    call MAPL_InternalESMFStateGet(this%gcs(this%root_id),state_name,state,_RC)
    call ESMF_StateGet(state,trim(field_name),field,_RC)
    _RETURN(_SUCCESS)

  end subroutine get_field_from_internal

  subroutine set_grid(this, grid, unusable, lm, grid_type, rc)
     class(MAPL_CapGridComp),          intent(inout) :: this
     type(ESMF_Grid),                  intent(in   ) :: grid
     class(KeywordEnforcer), optional, intent(in   ) :: unusable
     integer,                optional, intent(in   ) :: lm
     character(len=*),       optional, intent(in)    :: grid_type
     integer,                optional, intent(  out) :: rc

     type(ESMF_Grid)           :: mapl_grid
     type(ExternalGridFactory) :: external_grid_factory
     integer                   :: status
     character(len=ESMF_MAXSTR):: grid_type_


     _UNUSED_DUMMY(unusable)

     external_grid_factory = ExternalGridFactory(grid=grid, lm=lm, _RC)
     mapl_grid = grid_manager%make_grid(external_grid_factory, _RC)
     ! grid_type is an optional parameter that allows GridType to be set explicitly.
     call ESMF_ConfigGetAttribute(this%config, value = grid_type_, Label="GridType:", default="", rc=status)
     if (status == ESMF_RC_OBJ_NOT_CREATED) then
       grid_type_ = ""
     else
       _VERIFY(status)
     endif
     if (present(grid_type)) then
        if(grid_type_ /= "") then
          _ASSERT(grid_type_ == grid_type, "The grid types don't match")
        endif
        if (grid_manager%is_valid_prototype(grid_type)) then
           call ESMF_AttributeSet(mapl_grid, 'GridType', grid_type, _RC)
        else
           _RETURN(_FAILURE)
        end if
     else if (grid_type_ /= "") then
        if (grid_manager%is_valid_prototype(grid_type_)) then
           call ESMF_AttributeSet(mapl_grid, 'GridType', grid_type_, _RC)
        else
           _RETURN(_FAILURE)
        end if
     endif

     call ESMF_GridCompSet(this%gc, grid=mapl_grid, _RC)

     _RETURN(_SUCCESS)
  end subroutine set_grid

  subroutine inject_external_grid(this, unusable, rc)
     class(MAPL_CapGridComp),          intent(inout) :: this
     class(KeywordEnforcer), optional, intent(in   ) :: unusable
     integer,                optional, intent(  out) :: rc

     type(ESMF_GridMatch_Flag) :: grid_match
     type(ESMF_Grid)           :: cap_grid,            root_grid
     logical                   :: cap_grid_is_present, root_grid_is_present
     integer                   :: status

     _UNUSED_DUMMY(unusable)

     call ESMF_GridCompGet(this%gc, gridIsPresent=cap_grid_is_present, _RC)

     if (cap_grid_is_present) then
        call ESMF_GridCompGet(this%gc, grid=cap_grid, _RC)
        call ESMF_GridValidate(cap_grid, _RC)

        call ESMF_GridCompGet(this%gcs(this%root_id), gridIsPresent=root_grid_is_present, _RC)

        if (root_grid_is_present) then
           call ESMF_GridCompGet(this%gcs(this%root_id), grid=root_grid, _RC)
           call ESMF_GridValidate(root_grid, _RC)

           grid_match = ESMF_GridMatch(cap_grid, root_grid, _RC)
           _ASSERT(grid_match == ESMF_GRIDMATCH_EXACT, "Attempting to override root grid with non-matching external grid")
        else
            call ESMF_GridCompSet(this%gcs(this%root_id), grid=cap_grid, _RC)
        end if
     end if

     _RETURN(_SUCCESS)
  end subroutine inject_external_grid

  subroutine set_clock(this, clock, unusable, rc)
     class(MAPL_CapGridComp),          intent(inout) :: this
     type(ESMF_Clock),                 intent(in   ) :: clock
     class(KeywordEnforcer), optional, intent(in   ) :: unusable
     integer,                optional, intent(  out) :: rc

     integer :: status

     _UNUSED_DUMMY(unusable)

     call ESMF_GridCompSet(this%gc, clock=clock, _RC)

     _RETURN(_SUCCESS)
  end subroutine set_clock

  subroutine destroy_state(this, rc)
    class(MAPL_CapGridComp), intent(inout) :: this
    integer, intent(out) :: rc
    integer :: status

    call MAPL_DestroyStateSave(this%gcs(this%root_id),_RC)

     if (allocated(this%alarm_list)) deallocate(this%alarm_list)
     if (allocated(this%AlarmRingTime)) deallocate(this%alarmRingTime)
     if (allocated(this%ringingState)) deallocate(this%ringingState)

    _RETURN(_SUCCESS)

  end subroutine destroy_state

  subroutine rewind_clock(this, time, rc)
    class(MAPL_CapGridComp), intent(inout) :: this
    type(ESMF_Time), intent(inout) :: time
    integer, intent(out) :: rc
    integer :: status
    type(ESMF_Time) :: current_time,ct

    call ESMF_ClockGet(this%clock,currTime=current_time,_RC)
    if (current_time >  time) then
       call ESMF_ClockSet(this%clock,direction=ESMF_DIRECTION_REVERSE,_RC)
       do
          call ESMF_ClockAdvance(this%clock,_RC)
          call ESMF_ClockGet(this%clock,currTime=ct,_RC)
          if (ct==time) exit
       enddo
       call ESMF_ClockSet(this%clock,direction=ESMF_DIRECTION_FORWARD,_RC)
    end if

    call ESMF_ClockGet(this%clock_hist,currTime=current_time,_RC)
    if (current_time >  time) then
       call ESMF_ClockSet(this%clock_hist,direction=ESMF_DIRECTION_REVERSE,_RC)
       do
          call ESMF_ClockAdvance(this%clock_hist,_RC)
          call ESMF_ClockGet(this%clock_hist,currTime=ct,_RC)
          if (ct==time) exit
       enddo
       call ESMF_ClockSet(this%clock_hist,direction=ESMF_DIRECTION_FORWARD,_RC)
    end if


    _RETURN(_SUCCESS)
  end subroutine rewind_clock


!------------------------------------------------------------------------------
!>
! This is a private routine that sets the start and
! end times and the time interval of the application clock from the configuration.
! This time interal is the ``heartbeat'' of the application.
! The Calendar is set to Gregorian by default.
! The start time is temporarily set to 1 interval before the time in the
! configuration. Once the Alarms are set in intialize, the clock will
! be advanced to guarantee it and its alarms are in the same state as they
! were after the last advance before the previous Finalize.
!

  subroutine MAPL_ClockInit ( MAPLOBJ, Clock, nsteps, rc)

    type(MAPL_MetaComp), intent(inout) :: MAPLOBJ
    type(ESMF_Clock),    intent(  out) :: Clock
    integer,             intent(  out) :: nsteps
    integer, optional,   intent(  out) :: rc

    type(ESMF_Time)          :: StartTime    ! Initial     Begin  Time of Experiment
    type(ESMF_Time)          :: EndTime      ! Final       Ending Time of Experiment
    type(ESMF_Time)          :: StopTime     ! Final       Ending Time of Experiment
    type(ESMF_Time)          :: CurrTime     ! Current     Current Time of Experiment
    type(ESMF_TimeInterval)  :: timeStep     ! HEARTBEAT
    type(ESMF_TimeInterval)  :: duration
    type(ESMF_TimeInterval)  :: maxDuration
    type(ESMF_Calendar)      :: cal
    character(ESMF_MAXSTR)   :: calendar

    integer                  :: STATUS

    integer        :: BEG_YY
    integer        :: BEG_MM
    integer        :: BEG_DD
    integer        :: BEG_H
    integer        :: BEG_M
    integer        :: BEG_S

    integer        :: CUR_YY
    integer        :: CUR_MM
    integer        :: CUR_DD
    integer        :: CUR_H
    integer        :: CUR_M
    integer        :: CUR_S

    integer        :: END_YY
    integer        :: END_MM
    integer        :: END_DD
    integer        :: END_H
    integer        :: END_M
    integer        :: END_S

    integer        :: DUR_YY
    integer        :: DUR_MM
    integer        :: DUR_DD
    integer        :: DUR_H
    integer        :: DUR_M
    integer        :: DUR_S

    integer        :: HEARTBEAT_DT
    integer        :: NUM_DT
    integer        :: DEN_DT

    integer        :: UNIT
    integer        :: datetime(2)

    class(Logger), pointer :: lgr

    ! Begin
    !------

    ! Read Times From Config
    ! ----------------------

    !BOR

    call MAPL_GetResource( MAPLOBJ, datetime, label='BEG_DATE:', _RC )
    if(STATUS==ESMF_SUCCESS) then
       _ASSERT(is_valid_date(datetime(1)),'Invalid date in BEG_DATE')
       _ASSERT(is_valid_time(datetime(2)),'Invalid time in BEG_DATE')
       CALL MAPL_UnpackDateTime(DATETIME, BEG_YY, BEG_MM, BEG_DD, BEG_H, BEG_M, BEG_S)
    else

       ! !RESOURCE_ITEM: year :: Beginning year (integer)
       call MAPL_GetResource( MAPLOBJ, BEG_YY, label='BEG_YY:', DEFAULT=1, _RC )
       ! !RESOURCE_ITEM: month :: Beginning month (integer 1-12)
       call MAPL_GetResource( MAPLOBJ, BEG_MM, label='BEG_MM:', default=1, _RC )
       ! !RESOURCE_ITEM: day  :: Beginning day of month (integer 1-31)
       call MAPL_GetResource( MAPLOBJ, BEG_DD, label='BEG_DD:', default=1, _RC )
       ! !RESOURCE_ITEM: hour :: Beginning hour of day (integer 0-23)
       call MAPL_GetResource( MAPLOBJ, BEG_H , label='BEG_H:' , default=0, _RC )
       ! !RESOURCE_ITEM: minute :: Beginning minute (integer 0-59)
       call MAPL_GetResource( MAPLOBJ, BEG_M , label='BEG_M:' , default=0, _RC )
       ! !RESOURCE_ITEM: second :: Beginning second (integer 0-59)
       call MAPL_GetResource( MAPLOBJ, BEG_S , label='BEG_S:' , default=0, _RC )
    end if

    call MAPL_GetResource( MAPLOBJ, datetime, label='END_DATE:', _RC )
    if(STATUS==ESMF_SUCCESS) then
       _ASSERT(is_valid_date(datetime(1)),'Invalid date in END_DATE')
       _ASSERT(is_valid_time(datetime(2)),'Invalid time in END_DATE')
       CALL MAPL_UnpackDateTime(DATETIME, END_YY, END_MM, END_DD, END_H, END_M, END_S)
    else
       ! !RESOURCE_ITEM: year :: Ending year (integer)
       call MAPL_GetResource( MAPLOBJ, END_YY, label='END_YY:', DEFAULT=1, _RC )
       ! !RESOURCE_ITEM: month :: Ending month (integer 1-12)
       call MAPL_GetResource( MAPLOBJ, END_MM, label='END_MM:', default=1, _RC )
       ! !RESOURCE_ITEM: day  :: Ending day of month (integer 1-31)
       call MAPL_GetResource( MAPLOBJ, END_DD, label='END_DD:', default=1, _RC )
       ! !RESOURCE_ITEM: hour :: Ending hour of day (integer 0-23)
       call MAPL_GetResource( MAPLOBJ, END_H , label='END_H:' , default=0, _RC )
       ! !RESOURCE_ITEM: minute :: Ending minute (integer 0-59)
       call MAPL_GetResource( MAPLOBJ, END_M , label='END_M:' , default=0, _RC )
       ! !RESOURCE_ITEM: second :: Ending second (integer 0-59)
       call MAPL_GetResource( MAPLOBJ, END_S , label='END_S:' , default=0, _RC )
    end if

    ! Replace JOB_DURATION with JOB_SGMT as prefered RC parameter
    ! -----------------------------------------------------------
    call MAPL_GetResource( MAPLOBJ, datetime, label='JOB_SGMT:',     _RC )
    if(STATUS/=ESMF_SUCCESS) then
       call MAPL_GetResource( MAPLOBJ, datetime, label='JOB_DURATION:', _RC )
    end if

    if(STATUS==ESMF_SUCCESS) then
       CALL MAPL_UnpackDateTime(DATETIME, DUR_YY, DUR_MM, DUR_DD, DUR_H, DUR_M, DUR_S)
    else
       ! !RESOURCE_ITEM: year :: Ending year (integer)
       call MAPL_GetResource( MAPLOBJ, DUR_YY, label='DUR_YY:', DEFAULT=0, _RC )
       ! !RESOURCE_ITEM: month :: Ending month (integer 1-12)
       call MAPL_GetResource( MAPLOBJ, DUR_MM, label='DUR_MM:', default=0, _RC )
       ! !RESOURCE_ITEM: day  :: Ending day of month (integer 1-31)
       call MAPL_GetResource( MAPLOBJ, DUR_DD, label='DUR_DD:', default=1, _RC )
       ! !RESOURCE_ITEM: hour :: Ending hour of day (integer 0-23)
       call MAPL_GetResource( MAPLOBJ, DUR_H , label='DUR_H:' , default=0, _RC )
       ! !RESOURCE_ITEM: minute :: Ending minute (integer 0-59)
       call MAPL_GetResource( MAPLOBJ, DUR_M , label='DUR_M:' , default=0, _RC )
       ! !xRESOURCE_ITEM: second :: Ending second (integer 0-59)
       call MAPL_GetResource( MAPLOBJ, DUR_S , label='DUR_S:' , default=0, _RC )
    end if

    ! !RESOURCE_ITEM: seconds :: Interval of the application clock (the Heartbeat)
    call MAPL_GetResource( MAPLOBJ, HEARTBEAT_DT, label='HEARTBEAT_DT:',            _RC )
    ! !RESOURCE_ITEM: 1 :: numerator of decimal fraction of time step
    call MAPL_GetResource( MAPLOBJ, NUM_DT, label='NUM_DT:', default=0, _RC )
    ! !RESOURCE_ITEM: 1 :: denominator of decimal fraction of time step
    call MAPL_GetResource( MAPLOBJ, DEN_DT, label='DEN_DT:', default=1, _RC )
    ! !RESOURCE_ITEM: string :: Calendar type
    call MAPL_GetResource( MAPLOBJ, calendar, label='CALENDAR:', default="GREGORIAN", _RC )

    !EOR

    _ASSERT(NUM_DT>=0, 'NUM_DT should be >= 0.')
    _ASSERT(DEN_DT> 0, 'DEN_DT should be > 0.')
    _ASSERT(NUM_DT<DEN_DT, 'NUM_DT should be < DEN_DT')
    _ASSERT(HEARTBEAT_DT>=0, 'HEARTBEAT_DT should be >= 0.')

    ! initialize calendar to be Gregorian type
    ! ----------------------------------------

    if    (calendar=="GREGORIAN") then
       cal = ESMF_CalendarCreate( ESMF_CALKIND_GREGORIAN, name="ApplicationCalendar", _RC )
       call ESMF_CalendarSetDefault(ESMF_CALKIND_GREGORIAN, _RC)
    elseif(calendar=="JULIAN"   ) then
       cal = ESMF_CalendarCreate( ESMF_CALKIND_JULIAN, name="ApplicationCalendar", _RC )
       call ESMF_CalendarSetDefault(ESMF_CALKIND_JULIAN, _RC)
    elseif(calendar=="NOLEAP"   ) then
       cal = ESMF_CalendarCreate( ESMF_CALKIND_NOLEAP, name="ApplicationCalendar", _RC )
       call ESMF_CalendarSetDefault(ESMF_CALKIND_NOLEAP, _RC)
    else
       _FAIL('Unsupported calendar:'//trim(calendar))
    endif

    ! initialize start time for Alarm frequencies
    ! -------------------------------------------

    call ESMF_TimeSet( StartTime, YY = BEG_YY, &
         MM = BEG_MM, &
         DD = BEG_DD, &
         H = BEG_H , &
         M = BEG_M , &
         S = BEG_S , &
         calendar=cal, _RC)

    call ESMF_TimeSet(   EndTime, YY = END_YY, &
         MM = END_MM, &
         DD = END_DD, &
         H = END_H , &
         M = END_M , &
         S = END_S , &
         calendar=cal,  _RC)

    ! Read CAP Restart File for Current Time
    ! --------------------------------------

    CUR_YY = BEG_YY
    CUR_MM = BEG_MM
    CUR_DD = BEG_DD
    CUR_H  = BEG_H
    CUR_M  = BEG_M
    CUR_S  = BEG_S

    UNIT = GETFILE ( "cap_restart", form="formatted", ALL_PES=.true., _RC )

    rewind(UNIT)
    read(UNIT,100,err=999,end=999) datetime
100 format(i8.8,1x,i6.6)

    _ASSERT(is_valid_date(DATETIME(1)),'Invalid date in cap_restart')
    _ASSERT(is_valid_time(DATETIME(2)),'Invalid time in cap_restart')
    CALL MAPL_UnpackDateTime(DATETIME, CUR_YY, CUR_MM, CUR_DD, CUR_H, CUR_M, CUR_S)

    call MAPL_GetLogger(MAPLOBJ, lgr, _RC)

    call lgr%info('Read CAP restart properly, Current Date =   %i4.4~/%i2.2~/%i2.2', CUR_YY, CUR_MM, CUR_DD)
    call lgr%info('                           Current Time =   %i2.2~:%i2.2~:%i2.2', CUR_H, CUR_M, CUR_S)


999 continue  ! Initialize Current time

    call FREE_FILE (UNIT)

    call ESMF_TimeSet( CurrTime, YY = CUR_YY, &
         MM = CUR_MM, &
         DD = CUR_DD, &
         H = CUR_H , &
         M = CUR_M , &
         S = CUR_S , &
         calendar=cal, _RC)


    ! initialize final stop time
    ! --------------------------

    call ESMF_TimeIntervalSet(  duration, YY = DUR_YY, &
         MM = DUR_MM, &
         D = DUR_DD, &
         H = DUR_H , &
         M = DUR_M , &
         S = DUR_S , &
         startTime = currTime, &
         _RC)

    maxDuration = EndTime - currTime
    if (duration > maxDuration) duration = maxDuration

    stopTime = currTime + duration

    ! initialize model time step
    ! --------------------------

    call ESMF_TimeIntervalSet( timeStep, S=HEARTBEAT_DT, sN=NUM_DT, sD=DEN_DT, _RC )

    nsteps = duration/timestep

    ! Create Clock and set it to one time step before StartTime.
    ! After Initialize has created all alarms, we will advance the
    ! clock to ensure the proper ringing state of all alarms
    !-------------------------------------------------------------

    if (endTime < stopTime) then
       clock = ESMF_ClockCreate( name="ApplClock", timeStep=timeStep, &
            startTime=StartTime, stopTime=EndTime, _RC )
    else
       clock = ESMF_ClockCreate( name="ApplClock", timeStep=timeStep, &
            startTime=StartTime, stopTime=StopTime, _RC )
    end if

    call ESMF_ClockSet ( clock, CurrTime=CurrTime, _RC )

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_ClockInit


  subroutine CAP_FINALIZE ( clock,filen, rc )

    type(ESMF_Clock),    intent(in   ) :: clock
    character(len=*),    optional      :: filen
    integer, optional,   intent(  out) :: rc

    integer        :: UNIT
    integer        :: datetime(2)
    integer        :: YY, MM, DD, H, M, S
    integer        :: status
    character(len=ESMF_MAXSTR)            :: filen_

    type(ESMF_Time)     :: CurrentTime

    filen_ = "cap_restart"
    if (present(filen))     filen_ = trim(filen )

    ! Retrieve Current Time for Cap Restart
    ! -------------------------------------

    call ESMF_ClockGet ( clock, currTime=currentTime, _RC )
    call ESMF_TimeGet  ( CurrentTime, YY = YY, &
         MM = MM, &
         DD = DD, &
         H  = H , &
         M  = M , &
         S  = S, _RC )

    CALL MAPL_PackDateTime(DATETIME, YY, MM, DD, H, M, S)

    ! Write CAP Restart File and Ending Time for Current Segment
    ! ----------------------------------------------------------

    if( MAPL_AM_I_ROOT() ) then
       UNIT = GETFILE( filen_, form="formatted" )
       write(unit,100) datetime
100    format(i8.8,1x,i6.6)
       call FREE_FILE (UNIT)
    endif

    _RETURN(ESMF_SUCCESS)
  end subroutine CAP_FINALIZE

  subroutine Perpetual_Clock (this, rc)
     class(MAPL_CapGridComp), intent(inout) :: this
    integer,         intent(out)   :: rc

    type(ESMF_Time)                :: currTime
    type(ESMF_Alarm)               :: PERPETUAL
    type(ESMF_Calendar)            :: cal
    integer                        :: status
    integer                        :: HIST_YY, HIST_MM, HIST_DD, HIST_H, HIST_M, HIST_S
    integer                        :: AGCM_YY, AGCM_MM, AGCM_DD, AGCM_H, AGCM_M, AGCM_S

    type(ESMF_Clock) :: clock
    type(ESMF_Clock) :: clock_HIST
    integer          :: perpetual_year
    integer          :: perpetual_month
    integer          :: perpetual_day
    class(Logger), pointer :: lgr

    clock = this%clock
    clock_hist = this%clock_Hist
    perpetual_year = this%perpetual_year
    perpetual_month = this%perpetual_month
    perpetual_day = this%perpetual_day
    call MAPL_GetLogger(this%gc, lgr, _RC)

    call ESMF_ClockGetAlarm ( clock_HIST, alarmName='PERPETUAL', alarm=PERPETUAL, _RC )
    call ESMF_AlarmRingerOff( PERPETUAL, _RC )

    call ESMF_ClockGet ( clock, currTime=currTime, calendar=cal, _RC )
    call ESMF_TimeGet  ( CurrTime, YY = AGCM_YY, &
         MM = AGCM_MM, &
         DD = AGCM_DD, &
         H  = AGCM_H , &
         M  = AGCM_M , &
         S  = AGCM_S, _RC )

    call ESMF_ClockGet ( clock_HIST, CurrTime=CurrTime, calendar=cal, _RC )
    call ESMF_TimeGet  ( CurrTime, YY = HIST_YY, &
         MM = HIST_MM, &
         DD = HIST_DD, &
         H  = HIST_H , &
         M  = HIST_M , &
         S  = HIST_S, _RC )

    call lgr%debug('Inside PERP M0:   %i4.4~/%i2.2~/%i2.2  Time: %i2.2~/%i2.2~/%i2.2', AGCM_YY,AGCM_MM,AGCM_DD,AGCM_H,AGCM_M,AGCM_S)
    call lgr%debug('Inside PERP H0:   %i4.4~/%i2.2~/%i2.2  Time: %i2.2~/%i2.2~/%i2.2', HIST_YY,HIST_MM,HIST_DD,HIST_H,HIST_M,HIST_S)

    if( (PERPETUAL_YEAR  /= -999)  .and. &
         (PERPETUAL_MONTH == -999)  .and. &
         (PERPETUAL_DAY   == -999) ) then
       AGCM_YY  = PERPETUAL_YEAR
    endif

    if( (PERPETUAL_YEAR  /= -999)  .and. &
         (PERPETUAL_MONTH /= -999)  .and. &
         (PERPETUAL_DAY   == -999) ) then
       AGCM_YY  = PERPETUAL_YEAR
       AGCM_MM  = PERPETUAL_MONTH
       if( HIST_MM /= PERPETUAL_MONTH ) then
          HIST_MM  = PERPETUAL_MONTH
          if( PERPETUAL_MONTH /= 12) HIST_YY  = HIST_YY + 1
          call ESMF_AlarmRingerOn( PERPETUAL, _RC )
       endif

       call lgr%debug('Inside PERP M0:   %i4.4~/%i2.2~/%i2.2  Time: %i2.2~/%i2.2~/%i2.2', AGCM_YY,AGCM_MM,AGCM_DD,AGCM_H,AGCM_M,AGCM_S)
       call lgr%debug('Inside PERP H0:   %i4.4~/%i2.2~/%i2.2  Time: %i2.2~/%i2.2~/%i2.2', HIST_YY,HIST_MM,HIST_DD,HIST_H,HIST_M,HIST_S)

    endif

    if( (PERPETUAL_YEAR  == -999)  .and. &
         (PERPETUAL_MONTH /= -999)  .and. &
         (PERPETUAL_DAY   == -999) ) then
       AGCM_MM  = PERPETUAL_MONTH
       if( HIST_MM /= PERPETUAL_MONTH ) then
          HIST_MM  = PERPETUAL_MONTH
          if( PERPETUAL_MONTH /= 12) HIST_YY  = HIST_YY + 1
          AGCM_YY  = HIST_YY
          call ESMF_AlarmRingerOn( PERPETUAL, _RC )
       endif
    endif

    if( (PERPETUAL_YEAR  /= -999)  .and. &
         (PERPETUAL_MONTH /= -999)  .and. &
         (PERPETUAL_DAY   /= -999) ) then
       AGCM_YY  = PERPETUAL_YEAR
       AGCM_MM  = PERPETUAL_MONTH
       AGCM_DD  = PERPETUAL_DAY
       if( HIST_MM /= PERPETUAL_MONTH ) then
          HIST_MM  = PERPETUAL_MONTH
          if( PERPETUAL_MONTH /= 12) HIST_YY  = HIST_YY + 1
          call ESMF_AlarmRingerOn( PERPETUAL, _RC )
       endif
    endif

    call ESMF_TimeSet( CurrTime, YY = AGCM_YY, &
         MM = AGCM_MM, &
         DD = AGCM_DD, &
         H = AGCM_H , &
         M = AGCM_M , &
         S = AGCM_S , &
         calendar=cal,  _RC)
    call ESMFL_ClockSet ( clock, CurrTime=CurrTime, _RC )

    call ESMF_TimeSet( CurrTime, YY = HIST_YY, &
         MM = HIST_MM, &
         DD = HIST_DD, &
         H = HIST_H , &
         M = HIST_M , &
         S = HIST_S , &
         calendar=cal,  _RC)
    call ESMFL_ClockSet ( clock_HIST, CurrTime=CurrTime, _RC )

    _RETURN(ESMF_SUCCESS)
  end subroutine Perpetual_Clock

  subroutine ESMFL_ClockSet(clock, currTime, rc)
    ! Args
    type (ESMF_Clock)                :: clock
    type (ESMF_Time),  intent(IN   ) :: currTime
    integer, optional, intent(  OUT) :: rc

    ! ErrLog vars
    integer                                :: status

    ! Local Vars
    type(ESMF_Time)                        :: targetTime
    type(ESMF_Time)                        :: cTime
    type(ESMF_TimeInterval)                :: zero
    type(ESMF_TimeInterval)                :: delt
    type(ESMF_Time)                        :: ringTime
    type(ESMF_TimeInterval)                :: ringInterval
    type(ESMF_Alarm), allocatable          :: AlarmList(:)
    logical                                :: ringing
    integer                                :: I
    integer                                :: nalarms


    targetTime = currTime

    ! get the CurrentTime from the clock
    call ESMF_ClockGet(clock, alarmCount = nalarms, currTime=cTime, _RC)

    delt = targetTime - cTime

    call ESMF_TimeIntervalSet(zero, _RC)

    ! Get the list of current alarms in the clock
    allocate (alarmList(nalarms), _STAT)
    call ESMF_ClockGetAlarmList(clock, alarmListFlag=ESMF_ALARMLIST_ALL, &
         alarmList=alarmList, alarmCount = nalarms, _RC)

    ! Loop over all alarms
    DO I = 1, nalarms
       call ESMF_AlarmGet(alarmList(I), ringTime=ringTime, ringInterval=ringInterval, &
            ringing=ringing, _RC)

       ! skip alarms with zero ringing interval
       if (ringInterval == zero) cycle

       _ASSERT(mod(delt,ringInterval) == zero, 'Time-shift should be a multiple of ringing interval.')
       ringTime=ringTime + delt

       call ESMF_AlarmSet(alarmList(I), ringTime=ringTime, ringing=ringing, _RC)

    END DO

    ! Protection in case we reset the clock outside of StopTime
    call ESMF_ClockStopTimeDisable(clock, _RC)

    call ESMF_ClockSet(clock, currTime=targetTime, _RC)

    ! We do not need the protection anymore
    call ESMF_ClockStopTimeEnable(clock, _RC)

    ! clean-up
    deallocate(alarmList)

    _RETURN(ESMF_SUCCESS)
  end subroutine ESMFL_ClockSet

end module MAPL_CapGridCompMod
