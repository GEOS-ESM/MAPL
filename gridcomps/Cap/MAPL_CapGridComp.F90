#include "MAPL_Generic.h"
#include "unused_dummy.H"

module MAPL_CapGridCompMod
  use ESMF
  use MAPL_ExceptionHandling
  use MAPL_BaseMod
  use MAPL_ConstantsMod
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
  use MAPL_ExtDataGridCompMod, only : ExtData_SetServices => SetServices
  use MAPL_ExtDataGridCompMod, only : T_EXTDATA_STATE, EXTDATA_WRAP
  use MAPL_ConfigMod
  use MAPL_DirPathMod
  use MAPL_KeywordEnforcerMod
  use MAPL_ExternalGridFactoryMod
  use MAPL_GridManagerMod
  use pFIO
  use gFTL_StringVector
  use pflogger, only: logging, Logger
  use MAPL_TimeUtilsMod, only: is_valid_time, is_valid_date

  use iso_fortran_env
  
  implicit none
  private

  character(*), parameter :: internal_cap_name = "InternalCapGridComp"

  public :: MAPL_CapGridComp, MAPL_CapGridCompCreate, MAPL_CapGridComp_Wrapper

  type :: MAPL_CapGridComp
     private
     type (ESMF_GridComp)          :: gc
     procedure(), pointer, nopass  :: root_set_services => null()
     character(len=:), allocatable :: final_file, name, cap_rc_file
     integer :: nsteps, heartbeat_dt, perpetual_year, perpetual_month, perpetual_day
     logical :: amiroot, lperp, started_loop_timer
     integer :: extdata_id, history_id, root_id, printspec
     type(ESMF_Clock) :: clock, clock_hist
     type(ESMF_Config) :: cf_ext, cf_root, cf_hist, config
     type(ESMF_GridComp), allocatable :: gcs(:)
     type(ESMF_State), public :: import_state, export_state
     type(ESMF_State), allocatable :: child_imports(:), child_exports(:)
     type(ESMF_VM) :: vm
     real(kind=real64) :: loop_start_timer
     type(ESMF_Time) :: cap_restart_time
     type(ESMF_Alarm), allocatable :: alarm_list(:)
     type(ESMF_Time),  allocatable :: AlarmRingTime(:)
     logical,          allocatable :: ringingState(:)
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
  end type MAPL_CapGridComp

  type :: MAPL_CapGridComp_Wrapper
     type(MAPL_CapGridComp), pointer :: ptr => null()
  end type MAPL_CapGridComp_Wrapper

  include "mpif.h"

  character(len=*), parameter :: Iam = __FILE__

contains

  
   subroutine MAPL_CapGridCompCreate(cap, root_set_services, cap_rc, name, final_file)
      use mapl_StubComponent
    type(MAPL_CapGridComp), intent(out), target :: cap
    procedure() :: root_set_services
    character(*), intent(in) :: cap_rc, name
    character(len=*), optional, intent(in) :: final_file

    type(MAPL_CapGridComp_Wrapper) :: cap_wrapper
    type(MAPL_MetaComp), pointer :: meta => null()
    integer :: status, rc
    character(*), parameter :: cap_name = "CAP"
    type(StubComponent) :: stub_component
    
    cap%cap_rc_file = cap_rc
    cap%root_set_services => root_set_services
    if (present(final_file)) then
       allocate(cap%final_file, source=final_file)
    end if

    cap%config = ESMF_ConfigCreate(rc=status)
    _VERIFY(status)
    call ESMF_ConfigLoadFile(cap%config,cap%cap_rc_file,rc=STATUS)
    _VERIFY(STATUS)

    allocate(cap%name, source=name)
    cap%gc = ESMF_GridCompCreate(name=cap_name, config=cap%config, rc=status)
    _VERIFY(status)

    meta => null()
    call MAPL_InternalStateCreate(cap%gc, meta, rc=status)
    _VERIFY(status)

    call MAPL_Set(meta, name=cap_name, component=stub_component, rc=status)
    _VERIFY(status)

    cap_wrapper%ptr => cap
    call ESMF_UserCompSetInternalState(cap%gc, internal_cap_name, cap_wrapper, status)
    _VERIFY(status)

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

    type (T_ExtData_STATE), pointer       :: ExtData_internal_state => null()
    type (ExtData_wrap)                   :: wrap


    character(len=ESMF_MAXSTR )           :: timerModeStr
    integer                               :: timerMode
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


    type (MAPL_MetaComp), pointer :: maplobj
    procedure(), pointer :: root_set_services
    type(MAPL_CapGridComp), pointer :: cap
    class(BaseProfiler), pointer :: t_p
    class(Logger), pointer :: lgr
    type(ESMF_Clock) :: cap_clock

    _UNUSED_DUMMY(import_state)
    _UNUSED_DUMMY(export_state)
    _UNUSED_DUMMY(clock)

    cap => get_CapGridComp_from_gc(gc)
    call MAPL_InternalStateRetrieve(gc, maplobj, rc=status)
    _VERIFY(status)

    t_p => get_global_time_profiler()

    call ESMF_GridCompGet(gc, vm = cap%vm, rc = status)
    _VERIFY(status)
    call ESMF_VMGet(cap%vm, petcount = NPES, mpiCommunicator = comm, rc = status)
    _VERIFY(status)

    AmIRoot_ = MAPL_Am_I_Root(cap%vm)

    call MAPL_GetNodeInfo(comm = comm, rc = status)
    _VERIFY(STATUS)

    AmIRoot_ = MAPL_Am_I_Root(cap%vm)

    cap%AmIRoot = AmIRoot_

    !  CAP's MAPL MetaComp
    !---------------------

    ! Note the call to GetLogger must be _after_ the call to MAPL_Set().
    ! That call establishes the name of this component which is used in
    ! retrieving this component's logger.
    call MAPL_GetLogger(gc, lgr, rc=status)
    _VERIFY(status)

    ! Check if user wants to use node shared memory (default is no)
    !--------------------------------------------------------------
    call MAPL_GetResource(MAPLOBJ, useShmem,  label = 'USE_SHMEM:',  default = 0, rc = status)
    if (useShmem /= 0) then
       call MAPL_InitializeShmem (rc = status)
       _VERIFY(status)
    end if

    ! Check if a valid clock was provided externally
    !-----------------------------------------------

    call ESMF_GridCompGet(gc, clockIsPresent=cap_clock_is_present, rc=status)
    _VERIFY(status)

    if (cap_clock_is_present) then
        call ESMF_GridCompGet(gc, clock=cap_clock, rc=status)
        _VERIFY(status)
        call ESMF_ClockValidate(cap_clock, rc=status)
        _VERIFY(status)
        cap%clock = ESMF_ClockCreate(cap_clock, rc=status)
        _VERIFY(status)
        ! NOTE: We assume the MAPL components will only advance by
        ! one time step when driven with an external clock.
        !---------------------------------------------------------
        cap%nsteps = 1
    else
    !  Create Clock. This is a private routine that sets the start and 
    !   end times and the time interval of the clock from the configuration.
    !   The start time is temporarily set to 1 interval before the time in the
    !   configuration. Once the Alarms are set in intialize, the clock will
    !   be advanced to guarantee it and its alarms are in the same state as they
    !   were after the last advance before the previous Finalize.
    !---------------------------------------------------------------------------

        call MAPL_ClockInit(MAPLOBJ, cap%clock, nsteps, rc = status)
        _VERIFY(status)
        cap%nsteps = nsteps
    end if

    call ESMF_ClockGet(cap%clock,currTime=cap%cap_restart_time,rc=status)
    _VERIFY(status)

    cap%clock_hist = ESMF_ClockCreate(cap%clock, rc = STATUS )  ! Create copy for HISTORY
    _VERIFY(STATUS)

    CoresPerNode = MAPL_CoresPerNodeGet(comm,rc=status)
    _VERIFY(STATUS)

    ! We check resource for CoresPerNode (no longer needed to be in CAP.rc)
    ! If it is set in the resource, we issue an warning if the
    ! value does not agree with the detected CoresPerNode

    call ESMF_ConfigGetAttribute(cap%config, value = n, Label = "CoresPerNode:", rc = status)
    if (status == ESMF_SUCCESS) then
       if (CoresPerNode /= n) then
          call lgr%warning("CoresPerNode set (%i0), but does NOT match detected value (%i0)", CoresPerNode, n)
       end if
    end if

    call ESMF_VMGet(cap%vm, petcount=npes, mpicommunicator=comm, rc=status)
    _VERIFY(status)
     _ASSERT(CoresPerNode <= npes, 'something impossible happened')

    if (cap_clock_is_present) then
       call ESMF_ClockGet(cap%clock, timeStep=frequency, rc=status)
       _VERIFY(status)
       call ESMF_TimeIntervalGet(frequency, s=heartbeat_dt, rc=status)
       _VERIFY(status)
    else
       call ESMF_ConfigGetAttribute(cap%config, value = heartbeat_dt, Label = "HEARTBEAT_DT:", rc = status)
       _VERIFY(status)
       call ESMF_TimeIntervalSet(frequency, s = heartbeat_dt, rc = status)
       _VERIFY(status)
    end if

    cap%heartbeat_dt = heartbeat_dt


    perpetual = ESMF_AlarmCreate(clock = cap%clock_hist, name = 'PERPETUAL', ringinterval = frequency, sticky = .false., rc = status)
    _VERIFY(status)
    call ESMF_AlarmRingerOff(perpetual, rc = status)
    _VERIFY(status)

    ! Set CLOCK for AGCM if not externally provided
    ! ---------------------------------------------

    if (.not.cap_clock_is_present) then
       call MAPL_GetResource(MAPLOBJ, cap%perpetual_year, label='PERPETUAL_YEAR:',  default = -999, rc = status)
       _VERIFY(status)
       call MAPL_GetResource(MAPLOBJ, cap%perpetual_month, label='PERPETUAL_MONTH:', default = -999, rc = status)
       _VERIFY(status)
       call MAPL_GetResource(MAPLOBJ, cap%perpetual_day, label='PERPETUAL_DAY:',   default = -999, rc = status)
       _VERIFY(status)

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

          call Perpetual_Clock(cap, rc=status)
          _VERIFY(status)
       endif
    endif

    !  Get configurable info to create HIST 
    !  and the ROOT of the computational hierarchy
    !---------------------------------------------

    !BOR

    ! !RESOURCE_ITEM: string :: Name of ROOT's config file
    call MAPL_GetResource(MAPLOBJ, ROOT_CF, "ROOT_CF:", default = "ROOT.rc", rc = status) 
    _VERIFY(status)

    ! !RESOURCE_ITEM: string :: Name to assign to the ROOT component
    call MAPL_GetResource(MAPLOBJ, ROOT_NAME, "ROOT_NAME:", default = "ROOT", rc = status) 
    _VERIFY(status)

    ! !RESOURCE_ITEM: string :: Name of HISTORY's config file 
    call MAPL_GetResource(MAPLOBJ, HIST_CF, "HIST_CF:", default = "HIST.rc", rc = status) 
    _VERIFY(status)

    ! !RESOURCE_ITEM: string :: Name of ExtData's config file
    call MAPL_GetResource(MAPLOBJ, EXTDATA_CF, "EXTDATA_CF:", default = 'ExtData.rc', rc = status)
    _VERIFY(status)

    ! !RESOURCE_ITEM: string :: Control Timers 
    call MAPL_GetResource(MAPLOBJ, enableTimers, "MAPL_ENABLE_TIMERS:", default = 'NO', rc = status)
    _VERIFY(status)

    ! !RESOURCE_ITEM: string :: Control Memory Diagnostic Utility 
    call MAPL_GetResource(MAPLOBJ, enableMemUtils, "MAPL_ENABLE_MEMUTILS:", default='NO', rc = status)
    _VERIFY(status)
    call MAPL_GetResource(MAPLOBJ, MemUtilsMode, "MAPL_MEMUTILS_MODE:", default = MAPL_MemUtilsModeBase, rc = status)
    _VERIFY(status)
    !EOR
    enableTimers = ESMF_UtilStringUpperCase(enableTimers, rc = status)
    _VERIFY(status)

    if (enableTimers /= 'YES') then
       call MAPL_ProfDisable(rc = status)
       _VERIFY(status)
    else
       call MAPL_GetResource(MAPLOBJ, timerModeStr, "MAPL_TIMER_MODE:", &
            default='MINMAX', RC=STATUS )
       _VERIFY(STATUS)

       timerModeStr = ESMF_UtilStringUpperCase(timerModeStr, rc=STATUS)
       _VERIFY(STATUS) 

       TestTimerMode: select case(timerModeStr)
       case("OLD")
          timerMode = MAPL_TimerModeOld      ! this has barriers
       case("ROOTONLY")
          timerMode = MAPL_TimerModeRootOnly ! this is the fastest
       case("MAX")
          timerMode = MAPL_TimerModeMax      ! this is the default
       case("MINMAX")
          timerMode = MAPL_TimerModeMinMax      ! this is the default
       case default
          _FAIL('Unsupported option for timerModeStr: '//trim(timerModeStr))
       end select TestTimerMode
       call MAPL_TimerModeSet(timerMode, RC=status)
       _VERIFY(status)
    end if
    cap%started_loop_timer=.false.

    enableMemUtils = ESMF_UtilStringUpperCase(enableMemUtils, rc=STATUS)
    _VERIFY(STATUS)

    if (enableMemUtils /= 'YES') then
       call MAPL_MemUtilsDisable( rc=STATUS )
       _VERIFY(STATUS)
    else
       call MAPL_MemUtilsInit( mode=MemUtilsMode, rc=STATUS )
       _VERIFY(STATUS)
    end if

    call MAPL_GetResource( MAPLOBJ, cap%printSpec, label='PRINTSPEC:', default = 0, rc=STATUS )
    _VERIFY(STATUS)

    call dirpaths%append(".",rc=status)
    _VERIFY(status)
    call ESMF_ConfigFindLabel(cap%config,Label='USER_DIRPATH:',isPresent=foundPath,rc=status)
    if (foundPath) then
       tend=.false.
       do while (.not.tend)
          call ESMF_ConfigGetAttribute(cap%config,value=user_dirpath,default='',rc=status)
          if (tempstring /= '') then
             call dirpaths%append(user_dirpath,rc=status)
             _VERIFY(status)
          end if
          call ESMF_ConfigNextLine(cap%config,tableEnd=tend,rc=status)
          _VERIFY(STATUS)
       enddo
    end if

    ! Handle RUN_DT in ROOT_CF
    !-------------------------

    cap%cf_root = ESMF_ConfigCreate(rc=STATUS )
    _VERIFY(STATUS)
    call ESMF_ConfigLoadFile(cap%cf_root, ROOT_CF, rc=STATUS )
    _VERIFY(STATUS)

    call ESMF_ConfigGetAttribute(cap%cf_root, value=RUN_DT, Label="RUN_DT:", rc=status)
    if (STATUS == ESMF_SUCCESS) then
       if (heartbeat_dt /= run_dt) then
          call lgr%error('inconsistent values of HEARTBEAT_DT (%g0) and root RUN_DT (%g0)', heartbeat_dt, run_dt)
          _FAIL('inconsistent values of HEARTBEAT_DT and RUN_DT')
       end if
    else
       call MAPL_ConfigSetAttribute(cap%cf_root, value=heartbeat_dt, Label="RUN_DT:", rc=status)
       _VERIFY(STATUS)
    endif

    ! Add EXPID and EXPDSC from HISTORY.rc to AGCM.rc
    !------------------------------------------------
    cap%cf_hist = ESMF_ConfigCreate(rc=STATUS )
    _VERIFY(STATUS)
    call ESMF_ConfigLoadFile(cap%cf_hist, HIST_CF, rc=STATUS )
    _VERIFY(STATUS)

    call MAPL_ConfigSetAttribute(cap%cf_hist, value=HIST_CF, Label="HIST_CF:", rc=status)
    _VERIFY(STATUS)

    call ESMF_ConfigGetAttribute(cap%cf_hist, value=EXPID,  Label="EXPID:",  rc=status)
    _VERIFY(STATUS)
    call ESMF_ConfigGetAttribute(cap%cf_hist, value=EXPDSC, Label="EXPDSC:", rc=status)
    _VERIFY(STATUS)

    call MAPL_ConfigSetAttribute(cap%cf_hist, value=heartbeat_dt, Label="RUN_DT:", rc=status)
    _VERIFY(STATUS)

    call MAPL_ConfigSetAttribute(cap%cf_root, value=EXPID,  Label="EXPID:",  rc=status)
    _VERIFY(STATUS)
    call MAPL_ConfigSetAttribute(cap%cf_root, value=EXPDSC, Label="EXPDSC:", rc=status)
    _VERIFY(STATUS)

    call ESMF_ConfigGetAttribute(cap%cf_root, value = NX, Label="NX:", rc=status)
    _VERIFY(STATUS)
    call ESMF_ConfigGetAttribute(cap%cf_root, value = NY, Label="NY:", rc=status)
    _VERIFY(STATUS)
    call MAPL_ConfigSetAttribute(cap%cf_hist, value=NX,  Label="NX:",  rc=status)
    _VERIFY(STATUS)
    call MAPL_ConfigSetAttribute(cap%cf_hist, value=NY,  Label="NY:",  rc=status)
    _VERIFY(STATUS)

    ! Add CoresPerNode from CAP.rc to HISTORY.rc and AGCM.rc
    !-------------------------------------------------------
    call MAPL_ConfigSetAttribute(cap%cf_root, value=CoresPerNode,  Label="CoresPerNode:",  rc=status)
    _VERIFY(STATUS)
    call MAPL_ConfigSetAttribute(cap%cf_hist, value=CoresPerNode,  Label="CoresPerNode:",  rc=status)
    _VERIFY(STATUS)

    ! Add a SINGLE_COLUMN flag in HISTORY.rc based on DYCORE value(from AGCM.rc)
    !---------------------------------------------------------------------------
    call ESMF_ConfigGetAttribute(cap%cf_root, value=DYCORE,  Label="DYCORE:",  rc=status)
    _VERIFY(STATUS)
    if (DYCORE == 'DATMO') then
       snglcol = 1
       call MAPL_ConfigSetAttribute(cap%cf_hist, value=snglcol,  Label="SINGLE_COLUMN:",  rc=status)
       _VERIFY(STATUS)
    end if

    ! Detect if this a regular replay in the AGCM.rc
    ! ----------------------------------------------
    call ESMF_ConfigGetAttribute(cap%cf_root, value=ReplayMode, Label="REPLAY_MODE:", default="NoReplay", rc=status)
    _VERIFY(STATUS)


    ! Register the children with MAPL
    !--------------------------------

    !  Create Root child
    !-------------------
    call MAPL_Set(MAPLOBJ, CF=CAP%CF_ROOT, RC=STATUS)
    _VERIFY(STATUS)

    root_set_services => cap%root_set_services

    call t_p%start('SetService')
    cap%root_id = MAPL_AddChild(MAPLOBJ, name = root_name, SS = root_set_services, rc = status)  
    _VERIFY(status)

    !  Create History child
    !----------------------

    call MAPL_Set(MAPLOBJ, CF=CAP%CF_HIST, RC=STATUS)
    _VERIFY(STATUS)

    cap%history_id = MAPL_AddChild( MAPLOBJ, name = 'HIST', SS = HIST_SetServices, rc = status)  
    _VERIFY(status)


    !  Create ExtData child
    !----------------------
    cap%cf_ext = ESMF_ConfigCreate(rc=STATUS )
    _VERIFY(STATUS)
    call ESMF_ConfigLoadFile(cap%cf_ext, EXTDATA_CF, rc=STATUS )
    _VERIFY(STATUS)

    call ESMF_ConfigGetAttribute(cap%cf_ext, value=RUN_DT, Label="RUN_DT:", rc=status)
    if (STATUS == ESMF_SUCCESS) then
       if (heartbeat_dt /= run_dt) then
          call lgr%error('inconsistent values of HEARTBEAT_DT (%g0) and ExtData RUN_DT (%g0)', heartbeat_dt, run_dt)
          _FAIL('inconsistent values of HEARTBEAT_DT and RUN_DT')
       end if
    else
       call MAPL_ConfigSetAttribute(cap%cf_ext, value=heartbeat_dt, Label="RUN_DT:", rc=status)
       _VERIFY(STATUS)
    endif

    call MAPL_Set(MAPLOBJ, CF=CAP%CF_EXT, RC=STATUS)
    _VERIFY(STATUS)

    cap%extdata_id = MAPL_AddChild (MAPLOBJ, name = 'EXTDATA', SS = ExtData_SetServices, rc = status)
    _VERIFY(status)
    call t_p%stop('SetService')

    ! Add NX and NY from AGCM.rc to ExtData.rc as well as name of ExtData rc file
    call ESMF_ConfigGetAttribute(cap%cf_root, value = NX, Label="NX:", rc=status)
    _VERIFY(STATUS)
    call ESMF_ConfigGetAttribute(cap%cf_root, value = NY, Label="NY:", rc=status)
    _VERIFY(STATUS)
    call MAPL_ConfigSetAttribute(cap%cf_ext, value=NX,  Label="NX:",  rc=status)
    _VERIFY(STATUS)
    call MAPL_ConfigSetAttribute(cap%cf_ext, value=NY,  Label="NY:",  rc=status)
    _VERIFY(STATUS)
    call MAPL_ConfigSetAttribute(cap%cf_ext, value=EXTDATA_CF,  Label="CF_EXTDATA:",  rc=status)
    _VERIFY(STATUS)

    !  Query MAPL for the the children's for GCS, IMPORTS, EXPORTS
    !-------------------------------------------------------------

    call MAPL_Get(MAPLOBJ, childrens_gridcomps = cap%gcs, &
         childrens_import_states = cap%child_imports, childrens_export_states = cap%child_exports, rc = status)
    _VERIFY(status)


    !  Inject grid to root child if grid has been set externally
    !-----------------------------------------------------------

    call cap%inject_external_grid(__RC__)

    ! Run as usual unless PRINTSPEC> 0 as set in CAP.rc. If set then
    ! model will not run completely and instead it will simply run MAPL_SetServices
    ! and print out the IM/EX specs. This step uses MAPL_StatePrintSpecCSV found
    ! in MAPL_Generic.F90.


    if (cap%printSpec>0) then
       call MAPL_StatePrintSpecCSV(cap%gcs(cap%root_id), cap%printspec, rc = status)
       _VERIFY(status)
       call ESMF_VMBarrier(cap%vm, rc = status)
       _VERIFY(status)
    else
       !  Initialize the Computational Hierarchy
       !----------------------------------------

       call t_p%start('Initialize')
       call ESMF_GridCompInitialize(cap%gcs(cap%root_id), importState = cap%child_imports(cap%root_id), &
            exportState = cap%child_exports(cap%root_id), clock = cap%clock, userRC = status)
       _VERIFY(status)

       call cap%initialize_history(rc=status)
       _VERIFY(status)

       call cap%initialize_extdata(rc=status)
       _VERIFY(status)

       ! Finally check is this is a regular replay
       ! If so stuff gc and input state for ExtData in GCM internal state
       ! -----------------------------------------------------------------
       if (trim(replayMode)=="Regular") then
          call MAPL_GCGet(CAP%GCS(cap%root_id),"GCM",gcmGC,rc=status)
          _VERIFY(STATUS)
          call ESMF_GridCompGet(gcmGC,vm=gcmVM,rc=status)
          _VERIFY(STATUS)
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
    integer*8, pointer             :: LSADDR(:) => null()

    if (present(rc)) rc = ESMF_SUCCESS
    ! All the EXPORTS of the Hierachy are made IMPORTS of History
    !------------------------------------------------------------
    call ESMF_StateAdd(cap%child_imports(cap%history_id), [cap%child_exports(cap%root_id)], rc = status)
    _VERIFY(STATUS)

    allocate(lswrap%ptr, stat = status)
    _VERIFY(STATUS)
    call ESMF_UserCompSetInternalState(cap%gcs(cap%history_id), 'MAPL_LocStreamList', &
         lswrap, STATUS)
    _VERIFY(STATUS)
    call MAPL_GetAllExchangeGrids(CAP%GCS(cap%root_id), LSADDR, RC=STATUS)
    _VERIFY(STATUS)
    lswrap%ptr%LSADDR_PTR => LSADDR

    ! Initialize the History
    !------------------------

    call ESMF_GridCompInitialize (CAP%GCS(cap%history_id), importState=CAP%CHILD_IMPORTS(cap%history_id), &
         exportState=CAP%CHILD_EXPORTS(cap%history_id), clock=CAP%CLOCK_HIST, userRC=STATUS )
    _VERIFY(STATUS)

    _RETURN(ESMF_SUCCESS)
  end subroutine initialize_history


  subroutine initialize_extdata(cap , rc)
    class(MAPL_CapGridComp), intent(inout) :: cap
    integer, optional, intent(out) :: rc
    integer :: item_count, status
    type (ESMF_StateItem_Flag), pointer   :: item_types(:)
    character(len=ESMF_MAXSTR ), pointer  :: item_names(:)
    type(ESMF_Field) :: field
    type(ESMF_FieldBundle) :: bundle
    type(StringVector) :: cap_imports_vec, cap_exports_vec
    type(StringVectorIterator) :: iter
    integer :: i
    type(ESMF_State) :: state, root_imports, component_state
    character(len=:), allocatable :: component_name, field_name

    ! Prepare EXPORTS for ExtData
    ! ---------------------------
    cap_imports_vec = get_vec_from_config(cap%config, "CAP_IMPORTS")
    cap_exports_vec = get_vec_from_config(cap%config, "CAP_EXPORTS")

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

          call ESMF_StateGet(component_state, trim(field_name), field, rc = status)
          _VERIFY(status)

          call MAPL_StateAdd(cap%export_state, field, rc = status)
          _VERIFY(status)

          call iter%next()
       end do
    end if


    call ESMF_StateGet(cap%child_imports(cap%root_id), itemcount = item_count, rc = status)
    _VERIFY(status)
    allocate(item_names(item_count), stat = status)
    _VERIFY(status)
    allocate(item_types(item_count), stat = status)
    _VERIFY(status)

    call ESMF_StateGet(cap%child_imports(cap%root_id), itemnamelist = item_names, &
         itemtypelist = item_types, rc = status)
    _VERIFY(status)

    root_imports = cap%child_imports(cap%root_id)
    do i = 1, item_count
       if (vector_contains_str(cap_imports_vec, item_names(i))) then
          state = cap%import_state
       else
          state = cap%child_exports(cap%extdata_id)
       end if

       if (item_types(i) == ESMF_StateItem_Field) then
          call ESMF_StateGet(root_imports, item_names(i), field, rc = status)
          _VERIFY(status)
          call MAPL_StateAdd(state, field, rc = status)
          _VERIFY(status)
       else if (item_types(i) == ESMF_StateItem_FieldBundle) then
          call ESMF_StateGet(root_imports, item_names(i), bundle, rc = status)
          _VERIFY(status)
          call MAPL_StateAdd(state, bundle, rc = status)
          _VERIFY(status)
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

    integer :: status
    class (BaseProfiler), pointer :: t_p

    _UNUSED_DUMMY(import)
    _UNUSED_DUMMY(export)
    _UNUSED_DUMMY(clock)

    t_p => get_global_time_profiler()
    call t_p%start('Run')

    call run_MAPL_GridComp(gc, rc=status)
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
    
    cap => get_CapGridComp_from_gc(gc)
    call MAPL_GetObjectFromGC(gc, maplobj, rc=status)
    _VERIFY(status)

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


       call CAP_Finalize(CAP%CLOCK_HIST, "cap_restart", rc=STATUS)
       _VERIFY(status)

       call ESMF_ConfigDestroy(cap%cf_ext, rc = status)
       _VERIFY(status)
       call ESMF_ConfigDestroy(cap%cf_hist, rc = status)
       _VERIFY(status)
       call ESMF_ConfigDestroy(cap%cf_root, rc = status)
       _VERIFY(status)
       call ESMF_ConfigDestroy(cap%config, rc = status)
       _VERIFY(status)

       call MAPL_FinalizeShmem(rc = status)
       _VERIFY(STATUS)

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

    integer :: status

    call ESMF_GridCompSetEntryPoint(gc, ESMF_METHOD_INITIALIZE, userRoutine = initialize_gc, rc = status)
    _VERIFY(status)
    call ESMF_GridCompSetEntryPoint(gc, ESMF_METHOD_RUN, userRoutine = run_gc, rc = status)
    _VERIFY(status)
    call ESMF_GridCompSetEntryPoint(gc, ESMF_METHOD_FINALIZE, userRoutine = finalize_gc, rc = status)
    _VERIFY(status)
    _RETURN(ESMF_SUCCESS)

  end subroutine set_services_gc


  subroutine set_services(this, rc)
    class(MAPL_CapGridComp), intent(inout) :: this
    integer, optional, intent(out) :: rc
    integer :: status

    call ESMF_GridCompSetServices(this%gc, set_services_gc, rc = status)
    _VERIFY(status)
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


  subroutine run(this, rc)
    class(MAPL_CapGridComp), intent(inout) :: this
    integer, optional, intent(out) :: rc

    integer :: status
    integer :: userrc

    call ESMF_GridCompRun(this%gc, userrc=userrc, rc=status)
    _VERIFY(status)
    _VERIFY(userrc)
    _RETURN(ESMF_SUCCESS)

  end subroutine run

  subroutine finalize(this, rc)
    class(MAPL_CapGridComp), intent(inout) :: this
    integer, optional, intent(out) :: rc
    
    integer :: status    
    
    call ESMF_GridCompFinalize(this%gc, rc = status)
    _VERIFY(status)
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
    call ESMF_ClockGet(this%clock,currTime=current_time,rc=status)
    _VERIFY(status)

    _RETURN(ESMF_SUCCESS)

  end function get_current_time


  function get_CapGridComp_from_gc(gc) result(cap)
    type(ESMF_GridComp), intent(inout) :: gc
    type(MAPL_CapGridComp), pointer :: cap
    type(MAPL_CapGridComp_Wrapper) :: cap_wrapper
    integer :: rc
    call ESMF_UserCompGetInternalState(gc, internal_cap_name, cap_wrapper, rc)
    cap => cap_wrapper%ptr
  end function get_CapGridComp_from_gc

  
  
  function get_vec_from_config(config, key) result(vec)
    type(ESMF_Config), intent(inout) :: config
    character(len=*), intent(in) :: key
    logical :: present
    integer :: status, rc
    character(len=ESMF_MAXSTR) :: cap_import
    type(StringVector) :: vec
    
    call ESMF_ConfigFindLabel(config, key//":", isPresent = present, rc = status)
    _VERIFY(status)

    cap_import = ""
    if (present) then
       
       do while(trim(cap_import) /= "::")
          call ESMF_ConfigNextLine(config, rc = status)
          _VERIFY(status)
          call ESMF_ConfigGetAttribute(config, cap_import, rc = status)
          _VERIFY(status)
          if (trim(cap_import) /= "::") call vec%push_back(trim(cap_import))
       end do
    end if
       
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

  
  subroutine run_MAPL_GridComp(gc, rc)
    type (ESMF_Gridcomp) :: gc
    integer, optional, intent(out) :: rc
    
    integer :: n, status
    logical :: done

    type(MAPL_CapGridComp), pointer :: cap
    type (MAPL_MetaComp), pointer :: MAPLOBJ
    procedure(), pointer :: root_set_services

    cap => get_CapGridComp_from_gc(gc)
    call MAPL_GetObjectFromGC(gc, maplobj, rc=status)
    _VERIFY(status)

    if (.not. cap%printspec > 0) then

       ! Time Loop starts by checking for Segment Ending Time
       !-----------------------------------------------------
       call ESMF_VMBarrier(cap%vm,rc=status)
       _VERIFY(status)
       cap%loop_start_timer = MPI_WTime(status)
       cap%started_loop_timer = .true.
       TIME_LOOP: do n = 1, cap%nsteps

          call MAPL_MemUtilsWrite(cap%vm, 'MAPL_Cap:TimeLoop', rc = status)
          _VERIFY(status)

          if (.not.cap%lperp) then
             done = ESMF_ClockIsStopTime(cap%clock_hist, rc = status)
             _VERIFY(status)
             if (done) exit
          endif

          call cap%step(status)
          _VERIFY(status)

          ! Reset loop average timer to get a better
          ! estimate of true run time left by ignoring
          ! initialization costs in the averageing.
          !-------------------------------------------
          if (n == 1) then
             call ESMF_VMBarrier(cap%vm,rc=status)
             _VERIFY(status)
             cap%loop_start_timer = MPI_WTime(status)
          endif

       enddo TIME_LOOP ! end of time loop

    end if

    _RETURN(ESMF_SUCCESS)
  end subroutine run_MAPL_GridComp


  subroutine step(this, rc)
    class(MAPL_CapGridComp), intent(inout) :: this
    integer, intent(out) :: rc
    integer :: AGCM_YY, AGCM_MM, AGCM_DD, AGCM_H, AGCM_M, AGCM_S
    integer :: status
! For Throughout/execution estimates
      integer           :: HRS_R, MIN_R, SEC_R
      real(kind=REAL64) :: START_RUN_TIMER,END_RUN_TIMER,START_TIMER,END_TIMER
      real(kind=REAL64) :: TIME_REMAINING
      real(kind=REAL64) ::  LOOP_THROUGHPUT=0.0_REAL64
      real(kind=REAL64) ::  INST_THROUGHPUT=0.0_REAL64
      real(kind=REAL64) ::   RUN_THROUGHPUT=0.0_REAL64
      real              :: mem_total, mem_commit, mem_committed_percent
      real              :: mem_used, mem_used_percent
    
    type(ESMF_Time) :: currTime
    type(ESMF_TimeInterval) :: delt
    real(kind=REAL64) :: delt64
    integer :: n

    call ESMF_GridCompGet(this%gc, vm = this%vm)

    if (.not.this%started_loop_timer) then
       this%loop_start_timer = MPI_WTime(status)
       this%started_loop_timer=.true.
    end if
    start_timer = MPI_Wtime(status)
    ! Run the ExtData Component
    ! --------------------------

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

    ! Run the Gridded Component
    ! --------------------------
    start_run_timer = MPI_WTime(status)
    call ESMF_GridCompRun(this%gcs(this%root_id), importstate = this%child_imports(this%root_id), &
         exportstate = this%child_exports(this%root_id), &
         clock = this%clock, userrc = status)
    _VERIFY(status)
    call ESMF_VMBarrier(this%vm,rc=status)
    _VERIFY(status)
    end_run_timer = MPI_WTime(status)

    ! Synchronize for Next TimeStep
    ! -----------------------------

    call ESMF_VMBarrier(this%vm, rc = status)
    _VERIFY(STATUS)

    ! Advance the Clock before running History and Record
    ! ---------------------------------------------------
    call ESMF_ClockAdvance(this%clock, rc = status)
    _VERIFY(STATUS)
    call ESMF_ClockAdvance(this%clock_hist, rc = status)
    _VERIFY(STATUS)

    ! Update Perpetual Clock
    ! ----------------------

    if (this%lperp) then
       call Perpetual_Clock(this, status)
       _VERIFY(status)
    end if

    call ESMF_ClockGet(this%clock, CurrTime = currTime, rc = status)
    _VERIFY(status)
    call ESMF_TimeGet(CurrTime, YY = AGCM_YY, &
         MM = AGCM_MM, &
         DD = AGCM_DD, &
         H  = AGCM_H , &
         M  = AGCM_M , &
         S  = AGCM_S, rc=status)
    _VERIFY(status)
   delt=currTime-this%cap_restart_time

    call ESMF_GridCompRun(this%gcs(this%history_id), importstate=this%child_imports(this%history_id), &
         exportstate = this%child_exports(this%history_id), &
         clock = this%clock_hist, userrc = status)
    _VERIFY(status)

   ! Estimate throughput times
   ! ---------------------------
       
       ! Call system clock to estimate throughput simulated Days/Day
         call ESMF_VMBarrier( this%vm, RC=STATUS )
         _VERIFY(STATUS)
         END_TIMER = MPI_Wtime(status)
         call ESMF_TimeIntervalGet(delt,s_r8=delt64,rc=status)
         _VERIFY(status)
         n=delt64/real(this%heartbeat_dt,kind=real64)
       ! GridCompRun Timer [Inst]
         RUN_THROUGHPUT = REAL(  this%HEARTBEAT_DT,kind=REAL64)/(END_RUN_TIMER-START_RUN_TIMER)
       ! Time loop throughput [Inst]
         INST_THROUGHPUT = REAL(  this%HEARTBEAT_DT,kind=REAL64)/(END_TIMER-START_TIMER)
       ! Time loop throughput [Avg]
         LOOP_THROUGHPUT = REAL(n*this%HEARTBEAT_DT,kind=REAL64)/(END_TIMER- this%loop_start_timer)
       ! Estimate time remaining (seconds)
         TIME_REMAINING = REAL((this%nsteps-n)*this%HEARTBEAT_DT,kind=REAL64)/LOOP_THROUGHPUT
         HRS_R = FLOOR(TIME_REMAINING/3600.0)
         MIN_R = FLOOR(TIME_REMAINING/60.0  -   60.0*HRS_R)
         SEC_R = FLOOR(TIME_REMAINING       - 3600.0*HRS_R - 60.0*MIN_R)
       ! Reset Inst timer
         START_TIMER=END_TIMER
       ! Get percent of used memory
         call MAPL_MemUsed ( mem_total, mem_used, mem_used_percent, RC=STATUS )
         _VERIFY(STATUS)
       ! Get percent of committed memory
         call MAPL_MemCommited ( mem_total, mem_commit, mem_committed_percent, RC=STATUS )
         _VERIFY(STATUS)

         if( mapl_am_I_Root(this%vm) ) write(6,1000) AGCM_YY,AGCM_MM,AGCM_DD,AGCM_H,AGCM_M,AGCM_S,&
                                      LOOP_THROUGHPUT,INST_THROUGHPUT,RUN_THROUGHPUT,HRS_R,MIN_R,SEC_R,&
                                      mem_committed_percent,mem_used_percent
    1000 format(1x,'AGCM Date: ',i4.4,'/',i2.2,'/',i2.2,2x,'Time: ',i2.2,':',i2.2,':',i2.2, &
                2x,'Throughput(days/day)[Avg Tot Run]: ',f6.1,1x,f6.1,1x,f6.1,2x,'TimeRemaining(Est) ',i3.3,':'i2.2,':',i2.2,2x, &
                f5.1,'% : ',f5.1,'% Mem Comm:Used')
    _RETURN(ESMF_SUCCESS)
  end subroutine step

  subroutine record_state(this, rc)
    class(MAPL_CapGridComp), intent(inout) :: this
    integer, intent(out) :: rc
    integer :: status
    type(MAPL_MetaComp), pointer :: maplobj

    integer :: nalarms,i

    call MAPL_GetObjectFromGC(this%gcs(this%root_id),maplobj,rc=status)
    _VERIFY(status)
    call MAPL_GenericStateSave(this%gcs(this%root_id),this%child_imports(this%root_id), &
           this%child_exports(this%root_id),this%clock,rc=status)

    call ESMF_ClockGet(this%clock,alarmCount=nalarms,rc=status)
    _VERIFY(status)

    allocate(this%alarm_list(nalarms),this%ringingState(nalarms),this%alarmRingTime(nalarms),stat=status)
    _VERIFY(status)
    call ESMF_ClockGetAlarmList(this%clock, alarmListFlag=ESMF_ALARMLIST_ALL, &
         alarmList=this%alarm_list, rc=status)
    _VERIFY(status)
    do i = 1, nalarms
       call ESMF_AlarmGet(this%alarm_list(I), ringTime=this%alarmRingTime(I), ringing=this%ringingState(I), rc=status)
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
             this%child_exports(this%root_id),this%clock,rc=status)
    _VERIFY(status)
    DO I = 1, size(this%alarm_list)
       call ESMF_AlarmSet(this%alarm_list(I), ringTime=this%alarmRingTime(I), ringing=this%ringingState(I), rc=status)
       _VERIFY(STATUS)
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
         state_name,state,rc=status)
    _VERIFY(status)
    call ESMF_StateGet(state,trim(field_name),field,rc=status)
    _VERIFY(status)
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

    call MAPL_InternalESMFStateGet(this%gcs(this%root_id),state_name,state,rc=status)
    _VERIFY(status)
    call ESMF_StateGet(state,trim(field_name),field,rc=status)
    _VERIFY(status)
    _RETURN(_SUCCESS)

  end subroutine get_field_from_internal

  subroutine set_grid(this, grid, unusable, lm, rc)
     class(MAPL_CapGridComp),          intent(inout) :: this
     type(ESMF_Grid),                  intent(in   ) :: grid
     class(KeywordEnforcer), optional, intent(in   ) :: unusable
     integer,                optional, intent(in   ) :: lm
     integer,                optional, intent(  out) :: rc

     type(ESMF_Grid)           :: mapl_grid
     type(ExternalGridFactory) :: external_grid_factory
     integer                   :: status

     _UNUSED_DUMMY(unusable)

     external_grid_factory = ExternalGridFactory(grid=grid, lm=lm, __RC__)
     mapl_grid = grid_manager%make_grid(external_grid_factory, __RC__)

     call ESMF_GridCompSet(this%gc, grid=mapl_grid, __RC__)

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

     call ESMF_GridCompGet(this%gc, gridIsPresent=cap_grid_is_present, __RC__)

     if (cap_grid_is_present) then
        call ESMF_GridCompGet(this%gc, grid=cap_grid, __RC__)
        call ESMF_GridValidate(cap_grid, __RC__)

        call ESMF_GridCompGet(this%gcs(this%root_id), gridIsPresent=root_grid_is_present, __RC__)

        if (root_grid_is_present) then
           call ESMF_GridCompGet(this%gcs(this%root_id), grid=root_grid, __RC__)
           call ESMF_GridValidate(root_grid, __RC__)

           grid_match = ESMF_GridMatch(cap_grid, root_grid, __RC__)
           _ASSERT(grid_match == ESMF_GRIDMATCH_EXACT, "Attempting to override root grid with non-matching external grid")
        else
            call ESMF_GridCompSet(this%gcs(this%root_id), grid=cap_grid, __RC__)
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

     call ESMF_GridCompSet(this%gc, clock=clock, __RC__)

     _RETURN(_SUCCESS)
  end subroutine set_clock

  subroutine destroy_state(this, rc)
    class(MAPL_CapGridComp), intent(inout) :: this
    integer, intent(out) :: rc
    integer :: status

    call MAPL_DestroyStateSave(this%gcs(this%root_id),rc=status)
    _VERIFY(status)
   
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

    call ESMF_ClockGet(this%clock,currTime=current_time,rc=status)
    _VERIFY(status)
    if (current_time >  time) then
       call ESMF_ClockSet(this%clock,direction=ESMF_DIRECTION_REVERSE,rc=status)
       _VERIFY(status)
       do 
          call ESMF_ClockAdvance(this%clock,rc=status)
          _VERIFY(status)
          call ESMF_ClockGet(this%clock,currTime=ct,rc=status)
          _VERIFY(status)
          if (ct==time) exit
       enddo
       call ESMF_ClockSet(this%clock,direction=ESMF_DIRECTION_FORWARD,rc=status)
       _VERIFY(status)
    end if
      
    call ESMF_ClockGet(this%clock_hist,currTime=current_time,rc=status)
    _VERIFY(status)
    if (current_time >  time) then
       call ESMF_ClockSet(this%clock_hist,direction=ESMF_DIRECTION_REVERSE,rc=status)
       _VERIFY(status)
       do 
          call ESMF_ClockAdvance(this%clock_hist,rc=status)
          _VERIFY(status)
          call ESMF_ClockGet(this%clock_hist,currTime=ct,rc=status)
          _VERIFY(status)
          if (ct==time) exit
       enddo
       call ESMF_ClockSet(this%clock_hist,direction=ESMF_DIRECTION_FORWARD,rc=status)
       _VERIFY(status)
    end if
      
       
    _RETURN(_SUCCESS)
  end subroutine rewind_clock


  ! !IROUTINE: MAPL_ClockInit -- Sets the clock

  ! !INTERFACE: 

  subroutine MAPL_ClockInit ( MAPLOBJ, Clock, nsteps, rc)

    ! !ARGUMENTS:

    type(MAPL_MetaComp), intent(inout) :: MAPLOBJ
    type(ESMF_Clock),    intent(  out) :: Clock
    integer,             intent(  out) :: nsteps
    integer, optional,   intent(  out) :: rc

    !  !DESCRIPTION:

    !   This is a private routine that sets the start and 
    !   end times and the time interval of the application clock from the configuration.
    !   This time interal is the ``heartbeat'' of the application.
    !   The Calendar is set to Gregorian by default. 
    !   The start time is temporarily set to 1 interval before the time in the
    !   configuration. Once the Alarms are set in intialize, the clock will
    !   be advanced to guarantee it and its alarms are in the same state as they
    !   were after the last advance before the previous Finalize.
    !


    type(ESMF_Time)          :: StartTime    ! Initial     Begin  Time of Experiment
    type(ESMF_Time)          :: EndTime      ! Final       Ending Time of Experiment
    type(ESMF_Time)          :: StopTime     ! Final       Ending Time of Experiment
    type(ESMF_Time)          :: CurrTime     ! Current     Current Time of Experiment
    type(ESMF_TimeInterval)  :: timeStep     ! HEARTBEAT
    type(ESMF_TimeInterval)  :: duration
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

    call MAPL_GetResource( MAPLOBJ, datetime, label='BEG_DATE:', rc=STATUS )
    if(STATUS==ESMF_SUCCESS) then
       _ASSERT(is_valid_date(datetime(1)),'Invalid date in BEG_DATE')
       _ASSERT(is_valid_time(datetime(2)),'Invalid time in BEG_DATE')
       CALL MAPL_UnpackDateTime(DATETIME, BEG_YY, BEG_MM, BEG_DD, BEG_H, BEG_M, BEG_S)
    else

       ! !RESOURCE_ITEM: year :: Beginning year (integer)
       call MAPL_GetResource( MAPLOBJ, BEG_YY, label='BEG_YY:', DEFAULT=1, rc=STATUS )
       _VERIFY(STATUS)
       ! !RESOURCE_ITEM: month :: Beginning month (integer 1-12)
       call MAPL_GetResource( MAPLOBJ, BEG_MM, label='BEG_MM:', default=1, rc=STATUS )
       _VERIFY(STATUS)
       ! !RESOURCE_ITEM: day  :: Beginning day of month (integer 1-31)
       call MAPL_GetResource( MAPLOBJ, BEG_DD, label='BEG_DD:', default=1, rc=STATUS )
       _VERIFY(STATUS)
       ! !RESOURCE_ITEM: hour :: Beginning hour of day (integer 0-23)
       call MAPL_GetResource( MAPLOBJ, BEG_H , label='BEG_H:' , default=0, rc=STATUS )
       _VERIFY(STATUS)
       ! !RESOURCE_ITEM: minute :: Beginning minute (integer 0-59)
       call MAPL_GetResource( MAPLOBJ, BEG_M , label='BEG_M:' , default=0, rc=STATUS )
       _VERIFY(STATUS)
       ! !RESOURCE_ITEM: second :: Beginning second (integer 0-59)
       call MAPL_GetResource( MAPLOBJ, BEG_S , label='BEG_S:' , default=0, rc=STATUS )
       _VERIFY(STATUS)
    end if

    call MAPL_GetResource( MAPLOBJ, datetime, label='END_DATE:', rc=STATUS )
    if(STATUS==ESMF_SUCCESS) then
       _ASSERT(is_valid_date(datetime(1)),'Invalid date in END_DATE')
       _ASSERT(is_valid_time(datetime(2)),'Invalid time in END_DATE')
       CALL MAPL_UnpackDateTime(DATETIME, END_YY, END_MM, END_DD, END_H, END_M, END_S)
    else
       ! !RESOURCE_ITEM: year :: Ending year (integer)
       call MAPL_GetResource( MAPLOBJ, END_YY, label='END_YY:', DEFAULT=1, rc=STATUS )
       _VERIFY(STATUS)
       ! !RESOURCE_ITEM: month :: Ending month (integer 1-12)
       call MAPL_GetResource( MAPLOBJ, END_MM, label='END_MM:', default=1, rc=STATUS )
       _VERIFY(STATUS)
       ! !RESOURCE_ITEM: day  :: Ending day of month (integer 1-31)
       call MAPL_GetResource( MAPLOBJ, END_DD, label='END_DD:', default=1, rc=STATUS )
       _VERIFY(STATUS)
       ! !RESOURCE_ITEM: hour :: Ending hour of day (integer 0-23)
       call MAPL_GetResource( MAPLOBJ, END_H , label='END_H:' , default=0, rc=STATUS )
       _VERIFY(STATUS)
       ! !RESOURCE_ITEM: minute :: Ending minute (integer 0-59)
       call MAPL_GetResource( MAPLOBJ, END_M , label='END_M:' , default=0, rc=STATUS )
       _VERIFY(STATUS)
       ! !RESOURCE_ITEM: second :: Ending second (integer 0-59)
       call MAPL_GetResource( MAPLOBJ, END_S , label='END_S:' , default=0, rc=STATUS )
       _VERIFY(STATUS)
    end if

    ! Replace JOB_DURATION with JOB_SGMT as prefered RC parameter
    ! -----------------------------------------------------------
    call MAPL_GetResource( MAPLOBJ, datetime, label='JOB_SGMT:',     rc=STATUS )
    if(STATUS/=ESMF_SUCCESS) then
       call MAPL_GetResource( MAPLOBJ, datetime, label='JOB_DURATION:', rc=STATUS )
    end if

    if(STATUS==ESMF_SUCCESS) then
       CALL MAPL_UnpackDateTime(DATETIME, DUR_YY, DUR_MM, DUR_DD, DUR_H, DUR_M, DUR_S)
    else
       ! !RESOURCE_ITEM: year :: Ending year (integer)
       call MAPL_GetResource( MAPLOBJ, DUR_YY, label='DUR_YY:', DEFAULT=0, rc=STATUS )
       _VERIFY(STATUS)
       ! !RESOURCE_ITEM: month :: Ending month (integer 1-12)
       call MAPL_GetResource( MAPLOBJ, DUR_MM, label='DUR_MM:', default=0, rc=STATUS )
       _VERIFY(STATUS)
       ! !RESOURCE_ITEM: day  :: Ending day of month (integer 1-31)
       call MAPL_GetResource( MAPLOBJ, DUR_DD, label='DUR_DD:', default=1, rc=STATUS )
       _VERIFY(STATUS)
       ! !RESOURCE_ITEM: hour :: Ending hour of day (integer 0-23)
       call MAPL_GetResource( MAPLOBJ, DUR_H , label='DUR_H:' , default=0, rc=STATUS )
       _VERIFY(STATUS)
       ! !RESOURCE_ITEM: minute :: Ending minute (integer 0-59)
       call MAPL_GetResource( MAPLOBJ, DUR_M , label='DUR_M:' , default=0, rc=STATUS )
       _VERIFY(STATUS)
       ! !xRESOURCE_ITEM: second :: Ending second (integer 0-59)
       call MAPL_GetResource( MAPLOBJ, DUR_S , label='DUR_S:' , default=0, rc=STATUS )
       _VERIFY(STATUS)
    end if

    ! !RESOURCE_ITEM: seconds :: Interval of the application clock (the Heartbeat)
    call MAPL_GetResource( MAPLOBJ, HEARTBEAT_DT, label='HEARTBEAT_DT:',            rc=STATUS )
    _VERIFY(STATUS)
    ! !RESOURCE_ITEM: 1 :: numerator of decimal fraction of time step
    call MAPL_GetResource( MAPLOBJ, NUM_DT, label='NUM_DT:', default=0, rc=STATUS )
    _VERIFY(STATUS)
    ! !RESOURCE_ITEM: 1 :: denominator of decimal fraction of time step
    call MAPL_GetResource( MAPLOBJ, DEN_DT, label='DEN_DT:', default=1, rc=STATUS )
    _VERIFY(STATUS)
    ! !RESOURCE_ITEM: string :: Calendar type
    call MAPL_GetResource( MAPLOBJ, calendar, label='CALENDAR:', default="GREGORIAN", rc=STATUS )
    _VERIFY(STATUS)

    !EOR

    _ASSERT(NUM_DT>=0, 'NUM_DT should be >= 0.')
    _ASSERT(DEN_DT> 0, 'DEN_DT should be > 0.')
    _ASSERT(NUM_DT<DEN_DT, 'NUM_DT should be < DEN_DT')
    _ASSERT(HEARTBEAT_DT>=0, 'HEARTBEAT_DT should be >= 0.')

    ! initialize calendar to be Gregorian type
    ! ----------------------------------------

    if    (calendar=="GREGORIAN") then
       cal = ESMF_CalendarCreate( ESMF_CALKIND_GREGORIAN, name="ApplicationCalendar", rc=status )
       _VERIFY(STATUS)
       call ESMF_CalendarSetDefault(ESMF_CALKIND_GREGORIAN, RC=STATUS)
       _VERIFY(STATUS)
    elseif(calendar=="JULIAN"   ) then
       cal = ESMF_CalendarCreate( ESMF_CALKIND_JULIAN, name="ApplicationCalendar", rc=status )
       _VERIFY(STATUS)
       call ESMF_CalendarSetDefault(ESMF_CALKIND_JULIAN, RC=STATUS)
       _VERIFY(STATUS)
    elseif(calendar=="NOLEAP"   ) then
       cal = ESMF_CalendarCreate( ESMF_CALKIND_NOLEAP, name="ApplicationCalendar", rc=status )
       _VERIFY(STATUS)
       call ESMF_CalendarSetDefault(ESMF_CALKIND_NOLEAP, RC=STATUS)
       _VERIFY(STATUS)
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
         calendar=cal,  rc = STATUS  )
    _VERIFY(STATUS)

    call ESMF_TimeSet(   EndTime, YY = END_YY, &
         MM = END_MM, &
         DD = END_DD, &
         H = END_H , &
         M = END_M , &
         S = END_S , &
         calendar=cal,  rc = STATUS  )
    _VERIFY(STATUS)  

    ! Read CAP Restart File for Current Time
    ! --------------------------------------

    CUR_YY = BEG_YY
    CUR_MM = BEG_MM
    CUR_DD = BEG_DD
    CUR_H  = BEG_H
    CUR_M  = BEG_M
    CUR_S  = BEG_S

    UNIT = GETFILE ( "cap_restart", form="formatted", ALL_PES=.true., rc=status )
    _VERIFY(STATUS)

    rewind(UNIT)
    read(UNIT,100,err=999,end=999) datetime
100 format(i8.8,1x,i6.6)

    _ASSERT(is_valid_date(DATETIME(1)),'Invalid date in cap_restart')
    _ASSERT(is_valid_time(DATETIME(2)),'Invalid time in cap_restart')
    CALL MAPL_UnpackDateTime(DATETIME, CUR_YY, CUR_MM, CUR_DD, CUR_H, CUR_M, CUR_S)

    call MAPL_GetLogger(MAPLOBJ, lgr, rc=status)
    _VERIFY(status)

    call lgr%info('Read CAP restart properly, Current Date =   %i4.4~/%i2.2~/%i2.2', CUR_YY, CUR_MM, CUR_DD)
    call lgr%info('                           Current Time =   %i2.2~/%i2.2~/%i2.2', CUR_H, CUR_M, CUR_S)


999 continue  ! Initialize Current time

    call FREE_FILE (UNIT)

    call ESMF_TimeSet( CurrTime, YY = CUR_YY, &
         MM = CUR_MM, &
         DD = CUR_DD, &
         H = CUR_H , &
         M = CUR_M , &
         S = CUR_S , &
         calendar=cal,  rc = STATUS  )
    _VERIFY(STATUS)
   

    ! initialize final stop time
    ! --------------------------

    call ESMF_TimeIntervalSet(  duration, YY = DUR_YY, &
         MM = DUR_MM, &
         D = DUR_DD, &
         H = DUR_H , &
         M = DUR_M , &
         S = DUR_S , &
         startTime = currTime, &
         rc = STATUS  )
    _VERIFY(STATUS)

    stopTime = currTime + duration

    ! initialize model time step
    ! --------------------------

    call ESMF_TimeIntervalSet( timeStep, S=HEARTBEAT_DT, sN=NUM_DT, sD=DEN_DT, rc=STATUS )
    _VERIFY(STATUS)

    nsteps = duration/timestep

    ! Create Clock and set it to one time step before StartTime.
    ! After Initialize has created all alarms, we will advance the
    ! clock to ensure the proper ringing state of all alarms
    !-------------------------------------------------------------

    if (endTime < stopTime) then
       clock = ESMF_ClockCreate( name="ApplClock", timeStep=timeStep, &
            startTime=StartTime, stopTime=EndTime, rc=STATUS )
    else
       clock = ESMF_ClockCreate( name="ApplClock", timeStep=timeStep, &
            startTime=StartTime, stopTime=StopTime, rc=STATUS )
    end if
    _VERIFY(STATUS)

    call ESMF_ClockSet ( clock, CurrTime=CurrTime, rc=status )
    _VERIFY(STATUS)

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

    call ESMF_ClockGet ( clock, currTime=currentTime, rc=status )
    _VERIFY(STATUS)
    call ESMF_TimeGet  ( CurrentTime, YY = YY, &
         MM = MM, &
         DD = DD, &
         H  = H , &
         M  = M , &
         S  = S, rc=status )
    _VERIFY(STATUS)

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
    call MAPL_GetLogger(this%gc, lgr, rc=status)
    _VERIFY(status)

    call ESMF_ClockGetAlarm ( clock_HIST, alarmName='PERPETUAL', alarm=PERPETUAL, rc=status )
    _VERIFY(STATUS)
    call ESMF_AlarmRingerOff( PERPETUAL, rc=status )
    _VERIFY(STATUS)

    call ESMF_ClockGet ( clock, currTime=currTime, calendar=cal, rc=status )
    _VERIFY(STATUS)
    call ESMF_TimeGet  ( CurrTime, YY = AGCM_YY, &
         MM = AGCM_MM, &
         DD = AGCM_DD, &
         H  = AGCM_H , &
         M  = AGCM_M , &
         S  = AGCM_S, rc=status )
    _VERIFY(STATUS)

    call ESMF_ClockGet ( clock_HIST, CurrTime=CurrTime, calendar=cal, rc=status )
    _VERIFY(STATUS)
    call ESMF_TimeGet  ( CurrTime, YY = HIST_YY, &
         MM = HIST_MM, &
         DD = HIST_DD, &
         H  = HIST_H , &
         M  = HIST_M , &
         S  = HIST_S, rc=status )
    _VERIFY(STATUS)

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
          call ESMF_AlarmRingerOn( PERPETUAL, rc=status )
          _VERIFY(STATUS)
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
          call ESMF_AlarmRingerOn( PERPETUAL, rc=status )
          _VERIFY(STATUS)
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
          call ESMF_AlarmRingerOn( PERPETUAL, rc=status )
          _VERIFY(STATUS)
       endif
    endif

    call ESMF_TimeSet( CurrTime, YY = AGCM_YY, &
         MM = AGCM_MM, &
         DD = AGCM_DD, &
         H = AGCM_H , &
         M = AGCM_M , &
         S = AGCM_S , &
         calendar=cal,  rc = STATUS  )
    _VERIFY(STATUS)
    call ESMFL_ClockSet ( clock, CurrTime=CurrTime, rc=status )
    _VERIFY(STATUS)

    call ESMF_TimeSet( CurrTime, YY = HIST_YY, &
         MM = HIST_MM, &
         DD = HIST_DD, &
         H = HIST_H , &
         M = HIST_M , &
         S = HIST_S , &
         calendar=cal,  rc = STATUS  )
    _VERIFY(STATUS)
    call ESMFL_ClockSet ( clock_HIST, CurrTime=CurrTime, rc=status )
    _VERIFY(STATUS)

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
    call ESMF_ClockGet(clock, alarmCount = nalarms, currTime=cTime, rc=status)
    _VERIFY(STATUS)

    delt = targetTime - cTime

    call ESMF_TimeIntervalSet(zero, rc=status)
    _VERIFY(STATUS)

    ! Get the list of current alarms in the clock
    allocate (alarmList(nalarms), stat = status)
    _VERIFY(STATUS)
    call ESMF_ClockGetAlarmList(clock, alarmListFlag=ESMF_ALARMLIST_ALL, &
         alarmList=alarmList, alarmCount = nalarms, rc=status)
    _VERIFY(STATUS)

    ! Loop over all alarms
    DO I = 1, nalarms
       call ESMF_AlarmGet(alarmList(I), ringTime=ringTime, ringInterval=ringInterval, &
            ringing=ringing, rc=status)
       _VERIFY(STATUS)

       ! skip alarms with zero ringing interval
       if (ringInterval == zero) cycle

       _ASSERT(mod(delt,ringInterval) == zero, 'Time-shift should be a multiple of ringing interval.')
       ringTime=ringTime + delt

       call ESMF_AlarmSet(alarmList(I), ringTime=ringTime, ringing=ringing, rc=status)
       _VERIFY(STATUS)

    END DO

    ! Protection in case we reset the clock outside of StopTime
    call ESMF_ClockStopTimeDisable(clock, rc=status)
    _VERIFY(STATUS)

    call ESMF_ClockSet(clock, currTime=targetTime, rc=status)
    _VERIFY(STATUS)

    ! We do not need the protection anymore
    call ESMF_ClockStopTimeEnable(clock, rc=status)
    _VERIFY(STATUS)

    ! clean-up
    deallocate(alarmList)

    _RETURN(ESMF_SUCCESS)
  end subroutine ESMFL_ClockSet

end module MAPL_CapGridCompMod
