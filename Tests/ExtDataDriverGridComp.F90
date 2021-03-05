#include "MAPL_Generic.h"
#include "unused_dummy.H"

module ExtData_DriverGridCompMod
  use ESMF
  use MAPL
  use MAPL_ExtDataGridCompMod, only : ExtData_SetServices => SetServices
  use MAPL_HistoryGridCompMod, only : Hist_SetServices => SetServices
  use MAPL_Profiler

  implicit none
  private

  character(*), parameter :: internal_cap_name = "InternalCapGridComp"
  character(*), parameter :: internal_meta_comp_name = "InternalCapMetaComp"

  public :: ExtData_DriverGridComp
  public :: new_ExtData_DriverGridComp

  type :: ExtData_DriverGridComp
     private
     type (ESMF_GridComp)          :: gc
     procedure(), pointer, nopass  :: root_set_services => null()
     character(len=:), allocatable :: name, configFile
     logical :: amiroot, run_hist, run_extdata
     integer :: extdata_id, history_id, root_id, printspec
     integer :: nsteps
     type(ESMF_Clock) :: clock
     type(ESMF_Config) :: cf_ext, cf_root, cf_hist, config
     type(ESMF_GridComp), allocatable :: gcs(:)
     type(ESMF_State),    allocatable :: imports(:), exports(:)
     type(ESMF_VM) :: vm
     type(ESMF_Time), allocatable :: times(:)
   contains
     procedure :: set_services
     procedure :: initialize
     procedure :: run
     procedure :: run_one_step
     procedure :: finalize
     procedure :: get_am_i_root
     procedure :: parseTimes
     procedure :: advanceClockToTime
  end type ExtData_DriverGridComp

  type :: ExtData_DriverGridComp_Wrapper
     type(ExtData_DriverGridComp), pointer :: ptr => null()
  end type ExtData_DriverGridComp_Wrapper

  type :: MAPL_MetaComp_Wrapper
     type(MAPL_MetaComp), pointer :: ptr => null()
  end type MAPL_MetaComp_Wrapper
  
  include "mpif.h"

contains

  function new_ExtData_DriverGridComp(root_set_services, configFileName, name) result(cap)
    procedure() :: root_set_services
    character(len=*), optional, intent(in) :: name
    character(len=*), optional, intent(in) :: configFileName
    type(ExtData_DriverGridComp) :: cap

    type(ExtData_DriverGridComp_Wrapper) :: cap_wrapper
    type(MAPL_MetaComp_Wrapper) :: meta_comp_wrapper

    integer :: status, rc

    cap%root_set_services => root_set_services

    if (present(name)) then
       allocate(cap%name, source=name)
    else
       allocate(cap%name, source='CAP')
    end if

    if (present(configFileName)) then
       allocate(cap%configFile, source=configFileName)
    else
       allocate(cap%configFile, source='CAP.rc')
    end if

    cap%gc = ESMF_GridCompCreate(name='ExtData_DriverGridComp', rc = status)
    _VERIFY(status)

    allocate(cap_wrapper%ptr)
    cap_wrapper%ptr = cap
    call ESMF_UserCompSetInternalState(cap%gc, internal_cap_name, cap_wrapper, status)
    _VERIFY(status)

    allocate(meta_comp_wrapper%ptr)
    call ESMF_UserCompSetInternalState(cap%gc, internal_meta_comp_name, meta_comp_wrapper, status)
    _VERIFY(status)

  end function new_ExtData_DriverGridComp


  subroutine initialize_gc(gc, import_state, export_state, clock, rc)
    type(ESMF_GridComp) :: gc
    type(ESMF_State) :: import_state, export_state
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    integer :: comm
    integer                      :: NPES

    logical :: amIRoot_
    character(len=ESMF_MAXSTR)   :: enableTimers
    character(len=ESMF_MAXSTR)   :: enableMemUtils
    integer                      :: MemUtilsMode

    integer :: status

    character(len=ESMF_MAXSTR )           :: timerModeStr
    integer                               :: timerMode
    character(len=ESMF_MAXSTR)   :: ROOT_NAME

    ! Misc locals
    !------------
    character(len=ESMF_MAXSTR)   :: EXPID
    character(len=ESMF_MAXSTR)   :: EXPDSC


    ! Handles to the CAP's Gridded Components GCs
    ! -------------------------------------------

    integer                               :: i, itemcount
    type (ESMF_Field)                     :: field
    type (ESMF_FieldBundle)               :: bundle


    type (ESMF_StateItem_Flag), pointer   :: ITEMTYPES(:)
    character(len=ESMF_MAXSTR ), pointer  :: ITEMNAMES(:)

    integer                      :: RUN_DT
    integer :: nx
    integer :: ny

    integer                      :: HEARTBEAT_DT
    character(len=ESMF_MAXSTR)   :: HIST_CF, ROOT_CF, EXTDATA_CF

    type (MAPL_MetaComp), pointer :: MAPLOBJ
    procedure(), pointer :: root_set_services
    type(ExtData_DriverGridComp), pointer :: cap
    class(BaseProfiler), pointer :: t_p

    _UNUSED_DUMMY(import_state)
    _UNUSED_DUMMY(export_state)
    _UNUSED_DUMMY(clock)

    t_p => get_global_time_profiler()

    cap => get_CapGridComp_from_gc(gc)
    maplobj => get_MetaComp_from_gc(gc) 

    call ESMF_GridCompGet(gc, vm = cap%vm, rc = status)
    _VERIFY(status)
    call ESMF_VMGet(cap%vm, petcount = NPES, mpiCommunicator = comm, rc = status)
    _VERIFY(status)

    AmIRoot_ = MAPL_Am_I_Root(cap%vm)

    call MAPL_GetNodeInfo(comm = comm, rc = status)
    _VERIFY(STATUS)

    AmIRoot_ = MAPL_Am_I_Root(cap%vm)

    cap%AmIRoot = AmIRoot_

    !  Open the CAP's configuration from CAP.rc
    !------------------------------------------

    cap%config = ESMF_ConfigCreate(rc = status)
    _VERIFY(status)
    call ESMF_ConfigLoadFile(cap%config, cap%configFile, rc = status)
    _VERIFY(status)

    !  CAP's MAPL MetaComp
    !---------------------

    call MAPL_Set(MAPLOBJ,rc = status)
    _VERIFY(STATUS)

    call MAPL_Set(MAPLOBJ, name = cap%name, cf = cap%config, rc = status)
    _VERIFY(status)

    call ESMF_ConfigGetAttribute(cap%config,cap%run_hist,label="RUN_HISTORY:",default=.true.)
    call ESMF_ConfigGetAttribute(cap%config,cap%run_extdata,label="RUN_EXTDATA:",default=.true.)

    ! !RESOURCE_ITEM: string :: Name of ROOT's config file
    call MAPL_GetResource(MAPLOBJ, ROOT_CF, "ROOT_CF:", default = "ROOT.rc", rc = status) 
    _VERIFY(status)

    ! !RESOURCE_ITEM: string :: Name to assign to the ROOT component
    call MAPL_GetResource(MAPLOBJ, ROOT_NAME, "ROOT_NAME:", default = "ROOT", rc = status) 
    _VERIFY(status)

    ! !RESOURCE_ITEM: string :: Name of HISTORY's config file 
    call MAPL_GetResource(MAPLOBJ, HIST_CF, "HIST_CF:", default = "HISTORY.rc", rc = status) 
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
            default='MAX', RC=STATUS )
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
          _ASSERT(.false.,'needs informative message')
       end select TestTimerMode
       call MAPL_TimerModeSet(timerMode, RC=status)
       _VERIFY(status)
    end if

    enableMemUtils = ESMF_UtilStringUpperCase(enableMemUtils, rc=STATUS)
    _VERIFY(STATUS)

    if (enableMemUtils /= 'YES') then
       call MAPL_MemUtilsDisable( rc=STATUS )
       _VERIFY(STATUS)
    else
       call MAPL_MemUtilsInit( mode=MemUtilsMode, rc=STATUS )
       _VERIFY(STATUS)
    end if

    ! Handle RUN_DT in ROOT_CF
    !-------------------------

    cap%cf_root = ESMF_ConfigCreate(rc=STATUS )
    _VERIFY(STATUS)
    call ESMF_ConfigLoadFile(cap%cf_root, ROOT_CF, rc=STATUS )
    _VERIFY(STATUS)
    call ESMF_ConfigGetAttribute(cap%config, value=HEARTBEAT_DT, Label="HEARTBEAT_DT:", default=900, rc=status)
    call ESMF_ConfigGetAttribute(cap%cf_root, value=run_dt, Label="RUN_DT:", default=heartbeat_dt, rc=status)
    call MAPL_ConfigSetAttribute(cap%cf_root, value=run_dt, Label="RUN_DT:", rc=status)
    _VERIFY(STATUS)

    ! Add EXPID and EXPDSC from HISTORY.rc to AGCM.rc
    !------------------------------------------------
    if (cap%run_hist) then
       cap%cf_hist = ESMF_ConfigCreate(rc=STATUS )
       _VERIFY(STATUS)
       call ESMF_ConfigLoadFile(cap%cf_hist, HIST_CF, rc=STATUS )
       _VERIFY(STATUS)

       call MAPL_ConfigSetAttribute(cap%cf_hist, value=HIST_CF, Label="HIST_CF:", rc=status)
       _VERIFY(STATUS)

       call ESMF_ConfigGetAttribute(cap%cf_hist, value=EXPID,  Label="EXPID:",  default='', rc=status)
       call ESMF_ConfigGetAttribute(cap%cf_hist, value=EXPDSC, Label="EXPDSC:", default='', rc=status)

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
       call MAPL_ConfigSetAttribute(cap%cf_hist, value=run_dt, Label="RUN_DT:", rc=status)
       _VERIFY(STATUS)
    end if

    if (cap%run_extdata) then
       ! Add NX and NY from AGCM.rc to ExtData.rc as well as name of ExtData rc file
       cap%cf_ext = ESMF_ConfigCreate(rc=STATUS )
       _VERIFY(STATUS)
       call ESMF_ConfigLoadFile(cap%cf_ext, EXTDATA_CF, rc=STATUS )
       _VERIFY(STATUS)
       call ESMF_ConfigGetAttribute(cap%cf_root, value = NX, Label="NX:", rc=status)
       _VERIFY(STATUS)
       call ESMF_ConfigGetAttribute(cap%cf_root, value = NY, Label="NY:", rc=status)
       _VERIFY(STATUS)
       call MAPL_ConfigSetAttribute(cap%cf_ext, value=NX,  Label="NX:",  rc=status)
       _VERIFY(STATUS)
       call MAPL_ConfigSetAttribute(cap%cf_ext, value=NY,  Label="NY:",  rc=status)
       _VERIFY(STATUS)
       call MAPL_ConfigSetAttribute(cap%cf_ext, value=run_dt, Label="RUN_DT:", rc=status)
       _VERIFY(STATUS)
       call MAPL_ConfigSetAttribute(cap%cf_ext, value=EXTDATA_CF,  Label="CF_EXTDATA:",  rc=status)
       _VERIFY(STATUS)
    end if

    !  Create Root child
    !-------------------
    call MAPL_Set(MAPLOBJ, CF=CAP%CF_ROOT, RC=STATUS)
    _VERIFY(STATUS)

    call MAPL_ClockInit(cap%config, cap%clock, cap%nsteps, rc = status)
    _VERIFY(status)

    root_set_services => cap%root_set_services

    cap%root_id = MAPL_AddChild(MAPLOBJ, name = root_name, SS = root_set_services, rc = status)  
    _VERIFY(status)

    if (cap%run_hist) then

       call MAPL_Set(MAPLOBJ, CF=CAP%CF_HIST, RC=STATUS)
       _VERIFY(STATUS)

       cap%history_id = MAPL_AddChild( MAPLOBJ, name = 'HIST', SS = HIST_SetServices, rc = status)  
       _VERIFY(status)

    end if

    if (cap%run_extdata) then

       call MAPL_Set(MAPLOBJ, CF=CAP%CF_EXT, RC=STATUS)
       _VERIFY(STATUS)

       cap%extdata_id = MAPL_AddChild (MAPLOBJ, name = 'EXTDATA', SS = ExtData_SetServices, rc = status)
       _VERIFY(status)
    
    end if

    !  Query MAPL for the the children's for GCS, IMPORTS, EXPORTS
    !-------------------------------------------------------------

    call MAPL_Get(MAPLOBJ, childrens_gridcomps = cap%gcs, &
         childrens_import_states = cap%imports, childrens_export_states = cap%exports, rc = status)
    _VERIFY(status)

    !  Initialize the Computational Hierarchy
    !----------------------------------------

    call ESMF_GridCompInitialize(cap%gcs(cap%root_id), importState = cap%imports(cap%root_id), &
         exportState = cap%exports(cap%root_id), clock = cap%clock, userRC = status)
    _VERIFY(status)

    if (cap%run_hist) then

       ! All the EXPORTS of the Hierachy are made IMPORTS of History
       !------------------------------------------------------------

       call ESMF_StateAdd(cap%imports(cap%history_id), [cap%exports(cap%root_id)], rc = status)
       _VERIFY(STATUS)

       ! Initialize the History
       !------------------------

       call ESMF_GridCompInitialize (CAP%GCS(cap%history_id), importState=CAP%IMPORTS(cap%history_id), &
            exportState=CAP%EXPORTS(cap%history_id), clock=CAP%CLOCK, userRC=STATUS )
       _VERIFY(STATUS)

    end if

    if (cap%run_extdata) then

       ! Prepare EXPORTS for ExtData
       ! ---------------------------
       call ESMF_StateGet(cap%imports(cap%root_id), itemcount = itemcount, rc = status)
       _VERIFY(status)
       allocate(itemnames(itemcount), stat = status)
       _VERIFY(status)
       allocate(itemtypes(itemcount), stat = status)
       _VERIFY(status)

       call ESMF_StateGet(cap%imports(cap%root_id), itemnamelist = itemnames, &
            itemtypelist = itemtypes, rc = status)
       _VERIFY(status)

       do i = 1, itemcount
          if (ItemTypes(i) == ESMF_StateItem_Field) then
             call ESMF_StateGet(cap%imports(cap%root_id), itemnames(i), field, rc = status)
             _VERIFY(status)
             call MAPL_StateAdd(cap%exports(cap%extdata_id), field, rc = status)
             _VERIFY(status)
          else if (ItemTypes(i) == ESMF_StateItem_FieldBundle) then
             call ESMF_StateGet(cap%imports(cap%root_id), itemnames(i), bundle, rc = status)
             _VERIFY(status)
             call MAPL_StateAdd(cap%exports(cap%extdata_id), bundle, rc = status)
             _VERIFY(status)
          end if
       END DO
       deallocate(itemtypes)
       deallocate(itemnames)


       ! Initialize the ExtData
       !------------------------

       call ESMF_GridCompInitialize (cap%gcs(cap%extdata_id), importState = cap%imports(cap%extdata_id), &
            exportState = cap%exports(cap%extdata_id), & 
            clock = cap%clock, userRc = status)
       _VERIFY(status)

    end if

    call cap%parseTimes(rc=status)
    _VERIFY(status)

    _RETURN(ESMF_SUCCESS)
  end subroutine initialize_gc

  
  subroutine run_gc(gc, import, export, clock, rc)
    !ARGUMENTS:
    type(ESMF_GridComp) :: GC     ! Gridded component 
    type(ESMF_State) :: import ! Import state
    type(ESMF_State) :: export ! Export state
    type(ESMF_Clock) :: clock  ! The clock
    integer, intent(out) :: RC     ! Error code:

    integer :: status

    _UNUSED_DUMMY(import)
    _UNUSED_DUMMY(export)
    _UNUSED_DUMMY(clock)

    call run_MultipleTimes(gc, rc=status)
    _VERIFY(status)
    _RETURN(ESMF_SUCCESS)

  end subroutine run_gc


  subroutine finalize_gc(gc, import_state, export_state, clock, rc)
    type(ESMF_GridComp) :: gc
    type(ESMF_State) :: import_state, export_state
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    integer :: status

    type(ExtData_DriverGridComp), pointer :: cap
    type(MAPL_MetaComp), pointer :: MAPLOBJ

    _UNUSED_DUMMY(import_state)
    _UNUSED_DUMMY(export_state)
    _UNUSED_DUMMY(clock)
    
    cap => get_CapGridComp_from_gc(gc)
    MAPLOBJ => get_MetaComp_from_gc(gc)

    call ESMF_GridCompFinalize(cap%gcs(cap%root_id), importstate = cap%imports(cap%root_id), &
         exportstate=cap%exports(cap%root_id), clock = cap%clock, userrc = status)
    _VERIFY(status)

    if (cap%run_hist) then
       call ESMF_GridCompFinalize(cap%gcs(cap%history_id), importstate = cap%imports(cap%history_id), &
            exportstate = cap%exports(cap%history_id), clock = cap%clock, userrc = status)
       _VERIFY(status)
       call ESMF_ConfigDestroy(cap%cf_hist, rc = status)
       _VERIFY(status)
    end if

    if (cap%run_extdata) then
       call ESMF_GridCompFinalize(cap%gcs(cap%extdata_id), importstate = cap%imports(cap%extdata_id), &
            exportstate = cap%exports(cap%extdata_id), clock = cap%clock, userrc = status)
       _VERIFY(status)
       call ESMF_ConfigDestroy(cap%cf_ext, rc = status)
       _VERIFY(status)
    end if

    call ESMF_ConfigDestroy(cap%cf_root, rc = status)
    _VERIFY(status)
    call ESMF_ConfigDestroy(cap%config, rc = status)
    _VERIFY(status)

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
    class(ExtData_DriverGridComp), intent(inout) :: this
    integer, optional, intent(out) :: rc
    integer :: status

    call ESMF_GridCompSetServices(this%gc, set_services_gc, rc = status)
    _VERIFY(status)
    _RETURN(ESMF_SUCCESS)
  end subroutine set_services


  subroutine initialize(this, rc)
    class(ExtData_DriverGridComp), intent(inout) :: this
    integer, optional, intent(out) :: rc
    
    integer :: status
    
    call ESMF_GridCompInitialize(this%gc, userRc = status)
    _VERIFY(status)
    _RETURN(ESMF_SUCCESS)
  end subroutine initialize


  subroutine run(this, rc)
    class(ExtData_DriverGridComp), intent(inout) :: this
    integer, optional, intent(out) :: rc

    integer :: status
    integer :: userRc

    call ESMF_GridCompRun(this%gc, userRC=userRC,rc=status)
    _ASSERT(userRC==ESMF_SUCCESS .and. STATUS==ESMF_SUCCESS,'run failed')
    _RETURN(ESMF_SUCCESS)

  end subroutine run


  subroutine finalize(this, rc)
    class(ExtData_DriverGridComp), intent(inout) :: this
    integer, optional, intent(out) :: rc
    
    integer :: status    
    
    call ESMF_GridCompFinalize(this%gc, rc = status)
    _VERIFY(status)
    _RETURN(ESMF_SUCCESS)
  end subroutine finalize


  function get_am_i_root(this, rc) result (amiroot)
    class (ExtData_DriverGridComp) :: this
    integer, optional, intent(out) :: rc

    logical :: amiroot

    amiroot = this%amiroot

    _RETURN(ESMF_SUCCESS)

  end function get_am_i_root


  function get_CapGridComp_from_gc(gc) result(cap)
    type(ESMF_GridComp), intent(inout) :: gc
    type(ExtData_DriverGridComp), pointer :: cap
    type(ExtData_DriverGridComp_Wrapper) :: cap_wrapper
    integer :: rc
    call ESMF_UserCompGetInternalState(gc, internal_cap_name, cap_wrapper, rc)
    cap => cap_wrapper%ptr
  end function get_CapGridComp_from_gc

  
  function get_MetaComp_from_gc(gc) result(meta_comp)
    type(ESMF_GridComp), intent(inout) :: gc
    type(MAPL_MetaComp), pointer :: meta_comp
    type(MAPL_MetaComp_Wrapper) :: meta_comp_wrapper
    integer :: rc
    call ESMF_UserCompGetInternalState(gc, internal_meta_comp_name, meta_comp_wrapper, rc)
    meta_comp => meta_comp_wrapper%ptr
  end function get_MetaComp_from_gc


  subroutine run_MultipleTimes(gc, rc)
    type (ESMF_Gridcomp) :: gc
    integer, optional, intent(out) :: rc
    
    integer :: n, status

    type(ExtData_DriverGridComp), pointer :: cap
    type (MAPL_MetaComp), pointer :: MAPLOBJ
    procedure(), pointer :: root_set_services

    cap => get_CapGridComp_from_gc(gc)
    MAPLOBJ => get_MetaComp_from_gc(gc)

    if (allocated(cap%times)) then
       do n=1,size(cap%times)
          call cap%AdvanceClockToTime(cap%times(n),rc=status)
          _VERIFY(status)
          call cap%run_one_step(status)
          _VERIFY(status)
       enddo
    else
       do n=1,cap%nsteps
          call ESMF_ClockAdvance(cap%clock,rc=status)
          _VERIFY(status)
          call cap%run_one_step(status)
          _VERIFY(status)
       enddo
    endif

    _RETURN(ESMF_SUCCESS)
  end subroutine run_MultipleTimes


  subroutine run_one_step(this, rc)
    class(ExtData_DriverGridComp), intent(inout) :: this
    integer, intent(out) :: rc
    integer :: AGCM_YY, AGCM_MM, AGCM_DD, AGCM_H, AGCM_M, AGCM_S
    integer :: status

    type(ESMF_Time) :: currTime
    real            :: mem_total, mem_commit, mem_percent

    call ESMF_ClockGet(this%clock, CurrTime = currTime, rc = status)
    _VERIFY(status)
    call ESMF_TimeGet(CurrTime, YY = AGCM_YY, &
         MM = AGCM_MM, &
         DD = AGCM_DD, &
         H  = AGCM_H , &
         M  = AGCM_M , &
         S  = AGCM_S, rc=status)
    _VERIFY(status)

    call ESMF_GridCompGet(this%gc, vm = this%vm)
    ! Run the ExtData Component
    ! --------------------------

    if (this%run_extdata) then
       call ESMF_GridCompRun(this%gcs(this%extdata_id), importState = this%imports(this%extdata_id), &
            exportState = this%exports(this%extdata_id), &
            clock = this%clock, userrc = status)
       _VERIFY(status)
    end if

    ! Run the Gridded Component
    ! --------------------------
    call ESMF_GridCompRun(this%gcs(this%root_id), importstate = this%imports(this%root_id), &
         exportstate = this%exports(this%root_id), &
         clock = this%clock, userrc = status)
    _VERIFY(status)

    ! Call History Run for Output
    ! ---------------------------

    if (this%run_hist) then
       call ESMF_GridCompRun(this%gcs(this%history_id), importstate=this%imports(this%history_id), &
            exportstate = this%exports(this%history_id), &
            clock = this%clock, userrc = status)
       _VERIFY(status)
    end if
    call MAPL_MemCommited ( mem_total, mem_commit, mem_percent, RC=STATUS )
    if (this%AmIRoot) write(6,1000) AGCM_YY,AGCM_MM,AGCM_DD,AGCM_H,AGCM_M,AGCM_S,mem_percent
1000 format(1x,'AGCM Date: ',i4.4,'/',i2.2,'/',i2.2,2x,'Time: ',i2.2,':',i2.2,':',i2.2,2x,f5.1,'%Memory Committed')
    

    _RETURN(ESMF_SUCCESS)
  end subroutine run_one_step


  ! !IROUTINE: MAPL_ClockInit -- Sets the clock

  ! !INTERFACE: 

  subroutine MAPL_ClockInit ( cf, Clock, nsteps, rc)

    ! !ARGUMENTS:

    type(ESMF_Config), intent(inout) :: cf
    type(ESMF_Clock),    intent(  out) :: Clock
    integer, intent(out)               :: nsteps
    integer, optional,   intent(  out) :: rc

    integer        :: CUR_YY, DUR_YY
    integer        :: CUR_MM, DUR_MM
    integer        :: CUR_DD, DUR_DD
    integer        :: CUR_H, DUR_H
    integer        :: CUR_M, DUR_M
    integer        :: CUR_S, DUR_S
    integer        :: heartbeat_dt
    character(ESMF_MAXSTR)   :: CALENDAR
    integer                  :: status
    integer        :: datetime(2)
    type(ESMF_Calendar) :: cal
    type(ESMF_Time)          :: CurrTime
    type(ESMF_TimeInterval) :: timeInterval, duration


    ! initialize calendar to be Gregorian type
    ! ----------------------------------------

    call ESMF_ConfigGetAttribute( cf, CALENDAR, label='CALENDAR:', default="GREGORIAN", rc=STATUS )
    _VERIFY(STATUS)
    if    (CALENDAR=="GREGORIAN") then
       cal = ESMF_CalendarCreate( ESMF_CALKIND_GREGORIAN, name="ApplicationCalendar", rc=status )
       _VERIFY(STATUS)
       call ESMF_CalendarSetDefault(ESMF_CALKIND_GREGORIAN, RC=STATUS)
       _VERIFY(STATUS)
    elseif(CALENDAR=="JULIAN"   ) then
       cal = ESMF_CalendarCreate( ESMF_CALKIND_JULIAN, name="ApplicationCalendar", rc=status )
       _VERIFY(STATUS)
       call ESMF_CalendarSetDefault(ESMF_CALKIND_JULIAN, RC=STATUS)
       _VERIFY(STATUS)
    elseif(CALENDAR=="NOLEAP"   ) then
       cal = ESMF_CalendarCreate( ESMF_CALKIND_NOLEAP, name="ApplicationCalendar", rc=status )
       _VERIFY(STATUS)
       call ESMF_CalendarSetDefault(ESMF_CALKIND_NOLEAP, RC=STATUS)
       _VERIFY(STATUS)
    else
       _ASSERT(.false.,'needs informative message')
    endif

    call ESMF_ConfigGetAttribute(cf, datetime, label='BEG_DATE:',rc=status)
    _VERIFY(status)

    CALL MAPL_UnpackDateTime(DATETIME, CUR_YY, CUR_MM, CUR_DD, CUR_H, CUR_M, CUR_S)

    call ESMF_TimeSet( CurrTime, YY = CUR_YY, &
         MM = CUR_MM, &
         DD = CUR_DD, &
         H = CUR_H , &
         M = CUR_M , &
         S = CUR_S , &
         calendar=cal,  rc = STATUS  )
    _VERIFY(STATUS)

    call ESMF_ConfigGetAttribute(cf, heartbeat_dt, label='HEARTBEAT_DT:',rc=status)
    _VERIFY(status) 
    call ESMF_TimeIntervalSet( TimeInterval, h=0, m=0, s=heartbeat_dt, rc=status )
    _VERIFY(STATUS)
    Clock = ESMF_ClockCreate (timeInterval, CurrTime, rc=status )
    _VERIFY(STATUS)

    call ESMF_ConfigGetAttribute( cf, datetime, label='JOB_SGMT:',rc=STATUS )
    _VERIFY(status)
    CALL MAPL_UnpackDateTime(DATETIME, DUR_YY, DUR_MM, DUR_DD, DUR_H, DUR_M, DUR_S)

    call ESMF_TimeIntervalSet(  duration, YY = DUR_YY, &
         MM = DUR_MM, &
         D = DUR_DD, &
         H = DUR_H , &
         M = DUR_M , &
         S = DUR_S , &
         startTime = currTime, &
         rc = STATUS  )
    _VERIFY(STATUS)
    nsteps = duration/timeInterval
    _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_ClockInit


  subroutine parseTimes(this, rc)
    class(ExtData_DriverGridComp), intent(inout) :: this
    integer, intent(out), optional :: rc
    integer :: comp_YY, comp_MM, comp_DD, comp_H, comp_M, comp_S,columnCount,lineCount,i,ctime(2)
    integer :: status

    call ESMF_ConfigGetDim(this%config,lineCount,columnCount,label='RUN_TIMES::',rc=status)
    if (status==ESMF_SUCCESS) then
       allocate(this%times(lineCount))
       call ESMF_ConfigFindLabel(this%config,label='RUN_TIMES::',rc=status)
       _VERIFY(status)
       do i=1,lineCount
          call ESMF_ConfigNextLine(this%config,rc=status)
          _VERIFY(status)
          call ESMF_ConfigGetAttribute(this%config,ctime,rc=status)
          _VERIFY(status)
          call MAPL_UnpackDateTime(ctime,comp_YY,comp_MM,COMP_DD,COMP_H,COMP_M,COMP_S)
          call ESMF_TimeSet(this%times(i),yy=comp_yy,mm=comp_mm,dd=comp_dd,h=comp_h,m=comp_m,s=comp_s,rc=status)
          _VERIFY(status)
       enddo
    else
       _RETURN(ESMF_SUCCESS)
    end if

  end subroutine parseTimes

  subroutine advanceClockToTime(this, time,rc)
    class(ExtData_DriverGridComp), intent(inout) :: this
    type(ESMF_Time), intent(inout) :: time
    integer, intent(out), optional :: rc
    integer :: status

    type(ESMF_Time) :: currTime
    logical :: matched

    call ESMF_ClockGet(this%clock,currTime=currTime,rc=status)
    _VERIFY(status)
    if (time==currTime) then
       _RETURN(ESMF_SUCCESS)
    end if

    matched = .false.
    do while (.not. matched)
       call ESMF_ClockAdvance(this%clock,rc=status)
       _VERIFY(status)
       call ESMF_ClockGet(this%clock,currTime=currTime,rc=status)
       _VERIFY(status)
       if (currTime==Time) matched = .true.
    enddo
    _RETURN(ESMF_SUCCESS)

  end subroutine advanceClockToTime

end module ExtData_DriverGridCompMod
