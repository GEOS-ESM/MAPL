#include "MAPL_Generic.h"

program comp_testing_driver
  use ESMF
  use ESMFL_Mod
  use MAPL
  use MPI
  use MAPL_GenericMod
  use MAPL_BaseMod
  use MAPL_CapGridCompMod
  use MAPL_TimeDataMod
  use MAPL_GridManagerMod
  !use GEOS_GwdGridCompMod, only : GwdSetServices => SetServices

  implicit none

  call main()

  CONTAINS
    
    subroutine main()
      integer :: status, compStatus, rc, myPET, nPET, numArgs
      type(ESMF_VM) :: vm
      character(len=ESMF_MAXSTR) :: filename, compName
      type(ESMF_Config) :: config

      ! initialize
      call ESMF_Initialize(logKindFlag=ESMF_LOGKIND_MULTI, vm=vm, _RC)
      call ESMF_VMGet(vm, localPET=myPET, petCount=nPET, _RC)
      call MAPL_Initialize(_RC)
      call ESMF_CalendarSetDefault(ESMF_CALKIND_GREGORIAN, _RC) ! need in order to set time

      ! get rc filename and components to run
      call get_command_argument(1, filename)
      config = ESMF_ConfigCreate(_RC)
      call ESMF_ConfigLoadFile(config, filename, _RC)
      call ESMF_ConfigFindLabel(config, label="COMPONENT_TO_RUN:", _RC)
      compStatus = 0
      !do while (compStatus == 0)
         call ESMF_ConfigGetAttribute(config, value=compName, rc=compStatus)
         call driver_component(filename, compName, _RC)
      !end do

      ! finalize
      call MAPL_Finalize(_RC)
      call ESMF_Finalize (_RC)
  end subroutine main

  subroutine driver_component(filename, compName, rc)
    character(len=*), intent(in) :: filename, compName
    integer, intent(out) :: rc
    integer :: status, root_id, GWD
    character(len=ESMF_MAXSTR) :: time, startTime
    type(ESMF_Clock) :: clock
    type (MAPL_MetaComp), pointer :: state
    type(ESMF_Time), allocatable :: timeSeries(:)
    type(ESMF_TimeInterval) :: timeInterval
    type(ESMF_GridComp) :: GC
    type(ESMF_State) :: import, export
    type(ESMF_Config) :: config
    type(ESMF_Time) :: esmf_startTime
    type(ESMF_Grid) :: grid
    type (MAPL_MetaComp), pointer :: metacomp
    procedure(), pointer :: root_set_services
    character(len=ESMF_MAXSTR) :: GC_name
    type (MAPL_MetaComp), pointer :: maplobj
   
    print*, trim(compName)
    config = ESMF_ConfigCreate(_RC)
    call ESMF_ConfigLoadFile(config, filename, _RC)

    ! any additional attributes, possibly load these in main and pass as input, depending on how many there are
    call ESMF_ConfigGetAttribute(config, value=startTime, label="START_TIME:", _RC)
    call ESMF_ConfigGetAttribute(config, value=time, label="TIME:", _RC)
    
    ! Create a clock, set current time to required time consistent with checkpoints used 
    ! (we probably could pull this time straight from the checkpoint)
    call ESMF_TimeIntervalSet(timeInterval, h=6, m=0, s=0, _RC)
    startTime = trim(startTime)
    esmf_startTime = parse_time_string(startTime, _RC)
    clock = ESMF_ClockCreate(timeInterval, esmf_startTime, _RC)
    
    ! Create a grid (since the MAPL components do not do this other than GCM) we must do it here, 
    ! note for the vision of using a grid that is only a subset of columns we will have to see if we can 
    ! use an existing grid factory, or might require new factory
    grid=grid_manager%make_grid(config, _RC)

    ! Add component to be tested as the “child” via MAPL_AddChild
    !call MAPL_Set(metacomp, CF=config, _RC)
    !root_id = MAPL_AddChild(metacomp, name=compName, userRoutine="comp_test", _RC)
    !allocate(MAPLOBJ)
    !call MAPL_Set(MAPLOBJ, CF=config, RC=STATUS)
    GC_name = trim(compName)
    GC = ESMF_GridCompCreate(name=GC_name, _RC)
    maplobj => null()
    call MAPL_InternalStateCreate(gc, maplobj, _RC)
    call MAPL_InternalStateRetrieve(gc, maplobj, _RC)
    call MAPL_Set(MAPLOBJ, CF=config, _RC)
    root_id = MAPL_AddChild(MAPLOBJ, name=GC_name, userRoutine="setservices_", sharedObj="libGEOSgwd_GridComp", _RC)  ! add grid as well? need sharedObj?
    !GWD = MAPL_AddChild(GC, NAME='GWD', SS=MAPL_GenericSetServices, RC=STATUS)
    
    ! Set grid in child
    ! set coords? attributes?
    
    ! Will probably have to do something to force the right exports to get allocated when we run 
    ! genericinitialize, one idea is to use the checkpoint itself, examine, what variables are in
    ! the checkpoint for the export and ensure those variables are allocated in the genericinitialize, somehow...

    !call MAPL_GenericInitialize(GC, import, export, clock, _RC)
    !call MAPL_GenericRunChildren(GC, import, export, clock, _RC)
    !call MAPL_GenericFinalize(GC, import, export, clock, _RC)

    _RETURN(_SUCCESS)
  end subroutine driver_component

end program comp_testing_driver
