#include "MAPL_Generic.h"

program ComponentDriver

   use mapl_ErrorHandling
   use mapl3, only: MAPL_Initialize, MAPL_Finalize
   use mapl3, only: GriddedComponentDriver, MAPL_GridCompCreate
   use mapl3, only: initialize_phases, GENERIC_INIT_PHASE_SEQUENCE
   use mapl3, only: GENERIC_RUN_USER, GENERIC_RUN_CLOCK_ADVANCE
   use MAPL_TimeStringConversion, only: hconfig_to_esmf_timeinterval
   use mapl3g_UserSetServices
   use mapl3g_MultiState
   use esmf

   implicit none

   call main()

contains

   subroutine main(rc)
      integer, optional, intent(out) :: rc

      type(ESMF_HConfig) :: hconfig, cap_config
      logical :: is_model_pet, has_cap_config
      type(ESMF_GridComp), allocatable :: servers(:)
      type(GriddedComponentDriver) :: driver
      integer :: status

      call MAPL_Initialize(hconfig, is_model_pet=is_model_pet, servers=servers, _RC)

      has_cap_config = ESMF_HConfigIsDefined(hconfig, keystring="cap", _RC)
      _ASSERT(has_cap_config, "No cap section found in configuration file")
      cap_config = ESMF_HConfigCreateAt(hconfig, keystring="cap", _RC)

      driver = make_driver(cap_config, is_model_pet, _RC)
      call ESMF_HConfigDestroy(cap_config, _RC)

      if (is_model_pet) then
         call initialize_phases(driver, phases=GENERIC_INIT_PHASE_SEQUENCE, _RC)
         call driver%read_restart(_RC)
         ! call integrate(driver, _RC)
         call driver%write_restart(_RC)
         call driver%finalize(_RC)
      end if
      
      call MAPL_Finalize(_RC)

      _RETURN(_SUCCESS)
   end subroutine main

   function make_driver(hconfig, is_model_pet, rc) result(driver)
      use mapl3g_GenericGridComp, only: generic_SetServices => setServices
      type(GriddedComponentDriver) :: driver
      type(ESMF_HConfig), intent(inout) :: hconfig
      logical, intent(in) :: is_model_pet
      integer, optional, intent(out) :: rc

      type(ESMF_GridComp) :: root_gridcomp
      type(ESMF_Clock) :: clock
      integer :: status, user_status
      type(ESMF_HConfig) :: root_config
      integer, allocatable :: petList(:)

      clock = create_clock(hconfig, _RC)
      root_config = ESMF_HConfigCreateAt(hconfig, keystring="root", _RC)
      call ESMF_HConfigFileSave(root_config, "root.yaml", _RC)
      petList = get_model_pets(is_model_pet, _RC)
      root_gridcomp = MAPL_GridCompCreate("ROOT", user_setservices("libconfigurable_gridcomp"), root_config, _RC)
      call ESMF_GridCompSetServices(root_gridcomp, generic_setServices, userRC=user_status, _RC)
      _VERIFY(user_status)

      driver = GriddedComponentDriver(root_gridcomp, MultiState(), clock)
      _HERE

      _RETURN(_SUCCESS)
   end function make_driver

   ! Create function that accepts a logical flag returns list of mpi processes that have .true..
   function get_model_pets(flag, rc) result(petList)
      use mpi
      integer, allocatable :: petList(:)
      logical, intent(in) :: flag
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_VM) :: vm
      logical, allocatable, target :: flags(:)
      integer :: world_comm
      integer :: i, petCount

      call ESMF_VMGetCurrent(vm, _RC)
      call ESMF_VMGet(vm, petCount=petCount, mpiCommunicator=world_comm, _RC)
      allocate(flags(petCount))
      call MPI_Allgather(flag, 1, MPI_LOGICAL, flags, 1, MPI_LOGICAL, world_comm, status)
      _VERIFY(status)
      petList = pack([(i, i=0,petCount-1)], flags)

      _RETURN(_SUCCESS)
   end function get_model_pets

   function create_clock(hconfig, rc) result(clock)
      type(ESMF_Clock) :: clock
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: timeString
      type(ESMF_Time) :: startTime, stopTime
      type(ESMF_TimeInterval) :: timeStep
      type(ESMF_HConfig) :: clock_config

      clock_config = ESMF_HConfigCreateAt(hconfig, keystring="clock", _RC)

      call ESMF_CalendarSetDefault(ESMF_CALKIND_GREGORIAN,_RC)
      timeString = ESMF_HConfigAsString(clock_config, keystring="start", _RC)
      call ESMF_TimeSet(startTime, timeString=timeString, _RC)
      call ESMF_TimePrint(startTime, options="string", prestring="start time: " ,_RC)
      timeStep = hconfig_to_esmf_timeinterval(clock_config, "dt", _RC)
      stopTime = startTime + timeStep
      call ESMF_TimePrint(stopTime, options="string", prestring=" stop time: ", _RC)
      clock = ESMF_ClockCreate(timeStep=timeStep, startTime=startTime, stopTime=stopTime, refTime=startTime, _RC)

      _RETURN(_SUCCESS)
   end function create_clock

   subroutine integrate(driver, rc)
      type(GriddedComponentDriver), intent(inout) :: driver
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Clock) :: clock
      type(ESMF_Time) :: currTime, stopTime

      clock = driver%get_clock()
      call ESMF_ClockGet(clock, currTime=currTime, stopTime=stopTime, _RC)

      do while (currTime < stopTime)
         ! TODO:  include Bill's monitoring log messages here
         call driver%run(phase_idx=GENERIC_RUN_USER, _RC)
         call driver%run(phase_idx=GENERIC_RUN_CLOCK_ADVANCE, _RC)
         call driver%clock_advance(_RC)
         call ESMF_ClockGet(clock, currTime=currTime, _RC)
      end do
      call ESMF_TimePrint(currTime, options='string', preString='Cap time after loop: ', _RC)

      _RETURN(_SUCCESS)

   end subroutine integrate

end program ComponentDriver
