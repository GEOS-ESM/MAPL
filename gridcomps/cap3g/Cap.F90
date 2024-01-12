#include "MAPL_Generic.h"

! Responsibilities:
!  - Initialize MAPL "global" features
!      - **server**  (ignore in 1st pass)
!      - profiler (ignore in 1st pass)
!      - pflogger (ignore in 1st pass)
!      - ??? establish gregorian calendar
!  - Determine basic clock
!      - start, stop, dt

!  - Construct component driver for CapGridComp
!    - possibly allow other "root" here?
!  - Exercise driver through the init phases.
!  - Loop over time
!    - call run phase of capgridcomp

module mapl3g_Cap
   use mapl3g_CapGridComp, only: cap_setservices => setServices
   use mapl3g_GenericGridComp, only: generic_setservices => setServices
   use esmf
   implicit none
   private

   public :: run

   interface run
      procedure :: run_cap
      procedure :: run_driver
   end interface run

contains


   subroutine run(config_filename, unusable, comm, rc)
      character(*), intent(in) :: config_filename
      integer, optional, intent(in) :: comm
      integer, optional, intent(out) :: rc

      type(ESMF_HConfig) :: hconfig

      call MAPL_initialize(config_fileName, _RC)

      hconfig = MAPL_HConfigCreate(config_filename, _RC)
      driver = make_driver(hconfig, _RC)

      call initialize(driver, _RC)
      call run(driver, _RC)
      call finalize(driver, _RC)

      call ESMF_HConfigDestroy(config, nogarbage=.true., _RC)
      call MAPL_Finalize(_RC)

      _RETURN(_SUCCESS)
   end subroutine run

   function make_driver(hconfig, rc) result(driver)
      type(GriddedComponentDriver) :: driver

      integer :: status
      
      clock = make_clock(hconfig, _RC)
      cap_gridcomp = create_grid_comp(cap_name, cap_gc_setservices, hconfig, _RC)
      clock = make_clock(hconfig, _RC)
      driver = ComponentDriver(gridcomp, clock=clock, _RC)

      _RETURN(_SUCCESS)
   end function make_driver

   function create_clock(config, rc) result(clock)
      type(ESMF_Clock) :: clock
      type(ESMF_HConfig), intent(in) :: config
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Time) :: start_time, end_time, time_step
      type(ESMF_HConfig) :: clock_config
      
      clock_config = ESMF_HConfigCreateAt(hconfig, keystring='clock', _RC)

      call set_time_interval(start_time, 'start', clock_config, _RC)
      call set_time(end_time, 'end', clock_config, _RC)
      call set_time(time_step, 'dt', clock_config, _RC)
      clock = ESMF_ClockCreate(timestep=dt, startTime=t_begin, endTime=t_end, _RC)
      
      _RETURN(_SUCCESS)
   end function create_clock

   subroutine set_time_interval(interval, key, hconfig, rc)
      type(ESMF_TimeInterval), intent(out) :: interval
      character(*), intent(in) :: key
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc
      
      integer :: status
      character(:), allocatable :: iso_duration
      
      iso_duration = ESMF_HConfigAsString(hconfig, keystring=key, _RC)
      call ESMF_TimeIntervalSet(interval, timeString=iso_time, _RC)
      
      _RETURN(_SUCCESS)
   end subroutine set_time

   subroutine set_time(time, key, hconfig, rc)
      type(ESMF_Time), intent(out) :: time
      character(*), intent(in) :: key
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc
      
      integer :: status
      character(:), allocatable :: iso_time
      
      iso_time = ESMF_HConfigAsString(hconfig, keystring=key, _RC)
      call ESMF_TimeSet(time, timeString=iso_time, _RC)
      
      _RETURN(_SUCCESS)
   end subroutine set_time

   subroutine initialize_driver(driver, rc)

      integer :: i

      do i = 1, size(GENERIC_INIT_PHASE_SEQUENCE)
         call driver%initialize(phase=GENERIC_INIT_PHASE_SEQUENCE(i), _RC)
      end do
   end subroutine initialize_driver

   subroutine run_driver(driver, rc)

      clock = driver%get_clock()
      time = ESMF_ClockGet(clock, time=time, _RC)
      end_time = ...
      do while(time < end_time)
         call driver%run(_RC)
         call driver%clock_advance(_RC)
      end do
      
   end subroutine run_driver


 
  subroutine MAPL_Initialize(config_filename, mpi_communicator, rc)
      character(*), intent(in) :: config_filename
      integer, intent(in) :: mpi_communicator
      integer, optional, intent(out) :: rc

      integer :: status

      ! Cannot process config file until ESMF is initialized, so this is first.
      
      call ESMF_Initialize(configFileName=config_filename, configKey='esmf', &
           mpiCommunicator=mpi_communicator,_RC)
      call profiler_init(...)
      call pflogger_init(...)

      _RETURN(_SUCCESS)
   end subroutine MAPL_Initialize
      
   subroutine MAPL_Finalize(rc)
      integer, optional, intent(out) :: rc

      integer :: status

      ! Cannot process config file until ESMF is initialized, so this is first.

      call profiler_finalize(...)
      call pflogger_finalize(...)
      call ESMF_Finalize(_RC)

      _RETURN(_SUCCESS)
   end subroutine MAPL_Finalize
      

     
end module mapl3g_Cap
