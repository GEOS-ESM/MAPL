! This module allows pFlogger to access the current simulation time in
! MAPL and thereby annotate log entries with the current simulation
! time.  To accomplish this, the module hold a (shallow) copy
! of the clock in CapGridComp.

module mapl_SimulationTime
   use pflogger, only: StringUnlimitedMap
   use ESMF
   implicit none (type, external)
   private

   public :: set_reference_clock
   public :: fill_time_dict

   type(ESMF_Clock), save :: reference_clock

contains

   subroutine set_reference_clock(clock)
      type(ESMF_Clock), intent(in) :: clock
      reference_clock = clock
   end subroutine set_reference_clock

   subroutine fill_time_dict(dict)
      type (StringUnlimitedmap), intent(out) :: dict

      integer :: status
      type(ESMF_Time) :: time

      integer :: yy, mm, dd, h, m, s

      call ESMF_ClockValidate(reference_clock, rc=status)
      if (status /= 0) error stop "Must pass reference via set_reference_clock() before use."

      call ESMF_ClockGet(reference_clock, currTime=time, rc=status)
      if (status /= 0) error stop "could not get current time in SimulationTime.F90"

      call ESMF_TimeGet(time, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, rc=status)
      if (status /= 0) error stop "Failed to get data from ESMF_TimeGet()."

      call dict%insert('Y',yy)
      call dict%insert('M',mm)
      call dict%insert('D',dd)
      call dict%insert('HH',h)
      call dict%insert('MM',m)
      call dict%insert('SS',s)
      call dict%insert('MS',0)

   end subroutine Fill_Time_Dict

end module mapl_SimulationTime

