#include "MAPL.h"

module my_gc
   use esmf
   use mapl_ErrorHandlingMod
   implicit none
   private

   public :: gc_t
   public :: make_gc_t

   type :: GC_T
      type(ESMF_GridComp) :: gridcomp
      type(ESMF_State)    :: importState, exportState
      type(ESMF_Clock) :: clock
   end type GC_T


contains
   function make_gc_t(rc) result(gc)
      type(GC_T) :: gc
      integer, optional, intent(out) :: rc

      integer :: status

      gc%gridcomp = ESMF_GridcompCreate(name='foo',_RC)
      gc%importState = ESMF_StateCreate(_RC)
      gc%exportState = ESMF_StateCreate(_RC)
      gc%clock = create_clock(_RC)
      call ESMF_GridCompSetServices(gc%gridcomp, setServices, _RC)

      rc = 0
   end function make_gc_t

   subroutine setservices(gridcomp, rc)
      type(ESMF_GridComp) :: gridcomp
      integer, intent(out) :: rc
      integer :: status
      call ESMF_GridcompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, run, _RC)
      rc = 0
   end subroutine setservices

   subroutine run(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gridcomp
      type(ESMF_State) :: importState, exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      integer :: status
      call ESMF_ClockAdvance(clock, _RC)
      rc=0
   end subroutine run

   function create_clock(rc) result(clock)
      type(ESMF_Clock) :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Time) :: start_time, stop_time
      type(ESMF_TimeInterval) :: time_step

      call ESMF_CalendarSetDefault(ESMF_CALKIND_GREGORIAN, _RC)
      call ESMF_TimeIntervalSet(time_step, s=900, _RC)
      call ESMF_TimeSet(start_time, timeString='2023-12-22T21:00:00', _RC)
      call ESMF_TimeSet(stop_time, timeString='2023-12-23T21:00:00', _RC)
      clock = ESMF_ClockCreate(timestep=time_step, startTime=start_time, stopTime=stop_time, _RC)

      _RETURN(_SUCCESS)
   end function create_clock

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

end module my_gc


#define I_AM_MAIN
#include "MAPL.h"

program main
   use my_gc
   use esmf
   use mapl_ErrorHandlingMod
   use iso_fortran_env, only: INT64
   implicit none

   integer :: status

   type(GC_T), allocatable :: gcs(:)

   integer, parameter :: N_GCS = 100
   integer, parameter :: N_STEPS = 10
   integer :: i, j
   real :: t_all, t_one
   integer(kind=INT64) :: c0, c1, cr
   integer :: rc, userStatus

   call ESMF_Initialize(_RC)
   allocate(gcs(N_GCS))
   do i= 1, N_GCS
      gcs(i) = make_gc_t(_RC)
   end do

   call system_clock(c0, cr)
   do j = 1, N_STEPS
      do i = 1, N_GCS
         call ESMF_GridCompRun(gcs(i)%gridcomp, importState=gcs(i)%importState, exportState=gcs(i)%exportState, clock=gcs(i)%clock, userrc=userStatus, _RC)
         _VERIFY(userStatus)
      end do
   end do
   call system_clock(c1)

   t_all = real(c1-c0)/real(cr)
   t_one = t_all/real(N_GCS*N_STEPS)

   print*,'Time: ', t_one, t_all
   call ESMF_Finalize(_RC)


end program main


