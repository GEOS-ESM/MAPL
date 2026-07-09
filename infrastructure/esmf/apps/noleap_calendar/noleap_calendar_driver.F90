! Standalone driver that verifies ESMF_CALKIND_NOLEAP calendar behaviour.
!
! The default calendar kind is configured through the YAML file passed as
! the first command-line argument (see noleap_calendar.yaml).  ESMF reads
! the "esmf:" section of that file during ESMF_Initialize and sets the
! default calendar from the "defaultCalKind:" key.
!
! Usage:
!   mpirun -np 1 noleap_calendar_driver.x noleap_calendar.yaml
!
! Exit code:  0 – all checks passed
!            non-zero – one or more checks failed

program noleap_calendar_driver
   use esmf
   implicit none

   integer :: status
   type(ESMF_Config) :: config
   type(ESMF_CalKind_Flag) :: calKind
   type(ESMF_Time) :: feb28, next_day
   type(ESMF_TimeInterval) :: one_day
   integer :: yy, mm, dd
   integer :: n_pass, n_fail

   n_pass = 0
   n_fail = 0

   ! -----------------------------------------------------------------------
   ! Initialise ESMF from the YAML config file given as command-line arg 1.
   ! The "esmf:" section of the file is forwarded to ESMF_Initialize, which
   ! processes "defaultCalKind:" and calls ESMF_CalendarSetDefault internally.
   ! The defaultDefaultCalKind below serves as a fallback when the key is
   ! absent from the YAML; NOLEAP is used so that the driver remains useful
   ! even if run without a config file.
   ! -----------------------------------------------------------------------
   call ESMF_Initialize( &
        configFilenameFromArgNum=1, &
        configKey=['esmf'], &
        config=config, &
        defaultDefaultCalKind=ESMF_CALKIND_NOLEAP, &
        rc=status)
   if (status /= ESMF_SUCCESS) then
      print *, 'FAIL: ESMF_Initialize'
      error stop 1
   end if

   ! -----------------------------------------------------------------------
   ! Check 1: verify the default calendar set by ESMF_Initialize is NOLEAP.
   ! Set a time without specifying a calendar so ESMF uses the default, then
   ! query calkindflag from that time object to confirm the default kind.
   ! -----------------------------------------------------------------------
   call ESMF_TimeSet(feb28, yy=2000, mm=2, dd=28, h=0, m=0, s=0, rc=status)
   call report('ESMF_TimeSet(2000-02-28) with default calendar', &
        status == ESMF_SUCCESS, n_pass, n_fail)

   call ESMF_TimeGet(feb28, calkindflag=calKind, rc=status)
   call report('default calendar kind is ESMF_CALKIND_NOLEAP', &
        status == ESMF_SUCCESS .and. (ESMF_CALKIND_NOLEAP == calKind), &
        n_pass, n_fail)

   ! -----------------------------------------------------------------------
   ! Check 2: day after 2000-02-28 is 2000-03-01 with the default calendar.
   ! (In a leap-year calendar 2000 has a Feb 29; NOLEAP skips it.)
   ! feb28 was already set in Check 1 above.
   ! -----------------------------------------------------------------------
   call ESMF_TimeIntervalSet(one_day, d=1, rc=status)
   next_day = feb28 + one_day

   call ESMF_TimeGet(next_day, yy=yy, mm=mm, dd=dd, rc=status)
   if (status /= ESMF_SUCCESS) then
      call report('ESMF_TimeGet after Feb 28', .false., n_pass, n_fail)
   else
      call report('day after 2000-02-28 is 2000-03-01 (no Feb 29)', &
           yy == 2000 .and. mm == 3 .and. dd == 1, n_pass, n_fail)
      if (.not. (yy == 2000 .and. mm == 3 .and. dd == 1)) then
         write(*, '(a,3i4)') '      got:', yy, mm, dd
      end if
   end if

   ! -----------------------------------------------------------------------
   ! Summary
   ! -----------------------------------------------------------------------
   write(*, *)
   write(*, '(a,i0,a,i0,a)') 'Results: ', n_pass, ' passed, ', n_fail, ' failed'

   call ESMF_Finalize(rc=status)

   if (n_fail /= 0) error stop 1

contains

   subroutine report(label, passed, n_pass, n_fail)
      character(len=*), intent(in) :: label
      logical,          intent(in) :: passed
      integer,       intent(inout) :: n_pass, n_fail
      if (passed) then
         n_pass = n_pass + 1
         write(*, '(a,a)') 'PASS: ', label
      else
         n_fail = n_fail + 1
         write(*, '(a,a)') 'FAIL: ', label
      end if
   end subroutine report

end program noleap_calendar_driver
