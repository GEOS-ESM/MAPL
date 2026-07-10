#define I_AM_MAIN
#include "MAPL.h"
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
   use mapl_ErrorHandling_mod
   implicit none

   integer :: status
   type(ESMF_Config) :: config
   type(ESMF_CalKind_Flag) :: calKind
   type(ESMF_Time) :: feb28, next_day
   type(ESMF_TimeInterval) :: one_day
   integer :: yy, mm, dd

   integer :: rc

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
        defaultDefaultCalKind=ESMF_CALKIND_GREGORIAN, &
        _RC)

   ! -----------------------------------------------------------------------
   ! Check 1: verify the default calendar set by ESMF_Initialize is NOLEAP.
   ! Set a time without specifying a calendar so ESMF uses the default, then
   ! query calkindflag from that time object to confirm the default kind.
   ! -----------------------------------------------------------------------
   call ESMF_TimeSet(feb28, yy=2000, mm=2, dd=28, h=0, m=0, s=0, _RC)

   call ESMF_TimeGet(feb28, calkindflag=calKind, _RC)
   _ASSERT(calKind == ESMF_CALKIND_NOLEAP, 'calendar kind expected to be ESMF_CALKIND_NOLEAP')

   ! -----------------------------------------------------------------------
   ! Check 2: day after 2000-02-28 is 2000-03-01 with the default calendar.
   ! (In a leap-year calendar 2000 has a Feb 29; NOLEAP skips it.)
   ! feb28 was already set in Check 1 above.
   ! -----------------------------------------------------------------------
   call ESMF_TimeIntervalSet(one_day, d=1, _RC)
   next_day = feb28 + one_day

   call ESMF_TimeGet(next_day, yy=yy, mm=mm, dd=dd, _RC)
   _ASSERT(mm == 3 .and. dd == 1, 'Next Day after Feb 28 is not Mar 1; calendar kind is not ESMF_CALKIND_NOLEAP')

   ! -----------------------------------------------------------------------
   ! Summary
   ! -----------------------------------------------------------------------
   call ESMF_Finalize(_RC)

end program noleap_calendar_driver
