!------------------------------------------------------------------------------
!               Global Modeling and Assimilation Office (GMAO)                !
!                    Goddard Earth Observing System (GEOS)                    !
!                                 MAPL Component                              !
!------------------------------------------------------------------------------
!
#include "MAPL_Generic.h"
#include "unused_dummy.H"
!
!>
!### MODULE: `MAPL_HistoryGridCompMod`
!
! Author: GMAO SI-Team
!
! `MAPL_HistoryGridCompMod` contains the `Initialize`, `Run` and `Finalize` methods for `History`.
! The three methods are called at the level of CAP.
!
  SUBMODULE (MAPL_HistoryGridCompMod) Get_Tdim
!
! !USES:
!
  implicit none

contains


  MODULE SUBROUTINE Get_Tdim (list, clock, tdim)


! !IROUTINE: Get_Tdim -- Returns Time Dimension (Number of Records) in a HISTORY.rc collection file

! !USES:
    use ESMF
    use MAPL_CommsMod, only: MAPL_AM_I_ROOT

    implicit none

! !ARGUMENTS:

    type (HistoryCollection),  intent(IN ) :: list
    type (ESMF_Clock),    intent(IN ) :: clock
    integer,              intent(OUT) :: tdim

! ESMF stuff
!-----------
    type (ESMF_Time)            :: currTime
    type (ESMF_Time)            :: stopTime
    type (ESMF_TimeInterval)    :: tint

! Misc locals
!------------
    real                         :: rfreq
    real                         :: rdelt
    real                         :: rfrac
    integer                      :: nfreq
    integer                      :: ndelt
    integer                      :: STATUS

!  Initialize TDIM=-1 (UNLIMITED)
!--------------------------------
    tdim = -1

    if( list%tm == 0) then  ! Dynamic calculation of time dimension

       if( list%duration == 0 ) then
          ! compute duration from the ESMF clock
          call ESMF_ClockGet(clock, currTime=currTime, stopTime=stopTime, &
               RC=status)
          if (status /= ESMF_SUCCESS) goto 200
          tint = stopTime - currTime
          call ESMF_TimeIntervalGet(tint, s=ndelt, RC=status)
          if (status /= ESMF_SUCCESS) goto 200

          nfreq = MAPL_nsecf( list%frequency )
          rfreq = real(nfreq)
          rdelt = real(ndelt)
          rfrac = rdelt/rfreq - ndelt/nfreq
          if( rfrac.ne.0 ) rfrac = 1.0 - rfrac
          ndelt = ndelt  + rfrac*nfreq

       else
          ndelt = MAPL_nsecf( list%duration )
       endif

       nfreq = MAPL_nsecf( list%frequency )
       if (nfreq /=0) then
          tdim  = ndelt/nfreq
       end if

    else
       tdim = list%tm
    endif  ! End TM=0 Test

! Debug Prints
! ------------
200 continue
    if( MAPL_AM_I_ROOT() ) then
       write(6,100) list%frequency, list%duration, tdim, trim(list%collection)
100    format(1x,'Freq: ',i8.8,'  Dur: ',i8.8,'  TM: ',i4,'  Collection: ',a)
    endif

    return
  end subroutine Get_Tdim

END SUBMODULE
