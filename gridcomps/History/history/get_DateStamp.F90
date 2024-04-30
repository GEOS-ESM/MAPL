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
  SUBMODULE (MAPL_HistoryGridCompMod) get_DateStamp
!
! !USES:
!
  implicit none

contains


  MODULE SUBROUTINE get_DateStamp (clock, DateStamp, offset, rc)
    type (ESMF_Clock)                   :: clock
    character(len=ESMF_MAXSTR),optional :: DateStamp
    type(ESMF_TimeInterval),   optional :: offset
    integer, optional                   :: rc

    type(ESMF_Time)                   :: currentTime
    type(ESMF_Alarm)                  :: PERPETUAL
    character(len=ESMF_MAXSTR)        :: TimeString
    character(len=ESMF_MAXSTR)        :: clockname
    logical                           :: LPERP
    integer                           :: YY,MM,DD,H,M,S
    integer                           :: noffset

    integer                    :: STATUS

    call ESMF_ClockGet ( clock, name=clockname, currTime=currentTime, _RC)

    if (present(offset)) then
        call ESMF_TimeIntervalGet( OFFSET, S=noffset, _RC )
        if( noffset /= 0 ) then
            LPERP = ( index( trim(clockname),'_PERPETUAL' ).ne.0 )
        if( LPERP ) then
            call ESMF_ClockGetAlarm ( clock, AlarmName='PERPETUAL', alarm=PERPETUAL, _RC )
            if( ESMF_AlarmIsRinging(PERPETUAL) ) then
!
! Month has already been set back to PERPETUAL Month, therefore
! Time-Averaged Files (i.e., non-zero offset) need Month to be advanced for proper offset calculation
! ---------------------------------------------------------------------------------------------------
                call ESMF_TimeGet ( CurrentTime, YY = YY, &
                                                 MM = MM, &
                                                 DD = DD, &
                                                 H  = H , &
                                                 M  = M , &
                                                 S  = S, _RC )
                                                 MM = MM + 1
                call ESMF_TimeSet ( CurrentTime, YY = YY, &
                                                 MM = MM, &
                                                 DD = DD, &
                                                 H  = H , &
                                                 M  = M , &
                                                 S  = S, _RC )
#ifdef DEBUG
      if( MAPL_AM_I_ROOT() ) write(6,"(a,2x,i4.4,'/',i2.2,'/',i2.2,2x,'Time: ',i2.2,':',i2.2,':',i2.2)") "Inside HIST GetDate: ",YY,MM,DD,H,M,S
#endif
            endif
        endif
        endif
        currentTime = currentTime - offset
    end if

    call ESMF_TimeGet (currentTime, timeString=TimeString, _RC)

    if(present(DateStamp)) then
       associate ( &
         year   => TimeString( 1: 4), &
         month  => TimeString( 6: 7), &
         day    => TimeString( 9:10), &
         hour   => TimeString(12:13), &
         minute => TimeString(15:16), &
         second => TimeString(18:19)  &
         )
         DateStamp = year//month//day//'_'//hour//minute//second //'z'
      end associate

    end if

    _RETURN(ESMF_SUCCESS)
  end subroutine get_DateStamp


END SUBMODULE
