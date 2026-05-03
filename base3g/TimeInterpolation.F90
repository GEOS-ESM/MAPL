#include "MAPL.h"

module MAPL_TimeInterpolation
   use ESMF
   use MAPL_ExceptionHandling, only: MAPL_Assert, MAPL_Verify, MAPL_Return
   implicit none
   private

   public :: MAPL_Interp_Fac
   public :: MAPL_ClimInterpFac

contains

   subroutine MAPL_Interp_Fac(TIME0, TIME1, TIME2, FAC1, FAC2, RC)

      !  PURPOSE:
      !  ========
      !
      !    Compute interpolation factors, fac, to be used
      !    in the calculation of the instantaneous boundary
      !    conditions, ie:
      !
      !     q(i,j) = fac1*q1(i,j) + (1.-fac1)*q2(i,j)
      !
      !    where:
      !     q(i,j)  => Boundary Data valid    at time0
      !     q1(i,j) => Boundary Data centered at time1
      !     q2(i,j) => Boundary Data centered at time2
      !
      !  INPUT:
      !  ======
      !    time0    : Time of current timestep
      !    time1    : Time of boundary data 1
      !    time2    : Time of boundary data 2
      !
      !  OUTPUT:
      !  =======
      !     fac1    : Interpolation factor for Boundary Data 1

      type(ESMF_Time),   intent(in ) :: TIME0, TIME1, TIME2
      real,              intent(out) :: FAC1
      real,    optional, intent(out) :: FAC2
      integer, optional, intent(out) :: RC

      type(ESMF_TimeInterval) :: TimeDif1
      type(ESMF_TimeInterval) :: TimeDif

      TimeDif1 = TIME2 - TIME0
      TimeDif  = TIME2 - TIME1

      FAC1 = TimeDif1/TimeDif

      if (present(FAC2)) FAC2 = 1. - FAC1
      if (present(RC  )) RC   = ESMF_SUCCESS

   end subroutine MAPL_Interp_Fac


   subroutine MAPL_ClimInterpFac(CLOCK, I1, I2, FAC, RC)

      type(ESMF_Clock),  intent(in ) :: CLOCK
      integer,           intent(out) :: I1, I2
      real,              intent(out) :: FAC
      integer, optional, intent(out) :: RC

      integer                       :: STATUS
      character(len=ESMF_MAXSTR), parameter :: IAm = 'MAPL_ClimInterpFac'

      type(ESMF_Time)         :: CurrTime
      type(ESMF_Time)         :: midMonth
      type(ESMF_Time)         :: BEFORE, AFTER
      type(ESMF_TimeInterval) :: oneMonth
      type(ESMF_Calendar)     :: cal

      call ESMF_ClockGet       (CLOCK,    CurrTime=CurrTime, calendar=cal, _RC)
      call ESMF_TimeGet        (CurrTime, midMonth=midMonth,               _RC)
      call ESMF_TimeIntervalSet(oneMonth, MM=1, calendar=cal,              _RC)

      if (CurrTime < midMonth) then
         AFTER    = midMonth
         midMonth = midMonth - oneMonth
         call ESMF_TimeGet(midMonth, midMonth=BEFORE, _RC)
      else
         BEFORE   = midMonth
         midMonth = midMonth + oneMonth
         call ESMF_TimeGet(midMonth, midMonth=AFTER,  _RC)
      end if

      call MAPL_Interp_Fac(CurrTime, BEFORE, AFTER, FAC, _RC)

      call ESMF_TimeGet(BEFORE, MM=I1, _RC)
      call ESMF_TimeGet(AFTER,  MM=I2, _RC)

      _RETURN(ESMF_SUCCESS)

   end subroutine MAPL_ClimInterpFac

end module MAPL_TimeInterpolation
