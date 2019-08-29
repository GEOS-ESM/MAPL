! $Id$

#include "MAPL_ErrLog.h"

module MAPL_SunMod

!BOP

! !MODULE: MAPL_SunMod

!  !DESCRIPTION:   

!  This class is intended to manage the sun`s position and provide
!  the insolation at the top of the atmosphere.  The main method
!  is GEOS\_SunGetInsolation, which depends on an Orbit object.
!  The Orbit object defines this class and has public opaque type {\tt  GEOS\_SunOrbit}.
!  Methods are provided for creating it, destroying it, and making various queries.
!  \newline

! !USES:

  use ESMF
  use MAPL_ConstantsMod
  use MAPL_BaseMod
  use MAPL_IOMod
  use MAPL_CommsMod
  use MAPL_ErrorHandlingMod
  use netcdf

  implicit none
  private

! !PUBLIC MEMBER FUNCTIONS:

  public MAPL_SunOrbitCreate
  public MAPL_SunOrbitCreated
  public MAPL_SunOrbitDestroy
  public MAPL_SunOrbitQuery
  public MAPL_SunGetInsolation
  public MAPL_SunGetSolarConstant

! !PUBLIC TYPES:

  public MAPL_SunOrbit

!EOP

  integer, public, parameter :: MAPL_SunAutumnalEquinox = 1 
  integer, public, parameter :: MAPL_SunWinterSolstice  = 2 
  integer, public, parameter :: MAPL_SunVernalEquinox   = 3 
  integer, public, parameter :: MAPL_SunSummerSolstice  = 4 
  integer, public, parameter :: MAPL_SunDailyMean       = 5 
  integer, public, parameter :: MAPL_SunAnnualMean      = 6 


  interface MAPL_SunGetInsolation
     module procedure SOLAR_1D
     module procedure SOLAR_2D
     module procedure SOLAR_ARR_INT
  end interface

  interface MAPL_SunGetSolarConstant
     module procedure MAPL_SunGetSolarConstantByYearDoY
     module procedure MAPL_SunGetSolarConstantByTime
     module procedure MAPL_SunGetSolarConstantFromNetcdfFile
     module procedure MAPL_SunGetSolarConstantFromNRLFile
  end interface

  type MAPL_SunOrbit
     private
     type(ESMF_Clock)                 :: CLOCK
     real                             :: OB, ECC, PER, YEARLEN
     integer                          :: EQNX, YEARS_PER_CYCLE, DAYS_PER_CYCLE
     real, pointer, dimension(:)      :: ZC => null()
     real, pointer, dimension(:)      :: ZS => null()
     real, pointer, dimension(:)      :: PP => null()
     real, pointer, dimension(:)      :: TH => null()
     logical                          :: FIX_SUN
  end type MAPL_SunOrbit

contains

!==========================================================================

!BOPI

! !IROUTINE:  MAPL_SunOrbitCreate

! !DESCRIPTION:

!  Integrates the earth`s orbit and stores the necessary
!  parameters to easily compute the earth`s position for each day
!  of the full (usually 4-year) intercalation cycle. 
!  The orbital parameters are passed as arguments.
!  The full calendar intercalation cycle is obtained from the 
!  ESMF clock passed as an argument. This becomes the orbit`s
!  attached clock. Currently we assume a single intercalation.
!
!% \begin{itemize}
!%   \item[]
!\makebox[2in][l]{\bf \em CLOCK}
!                   \parbox[t]{4in}{The orbit will depend on the calendar in this clock
!                   This is used for the length of year, to set intercalation cycle}
!%   \item[]
!
!\makebox[2in][l]{\bf \em ECCENTRICITY}
!                   \parbox[t]{4in}{Eccentricity of the Earth`s orbit}
!%   \item[]
!
!\makebox[2in][l]{\bf \em PERIHELION}
!                   \parbox[t]{4in}{Longitude of perihelion, measured in degrees from 
!                   autumnal equinox in the direction of the Earth`s motion.}
!%   \item[]
!
!\makebox[2in][l]{\bf \em OBLIQUITY} 
!                   \parbox[t]{4in}{Tilt of the Earth`s rotation axis from a 
!                   normal to the plane of the orbit. In degrees.}
!
!%   \item[]
!\makebox[2in][l]{\bf \em EQUINOX}
!                   \parbox[t]{4in}{Day of year of vernal equinox.
!                   Equinox is assumed to occur at 0Z on this day
!                   on the first year of the cycle.}
!% \end{itemize}
!

! !INTERFACE:

type(MAPL_SunOrbit) function MAPL_SunOrbitCreate(CLOCK,       &
                                                 ECCENTRICITY,&
                                                 OBLIQUITY,   &
                                                 PERIHELION,  &
                                                 EQUINOX,     &
                                                 FIX_SUN,     &
                                                           RC )

! !ARGUMENTS:

 type(ESMF_Clock)  , intent(IN ) :: CLOCK
 real              , intent(IN ) :: ECCENTRICITY
 real              , intent(IN ) :: OBLIQUITY
 real              , intent(IN ) :: PERIHELION
 integer           , intent(IN ) :: EQUINOX
 logical, optional , intent(IN ) :: FIX_SUN
 integer, optional , intent(OUT) :: RC

!EOPI

! Locals

      character(len=ESMF_MAXSTR), parameter :: IAm = "SunOrbitCreate"

      integer :: YEARS_PER_CYCLE, DAYS_PER_CYCLE
      integer :: KM, K, KP
      real*8  :: T1, T2, T3, T4, FUN, Y, SOB, OMG, PRH, TT
      real*8  :: YEARLEN
      integer :: STATUS
      type(MAPL_SunOrbit) :: ORBIT

!  STATEMENT FUNCTION

      FUN(Y) = OMG*(1.0-ECCENTRICITY*cos(Y-PRH))**2

!MJS:  This needs to come from the calendar when the time manager works right.

      YEARLEN = 365.25

!  Factors involving the orbital parameters
!------------------------------------------

      OMG  = (2.0*MAPL_PI/YEARLEN) / (sqrt(1.-ECCENTRICITY**2)**3)
      PRH  = PERIHELION*(MAPL_PI/180.)
      SOB  = sin(OBLIQUITY*(MAPL_PI/180.))

!  Compute length of leap cycle
!------------------------------

      if(YEARLEN-int(YEARLEN) > 0.) then
       YEARS_PER_CYCLE = nint(1./(YEARLEN-int(YEARLEN)))
      else
       YEARS_PER_CYCLE = 1
      endif

      DAYS_PER_CYCLE=nint(YEARLEN*YEARS_PER_CYCLE)

      if(associated(ORBIT%TH)) deallocate(ORBIT%TH)
      allocate(ORBIT%TH(DAYS_PER_CYCLE), stat=status)
      _VERIFY(STATUS)
 
      if(associated(ORBIT%ZC)) deallocate(ORBIT%ZC)
      allocate(ORBIT%ZC(DAYS_PER_CYCLE), stat=status)
      _VERIFY(STATUS)

      if(associated(ORBIT%ZS)) deallocate(ORBIT%ZS)
      allocate(ORBIT%ZS(DAYS_PER_CYCLE), stat=status)
      _VERIFY(STATUS)

      if(associated(ORBIT%PP)) deallocate(ORBIT%PP)
      allocate(ORBIT%PP(DAYS_PER_CYCLE), stat=status)
      _VERIFY(STATUS)

      ORBIT%CLOCK           = CLOCK
      ORBIT%OB              = OBLIQUITY
      ORBIT%ECC             = ECCENTRICITY
      ORBIT%PER             = PERIHELION
      ORBIT%EQNX            = EQUINOX
      ORBIT%YEARLEN         = YEARLEN
      ORBIT%YEARS_PER_CYCLE = YEARS_PER_CYCLE
      ORBIT%DAYS_PER_CYCLE  = DAYS_PER_CYCLE

!   TH:   Orbit anomaly (radians)
!   ZS:   Sine of declination
!   ZC:   Cosine of declination
!   PP:   Inverse of square of earth-sun distance (1/(au**2))

!  Begin integration at vernal equinox

      KP           = EQUINOX
      TT           = 0.0
      ORBIT%ZS(KP) = sin(TT)*SOB
      ORBIT%ZC(KP) = sqrt(1.0-ORBIT%ZS(KP)**2)
      ORBIT%PP(KP) = ( ( 1.0-ECCENTRICITY*cos(TT-PRH) ) &
                     / ( 1.0-ECCENTRICITY**2          ) )**2
      ORBIT%TH(KP) = TT

!  Integrate orbit for entire leap cycle using Runge-Kutta

      do K=2,DAYS_PER_CYCLE
       T1 = FUN(TT       )
       T2 = FUN(TT+T1*0.5)
       T3 = FUN(TT+T2*0.5)
       T4 = FUN(TT+T3    )
       KP  = mod(KP,DAYS_PER_CYCLE) + 1
       TT  = TT + (T1 + 2.0*(T2 + T3) + T4) / 6.0
       ORBIT%ZS(KP) = sin(TT)*SOB
       ORBIT%ZC(KP) = sqrt(1.0-ORBIT%ZS(KP)**2)
       ORBIT%PP(KP) = ( ( 1.0-ECCENTRICITY*cos(TT-PRH) ) &
                      / ( 1.0-ECCENTRICITY**2          ) )**2
       ORBIT%TH(KP) = TT
      enddo

      if (present(FIX_SUN)) then
         ORBIT%FIX_SUN=FIX_SUN
      else
         ORBIT%FIX_SUN=.FALSE.
      end if

      MAPL_SunOrbitCreate = ORBIT

      _RETURN(ESMF_SUCCESS)

    end function MAPL_SunOrbitCreate

!==========================================================================

!BOP

! !IROUTINE:  MAPL_SunOrbitDestroy

! !DESCRIPTION:
! Destroys a {\tt GEOS\_SunOrbit} object, deallocating the space used to save the ephemeris.

! !INTERFACE:

    subroutine MAPL_SunOrbitDestroy(ORBIT, RC)

! !ARGUMENTS:

       type(MAPL_SunOrbit),    intent(INOUT) :: ORBIT
       integer, optional,      intent(  OUT) :: RC
!EOP

       character(len=ESMF_MAXSTR), parameter :: IAm = "SunOrbitDestroy"
       integer :: STATUS

       if(associated(ORBIT%TH)) deallocate(ORBIT%TH)
       if(associated(ORBIT%ZC)) deallocate(ORBIT%ZC)
       if(associated(ORBIT%ZS)) deallocate(ORBIT%ZS)
       if(associated(ORBIT%PP)) deallocate(ORBIT%PP)

       _RETURN(ESMF_SUCCESS)

     end subroutine MAPL_SunOrbitDestroy



!==========================================================================

!BOPI

! !IROUTINE:  MAPL_SunOrbitCreated

! !DESCRIPTION:

!  Returns {\tt .true.} if the given orbit object has been initilized.

! !INTERFACE:

       logical function  MAPL_SunOrbitCreated(ORBIT, RC)

! !ARGUMENTS:

       type(MAPL_SunOrbit),    intent(IN ) :: ORBIT
       integer, optional,      intent(OUT) :: RC

!EOPI

       character(len=ESMF_MAXSTR), parameter :: IAm = "SunOrbitCreated"
       integer :: STATUS

       MAPL_SunOrbitCreated = associated(ORBIT%TH)
       _RETURN(ESMF_SUCCESS)
       return

     end function MAPL_SunOrbitCreated

!==========================================================================

!BOPI

! !IROUTINE:  MAPL_SunOrbitQuery

! !DESCRIPTION:
!   Query for quantities in an orbit object.
!   Optionally returns the parameters of the orbit and its
!   associated {\tt ESMF\_Clock}. It fails
!   if the orbit has not been created.

! !INTERFACE:

subroutine  MAPL_SunOrbitQuery(ORBIT,           &
                               ECCENTRICITY,    &
                               OBLIQUITY,       &
                               PERIHELION,      &
                               EQUINOX,         &
                               YEAR_LENGTH,     &
                               YEARS_PER_CYCLE, &
                               DAYS_PER_CYCLE,  &
                               CLOCK,           & 
                               ZS,              &
                               ZC,              &
                                             RC )

! !ARGUMENTS:

       type(MAPL_SunOrbit),           intent(IN ) :: ORBIT
       real,                optional, intent(OUT) :: OBLIQUITY
       real,                optional, intent(OUT) :: ECCENTRICITY
       real,                optional, intent(OUT) :: PERIHELION
       real,                optional, intent(OUT) :: YEAR_LENGTH
       integer,             optional, intent(OUT) :: EQUINOX
       integer,             optional, intent(OUT) :: YEARS_PER_CYCLE
       integer,             optional, intent(OUT) :: DAYS_PER_CYCLE
       type(ESMF_Clock   ), optional, intent(OUT) :: CLOCK
       real,                optional, pointer, dimension(:) :: ZS
       real,                optional, pointer, dimension(:) :: ZC
       integer,             optional, intent(OUT) :: RC

!EOPI


       character(len=ESMF_MAXSTR), parameter :: IAm = "SunOrbitQuery"
       integer :: STATUS

       _ASSERT(MAPL_SunOrbitCreated(ORBIT,RC=STATUS),'needs informative message')

       if(present(CLOCK          )) CLOCK           = ORBIT%CLOCK
       if(present(OBLIQUITY      )) OBLIQUITY       = ORBIT%OB
       if(present(ECCENTRICITY   )) ECCENTRICITY    = ORBIT%ECC
       if(present(PERIHELION     )) PERIHELION      = ORBIT%PER
       if(present(EQUINOX        )) EQUINOX         = ORBIT%EQNX
       if(present(YEAR_LENGTH    )) YEAR_LENGTH     = ORBIT%YEARLEN
       if(present(DAYS_PER_CYCLE )) DAYS_PER_CYCLE  = ORBIT%DAYS_PER_CYCLE
       if(present(YEARS_PER_CYCLE)) YEARS_PER_CYCLE = ORBIT%YEARS_PER_CYCLE
       if(present(ZS             )) ZS => ORBIT%ZS
       if(present(ZC             )) ZC => ORBIT%ZC

       _RETURN(ESMF_SUCCESS)

     end subroutine MAPL_SunOrbitQuery

!==========================================================================

!BOPI

! !IROUTINE:  MAPL_SunGetInsolation

! !DESCRIPTION:  

! GEOS\_SunGetInsolation returns the cosine of the solar zenith angle and the 
! insolation at the top of the atmosphere for the given reference time, latitudes, 
! longitudes, and orbit.  It is overloaded to accept either 1d or 2d
! FORTRAN arrays or ESMF arrays of lats and lons and to produce the
! corresponding outputs.
!
! The reference time is obtained as follows. If CurrTime is specified, it is used;
! otherwise, if the optional clock is specified, it is set to the time
! on that clock. If neither currTime nor clock are given, the time on the attached
! clock is used.
!
! If the optional time interval is specified, the return values are averages
! over that interval following the reference time. In this case, the cosine of
! the solar zenith angle is an insolation-weighted average. The straight average
! of the zenith angle and the average over the daylight part of the
! interval (ZTHB,ZTHD) and the values at the beginning and end of the interval
! (ZTH1,ZTHN) are also optionally available. The last two are supported only for
! first two (non-ESMF) overloads.
!
! If the interval is not specified, the values are instantaneous values valid at
! the reference time.
! 
! The optional {\tt TIME} argument is used to return some specialized
! insolations. For example, the orbit at any of four Equinox or Solstice
! positions. If {\tt TIME} is present, only the time of day is used from the clock,
! and a time interval, if specified, must be less than 24 hours. It can also be
! used to return daily-mean insolation for the date on the clock, or the annual-mean
! insolation for the year on the clock.
!
! The {\tt TIME} argument can be any of the following:
!\begin{verbatim}
!      MAPL_SunAutumnalEquinox 
!      MAPL_SunWinterSolstice  
!      MAPL_SunVernalEquinox   
!      MAPL_SunSummerSolstice  
!      MAPL_SunDailyMean       
!      MAPL_SunAnnualMean      
!\end{verbatim}

! !INTERFACE:

!   subroutine MAPL_SunGetInsolation(LONS, LATS, ORBIT,ZTH,SLR,INTV,CLOCK, &
!                                    TIME,currTime,DIST,ZTHB,ZTHD,ZTH1,ZTHN, RC)

! !ARGUMENTS:

!      type (MAPL_SunOrbit),               intent(IN ) :: ORBIT
!      TYPE             ,                  intent(IN ) :: LATS
!      TYPE             ,                  intent(IN ) :: LONS
!      TYPE             ,                  intent(OUT) :: ZTH
!      TYPE             ,                  intent(OUT) :: SLR
!      type (ESMF_TimeInterval), optional, intent(IN ) :: INTV
!      type (ESMF_Clock),        optional, intent(IN ) :: CLOCK
!      integer,                  optional, intent(IN ) :: TIME
!      type (ESMF_Time),         optional, intent(IN ) :: currTime
!      real,                     optional, intent(OUT) :: DIST
!      TYPE             ,        optional, intent(OUT) :: ZTHB
!      TYPE             ,        optional, intent(OUT) :: ZTHD
!      TYPE             ,        optional, intent(OUT) :: ZTH1
!      TYPE             ,        optional, intent(OUT) :: ZTHN
!      integer,                  optional, intent(OUT) :: RC
!\end{verbatim}
! where we currently support three overloads for {\tt TYPE} : 
!\begin{verbatim}
!  type (ESMF_Array)
!  real, dimension(:)
!  real, dimension(:,:)
!EOPI

#undef  DIMENSIONS
#define DIMENSIONS (:)
#define THE_SIZE   (size(LONS,1))
      recursive subroutine SOLAR_1D(LONS, LATS, ORBIT,ZTH,SLR,INTV,CLOCK, &
                                    TIME,currTime,DIST,ZTHB,ZTHD,ZTH1,ZTHN,RC)
#include "sun.H"
      end subroutine SOLAR_1D

!==========================================================================

#undef  DIMENSIONS
#undef  THE_SIZE
#define DIMENSIONS (:,:)
#define THE_SIZE   (size(LONS,1),size(LONS,2))
      recursive subroutine SOLAR_2D(LONS, LATS, ORBIT,ZTH,SLR,INTV,CLOCK, &
                                    TIME,currTime,DIST,ZTHB,ZTHD,ZTH1,ZTHN,RC)
#include "sun.H"
      end subroutine SOLAR_2D
#undef  DIMENSIONS
#undef  THE_SIZE

!BOP



!EOP


!==========================================================================

      subroutine SOLAR_ARR_INT(LONS, LATS, ORBIT, ZTH, SLR, INTV, CLOCK,  &
                               TIME, currTime, DIST, ZTHB, ZTHD,  RC)

      type (MAPL_SunOrbit),               intent(IN ) :: ORBIT
      type (ESMF_Array),                  intent(IN ) :: LATS
      type (ESMF_Array),                  intent(IN ) :: LONS
      type (ESMF_Array),                  intent(OUT) :: ZTH
      type (ESMF_Array),                  intent(OUT) :: SLR
      type (ESMF_TimeInterval), optional, intent(INout) :: INTV
      type (ESMF_Clock),        optional, intent(IN ) :: CLOCK
      type (ESMF_Time),         optional, intent(IN ) :: currTime
      integer,                  optional, intent(IN ) :: TIME
      real,                     optional, intent(OUT) :: DIST
      type (ESMF_Array),        optional, intent(OUT) :: ZTHB
      type (ESMF_Array),        optional, intent(OUT) :: ZTHD
      integer,                  optional, intent(OUT) :: RC

!   Locals

      
      character(len=ESMF_MAXSTR)      :: IAm = "SunGetInsolationArr"
      integer                         :: STATUS

      real, pointer, dimension (:  )  :: LONS1, LATS1, ZTH1, SLR1, ZTHB1, ZTHD1
      real, pointer, dimension (:,:)  :: LONS2, LATS2, ZTH2, SLR2, ZTHB2, ZTHD2

      integer                         :: RANK

!   Begin

      call ESMF_ArrayGet(LONS, RANK=RANK, RC=STATUS)
      _VERIFY(STATUS)

      select case(RANK)

      case(1)
         call ESMF_ArrayGet(LATS, localDE=0, farrayptr=LATS1, RC=STATUS)
         _VERIFY(STATUS)
         call ESMF_ArrayGet(LONS,localDE=0, farrayptr=LONS1, RC=STATUS)
         _VERIFY(STATUS)
         call ESMF_ArrayGet(ZTH ,localDE=0, farrayptr=ZTH1, RC=STATUS)
         _VERIFY(STATUS)
         call ESMF_ArrayGet(SLR ,localDE=0, farrayptr=SLR1, RC=STATUS)
         _VERIFY(STATUS)

         if(present(ZTHB) .and. present(ZTHD)) then
            call ESMF_ArrayGet(ZTHB ,localDE=0, farrayptr=ZTHB1 ,RC=STATUS)
            _VERIFY(STATUS)
            call ESMF_ArrayGet(ZTHD ,localDE=0, farrayptr=ZTHD1 ,RC=STATUS)
            _VERIFY(STATUS)
            call MAPL_SunGetInsolation(LONS1,LATS1,ORBIT,ZTH1,SLR1,INTV,CLOCK,&
                 TIME,currTime,DIST=DIST,ZTHB=ZTHB1,ZTHD=ZTHD1,RC=STATUS)
         elseif(present(ZTHB)) then
            call ESMF_ArrayGet(ZTHB ,localDE=0, farrayptr=ZTHB1 ,RC=STATUS)
            _VERIFY(STATUS)
            call MAPL_SunGetInsolation(LONS1,LATS1,ORBIT,ZTH1,SLR1,INTV,CLOCK,&
                 TIME,currTime,DIST=DIST,ZTHB=ZTHB1,RC=STATUS)
         elseif(present(ZTHD)) then
            call ESMF_ArrayGet(ZTHD ,localDE=0, farrayptr=ZTHD1 ,RC=STATUS)
            _VERIFY(STATUS)
            call MAPL_SunGetInsolation(LONS1,LATS1,ORBIT,ZTH1,SLR1,INTV,CLOCK,&
                 TIME,currTime,DIST=DIST,ZTHD=ZTHD1,RC=STATUS)
         else
            call MAPL_SunGetInsolation(LONS1,LATS1,ORBIT,ZTH1,SLR1,INTV,CLOCK,&
                 TIME,currTime,DIST=DIST,RC=STATUS)
         endif
         _VERIFY(STATUS)

      case(2)
         call ESMF_ArrayGet(LATS,localDE=0, farrayptr=LATS2,RC=STATUS)
         _VERIFY(STATUS)
         call ESMF_ArrayGet(LONS,localDE=0, farrayptr=LONS2,RC=STATUS)
         _VERIFY(STATUS)
         call ESMF_ArrayGet(ZTH ,localDE=0, farrayptr=ZTH2 ,RC=STATUS)
         _VERIFY(STATUS)
         call ESMF_ArrayGet(SLR ,localDE=0, farrayptr=SLR2 ,RC=STATUS)
         _VERIFY(STATUS)

         if(present(ZTHB) .and. present(ZTHD)) then
            call ESMF_ArrayGet(ZTHB ,localDE=0, farrayptr=ZTHB2 ,RC=STATUS)
            _VERIFY(STATUS)
            call ESMF_ArrayGet(ZTHD ,localDE=0, farrayptr=ZTHD2 ,RC=STATUS)
            _VERIFY(STATUS)
            call MAPL_SunGetInsolation(LONS2,LATS2,ORBIT,ZTH2,SLR2,INTV,CLOCK,&
                 TIME,currTime,DIST=DIST,ZTHB=ZTHB2,ZTHD=ZTHD2,RC=STATUS)
         elseif(present(ZTHB)) then
            call ESMF_ArrayGet(ZTHB ,localDE=0, farrayptr=ZTHB2 ,RC=STATUS)
            _VERIFY(STATUS)
            call MAPL_SunGetInsolation(LONS2,LATS2,ORBIT,ZTH2,SLR2,INTV,CLOCK,&
                 TIME,currTime,DIST=DIST,ZTHB=ZTHB2,RC=STATUS)
         elseif(present(ZTHD)) then
            call ESMF_ArrayGet(ZTHD ,localDE=0, farrayptr=ZTHD2 ,RC=STATUS)
            _VERIFY(STATUS)
            call MAPL_SunGetInsolation(LONS2,LATS2,ORBIT,ZTH2,SLR2,INTV,CLOCK,&
                 TIME,currTime,DIST=DIST,ZTHD=ZTHD2,RC=STATUS)
         else
            call MAPL_SunGetInsolation(LONS2,LATS2,ORBIT,ZTH2,SLR2,INTV,CLOCK,&
                 TIME,currTime,DIST=DIST,RC=STATUS)
         endif
         _VERIFY(STATUS)

      case default
         _RETURN(ESMF_FAILURE)

      end select

      _RETURN(ESMF_SUCCESS)

    end subroutine SOLAR_ARR_INT

    subroutine GETIDAY(IDAY,TIME,ORBIT,RC)
      integer,              intent(OUT) :: IDAY
      integer,              intent(IN ) :: TIME
      type(MAPL_SunORBIT),  intent(IN ) :: ORBIT
      integer, optional,    intent(OUT) :: RC

      character(len=ESMF_MAXSTR)      :: IAm = "GetIDAY"
      integer                         :: STATUS

      real :: ANOMALY

      select case(TIME)
      case (MAPL_SunAutumnalEquinox)
         ANOMALY = MAPL_PI
      case (MAPL_SunWinterSolstice )
         ANOMALY = (MAPL_PI*3.0)/2.0
      case (MAPL_SunVernalEquinox  )
         ANOMALY = 0.0
      case (MAPL_SunSummerSolstice )
         ANOMALY = MAPL_PI/2.0
      case  default
         _RETURN(ESMF_FAILURE)
      end select

      do IDAY=1,ORBIT%DAYS_PER_CYCLE-1
         if(ORBIT%TH(IDAY)<=ANOMALY .and. ORBIT%TH(IDAY+1)>ANOMALY) then
            _RETURN(ESMF_SUCCESS)
         end if
      end do

      _RETURN(ESMF_FAILURE)
    end subroutine GETIDAY
!==========================================================================
    
    subroutine MAPL_SunGetSolarConstantByTime(Time,SC,HK,rc)

      type (ESMF_Time),  intent(INOUT) :: Time
      real,              intent(OUT)   :: SC
      real,   optional,  intent(OUT)   :: HK(:)
      integer, optional, intent(OUT)   :: rc

      integer    :: YY, DOY
      integer    :: STATUS
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_SunGetSolarConstantByTime"

      call ESMF_TimeGet (TIME, YY=YY, DayOfYear=DOY, RC=STATUS)
      _VERIFY(STATUS)

      call MAPL_SunGetSolarConstantByYearDoY(YY,DOY,SC,HK, RC=STATUS)
      _VERIFY(STATUS)

      _RETURN(ESMF_SUCCESS)
    end subroutine MAPL_SunGetSolarConstantByTime

!-------------------------------------------------------------------------
!NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1, GEOS/DAS!
!-------------------------------------------------------------------------
!BOP
!
! !INTERFACE:

   subroutine MAPL_SunGetSolarConstantByYearDoY(year,dayofyear,SC,HK, rc)

! !DESCRIPTION:
!
!  Given the year and day-of-year, this function returns the CMIP5 solar constant
!  and the 8 band fractions for the Chou solar code. These are based on annual
!  values from 1610 to 2008 and a repeating cycle after 2008. For dates prior
!  tp 1 January 1610, it returns the value at the start of 1610.
!
!  DayOfYear is expected to have a value of 1.00 at 0:00 UTC Jan 1 and leap years
!  are acounted for by repeating the 366th day on January first of the following year.
!
!  The SC values have been multiplied by .9965 to calibrate to the SOURCE/TIM scale.
!  The SC values include the "background" variation.
!
! !BUGS:
! 
!  Band values for RRTMG not implemented
!
!EOP
! ---------------------------------------------------------------------------------

   integer,           intent(IN)  :: Year
   integer,           intent(IN)  :: DayOfYear
   real,              intent(OUT) :: SC
   real,    optional, intent(OUT) :: HK(:)
   integer, optional, intent(OUT) :: rc

   real    :: F
   integer :: i1,i2,Current, STATUS
   character(len=ESMF_MAXSTR) :: IAm = "MAPL_SunGetSolarConstantByYearDoY"

   integer, parameter :: firstYear = 1610
   integer, parameter :: finalYear = 2008
   integer, parameter :: SolCycle  = 12  ! Cycle 23 was 12.2 years

   real, parameter ::  SO(firstYear:finalYear) = (/  &
         1360.768, 1360.751, 1361.147, 1361.259, 1361.333,&
         1361.051, 1360.636, 1360.510, 1360.500, 1360.590,&
         1360.586, 1360.582, 1360.578, 1360.574, 1360.534,&
         1360.751, 1360.640, 1360.563, 1360.573, 1360.514,&
         1360.365, 1360.373, 1360.340, 1360.438, 1360.358,&
         1360.363, 1360.330, 1360.327, 1360.792, 1360.844,&
         1360.415, 1360.304, 1360.622, 1360.404, 1360.349,&
         1360.268, 1360.267, 1360.267, 1360.265, 1360.262,&
         1360.261, 1360.261, 1360.288, 1360.266, 1360.258,&
         1360.248, 1360.247, 1360.242, 1360.237, 1360.235,&
         1360.248, 1360.240, 1360.235, 1360.235, 1360.235,&
         1360.235, 1360.235, 1360.235, 1360.235, 1360.235,&
         1360.235, 1360.241, 1360.237, 1360.235, 1360.236,&
         1360.234, 1360.246, 1360.236, 1360.235, 1360.234,&
         1360.239, 1360.234, 1360.234, 1360.234, 1360.244,&
         1360.234, 1360.238, 1360.235, 1360.238, 1360.237,&
         1360.237, 1360.238, 1360.240, 1360.240, 1360.240,&
         1360.241, 1360.240, 1360.240, 1360.240, 1360.241,&
         1360.244, 1360.246, 1360.249, 1360.267, 1360.282,&
         1360.297, 1360.286, 1360.302, 1360.288, 1360.284,&
         1360.286, 1360.296, 1360.302, 1360.308, 1360.325,&
         1360.352, 1360.391, 1360.451, 1360.393, 1360.564,&
         1360.507, 1360.510, 1360.443, 1360.406, 1360.490,&
         1360.479, 1360.648, 1360.654, 1360.844, 1360.569,&
         1360.881, 1360.403, 1360.530, 1360.420, 1360.430,&
         1360.560, 1360.769, 1360.604, 1360.558, 1360.800,&
         1360.503, 1360.831, 1360.546, 1360.491, 1360.436,&
         1360.438, 1360.444, 1360.452, 1360.875, 1360.895,&
         1360.859, 1360.689, 1360.661, 1360.643, 1360.556,&
         1360.543, 1360.571, 1360.694, 1360.807, 1360.870,&
         1360.846, 1361.010, 1360.872, 1360.802, 1360.778,&
         1360.628, 1360.597, 1360.802, 1361.054, 1361.238,&
         1361.241, 1361.135, 1361.063, 1360.845, 1360.807,&
         1360.670, 1360.725, 1360.886, 1361.115, 1361.173,&
         1360.995, 1361.106, 1360.842, 1360.749, 1360.618,&
         1360.677, 1360.988, 1361.153, 1361.100, 1361.079,&
         1360.980, 1360.830, 1360.817, 1360.796, 1360.702,&
         1360.586, 1360.556, 1360.503, 1360.478, 1360.485,&
         1360.525, 1360.796, 1360.675, 1360.543, 1360.547,&
         1360.557, 1360.460, 1360.393, 1360.368, 1360.340,&
         1360.332, 1360.344, 1360.385, 1360.434, 1360.457,&
         1360.512, 1360.608, 1360.581, 1360.533, 1360.518,&
         1360.463, 1360.441, 1360.465, 1360.470, 1360.511,&
         1360.584, 1360.687, 1360.798, 1360.885, 1360.903,&
         1360.943, 1360.779, 1360.678, 1360.582, 1360.618,&
         1360.878, 1361.258, 1361.331, 1361.119, 1361.037,&
         1360.921, 1360.770, 1360.715, 1360.645, 1360.675,&
         1360.803, 1360.900, 1361.001, 1361.197, 1361.188,&
         1360.990, 1360.994, 1360.915, 1360.805, 1360.682,&
         1360.604, 1360.606, 1360.715, 1360.908, 1361.125,&
         1361.191, 1361.082, 1360.932, 1360.849, 1360.787,&
         1360.696, 1360.633, 1360.581, 1360.740, 1360.976,&
         1361.213, 1361.154, 1361.107, 1360.907, 1360.779,&
         1360.631, 1360.581, 1360.581, 1360.552, 1360.575,&
         1360.721, 1360.865, 1360.888, 1360.935, 1360.837,&
         1360.728, 1360.563, 1360.520, 1360.484, 1360.456,&
         1360.491, 1360.741, 1360.839, 1360.980, 1361.075,&
         1360.959, 1360.779, 1360.634, 1360.611, 1360.559,&
         1360.529, 1360.451, 1360.459, 1360.669, 1360.938,&
         1360.750, 1360.946, 1360.830, 1360.895, 1360.785,&
         1360.652, 1360.569, 1360.523, 1360.539, 1360.605,&
         1360.909, 1361.118, 1361.267, 1361.229, 1361.000,&
         1360.838, 1360.733, 1360.616, 1360.648, 1360.679,&
         1360.882, 1360.983, 1361.166, 1361.044, 1361.003,&
         1360.985, 1360.864, 1360.757, 1360.637, 1360.748,&
         1360.964, 1361.352, 1361.286, 1361.222, 1361.206,&
         1361.143, 1361.065, 1360.962, 1360.805, 1360.834,&
         1361.100, 1361.198, 1361.437, 1361.567, 1361.474,&
         1361.232, 1360.996, 1360.987, 1360.849, 1360.877,&
         1360.997, 1361.529, 1361.885, 1361.850, 1361.601,&
         1361.495, 1361.139, 1360.968, 1360.916, 1360.918,&
         1360.954, 1361.137, 1361.333, 1361.383, 1361.466,&
         1361.461, 1361.177, 1361.271, 1361.019, 1360.947,&
         1360.755, 1360.866, 1361.053, 1361.493, 1361.852,&
         1361.865, 1361.912, 1361.504, 1361.418, 1361.030,&
         1360.862, 1360.858, 1361.010, 1361.301, 1361.865,&
         1361.770, 1361.663, 1361.520, 1361.247, 1361.017,&
         1360.920, 1360.832, 1360.960, 1361.321, 1361.603,&
         1361.882, 1361.819, 1361.897, 1361.448, 1361.267,&
         1361.074, 1361.030, 1360.944, 1360.912 /)

   real, parameter :: CHOUBAND1(firstYear:finalYear) = (/ &
         0.00516,   0.00515,   0.00519,   0.00519,   0.00520,&
         0.00518,   0.00515,   0.00514,   0.00514,   0.00514,&
         0.00514,   0.00514,   0.00514,   0.00514,   0.00514,&
         0.00516,   0.00515,   0.00514,   0.00514,   0.00514,&
         0.00513,   0.00513,   0.00513,   0.00514,   0.00513,&
         0.00513,   0.00513,   0.00513,   0.00517,   0.00517,&
         0.00514,   0.00513,   0.00515,   0.00514,   0.00513,&
         0.00513,   0.00513,   0.00513,   0.00513,   0.00513,&
         0.00513,   0.00513,   0.00513,   0.00513,   0.00513,&
         0.00513,   0.00513,   0.00513,   0.00513,   0.00513,&
         0.00513,   0.00513,   0.00513,   0.00513,   0.00513,&
         0.00513,   0.00513,   0.00513,   0.00513,   0.00513,&
         0.00513,   0.00513,   0.00513,   0.00513,   0.00513,&
         0.00513,   0.00513,   0.00513,   0.00513,   0.00513,&
         0.00513,   0.00513,   0.00513,   0.00513,   0.00513,&
         0.00513,   0.00513,   0.00513,   0.00513,   0.00513,&
         0.00513,   0.00513,   0.00513,   0.00513,   0.00513,&
         0.00513,   0.00513,   0.00513,   0.00513,   0.00513,&
         0.00513,   0.00513,   0.00513,   0.00513,   0.00513,&
         0.00513,   0.00513,   0.00513,   0.00513,   0.00513,&
         0.00513,   0.00513,   0.00513,   0.00513,   0.00513,&
         0.00513,   0.00513,   0.00514,   0.00513,   0.00515,&
         0.00514,   0.00514,   0.00514,   0.00513,   0.00514,&
         0.00514,   0.00515,   0.00515,   0.00517,   0.00514,&
         0.00517,   0.00513,   0.00514,   0.00513,   0.00513,&
         0.00514,   0.00516,   0.00515,   0.00514,   0.00516,&
         0.00514,   0.00516,   0.00514,   0.00514,   0.00513,&
         0.00513,   0.00513,   0.00513,   0.00517,   0.00517,&
         0.00517,   0.00515,   0.00515,   0.00515,   0.00514,&
         0.00514,   0.00514,   0.00515,   0.00516,   0.00516,&
         0.00516,   0.00517,   0.00516,   0.00516,   0.00515,&
         0.00514,   0.00514,   0.00516,   0.00518,   0.00519,&
         0.00519,   0.00518,   0.00518,   0.00516,   0.00515,&
         0.00514,   0.00515,   0.00516,   0.00518,   0.00518,&
         0.00517,   0.00518,   0.00516,   0.00515,   0.00514,&
         0.00515,   0.00517,   0.00518,   0.00518,   0.00518,&
         0.00517,   0.00516,   0.00516,   0.00516,   0.00515,&
         0.00514,   0.00514,   0.00514,   0.00514,   0.00514,&
         0.00514,   0.00516,   0.00515,   0.00514,   0.00514,&
         0.00514,   0.00514,   0.00513,   0.00513,   0.00513,&
         0.00513,   0.00513,   0.00513,   0.00514,   0.00514,&
         0.00514,   0.00515,   0.00515,   0.00514,   0.00514,&
         0.00514,   0.00513,   0.00514,   0.00513,   0.00514,&
         0.00514,   0.00515,   0.00516,   0.00517,   0.00517,&
         0.00517,   0.00516,   0.00515,   0.00514,   0.00514,&
         0.00516,   0.00519,   0.00520,   0.00518,   0.00517,&
         0.00517,   0.00515,   0.00515,   0.00514,   0.00515,&
         0.00516,   0.00516,   0.00517,   0.00519,   0.00518,&
         0.00517,   0.00517,   0.00517,   0.00516,   0.00515,&
         0.00514,   0.00514,   0.00515,   0.00516,   0.00518,&
         0.00519,   0.00518,   0.00517,   0.00516,   0.00516,&
         0.00515,   0.00514,   0.00514,   0.00515,   0.00517,&
         0.00519,   0.00518,   0.00518,   0.00517,   0.00516,&
         0.00515,   0.00514,   0.00514,   0.00514,   0.00514,&
         0.00515,   0.00516,   0.00516,   0.00517,   0.00517,&
         0.00516,   0.00515,   0.00514,   0.00514,   0.00514,&
         0.00514,   0.00516,   0.00517,   0.00518,   0.00518,&
         0.00517,   0.00516,   0.00515,   0.00515,   0.00514,&
         0.00514,   0.00514,   0.00514,   0.00515,   0.00516,&
         0.00517,   0.00517,   0.00517,   0.00517,   0.00516,&
         0.00515,   0.00514,   0.00514,   0.00514,   0.00515,&
         0.00517,   0.00518,   0.00520,   0.00519,   0.00518,&
         0.00516,   0.00516,   0.00515,   0.00515,   0.00515,&
         0.00517,   0.00518,   0.00518,   0.00519,   0.00518,&
         0.00517,   0.00516,   0.00515,   0.00515,   0.00515,&
         0.00517,   0.00519,   0.00521,   0.00520,   0.00520,&
         0.00518,   0.00517,   0.00517,   0.00516,   0.00516,&
         0.00517,   0.00520,   0.00522,   0.00522,   0.00522,&
         0.00519,   0.00518,   0.00517,   0.00516,   0.00516,&
         0.00517,   0.00522,   0.00525,   0.00525,   0.00523,&
         0.00521,   0.00517,   0.00517,   0.00516,   0.00516,&
         0.00516,   0.00517,   0.00520,   0.00520,   0.00520,&
         0.00521,   0.00518,   0.00519,   0.00517,   0.00516,&
         0.00515,   0.00516,   0.00517,   0.00520,   0.00523,&
         0.00523,   0.00523,   0.00522,   0.00519,   0.00517,&
         0.00516,   0.00516,   0.00516,   0.00519,   0.00523,&
         0.00522,   0.00522,   0.00520,   0.00518,   0.00517,&
         0.00516,   0.00515,   0.00516,   0.00518,   0.00520,&
         0.00522,   0.00522,   0.00522,   0.00519,   0.00518,&
         0.00517,   0.00516,   0.00516,   0.00516     /)

   real, parameter :: CHOUBAND2(firstYear:finalYear) = (/ &
         0.00567,   0.00567,   0.00569,   0.00570,   0.00570,&
         0.00569,   0.00567,   0.00566,   0.00566,   0.00567,&
         0.00567,   0.00567,   0.00567,   0.00567,   0.00566,&
         0.00567,   0.00567,   0.00567,   0.00567,   0.00566,&
         0.00566,   0.00566,   0.00566,   0.00566,   0.00566,&
         0.00566,   0.00566,   0.00566,   0.00568,   0.00568,&
         0.00566,   0.00566,   0.00567,   0.00566,   0.00566,&
         0.00566,   0.00566,   0.00566,   0.00566,   0.00566,&
         0.00566,   0.00566,   0.00566,   0.00566,   0.00566,&
         0.00566,   0.00566,   0.00566,   0.00566,   0.00566,&
         0.00566,   0.00566,   0.00566,   0.00566,   0.00566,&
         0.00566,   0.00566,   0.00566,   0.00566,   0.00566,&
         0.00566,   0.00566,   0.00566,   0.00566,   0.00566,&
         0.00566,   0.00566,   0.00566,   0.00566,   0.00566,&
         0.00566,   0.00566,   0.00566,   0.00566,   0.00566,&
         0.00566,   0.00566,   0.00566,   0.00566,   0.00566,&
         0.00566,   0.00566,   0.00566,   0.00566,   0.00566,&
         0.00566,   0.00566,   0.00566,   0.00566,   0.00566,&
         0.00566,   0.00566,   0.00566,   0.00566,   0.00566,&
         0.00566,   0.00566,   0.00566,   0.00566,   0.00566,&
         0.00566,   0.00566,   0.00566,   0.00566,   0.00566,&
         0.00566,   0.00566,   0.00566,   0.00566,   0.00567,&
         0.00567,   0.00567,   0.00566,   0.00566,   0.00566,&
         0.00566,   0.00567,   0.00567,   0.00568,   0.00567,&
         0.00568,   0.00566,   0.00567,   0.00566,   0.00566,&
         0.00567,   0.00568,   0.00567,   0.00567,   0.00568,&
         0.00566,   0.00568,   0.00567,   0.00566,   0.00566,&
         0.00566,   0.00566,   0.00566,   0.00568,   0.00568,&
         0.00568,   0.00567,   0.00567,   0.00567,   0.00566,&
         0.00566,   0.00566,   0.00567,   0.00568,   0.00568,&
         0.00568,   0.00568,   0.00568,   0.00567,   0.00567,&
         0.00567,   0.00566,   0.00567,   0.00569,   0.00569,&
         0.00569,   0.00569,   0.00569,   0.00568,   0.00567,&
         0.00567,   0.00567,   0.00568,   0.00569,   0.00569,&
         0.00568,   0.00569,   0.00568,   0.00567,   0.00567,&
         0.00567,   0.00568,   0.00569,   0.00569,   0.00569,&
         0.00568,   0.00568,   0.00568,   0.00567,   0.00567,&
         0.00567,   0.00567,   0.00566,   0.00566,   0.00566,&
         0.00566,   0.00568,   0.00567,   0.00567,   0.00567,&
         0.00567,   0.00566,   0.00566,   0.00566,   0.00566,&
         0.00566,   0.00566,   0.00566,   0.00566,   0.00566,&
         0.00566,   0.00567,   0.00567,   0.00567,   0.00566,&
         0.00566,   0.00566,   0.00566,   0.00566,   0.00566,&
         0.00567,   0.00567,   0.00568,   0.00568,   0.00568,&
         0.00568,   0.00567,   0.00567,   0.00566,   0.00567,&
         0.00568,   0.00569,   0.00570,   0.00569,   0.00568,&
         0.00568,   0.00567,   0.00567,   0.00567,   0.00567,&
         0.00567,   0.00568,   0.00568,   0.00569,   0.00569,&
         0.00568,   0.00568,   0.00568,   0.00567,   0.00567,&
         0.00567,   0.00567,   0.00567,   0.00568,   0.00569,&
         0.00569,   0.00569,   0.00568,   0.00568,   0.00567,&
         0.00567,   0.00567,   0.00566,   0.00567,   0.00568,&
         0.00569,   0.00569,   0.00569,   0.00568,   0.00567,&
         0.00567,   0.00567,   0.00567,   0.00566,   0.00566,&
         0.00567,   0.00568,   0.00568,   0.00568,   0.00568,&
         0.00568,   0.00567,   0.00567,   0.00566,   0.00566,&
         0.00567,   0.00568,   0.00568,   0.00569,   0.00569,&
         0.00568,   0.00568,   0.00567,   0.00567,   0.00567,&
         0.00567,   0.00566,   0.00566,   0.00567,   0.00568,&
         0.00568,   0.00568,   0.00568,   0.00568,   0.00568,&
         0.00567,   0.00567,   0.00567,   0.00567,   0.00567,&
         0.00568,   0.00569,   0.00570,   0.00569,   0.00569,&
         0.00568,   0.00567,   0.00567,   0.00567,   0.00567,&
         0.00568,   0.00569,   0.00569,   0.00569,   0.00569,&
         0.00568,   0.00568,   0.00567,   0.00567,   0.00567,&
         0.00568,   0.00570,   0.00570,   0.00570,   0.00570,&
         0.00569,   0.00568,   0.00568,   0.00568,   0.00567,&
         0.00568,   0.00570,   0.00571,   0.00571,   0.00571,&
         0.00569,   0.00569,   0.00568,   0.00568,   0.00567,&
         0.00568,   0.00571,   0.00573,   0.00573,   0.00572,&
         0.00570,   0.00569,   0.00568,   0.00568,   0.00568,&
         0.00568,   0.00569,   0.00570,   0.00570,   0.00570,&
         0.00570,   0.00569,   0.00569,   0.00568,   0.00568,&
         0.00567,   0.00568,   0.00568,   0.00570,   0.00572,&
         0.00572,   0.00572,   0.00571,   0.00570,   0.00568,&
         0.00568,   0.00567,   0.00568,   0.00570,   0.00572,&
         0.00571,   0.00571,   0.00570,   0.00569,   0.00568,&
         0.00568,   0.00567,   0.00568,   0.00569,   0.00570,&
         0.00571,   0.00571,   0.00572,   0.00570,   0.00569,&
         0.00568,   0.00568,   0.00568,   0.00567    /)

   real, parameter :: CHOUBAND3(firstYear:finalYear) =  (/ &
         0.01202,   0.01202,   0.01203,   0.01203,   0.01203,&
         0.01202,   0.01201,   0.01201,   0.01201,   0.01201,&
         0.01201,   0.01201,   0.01201,   0.01201,   0.01201,&
         0.01202,   0.01202,   0.01201,   0.01201,   0.01201,&
         0.01201,   0.01201,   0.01201,   0.01201,   0.01201,&
         0.01201,   0.01201,   0.01201,   0.01202,   0.01202,&
         0.01201,   0.01201,   0.01202,   0.01201,   0.01201,&
         0.01201,   0.01201,   0.01201,   0.01201,   0.01201,&
         0.01201,   0.01201,   0.01201,   0.01201,   0.01201,&
         0.01201,   0.01201,   0.01201,   0.01201,   0.01201,&
         0.01201,   0.01201,   0.01201,   0.01201,   0.01201,&
         0.01201,   0.01201,   0.01201,   0.01201,   0.01201,&
         0.01201,   0.01201,   0.01201,   0.01201,   0.01201,&
         0.01201,   0.01201,   0.01201,   0.01201,   0.01201,&
         0.01201,   0.01201,   0.01201,   0.01201,   0.01201,&
         0.01201,   0.01201,   0.01201,   0.01201,   0.01201,&
         0.01201,   0.01201,   0.01201,   0.01201,   0.01201,&
         0.01201,   0.01201,   0.01201,   0.01201,   0.01201,&
         0.01201,   0.01201,   0.01201,   0.01201,   0.01201,&
         0.01201,   0.01201,   0.01201,   0.01201,   0.01201,&
         0.01201,   0.01201,   0.01201,   0.01201,   0.01201,&
         0.01201,   0.01201,   0.01201,   0.01201,   0.01201,&
         0.01201,   0.01201,   0.01201,   0.01201,   0.01201,&
         0.01201,   0.01202,   0.01202,   0.01202,   0.01201,&
         0.01202,   0.01201,   0.01201,   0.01201,   0.01201,&
         0.01201,   0.01202,   0.01201,   0.01201,   0.01202,&
         0.01201,   0.01202,   0.01201,   0.01201,   0.01201,&
         0.01201,   0.01201,   0.01201,   0.01202,   0.01202,&
         0.01202,   0.01202,   0.01202,   0.01202,   0.01201,&
         0.01201,   0.01201,   0.01202,   0.01202,   0.01202,&
         0.01202,   0.01202,   0.01202,   0.01202,   0.01202,&
         0.01201,   0.01201,   0.01202,   0.01202,   0.01203,&
         0.01203,   0.01203,   0.01202,   0.01202,   0.01202,&
         0.01201,   0.01202,   0.01202,   0.01203,   0.01203,&
         0.01202,   0.01202,   0.01202,   0.01202,   0.01201,&
         0.01202,   0.01202,   0.01203,   0.01203,   0.01202,&
         0.01202,   0.01202,   0.01202,   0.01202,   0.01202,&
         0.01201,   0.01201,   0.01201,   0.01201,   0.01201,&
         0.01201,   0.01202,   0.01202,   0.01201,   0.01201,&
         0.01201,   0.01201,   0.01201,   0.01201,   0.01201,&
         0.01201,   0.01201,   0.01201,   0.01201,   0.01201,&
         0.01201,   0.01201,   0.01201,   0.01201,   0.01201,&
         0.01201,   0.01201,   0.01201,   0.01201,   0.01201,&
         0.01201,   0.01202,   0.01202,   0.01202,   0.01202,&
         0.01202,   0.01202,   0.01202,   0.01201,   0.01201,&
         0.01202,   0.01203,   0.01203,   0.01203,   0.01202,&
         0.01202,   0.01202,   0.01202,   0.01201,   0.01202,&
         0.01202,   0.01202,   0.01202,   0.01203,   0.01203,&
         0.01202,   0.01202,   0.01202,   0.01202,   0.01202,&
         0.01201,   0.01201,   0.01202,   0.01202,   0.01203,&
         0.01203,   0.01202,   0.01202,   0.01202,   0.01202,&
         0.01202,   0.01201,   0.01201,   0.01202,   0.01202,&
         0.01203,   0.01203,   0.01203,   0.01202,   0.01202,&
         0.01201,   0.01201,   0.01201,   0.01201,   0.01201,&
         0.01202,   0.01202,   0.01202,   0.01202,   0.01202,&
         0.01202,   0.01202,   0.01201,   0.01201,   0.01201,&
         0.01201,   0.01202,   0.01202,   0.01203,   0.01203,&
         0.01202,   0.01202,   0.01202,   0.01202,   0.01201,&
         0.01201,   0.01201,   0.01201,   0.01202,   0.01202,&
         0.01202,   0.01202,   0.01202,   0.01202,   0.01202,&
         0.01202,   0.01201,   0.01201,   0.01201,   0.01202,&
         0.01202,   0.01203,   0.01203,   0.01203,   0.01202,&
         0.01202,   0.01202,   0.01202,   0.01202,   0.01202,&
         0.01202,   0.01202,   0.01203,   0.01203,   0.01203,&
         0.01202,   0.01202,   0.01202,   0.01202,   0.01202,&
         0.01202,   0.01203,   0.01203,   0.01203,   0.01203,&
         0.01203,   0.01202,   0.01202,   0.01202,   0.01202,&
         0.01202,   0.01203,   0.01204,   0.01204,   0.01204,&
         0.01203,   0.01202,   0.01202,   0.01202,   0.01202,&
         0.01202,   0.01204,   0.01205,   0.01205,   0.01204,&
         0.01203,   0.01203,   0.01202,   0.01202,   0.01202,&
         0.01202,   0.01203,   0.01203,   0.01203,   0.01203,&
         0.01203,   0.01203,   0.01203,   0.01202,   0.01202,&
         0.01202,   0.01202,   0.01202,   0.01203,   0.01204,&
         0.01204,   0.01204,   0.01204,   0.01203,   0.01202,&
         0.01202,   0.01202,   0.01202,   0.01203,   0.01204,&
         0.01204,   0.01204,   0.01203,   0.01203,   0.01202,&
         0.01202,   0.01202,   0.01202,   0.01203,   0.01203,&
         0.01204,   0.01204,   0.01204,   0.01203,   0.01203,&
         0.01202,   0.01202,   0.01202,   0.01202     /)

   real, parameter :: CHOUBAND4(firstYear:finalYear) = (/ &
         0.05687,   0.05687,   0.05690,   0.05690,   0.05691,&
         0.05689,   0.05686,   0.05685,   0.05685,   0.05686,&
         0.05686,   0.05686,   0.05686,   0.05686,   0.05685,&
         0.05687,   0.05686,   0.05686,   0.05686,   0.05685,&
         0.05684,   0.05685,   0.05684,   0.05685,   0.05684,&
         0.05685,   0.05684,   0.05684,   0.05687,   0.05688,&
         0.05685,   0.05684,   0.05686,   0.05685,   0.05685,&
         0.05684,   0.05684,   0.05684,   0.05684,   0.05684,&
         0.05684,   0.05684,   0.05684,   0.05684,   0.05684,&
         0.05684,   0.05684,   0.05684,   0.05684,   0.05684,&
         0.05684,   0.05684,   0.05684,   0.05684,   0.05684,&
         0.05684,   0.05684,   0.05684,   0.05684,   0.05684,&
         0.05684,   0.05684,   0.05684,   0.05684,   0.05684,&
         0.05684,   0.05684,   0.05684,   0.05684,   0.05684,&
         0.05684,   0.05684,   0.05684,   0.05684,   0.05684,&
         0.05684,   0.05684,   0.05684,   0.05684,   0.05684,&
         0.05684,   0.05684,   0.05684,   0.05684,   0.05684,&
         0.05684,   0.05684,   0.05684,   0.05684,   0.05684,&
         0.05684,   0.05684,   0.05684,   0.05684,   0.05684,&
         0.05684,   0.05684,   0.05684,   0.05684,   0.05684,&
         0.05684,   0.05684,   0.05684,   0.05684,   0.05684,&
         0.05684,   0.05685,   0.05685,   0.05685,   0.05686,&
         0.05685,   0.05685,   0.05685,   0.05685,   0.05685,&
         0.05685,   0.05686,   0.05686,   0.05688,   0.05686,&
         0.05688,   0.05685,   0.05686,   0.05685,   0.05685,&
         0.05686,   0.05687,   0.05686,   0.05686,   0.05687,&
         0.05685,   0.05688,   0.05686,   0.05685,   0.05685,&
         0.05685,   0.05685,   0.05685,   0.05688,   0.05688,&
         0.05688,   0.05687,   0.05686,   0.05686,   0.05686,&
         0.05685,   0.05686,   0.05686,   0.05687,   0.05688,&
         0.05687,   0.05689,   0.05688,   0.05687,   0.05687,&
         0.05686,   0.05686,   0.05687,   0.05689,   0.05690,&
         0.05690,   0.05689,   0.05689,   0.05687,   0.05687,&
         0.05686,   0.05686,   0.05688,   0.05689,   0.05690,&
         0.05688,   0.05689,   0.05687,   0.05687,   0.05686,&
         0.05686,   0.05688,   0.05690,   0.05689,   0.05689,&
         0.05688,   0.05687,   0.05687,   0.05687,   0.05687,&
         0.05686,   0.05686,   0.05685,   0.05685,   0.05685,&
         0.05685,   0.05687,   0.05686,   0.05686,   0.05686,&
         0.05686,   0.05685,   0.05685,   0.05685,   0.05684,&
         0.05684,   0.05684,   0.05685,   0.05685,   0.05685,&
         0.05685,   0.05686,   0.05686,   0.05686,   0.05685,&
         0.05685,   0.05685,   0.05685,   0.05685,   0.05685,&
         0.05686,   0.05686,   0.05687,   0.05688,   0.05688,&
         0.05688,   0.05687,   0.05686,   0.05686,   0.05686,&
         0.05688,   0.05690,   0.05691,   0.05689,   0.05689,&
         0.05688,   0.05687,   0.05686,   0.05686,   0.05686,&
         0.05687,   0.05688,   0.05688,   0.05690,   0.05690,&
         0.05688,   0.05688,   0.05688,   0.05687,   0.05686,&
         0.05686,   0.05686,   0.05686,   0.05688,   0.05689,&
         0.05690,   0.05689,   0.05688,   0.05687,   0.05687,&
         0.05686,   0.05686,   0.05686,   0.05687,   0.05688,&
         0.05690,   0.05690,   0.05689,   0.05688,   0.05687,&
         0.05686,   0.05686,   0.05686,   0.05685,   0.05686,&
         0.05687,   0.05688,   0.05688,   0.05688,   0.05688,&
         0.05687,   0.05686,   0.05686,   0.05686,   0.05685,&
         0.05686,   0.05687,   0.05688,   0.05689,   0.05690,&
         0.05689,   0.05687,   0.05687,   0.05687,   0.05686,&
         0.05686,   0.05685,   0.05686,   0.05687,   0.05688,&
         0.05688,   0.05689,   0.05688,   0.05688,   0.05688,&
         0.05687,   0.05686,   0.05686,   0.05686,   0.05686,&
         0.05688,   0.05689,   0.05691,   0.05690,   0.05689,&
         0.05688,   0.05687,   0.05686,   0.05686,   0.05687,&
         0.05688,   0.05689,   0.05690,   0.05690,   0.05689,&
         0.05688,   0.05688,   0.05687,   0.05686,   0.05687,&
         0.05688,   0.05691,   0.05691,   0.05691,   0.05691,&
         0.05690,   0.05689,   0.05688,   0.05687,   0.05687,&
         0.05689,   0.05691,   0.05693,   0.05693,   0.05692,&
         0.05690,   0.05689,   0.05688,   0.05688,   0.05688,&
         0.05689,   0.05693,   0.05695,   0.05695,   0.05694,&
         0.05692,   0.05689,   0.05688,   0.05688,   0.05688,&
         0.05688,   0.05689,   0.05691,   0.05691,   0.05692,&
         0.05692,   0.05690,   0.05690,   0.05689,   0.05688,&
         0.05687,   0.05688,   0.05689,   0.05692,   0.05694,&
         0.05694,   0.05695,   0.05693,   0.05691,   0.05689,&
         0.05688,   0.05687,   0.05688,   0.05691,   0.05694,&
         0.05694,   0.05693,   0.05692,   0.05690,   0.05688,&
         0.05688,   0.05687,   0.05688,   0.05690,   0.05692,&
         0.05694,   0.05694,   0.05694,   0.05691,   0.05690,&
         0.05689,   0.05689,   0.05688,   0.05688    /)

   real, parameter :: CHOUBAND5(firstYear:finalYear) = (/ &
         0.37831,   0.37831,   0.37831,   0.37832,   0.37832,&
         0.37831,   0.37830,   0.37830,   0.37830,   0.37830,&
         0.37830,   0.37830,   0.37830,   0.37830,   0.37830,&
         0.37831,   0.37830,   0.37830,   0.37830,   0.37830,&
         0.37829,   0.37829,   0.37829,   0.37830,   0.37829,&
         0.37829,   0.37829,   0.37829,   0.37830,   0.37830,&
         0.37829,   0.37829,   0.37830,   0.37829,   0.37829,&
         0.37829,   0.37829,   0.37829,   0.37829,   0.37829,&
         0.37829,   0.37829,   0.37829,   0.37829,   0.37829,&
         0.37829,   0.37829,   0.37829,   0.37829,   0.37829,&
         0.37829,   0.37829,   0.37829,   0.37829,   0.37829,&
         0.37829,   0.37829,   0.37829,   0.37829,   0.37829,&
         0.37829,   0.37829,   0.37829,   0.37829,   0.37829,&
         0.37829,   0.37829,   0.37829,   0.37829,   0.37829,&
         0.37829,   0.37829,   0.37829,   0.37829,   0.37829,&
         0.37829,   0.37829,   0.37829,   0.37829,   0.37829,&
         0.37829,   0.37829,   0.37829,   0.37829,   0.37829,&
         0.37829,   0.37829,   0.37829,   0.37829,   0.37829,&
         0.37829,   0.37829,   0.37829,   0.37829,   0.37829,&
         0.37829,   0.37829,   0.37829,   0.37829,   0.37829,&
         0.37829,   0.37829,   0.37829,   0.37829,   0.37829,&
         0.37829,   0.37829,   0.37830,   0.37829,   0.37830,&
         0.37830,   0.37830,   0.37830,   0.37830,   0.37830,&
         0.37830,   0.37830,   0.37830,   0.37831,   0.37830,&
         0.37831,   0.37830,   0.37830,   0.37830,   0.37830,&
         0.37830,   0.37831,   0.37830,   0.37830,   0.37831,&
         0.37830,   0.37831,   0.37830,   0.37830,   0.37830,&
         0.37830,   0.37830,   0.37830,   0.37831,   0.37831,&
         0.37831,   0.37830,   0.37830,   0.37830,   0.37830,&
         0.37830,   0.37830,   0.37831,   0.37831,   0.37831,&
         0.37831,   0.37831,   0.37831,   0.37831,   0.37831,&
         0.37831,   0.37831,   0.37831,   0.37831,   0.37832,&
         0.37832,   0.37832,   0.37832,   0.37831,   0.37831,&
         0.37831,   0.37831,   0.37831,   0.37832,   0.37832,&
         0.37831,   0.37832,   0.37831,   0.37831,   0.37831,&
         0.37831,   0.37831,   0.37832,   0.37831,   0.37831,&
         0.37831,   0.37831,   0.37831,   0.37831,   0.37831,&
         0.37830,   0.37830,   0.37830,   0.37830,   0.37830,&
         0.37830,   0.37831,   0.37830,   0.37830,   0.37830,&
         0.37830,   0.37830,   0.37830,   0.37829,   0.37829,&
         0.37829,   0.37829,   0.37830,   0.37830,   0.37830,&
         0.37830,   0.37830,   0.37830,   0.37830,   0.37830,&
         0.37830,   0.37830,   0.37830,   0.37830,   0.37830,&
         0.37830,   0.37830,   0.37831,   0.37831,   0.37831,&
         0.37831,   0.37831,   0.37831,   0.37830,   0.37831,&
         0.37831,   0.37832,   0.37832,   0.37832,   0.37831,&
         0.37831,   0.37831,   0.37831,   0.37831,   0.37831,&
         0.37831,   0.37831,   0.37831,   0.37832,   0.37832,&
         0.37831,   0.37831,   0.37831,   0.37831,   0.37831,&
         0.37831,   0.37831,   0.37831,   0.37831,   0.37832,&
         0.37832,   0.37832,   0.37831,   0.37831,   0.37831,&
         0.37831,   0.37831,   0.37830,   0.37831,   0.37831,&
         0.37832,   0.37832,   0.37832,   0.37831,   0.37831,&
         0.37830,   0.37830,   0.37830,   0.37830,   0.37830,&
         0.37831,   0.37831,   0.37831,   0.37831,   0.37831,&
         0.37830,   0.37830,   0.37830,   0.37830,   0.37830,&
         0.37830,   0.37831,   0.37831,   0.37831,   0.37832,&
         0.37831,   0.37831,   0.37830,   0.37830,   0.37830,&
         0.37830,   0.37830,   0.37830,   0.37831,   0.37832,&
         0.37830,   0.37832,   0.37831,   0.37831,   0.37831,&
         0.37831,   0.37830,   0.37830,   0.37830,   0.37831,&
         0.37832,   0.37832,   0.37832,   0.37833,   0.37832,&
         0.37831,   0.37831,   0.37830,   0.37831,   0.37831,&
         0.37831,   0.37831,   0.37832,   0.37831,   0.37831,&
         0.37832,   0.37832,   0.37831,   0.37831,   0.37831,&
         0.37832,   0.37833,   0.37832,   0.37832,   0.37832,&
         0.37832,   0.37832,   0.37832,   0.37831,   0.37832,&
         0.37833,   0.37832,   0.37832,   0.37833,   0.37833,&
         0.37832,   0.37831,   0.37832,   0.37832,   0.37832,&
         0.37832,   0.37833,   0.37834,   0.37833,   0.37832,&
         0.37833,   0.37833,   0.37832,   0.37832,   0.37832,&
         0.37832,   0.37833,   0.37833,   0.37833,   0.37833,&
         0.37833,   0.37832,   0.37833,   0.37832,   0.37832,&
         0.37831,   0.37832,   0.37833,   0.37834,   0.37835,&
         0.37835,   0.37835,   0.37833,   0.37834,   0.37832,&
         0.37832,   0.37832,   0.37832,   0.37833,   0.37835,&
         0.37834,   0.37833,   0.37834,   0.37833,   0.37832,&
         0.37832,   0.37832,   0.37832,   0.37833,   0.37834,&
         0.37835,   0.37835,   0.37835,   0.37833,   0.37833,&
         0.37832,   0.37832,   0.37832,   0.37832    /)

   real, parameter :: CHOUBAND6(firstYear:finalYear) = (/ &
         0.33338,   0.33338,   0.33334,   0.33332,   0.33332,&
         0.33335,   0.33339,   0.33340,   0.33341,   0.33340,&
         0.33340,   0.33340,   0.33340,   0.33340,   0.33340,&
         0.33338,   0.33339,   0.33340,   0.33340,   0.33340,&
         0.33342,   0.33341,   0.33342,   0.33341,   0.33342,&
         0.33341,   0.33342,   0.33342,   0.33337,   0.33336,&
         0.33341,   0.33342,   0.33339,   0.33341,   0.33341,&
         0.33342,   0.33342,   0.33342,   0.33342,   0.33342,&
         0.33342,   0.33342,   0.33342,   0.33342,   0.33342,&
         0.33342,   0.33342,   0.33342,   0.33343,   0.33343,&
         0.33342,   0.33342,   0.33343,   0.33343,   0.33343,&
         0.33343,   0.33343,   0.33343,   0.33343,   0.33343,&
         0.33343,   0.33342,   0.33342,   0.33343,   0.33343,&
         0.33343,   0.33342,   0.33343,   0.33343,   0.33343,&
         0.33342,   0.33343,   0.33343,   0.33343,   0.33342,&
         0.33343,   0.33342,   0.33343,   0.33342,   0.33343,&
         0.33343,   0.33342,   0.33342,   0.33342,   0.33342,&
         0.33342,   0.33342,   0.33342,   0.33342,   0.33342,&
         0.33342,   0.33342,   0.33342,   0.33342,   0.33342,&
         0.33342,   0.33342,   0.33342,   0.33342,   0.33342,&
         0.33342,   0.33342,   0.33342,   0.33342,   0.33342,&
         0.33342,   0.33341,   0.33341,   0.33341,   0.33339,&
         0.33340,   0.33340,   0.33341,   0.33341,   0.33340,&
         0.33340,   0.33339,   0.33339,   0.33337,   0.33340,&
         0.33336,   0.33341,   0.33340,   0.33341,   0.33341,&
         0.33340,   0.33337,   0.33339,   0.33340,   0.33337,&
         0.33340,   0.33337,   0.33340,   0.33340,   0.33341,&
         0.33341,   0.33341,   0.33341,   0.33336,   0.33336,&
         0.33337,   0.33338,   0.33339,   0.33339,   0.33340,&
         0.33340,   0.33340,   0.33339,   0.33337,   0.33337,&
         0.33337,   0.33335,   0.33337,   0.33338,   0.33338,&
         0.33339,   0.33340,   0.33338,   0.33335,   0.33333,&
         0.33333,   0.33334,   0.33335,   0.33337,   0.33338,&
         0.33339,   0.33339,   0.33337,   0.33334,   0.33334,&
         0.33336,   0.33335,   0.33337,   0.33338,   0.33340,&
         0.33339,   0.33336,   0.33334,   0.33334,   0.33334,&
         0.33336,   0.33337,   0.33337,   0.33338,   0.33338,&
         0.33340,   0.33340,   0.33340,   0.33341,   0.33341,&
         0.33340,   0.33337,   0.33338,   0.33340,   0.33340,&
         0.33340,   0.33341,   0.33341,   0.33341,   0.33342,&
         0.33342,   0.33342,   0.33341,   0.33341,   0.33341,&
         0.33340,   0.33339,   0.33339,   0.33340,   0.33340,&
         0.33341,   0.33341,   0.33341,   0.33341,   0.33340,&
         0.33340,   0.33339,   0.33337,   0.33336,   0.33336,&
         0.33336,   0.33338,   0.33339,   0.33340,   0.33340,&
         0.33337,   0.33333,   0.33332,   0.33334,   0.33335,&
         0.33336,   0.33338,   0.33339,   0.33339,   0.33339,&
         0.33338,   0.33337,   0.33336,   0.33333,   0.33334,&
         0.33336,   0.33336,   0.33336,   0.33338,   0.33339,&
         0.33340,   0.33340,   0.33339,   0.33337,   0.33334,&
         0.33334,   0.33335,   0.33336,   0.33337,   0.33338,&
         0.33339,   0.33339,   0.33340,   0.33338,   0.33336,&
         0.33333,   0.33334,   0.33334,   0.33336,   0.33338,&
         0.33339,   0.33340,   0.33340,   0.33340,   0.33340,&
         0.33338,   0.33337,   0.33337,   0.33336,   0.33336,&
         0.33337,   0.33339,   0.33340,   0.33340,   0.33340,&
         0.33340,   0.33337,   0.33336,   0.33334,   0.33334,&
         0.33335,   0.33337,   0.33338,   0.33339,   0.33339,&
         0.33340,   0.33340,   0.33340,   0.33338,   0.33336,&
         0.33336,   0.33336,   0.33336,   0.33336,   0.33337,&
         0.33338,   0.33339,   0.33340,   0.33340,   0.33339,&
         0.33336,   0.33334,   0.33332,   0.33333,   0.33335,&
         0.33337,   0.33338,   0.33339,   0.33339,   0.33338,&
         0.33336,   0.33334,   0.33333,   0.33334,   0.33334,&
         0.33336,   0.33337,   0.33338,   0.33339,   0.33338,&
         0.33336,   0.33332,   0.33331,   0.33331,   0.33332,&
         0.33334,   0.33335,   0.33336,   0.33337,   0.33337,&
         0.33335,   0.33332,   0.33329,   0.33329,   0.33329,&
         0.33333,   0.33334,   0.33336,   0.33337,   0.33337,&
         0.33336,   0.33329,   0.33325,   0.33325,   0.33327,&
         0.33330,   0.33334,   0.33336,   0.33337,   0.33337,&
         0.33337,   0.33335,   0.33332,   0.33331,   0.33331,&
         0.33330,   0.33333,   0.33333,   0.33336,   0.33336,&
         0.33338,   0.33337,   0.33336,   0.33331,   0.33327,&
         0.33327,   0.33326,   0.33329,   0.33332,   0.33335,&
         0.33337,   0.33337,   0.33336,   0.33332,   0.33327,&
         0.33328,   0.33328,   0.33331,   0.33334,   0.33336,&
         0.33337,   0.33338,   0.33336,   0.33333,   0.33330,&
         0.33328,   0.33328,   0.33327,   0.33332,   0.33334,&
         0.33335,   0.33336,   0.33337,   0.33337    /)

   real, parameter :: CHOUBAND7(firstYear:finalYear) = (/ &
         0.16512,   0.16512,   0.16508,   0.16507,   0.16506,&
         0.16509,   0.16513,   0.16515,   0.16515,   0.16514,&
         0.16514,   0.16514,   0.16514,   0.16514,   0.16515,&
         0.16512,   0.16513,   0.16514,   0.16514,   0.16515,&
         0.16516,   0.16516,   0.16516,   0.16515,   0.16516,&
         0.16516,   0.16517,   0.16517,   0.16511,   0.16511,&
         0.16516,   0.16517,   0.16513,   0.16516,   0.16516,&
         0.16517,   0.16517,   0.16517,   0.16517,   0.16517,&
         0.16517,   0.16517,   0.16517,   0.16517,   0.16517,&
         0.16517,   0.16517,   0.16517,   0.16517,   0.16517,&
         0.16517,   0.16517,   0.16517,   0.16517,   0.16517,&
         0.16517,   0.16517,   0.16517,   0.16517,   0.16517,&
         0.16517,   0.16517,   0.16517,   0.16517,   0.16517,&
         0.16517,   0.16517,   0.16517,   0.16517,   0.16517,&
         0.16517,   0.16517,   0.16517,   0.16517,   0.16517,&
         0.16517,   0.16517,   0.16517,   0.16517,   0.16517,&
         0.16517,   0.16517,   0.16517,   0.16517,   0.16517,&
         0.16517,   0.16517,   0.16517,   0.16517,   0.16517,&
         0.16517,   0.16517,   0.16517,   0.16517,   0.16517,&
         0.16517,   0.16517,   0.16517,   0.16517,   0.16517,&
         0.16517,   0.16517,   0.16517,   0.16517,   0.16517,&
         0.16516,   0.16516,   0.16515,   0.16516,   0.16514,&
         0.16515,   0.16515,   0.16515,   0.16516,   0.16515,&
         0.16515,   0.16513,   0.16513,   0.16511,   0.16514,&
         0.16511,   0.16516,   0.16514,   0.16516,   0.16516,&
         0.16514,   0.16512,   0.16514,   0.16514,   0.16512,&
         0.16515,   0.16511,   0.16514,   0.16515,   0.16516,&
         0.16516,   0.16515,   0.16515,   0.16511,   0.16510,&
         0.16511,   0.16513,   0.16513,   0.16513,   0.16514,&
         0.16515,   0.16514,   0.16513,   0.16512,   0.16511,&
         0.16511,   0.16509,   0.16511,   0.16512,   0.16512,&
         0.16514,   0.16514,   0.16512,   0.16509,   0.16507,&
         0.16507,   0.16508,   0.16509,   0.16511,   0.16512,&
         0.16513,   0.16513,   0.16511,   0.16508,   0.16508,&
         0.16510,   0.16508,   0.16511,   0.16512,   0.16514,&
         0.16513,   0.16510,   0.16508,   0.16508,   0.16509,&
         0.16510,   0.16511,   0.16512,   0.16512,   0.16513,&
         0.16514,   0.16514,   0.16515,   0.16515,   0.16515,&
         0.16515,   0.16512,   0.16513,   0.16514,   0.16514,&
         0.16514,   0.16515,   0.16516,   0.16516,   0.16516,&
         0.16517,   0.16516,   0.16516,   0.16515,   0.16515,&
         0.16515,   0.16514,   0.16514,   0.16514,   0.16515,&
         0.16515,   0.16515,   0.16515,   0.16515,   0.16515,&
         0.16514,   0.16513,   0.16512,   0.16511,   0.16510,&
         0.16510,   0.16512,   0.16513,   0.16514,   0.16514,&
         0.16511,   0.16507,   0.16506,   0.16508,   0.16509,&
         0.16510,   0.16512,   0.16513,   0.16514,   0.16513,&
         0.16512,   0.16511,   0.16510,   0.16507,   0.16508,&
         0.16510,   0.16510,   0.16511,   0.16512,   0.16513,&
         0.16514,   0.16514,   0.16513,   0.16511,   0.16508,&
         0.16507,   0.16509,   0.16510,   0.16511,   0.16512,&
         0.16513,   0.16514,   0.16514,   0.16512,   0.16510,&
         0.16507,   0.16508,   0.16508,   0.16511,   0.16512,&
         0.16514,   0.16514,   0.16514,   0.16514,   0.16514,&
         0.16513,   0.16511,   0.16511,   0.16510,   0.16510,&
         0.16511,   0.16513,   0.16514,   0.16514,   0.16515,&
         0.16514,   0.16512,   0.16510,   0.16508,   0.16508,&
         0.16509,   0.16511,   0.16513,   0.16513,   0.16514,&
         0.16514,   0.16515,   0.16515,   0.16512,   0.16510,&
         0.16511,   0.16509,   0.16510,   0.16510,   0.16511,&
         0.16513,   0.16514,   0.16514,   0.16514,   0.16513,&
         0.16510,   0.16508,   0.16506,   0.16507,   0.16509,&
         0.16511,   0.16512,   0.16513,   0.16513,   0.16512,&
         0.16510,   0.16509,   0.16507,   0.16508,   0.16508,&
         0.16509,   0.16511,   0.16512,   0.16513,   0.16512,&
         0.16510,   0.16505,   0.16505,   0.16505,   0.16506,&
         0.16507,   0.16509,   0.16510,   0.16511,   0.16511,&
         0.16509,   0.16506,   0.16503,   0.16502,   0.16503,&
         0.16506,   0.16509,   0.16510,   0.16511,   0.16511,&
         0.16509,   0.16502,   0.16498,   0.16499,   0.16501,&
         0.16503,   0.16508,   0.16510,   0.16510,   0.16511,&
         0.16510,   0.16508,   0.16505,   0.16505,   0.16504,&
         0.16504,   0.16507,   0.16506,   0.16509,   0.16510,&
         0.16512,   0.16511,   0.16509,   0.16504,   0.16500,&
         0.16500,   0.16499,   0.16503,   0.16505,   0.16509,&
         0.16511,   0.16511,   0.16510,   0.16506,   0.16500,&
         0.16501,   0.16501,   0.16504,   0.16507,   0.16509,&
         0.16510,   0.16511,   0.16510,   0.16506,   0.16503,&
         0.16500,   0.16501,   0.16500,   0.16505,   0.16507,&
         0.16509,   0.16510,   0.16510,   0.16511    /)

   real, parameter :: CHOUBAND8(firstYear:finalYear) = (/ &
         0.04348,   0.04348,   0.04347,   0.04347,   0.04347,&
         0.04347,   0.04348,   0.04348,   0.04348,   0.04348,&
         0.04348,   0.04348,   0.04348,   0.04348,   0.04348,&
         0.04348,   0.04348,   0.04348,   0.04348,   0.04348,&
         0.04348,   0.04348,   0.04348,   0.04348,   0.04348,&
         0.04348,   0.04348,   0.04348,   0.04348,   0.04348,&
         0.04348,   0.04348,   0.04348,   0.04348,   0.04348,&
         0.04349,   0.04349,   0.04349,   0.04349,   0.04349,&
         0.04349,   0.04349,   0.04349,   0.04349,   0.04349,&
         0.04349,   0.04349,   0.04349,   0.04349,   0.04349,&
         0.04349,   0.04349,   0.04349,   0.04349,   0.04349,&
         0.04349,   0.04349,   0.04349,   0.04349,   0.04349,&
         0.04349,   0.04349,   0.04349,   0.04349,   0.04349,&
         0.04349,   0.04349,   0.04349,   0.04349,   0.04349,&
         0.04349,   0.04349,   0.04349,   0.04349,   0.04349,&
         0.04349,   0.04349,   0.04349,   0.04349,   0.04349,&
         0.04349,   0.04349,   0.04349,   0.04349,   0.04349,&
         0.04349,   0.04349,   0.04349,   0.04349,   0.04349,&
         0.04349,   0.04349,   0.04349,   0.04349,   0.04349,&
         0.04349,   0.04349,   0.04349,   0.04349,   0.04349,&
         0.04349,   0.04349,   0.04348,   0.04348,   0.04348,&
         0.04348,   0.04348,   0.04348,   0.04348,   0.04348,&
         0.04348,   0.04348,   0.04348,   0.04348,   0.04348,&
         0.04348,   0.04348,   0.04348,   0.04348,   0.04348,&
         0.04348,   0.04348,   0.04348,   0.04348,   0.04348,&
         0.04348,   0.04348,   0.04348,   0.04348,   0.04348,&
         0.04348,   0.04348,   0.04348,   0.04348,   0.04348,&
         0.04348,   0.04348,   0.04348,   0.04348,   0.04348,&
         0.04348,   0.04348,   0.04348,   0.04348,   0.04348,&
         0.04348,   0.04348,   0.04348,   0.04348,   0.04348,&
         0.04348,   0.04347,   0.04348,   0.04348,   0.04348,&
         0.04348,   0.04348,   0.04348,   0.04347,   0.04347,&
         0.04347,   0.04347,   0.04347,   0.04348,   0.04348,&
         0.04348,   0.04348,   0.04348,   0.04347,   0.04347,&
         0.04347,   0.04347,   0.04348,   0.04348,   0.04348,&
         0.04348,   0.04347,   0.04347,   0.04347,   0.04347,&
         0.04347,   0.04348,   0.04348,   0.04348,   0.04348,&
         0.04348,   0.04348,   0.04348,   0.04348,   0.04348,&
         0.04348,   0.04348,   0.04348,   0.04348,   0.04348,&
         0.04348,   0.04348,   0.04348,   0.04348,   0.04348,&
         0.04348,   0.04348,   0.04348,   0.04348,   0.04348,&
         0.04348,   0.04348,   0.04348,   0.04348,   0.04348,&
         0.04348,   0.04348,   0.04348,   0.04348,   0.04348,&
         0.04348,   0.04348,   0.04348,   0.04348,   0.04348,&
         0.04347,   0.04348,   0.04348,   0.04348,   0.04348,&
         0.04348,   0.04347,   0.04347,   0.04347,   0.04347,&
         0.04347,   0.04348,   0.04348,   0.04348,   0.04348,&
         0.04348,   0.04348,   0.04347,   0.04347,   0.04347,&
         0.04347,   0.04347,   0.04348,   0.04348,   0.04348,&
         0.04348,   0.04348,   0.04348,   0.04348,   0.04347,&
         0.04347,   0.04347,   0.04347,   0.04348,   0.04348,&
         0.04348,   0.04348,   0.04348,   0.04348,   0.04347,&
         0.04347,   0.04347,   0.04347,   0.04348,   0.04348,&
         0.04348,   0.04348,   0.04348,   0.04348,   0.04348,&
         0.04348,   0.04348,   0.04348,   0.04347,   0.04348,&
         0.04348,   0.04348,   0.04348,   0.04348,   0.04348,&
         0.04348,   0.04348,   0.04348,   0.04347,   0.04347,&
         0.04347,   0.04348,   0.04348,   0.04348,   0.04348,&
         0.04348,   0.04348,   0.04348,   0.04348,   0.04347,&
         0.04348,   0.04347,   0.04348,   0.04347,   0.04348,&
         0.04348,   0.04348,   0.04348,   0.04348,   0.04348,&
         0.04347,   0.04347,   0.04347,   0.04347,   0.04347,&
         0.04347,   0.04348,   0.04348,   0.04348,   0.04348,&
         0.04347,   0.04347,   0.04347,   0.04347,   0.04347,&
         0.04347,   0.04347,   0.04348,   0.04348,   0.04348,&
         0.04347,   0.04347,   0.04347,   0.04347,   0.04347,&
         0.04347,   0.04347,   0.04347,   0.04348,   0.04347,&
         0.04347,   0.04347,   0.04347,   0.04346,   0.04346,&
         0.04347,   0.04347,   0.04347,   0.04347,   0.04347,&
         0.04347,   0.04346,   0.04346,   0.04346,   0.04346,&
         0.04346,   0.04347,   0.04347,   0.04347,   0.04347,&
         0.04347,   0.04347,   0.04347,   0.04347,   0.04346,&
         0.04346,   0.04347,   0.04347,   0.04347,   0.04347,&
         0.04348,   0.04347,   0.04347,   0.04346,   0.04346,&
         0.04346,   0.04346,   0.04346,   0.04346,   0.04347,&
         0.04347,   0.04347,   0.04347,   0.04347,   0.04346,&
         0.04346,   0.04346,   0.04346,   0.04347,   0.04347,&
         0.04347,   0.04347,   0.04347,   0.04347,   0.04346,&
         0.04346,   0.04346,   0.04346,   0.04346,   0.04347,&
         0.04347,   0.04347,   0.04347,   0.04347    /) 

   ! Establish location in table for current, previous and next year.
   ! ----------------------------------------------------------------

   if(Year>finalYear .or. (Year==finalYear .and. dayOfYear > 182) ) then
      Current = (finalYear - SolCycle) + mod(Year - finalYear, SolCycle)
   else
      Current = Year
   endif

   ! Divide the year into halves.
   ! ---------------------------
   
   if(dayOfYear <= 182 .or. Current<firstYear) then
      i1 = max(Current-1,firstYear)
      i2 = max(Current  ,firstYear)
      F  = (dayOfYear+183)/365.00
   else
      i1 = Current
      i2 = Current+1
      F  = (dayOfYear-183)/365.00
   end if

   ! Linear interpolation to the given day-of-year.
   ! ----------------------------------------------

   SC    = so (i1)*(1.-F) + so (i2)*F

   if(present(HK)) then
      if(size(HK)==8) then ! Chou Bands
         HK(1) = ChouBand1(i1)*(1.-F) + ChouBand1(i2)*F
         HK(2) = ChouBand2(i1)*(1.-F) + ChouBand2(i2)*F
         HK(3) = ChouBand3(i1)*(1.-F) + ChouBand3(i2)*F
         HK(4) = ChouBand4(i1)*(1.-F) + ChouBand4(i2)*F
         HK(5) = ChouBand5(i1)*(1.-F) + ChouBand5(i2)*F
         HK(6) = ChouBand6(i1)*(1.-F) + ChouBand6(i2)*F
         HK(7) = ChouBand7(i1)*(1.-F) + ChouBand7(i2)*F
         HK(8) = ChouBand8(i1)*(1.-F) + ChouBand8(i2)*F
         _ASSERT(abs(1.0-sum(HK))<1.e-4,'needs informative message')
      else
         _ASSERT(.false.,'needs informative message')
      endif
   end if

   _RETURN(ESMF_SUCCESS)
   end subroutine MAPL_SunGetSolarConstantByYearDoY

!-------------------------------------------------------------------------
!NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1, GEOS/DAS!
!-------------------------------------------------------------------------
!BOP
!
! !INTERFACE:

   subroutine MAPL_SunGetSolarConstantFromNetcdfFile(CLOCK,fileName,SC,HK,MESOPHOT,JCALC4,rc)

! !DESCRIPTION:
!
!  Acquire the solar constant and the eight band fractions for the Chou solar code
!  from an external NetCDF file.  The initial configuration assumes monthly values
!  beginning in January, and does not interpolate beyond the range of available data.
!
! !BUGS:
! 
!  Band values for RRTMG not implemented
!
!EOP
! ---------------------------------------------------------------------------------

      implicit none

      type(ESMF_Clock), intent(in)           :: CLOCK
      character(len=*), intent(in)           :: fileName
      real, intent(out)                      :: SC
      real, optional, intent(out)            :: HK(:)
      real, optional, intent(out)            :: MESOPHOT(:)
      real, optional, intent(out)            :: JCALC4(:)
      integer, optional, intent(out)         :: rc

      type(ESMF_VM)              :: VM
      type(ESMF_Time)            :: time
      integer                    :: i, k, N
      integer                    :: begYear, endYear
      integer                    :: INDX1, INDX2
      integer                    :: MM, YY, DD, CCYY

      integer                    :: STATUS

      real                       :: FAC

      integer :: ncid

      integer :: ndate,          dimid_ndate
      integer :: nbin_sorad,     dimid_nbin_sorad
      integer :: nbin_meso_phot, dimid_nbin_meso_phot
      integer :: nbin_jcalc4,    dimid_nbin_jcalc4

      integer, dimension(:  ), allocatable :: date_year
      integer                              :: varid_date_year

      integer, dimension(:  ), allocatable :: date_month
      integer                              :: varid_date_month

      real,    dimension(:  ), allocatable :: tsi
      integer                              :: varid_tsi

      real,    dimension(:,:), allocatable :: coef_sorad
      integer                              :: varid_coef_sorad

      real,    dimension(:,:), allocatable :: coef_meso_phot
      integer                              :: varid_coef_meso_phot

      real,    dimension(:,:), allocatable :: coef_jcalc4
      integer                              :: varid_coef_jcalc4

      character(len=ESMF_MAXSTR) :: shortName
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_SunGetSolarConstantFromNetcdfFile"

      ! Open the file
      ! -------------
      STATUS = nf90_open(trim(fileName), NF90_NOWRITE, ncid)
      if (STATUS /= NF90_NOERR) then
         write (*,*) trim(Iam)//': Error opening file ', trim(fileName), status
         write (*,*) nf90_strerror(status)
         _ASSERT(.false.,'needs informative message')
      end if

      ! Read in dimensions
      ! ------------------

      status = nf90_inq_dimid(ncid, 'ndate', dimid_ndate)
      if (STATUS /= NF90_NOERR) then
         write (*,*) trim(Iam)//': Error getting ndate dimid', status
         write (*,*) nf90_strerror(status)
         _ASSERT(.false.,'needs informative message')
      end if

      status = nf90_inquire_dimension(ncid, dimid_ndate, len = ndate)
      if (STATUS /= NF90_NOERR) then
         write (*,*) trim(Iam)//': Error getting ndate length', status
         write (*,*) nf90_strerror(status)
         _ASSERT(.false.,'needs informative message')
      end if

      if (present(HK)) then
         status = nf90_inq_dimid(ncid, 'nbin_sorad', dimid_nbin_sorad)
         if (STATUS /= NF90_NOERR) then
            write (*,*) trim(Iam)//': Error getting nbin_sorad dimid', status
            write (*,*) nf90_strerror(status)
            _ASSERT(.false.,'needs informative message')
         end if

         status = nf90_inquire_dimension(ncid, dimid_nbin_sorad, len = nbin_sorad)
         if (STATUS /= NF90_NOERR) then
            write (*,*) trim(Iam)//': Error getting nbin_sorad length', status
            write (*,*) nf90_strerror(status)
            _ASSERT(.false.,'needs informative message')
         end if
      end if

      if (present(MESOPHOT)) then
         status = nf90_inq_dimid(ncid, 'nbin_meso_phot', dimid_nbin_meso_phot)
         if (STATUS /= NF90_NOERR) then
            write (*,*) trim(Iam)//': Error getting nbin_meso_phot dimid', status
            write (*,*) nf90_strerror(status)
            _ASSERT(.false.,'needs informative message')
         end if

         status = nf90_inquire_dimension(ncid, dimid_nbin_meso_phot, len = nbin_meso_phot)
         if (STATUS /= NF90_NOERR) then
            write (*,*) trim(Iam)//': Error getting nbin_meso_phot length', status
            write (*,*) nf90_strerror(status)
            _ASSERT(.false.,'needs informative message')
         end if
      end if

      if (present(JCALC4)) then
         status = nf90_inq_dimid(ncid, 'nbin_jcalc4', dimid_nbin_jcalc4)
         if (STATUS /= NF90_NOERR) then
            write (*,*) trim(Iam)//': Error getting nbin_jcalc4 dimid', status
            write (*,*) nf90_strerror(status)
            _ASSERT(.false.,'needs informative message')
         end if

         status = nf90_inquire_dimension(ncid, dimid_nbin_jcalc4, len = nbin_jcalc4)
         if (STATUS /= NF90_NOERR) then
            write (*,*) trim(Iam)//': Error getting nbin_jcalc4 length', status
            write (*,*) nf90_strerror(status)
            _ASSERT(.false.,'needs informative message')
         end if
      end if

      ! Allocate our arrays
      ! -------------------

      allocate(date_year(ndate), source=0, stat=status)
      _VERIFY(STATUS)

      allocate(date_month(ndate), source=0, stat=status)
      _VERIFY(STATUS)

      allocate(tsi(ndate), source=0.0, stat=status)
      _VERIFY(STATUS)

      if (present(HK) ) then
         allocate(coef_sorad(nbin_sorad,ndate), source=0.0, stat=status)
         _VERIFY(STATUS)
      end if

      if (present(MESOPHOT) ) then
         allocate(coef_meso_phot(nbin_meso_phot,ndate), source=0.0, stat=status)
         _VERIFY(STATUS)
      end if

      if (present(JCALC4) ) then
         allocate(coef_jcalc4(nbin_jcalc4,ndate), source=0.0, stat=status)
         _VERIFY(STATUS)
      end if

      ! Read in date_year
      ! -----------------

      status = nf90_inq_varid(ncid, 'date_year', varid_date_year)
      if (STATUS /= NF90_NOERR) then
         write (*,*) trim(Iam)//': Error getting date_year varid', status
         write (*,*) nf90_strerror(status)
         _ASSERT(.false.,'needs informative message')
      end if

      status = nf90_get_var(ncid, varid_date_year, date_year)
      if (STATUS /= NF90_NOERR) then
         write (*,*) trim(Iam)//': Error getting date_year variable', status
         write (*,*) nf90_strerror(status)
         _ASSERT(.false.,'needs informative message')
      end if

      ! Read in date_month
      ! ------------------

      status = nf90_inq_varid(ncid, 'date_month', varid_date_month)
      if (STATUS /= NF90_NOERR) then
         write (*,*) trim(Iam)//': Error getting date_month varid', status
         write (*,*) nf90_strerror(status)
         _ASSERT(.false.,'needs informative message')
      end if

      status = nf90_get_var(ncid, varid_date_month, date_month)
      if (STATUS /= NF90_NOERR) then
         write (*,*) trim(Iam)//': Error getting date_month variable', status
         write (*,*) nf90_strerror(status)
         _ASSERT(.false.,'needs informative message')
      end if

      ! Read in tsi
      ! -----------

      status = nf90_inq_varid(ncid, 'tsi', varid_tsi)
      if (STATUS /= NF90_NOERR) then
         write (*,*) trim(Iam)//': Error getting tsi varid', status
         write (*,*) nf90_strerror(status)
         _ASSERT(.false.,'needs informative message')
      end if

      status = nf90_get_var(ncid, varid_tsi, tsi)
      if (STATUS /= NF90_NOERR) then
         write (*,*) trim(Iam)//': Error getting tsi variable', status
         write (*,*) nf90_strerror(status)
         _ASSERT(.false.,'needs informative message')
      end if

      ! Read in coef_sorad
      ! ------------------

      if (present(HK)) then

         status = nf90_inq_varid(ncid, 'coef_sorad', varid_coef_sorad)
         if (STATUS /= NF90_NOERR) then
            write (*,*) trim(Iam)//': Error getting coef_sorad varid', status
            write (*,*) nf90_strerror(status)
            _ASSERT(.false.,'needs informative message')
         end if

         status = nf90_get_var(ncid, varid_coef_sorad, coef_sorad)
         if (STATUS /= NF90_NOERR) then
            write (*,*) trim(Iam)//': Error getting coef_sorad variable', status
            write (*,*) nf90_strerror(status)
            _ASSERT(.false.,'needs informative message')
         end if

      end if

      ! Read in coef_meso_phot
      ! ----------------------

      if (present(MESOPHOT)) then

         status = nf90_inq_varid(ncid, 'coef_meso_phot', varid_coef_meso_phot)
         if (STATUS /= NF90_NOERR) then
            write (*,*) trim(Iam)//': Error getting coef_meso_phot varid', status
            write (*,*) nf90_strerror(status)
            _ASSERT(.false.,'needs informative message')
         end if

         status = nf90_get_var(ncid, varid_coef_meso_phot, coef_meso_phot)
         if (STATUS /= NF90_NOERR) then
            write (*,*) trim(Iam)//': Error getting coef_meso_phot variable', status
            write (*,*) nf90_strerror(status)
            _ASSERT(.false.,'needs informative message')
         end if

      end if

      ! Read in coef_jcalc4
      ! -------------------

      if (present(JCALC4)) then

         status = nf90_inq_varid(ncid, 'coef_jcalc4', varid_coef_jcalc4)
         if (STATUS /= NF90_NOERR) then
            write (*,*) trim(Iam)//': Error getting coef_jcalc4 varid', status
            write (*,*) nf90_strerror(status)
            _ASSERT(.false.,'needs informative message')
         end if

         status = nf90_get_var(ncid, varid_coef_jcalc4, coef_jcalc4)
         if (STATUS /= NF90_NOERR) then
            write (*,*) trim(Iam)//': Error getting coef_jcalc4 variable', status
            write (*,*) nf90_strerror(status)
            _ASSERT(.false.,'needs informative message')
         end if

      end if

      ! Time interpolation parameters
      ! -----------------------------
      call MAPL_ClimInterpFac(CLOCK, INDX1, INDX2, FAC, RC=STATUS)
      _VERIFY(STATUS)

      ! Time: Year and month
      ! --------------------
      call ESMF_ClockGet(CLOCK, CURRTIME=time, RC=STATUS)
      _VERIFY(STATUS)
      call ESMF_TimeGet(time, YY=YY, MM=MM, DD=DD, RC=STATUS)
      _VERIFY(STATUS)

      ! This index search routine below is adaptedfrom PChemGridComp
      !
      ! Read bracketing months, make sure INDX1 and INDX2 are in range. Annual
      ! cycle is preserved for years that precede and succeed the climatology.
      ! ----------------------------------------------------------------------

       N = 12*ndate
       begYear = date_year(1)
       endYear = date_year(ndate)

       CCYY = YY
       IF(CCYY < begYear) CCYY = begYear
       IF(CCYY > endYear) CCYY = endYear

       INDX1 = INDX1+(CCYY-begYear)*12
       INDX2 = INDX2+(CCYY-begYear)*12

       IF(MM ==  1 .AND. INDX1 > INDX2) INDX1 = INDX1-12
       IF(MM == 12 .AND. INDX2 < INDX1) INDX2 = INDX2+12

       IF(INDX1 == 0) INDX1 = 12
       IF(YY < begYear .AND. INDX2 > 12) INDX2 = 1

       IF(INDX2 == N+1) INDX2 = N-11
       IF(YY > endYear .AND. INDX1 == N-12) INDX1 = N


      ! Linear Interpolation to the given day-of-month
      ! ----------------------------------------------

      SC = tsi(INDX1)*FAC + tsi(INDX2)*(1.0-FAC)

      if (present(HK)) then
         HK = coef_sorad(:,INDX1)*FAC + coef_sorad(:,INDX2)*(1.0-FAC)
      end if

      if (present(MESOPHOT)) then
         HK = coef_meso_phot(:,INDX1)*FAC + coef_meso_phot(:,INDX2)*(1.0-FAC)
      end if

      if (present(JCALC4)) then
         HK = coef_jcalc4(:,INDX1)*FAC + coef_jcalc4(:,INDX2)*(1.0-FAC)
      end if

      ! Close the file
      ! --------------
      STATUS = nf90_close(ncid)
      _VERIFY(STATUS)

      ! Bounds check
      ! ------------
      if (PRESENT(HK)) then
         _ASSERT(ABS(1.0-SUM(HK)) < 1.E-4,'needs informative message')
      end if

      ! Deallocate our arrays
      ! ---------------------

      deallocate(date_year, stat=status)
      _VERIFY(STATUS)

      deallocate(date_month, stat=status)
      _VERIFY(STATUS)

      deallocate(tsi, stat=status)
      _VERIFY(STATUS)

      if (present(HK)) then
         deallocate(coef_sorad, stat=status)
         _VERIFY(STATUS)
      end if

      if (present(MESOPHOT)) then
         deallocate(coef_meso_phot, stat=status)
         _VERIFY(STATUS)
      end if

      if (present(JCALC4)) then
         deallocate(coef_jcalc4, stat=status)
         _VERIFY(STATUS)
      end if

      _RETURN(ESMF_SUCCESS)

   end subroutine MAPL_SunGetSolarConstantFromNetcdfFile

!-------------------------------------------------------------------------
!NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1, GEOS/DAS!
!-------------------------------------------------------------------------
!BOP
!
! !INTERFACE:

   subroutine MAPL_SunGetSolarConstantFromNRLFile(CLOCK,filename_in,SC,MG,SB,PersistSolar,rc)

! !DESCRIPTION:
!
!  Acquire the TSI, Mg, and SB from file
!
! !BUGS:
! 
!  Band values for RRTMG not implemented
!
!EOP
! ---------------------------------------------------------------------------------

      implicit none

      type(ESMF_Clock), intent(in)       :: CLOCK
      character(len=*), intent(in)       :: filename_in
      real, intent(out)                  :: SC
      real, intent(out)                  :: MG
      real, intent(out)                  :: SB
      logical, optional, intent(in)      :: PersistSolar
      integer, optional, intent(out)     :: rc

      type(ESMF_VM)              :: VM
      type(ESMF_Time)            :: noonCurrentDay, prevNoon, nextNoon
      type(ESMF_TimeInterval)    :: intToNextNoon, oneDayInterval

      type(ESMF_Time)            :: currentTime, origTime
      type(ESMF_Time)            :: timeBasedOnCycle23
      type(ESMF_Time)            :: startCycle23, startCycle24
      type(ESMF_TimeInterval)    :: timeSinceStartOfCycle24

      integer :: currentYear, currentMon, currentDay, currentDOY, &
                 prevDay, nextDay
      integer :: prevDOY, nextDOY, prevNoonYear, nextNoonYear
      integer :: originalYear, originalMon, originalDay, origDOY

      real(ESMF_KIND_R8)         :: days_r8

      integer                    :: INDX1, INDX2, i

      integer                    :: STATUS, UNIT

      real                       :: FAC

      character(len=ESMF_MAXPATHLEN) :: FILENAME
      logical :: found
      logical :: amIRoot
      integer :: deId, NPES
      logical :: outOfTable

      logical :: PersistSolar_

      integer, parameter :: YEAR_NOT_FOUND = -99

      logical, parameter :: DEBUGPRINT = .FALSE.

      logical, save :: TableCreated = .FALSE.
      integer, save, allocatable, dimension(:) :: yearTable, doyTable
      real,    save, allocatable, dimension(:) :: tsi, mgindex, sbindex
      integer, save :: numlines

      character(len=ESMF_MAXSTR) :: IAm = "MAPL_SunGetSolarConstantFromNRLFile"

      if (present(PersistSolar)) then
         PersistSolar_ = PersistSolar
      else
         PersistSolar_ = .TRUE.
      end if

      call ESMF_VMGetCurrent(vm, rc=status)

      call ESMF_VmGet(VM, localPet=deId, petCount=npes, rc=status)
      _VERIFY(STATUS)
      amIRoot = (deId == 0)

      CREATE_TABLE: if (.not. TableCreated) then

         ! Open the file
         ! -------------

         filename = trim(filename_in)

         ! Does the file exist?
         inquire( FILE=FILENAME, EXIST=found )
         _ASSERT( FOUND ,'needs informative message')

         UNIT = GETFILE(filename, DO_OPEN=0, form="formatted", rc=status)
         _VERIFY(STATUS)

         open(unit=unit, file=filename)

         if (amIRoot) then

            ! Determine length of file
            ! ------------------------

            numlines = num_lines_in_file(UNIT)

            ! Allocate our arrays
            ! -------------------

            allocate(yearTable(numlines), source=0, stat=status)
            _VERIFY(STATUS)

            allocate(doyTable(numlines), source=0, stat=status)
            _VERIFY(STATUS)

            allocate(tsi(numlines), source=0.0, stat=status)
            _VERIFY(STATUS)

            allocate(mgindex(numlines), source=0.0, stat=status)
            _VERIFY(STATUS)

            allocate(sbindex(numlines), source=0.0, stat=status)
            _VERIFY(STATUS)

            ! Read in arrays
            ! --------------

            if (DEBUGPRINT) write (*,*) "Reading the Solar Table"

            do i = 1, numlines
               read(unit,*) yearTable(i), doyTable(i), tsi(i), mgindex(i), sbindex(i)
            end do

         end if

         ! Close the file
         ! --------------

         call FREE_FILE(UNIT)

         TableCreated = .TRUE.

      end if CREATE_TABLE

      ON_ROOT: if (amIRoot) then

         ! Now we need to find the two bracketing days
         ! -------------------------------------------

         ! Get current time
         ! ----------------
         call ESMF_ClockGet(CLOCK, CURRTIME=currentTime, RC=STATUS)
         _VERIFY(STATUS)

         call ESMF_TimeGet( currentTime, YY = currentYear, &
                                         MM = currentMon,  &
                                         DD = currentDay,  &
                                  dayOfYear = currentDOY,  &
                                         RC = STATUS       )
         _VERIFY(STATUS)

         ! Test if current time is outside our file
         ! ----------------------------------------

         outOfTable = .FALSE.

         ! First is current year higher than last in file...
         if ( currentYear > yearTable(numlines) ) then
            outOfTable = .TRUE.
         ! ...or if a partial year, are we near the end
         else if ( currentYear == yearTable(numlines) .and. currentDOY >= doyTable(numlines)) then
            outOfTable = .TRUE.
         end if

         ! If we are out of the table and not persisting, we must 
         ! recenter our day to be based on the last complete Solar Cycle
         ! -------------------------------------------------------------
         OUT_OF_TABLE_AND_CYCLE: if ( outOfTable .and. (.not. PersistSolar_) ) then

            ! Create an ESMF_Time at start of Cycle 23
            ! ----------------------------------------
            call ESMF_TimeSet( startCycle23, YY = 1996,  &
                                             MM = 8,     &
                                             DD = 1,     &
                                              H = 12,    &
                                              M = 00,    &
                                              S = 00,    &
                                             RC = STATUS )
            _VERIFY(STATUS)

            ! Create an ESMF_Time at start of Cycle 24
            ! ----------------------------------------
            call ESMF_TimeSet( startCycle24, YY = 2008,  &
                                             MM = 12,    &
                                             DD = 1,     &
                                              H = 12,    &
                                              M = 00,    &
                                              S = 00,    &
                                             RC = STATUS )
            _VERIFY(STATUS)

            ! Create TimeInterval based on interval 
            ! from start of latest Cycle 24
            ! -------------------------------------

            timeSinceStartOfCycle24 = currentTime - startCycle24

            ! Make a new time based on that
            ! interval past start of Cycle 23
            ! -------------------------------

            timeBasedOnCycle23 = startCycle23 + timeSinceStartOfCycle24

            ! Store our original time just in case
            ! ------------------------------------
            origTime     = currentTime
            originalYear = currentYear
            originalMon  = currentMon
            originalDay  = currentDay
            origDOY      = currentDOY

            ! Make our "current" time the one calculated above
            ! ------------------------------------------------
            currentTime = timeBasedOnCycle23

            ! Get new currentYear, currentMon, currentDay
            ! -------------------------------------------

            call ESMF_TimeGet( currentTime, YY = currentYear, &
                                            MM = currentMon,  &
                                            DD = currentDay,  &
                                     dayOfYear = currentDOY,  &
                                            RC = STATUS       )
            _VERIFY(STATUS)


            ! Debugging Prints
            ! ----------------
            if (DEBUGPRINT) then
               write (*,'(1X,A)') 'Off the end of table, moving into last complete cycle'
               write (*,*)

               write (*,'(1X,A,1X,I4,A,I0.2,A,I0.2)') 'Original Year/Mon/Day to Find:   ', originalYear,'-',originalMon,'-',originalDay
               write (*,'(1X,A,1X,I0.3)')             'Original Day of Year:            ', origDOY
               write (*,*)

               write (*,'(1X,A,1X,I4,A,I0.2,A,I0.2)') 'New Year/Mon/Day to Find:        ', currentYear,'-',currentMon,'-',currentDay
               write (*,'(1X,A,1X,I0.3)')             'New Day of Year:                 ', currentDOY
               write (*,*)
            end if

         end if OUT_OF_TABLE_AND_CYCLE

         ! Create an ESMF_Time on noon of current day
         ! ------------------------------------------
         call ESMF_TimeSet( noonCurrentDay, YY = currentYear, &
                                            MM = currentMon,  &
                                            DD = currentDay,  &
                                             H = 12,          &
                                             M = 00,          &
                                             S = 00,          &
                                            RC = STATUS       )
         _VERIFY(STATUS)

         ! Figure out bracketing days for interpolation
         ! NOTE: nextNoon is mainly for debugging purposes
         ! -----------------------------------------------
         call ESMF_TimeIntervalSet(oneDayInterval, D=1, rc=status)
         if (currentTime <= noonCurrentDay) then
            prevNoon = noonCurrentDay - oneDayInterval
            nextNoon = noonCurrentDay
         else
            prevNoon = noonCurrentDay
            nextNoon = noonCurrentDay + oneDayInterval
         end if

         ! Get the DOYs
         ! ------------
         call ESMF_TimeGet( prevNoon, YY = prevNoonYear, dayOfYear = prevDOY, rc = status )
         call ESMF_TimeGet( nextNoon, YY = nextNoonYear, dayOfYear = nextDOY, rc = status )

         ! Our interpolation factor is based of when we are compared to the next noon
         ! --------------------------------------------------------------------------
         intToNextNoon = nextNoon-currentTime

         ! The FAC for interpolating is just the real version
         ! of the size of the timeinterval to the next noon
         ! --------------------------------------------------
         call ESMF_TimeIntervalGet(intToNextNoon, d_r8=days_r8, rc=STATUS)
         _VERIFY(STATUS)
         FAC = real(days_r8)

         ! Use our find_file_index function to get the index for previous noon
         ! -------------------------------------------------------------------
         INDX1 = find_file_index(numlines, yearTable, prevNoonYear, prevDOY)
         INDX2 = INDX1 + 1

         ! If we are outOfTable and we have the PersistSolar
         ! option we just use the last value in the table...
         ! -------------------------------------------------
         OUT_OF_TABLE_AND_PERSIST: if ( outOfTable .and. PersistSolar_) then

            SC =     tsi(numlines)
            MG = mgindex(numlines)
            SB = sbindex(numlines)

            ! Debugging Prints
            ! ----------------
            if (DEBUGPRINT) then
               write (*,'(1X,A)') 'Off the end of table, persisting last values'
               write (*,*)
               write (*,'(1X,A,1X,F8.3)') 'tsi at end of table:     ', tsi(numlines)
               write (*,'(1X,A,1X,F8.6)') 'mgindex at end of table: ', mgindex(numlines)
               write (*,'(1X,A,1X,F9.4)') 'sbindex at end of table: ', sbindex(numlines)
               write (*,*) 
            end if

         ! Otherwise we interpolate to the table
         ! -------------------------------------
         else

            ! Linear Interpolation to the given day-of-month
            ! ----------------------------------------------

            SC =     tsi(INDX1)*FAC +     tsi(INDX2)*(1.0-FAC)
            MG = mgindex(INDX1)*FAC + mgindex(INDX2)*(1.0-FAC)
            SB = sbindex(INDX1)*FAC + sbindex(INDX2)*(1.0-FAC)

            ! Debugging Prints
            ! ----------------
            if (DEBUGPRINT) then
               write (*,'(1X,A,1X,I3)')   'First DOY to Find:   ', prevDOY
               write (*,'(1X,A,1X,I6)')   'file_index for date: ', INDX1
               write (*,'(1X,A,1X,I4)')   'yearTable(date):     ', yearTable(INDX1)
               write (*,'(1X,A,1X,I3)')   'doyTable(date):      ', doyTable(INDX1)
               write (*,'(1X,A,1X,F8.3)') 'tsi(date):           ', tsi(INDX1)
               write (*,'(1X,A,1X,F8.6)') 'mgindex(date):       ', mgindex(INDX1)
               write (*,'(1X,A,1X,F9.4)') 'sbindex(date):       ', sbindex(INDX1)
               write (*,*) 

               write (*,'(1X,A,1X,I3)')   'Second DOY to Find:  ', nextDOY
               write (*,'(1X,A,1X,I6)')   'file_index for date: ', INDX2
               write (*,'(1X,A,1X,I4)')   'yearTable(date):     ', yearTable(INDX2)
               write (*,'(1X,A,1X,I3)')   'doyTable(date):      ', doyTable(INDX2)
               write (*,'(1X,A,1X,F8.3)') 'tsi(date):           ', tsi(INDX2)
               write (*,'(1X,A,1X,F8.6)') 'mgindex(date):       ', mgindex(INDX2)
               write (*,'(1X,A,1X,F9.4)') 'sbindex(date):       ', sbindex(INDX2)
               write (*,*) 

               write (*,'(1X,A,1X,F8.6)') 'Interpolation Factor:', FAC
               write (*,*) 
            end if
         end if OUT_OF_TABLE_AND_PERSIST
      end if ON_ROOT

      ! Broadcast the values
      ! --------------------

      call MAPL_CommsBcast(vm, DATA=SC, N=1, ROOT=0, RC=status)
      _VERIFY(STATUS)
      call MAPL_CommsBcast(vm, DATA=MG, N=1, ROOT=0, RC=status)
      _VERIFY(STATUS)
      call MAPL_CommsBcast(vm, DATA=SB, N=1, ROOT=0, RC=status)
      _VERIFY(STATUS)

      _RETURN(ESMF_SUCCESS)

      contains
         
         integer function num_lines_in_file(unit) result(count)

            implicit none

            integer, intent(in) :: unit
            integer :: stat
            
            count = 0

            do
               read(unit,*,iostat=stat) 
               if (is_iostat_end(stat)) exit
               count = count + 1
            end do

            rewind(unit)

         end function num_lines_in_file

         integer function find_file_index(numlines, year, year_to_find, doy_to_find)

            implicit none

            integer, intent(in) :: numlines
            integer, intent(in), dimension(numlines) :: year
            integer, intent(in) :: year_to_find, doy_to_find

            integer :: i, yr_index

            ! Find the index for the year
            ! ---------------------------

            yr_index = YEAR_NOT_FOUND

            do i = 1, numlines
               if (year(i) == year_to_find) then
                  yr_index = i
                  exit
               end if
            end do

            if (yr_index == YEAR_NOT_FOUND) then
               find_file_index = YEAR_NOT_FOUND
            else
               find_file_index = yr_index + doy_to_find - 1
            end if

         end function find_file_index


   end subroutine MAPL_SunGetSolarConstantFromNRLFile

end module MAPL_SunMod
