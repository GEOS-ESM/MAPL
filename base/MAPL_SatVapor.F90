!------------------------------------------------------------------------------
!               Global Modeling and Assimilation Office (GMAO)                !
!                    Goddard Earth Observing System (GEOS)                    !
!                                 MAPL Component                              !
!------------------------------------------------------------------------------
!>
!### MODULE: `MAPL_SatVaporMod`
!
! Author: GMAO SI-Team
!
! The module `MAPL_SatVaporMod` provides a function that returns
! the saturation specific humidity \(q_s\), mixing ratio, \(r_s\),
! or vapor pressure \(e_s\), over either liquid water or ice.
! The function can also return the derivatives \(\frac{d q_s}{dT}\),
! \(\frac{d r_s}{dT}\), or \(\frac{d e_s}{dT}\) through an optional argument.
! The module does not depend on ESMF and its only dependence
! on the rest of MAPL is for the definition
! of gas constants. If the preprocessor macro MAPL_MODE is not defined,
! these are assigned standard values and the build becomes
! independent of the rest of MAPL.
!
!#### File Used
! The main computations are done in the following include files:
!```
!      eqsat.H esatlqu.H esatice.H qsatlqu.H qsatice.H.
!```
!
! @bug
! The tables and some control parameters are globals.
! This can result in unsafe race conditions when called from
! multiple threads. Most of these, however, will be benign.
!@endbug
!
module MAPL_SatVaporMod

! !USES:
!
#ifdef MAPL_MODE
  use MAPL_Constants
#endif
  use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
!
  implicit none
  private
!
! !PUBLIC MEMBER FUNCTIONS:
!
!
    public MAPL_EQsatSET ! A subroutine to set parameters that control
                         ! the behavior of MAPL\_EQsat
!
    public MAPL_EQsat    ! The working function.
!
! !PUBLIC DATA MEMBERS:
!
! Enumeration values for the saturation vapor pressure formulation to be used.

  interface MAPL_EQsat
     module procedure QSAT0
     module procedure QSAT1
     module procedure QSAT2
     module procedure QSAT3
     module procedure QSATd0
     module procedure QSATd1
     module procedure QSATd2
     module procedure QSATd3
  end interface

#ifndef MAPL_MODE
  real(kind=REAL64),    parameter :: ESFAC      = 0.622
  real(kind=REAL64),    parameter :: ZEROC      = 273.16  ! K
#else
  real(kind=REAL64),    parameter :: ESFAC      = MAPL_H2OMW/MAPL_AIRMW
  real(kind=REAL64),    parameter :: ZEROC      = MAPL_TICE
#endif

! Physical parameters

  real(kind=REAL64),    parameter :: MINPFAC    = 2.0
  real(kind=REAL64),    parameter :: MAX_RS     = 1.0/(MINPFAC-1.0)
  real(kind=REAL64),    parameter :: MAX_QS     = MAX_RS/(1.0+MAX_RS)

! Table parameters

  real(kind=REAL64),    parameter :: TMINTBL    =  150.0       ! lower T bound of tables
  real(kind=REAL64),    parameter :: TMAXTBL    =  333.0       ! upper T bound of tables

! Some limits

  real(kind=REAL64),    parameter :: TMINICE    =  ZEROC - 95.
  real(kind=REAL64),    parameter :: TMAXICE    =  ZEROC
  real(kind=REAL64),    parameter :: TMINLQU    =  ZEROC - 40.
  real(kind=REAL64),    parameter :: TMAXLQU    =  TMAXTBL

! Starr parameters

  real(kind=REAL64),    parameter :: TMINSTR = TMINICE - ZEROC
  real(kind=REAL64),    parameter :: TSTARR1 = -75.
  real(kind=REAL64),    parameter :: TSTARR2 = -65.
  real(kind=REAL64),    parameter :: TSTARR3 = -50.
  real(kind=REAL64),    parameter :: TSTARR4 = -40.
  real(kind=REAL64),    parameter :: TMAXSTR = +60.

  real(kind=REAL64),    parameter :: B6 = 6.136820929E-11*100.0
  real(kind=REAL64),    parameter :: B5 = 2.034080948E-8 *100.0
  real(kind=REAL64),    parameter :: B4 = 3.031240396E-6 *100.0
  real(kind=REAL64),    parameter :: B3 = 2.650648471E-4 *100.0
  real(kind=REAL64),    parameter :: B2 = 1.428945805E-2 *100.0
  real(kind=REAL64),    parameter :: B1 = 4.436518521E-1 *100.0
  real(kind=REAL64),    parameter :: B0 = 6.107799961E+0 *100.0
  real(kind=REAL64),    parameter :: BI6= 1.838826904E-10*100.0
  real(kind=REAL64),    parameter :: BI5= 4.838803174E-8 *100.0
  real(kind=REAL64),    parameter :: BI4= 5.824720280E-6 *100.0
  real(kind=REAL64),    parameter :: BI3= 4.176223716E-4 *100.0
  real(kind=REAL64),    parameter :: BI2= 1.886013408E-2 *100.0
  real(kind=REAL64),    parameter :: BI1= 5.034698970E-1 *100.0
  real(kind=REAL64),    parameter :: BI0= 6.109177956E+0 *100.0
  real(kind=REAL64),    parameter :: S16= 0.516000335E-11*100.0
  real(kind=REAL64),    parameter :: S15= 0.276961083E-8 *100.0
  real(kind=REAL64),    parameter :: S14= 0.623439266E-6 *100.0
  real(kind=REAL64),    parameter :: S13= 0.754129933E-4 *100.0
  real(kind=REAL64),    parameter :: S12= 0.517609116E-2 *100.0
  real(kind=REAL64),    parameter :: S11= 0.191372282E+0 *100.0
  real(kind=REAL64),    parameter :: S10= 0.298152339E+1 *100.0
  real(kind=REAL64),    parameter :: S26= 0.314296723E-10*100.0
  real(kind=REAL64),    parameter :: S25= 0.132243858E-7 *100.0
  real(kind=REAL64),    parameter :: S24= 0.236279781E-5 *100.0
  real(kind=REAL64),    parameter :: S23= 0.230325039E-3 *100.0
  real(kind=REAL64),    parameter :: S22= 0.129690326E-1 *100.0
  real(kind=REAL64),    parameter :: S21= 0.401390832E+0 *100.0
  real(kind=REAL64),    parameter :: S20= 0.535098336E+1 *100.0

! Goff-Gratch Parameters

  real(kind=REAL64),    parameter :: DL(1:6) = (/-7.902980, 5.02808, -1.3816E-7, 11.344, 8.1328E-3, -3.49149 /)
  real(kind=REAL64),    parameter :: DI(0:3) = (/ 57518.5606E08, 2.01889049, 3.56654, 20.947031        /)
  real(kind=REAL64),    parameter :: LOGPS   = 3.005714898  ! log10(1013.246)
  real(kind=REAL64),    parameter :: TS      = 373.16

! Murphy and Koop Parameters

  real(kind=REAL64),    parameter :: CL(0:9) = (/ 54.842763, -6763.22, -4.21000, .000367, &
                                       0.0415, 218.8,  53.878000, -1331.22,    &
                                      -9.44523, 0.014025                      /)
  real(kind=REAL64),    parameter :: CI(0:3) = (/ 9.550426, -5723.265, 3.53068, -.00728332             /)

! Enumeration for formulation type

  integer,   parameter :: Starr      = MAPL_UseStarrQsat
  integer,   parameter :: GoffGratch = MAPL_UseGoffGratchQsat
  integer,   parameter :: MurphyKoop = MAPL_UseMurphyKoopQsat

! Tables and other Global variables

  integer,   parameter :: DEFAULT_SUBS = 100

  logical,   save      :: UTBL       = .true.
  logical,   save      :: MXRT       = .false.
  integer,   save      :: TYPE       =  1
  logical,   save      :: TableReady = .false.

  integer,   save      :: DEGSUBS    =  DEFAULT_SUBS ! subdivisions per deg K
  integer,   save      :: TABLESIZE  =  nint(TMAXTBL-TMINTBL)*DEFAULT_SUBS + 1
  real(kind=REAL64),    save      :: DELTA_T    =  1.0 / DEFAULT_SUBS

  real(kind=REAL64),    save      :: ESTFRZ
  real(kind=REAL64),    save      :: ESTLQU

  real(kind=REAL64), allocatable, save :: ESTBLE(:)
  real(kind=REAL64), allocatable, save :: ESTBLW(:)

  integer,   parameter :: WATER   = 1
  integer,   parameter :: ICE     = 2

  real(kind=REAL64),    save      :: TMIN(2) = (/ TMINLQU, TMINICE /)
  real(kind=REAL64),    save      :: TMAX(2) = (/ TMAXLQU, TMAXICE /)

! New variables to emulate ramping a la GEOS_Utilities

  real(kind=REAL64), allocatable, save :: ESTBLX(:)
  real(kind=REAL64), parameter         :: TMIX = -20.
  real(kind=REAL64), parameter         :: DefaultRamp = TMIX

contains

!==============================================
!>
! The subroutine `MAPL_EQsatSET` set behavior of MAPL_EQsat.
! `MAPL_EQsatSet` can be used to set three parameters that control
! the behavior of the working routine, MAPL_EQsat.
!
! If **UseTable** is true, tabled values of the saturation vapor pressures are used,
! instead of the `exact` calculations.
! These tables are automatically generated at a 0.01K resolution for whatever
! vapor pressure formulation is being used. If never set, MAPL_EQsat will use the tables.
! If not specified, the table behavior is left unmodified.
!
! **Formulation** sets the saturation vapor pressure function to use
! to use in generating tables or for `exact` calculations.
! Three formulations of saturation vapor pressure are supported:
!- the Starr code, as was used in NSIPP-1 (MAPL_UseStarrQsat),
!- the Goff-Gratch formulation, as used in CAM (MAPL_UseGoffGratchQsat), and
!- Murphy and Koop (2005, QJRMS) (MAPL_UseMurphyKoopQsat).
!
! If it has not been set, the formulation is Starr (MAPL_UseStarrQsat).
! If not specified, the formulation is left unmodified.
!
! **Subdivisions** sets the number of subdivisions per degree in the saturation
! vapor pressure tables. If never set, it is 100; if not specified, it is left unmodified.
!
! **MixingRatio** sets whether MAPL_EQsat will return saturation mixing ratio (true)
! or saturation specific humidity (false) when the pressure is present
! (see MAPL_EQsat documentation). If never set, it is false;
! if not specified, it is left unmodified.
!
! MAPL_EQsatSET also initializes the tables. If MAPL_EQsatSET is
! not called and tables are required, they will be initialized the first time
! one of the  MAPL_EQsat functions is called. The tables are reset with every call
! to MAPL_EQsatSET.
!
  subroutine MAPL_EQsatSET(UseTable,Formulation,Subdivisions,MixingRatio)

! !ARGUMENTS:

    logical, optional, intent(IN) :: UseTable
    integer, optional, intent(IN) :: Formulation
    integer, optional, intent(IN) :: Subdivisions
    logical, optional, intent(IN) :: MixingRatio

! Set the global flags

    if(present(UseTable   )) UTBL = UseTable
    if(present(Formulation)) TYPE = Formulation
    if(present(MixingRatio)) MXRT = MixingRatio

    if(present(SubDivisions)) then
       DEGSUBS    =  SubDivisions
       TABLESIZE  =  nint(TMAXTBL-TMINTBL)*DEGSUBS + 1
       DELTA_T    =  1.0 / DEGSUBS
    endif

    if(TYPE/=Starr .and. TYPE/=GoffGratch .and. TYPE/=MurphyKoop) then
       print *, 'Bad argument to MAPL_EQsatSET: FORMULATION=',TYPE
       print *, 'Must be one of: ', Starr, GoffGratch, MurphyKoop
       stop 999
    end if

! Set the formulation dependent limits

    if(TYPE==MurphyKoop)  then
       TMIN(ICE  ) =  max(TMINTBL,110._REAL64)
       TMIN(WATER) =  max(TMINTBL,123._REAL64)
    else
       TMIN(WATER) =  TMINLQU
       TMIN(ICE  ) =  TMINICE
    endif

! Initialize or reset the tables, even if not needed.

    if(allocated(ESTBLE)) deallocate(ESTBLE)
    if(allocated(ESTBLW)) deallocate(ESTBLW)
    if(allocated(ESTBLX)) deallocate(ESTBLX)

    allocate(ESTBLE(TABLESIZE))
    allocate(ESTBLW(TABLESIZE))
    allocate(ESTBLX(TABLESIZE))

    call ESINIT

    return

  contains
!=======================================================================================

    subroutine ESINIT

! Saturation vapor pressure table initialization. This is invoked if UTBL is true
! on the first call to any qsat routine or whenever MAPL_QsatSet is called
! N.B.--Tables are in Pa

      integer :: I
      real(kind=REAL64)  :: T
      logical :: UT

! Save the value of UTBL and temporarily set it to false to get the exact
! formulation.

      UT   = UTBL
      UTBL =.false.

! The size of the table and its resolution are currently hardwired.

      do I=1,TABLESIZE

         T = (I-1)*DELTA_T + TMINTBL

! The two standard tables. ESTBLW contains saturation vapor pressures over liquid
! for all temperatures. ESTBLE contains saturation vapor pressures over ice for
! temperatures below 0C and over water above 0C.

         ESTBLW(I) = QSATD0(T,OverIce=.false.)
         ESTBLE(I) = QSATD0(T,OverIce=.true. )

! Now, like GEOS_Utilities, create ESTBLX which is a table created
! using the TMIX value above as a default Ramp

         T = T - ZEROC

         if (T >= TMIX .and. T < 0.0 ) then
            ESTBLX(I) = ( T/TMIX )* ( ESTBLE(I) - ESTBLW(I) ) + ESTBLW(I)
         else
            ESTBLX(I) = ESTBLE(I)
         end if

      end do

! Set ESTFRZ and ESTLQU (a la GEOS_Utilities)

      ESTFRZ = QSATD0(ZEROC  ,OverIce=.false.)
      ESTLQU = QSATD0(TMINLQU,OverIce=.false.)

! Reset UTBL to what it was on entry.

      UTBL = UT

! Mark table as initialized

      TableReady = .true.

    end subroutine ESINIT

  end subroutine MAPL_EQsatSET

!=========================================================================

! !IROUTINE: MAPL_EQsat - Computes saturation vapor pressure or specific humidity

! !INTERFACE:

!    function MAPL_EQsat(TL,PL,DQ,OverIce,UseRamp,Ramp,Pascals) result(QS)
!
! !ARGUMENTS:
!
!      real,               intent(IN)  :: TL      ! Temperature in Kelvins.
!      real,     optional, intent(IN)  :: PL      ! Air pressure in hectopascals (by default)
                                                  ! or pascals if the optional Pascals is set
!      real,     optional, intent(OUT) :: DQ      ! Derivative of result wrt TL.
!      logical,  optional, intent(IN)  :: OverIce ! If set, result is over ice;
                                                  ! otherwise it is over liquid
!      logical,  optional, intent(IN)  :: UseRamp ! If set, use the Ramp/linear
                                                  ! interpolation method a la GEOS_QSAT
!      real,     optional, intent(IN)  :: Ramp    ! If set, use this value (in degrees
                                                  ! *Celsius*) below which an ice calculation
                                                  ! is done, ramping to 0°C where liquid
                                                  ! is used. This setting overrides OverIce.
!      logical,  optional, intent(IN)  :: Pascals ! If TRUE, pressure is in pascals
                                                  ! else it in millibars. By default,
                                                  ! the assumption is that pressure
                                                  ! is passed in in hectopascals to
                                                  ! emulate GEOS_QSAT
!      real                            :: QS      ! Result is in pascals for pressure
                                                  ! otherwise, nondimensional.
!
!    Overloads:
!
!      TL, PL, QL, and QS must all be of the same kind and shape.
!      Overloads exist for kinds 4 and 8 and for ranks 0, 1, 2, and 3.
!
!      Also, Ramp must match the kind of TL, PL, QL, and QS
!
!
! !DESCRIPTION:  MAPL\_EQsat uses various formulations of the saturation
!                vapor pressure to compute the saturated specific
!    humidity and, optionally, its derivative with respect to temperature
!    for temperature TL and pressure PL. If PL is not present
!    it returns the saturation vapor pressure and, optionally,
!    its derivative with respect to temperature.
!    If MixingRatio has been set using MAPL\_EQsatSet and PL is present, it returns saturated
!    mixing ratio instead of saturated specific humidity.
! \newline

!    If UseRamp is provided, a linear interpolation method is used.
!    If a Ramp value is supplied, then for temperatures <= Ramp (Default = -20°C, set in this module)
!    the calculation is done over ice; for temperatures >= ZEROC (0°C) the calculation
!    is done over liquid water; and in between these values,
!    it interpolates linearly between the two. Use of this setting overrides
!    OverIce (\textit{vide infra}).
! \newline

!    Pressures can be passed in as hectopascals (default) or as pascals if an optional Pascals
!    arguement is provided. All temperatures in Kelvins, except Ramp which is
!    supplied in degrees Celsius.
! \newline

!    If the logical argument Pascals is supplied and is .FALSE., the pressure is assumed
!    to be in millibars and is converted to pascals internally. If .TRUE., the pressure
!    is assumed to be in pascals. All internal calculations are done in pascals.
! \newline

!    The choice of saturation vapor pressure formulation is set with MAPL\_EQsatSET.
!    See MAPL\_EQsatSET for a full explanation.
! \newline

!    Another choice is whether to use the exact formulation
!    or a table look-up. This can also be controlled with MAPL\_EQsatSet.
!    The default is to do a table look-up.
! \newline

!    The logical argument OverIce determines whether the saturation vapor pressure
!    is computed over liquid or frozen water. If T is above 273.16K, OverIce is ignored
!    the value returned is always over liquid.
!    All three formulations are valid up to 333K.
!    Murphy and Koop is valid down to 150K, for both liquid and ice.
!    The other two are valid down to 178K for ice and 233K for super-cooled liquid.
!    Outside these ranges, the nearest valid value is used for vapor pressure,
!    and the derivatives with respect to temperature are set to zero.
! \newline

!    Once the saturation vapor pressure is obtained, the saturation specific humidity
!    is computed from:
!$$
!   q_s(T,p) = \frac{M_v}{M_d}  \frac{e_s(T)}{p + (\frac{M_v}{M_d}-1) e_s(T)}
!$$
!    and its derivative from:
!$$
!   \frac{d q_s(T,p)}{dT} = \frac{M_v}{M_d}  \frac{d e_s(T)}{dT}
!                           \frac{p}{(p + (\frac{M_v}{M_d}-1) e_s(T))^2}
!$$
!   If saturation mixing ratios are called for, they are computed from:
!$$
!   r_s(T,p) = \frac{M_v}{M_d}  \frac{e_s(T)}{p-e_s(T)} = \frac{q_s}{1-q_s}
!$$
!    and its derivative from:
!$$
!   \frac{d r_s(T,p)}{dT} = \frac{M_v}{M_d}  \frac{d e_s(T)}{dT}
!                           \frac{p}{(p -  e_s(T))^2}
!$$
!   The ratio of the molecular weights of vapor and dry air is
!   $\frac{M_v}{M_d} = \frac{18.01}{28.97} \approx 0.622$.
! \newline

!   At low pressures ($p < 2 e_s(T)$), the saturation mixing ratio is capped at 1 and the
!   saturation specific humidity at $\frac{1}{2}$. In either case the derivative is set to zero.
! \newline
!

#define TX TL
#define PX PL
#define EX QS
#define DX DQ
#define KIND_ 4
  recursive function QSAT0(TL,PL,DQ,OverIce,UseRamp,Ramp,Pascals) result(QS)
    real(kind=REAL32),              intent(IN) :: TL
    real(kind=REAL32), optional,    intent(IN) :: PL
    real(kind=REAL32), optional,    intent(OUT):: DQ
    logical,optional,    intent(IN) :: OverIce
    logical,optional,    intent(IN) :: UseRamp
    real(kind=REAL32), optional,    intent(IN) :: Ramp
    logical,optional,    intent(IN) :: Pascals
    real(kind=REAL32)                          :: QS


    real(kind=REAL32)    :: TI,W
    real(kind=REAL32)    :: DD, TT, EF
    real(kind=REAL32)    :: DDQ
    integer   :: IT
    logical   :: OverLqu

    logical   :: UseRamp_
    real(kind=REAL32)    :: Ramp_
    real(kind=REAL32)    :: PP, URAMP, QQ, QI, DQQ, DQI

    if(present(UseRamp)) then
       UseRamp_ = UseRamp
    else
       UseRamp_ = .false.
    end if

    if(present(Ramp)) then
       UseRamp_=.true.
       Ramp_=Ramp
    else
       if(UseRamp_) Ramp_=DefaultRamp
    end if

    OverLqu = .true.
    if(present(OverIce)) OverLqu=.not.OverIce

    if (present(PL)) then
       if (present(Pascals)) then
          if (Pascals) then
             PP = PX
          else
             PP = PX * 100.
          end if
       else
          PP = PX * 100.
       end if
    end if

    if(UseRamp_) then
#include "qsatramp.H"
    else
      if(OverLqu) then
#include "qsatlqu.H"
      else
#include "qsatice.H"
      end if
   end if

    return
  end function QSAT0

#undef  KIND_
#define KIND_ 8

  recursive function QSATD0(TL,PL,DQ,OverIce,UseRamp,Ramp,Pascals) result(QS)
    real(kind=REAL64),              intent(IN) :: TL
    real(kind=REAL64), optional,    intent(IN) :: PL
    real(kind=REAL64), optional,    intent(OUT):: DQ
    logical,optional,    intent(IN) :: OverIce
    logical,optional,    intent(IN) :: UseRamp
    real(kind=REAL64), optional,    intent(IN) :: Ramp
    logical,optional,    intent(IN) :: Pascals
    real(kind=REAL64)                          :: QS


    real(kind=REAL64)    :: TI,W
    real(kind=REAL64)    :: DD, TT, EF
    real(kind=REAL64)    :: DDQ
    integer   :: IT
    logical   :: OverLqu

    logical   :: UseRamp_
    real(kind=REAL64)    :: Ramp_
    real(kind=REAL64)    :: PP, URAMP, QQ, QI, DQQ, DQI

    if(present(UseRamp)) then
       UseRamp_ = UseRamp
    else
       UseRamp_ = .false.
    end if

    if(present(Ramp)) then
       UseRamp_=.true.
       Ramp_=Ramp
    else
       if(UseRamp_) Ramp_=DefaultRamp
    end if

    OverLqu = .true.
    if(present(OverIce)) OverLqu=.not.OverIce

    if (present(PL)) then
       if (present(Pascals)) then
          if (Pascals) then
             PP = PX
          else
             PP = PX * 100.D0
          end if
       else
          PP = PX * 100.D0
       end if
    end if

    if(UseRamp_) then
#include "qsatramp.H"
    else
      if(OverLqu) then
#include "qsatlqu.H"
      else
#include "qsatice.H"
      end if
   end if

    return
  end function QSATD0

#undef  DX
#undef  TX
#undef  EX
#undef  PX

#define TX TL(I)
#define PX PL(I)
#define EX QS(I)
#define DX DQ(I)

#undef  KIND_
#define KIND_ 4

   function QSAT1(TL,PL,DQ,OverIce,UseRamp,Ramp,Pascals) result(QS)
    real(kind=REAL32),              intent(IN) :: TL(:)
    real(kind=REAL32), optional,    intent(IN) :: PL(:)
    real(kind=REAL32), optional,    intent(OUT):: DQ(:)
    logical,optional,    intent(IN) :: OverIce
    logical,optional,    intent(IN) :: UseRamp
    real(kind=REAL32), optional,    intent(IN) :: Ramp
    logical,optional,    intent(IN) :: Pascals
    real(kind=REAL32)                          :: QS(SIZE(TL,1))

    integer   :: I
    real(kind=REAL32)    :: TI,W
    real(kind=REAL32)    :: DD, TT, EF
    real(kind=REAL32)    :: DDQ
    integer   :: IT
    logical   :: OverLqu

    logical   :: UseRamp_
    real(kind=REAL32)    :: Ramp_
    real(kind=REAL32)    :: PP, URAMP, QQ, QI, DQQ, DQI

    if(present(UseRamp)) then
       UseRamp_ = UseRamp
    else
       UseRamp_ = .false.
    end if

    if(present(Ramp)) then
       UseRamp_=.true.
       Ramp_=Ramp
    else
       if(UseRamp_) Ramp_=DefaultRamp
    end if

    OverLqu = .true.
    if(present(OverIce)) OverLqu=.not.OverIce

    do I=1,size(TL,1)

       if (present(PL)) then
          if (present(Pascals)) then
             if (Pascals) then
                PP = PX
             else
                PP = PX * 100.
             end if
          else
             PP = PX * 100.
          end if
       end if

       if(UseRamp_) then
#include "qsatramp.H"
       else
         if(OverLqu) then
#include "qsatlqu.H"
         else
#include "qsatice.H"
         end if
      end if
    end do

    return
  end function QSAT1

#undef  KIND_
#define KIND_ 8

   function QSATD1(TL,PL,DQ,OverIce,UseRamp,Ramp,Pascals) result(QS)
    real(kind=REAL64),              intent(IN) :: TL(:)
    real(kind=REAL64), optional,    intent(IN) :: PL(:)
    real(kind=REAL64), optional,    intent(OUT):: DQ(:)
    logical,optional,    intent(IN) :: OverIce
    logical,optional,    intent(IN) :: UseRamp
    real(kind=REAL64), optional,    intent(IN) :: Ramp
    logical,optional,    intent(IN) :: Pascals
    real(kind=REAL64)                          :: QS(SIZE(TL,1))

    integer   :: I
    real(kind=REAL64)    :: TI,W
    real(kind=REAL64)    :: DDQ
    real(kind=REAL64)    :: DD, TT, EF
    integer   :: IT
    logical   :: OverLqu

    logical   :: UseRamp_
    real(kind=REAL64)    :: Ramp_
    real(kind=REAL64)    :: PP, URAMP, QQ, QI, DQQ, DQI

    if(present(UseRamp)) then
       UseRamp_ = UseRamp
    else
       UseRamp_ = .false.
    end if

    if(present(Ramp)) then
       UseRamp_=.true.
       Ramp_=Ramp
    else
       if(UseRamp_) Ramp_=DefaultRamp
    end if

    OverLqu = .true.
    if(present(OverIce)) OverLqu=.not.OverIce

    do I=1,size(TL,1)

       if (present(PL)) then
          if (present(Pascals)) then
             if (Pascals) then
                PP = PX
             else
                PP = PX * 100.D0
             end if
          else
             PP = PX * 100.D0
          end if
       end if

       if(UseRamp_) then
#include "qsatramp.H"
       else
         if(OverLqu) then
#include "qsatlqu.H"
         else
#include "qsatice.H"
         end if
      end if
    end do

    return
  end function QSATD1

#undef  DX
#undef  TX
#undef  PX
#undef  EX

#define TX TL(I,J)
#define PX PL(I,J)
#define EX QS(I,J)
#define DX DQ(I,J)

#undef  KIND_
#define KIND_ 4

   function QSAT2(TL,PL,DQ,OverIce,UseRamp,Ramp,Pascals) result(QS)
    real(kind=REAL32),              intent(IN) :: TL(:,:)
    real(kind=REAL32), optional,    intent(IN) :: PL(:,:)
    real(kind=REAL32), optional,    intent(OUT):: DQ(:,:)
    logical,optional,    intent(IN) :: OverIce
    logical,optional,    intent(IN) :: UseRamp
    real(kind=REAL32), optional,    intent(IN) :: Ramp
    logical,optional,    intent(IN) :: Pascals
    real(kind=REAL32)    :: QS(SIZE(TL,1),SIZE(TL,2))

    integer   :: I, J
    real(kind=REAL32)    :: TI,W
    real(kind=REAL32)    :: DDQ
    real(kind=REAL32)    :: DD, TT, EF
    integer   :: IT
    logical   :: OverLqu

    logical   :: UseRamp_
    real(kind=REAL32)    :: Ramp_
    real(kind=REAL32)    :: PP, URAMP, QQ, QI, DQQ, DQI

    if(present(UseRamp)) then
       UseRamp_ = UseRamp
    else
       UseRamp_ = .false.
    end if

    if(present(Ramp)) then
       UseRamp_=.true.
       Ramp_=Ramp
    else
       if(UseRamp_) Ramp_=DefaultRamp
    end if

    OverLqu = .true.
    if(present(OverIce)) OverLqu=.not.OverIce

    do J=1,size(TL,2)
       do I=1,size(TL,1)

          if (present(PL)) then
             if (present(Pascals)) then
                if (Pascals) then
                   PP = PX
                else
                   PP = PX * 100.
                end if
             else
                PP = PX * 100.
             end if
          end if

          if(UseRamp_) then
#include "qsatramp.H"
          else
            if(OverLqu) then
#include "qsatlqu.H"
            else
#include "qsatice.H"
            end if
         end if
       end do
    end do

    return
  end function QSAT2

#undef  KIND_
#define KIND_ 8

   function QSATD2(TL,PL,DQ,OverIce,UseRamp,Ramp,Pascals) result(QS)
    real(kind=REAL64),              intent(IN) :: TL(:,:)
    real(kind=REAL64), optional,    intent(IN) :: PL(:,:)
    real(kind=REAL64), optional,    intent(OUT):: DQ(:,:)
    logical,optional,    intent(IN) :: OverIce
    logical,optional,    intent(IN) :: UseRamp
    real(kind=REAL64), optional,    intent(IN) :: Ramp
    logical,optional,    intent(IN) :: Pascals
    real(kind=REAL64)    :: QS(SIZE(TL,1),SIZE(TL,2))

    integer   :: I, J
    real(kind=REAL64)    :: TI,W
    real(kind=REAL64)    :: DDQ
    real(kind=REAL64)    :: DD, TT, EF
    integer   :: IT
    logical   :: OverLqu

    logical   :: UseRamp_
    real(kind=REAL64)    :: Ramp_
    real(kind=REAL64)    :: PP, URAMP, QQ, QI, DQQ, DQI

    if(present(UseRamp)) then
       UseRamp_ = UseRamp
    else
       UseRamp_ = .false.
    end if

    if(present(Ramp)) then
       UseRamp_=.true.
       Ramp_=Ramp
    else
       if(UseRamp_) Ramp_=DefaultRamp
    end if

    OverLqu = .true.
    if(present(OverIce)) OverLqu=.not.OverIce

    do J=1,size(TL,2)
       do I=1,size(TL,1)

          if (present(PL)) then
             if (present(Pascals)) then
                if (Pascals) then
                   PP = PX
                else
                   PP = PX * 100.D0
                end if
             else
                PP = PX * 100.D0
             end if
          end if

          if(UseRamp_) then
#include "qsatramp.H"
          else
            if(OverLqu) then
#include "qsatlqu.H"
            else
#include "qsatice.H"
            end if
         end if
       end do
    end do

    return
  end function QSATD2

#undef  DX
#undef  TX
#undef  PX
#undef  EX

#define TX TL(I,J,K)
#define PX PL(I,J,K)
#define EX QS(I,J,K)
#define DX DQ(I,J,K)

#undef  KIND_
#define KIND_ 4

   function QSAT3(TL,PL,DQ,OverIce,UseRamp,Ramp,Pascals) result(QS)
    real(kind=REAL32),              intent(IN) :: TL(:,:,:)
    real(kind=REAL32), optional,    intent(IN) :: PL(:,:,:)
    real(kind=REAL32), optional,    intent(OUT):: DQ(:,:,:)
    logical,optional,    intent(IN) :: OverIce
    logical,optional,    intent(IN) :: UseRamp
    real(kind=REAL32), optional,    intent(IN) :: Ramp
    logical,optional,    intent(IN) :: Pascals
    real(kind=REAL32)    :: QS(SIZE(TL,1),SIZE(TL,2),SIZE(TL,3))

    integer   :: I, J, K
    real(kind=REAL32)    :: TI,W
    real(kind=REAL32)    :: DDQ
    real(kind=REAL32)    :: DD, TT, EF
    integer   :: IT
    logical   :: OverLqu

    logical   :: UseRamp_
    real(kind=REAL32)    :: Ramp_
    real(kind=REAL32)    :: PP, URAMP, QQ, QI, DQQ, DQI

    if(present(UseRamp)) then
       UseRamp_ = UseRamp
    else
       UseRamp_ = .false.
    end if

    if(present(Ramp)) then
       UseRamp_=.true.
       Ramp_=Ramp
    else
       if(UseRamp_) Ramp_=DefaultRamp
    end if

    OverLqu = .true.
    if(present(OverIce)) OverLqu=.not.OverIce

    do K=1,size(TL,3)
       do J=1,size(TL,2)
          do I=1,size(TL,1)

             if (present(PL)) then
                if (present(Pascals)) then
                   if (Pascals) then
                      PP = PX
                   else
                      PP = PX * 100.
                   end if
                else
                   PP = PX * 100.
                end if
             end if

             if(UseRamp_) then
#include "qsatramp.H"
             else
               if(OverLqu) then
#include "qsatlqu.H"
               else
#include "qsatice.H"
               end if
            end if
          end do
       end do
    end do

    return
  end function QSAT3

#undef  KIND_
#define KIND_ 8

   function QSATD3(TL,PL,DQ,OverIce,UseRamp,Ramp,Pascals) result(QS)
    real(kind=REAL64),              intent(IN) :: TL(:,:,:)
    real(kind=REAL64), optional,    intent(IN) :: PL(:,:,:)
    real(kind=REAL64), optional,    intent(OUT):: DQ(:,:,:)
    logical,optional,    intent(IN) :: OverIce
    logical,optional,    intent(IN) :: UseRamp
    real(kind=REAL64), optional,    intent(IN) :: Ramp
    logical,optional,    intent(IN) :: Pascals
    real(kind=REAL64)    :: QS(SIZE(TL,1),SIZE(TL,2),SIZE(TL,3))

    integer   :: I, J, K
    real(kind=REAL64)    :: TI,W
    real(kind=REAL64)    :: DDQ
    real(kind=REAL64)    :: DD, TT, EF
    integer   :: IT
    logical   :: OverLqu

    logical   :: UseRamp_
    real(kind=REAL64)    :: Ramp_
    real(kind=REAL64)    :: PP, URAMP, QQ, QI, DQQ, DQI

    if(present(UseRamp)) then
       UseRamp_ = UseRamp
    else
       UseRamp_ = .false.
    end if

    if(present(Ramp)) then
       UseRamp_=.true.
       Ramp_=Ramp
    else
       if(UseRamp_) Ramp_=DefaultRamp
    end if

    OverLqu = .true.
    if(present(OverIce)) OverLqu=.not.OverIce

    do K=1,size(TL,3)
       do J=1,size(TL,2)
          do I=1,size(TL,1)

             if (present(PL)) then
                if (present(Pascals)) then
                   if (Pascals) then
                      PP = PX
                   else
                      PP = PX * 100.D0
                   end if
                else
                   PP = PX * 100.D0
                end if
             end if

             if(UseRamp_) then
#include "qsatramp.H"
             else
               if(OverLqu) then
#include "qsatlqu.H"
               else
#include "qsatice.H"
               end if
            end if
          end do
       end do
    end do

    return
  end function QSATD3

#undef  DX
#undef  TX
#undef  PX
#undef  EX

#undef  KIND_


end module MAPL_SatVaporMod

