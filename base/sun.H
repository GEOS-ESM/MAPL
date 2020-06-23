

      type(MAPL_SunOrbit),               intent(IN ) :: ORBIT
      real,                              intent(IN ) :: LONS DIMENSIONS
      real,                              intent(IN ) :: LATS DIMENSIONS
      real,                              intent(OUT) :: ZTH  DIMENSIONS
      real,                              intent(OUT) :: SLR  DIMENSIONS
      type(ESMF_TimeInterval), optional, intent(INout) :: INTV
      type(ESMF_Clock),        optional, intent(IN ) :: CLOCK
      integer,                 optional, intent(IN ) :: TIME
      type(ESMF_Time),         optional, intent(IN ) :: currTime
      real,                    optional, intent(OUT) :: DIST
      real,                    optional, intent(OUT) :: ZTHB DIMENSIONS
      real,                    optional, intent(OUT) :: ZTHD DIMENSIONS
      real,                    optional, intent(OUT) :: ZTH1 DIMENSIONS
      real,                    optional, intent(OUT) :: ZTHN DIMENSIONS
      real,                    optional, intent(OUT) :: ZTHP DIMENSIONS
      real,                    optional, intent(IN)  :: STEPSIZE
      integer,                 optional, intent(OUT) :: RC

!     Locals

      character(len=ESMF_MAXSTR), parameter :: IAm = "MAPL_SunGetInsolation"
      integer                               :: STATUS

      integer           :: IDAY, IDAYP1, TIME_
      integer           :: NT
      real              :: FAC, ZS, ZC, ANG, AA, DD, ET
      real*8            :: SECS

      integer           :: YEAR
      integer           :: SEC_OF_DAY
      integer           :: DAY_OF_YEAR
      integer           :: L
      type (ESMF_Time)  :: CURRENTTIME
      type (ESMF_Clock) :: MYCLOCK
      type (ESMF_TimeInterval)  :: ts
      real, dimension THE_SIZE :: ZTT, SLT, Y, NCC, ZTP

      real(ESMF_KIND_R8) :: days
      real :: ECC, OBQ, LAMBDAP
      real :: OMECC, OPECC, OMSQECC, EAFAC
      real :: MA, EA, dE, TA, LAMBDA, RT, RM
      integer :: nits

!     Begin
      
      _ASSERT(MAPL_SunOrbitCreated(ORBIT),'MAPL_SunOrbit not yet created!')
      
      ! which time mode?
      if (present(TIME)) then
         TIME_ = TIME
      else
         TIME_ = 0
      endif

      ! which current time?
      if (present(currTime)) then
         CURRENTTIME = CURRTIME
      else
         if (present(CLOCK)) then
            call ESMF_ClockGet(      CLOCK, currTime=CURRENTTIME, RC=STATUS)
         else
            call ESMF_ClockGet(ORBIT%CLOCK, currTime=CURRENTTIME, RC=STATUS)
         end if
         _VERIFY(STATUS)
      end if

      ! fixed sun option
      if (ORBIT%FIX_SUN) then
         call WRITE_PARALLEL('--- WARNING --- sun.H --- Doubly Periodic Using Daily Mean Solar Insolation')
         TIME_=MAPL_SunDailyMean
      end if

      ! default value for ZTP since not fully implemented for some options
      ZTP = 0.

      ! analytic two-body currently only works with TIME_=0 currently
      _ASSERT(.NOT.(ORBIT%ANAL2B.AND.TIME_/=0),'analytic two-body orbit currently requires TIME_=0') 

      MEAN_OR_INST: if(.not.present(INTV) .or. TIME_==MAPL_SunDailyMean &
                       .or.  TIME_==MAPL_SunAnnualMean) then

         call ESMF_TimeGet(CURRENTTIME, YY=YEAR, S=SEC_OF_DAY, &
             dayOfYear=DAY_OF_YEAR, RC=STATUS)
         _VERIFY(STATUS)

         if (.NOT. ORBIT%ANAL2B) then

            if (ORBIT%FIX_SUN) then
               TIME_=10
               YEAR=2005
               SEC_OF_DAY=86400/2
               DAY_OF_YEAR=31*7
            end if

            YEAR = mod(YEAR-1,ORBIT%YEARS_PER_CYCLE)
            IDAY = YEAR*int(ORBIT%YEARLEN)+DAY_OF_YEAR

            if(present(DIST)) DIST = ORBIT%PP(IDAY)

         endif

         select case (TIME_)

         case(MAPL_SunDailyMean)

            ! pmn: EOT will just displace sunlit period wrt mean noon,
            !      but the daily mean values will not change

            _ASSERT(.FALSE.,'pmn: MAPL_SunDailyMean probably in error!') 
            _ASSERT(.NOT.ORBIT%ANAL2B,'not implemented for analytic two-body orbit') 

            SLR  = sin(LATS)*ORBIT%ZS(IDAY)
            ZTH  = cos(LATS)*ORBIT%ZC(IDAY)
            Y    =  max(-1.0,-SLR/ZTH)

            where (Y < 1.0)
               Y   = ACOS(Y)
               SLR = (4.0*ORBIT%PP(IDAY)/MAPL_PI) * (Y *SLR + SIN(Y)*ZTH)
                  ! pmn: I get this without factor of 4.0
               ZTH = SLR*MAPL_PI / (4.0*Y)
                  ! pmn: If remove factor of 4 above, remove it here too.
                  ! pmn: This is also wrong because includes the ORBIT%PP(IDAY) factor

               ! pmn: I think these lines should read:
               !   SLR = Y * SLR + SIN(Y) * ZTH
               !   ZTH = SLR / Y
               !   SLR = ORBIT%PP(IDAY) * SLR / MAPL_PI
               ! On the assumption that ZTH is meant to be the linear average
               !   of cos(sza) over the sunlit part of the day, or what we call ZTHD
               ! This routine should also produce an insolation-weighted mean ZTH.
               ! After end select, all these different ZTHs are set to ZTH, since this
               ! branch is the instantaneous branch, which is clearly not appropriate
               ! for a daily or annual mean.

            elsewhere
               SLR = 0.0
               ZTH = 0.0
            endwhere

            _ASSERT(.not.present(ZTHP),'ZTHP not implemented for SunDailyMean') 

         case(MAPL_SunAnnualMean)

            !pmn: consistent with above (and erroneous) SunDailyMean,
            !     but unlike MAPL sun_uc.F90 comment:
            !       "annual-mean insolation for the year on the clock"
            !     its a mean over the whole currently fixed 4-year cycle.

            ! see above
            _ASSERT(.FALSE.,'pmn: MAPL_SunAnnualMean probably in error!') 
            _ASSERT(.NOT.ORBIT%ANAL2B,'not implemented for analytic two-body orbit') 

            SLR  = 0.0
            ZTH  = 0.0

            do IDAY = 1,ORBIT%DAYS_PER_CYCLE
               SLT  = sin(LATS)*ORBIT%ZS(IDAY)
               ZTT  = cos(LATS)*ORBIT%ZC(IDAY)
               Y    =  max(-1.0,-SLT/ZTT)
               where (Y < 1.0)
                  Y   = ACOS(Y)
                  SLT = (4.0*ORBIT%PP(IDAY)/MAPL_PI) * (Y *SLT + SIN(Y)*ZTT)
                  ZTT = SLR*MAPL_PI / (4.0*Y)
               elsewhere
                  SLT = 0.0
                  ZTT = 0.0
               endwhere
               SLR = SLR + SLT
               ZTH = ZTH + ZTT
            end do

            SLR = SLR / float(ORBIT%DAYS_PER_CYCLE)
            ZTH = ZTH / float(ORBIT%DAYS_PER_CYCLE)

            if(present(DIST)) DIST = 1.0

            _ASSERT(.not.present(ZTHP),'ZTHP not implemented for SunAnnualMean') 

         case (0,                       &
               MAPL_SunAutumnalEquinox, & 
               MAPL_SunWinterSolstice , & 
               MAPL_SunVernalEquinox  , & 
               MAPL_SunSummerSolstice   ) 

            ! Greenwich MEAN solar hour angle OFFSET by PI
            ! (since FAC is zero at mignight)
            FAC = real(SEC_OF_DAY)/86400.
            ANG = 2.0*MAPL_PI*FAC

            if (ORBIT%ANAL2B) then

               ! include time variation in orbit from reference time
               call ESMF_TimeIntervalGet( &
                 CURRENTTIME - ORBIT%ORB2B_TIME_REF, &
                 d_r8=days, rc=STATUS)
               _VERIFY(STATUS)
               ECC     = ORBIT%ORB2B_ECC_REF     + days * ORBIT%ORB2B_ECC_RATE
               OBQ     = ORBIT%ORB2B_OBQ_REF     + days * ORBIT%ORB2B_OBQ_RATE
               LAMBDAP = ORBIT%ORB2B_LAMBDAP_REF + days * ORBIT%ORB2B_LAMBDAP_RATE
               ! derived quantities
               OMECC = 1. - ECC
               OPECC = 1. + ECC
               OMSQECC = OMECC * OPECC
               EAFAC = sqrt(OMECC/OPECC)
               ! time interval since perhelion in days
               call ESMF_TimeIntervalGet( &
                 CURRENTTIME - ORBIT%ORB2B_TIME_PERI, &
                 d_r8=days, rc=STATUS)
               _VERIFY(STATUS)
               ! mean anomaly
               MA = ORBIT%ORB2B_OMG0 * days
               ! eccentric anomaly
               call invert_Keplers_Newton(MA,ECC,EA,dE,nits)
               ! true anomaly
               TA = calcTAfromEA(EA,EAFAC)
               ! inverse distance to sun squared
               AA = 1. / calcRadfromTA(TA,ECC,OMSQECC) ** 2
               ! celestial longitude
               LAMBDA = TA + LAMBDAP
               ! sin and cos of solar declination
               ZS = sin(LAMBDA) * sin(OBQ)
               ZC = sqrt(1. - ZS**2)
               if (ORBIT%EOT) then
                 ! solar right ascension (true and mean)
                 RT = atan2(sin(LAMBDA)*cos(OBQ),cos(LAMBDA))
                 RM = MA + LAMBDAP
                 ! equation of time
                 ET = RECT_PMPI(RM - RT)
               end if

            else

               if(TIME_==0) then
                  IDAYP1 = mod(IDAY,ORBIT%DAYS_PER_CYCLE) + 1

                  ZS = ORBIT%ZS(IDAYP1)*FAC + ORBIT%ZS(IDAY)*(1.-FAC)
                  ZC = ORBIT%ZC(IDAYP1)*FAC + ORBIT%ZC(IDAY)*(1.-FAC)
                  AA = ORBIT%PP(IDAYP1)*FAC + ORBIT%PP(IDAY)*(1.-FAC)
                  if (ORBIT%EOT) &
                    ET = ORBIT%ET(IDAYP1)*FAC + ORBIT%ET(IDAY)*(1.-FAC)
               else
                  call GETIDAY(IDAY,TIME_,ORBIT,RC=STATUS)
                  _VERIFY(STATUS)

                  ZS = ORBIT%ZS(IDAY)
                  ZC = ORBIT%ZC(IDAY)
                  AA = ORBIT%PP(IDAY)
                  if (ORBIT%EOT) ET = ORBIT%ET(IDAY)
               endif

            endif

            ! apply equation of time correction?
            if (ORBIT%EOT) then

              ! the real (zero at noon) Greenwich MEAN solar hour angle
              ANG = ANG + MAPL_PI

              ! Greenwich TRUE solar hour angle
              ANG = ANG + ET

              ! LOCAL solar zenith angle
              ZTH = ZS*SIN(LATS) + ZC*COS(LATS)*COS(ANG+LONS)

            else

              ! the historical GEOS-5 calculation based on mean solar time,
              ! i.e., lacking the required equation of time correction

              ! pmn: this branch can eventually go if EOT becomes default
              ! pmn: the sin*sin-cos*cos is less clear and probably less
              !      efficient than the EOT branch anyway

              ! solar zenith angle (based on MEAN solar time)
              ZTH = ZS*SIN(LATS) + &
                    ZC*COS(LATS) * (sin(ANG)*SIN(LONS) - cos(ANG)*COS(LONS))

            end if

            ! copy ZTH to ZTP before above horizon enforcement
            ZTP = ZTH

            ! enforce zero insolation for sun below horizon
            ZTH = max(ZTH, 0.0)

            ! normalized downward solar flux at TOA
            SLR = ZTH*AA

            if(present(DIST)) DIST = AA

            ! ---%---

! pmn: Andrea Molod said the following fixed cases are for single column tests,
!      which are run at the same start date near or at the equinox. Technic-
!      ally, the value of DIST should also be set consistent with these cases,
!      since RRTMG uses SC and DIST (not SLR) during the solar REFRESH phase.
!      But since the single column test runs always use the same cap_restart
!      and so get the same set of default DIST values from above the current
!      select statement, at least we are using a CONSISTENT wrong DIST values!
!      In fact, the RRTMG REFRESH uses the DIST to multiply the solar input
!      at TOA, which is then divided through by again at the end of REFRESH
!      to get the required NORMALIZED fluxes. So this is probably only a small
!      non-linear effect. In the solar UPDATE_EXPORT() at the heartbeat the
!      normalized fluxes are always re-multiplied by SLR, and so the SLR 
!      values below will be used directly (without reference to DIST.)

         case(10)
            SLR = 0.3278
            ZTH = 0.6087247
            ZTP = ZTH

         case(11)
            SLR = 0.3449
            ZTH = 0.5962
            ZTP = ZTH

         case(12)
            SLR = 0.3461
            ZTH = 0.5884
            ZTP = ZTH

         end select

         if(present(ZTHB)) ZTHB = ZTH
         if(present(ZTHD)) ZTHD = ZTH
         if(present(ZTH1)) ZTH1 = ZTH
         if(present(ZTHN)) ZTHN = ZTH
         if(present(ZTHP)) ZTHP = ZTP

       else ! Return Time-Interval-Mean present(INTV)

         call ESMF_TimeIntervalGet (INTV, S_R8=SECS, rc=STATUS)
         _VERIFY(STATUS)

         if(present(STEPSIZE)) then ! interval for integration
            NT = nint(SECS/STEPSIZE)
         else if(SECS>1500.) then
            NT = nint(SECS/300.0)
         else
            NT = 5
         end if

         TS = INTV/dble(NT)

         MYCLOCK =  ESMF_ClockCreate(name="SUN_CLOCK", &
              TimeStep  = ts                   , &
              StartTime = CURRENTTIME          , &
              StopTime  = CURRENTTIME + INTV   , &
              rc        = STATUS                 )
         _VERIFY(STATUS)

         SLR = 0.0
         ZTH = 0.0

         if(present(ZTHD)) then
            ZTHD = 0.0
            NCC  = 0.0
         end if

         if(present(ZTHB)) ZTHB = 0.0
         if(present(DIST)) DIST = 0.0
         if(present(ZTHP)) ZTHP = 0.0

         call MAPL_SunGetInsolation( LONS, LATS, ORBIT, ZTT, SLT, &
              CLOCK=MYCLOCK, TIME=TIME, DIST=DD, ZTHP=ZTP, &
              RC=STATUS)
         _VERIFY(STATUS)

         if(present(ZTH1)) ZTH1 = max(ZTT,0.0)

         do L=1,NT
            SLR = SLR +     SLT*0.5
            ZTH = ZTH + ZTT*SLT*0.5

            if(present(ZTHB)) ZTHB = ZTHB + 0.5*ZTT
            if(present(ZTHP)) ZTHP = ZTHP + 0.5*ZTP
            if(present(ZTHD)) then
               where(ZTT>0.0)
                  ZTHD = ZTHD + 0.5*ZTT
                  NCC  = NCC  + 0.5
               end where
            end if

            if(present(DIST)) DIST = DIST + 0.5*DD

            call ESMF_ClockAdvance    ( MYCLOCK, RC=STATUS )
            _VERIFY(STATUS)

            call MAPL_SunGetInsolation( LONS, LATS, ORBIT, ZTT, SLT, &
                 CLOCK=MYCLOCK, TIME=TIME, DIST=DD, ZTHP=ZTP, &
                 RC=STATUS)
            _VERIFY(STATUS)

            SLR = SLR +     SLT*0.5
            ZTH = ZTH + ZTT*SLT*0.5

            if(present(ZTHB)) ZTHB = ZTHB + 0.5*ZTT
            if(present(ZTHP)) ZTHP = ZTHP + 0.5*ZTP
            if(present(ZTHD)) then
               where(ZTT>0.0)
                  ZTHD = ZTHD + 0.5*ZTT
                  NCC  = NCC  + 0.5
               end where
            end if

            if(present(DIST)) DIST = DIST + 0.5*DD
         enddo

         if(present(ZTHN)) ZTHN = max(ZTT,0.0)

         where(SLR > 0.0)
            SLR = SLR/dble(NT)
            ZTH = ZTH/dble(NT)
            ZTH = ZTH / SLR
         elsewhere
            ZTH = 0.0
            SLR = 0.0
         end where

         if(present(ZTHB)) ZTHB = ZTHB/dble(NT)
         if(present(ZTHP)) ZTHP = ZTHP/dble(NT)

         if(present(ZTHD)) then
            where(NCC>0.0)
               ZTHD = ZTHD/dble(NCC)
            end where
         end if

         if(present(DIST)) DIST = DIST/dble(NT)

         call ESMF_ClockDestroy(myclock, rc=STATUS)
         _VERIFY(STATUS)

      endif MEAN_OR_INST


      _RETURN(ESMF_SUCCESS)

