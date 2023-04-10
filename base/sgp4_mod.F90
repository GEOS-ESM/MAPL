  MODULE sgp4_mod 

      USE TLE_mod

      IMPLICIT NONE

! !PUBLIC MEMBER FUNCTIONS:

       PRIVATE
       PUBLIC Sgp4_Track    ! Generate satellite ground tracks
       PUBLIC Sgp4_Swath    ! Generate satellite swath tracks'
       PUBLIC XYZ2LL        ! Convert (X,Y,Z) coordinates to lat/lon
 
!      Satellite names (for non-SGP4 orbits)
!      ------------------------------------
       INTEGER, PARAMETER :: Aqua     = 1
       INTEGER, PARAMETER :: Calipso  = 2
       INTEGER, PARAMETER :: CloudSat = 3
       INTEGER, PARAMETER :: Aura     = 4
       INTEGER, PARAMETER :: Terra    = 5

       INTEGER :: RefDate,  RefTime ! this is date learning started
                                    ! if p is loaded from tle data set 
                                    ! needs to be retrieved
!      Constants
!      ---------
       REAL(r8), PARAMETER :: pi      = 3.141592653589793
       REAL(r8), PARAMETER :: TwoPi   = 2.0D0 * pi   
       REAL(r8), PARAMETER :: HalfPi  =  0.5D0*pi
       REAL(r8), PARAMETER :: earth_radius=6371.0       ! sphere
       REAL(r8), PARAMETER :: myeps   = 0.0000001       ! for zero checks
       REAL(r8), PARAMETER :: mu      = 398600.4415     ! km^3/s^2  Gravitational constant of Earth
       REAL(r8), PARAMETER :: cdeg2km = earth_radius * (pi / 180.0) ! constant for deg2km 
       REAL(r8), PARAMETER :: ckm2deg = (1.0/earth_radius) * (180.0/pi)
       REAL(r8), PARAMETER :: cr2d    = 180/pi          ! radian2degree
       REAL(r8), PARAMETER :: Rad     = 180/pi          ! radian2degree
       REAL(r8), PARAMETER :: cd2r    = pi/180          ! deg2rad
       REAL(r8), PARAMETER :: Rad2Deg = 180.0D0/pi
       REAL(r8), PARAMETER :: Deg2Rad = pi/180.0D0
       REAL(r8), PARAMETER :: DE2RA   = pi / 180.0D0    ! 0.01745329251994330D0
       REAL(r8), PARAMETER :: cd2s    = 24.0*60.0*60.0  ! day2second
       REAL(r8), PARAMETER :: co      = 8681660.4       ! %n~8681660.4 * a^(-3/2) 
                                                        ! Space mission analysis and design
       REAL(r8), PARAMETER :: xpdotp  =  1440.0 / (2.0 *pi) ! 229.1831180523293D0

       REAL(r8), PARAMETER :: Small     = 0.00000001D0
       REAL(r8), PARAMETER :: Infinite  = 999999.9D0
       REAL(r8), PARAMETER :: Undefined = 999999.1D0

!     Definition of overloaded functions 
!     ---------------------------------
        interface Sgp4_Track
              module procedure Sgp4_track0 ! p is       (input)
              module procedure Sgp4_track1 ! sat number (input)
              module procedure Sgp4_track2 ! sat name   (input)
        end interface     

        interface Sgp4_Swath
              module procedure Sgp4_swath1 ! sat nuber (input)  
              module procedure Sgp4_swath2 ! sat name  (input)
        end interface     

        interface linspace
             module procedure linspace1 ! -similar to matlab
             module procedure linspace2
        end interface  

!     
!     Sat Related definitions 
!     -----------------------
!       definitions-> everything in degrees or km
!       eccentricity       :  eccentricity
!       anomaly           :  mean anamoly
!       inclination         :  sattelite inclination angle
!       ascension         :  right ascension of node
!       omega              :  argument of perigee
!       nrevolution        :  num of rev per data (nbar)
!       altitude              :  height
!       semimajor         :  Semi major axis

!     Include file from SGP4.CMN
!     --------------------------
      CHARACTER(len=12) :: SatName
      INTEGER   :: SatNum, ELNO  , EPHTYP, REVI  , EpochYr
      REAL(r8)  :: BStar , Ecco  , Inclo , nodeo, Argpo , No    , Mo    ,&
                   NDot  , NDDot , alta  , altp  , a     ,               &
                   JDSatEpoch, EpochDays

      REAL(r8) :: Aycof , CON41 , Cc1   , Cc4   , Cc5   , D2    , D3  ,&
                  D4    , Delmo , Eta   , ArgpDot,Omgcof, Sinmao,        &
                  T2cof , T3cof , T4cof , T5cof , X1mth2, X7thm1, MDot  ,&
                  nodeDot,Xlcof, Xmcof , Xnodcf                          

!     DS values
!     ---------
      REAL(r8) :: D2201 , D2211 , D3210 , D3222 , D4410 , D4422 , D5220 , &
                  D5232 , D5421 , D5433 , Dedt  , Del1  , Del2  , Del3  , &
                  Didt  , Dmdt  , Dnodt , Domdt , E3    , Ee2   , Peo   ,&
                  Pgho  , Pho   , Pinco , Plo   , Se2   , Se3   , Sgh2  ,&
                  Sgh3  , Sgh4  , Sh2   , Sh3   , Si2   , Si3   , Sl2   ,&
                  Sl3   , Sl4   , GSTo  , Xfact , Xgh2  , Xgh3  , Xgh4  ,&
                  Xh2   , Xh3   , Xi2   , Xi3   , Xl2   , Xl3   , Xl4   ,&
                  Xlamo , Zmol  , Zmos  , Atime , Xli   , Xni

      INTEGER :: IRez

!     NE values
!     ---------
      INTEGER   :: Isimp
      CHARACTER :: Init, Method, Opsmode


!     ERROR codes 
!     -----------
!       0 - OK
!       3 - ALLOCATE (memory) problem
!       5 - satelite is not defined
!       6 - reading tle data file failed 
!       90 - Swath width is not given correctly
!       99 - Satellite name is not correct


CONTAINS

   SUBROUTINE Sgp4_track0(lons, lats, nymd, nhms, deltamin, p, rc)

      real(r8), pointer       :: lons(:)  ! Ground track longitudes [degrees]  
      real(r8), pointer       :: lats(:)  ! Ground track latitudes  [degrees]  
      integer,  intent(in)    :: nymd(2), nhms(2)
      real(r8), intent(in)    :: deltamin
      integer, intent(out)    :: rc

!      Other Variables
       Character :: typerun, typeinput
       Character(len=30) :: InFileName

       Character(len=30) :: MonStr,Monthtitle(12) 
       Integer           :: Code, NumSats, TotalNumSats, k, error, whichconst
       REAL(r8)          :: ro(3),vo(3), startmfe, stopmfe
       REAL(r8)          :: J2,mu1, RadiusEarthKm,VKmPerSec, xke
       REAL(r8)          :: T, sec, JD, j3, j4, j3oj2, tumin
       !INTEGER           :: i,j, Year,yr,mon,day,hr,min
       INTEGER           :: i,j,yr, Year,Day,mon,hr,min
       INTEGER           :: YrPer, DayPer,MonPer,HrPer,MinutePer
       INTEGER           :: Topmin, YearPer2,MonPer2,DayPer2,HrPer2,MinPer2
       REAL(r8)          :: SecPer, SecPer2, ttemp, JDSatEpochPer, JDPER
       INTEGER           :: nymdPer(2), nhmsPer(2)
       INTEGER           :: sayac = 1


        REAL(r8) :: s_fraction2day, e_fraction2day  !, ntime_day
        INTEGER  :: sYear, sMonth, sDay, sHour, sMinute  
        INTEGER  :: eYear, eMonth, eDay, eHour, eMinute  
        REAL(r8) :: sSeconds
        REAL(r8) :: eSeconds
        REAL(r8) :: Latgc, Latgd, Lon, Hellp
        INTEGER  :: ierr1, sayac1, rc1

       type (TLE) :: p

       rc = 0
       rc1 = 0

!      Defined options addapted from main program
       Opsmode = 'i'    ! improved sgp4 operation
       typerun = 'M'    ! maunual operation
       typeinput = 'E'; ! full epoch input
       whichconst = 84  ! input('input constants 721, 72, 84 ');


!rad = 180.0 / pi;
       CALL getgravconst(whichconst, tumin, mu1, radiusearthkm, xke, j2, j3, j4, j3oj2)
       VKmPerSec = RadiusEarthKm * xke/60.0D0

       MonthTitle( 1)= 'Jan'
       MonthTitle( 2)= 'Feb'
       MonthTitle( 3)= 'Mar'
       MonthTitle( 4)= 'Apr'
       MonthTitle( 5)= 'May'
       MonthTitle( 6)= 'Jun'
       MonthTitle( 7)= 'Jul'
       MonthTitle( 8)= 'Aug'
       MonthTitle( 9)= 'Sep'
       MonthTitle(10)= 'Oct'
       MonthTitle(11)= 'Nov'
       MonthTitle(12)= 'Dec'


! infilename = 'SGP4-VER.TLE'; %input('input elset filename: ','s');
! infile = fopen(infilename, 'r');
! if (infile == -1)
!     fprintf(1,'Failed to open file: %s\n', infilename);
!     return;
! end
! outfile = fopen('tmat.out', 'wt');



      !InFileName = 'SGP4-VER.TLE'
      !@OPEN(10,FILE = InFileName ,STATUS='OLD', ACCESS = 'SEQUENTIAL' )


      NumSats = 0
      Numsats = NumSats + 1
      CALL getr_time(nymd(1), nhms(1), s_fraction2day, sYear, sMonth, sDay, sHour, sMinute, sSeconds ) ! handle time 
      CALL getr_time(nymd(2), nhms(2), e_fraction2day, eYear, eMonth, eDay, eHour, eMinute, eSeconds)

!       CALL TwoLine2RVSGP4 ( NumSats,typerun,typeinput,whichconst,            &
!                             startmfe,stopmfe,deltamin,Code,                  &
!                             sYear, sMonth, sDay, sHour, sMinute, sSeconds,   &
!                             eYear, eMonth, eDay, eHour, eMinute, eSeconds   ) !initial prediction at epoch and reading from TLE
!
!

      CALL TwoLine2RVSGP4 ( NumSats,typerun,typeinput,whichconst,            &
                            startmfe,stopmfe,deltamin,Code,                              &
                            sYear, sMonth, sDay, sHour, sMinute, sSeconds,               &
                            eYear, eMonth, eDay, eHour, eMinute, eSeconds, p, rc1) !initial prediction at epoch and reading from TLE

!           Find how many members we have ...
       CALL SGP4 ( whichconst, T, Ro, Vo, Error )
       T = startmfe
       sayac = 1
       DO WHILE ( (T.lt.stopmfe).and.(Error.eq.0) )
                T = T + DeltaMin
                IF (T.gt.stopmfe) THEN
                    T = stopmfe
                ENDIF
                sayac = sayac + 1
       END DO
       ALLOCATE(lons(1:sayac),  stat=ierr1) ! deallocate in the driver
       ALLOCATE(lats(1:sayac),  stat=ierr1) ! deallocate in the driver
       if ( ierr1 /= 0 ) then
         rc = 3 !
         return
       endif


       DO WHILE (Code.ne.999)
            T = 0.0D0
            CALL SGP4 ( whichconst, T, Ro, Vo, Error )
            ! now initialize time variables
            T      = startmfe

            ! check so the first value isn't written twice
            IF ( DABS(T).gt.0.00000001D0 ) THEN
                 !print*, " in if ", T,  DeltaMin
                T = T - DeltaMin
                 !print*, " in if = ", T,  DeltaMin
              ENDIF

            sayac1 = 1

            DO WHILE ( ( (T.lt.stopmfe).and.(Error.eq.0) )) ! .AND. (Sayac1<Sayac) )
   !         print*,"i = ", sayac1, T, stopmfe, Error
                T = T + DeltaMin
                IF (T.gt.stopmfe) THEN
                    T = stopmfe
                ENDIF
                CALL SGP4 ( whichconst, T, Ro, Vo, Error )

                IF (Error .gt. 0 ) THEN
                    Write(*,*) '# Error in SGP4 .. ', Error
                ENDIF

                IF ( error .eq. 0) THEN

                 IF ((typerun.ne.'V').and.(typerun.ne.'C')) THEN
                     JD = JDSatEpoch + T/1440.0D0
                     CALL INVJDAY( JD, Year,Mon,Day,Hr,Min, Sec )
                     IF (Year.ge.2000) THEN
                         Yr = Year - 2000
                       ELSE
                         Yr = Year - 1900
                       ENDIF
                     MonStr = MonthTitle( Mon )
 
                 ELSE
!                      WRITE( 11,800 )
!      &                   T,ro(1),ro(2),ro(3),vo(1),vo(2),vo(3)
!   800  FORMAT(F17.8,3F17.8,3(1X,F14.9))
! 
! c                     call rv2coe(ro, vo, mu, p, a, ecc, incl, node, 
! c     &                           argp, nu, m, arglat, truelon, 
! c     &                           lonper )
! c
! c                     write(11,801) T ,ro(1),ro(2),ro(3),vo(1),vo(2),
! c     &                             vo(3),a, ecc, incl*rad, node*rad,
! c     &                             argp*rad, nu*rad, m*rad
! 
!   801  FORMAT(F17.8,3F17.8,3(1X,F13.9),f15.6,f9.6,f11.5,f11.5,f11.5,
!      &          f11.5,f11.5)
! 
                 ENDIF ! typerun
                ENDIF ! if error
                CALL ijk2llE ( Ro, JD, Latgc, Latgd, Lon, Hellp )

!                 lons(sayac1) = Lon * rad2deg
!                 !lats(sayac1) = Latgc * rad2deg
!                 lats(sayac1) = Latgd * rad2deg

                lons(sayac1) = Lon * rad2deg
                lats(sayac1) = Latgc * rad2deg
                lats(sayac1) = Latgd * rad2deg
                sayac1 = sayac1 + 1

!                print*, sayac1, lats(sayac1),lats(sayac1)  

             END DO ! propagating the case
 
             ! get code for next satellite if it's there
            Numsats = NumSats + 1

!             CALL TwoLine2RVSGP4 ( NumSats,typerun,typeinput,whichconst,      &
!                                   startmfe,stopmfe,deltamin,Code,            &
!                             sYear, sMonth, sDay, sHour, sMinute, sSeconds,   &
!                             eYear, eMonth, eDay, eHour, eMinute, eSeconds   ) !initial prediction at epoch and reading from TLE


            CALL TwoLine2RVSGP4 ( NumSats,typerun,typeinput,whichconst,      &
                                  startmfe,stopmfe,deltamin,Code,            &
                            sYear, sMonth, sDay, sHour, sMinute, sSeconds,   &
                            eYear, eMonth, eDay, eHour, eMinute, eSeconds, p, rc   ) !initial prediction at epoch and reading from TLE


!print*,  NumSats,typerun,typeinput,whichconst,      &
!                                  startmfe,stopmfe,deltamin,Code,            &
!                            sYear, sMonth, sDay, sHour, sMinute, sSeconds,   &
!                            eYear, eMonth, eDay, eHour, eMinute, eSeconds
            Code = 999
          ENDDO ! while through file

!print*, "num of sat", Numsats
!print*, "lat", lats

        END SUBROUTINE Sgp4_track0


   SUBROUTINE Sgp4_track1(lons, lats, iSat, nymd, nhms, deltatsec, rc)
        IMPLICIT NONE
 
 ! !INPUT PARAMETERS:
       integer, intent(in)  :: iSat     ! Satellite nunber
       integer, intent(in)  :: nymd(2)  ! Beginning/ending date: YYYYMMDD
       integer, intent(in)  :: nhms(2)  ! Beginning/ending time: HHMMSS
       INTEGER, intent(in)  :: deltatsec   ! Time step [secs]
 
 ! !OUTPUT PARAMETERS:
       real(r8), pointer    :: lons(:)  ! Ground track longitudes [degrees]  
       real(r8), pointer    :: lats(:)  ! Ground track latitudes  [degrees]  
       integer, intent(out) :: rc       ! Error code = 0 all is well
                                       !            = 3 memory allocation error
 ! !Other Parameters:
       type (TLE)           :: p
       real(r8)             :: deltat
       
       deltat = real(deltatsec)/60.0
 
       rc = 0 ! Initiate error code to 0
       if ( rc /= 0 ) return
       CALL iSat2p(iSat,p,rc)
       Call Sgp4_track0(lons, lats, nymd, nhms, deltat, p, rc) ! default is repeated
       if (rc>0) return
 
   END SUBROUTINE Sgp4_track1
 
! ............................................................................

  SUBROUTINE Sgp4_track2(lons, lats, Sat_name, nymd, nhms, deltatsec, rc)

       IMPLICIT NONE

! !INPUT PARAMETERS:
       character(len=*), intent(in) :: Sat_name ! Satellite name
       integer, intent(in)  :: nymd(2)  ! Beginning/ending date: YYYYMMDD
       integer, intent(in)  :: nhms(2)  ! Beginning/ending time: HHMMSS
       INTEGER, intent(in)  :: deltatsec   ! Time step [secs]

! !OUTPUT PARAMETERS:
       real(r8), pointer    :: lons(:)  ! Ground track longitudes [degrees]  
       real(r8), pointer    :: lats(:)  ! Ground track latitudes  [degrees]  
       integer, intent(out) :: rc       ! Error code = 0 all is well
                 
! !Other parameters
       logical :: flagfile
       type (TLE) :: p
       real(r8)            :: deltat
       
       deltat = real(deltatsec)/60.0
       rc = 0 ! Initiate error code to 0
       CALL isfile(Sat_name, flagfile)
       CALL name2p(Sat_name, p, rc, flagfile)
       if (rc>0) return

       Call Sgp4_track0(lons, lats, nymd, nhms, deltat, p, rc)   ! default is repeated

       !!Call Sgp4_track0(lons, lats, nymd, nhms, deltat, p, rc)
        !print*,   "p%TICRDNO  = ", p%TICRDNO              ! TLE first line
        !print*,   "p%TSatNum  = ", p%TSatNum  
        !print*,   "p%TSatName = ", p%TSatName 
        !print*,   "p%TEpochYr = ", p%TEpochYr 
        !print*,   "p%TEpDay   = ", p%TEpDay   
        !print*,   "p%TNDot    = ", p%TNDot    
        !print*,   "p%TNDDot   = ", p%TNDDot   
        !print*,   "p%Tnexp    = ", p%Tnexp    
        !print*,   "p%TBStar   = ", p%TBStar   
        !print*,   "p%Tbexp    = ", p%Tbexp    
        !print*,   "p%TEPHTYP  = ", p%TEPHTYP  
        !print*,   "p%TELNO    = ", p%TELNO    

        !print*,   "p%TICRDNO  = ", p%TICRDNO              ! TLE second line
        !print*,   "p%TInclo   = ", p%TInclo  
        !print*,   "p%Tnodeo   = ", p%Tnodeo  
        !print*,   "p%TEcco    = ", p%TEcco   
        !print*,   "p%TArgpo   = ", p%TArgpo   
        !print*,   "p%TMo      = ", p%TMo      
        !print*,   "p%TNo      = ", p%TNo      
        !print*,   "p%TREVI    = ", p%TREVI    
        !print*,   "p%Tstartmfe = ", p%Tstartmfe 
        !print*,   "p%Tstopmfe  = ", p%Tstopmfe  
        !print*,   "p%TDeltaMin = ", p%TDeltaMin 
        !!pause


       if (rc>0) return
 
  END SUBROUTINE Sgp4_track2 

! ............................................................................

 SUBROUTINE name2p(name1, p, rc, flagfile)
     IMPLICIT NONE
 
! !INPUT PARAMETERS:
       character(len=*), intent(in) :: name1 ! Satellite name
       logical, intent(in) :: flagfile
       !Character(len=12), intent(in), optional :: InFileName

! !OUTPUT PARAMETERS:

! !Other parameters
       integer         :: iSat
       integer         :: rc
       type (TLE)                 :: p

       rc = 0
       IF (flagfile) THEN
          iSat = -1
          CALL iSat2p(iSat, p, rc, name1)

       else
          CALL satname2int(name1, iSat, rc)
          CALL iSat2p(iSat, p, rc)
       endif

 
 END SUBROUTINE name2p

! ............................................................................

SUBROUTINE iSat2p(iSat, p, rc, InFileName)
     IMPLICIT NONE
 
! !INPUT PARAMETERS:
       Character(len=*), intent(in), optional :: InFileName
       integer, intent(in)                    :: iSat

! !OUTPUT PARAMETERS:

       integer, intent(out)                :: rc ! error code

!  !Other parameters!
       type (TLE)                 :: p
       real(r8)                            :: co
       logical                             :: fileflag


     rc = 0
     IF (PRESENT(InFileName)) THEN        ! Printing of B is conditional
        fileflag = .TRUE.      !   on its presence
        !print*, " we are in iSat2p TRUE", InFileName 
     ELSE
        fileflag = .FALSE.
        !print*, " we are in iSat2p FALSE" 
     ENDIF

     IF (fileflag) then
         call TLE_Read(InFileName, p, rc)


 !print*, " ++++++++++++++++++++++++++ "
 !print*, InFileName
 !print*, " "
 !print*, p
 !print*, " "

 !print*, "line 1 "
 !       print*, p%TICRDNO,p%TSatNum,p%TSatName,p%TEpochYr,p%TEpDay,       &
 !                           p%TNDot,p%TNDDot,p%Tnexp,p%TBStar,p%Tbexp,p%TEPHTYP,p%TELNO
   
 !print*, " line 2"
 !       print*,  p%TICRDNO,p%TInclo,p%Tnodeo,p%TEcco,p%TArgpo,p%TMo,p%TNo,p%TREVI

 !print*, " ++++++++++++++++++++++++++ "


         !print*, "rc is = ", RefDate, RefTime, rc
         !print*, RefDate, RefTime
     else
!         RefDate  = 20091009 ! this is date for following parameters
!         RefTime  = 021057   ! this is time 
!         !print*, RefDate, RefTime
!         !CALL satname2int(Sat_name, iSat, rc)
      if (iSat==1) then !aqua
            p%TICRDNO  =            2
            p%TSatNum  =        27424
            p%TSatName =        "02022A"
            p%TEpochYr =           10
            p%TEpDay   =    78.496142440000000
            p%TNDot    =   6.28999999999999992E-006
            p%TNDDot   =    0.0000000000000000
            p%Tnexp    =            0
            p%TBStar   =   0.14949999999999999
            p%Tbexp    =           -3
            p%TEPHTYP  =            0
            p%TELNO    =          587
            p%TICRDNO  =            2
            p%TInclo   =    98.195099999999996
            p%Tnodeo   =    21.329799999999999
            p%TEcco    =   1.58400000000000001E-004
            p%TArgpo   =    57.276699999999998
            p%TMo      =    302.85620000000000
            p%TNo      =    14.571296680000000
            p%TREVI    =        41885
            p%Tstartmfe =  Small 
            p%Tstopmfe  =  Small
            p%TDeltaMin =  Small 

      elseif (iSat==2) then !calipso
            p%TICRDNO  =            2
            p%TSatNum  =        29108
            p%TSatName =  "06016B"
            p%TEpochYr =           10
            p%TEpDay   =    77.878981859999996
            p%TNDot    =   2.26999999999999987E-006
            p%TNDDot   =    0.0000000000000000
            p%Tnexp    =            0
            p%TBStar   =   0.60279000000000005
            p%Tbexp    =           -4
            p%TEPHTYP  =            0
            p%TELNO    =          239
            p%TICRDNO  =            2
            p%TInclo   =    98.189200000000000
            p%Tnodeo   =    22.948599999999999
            p%TEcco    =   1.18300000000000002E-004
            p%TArgpo   =    89.483199999999997
            p%TMo      =    270.65199999999999
            p%TNo      =    14.571183500000000
            p%TREVI    =        20685
            p%Tstartmfe =   Small
            p%Tstopmfe  =   Small
            p%TDeltaMin =   Small

       elseif (iSat==3) then !cloudsat
            p%TICRDNO  =            2
            p%TSatNum  =        29107
            p%TSatName =  "06016A"
            p%TEpochYr =           10
            p%TEpDay   =    77.878769009999999
            p%TNDot    =   1.01000000000000007E-006
            p%TNDDot   =    0.0000000000000000
            p%Tnexp    =            0
            p%TBStar   =   0.32508999999999999
            p%Tbexp    =           -4
            p%TEPHTYP  =            0
            p%TELNO    =          229
            p%TICRDNO  =            2
            p%TInclo   =    98.188999999999993
            p%Tnodeo   =    22.898800000000001
            p%TEcco    =   1.12200000000000003E-004
            p%TArgpo   =    92.728600000000000
            p%TMo      =    267.40660000000003
            p%TNo      =    14.571181390000000
            p%TREVI    =        20687
            p%Tstartmfe =   Small 
            p%Tstopmfe  =   Small
            p%TDeltaMin =   Small
        elseif (iSat==4) then !aura
            p%TICRDNO  =            2
            p%TSatNum  =        28376
            p%TSatName =  "04026A"
            p%TEpochYr =           10
            p%TEpDay   =    78.226799090000000
            p%TNDot    =   3.76999999999999990E-006
            p%TNDDot   =    0.0000000000000000
            p%Tnexp    =            0
            p%TBStar   =   0.93589000000000000
            p%Tbexp    =           -4
            p%TEPHTYP  =            0
            p%TELNO    =          751
            p%TICRDNO  =            2
            p%TInclo   =    98.196899999999999
            p%Tnodeo   =    23.139199999999999
            p%TEcco    =   1.34000000000000005E-004
            p%TArgpo   =    64.772300000000001
            p%TMo      =    295.36279999999999
            p%TNo      =    14.571231830000000
            p%TREVI    =        30186
            p%Tstartmfe =   Small
            p%Tstopmfe  =   Small
            p%TDeltaMin =   Small
        elseif (iSat==5) then !terra
            p%TICRDNO  =            2
            p%TSatNum  =        25994
            p%TSatName =  "99068A"
            p%TEpochYr =           10
            p%TEpDay   =    78.475268459999995
            p%TNDot    =   8.99999999999999959E-007
            p%TNDDot   =    0.0000000000000000
            p%Tnexp    =            0
            p%TBStar   =   0.29984000000000000
            p%Tbexp    =           -4
            p%TEPHTYP  =            0
            p%TELNO    =          610
            p%TICRDNO  =            2
            p%TInclo   =    98.209800000000001
            p%Tnodeo   =    154.40060000000000
            p%TEcco    =   6.33999999999999961E-005
            p%TArgpo   =    103.11510000000000
            p%TMo      =    257.01220000000001
            p%TNo      =    14.571126820000000
            p%TREVI    =        54524
            p%Tstartmfe =   Small 
            p%Tstopmfe  =   Small
            p%TDeltaMin =   Small


! P(6) = SatParam%nrevolution
!  P(8) = (SatParam%nrevolution/co)**(-2.0/3.0);   !7083.4456; % sattellite semi-major axis -- km
!  P(9) = (P(8)/331.24915)**(3.0/2.0)*60; ! Sat Period in seconds


!         elseif (iSat==2) then !calipso
!            SatParam%eccentricity    =  0.0001054   ! eccentricity
!            SatParam%anomaly         =  276.1217    ! mean anamoly
!            SatParam%inclination     =  98.1903     ! sattelite inclination angle
!            SatParam%ascension       =  237.3156    ! right ascension of node
!            SatParam%omega           =  84.0117     ! degree argument of perigee
!            SatParam%nrevolution     =  14.571294  ! num of rev per data (nbar) mean motion
!            SatParam%altitude        =  702         ! km
!            SatParam%semimajor       =  -9999999.0  ! km later will be calculated
!            SatParam%period          =  -9999999.0  ! period in seconds 
!         elseif (iSat==3) then !cloudsat
!            SatParam%eccentricity    =  0.0000864   ! eccentricity
!            SatParam%anomaly         =  270.1434    ! mean anamoly
!            SatParam%inclination     =  98.1903     ! sattelite inclination angle
!            SatParam%ascension       =  236.8604    ! right ascension of node
!            SatParam%omega           =  89.9856     ! degree argument of perigee
!            SatParam%nrevolution     =  14.5712970  ! num of rev per data (nbar) mean motion
!            SatParam%altitude        =  702         ! km
!            SatParam%semimajor       =  -9999999.0  ! km later will be calculated
!            SatParam%period          =  -9999999.0  ! period in seconds 
!         elseif (iSat==4) then !aura
!            SatParam%eccentricity    =  0.0001545   ! eccentricity
!            SatParam%anomaly         =  268.2933    ! mean anamoly
!            SatParam%inclination     =  98.1883     ! sattelite inclination angle
!            SatParam%ascension       =  237.1841    ! right ascension of node
!            SatParam%omega           =  91.8445     ! degree argument of perigee
!            SatParam%nrevolution     =  14.5712533  ! num of rev per data (nbar) mean motion
!            SatParam%altitude        =  702         ! km
!            SatParam%semimajor       =  -9999999.0  ! km later will be calculated
!            SatParam%period          =  -9999999.0  ! period in seconds 
!         elseif (iSat==5) then !terra
!            SatParam%eccentricity    =  0.0001307   ! eccentricity
!            SatParam%anomaly         =  293.0040    ! mean anamoly
!            SatParam%inclination     =  98.2183     ! sattelite inclination angle
!            SatParam%ascension       =  359.8416    ! right ascension of node
!            SatParam%omega           =  8.8523      ! degree argument of perigee
!            SatParam%nrevolution     =  14.57102212 ! num of rev per data (nbar) mean motion
!            SatParam%altitude        =  702         ! km
!            SatParam%semimajor       =  -9999999.0  ! km later will be calculated
!            SatParam%period          =  -9999999.0  ! period in seconds 
         else
               rc = 5 !satellite is not defined
               return
          endif 


       endif


 
END SUBROUTINE iSat2p


! ............................................................................


SUBROUTINE isfile(Sat_name, flagfile)
     implicit none

! !INPUT PARAMETERS:
       character(len=*), intent(in) :: Sat_name
! !OUTPUT PARAMETERS:
       logical, intent(out)         :: flagfile
! !Other parameters!
       integer          :: length_string
       character(len=4) :: subsat


      ! check if this is file name or satellite name...
      length_string = LEN(TRIM(Sat_name))
      subsat = TRIM(Sat_name(length_string-3:length_string))
      IF ( (".TLE" .EQ. subsat) .OR. (".tle" .EQ. subsat) .OR.  &
          (".Tle" .EQ. subsat) )  THEN
              flagfile = .TRUE.
      ELSE
              flagfile = .FALSE.
      ENDIF

END SUBROUTINE isfile
 

! ............................................................................


! ............................................................................
! co = 8681660.4;



!PROBLEM 1.7 

!A satellite in earth orbit has a semi-major axis of 6,700 km and an eccentricity of 0.01.
!Calculate the satellite's altitude at both perigee and apogee.


!SOLUTION,
!
!   Given:  r = 6,700 km
!           e = 0.01!
!
!   Equation (1.21) and (1.22),!
!
!      Rp = r x (1 - e)
!      Rp = 6,700 x (1 - .01)
!      Rp = 6,633 km
!
!      Altitude @ perigee = 6,633 - 6,375 = 258 km!
!
!      Ra = r x (1 + e)
!      Ra = 6,700 x (1 + .01)
!      Ra = 6,767 km!
!
!      Altitude @ apogee = 6,767 - 6,375 = 392 km

! ............................................................................


SUBROUTINE  Groundtrack(lons, lats, p, GMSTo, Tf, dT, basla, son,  rc)   

       IMPLICIT NONE
! !INPUT PARAMETERS:

      real(r8), pointer       :: lons(:)  ! Ground track longitudes [degrees]  
      real(r8), pointer       :: lats(:)  ! Ground track latitudes  [degrees]  
      REAL(r8), DIMENSION(:), INTENT(IN)  :: p
      REAL(r8), INTENT(IN)                :: GMSTo
      !INTEGER, DIMENSION(:), INTENT(IN)   :: tt     ! time vector
      INTEGER, INTENT(IN)                 :: dt, Tf  ! seconds
      INTEGER, INTENT(IN)                 :: basla, son
      integer, intent(out):: rc       
! Other variables
      REAL(r8), DIMENSION(:),  ALLOCATABLE :: t1, Lat_rad, Long_rad
      REAL(r8), DIMENSION(3,3) :: RotM1_g, RotM3_g
      INTEGER,  DIMENSION(1:2)             :: bp
      REAL(r8) :: a, ecc, inc, O, w, MA
      REAL(r8), PARAMETER ::  w_earth  = 7.2921158553e-5 !rad/s  %Rotation rate of Earth
      REAL(r8)            ::  n  !mean motion of satellite
      REAL(r8)            ::  GMST, temp1, alt
      REAL(r8)            ::  M  ! mean anamoly in radians
      REAL(r8)            ::  nu !True anomaly in rad
      INTEGER             ::  say, k, i, j
      INTEGER :: ierr1, tempind
      REAL(r8), DIMENSION(3,1) :: ECEF, R, V


       rc = 0
       a   = p(8)
       ecc = p(1) 
       inc = p(3) * cd2r ! deg2rad
       O   = p(4) * cd2r
       w   = p(5) * cd2r
       MA =  p(2) * cd2r

       n = (mu/a**3.0)**(.5)  !Mean motion of satellite
       CALL built_vecsize( real(basla,r8), real(son,r8), real(dt,r8), say)
       bp(1)    = 0
       bp(2)    = say
       k        = 2

       ALLOCATE(lons(1:say),  stat=ierr1) ! deallocate in the driver
       ALLOCATE(lats(1:say),  stat=ierr1) ! deallocate in the driver
       ALLOCATE (Lat_rad(1:say),  stat = ierr1)
       ALLOCATE (Long_rad(1:say), stat = ierr1)
       if ( ierr1 /= 0 ) then
         rc = 3 !
         return
       endif

       tempind = 1
       do j=basla,son, dT
          temp1 = GMSTo + w_earth*j !t1(j)
          CALL zeroTo360(temp1,1, GMST)  !GMST in radians
          temp1 = MA + n*j !t1(j)
          CALL zeroTo360(temp1,1, M)  !Mean anomaly in rad
          nu = calc_nu(M,ecc)
          CALL pqw2ECI(a,ecc,inc,O,w, nu, R, V)
          CALL Rotate(GMST, RotM1_g, RotM3_g)
          ECEF = MATMUL(RotM3_g, R)
          CALL ECEF2LLA(ECEF(1,1)*1000.0, ECEF(2,1)*1000.0, ECEF(3,1)*1000.0, Lat_rad(tempind), Long_rad(tempind), alt)
          tempind = tempind + 1
          ! if j > 1 && ((Long_rad(j)-Long_rad(j-1)) < -pi || (Long_rad(j)-Long_rad(j-1)) > pi)
          ! bp(k) = j-1
          ! k     = k+1
          ! end
          ! end
          ! bp(length(bp)+1) = length(time)
          ! Lat              = Lat_rad.*r2d
          ! Long             = Long_rad.*r2d
       end do


       lons(1:say) = Long_rad(1:say) * cr2d
       lats(1:say) = Lat_rad(1:say)  * cr2d

END SUBROUTINE Groundtrack


! ............................................................................


SUBROUTINE pqw2ECI(a,ecc,inc,Omega,w, nu, R, V)

       IMPLICIT NONE
       
       REAL(r8), INTENT(IN) :: a, ecc, inc, Omega, w, nu
       REAL(r8), DIMENSION(3,1) :: R, V, R_pqw, V_pqw
       REAL(r8), DIMENSION(3,3) :: RotM1_O, RotM3_O, RotM1_inc, RotM3_inc, RotM1_w, RotM3_w
       REAL(r8), PARAMETER      :: U = 398600.4415; !Gravitational Constant for Earth (km^3/s^2) same as mu
       REAL(r8)  :: pa


       pa = a*(1.0-ecc**(2.0)) 

!      CREATING THE R VECTOR IN THE pqw COORDINATE FRAME
       R_pqw(1,1) = pa*cos(nu)/(1.0 + ecc*cos(nu))
       R_pqw(2,1) = pa*sin(nu)/(1.0 + ecc*cos(nu))
       R_pqw(3,1) = 0.0
!      CREATING THE V VECTOR IN THE pqw COORDINATE FRAME    
       V_pqw(1,1) = -(U/pa)**(.5)*sin(nu)
       V_pqw(2,1) =  (U/pa)**(.5)*(ecc + cos(nu))
       V_pqw(3,1) =   0.0
!      ROTATING THE pqw VECOTRS INTO THE ECI FRAME (ijk)
       CALL Rotate(-Omega, RotM1_O, RotM3_O)
       CALL Rotate(-inc, RotM1_inc, RotM3_inc)
       CALL Rotate(-w, RotM1_w, RotM3_w)
       R = MATMUL(MATMUL(MATMUL(RotM3_O, RotM1_inc), RotM3_w), R_pqw)
       V = MATMUL(MATMUL(MATMUL(RotM3_O, RotM1_inc), RotM3_w), V_pqw)

END SUBROUTINE pqw2ECI

! ............................................................................

SUBROUTINE Rotate(angle, RotM1, RotM3)
      IMPLICIT NONE
        REAL(r8), DIMENSION(:,:), INTENT(INOUT) :: RotM1, RotM3
        REAL(r8), INTENT(IN)                    :: angle
        REAL(r8) :: a1, a2


        RotM3 = 0
        a1 = cos(angle)
        a2 = sin(angle)

        RotM3(1,1) = a1
        RotM3(1,2) = a2
        RotM3(2,1) = -a2
        RotM3(2,2) = a1
        RotM3(3,3) = 1.0

        RotM1 = 0
        a1 = cos(angle)
        a2 = sin(angle)
        RotM1(1,1) = 1.0
        RotM1(2,2) = a1
        RotM1(2,3) = a2
        RotM1(3,2) = -a2
        RotM1(3,3) = a1


END SUBROUTINE Rotate

! ............................................................................

REAL(r8) FUNCTION calc_nu(M,ecc)
!      Determining eccentric anomaly from mean anomaly
       IMPLICIT NONE

       REAL(r8), PARAMETER :: tol = 10**(-8)
       REAL(r8), INTENT(IN) :: M,ecc
       REAL(r8)             :: E,  Etemp, Etemp2


       if ( ( (M > -pi).AND.(M < 0.0) ).OR.( M > pi) ) then
          E = M - ecc
       else
          E = M + ecc
       endif
       Etemp  = E + (M - E + ecc*sin(E))/(1.0-ecc*cos(E))
       Etemp2 = E
       do while (abs(Etemp - Etemp2) > tol)
          Etemp = Etemp2
          Etemp2 = Etemp + (M - Etemp + ecc*sin(Etemp))/(1.0-ecc*cos(Etemp))
       enddo
       E = Etemp2
       calc_nu = atan2((sin(E)*(1.0-ecc**2.0)**(.5)), (cos(E)-ecc) )
END FUNCTION calc_nu

! ............................................................................

SUBROUTINE zeroTo360(x, unit, y)
       IMPLICIT NONE

       INTEGER,                INTENT(IN)  :: unit
       REAL(r8), INTENT(INOUT)  :: x
       REAL(r8), INTENT(OUT) :: y
       REAL(r8)   :: deg
       INTEGER    :: j


       if (unit.EQ.1) then
           deg = 2.0*pi
       else 
           deg = 360.0
       endif
       if (x >= deg) then 
          x = x - aint(x/deg)*deg
       elseif (x < 0) then
          x = x - (aint(x/deg) - 1)*deg
       endif
       y = x
END SUBROUTINE zeroTo360




!-------------------------------------------------------------------------


!                         -----------------
!                         INTERNAL ROUTINES
!                         -----------------
!-------------------------------------------------------------------------

Subroutine satname2int(name, satnum, rc)
       IMPLICIT NONE
       character(len=*), intent(in) :: name ! Satellite name
       integer, intent(out) :: satnum       ! Satellite number
       integer, intent(out) :: rc

       rc = 0
       if ((name.EQ."AQUA").or.(name.EQ."aqua").or.(name.EQ."Aqua") .or. (name.EQ."02022A") ) then         
           satnum = 1
       elseif ((name.EQ."calipso").or.(name.EQ."CALIPSO").or.(name.EQ."Calipso")) then         
           satnum = 2
       elseif ((name.EQ."cloudsat").or.(name.EQ."CLOUDSAT").or.(name.EQ."CloudSat")) then         
           satnum = 3
       elseif ((name.EQ."aura").or.(name.EQ."AURA").or.(name.EQ."Aura")) then         
           satnum = 4
       elseif ((name.EQ."terra").or.(name.EQ."TERRA").or.(name.EQ."Terra")) then         
           satnum = 5
       else 
          rc =99
          return
       endif
end Subroutine satname2int

! ............................................................................

! SUBROUTINE  ECI2ECEF(iSat,fraction2day,  ECI_est, ECEF_est)
!         IMPLICIT NONE
!         INTEGER, INTENT(IN)  :: iSat
!         REAL(r8), DIMENSION(:,:), INTENT(IN)  :: ECI_est
!         REAL(r8), DIMENSION(:,:), INTENT(OUT) :: ECEF_est
!         REAL(r8), DIMENSION(3,3) :: RotM        ! rotation matrix from In2ECEF         
!         INTEGER   :: t1 
!         REAL(r8)      :: Goft, a1, a2
!         REAL(r8)      :: fraction2day 
! 
!         t1 =  Day2year(iSat)
!         Goft = g0 + g1p*t1 + g2p(iSat) * fraction2day
!         a1 = cos(Goft);
!         a2 = sin(Goft);
!         RotM = 0.0
!         RotM(1,1) = a1
!         RotM(1,2) = a2
!         RotM(2,1) = -a2
!         RotM(2,2) = a1
!         RotM(3,3) = 1
!         ECEF_est = MATMUL(RotM, ECI_est)
! END SUBROUTINE ECI2ECEF  



SUBROUTINE ECEF2LLA(x, y, z, lat, lon, alt)
!       % ECEF3LLA - convert earth-centered earth-fixed (ECEF)
!       %            cartesian coordinates to latitude, longitude,
!       %            and altitude
!       %
!       % USAGE:
!       % [lat,lon,alt] = ecef2lla(x,y,z)
!       %
!       % lat = geodetic latitude (radians)
!       % lon = longitude (radians)
!       % alt = height above WGS84 ellipsoid (m)
!       % x = ECEF X-coordinate (m)
!       % y = ECEF Y-coordinate (m)
!       % z = ECEF Z-coordinate (m)
!       %
!       % Notes: (1) This function assumes the WGS84 model.
!       %        (2) Latitude is customary geodetic (not geocentric).
!       %        (3) Inputs may be scalars, vectors, or matrices of the same
!       %            size and shape. Outputs will have that same size and shape.
!       %        (4) Tested but no warranty; use at your own risk.
!       %        (5) Michael Kleder, April 2006
       REAL(r8), INTENT(IN)  :: x,y,z
       REAL(r8), INTENT(OUT) :: lat,lon,alt
!      % WGS84 ellipsoid constants:
       REAL(r8), PARAMETER  :: a = 6378137 ! those should be defined outside
       REAL(r8), PARAMETER  :: e = 8.1819190842622e-2
       REAL(r8), PARAMETER  :: pi = 3.14159265358979323846 
       REAL(r8)             :: b, ep, p, th, N
       LOGICAL k

       b   = sqrt(a**2 * (1-e**2))
       ep  = sqrt((a**2-b**2)/b**2)
       p   = sqrt(x**2+y**2)
       th  = atan2(a*z,b*p)
       lon = atan2(y,x)
       lat = atan2((z+ep**2*b*sin(th)**3),(p-e**2*a*cos(th)**3))
       N   = a/sqrt(1-e**2*sin(lat)**2)
       alt = p/cos(lat)-N
!%     !return lon in range [0,2*pi)
       !lon = mod(lon,2*pi) ! does not the same as matlab
       lon = lon-floor(lon/(2*pi))*(2*pi)
!      % correct for numerical instability in altitude near exact poles:
!      % (after this correction, error is about 2 millimeters, which is about
!      % the same as the numerical precision of the overall function)
       !k= (abs(x)<1.AND.abs(y)<1 )
       !print *, k
       !!alt(k) = abs(z(k))-b
END SUBROUTINE ECEF2LLA

! ............................................................................


SUBROUTINE getr_time(nymd, nhms, fraction2day, Year, Month, Day, Hour, Minute, Seconds)

          IMPLICIT  NONE

        INTEGER, INTENT(IN) :: nymd, nhms
        REAL(r8), INTENT(OUT)   :: fraction2day  !, ntime_day
        INTEGER :: Year, Month, Day, Hour, Minute  
        REAL(r8) :: Seconds

        Year   = int(nymd / 10000 )
        Month  = mod ( nymd,  10000 ) / 100
        Day    = mod ( nymd,    100 )
        Hour = int(nhms / 10000 )
        Minute = mod ( nhms,  10000 ) / 100
        Seconds = mod ( nhms,    100 )
        fraction2day = (Hour*60*60+Minute*60+Seconds)/(24.0*60.0*60.0)  !fraction

END SUBROUTINE getr_time


SUBROUTINE get_time(iSat, nymd, nhms, fraction2day, tt)
!SUBROUTINE get_time(iSat, nymd, nhms, fraction2day, ntime_day, tt)
        IMPLICIT  NONE
        INTEGER, INTENT(IN) :: iSat, nymd, nhms
        REAL(r8), INTENT(OUT)   :: fraction2day  !, ntime_day
        INTEGER,  DIMENSION(1:6), INTENT(OUT)   :: tt
        INTEGER :: Year, Month, Day, Hour, Minute, Seconds 

        Year   = int(nymd / 10000 )
        Month  = mod ( nymd,  10000 ) / 100
        Day    = mod ( nymd,    100 )
        Hour = int(nhms / 10000 )
        Minute = mod ( nhms,  10000 ) / 100
        Seconds = mod ( nhms,    100 )
        fraction2day = (Hour*60*60+Minute*60+Seconds)/(24.0*60.0*60.0)  !fraction
                                         !calculated from seconds  
        !ntime_day=(-ODS_Julian(RefDate(iSat))+ODS_Julian(nymd))+fraction2day
        tt = (/Year, Month, Day, Hour, Minute, Seconds/)

END SUBROUTINE get_time

! ............................................................................

integer function ODS_Julian ( CalDate )
      implicit NONE
      integer  CalDate  ! Calendar date in the format YYYYMMDD
                        !   where YYYY is the year, MM is the
                        !   month and DD is the day.  A negative
                        !   number implies that the year is B.C.
!     Other variables
!     ---------------
      integer  ::   Year
      integer  ::   Month
      integer  ::   Day
      integer  ::   iGreg  ! Gregorian Calendar adopted Oct 12, 1582
      parameter ( iGreg = 15 + 31 * ( 10 + 12 * 1582 ) )
      integer  ::   JulDay
      integer  ::   jy, jm, ja

      Year   =       CalDate / 10000
      Month  = mod ( CalDate,  10000 ) / 100
      Day    = mod ( CalDate,    100 )
!     Change year 0 to year 1
      if ( Year  .eq. 0 ) Year = 1
!     Account for the nonexisting year 0
      if ( Year  .lt. 0 ) Year = Year + 1
      if ( Month .gt. 2 ) then
         jy = Year
         jm = Month + 1
      else
         jy = Year  - 1
         jm = Month + 13
      endif
      JulDay = int ( 365.25  * jy )       &
             + int ( 30.6001 * jm )       &
             + Day + 1720995
!     Test whether to change to Gregorian Celendar
      if ( Day + 31 * ( Month + 12 * Year ) .ge. iGreg) then
        ja     = int ( 0.01 * jy )
        Julday = JulDay + 2 - ja + int ( 0.25 * ja )
      endif
      ODS_Julian = JulDay
      return
end function ODS_Julian

! ............................................................................

REAL(r8) FUNCTION get_fraction(t1)
       IMPLICIT NONE
       REAL(r8), INTENT(IN) :: t1
       REAL(r8) :: fraction2day, temp3, temp4 

       if (t1.EQ.0.0) then
           fraction2day = 0.0
       else if (t1.lt.1.0 .AND. t1.gt.0) then
           fraction2day = t1
       else
           temp3 = floor(t1)
           temp4 = t1
           if (temp3 == temp4) then 
             fraction2day = temp3 ! either is ok 
           else
             fraction2day = MOD(temp4,temp3)
           end if
       end if
       get_fraction = fraction2day
END FUNCTION get_fraction

! ............................................................................

REAL(r8) FUNCTION timeday2sidereal(tm)
       IMPLICIT NONE
!      Ref:  Fundementals of astrodynamics and Applications Vallado p192  
!      output in degrees
       INTEGER, DIMENSION(1:6), INTENT(IN) :: tm
       INTEGER :: yr, mo, day, hr, mi, sec 
       INTEGER, PARAMETER :: hr2sec = 60*60
       REAL(r8) :: temp1, JD, TUT1, TETAGMST
!timem = [1992 08  20 12 14 00]
       yr   = tm(1);
       mo   = tm(2);
       day  = tm(3);
       hr   = tm(4);
       mi   = tm(5);
       sec  = tm(6);
!      calc jul date 
       temp1 = ((real(sec,r8)/60.0 + real(mi,r8))/60.0) + real(hr,r8);
       JD = 367 * real(yr,r8) - floor(  7*  (real(yr,r8) + floor( (real(mo,r8)+9.0)/12.0))  / 4.0)    +   &
            floor(275*real(mo,r8)/9.0)  + real(day,r8) + 1721013.5 + temp1/24.0   !2.455113590937500e+06
!      Julian centuries for particular epoch (J200)
       TUT1 = (JD - 2451545.0) / 36525.0;
!      find GMST
       TETAGMST = 67310.54841 + (876600.0 * real(hr2sec,r8) + 8640184.812866)*TUT1 +  &
                  0.093104 * (TUT1**2) - 6.2*(10**(-6)) * (TUT1**3);  ! need extended precision  
      TETAGMST=mod(TETAGMST,86400.0);
      TETAGMST=TETAGMST/240; !seconds to degrees ! tothours = TETAGMST/15
      timeday2sidereal = TETAGMST
END FUNCTION timeday2sidereal

! ............................................................................


!* -----------------------------------------------------------------------------
!*
!*                           SUBROUTINE JDay
!*
!*  This subroutine finds the Julian date given the Year, Month, Day, and Time.
!*
!*  Author        : David Vallado                  719-573-2600    1 Mar 2001
!*
!*  Inputs          Description                    Range / Units
!*    Year        - Year                           1900 .. 2100
!*    Mon         - Month                          1 .. 12
!*    Day         - Day                            1 .. 28,29,30,31
!*    Hr          - Universal Time Hour            0 .. 23
!*    Min         - Universal Time Min             0 .. 59
!*    Sec         - Universal Time Sec             0.0D0 .. 59.999D0
!*    WhichType   - Julian .or. Gregorian calender   'J' .or. 'G'
!*
!*  Outputs       :
!*    JD          - Julian Date                    days from 4713 BC
!*
!*  Locals        :
!*    B           - Var to aid Gregorian dates
!*
!*  Coupling      :
!*    None.
!*
!*  References    :
!*    Vallado       2007, 189, Alg 14, Ex 3-14
!* -----------------------------------------------------------------------------
SUBROUTINE JDay ( Year,Mon,Day,Hr,Mini, Sec, JD )

        IMPLICIT NONE
        INTEGER, INTENT(IN)   :: Year, Mon, Day, Hr, Mini 
        REAL(r8), INTENT(IN)  :: Sec
        REAL(r8), INTENT(OUT) :: JD
       
        JD= 367.0D0 * Year                                       &
             - INT( (7* (Year+INT ( (Mon+9)/12) ) ) * 0.25D0 )   &
             + INT( 275*Mon / 9 )                                &
             + Day + 1721013.5D0                                 &
             + ( (Sec/60.0D0 + Mini ) / 60.0D0 + Hr ) / 24.0D0    
!            - 0.5D0*DSIGN(1.0D0, 100.0D0*Year + Mon - 190002.5D0) + 0.5D0 
      RETURN
END SUBROUTINE JDay



!* -----------------------------------------------------------------------------
!*
!*                           SUBROUTINE INVJDay
!*
!*  This subroutine finds the Year, month, day, hour, Minute and second
!*  given the Julian date. TU can be UT1, TDT, TDB, etc.
!*
!*  Author        : David Vallado                  719-573-2600    1 Mar 2001
!*
!*  Inputs          Description                    Range / Units
!*    JD          - Julian Date                    days from 4713 BC
!*
!*  OutPuts       :
!*    Year        - Year                           1900 .. 2100
!*    Mon         - Month                          1 .. 12
!*    Day         - Day                            1 .. 28,29,30,31
!*    Hr          - Hour                           0 .. 23
!*    Min         - Minute                         0 .. 59
!*    Sec         - Second                         0.0D0 .. 59.999D0
!*
!*  Locals        :
!*    Days        - Day of year plus fractional
!*                  portion of a day               days
!*    Tu          - Julian Centuries from 0 h
!*                  Jan 0, 1900
!*    Temp        - Temporary real values
!*    LeapYrs     - Number of Leap years from 1900
!*
!*  Coupling      :
!*    DAYS2MDHMS  - Finds MD HMS given Days and Year
!*
!*  References    :
!*    Vallado       2007, 208, Alg 22, Ex 3-13
!* -----------------------------------------------------------------------------

 SUBROUTINE INVJDay     ( JD, Year,Mon,Day,Hr,Min, Sec )

          IMPLICIT NONE

        INTEGER Year, Mon, Day, Hr, Min
        REAL(r8) ::  Sec, JD
!* ----------------------------  Locals  -------------------------------
        INTEGER LeapYrs
        REAL(r8) ::  Days, Tu, Temp

        ! --------------------  Implementation   ----------------------
        ! ---------------- Find Year and Days of the year -------------
        Temp   = JD-2415019.5D0
        Tu     = Temp / 365.25D0
        Year   = 1900 + IDINT( Tu )
        LeapYrs= IDINT( ( Year-1901 )*0.25D0 )
        Days   = Temp - ((Year-1900)*365.0D0 + LeapYrs )

        ! -------------- Check for case of beginning of a year --------
        IF ( Days .lt. 1.0D0 ) THEN
            Year   = Year - 1
            LeapYrs= IDINT( ( Year-1901 )*0.25D0 )
            Days   = Temp - ((Year-1900)*365.0D0 + LeapYrs )
          ENDIF

        ! ------------------ Find remaing data  -----------------------
        CALL DAYS2MDHMS( Year,Days, Mon,Day,Hr,Min,Sec )

      RETURN
END SUBROUTINE INVJDay









! ............................................................................

SUBROUTINE jd2sse      ( jd, Direction, sse )
       IMPLICIT NONE
       REAL(r8), INTENT(INOUT) ::  sse, jd
       CHARACTER*4 Direction

        ! --------------------  Implementation   ----------------------
        IF ( Direction.eq.'FROM' ) THEN
            jd = 2451544.5D0 + sse/86400.0D0
          ELSE
            sse = (jd - 2451544.5D0) * 86400.0D0
          ENDIF
      RETURN
END SUBROUTINE jd2sse


! ............................................................................


     SUBROUTINE DAYS2MDHMS  ( Year,Days,  Mon,Day,Hr,Minute,Sec )
        IMPLICIT NONE
        REAL(r8), intent(inout) :: Days, Sec
        INTEGER, intent(inout)  :: Year, Mon, Day, Hr, Minute
!       ----------------------------  Locals  -------------------------------
        INTEGER  ::  IntTemp,i,DayofYr, LMonth(12)
        REAL(r8) ::  Temp

        ! --------------------  Implementation   ----------------------
        ! -------------- Set up array of days in month  ---------------
        DO i = 1,12
           LMonth(i) = 31
        ENDDO
        LMonth( 2) = 28
        LMonth( 4) = 30
        LMonth( 6) = 30
        LMonth( 9) = 30
        LMonth(11) = 30

        DayofYr= IDINT(Days )

        ! ---------------- Find month and Day of month ----------------
        IF (MOD(Year,4).eq.0) THEN
            LMonth(2)= 29
          ENDIF
        i= 1
        IntTemp= 0
        DO WHILE ( (DayofYr.gt.IntTemp + LMonth(i) ) .and. ( i.lt.12 ))
            IntTemp= IntTemp + LMonth(i)
            i= i+1
          ENDDO
        Mon= i
        Day= DayofYr - IntTemp

        ! ---------------- Find hours Minutes and seconds -------------
        Temp= (Days - DayofYr )*24.0D0
        Hr  = IDINT( Temp )
        Temp= (Temp-Hr) * 60.0D0
        Minute = IDINT( Temp )
        Sec = (Temp-Minute) * 60.0D0

        ! ---- Check for roundoff errors
!        IF (Sec .ge. 59.9999D0) THEN
!            Sec = 0.0D0
!            Min = Min + 1
!            IF (Min .gt. 59) THEN
!                Min = 0
!                Hr = Hr + 1
!                IF (Hr .gt. 23) THEN
!                    Hr = 0
!                    Day = Day + 1
!                  ENDIF
!              ENDIF
!          ENDIF
      RETURN
      END Subroutine DAYS2MDHMS ! end days2mdhms
! +++++++++++++++++++++++++++++++++++++++++++



subroutine built_vecsize(sim_start, sim_end, time_day, say)
       implicit none
       real(r8), intent(in) :: sim_start, sim_end, time_day
       integer          :: say
       real(r8)             :: temp1

       say = 1
       temp1 = sim_start

       !print*, "inside simend-----", sim_end, temp1, time_day 
       !WRITE(*,"(1F10.2)")  sim_end

       do while (sim_end.GE.temp1)
         temp1 = temp1 + time_day
         if (temp1.LE.sim_end) then
          say = say + 1 
           end if
            enddo
end subroutine built_vecsize

subroutine built_vec(sim_start, sim_end, time_day, t1)
       implicit none
       real(r8), intent(in) :: sim_start, sim_end, time_day
       integer          :: say
       real(r8)             :: temp1
       real(r8),  dimension(:) :: t1 

       say = 1         
       temp1 = sim_start ! garante that it lesim start !time
       t1(1) = sim_start
       do while (sim_end.GE.temp1)
         temp1 = temp1 + time_day
          say = say + 1 
          if (say>size(t1)) then
             exit 
          end if
          t1(say) = temp1
       enddo
end subroutine built_vec















! **************************************************************************
! **************************************************************************
! Swath Calc ....


   SUBROUTINE Sgp4_swath1 (slons, slats, iSat, nymd, nhms, deltat, &
                           SwathWidth, rc, wrap)

      IMPLICIT NONE

! !INPUT PARAMETERS:
      real(r8),    pointer         :: slons(:,:)      !Track longitude [degree]
      real(r8),    pointer         :: slats(:,:)      !Track latitude  [degree]
      integer, intent(in)          :: iSat            !Satellite name Aqua etc ...
      integer,  intent(in)         :: nymd(2)         !Beginning/ending date: YYYYMMDD
      integer,  intent(in)         :: nhms(2)         !Beginning/ending time: HHMMSS
      REAL(r8), INTENT(IN)         :: SwathWidth(:)   !Right, left swath [km]
      INTEGER, intent(in)          :: deltat          ! Time step [secs]
      LOGICAL, intent(in), optional :: wrap       ! if true then wrap to -180 to 180

      !LOGICAL                      :: wrapon       ! if true then wrap to -180 to 180


! !OUTPUT PARAMETERS:

      integer, intent(out)        :: rc

!     Other parameters!

      real(r8),    pointer  :: lons(:)      !Ground Track longitude [degree]
      real(r8),    pointer  :: lats(:)      !Ground Track latitude  [degree]
      REAL(r8),    DIMENSION(:,:,:), ALLOCATABLE  :: latlonshift
      real(r8)              :: dist2creat, temp1
      integer, parameter    :: iii = 3      !this is used for the number of swaths
                                            !here is 3 indicating mid left right
      integer               :: Sat          ! Satellite number
      integer               :: ierr1

      logical               :: wrapon

      if ( present(wrap) ) then
           wrapon = wrap
      else                 
           wrapon = .true.
      end if

      rc = 0 ! Initiate error code to 0

      Call Sgp4_track(lons, lats, iSat, nymd, nhms, deltat, rc)

      if ( rc /= 0 ) return
      Sat = iSat ! didn't want to change the code...
      !CALL satname2int(Sat_name, Sat, rc)  ! name to sat number rc will return 99
      !if ( rc /= 0 ) return

!     Check if the Swath width is given correctly
      if ( SwathWidth(1)<0.AND.SwathWidth(2) < 0 ) then
         rc = 90
         return
      else if (SwathWidth(1)<0.AND.SwathWidth(2) >= 0) then
         temp1 = SwathWidth(2) + SwathWidth(1)
         if (temp1<=0) then
            rc = 90
            return
         end if
      else if (SwathWidth(2)<0.AND.SwathWidth(1) >= 0) then
         temp1 = SwathWidth(1) + SwathWidth(2)
         if (temp1<=0) then
            rc = 90
            return
         end if
      end if

!     2- Find sweep points
      if (allocated(latlonshift)) deallocate(latlonshift) ! this is main
      ALLOCATE(latlonshift(iii, SIZE(lats) ,2), stat = ierr1 ) ! either is ok size of lat or lon
      if ( ierr1 /= 0 ) then
          rc = 3 !
          return
      endif
      latlonshift = 0
      !CALL find_sweeppoints(iii,lat_l(1:las),lon_l(1:los), real(swath(1),r8),  &
      !                  real(swath(2),r8), latlonshift)
      CALL find_sweeppoints(iii, lats, lons, -SwathWidth(1), SwathWidth(2), wrapon, latlonshift)
      ALLOCATE(slats(iii, SIZE(lats)), stat = ierr1 ) ! either is ok size of lat or lon
      ALLOCATE(slons(iii, SIZE(lons)), stat = ierr1 ) ! either is ok size of lat or lon

      slats(1:iii, 1:SIZE(lats)) = latlonshift(1:iii, 1:SIZE(lats) ,1)
      slons(1:iii, 1:SIZE(lons)) = latlonshift(1:iii, 1:SIZE(lons) ,2)
      slats(2,1:SIZE(lats))  = lats(1:SIZE(lats))  ! slats(1:3, :) 1- right 2- original ground track 3-left
      slons(2, 1:SIZE(lons)) = lons(1:SIZE(lons))

      DEALLOCATE(latlonshift, stat = ierr1)
      if (ierr1 /= 0) then
        rc = 3 !
        return
      endif       

  END SUBROUTINE Sgp4_swath1



   SUBROUTINE Sgp4_swath2 (slons, slats, Sat_name, nymd, nhms, deltat, &
                           SwathWidth, rc, wrap)
! SUBROUTINE Sgp4_swath (slons, slats, Sat_name, nymd, nhms, deltat, SwathWidth, wrapon, rc)

      IMPLICIT NONE
! !INPUT PARAMETERS:
      real(r8),    pointer         :: slons(:,:)      !Track longitude [degree]
      real(r8),    pointer         :: slats(:,:)      !Track latitude  [degree]
      character(len=*),intent(in)  :: Sat_name        !Satellite name Aqua etc ...
      integer,  intent(in)         :: nymd(2)         !Beginning/ending date: YYYYMMDD
      integer,  intent(in)         :: nhms(2)         !Beginning/ending time: HHMMSS
      REAL(r8), INTENT(IN)         :: SwathWidth(:)   !Right, left swath [km]
      INTEGER,  intent(in)          :: deltat          ! Time step [secs]
      LOGICAL,  intent(in), optional:: wrap       ! if true then wrap to -180 to 180

      !LOGICAL                      :: wrapon       ! if true then wrap to -180 to 180


! !OUTPUT PARAMETERS:
      integer, intent(out)        :: rc

!     Other parameters!

      real(r8),    pointer  :: lons(:)      !Ground Track longitude [degree]
      real(r8),    pointer  :: lats(:)      !Ground Track latitude  [degree]
      REAL(r8),    DIMENSION(:,:,:), ALLOCATABLE  :: latlonshift
      real(r8)              :: dist2creat, temp1
      integer, parameter    :: iii = 3      !this is used for the number of swaths
                                            !here is 3 indicating mid left right
      integer               :: Sat          ! Satellite number
      integer               :: ierr1

      logical               :: wrapon
      logical               :: flagfile

      if ( present(wrap) ) then
           wrapon = wrap
      else                 
           wrapon = .true.
      end if

      rc = 0 ! Initiate error code to 0

      !CALL  isfile(Sat_name, flagfile)
      !CALL name2p(Sat_name, p, rc, flagfile)
      !if (rc>0) return
      !Call Sgp4_track0(lons, lats, nymd, nhms, deltat, p, rc)
      !if (rc>0) return

       Call Sgp4_track(lons, lats, Sat_name, nymd, nhms, deltat, rc)

      !CALL Sgp4_track(lons, lats, Sat_name, nymd, nhms, deltat, rc)


      !print*, " inside the function "
! print*, deltat, nymd, nhms		
!        print*, Sat_name
!        print*, lons
!        print*, " "
!        print*, lats
!        print*, size(lats), size(lons)
!print*, " "
!print*, " "

      if ( rc /= 0 ) return
      !CALL satname2int(Sat_name, Sat, rc)  ! name to sat number rc will return 99
      !if ( rc /= 0 ) return

!     Check if the Swath width is given correctly
      if ( SwathWidth(1)<0.AND.SwathWidth(2) < 0 ) then
         rc = 90
         return
      else if (SwathWidth(1)<0.AND.SwathWidth(2) >= 0) then
         temp1 = SwathWidth(2) + SwathWidth(1)
         if (temp1<=0) then
            rc = 90
            return
         end if
      else if (SwathWidth(2)<0.AND.SwathWidth(1) >= 0) then
         temp1 = SwathWidth(1) + SwathWidth(2)
         if (temp1<=0) then
            rc = 90
            return
         end if
      end if

!     2- Find sweep points
      if (allocated(latlonshift)) deallocate(latlonshift) ! this is main
      ALLOCATE(latlonshift(iii, SIZE(lats) ,2), stat = ierr1 ) ! either is ok size of lat or lon
      if ( ierr1 /= 0 ) then
          rc = 3 !
          return
      endif
      latlonshift = 0
      !CALL find_sweeppoints(iii,lat_l(1:las),lon_l(1:los), real(swath(1),r8),  &
      !                  real(swath(2),r8), latlonshift)
      CALL find_sweeppoints(iii, lats, lons, -SwathWidth(1), SwathWidth(2), wrapon, latlonshift)
      ALLOCATE(slats(iii, SIZE(lats)), stat = ierr1 ) ! either is ok size of lat or lon
      ALLOCATE(slons(iii, SIZE(lons)), stat = ierr1 ) ! either is ok size of lat or lon

      slats(1:iii, 1:SIZE(lats)) = latlonshift(1:iii, 1:SIZE(lats) ,1)
      slons(1:iii, 1:SIZE(lons)) = latlonshift(1:iii, 1:SIZE(lons) ,2)
      slats(2,1:SIZE(lats))  = lats(1:SIZE(lats))  ! slats(1:3, :) 1- right 2- original ground track 3-left
      slons(2, 1:SIZE(lons)) = lons(1:SIZE(lons))

      DEALLOCATE(latlonshift, stat = ierr1)
      if (ierr1 /= 0) then
        rc = 3 !
        return
      endif       

  END SUBROUTINE Sgp4_swath2


SUBROUTINE find_sweeppoints(iii, latwayp, longwayp, l, r, wrapon, latlonshift)
       IMPLICIT NONE
! !ARGUMENTS:
!
       REAL(r8), DIMENSION(:),     INTENT(IN)   :: latwayp, longwayp
       REAL(r8), INTENT(IN)     :: l, r  ! left right swap space
       INTEGER, INTENT(IN)      :: iii
       REAL(r8), DIMENSION(:,:,:), INTENT(INOUT):: latlonshift
       LOGICAL, intent(in)      :: wrapon       ! if true then wrap to -180 to 180
 
       REAL(r8), DIMENSION(:), ALLOCATABLE :: myvec, myvec_deg
       REAL(r8), DIMENSION(:), ALLOCATABLE :: latshift1, lonshift1 
       REAL(r8)                            :: az, latout1, lonout1
       INTEGER                         :: say, i, count1
!DESCRIPTION:
!      Swap points are the points that gives the satellite instrument view 
!      points that is different than the original track. For now calculation
!      is basic. r, and l gives how far sweep points will go vertically from the orbit points. 
!
! !SEE ALSO:
!  get_recon, get_azimuth
!
       ALLOCATE(myvec(iii), myvec_deg(iii))
       if (r>l) then 
               CALL linspace(l,r,iii,myvec)
       else
               CALL linspace(r,l,iii,myvec)
       end if
       do i=1, size(myvec)
          myvec_deg(i) =  ckm2deg * myvec(i)
       end do
      DO say = 1,size(myvec)
          ALLOCATE(latshift1(1:size(longwayp)),lonshift1(1:size(longwayp)))
          count1 = 1
          DO i=1,size(longwayp)-1
           az = get_azimuth(latwayp(i),longwayp(i),latwayp(i+1),longwayp(i+1), wrapon)
           CALL get_reckon(latwayp(i),longwayp(i), az+90.0,      &
                          myvec_deg(say), latout1, lonout1, wrapon)
           latshift1(count1) = latout1
           lonshift1(count1) = lonout1
           count1 = count1 + 1
          END DO
          ! i increased by 1
          CALL get_reckon(latwayp(i),longwayp(i), az+90.0,  &
                  myvec_deg(say), latout1, lonout1, wrapon)
          latshift1(count1) = latout1
          lonshift1(count1) = lonout1
          latlonshift(say,:,1) = latshift1(:)
          latlonshift(say,:,2) = lonshift1(:)
          DEALLOCATE(latshift1, lonshift1)
      END DO 
      DEALLOCATE(myvec, myvec_deg)
END SUBROUTINE find_sweeppoints

REAL(r8) FUNCTION get_azimuth(lt1, ln1, lt2, ln2, wrapon) ! need to be
       ! Computes great circle distance and azimuth
       ! Lat - lon in degrees
       IMPLICIT NONE
       REAL(r8) :: lat1, lat2, lon1, lon2
       REAL(r8) :: lt1, lt2, ln1, ln2
       REAL(r8) :: az, epsilone, saz
       LOGICAL, intent(in) :: wrapon       ! if true then wrap to -180 to 180

       lat1 = deg2rad * lt1
       lon1 = deg2rad * ln1
       lat2 = deg2rad * lt2
       lon2 = deg2rad * ln2
       ! Inputs LAT1, LON1, LAT2, LON2 are in units of radians.
        az = atan2(cos(lat2) * sin(lon2-lon1), cos(lat1) * sin(lat2)-  &
             sin(lat1) * cos(lat2) * cos(lon2-lon1))
       ! Azimuths are undefined at the poles, so we choose a convention: zero at
       ! the north pole and pi at the south pole.
       if (lat1 <= -pi/2.0) az = 0.0
       if (lat2 >=  pi/2.0) az = 0.0
       if (lat2 <=  -pi/2.0) az = pi
       if (lat1 >=   pi/2.0) az = pi
       epsilone = 1.74E-8


       if (wrapon) then
           if ( az>-epsilone .AND. az < epsilone) then
               saz = 0
           else
               saz = az/abs(az)
           end if
           az = pi*((abs(az)/pi) - 2*ceiling(((abs(az)/pi)-1)/2)) * saz
       end if

       if ( az < -epsilone) then ! change to 0 to 2pi
           az = az + 2*pi
       end if
       !  Reset near-zero points
       if (az<0.0) az=0
       get_azimuth = Rad2Deg * az
END FUNCTION get_azimuth


SUBROUTINE get_reckon(phi1, lambda1, az1, rng1, phi, lambda, wrapon)
       ! phi0 - > lat, lambda0 -> lon
       ! calculates a position (LATOUT, LONOUT) at a given range RNG and azimuth
       ! AZ along a great circle from a starting point defined by LAT and LON.
       ! LAT and LON are in degrees.  The range is in degrees of arc length on a
       ! sphere.  The input azimuth is in degrees, measured clockwise from due
       ! north. Translated from MatLab 
       IMPLICIT NONE

       REAL(r8), INTENT(IN) :: phi1, lambda1, az1, rng1
       REAL(r8), INTENT(INOUT) :: phi, lambda
       REAL(r8)                :: phi0, lambda0 
       REAL(r8) :: epsilone, az, saz, rng
       LOGICAL, intent(in) :: wrapon       ! if true then wrap to -180 to 180

       epsilone = 10*1.74E-8
       phi0 = deg2rad * phi1
       lambda0 = deg2rad * lambda1
       az = deg2rad * az1
       rng = deg2rad * rng1
       if (phi0 >= pi/2-epsilone) az = pi   ! starting at north pole
       if (phi0 <= epsilone-pi/2) az = 0    ! starting at south pole
       ! Calculate coordinates of great circle end point using spherical trig.
       phi = asin(sin(phi0)*cos(rng) + cos(phi0)*sin(rng)*cos(az))
       lambda = lambda0 + atan2( sin(rng)*sin(az),            &
                cos(phi0)*cos(rng) - sin(phi0)*sin(rng)*cos(az) )


       if (wrapon) then 
           if ( lambda>-epsilone .AND. lambda < epsilone) then
               saz = 0
           else
               saz = lambda/abs(lambda)
           end if
           lambda = pi*((abs(lambda)/pi) - 2*ceiling(((abs(lambda)/pi)-1)/2)) * saz
       end if

       lambda = rad2deg * lambda
       phi    = rad2deg * phi
END SUBROUTINE get_reckon

SUBROUTINE linspace1(d1,d2,n,y)
       ! from d1 to d2, n by n -> y
         IMPLICIT   NONE
        REAL(r8), INTENT(IN)   :: d1, d2, n
        INTEGER            :: n2, i
        REAL(r8), DIMENSION(:), INTENT(INOUT) :: y
        n2 = n
        DO i = 0, n2-2
          y(i+1) = i 
        END DO
        y(1:n2-1) = (d1+y(1:n2-1)*(d2-d1)/(floor(n)-1))   ! d2);
        y(n2) = d2
END SUBROUTINE linspace1

SUBROUTINE linspace2(d1,d2,n,y)
        IMPLICIT   NONE
        REAL(r8), INTENT(IN)   :: d1, d2
        INTEGER, INTENT(IN)   :: n
        INTEGER            :: n2, i
        REAL(r8), DIMENSION(:), INTENT(INOUT) :: y

        n2 = n
        DO i = 0, n2-2
          y(i+1) = i 
        END DO
        y(1:n2-1) = (d1+y(1:n2-1)*(d2-d1)/(n-1))   ! d2);
        y(n2) = d2
END SUBROUTINE linspace2


REAL(r8) FUNCTION deg2km(angle)
       IMPLICIT NONE    
       REAL(r8) angle

       deg2km = earth_radius * deg2rad * angle 
END FUNCTION deg2km

REAL(r8) FUNCTION km2deg(r)
       IMPLICIT NONE    
       REAL(r8) :: r, rad

       rad = r/earth_radius
       km2deg = rad2deg * rad
END FUNCTION km2deg

! REAL(r8) FUNCTION deg2rad(angle)
!        IMPLICIT NONE
! 
!        REAL(r8), INTENT(IN) :: angle
!        deg2rad = (pi/180.0) * angle
! END FUNCTION deg2rad
! 
! REAL(r8) FUNCTION rad2deg(rad)
!        IMPLICIT NONE
!        REAL(r8), INTENT(IN) :: rad
! 
!        rad2deg = (180.0/pi) * rad
! END FUNCTION rad2deg


! ******************************************
! ******************************************
! ******** Functions from book


















! ************************************

!     ----------------------------------------------------------------
!
!                               sgp4io.for
!
!    this file contains a function to read two line element sets. while 
!    not formerly part of the sgp4 mathematical theory, it is 
!    required for practical implemenation.
!
!                            companion code for
!               fundamentals of astrodynamics and applications
!                                    2007
!                              by david vallado
!
!       (w) 719-573-2600, email dvallado@agi.com
!
!  current :
!             3 jul 08  david vallado
!                        add switch for afspc compatibility and improved operation
!  changes :
!              14 mar 07  david vallado
!                           misc fixes and manual operation
!              15 aug 06  david vallado
!                           original baseline
!       ----------------------------------------------------------------


! -----------------------------------------------------------------------------
!
!                           SUBROUTINE TWOLINE2RVSGP4
!
!  this function converts the two line element set character string data to
!    variables and initializes the sgp4 variables. several intermediate varaibles
!    and quantities are determined. note that the result is a "structure" so multiple
!    satellites can be processed simultaneously without having to reinitialize. the
!    verification mode is an important option that permits quick checks of any
!    changes to the underlying technical theory. this option works using a
!    modified tle file in which the start, stop, and delta time values are
!    included at the end of the second line of data. this only works with the
!    verification mode. the catalog mode simply propagates from -1440 to 1440 min
!    from epoch and is useful when performing entire catalog runs.
!
!  author        : david vallado                  719-573-2600    1 mar 2001
!
!  inputs        :
!    Numsats     - Number of satellites processed. It also becomes the record
!                  number for each satellite
!    typerun     - type of run                    verification 'V', catalog 'C', 
!                                                 manual 'M'
!    typeinput   - type of manual input           mfe 'M', epoch 'E', dayofyr 'D'
!    whichconst  - which set of constants to use  72, 84
!    opsmode   - type of manual input           afspc 'a', imporved 'i'
!
!  outputs       :
!    Code        - EOF indicator. Code = 999 when EOF reached
!    startmfe    - starttime of simulation,       min from epoch
!    stopmfe     - stoptime of simulation,        min from epoch
!    deltamin    - time step                      min
!
!  coupling      :
!    days2mdhms  - conversion of days to month, day, hour, minute, second
!    jday        - convert day month year hour minute second into julian date
!    sgp4init    - initialize the sgp4 variables
!
!  Files         :
!    Unit 10     - test.elm        input 2-line element set file
!    Unit 11     - test.bak        output file
!    Unit 15     - sgp4rec.bak     temporary file of record for 2 line element sets
!
!  references    :
!    norad spacetrack report #3
!    vallado, crawford, hujsak, kelso  2006
!------------------------------------------------------------------------------

! SUBROUTINE TwoLine2RVSGP4 ( NumSats, Typerun, typeinput, whichconst,  & 
!                                   startmfe, stopmfe, deltamin, Code,  &
!                                   startyear,  startmon,  startday, starthr, startmin, startsec, &
!                                   stopyear,  stopmon,    stopday, stophr, stopmin,stopsec)

!SUBROUTINE TwoLine2RVSGP4 ( InFileName, NumSats, Typerun, typeinput, whichconst,  & 
!                                  startmfe, stopmfe, deltamin, Code,  &
!                                  startyear,  startmon,  startday, starthr, startmin, startsec, &
!                                  stopyear,  stopmon,    stopday, stophr, stopmin,stopsec, p, rc)

SUBROUTINE TwoLine2RVSGP4 ( NumSats, Typerun, typeinput, whichconst,  & 
                                  startmfe, stopmfe, deltamin, Code,  &
                                  startyear,  startmon,  startday, starthr, startmin, startsec, &
                                  stopyear,  stopmon,    stopday, stophr, stopmin,stopsec, p, rc)

          IMPLICIT NONE

       !Character(len=*), intent(in)  :: InFileName
       integer, intent(out)          :: rc ! error code

!  !Other parameters!
       type (TLE)                 :: p


        Character :: Typerun, typeinput
        Integer   :: Code, NumSats, whichconst
        REAL(r8)  :: startmfe, stopmfe, deltamin

        INTEGER, INTENT(in)   :: startyear, startmon, startday, starthr, startmin
        INTEGER, INTENT(in)   :: stopyear, stopmon, stopday, stophr, stopmin 
        REAL(r8),INTENT(in)   :: startsec, stopsec
! ----------------------------  Locals  -------------------------------
        REAL(r8)  :: J2, mu1, RadiusEarthKm,VKmPerSec, xke, tumin
        REAL(r8)  :: BC,EPDay, sec, xpdotp, j3, j4, j3oj2 
        REAL(r8)  ::  startdayofyr, stopdayofyr, jdstart, jdstop
        !INTEGER   :: startyear, stopyear, startmon, stopmon, startday
        !INTEGER   :: stopday, starthr, stophr, startmin, stopmin 
        INTEGER   :: Yr,Mon,Day,Hr,Minute,  ICrdno,nexp,bexp, error
        CHARACTER :: Show
        Character(len=130) :: LongStr1,LongStr2

        rc = 0
! 
!         ! --------------------  Implementation   ----------------------
        xpdotp        =  1440.0D0 / (2.0D0 * pi) ! 229.1831180523293

        CALL getgravconst( whichconst, tumin, mu1, radiusearthkm, xke, j2, j3, j4, j3oj2 );
        VKmPerSec     =  RadiusEarthKm * xke / 60.0D0
! 
! c        make sure the main program opens this file, otherwise do so here
! c        ! store results in a temporary file of record
! c        OPEN(15,FILE='Sgp4Rec.bak',ACCESS='DIRECT', FORM='UNFORMATTED',
! c     &       RECL=1000,STATUS='UNKNOWN')
! 
!* ----------------- READ THE TLE (LINE OF ELEMENT SET) ----------------

       Code = 0

       ICRDNO  = p%TICRDNO
       SatNum  = p%TSatNum
       SatName = p%TSatName
       EpochYr = p%TEpochYr
       EpDay   = p%TEpDay
       NDot    = p%TNDot
       NDDot   = p%TNDDot
       nexp    = p%Tnexp
       BStar   = p%TBStar
       bexp    = p%Tbexp
       EPHTYP  = p%TEPHTYP
       ELNO    = p%TELNO

       ICRDNO = p%TInclo
       Inclo  = p%TInclo
       nodeo  = p%Tnodeo
       Ecco   = p%TEcco
       Argpo  = p%TArgpo
       Mo     = p%TMo
       No     = p%TNo
       REVI   = p%TREVI

       NDDot  = NDDot * 10.0D0**Nexp
       NDot   = NDot / (XPDOTP*1440)
       NDDot  = NDDot / (XPDOTP*1440*1440)
       BStar  = BStar * 10.0D0**Bexp
       
       No     = No / XPDOTP
       a      = (No*TUMin)**(-2.0D0/3.0D0)
       Inclo  = Inclo  * Deg2Rad
       nodeo  = nodeo * Deg2Rad
       Argpo  = Argpo * Deg2Rad
       Mo     = Mo   * Deg2Rad
                                                                        
       IF (DABS(Ecco-1.0D0) .gt. 0.000001D0) THEN
           Altp= (a*(1.0D0-Ecco))-1.0D0
           Alta= (a*(1.0D0+Ecco))-1.0D0
       ELSE
          Alta= 999999.9D0
          Altp= 2.0D0* (4.0D0/(No*No)**(1.0D0/3.0D0))
       ENDIF

        ! ---- Ballistic Coefficient ----
       IF (DABS(BStar) .gt. 0.00000001D0) THEN
          BC= 1.0D0/(12.741621D0*BStar)
       ELSE
          BC= 1.111111111111111D0
       ENDIF

        ! ----------------------------------------------------------------
        ! find sgp4epoch time of element set
        ! remember that sgp4 uses units of days from 0 jan 1950 (sgp4epoch)
        ! and minutes from the epoch (time)
        ! ----------------------------------------------------------------

        ! Temporary year fix
        IF (EpochYr.lt.57) THEN
            Yr = EpochYr + 2000
        ELSE
            Yr = EpochYr + 1900
        ENDIF

        CALL Days2MDHMS( Yr,EpDay, Mon,Day,Hr,Minute,Sec )
        CALL JDAY ( Yr,Mon,Day,Hr,Minute,Sec,  JDSatEpoch )

        if (typeinput .eq. 'E') then
           !write(*,*) 'input start y m d h m s '
           !read(*,*) startyear,startmon,startday,starthr,startmin, startsec
           CALL jday( startyear,startmon,startday,starthr,startmin, startsec, jdstart)
           !read(*,*) stopyear,stopmon,stopday,stophr,stopmin, stopsec
           CALL jday( stopyear,stopmon,stopday,stophr,stopmin, stopsec, jdstop )
           
           startmfe = (jdstart - jdsatepoch)*1440.0D0
           stopmfe  = (jdstop - jdsatepoch)*1440.0D0
           
           !write(*,*)'input time step in minutes '
           !read(*,*) deltamin
           
        ENDIF

        ! -------- perform complete catalog evaluation, -+ 1 day ------- 
        if (typerun .eq. 'C') THEN
            startmfe = -1440.0D0
            stopmfe  =  1440.0D0
            deltamin =    10.0D0
        ENDIF

!* ------------------- MAKE INITIAL PREDICTION AT EPOCH ----------------
        ! 2433281.5 - 2400000.5 = 33281.0, thus time from 1950
        CALL SGP4Init( whichconst,SatNum,BStar, Ecco, JDSatEpoch-2433281.5D0, &
                       Argpo,Inclo,Mo,No, nodeo, Error )

        ! ---- Fix to indicate end-of-file
        GOTO 1000
  999   Code = 999
 1000   CONTINUE

       RETURN

END SUBROUTINE TwoLine2RVSGP4

! ***********************************

! SGP4UNIT

!*   -------------------------------------------------------------------
!*
!*                               sgp4unit.for
!*
!*    this file contains the sgp4 procedures for analytical propagation
!*    of a satellite. the code was originally released in the 1980 and 1986
!*    spacetrack papers. a detailed discussion of the theory and history
!*    may be found in the 2006 aiaa paper by vallado, crawford, hujsak,
!*    and kelso.
!*
!*                            companion code for
!*               fundamentals of astrodynamics and applications
!*                                    2007
!*                              by david vallado
!*
!*       (w) 719-573-2600, email dvallado@agi.com
!*
!*    current :
!*              26 Aug 08  david vallado
!*                           fix atime for faster operation in dspace
!*                           add operationmode for afspc (a) or improved (i)
!*                           performance mode
!*    changes :
!*              16 jun 08  david vallado
!*                           update small eccentricity check
!*              16 nov 07  david vallado
!*                           misc fixes for better compliance
!*               2 apr 07  david vallado
!*                           misc fixes for constants
!*              14 aug 06  david vallado
!*                           chg lyddane choice back to strn3, constants,
!*                           separate debug and writes, misc doc
!*              26 jul 05  david vallado
!*                           fixes for paper
!*                           note that each fix is preceded by a
!*                           comment with "sgp4fix" and an explanation of
!*                           what was changed
!*              10 aug 04  david vallado
!*                           2nd printing baseline working
!*              14 may 01  david vallado
!*                           2nd edition baseline
!*                     80  norad
!*                           original baseline
!*
!*     *****************************************************************
!*  Files         :
!*    Unit 14     - sgp4test.dbg    debug output file


!* -----------------------------------------------------------------------------








!*
!*                           SUBROUTINE DPPER
!*
!*  This Subroutine provides deep space long period periodic contributions
!*    to the mean elements.  by design, these periodics are zero at epoch.
!*    this used to be dscom which included initialization, but it's really a
!*    recurring function.
!*
!*  author        : david vallado                  719-573-2600   28 jun 2005
!*
!*  inputs        :
!*    e3          -
!*    ee2         -
!*    peo         -
!*    pgho        -
!*    pho         -
!*    pinco       -
!*    plo         -
!*    se2 , se3 , Sgh2, Sgh3, Sgh4, Sh2, Sh3, Si2, Si3, Sl2, Sl3, Sl4 -
!*    t           -
!*    xh2, xh3, xi2, xi3, xl2, xl3, xl4 -
!*    zmol        -
!*    zmos        -
!*    ep          - eccentricity                           0.0 - 1.0
!*    inclo       - inclination - needed for lyddane modification
!*    nodep       - right ascension of ascending node
!*    argpp       - argument of perigee
!*    mp          - mean anomaly
!*
!*  outputs       :
!*    ep          - eccentricity                           0.0 - 1.0
!*    inclp       - inclination
!*    nodep       - right ascension of ascending node
!*    argpp       - argument of perigee
!*    mp          - mean anomaly
!*
!*  locals        :
!*    alfdp       -
!*    betdp       -
!*    cosip  , sinip  , cosop  , sinop  ,
!*    dalf        -
!*    dbet        -
!*    dls         -
!*    f2, f3      -
!*    pe          -
!*    pgh         -
!*    ph          -
!*    pinc        -
!*    pl          -
!*    sel   , ses   , sghl  , sghs  , shl   , shs   , sil   , sinzf , sis   ,
!*    sll   , sls
!*    xls         -
!*    xnoh        -
!*    zf          -
!*    zm          -
!*
!*  coupling      :
!*    none.
!*
!*  references    :
!*    hoots, roehrich, norad spacetrack report #3 1980
!*    hoots, norad spacetrack report #6 1986
!*    hoots, schumacher and glover 2004
!*    vallado, crawford, hujsak, kelso  2006
!*------------------------------------------------------------------------------

      SUBROUTINE DPPER( e3    , ee2   , peo   , pgho  , pho   , pinco ,  &
                       plo   , se2   , se3   , sgh2  , sgh3  , sgh4  ,  &
                       sh2   , sh3   , si2   , si3   , sl2   , sl3   ,  &
                       sl4   , T     , xgh2  , xgh3  , xgh4  , xh2   ,  &
                       xh3   , xi2   , xi3   , xl2   , xl3   , xl4   ,  &
                       zmol  , zmos  , inclo , init  ,                  &
                       Eccp  , Inclp , nodep, Argpp , Mp,               &
                       operationmode )
        IMPLICIT NONE
        CHARACTER Init, operationmode
        REAL(r8) ::  e3    , ee2   , peo   , pgho  , pho   , pinco , plo   , &
                se2   , se3   , sgh2  , sgh3  , sgh4  , sh2   , sh3   , &
                si2   , si3   , sl2   , sl3   , sl4   , T     , xgh2  , &
                xgh3  , xgh4  , xh2   , xh3   , xi2   , xi3   , xl2   , &
                xl3   , xl4   , zmol  , zmos  , inclo ,                 &
                Eccp  , Inclp , nodep, Argpp , Mp

!* -------------------------- Local Variables --------------------------
        REAL(r8) ::  alfdp , betdp , cosip , cosop , dalf  , dbet  , dls   , &
               f2    , f3    , pe    , pgh   , ph    , pinc  , pl    , &
               sel   , ses   , sghl  , sghs  , shl   , shs   , sil   , &
               sinip , sinop , sinzf , sis   , sll   , sls   , xls   , &
               xnoh  , zf    , zm
        REAL(r8) ::  Zel   , Zes   , Znl   , Zns

!* ----------------------------- Constants -----------------------------
        ZES  = 0.01675D0
        ZEL  = 0.05490D0
        ZNS  = 1.19459D-5
        ZNL  = 1.5835218D-4

!* ------------------- CALCULATE TIME VARYING PERIODICS ----------------
        ZM   = ZMOS + ZNS*T

        IF (Init.eq.'y') ZM = ZMOS
        ZF   = ZM + 2.0D0*ZES*DSIN(ZM)
        SINZF= DSIN(ZF)
        F2   =  0.5D0*SINZF*SINZF - 0.25D0
        F3   = -0.5D0*SINZF*DCOS(ZF)
        SES  = SE2*F2 + SE3*F3
        SIS  = SI2*F2 + SI3*F3
        SLS  = SL2*F2 + SL3*F3 + SL4*SINZF
        SGHS = SGH2*F2 + SGH3*F3 + SGH4*SINZF
        SHS  = SH2*F2 + SH3*F3
        ZM   = ZMOL + ZNL*T

        IF (Init.eq.'y') ZM = ZMOL
        ZF   = ZM + 2.0D0*ZEL*DSIN(ZM)
        SINZF= DSIN(ZF)
        F2   =  0.5D0*SINZF*SINZF - 0.25D0
        F3   = -0.5D0*SINZF*DCOS(ZF)
        SEL  = EE2*F2 + E3*F3
        SIL  = XI2*F2 + XI3*F3
        SLL  = XL2*F2 + XL3*F3 + XL4*SINZF
        SGHL = XGH2*F2 + XGH3*F3 + XGH4*SINZF
        SHL  = XH2*F2 + XH3*F3
        PE   = SES + SEL
        PINC = SIS + SIL
        PL   = SLS + SLL
        PGH  = SGHS + SGHL
        PH   = SHS + SHL

        IF (Init.eq.'n') THEN
            PE    = PE   - PEO
            PINC  = PINC - PINCO
            PL    = PL   - PLO
            PGH   = PGH  - PGHO
            PH    = PH   - PHO
            Inclp = Inclp  + PINC
            Eccp  = Eccp   + PE
            SINIP = DSIN(Inclp)
            COSIP = DCOS(Inclp)

!* ------------------------- APPLY PERIODICS DIRECTLY ------------------
!c    sgp4fix for lyddane choice
!c    strn3 used original inclination - this is technically feasible
!c    gsfc used perturbed inclination - also technically feasible
!c    probably best to readjust the 0.2 limit value and limit discontinuity
!c    0.2 rad = 11.45916 deg
!c    use next line for original strn3 approach and original inclination
!c            IF (inclo.ge.0.2D0) THEN
!c    use next line for gsfc version and perturbed inclination
            IF (Inclp.ge.0.2D0) THEN

                PH     = PH/SINIP
                PGH    = PGH - COSIP*PH
                Argpp  = Argpp + PGH
                nodep  = nodep + PH
                Mp     = Mp + PL
              ELSE

!* ----------------- APPLY PERIODICS WITH LYDDANE MODIFICATION ---------
                SINOP  = DSIN(nodep)
                COSOP  = DCOS(nodep)
                ALFDP  = SINIP*SINOP
                BETDP  = SINIP*COSOP
                DALF   =  PH*COSOP + PINC*COSIP*SINOP
                DBET   = -PH*SINOP + PINC*COSIP*COSOP
                ALFDP  = ALFDP + DALF
                BETDP  = BETDP + DBET
                nodep = DMOD(nodep,TwoPi)
!                ! sgp4fix for afspc written intrinsic functions
!                ! nodep used without a trigonometric function ahead
                IF ((nodep .LT. 0.0D0) .and. (operationmode .eq. 'a')) THEN
                    nodep = nodep + twopi
                  ENDIF
                XLS    = Mp + Argpp + COSIP*nodep
                DLS    = PL + PGH - PINC*nodep*SINIP
                XLS    = XLS + DLS
                XNOH   = nodep
                nodep  = DATAN2(ALFDP,BETDP)
                ! sgp4fix for afspc written intrinsic functions
                ! nodep used without a trigonometric function ahead
                IF ((nodep .LT. 0.0D0) .and. (operationmode .eq. 'a')) THEN
                    nodep = nodep + twopi
                  ENDIF
                IF (DABS(XNOH-nodep) .GT. PI) THEN
                    IF(nodep .lt. XNOH) THEN
                        nodep = nodep+TWOPI
                      ELSE
                        nodep = nodep-TWOPI
                      ENDIF
                  ENDIF
                Mp   = Mp + PL
                Argpp=  XLS - Mp - COSIP*nodep
              ENDIF
          ENDIF

      RETURN
      END SUBROUTINE dpper

!* -----------------------------------------------------------------------------
!*
!*                           SUBROUTINE DSCOM
!*
!*  This Subroutine provides deep space common items used by both the secular
!*    and periodics subroutines.  input is provided as shown. this routine
!*    used to be called dpper, but the functions inside weren't well organized.
!*
!*  author        : david vallado                  719-573-2600   28 jun 2005
!*
!*  inputs        :
!*    epoch       -
!*    ep          - eccentricity
!*    argpp       - argument of perigee
!*    tc          -
!*    inclp       - inclination
!*    nodep      - right ascension of ascending node
!*    np          - mean motion
!*
!*  outputs       :
!*    sinim  , cosim  , sinomm , cosomm , snodm  , cnodm
!*    day         -
!*    e3          -
!*    ee2         -
!*    em          - eccentricity
!*    emsq        - eccentricity squared
!*    gam         -
!*    peo         -
!*    pgho        -
!*    pho         -
!*    pinco       -
!*    plo         -
!*    rtemsq      -
!*    se2, se3         -
!*    sgh2, sgh3, sgh4        -
!*    sh2, sh3, si2, si3, sl2, sl3, sl4         -
!*    s1, s2, s3, s4, s5, s6, s7          -
!*    ss1, ss2, ss3, ss4, ss5, ss6, ss7, sz1, sz2, sz3         -
!*    sz11, sz12, sz13, sz21, sz22, sz23, sz31, sz32, sz33        -
!*    xgh2, xgh3, xgh4, xh2, xh3, xi2, xi3, xl2, xl3, xl4         -
!*    nm          - mean motion
!*    z1, z2, z3, z11, z12, z13, z21, z22, z23, z31, z32, z33         -
!*    zmol        -
!*    zmos        -
!*
!*  locals        :
!*    a1, a2, a3, a4, a5, a6, a7, a8, a9, a10         -
!*    betasq      -
!*    cc          -
!*    ctem, stem        -
!*    x1, x2, x3, x4, x5, x6, x7, x8          -
!*    xnodce      -
!*    xnoi        -
!*    zcosg  , zsing  , zcosgl , zsingl , zcosh  , zsinh  , zcoshl , zsinhl ,
!*    zcosi  , zsini  , zcosil , zsinil ,
!*    zx          -
!*    zy          -
!*
!*  coupling      :
!*    none.
!*
!*  references    :
!*    hoots, roehrich, norad spacetrack report #3 1980
!*    hoots, norad spacetrack report #6 1986
!*    hoots, schumacher and glover 2004
!*    vallado, crawford, hujsak, kelso  2006
!*------------------------------------------------------------------------------

SUBROUTINE DSCOM( EPOCH , Eccp  , Argpp , Tc    , Inclp , nodep,  &
                        Np    ,                                         &
                        SNODM , CNODM , SINIM , COSIM , SINOMM, COSOMM, &
                        DAY   , E3    , Ee2   , Eccm  , EMSQ  , GAM   , &
                        Peo   , Pgho  , Pho   , PInco , Plo   ,         &
                        RTemSq, Se2   , Se3   , Sgh2  , Sgh3  , Sgh4  , &
                        Sh2   , Sh3   , Si2   , Si3   , Sl2   , Sl3   , &
                        Sl4   , S1    , S2    , S3    , S4    , S5    , &
                        S6    , S7    , SS1   , SS2   , SS3   , SS4   , &
                        SS5   , SS6   , SS7   , SZ1   , SZ2   , SZ3   , &
                        SZ11  , SZ12  , SZ13  , SZ21  , SZ22  , SZ23  , &
                        SZ31  , SZ32  , SZ33  , Xgh2  , Xgh3  , Xgh4  , &
                        Xh2   , Xh3   , Xi2   , Xi3   , Xl2   , Xl3   , &
                        Xl4   , Xn    , Z1    , Z2    , Z3    , Z11   , &
                        Z12   , Z13   , Z21   , Z22   , Z23   , Z31   , &
                        Z32   , Z33   , Zmol  , Zmos )

          IMPLICIT NONE

        REAL(r8) ::  EPOCH , Eccp  , Argpp , Tc    , Inclp , nodep, Np    ,  &
                SNODM , CNODM , SINIM , COSIM , SINOMM, COSOMM, DAY   , &
                E3    , Ee2   , Eccm  , EMSQ  , GAM   , RTemSq, Se2   , &
                Peo   , Pgho  , Pho   , PInco , Plo   ,                 &
                Se3   , Sgh2  , Sgh3  , Sgh4  , Sh2   , Sh3   , Si2   , &
                Si3   , Sl2   , Sl3   , Sl4   , S1    , S2    , S3    , &
                S4    , S5    , S6    , S7    , SS1   , SS2   , SS3   , &
                SS4   , SS5   , SS6   , SS7   , SZ1   , SZ2   , SZ3   , &
                SZ11  , SZ12  , SZ13  , SZ21  , SZ22  , SZ23  , SZ31  , &
                SZ32  , SZ33  , Xgh2  , Xgh3  , Xgh4  , Xh2   , Xh3   , &
                Xi2   , Xi3   , Xl2   , Xl3   , Xl4   , Xn    , Z1    , &
                Z2    , Z3    , Z11   , Z12   , Z13   , Z21   , Z22   , &
                Z23   , Z31   , Z32   , Z33   , Zmol  , Zmos

!* -------------------------- Local Variables --------------------------
        REAL(r8) ::  c1ss  , c1L   , zcosis, zsinis, zsings, zcosgs,Zes, zel
        INTEGER  ::  LsFlg
        REAL(r8) ::  a1    , a2    , a3    , a4    , a5    , a6    , a7    , &
                a8    , a9    , a10   , betasq, cc    , ctem  , stem  , &
                x1    , x2    , x3    , x4    , x5    , x6    , x7    , &
                x8    , xnodce, xnoi  , zcosg , zcosgl, zcosh , zcoshl, &
                zcosi , zcosil, zsing , zsingl, zsinh , zsinhl, zsini , &
                zsinil, zx    , zy

!* ------------------------------ Constants ----------------------------
        ZES    =  0.01675D0
        ZEL    =  0.05490D0
        C1SS   =  2.9864797D-6
        C1L    =  4.7968065D-7
        ZSINIS =  0.39785416D0
        ZCOSIS =  0.91744867D0
        ZCOSGS =  0.1945905D0
        ZSINGS = -0.98088458D0

!* ----------------- DEEP SPACE PERIODICS INITIALIZATION ---------------
        XN     = Np
        Eccm   = Eccp
        SNODM  = DSIN(nodep)
        CNODM  = DCOS(nodep)
        SINOMM = DSIN(Argpp)
        COSOMM = DCOS(Argpp)
        SINIM  = DSIN(Inclp)
        COSIM  = DCOS(Inclp)
        EMSQ   = Eccm*Eccm
        BETASQ = 1.0D0-EMSQ
        RTEMSQ = DSQRT(BETASQ)

!* --------------------- INITIALIZE LUNAR SOLAR TERMS ------------------
        PEO    = 0.0D0
        PINCO  = 0.0D0
        PLO    = 0.0D0
        PGHO   = 0.0D0
        PHO    = 0.0D0
        DAY    = EPOCH + 18261.5D0 + TC/1440.0D0
        XNODCE = DMOD(4.5236020D0 - 9.2422029D-4*DAY,TwoPi)
        STEM   = DSIN(XNODCE)
        CTEM   = DCOS(XNODCE)
        ZCOSIL = 0.91375164D0 - 0.03568096D0*CTEM
        ZSINIL = DSQRT(1.0D0 - ZCOSIL*ZCOSIL)
        ZSINHL = 0.089683511D0*STEM / ZSINIL
        ZCOSHL = DSQRT(1.0D0 - ZSINHL*ZSINHL)
        GAM    = 5.8351514D0 + 0.0019443680D0*DAY
        ZX     = 0.39785416D0*STEM/ZSINIL
        ZY     = ZCOSHL*CTEM + 0.91744867D0*ZSINHL*STEM
        ZX     = DATAN2(ZX,ZY)
        ZX     = GAM + ZX - XNODCE
        ZCOSGL = DCOS(ZX)
        ZSINGL = DSIN(ZX)

!* ---------------------------- DO SOLAR TERMS -------------------------
        ZCOSG = ZCOSGS
        ZSING = ZSINGS
        ZCOSI = ZCOSIS
        ZSINI = ZSINIS
        ZCOSH = CNODM
        ZSINH = SNODM
        CC    = C1SS
        XNOI  = 1.0D0 / XN

        DO LSFlg = 1,2
            A1 =   ZCOSG*ZCOSH + ZSING*ZCOSI*ZSINH
            A3 =  -ZSING*ZCOSH + ZCOSG*ZCOSI*ZSINH
            A7 =  -ZCOSG*ZSINH + ZSING*ZCOSI*ZCOSH
            A8 =   ZSING*ZSINI
            A9 =   ZSING*ZSINH + ZCOSG*ZCOSI*ZCOSH
            A10=   ZCOSG*ZSINI
            A2 =   COSIM*A7 + SINIM*A8
            A4 =   COSIM*A9 + SINIM*A10
            A5 =  -SINIM*A7 + COSIM*A8
            A6 =  -SINIM*A9 + COSIM*A10

            X1 =  A1*COSOMM + A2*SINOMM
            X2 =  A3*COSOMM + A4*SINOMM
            X3 = -A1*SINOMM + A2*COSOMM
            X4 = -A3*SINOMM + A4*COSOMM
            X5 =  A5*SINOMM
            X6 =  A6*SINOMM
            X7 =  A5*COSOMM
            X8 =  A6*COSOMM

            Z31= 12.0D0*X1*X1 - 3.0D0*X3*X3
            Z32= 24.0D0*X1*X2 - 6.0D0*X3*X4
            Z33= 12.0D0*X2*X2 - 3.0D0*X4*X4
            Z1 =  3.0D0* (A1*A1 + A2*A2) + Z31*EMSQ
            Z2 =  6.0D0* (A1*A3 + A2*A4) + Z32*EMSQ
            Z3 =  3.0D0* (A3*A3 + A4*A4) + Z33*EMSQ
            Z11= -6.0D0*A1*A5 + EMSQ* (-24.0D0*X1*X7-6.0D0*X3*X5)
            Z12= -6.0D0* (A1*A6 + A3*A5) + EMSQ* ( -24.0D0*(X2*X7+X1*X8) - 6.0D0*(X3*X6+X4*X5) )
            Z13= -6.0D0*A3*A6 + EMSQ*(-24.0D0*X2*X8 - 6.0D0*X4*X6)
            Z21=  6.0D0*A2*A5 + EMSQ*(24.0D0*X1*X5-6.0D0*X3*X7)
            Z22=  6.0D0* (A4*A5 + A2*A6) + EMSQ* (  24.0D0*(X2*X5+X1*X6) - 6.0D0*(X4*X7+X3*X8) )
            Z23=  6.0D0*A4*A6 + EMSQ*(24.0D0*X2*X6 - 6.0D0*X4*X8)
            Z1 = Z1 + Z1 + BETASQ*Z31
            Z2 = Z2 + Z2 + BETASQ*Z32
            Z3 = Z3 + Z3 + BETASQ*Z33
            S3 = CC*XNOI
            S2 = -0.5D0*S3 / RTEMSQ
            S4 = S3*RTEMSQ
            S1 = -15.0D0*Eccm*S4
            S5 = X1*X3 + X2*X4
            S6 = X2*X3 + X1*X4
            S7 = X2*X4 - X1*X3

! * ------------------------------ DO LUNAR TERMS -----------------------
            IF (LSFLG.eq.1) THEN
                SS1   = S1
                SS2   = S2
                SS3   = S3
                SS4   = S4
                SS5   = S5
                SS6   = S6
                SS7   = S7
                SZ1   = Z1
                SZ2   = Z2
                SZ3   = Z3
                SZ11  = Z11
                SZ12  = Z12
                SZ13  = Z13
                SZ21  = Z21
                SZ22  = Z22
                SZ23  = Z23
                SZ31  = Z31
                SZ32  = Z32
                SZ33  = Z33
                ZCOSG = ZCOSGL
                ZSING = ZSINGL
                ZCOSI = ZCOSIL
                ZSINI = ZSINIL
                ZCOSH = ZCOSHL*CNODM+ZSINHL*SNODM
                ZSINH = SNODM*ZCOSHL-CNODM*ZSINHL
                CC    = C1L
              ENDIF
          ENDDO

        ZMOL  = DMOD( 4.7199672D0 + 0.22997150D0*DAY-GAM,TwoPi )
        ZMOS  = DMOD( 6.2565837D0 + 0.017201977D0*DAY,TwoPi )

!* ---------------------------- DO SOLAR TERMS -------------------------
        SE2 =   2.0D0*SS1*SS6
        SE3 =   2.0D0*SS1*SS7
        SI2 =   2.0D0*SS2*SZ12
        SI3 =   2.0D0*SS2*(SZ13-SZ11)
        SL2 =  -2.0D0*SS3*SZ2
        SL3 =  -2.0D0*SS3*(SZ3-SZ1)
        SL4 =  -2.0D0*SS3*(-21.0D0-9.0D0*EMSQ)*ZES
        SGH2=   2.0D0*SS4*SZ32
        SGH3=   2.0D0*SS4*(SZ33-SZ31)
        SGH4= -18.0D0*SS4*ZES
        SH2 =  -2.0D0*SS2*SZ22
        SH3 =  -2.0D0*SS2*(SZ23-SZ21)

!* ---------------------------- DO LUNAR TERMS -------------------------
        EE2 =   2.0D0*S1*S6
        E3  =   2.0D0*S1*S7
        XI2 =   2.0D0*S2*Z12
        XI3 =   2.0D0*S2*(Z13-Z11)
        XL2 =  -2.0D0*S3*Z2
        XL3 =  -2.0D0*S3*(Z3-Z1)
        XL4 =  -2.0D0*S3*(-21.0D0-9.0D0*EMSQ)*ZEL
        XGH2=   2.0D0*S4*Z32
        XGH3=   2.0D0*S4*(Z33-Z31)
        XGH4= -18.0D0*S4*ZEL
        XH2 =  -2.0D0*S2*Z22
        XH3 =  -2.0D0*S2*(Z23-Z21)

      RETURN
END SUBROUTINE dscom

!* -----------------------------------------------------------------------------
!*
!*                           SUBROUTINE DSINIT
!*
!*  This Subroutine provides Deep Space contributions to Mean Motion Dot due
!*    to geopotential resonance with half day and one day orbits.
!*
!*  Inputs        :
!*    Cosim, Sinim-
!*    Emsq        - Eccentricity squared
!*    Argpo       - Argument of Perigee
!*    S1, S2, S3, S4, S5      -
!*    Ss1, Ss2, Ss3, Ss4, Ss5 -
!*    Sz1, Sz3, Sz11, Sz13, Sz21, Sz23, Sz31, Sz33 -
!*    T           - Time
!*    Tc          -
!*    GSTo        - Greenwich sidereal time                   rad
!*    Mo          - Mean Anomaly
!*    MDot        - Mean Anomaly dot (rate)
!*    No          - Mean Motion
!*    nodeo       - right ascension of ascending node
!*    nodeDot     - right ascension of ascending node dot (rate)
!*    XPIDOT      -
!*    Z1, Z3, Z11, Z13, Z21, Z23, Z31, Z33 -
!*    Eccm        - Eccentricity
!*    Argpm       - Argument of perigee
!*    Inclm       - Inclination
!*    Mm          - Mean Anomaly
!*    Xn          - Mean Motion
!*    nodem       - right ascension of ascending node
!*
!*  Outputs       :
!*    Eccm        - Eccentricity
!*    Argpm       - Argument of perigee
!*    Inclm       - Inclination
!*    Mm          - Mean Anomaly
!*    Xn          - Mean motion
!*    nodem       - right ascension of ascending node
!*    IRez        - Resonance flags              0-none, 1-One day,  2-Half day
!*    Atime       -
!*    D2201, D2211, D3210, D3222, D4410, D4422, D5220, D5232, D5421, D5433       -
!*    Dedt        -
!*    Didt        -
!*    DMDT        -
!*    DNDT        -
!*    DNODT       -
!*    DOMDT       -
!*    Del1, Del2, Del3 -
!*    Ses  , Sghl , Sghs , Sgs  , Shl  , Shs  , Sis  , Sls
!*    THETA       -
!*    Xfact       -
!*    Xlamo       -
!*    Xli         -
!*    Xni
!*
!*  Locals        :
!*    ainv2       -
!*    aonv        -
!*    cosisq      -
!*    eoc         -
!*    f220, f221, f311, f321, f322, f330, f441, f442, f522, f523, f542, f543        -
!*    g200, g201, g211, g300, g310, g322, g410, g422, g520, g521, g532, g533        -
!*    sini2       -
!*    temp, temp1 -
!*    Theta       -
!*    xno2        -
!*
!*  Coupling      :
!*    getgravconst-
!*
!*  references    :
!*    hoots, roehrich, norad spacetrack report #3 1980
!*    hoots, norad spacetrack report #6 1986
!*    hoots, schumacher and glover 2004
!*    vallado, crawford, hujsak, kelso  2006
!*------------------------------------------------------------------------------

      SUBROUTINE DSINIT( whichconst,                                      &
                         Cosim , Emsq  , Argpo , S1    , S2    , S3    ,  &
                         S4    , S5    , Sinim , Ss1   , Ss2   , Ss3   ,  &
                        Ss4   , Ss5   , Sz1   , Sz3   , Sz11  , Sz13  ,  &
                        Sz21  , Sz23  , Sz31  , Sz33  , T     , Tc    ,  &
                        GSTo  , Mo    , MDot  , No    , nodeo ,nodeDot,  &
                        XPIDOT, Z1    , Z3    , Z11   , Z13   , Z21   ,  &
                        Z23   , Z31   , Z33   , Ecco  , EccSq ,          &
                        Eccm  , Argpm , Inclm , Mm    , Xn    , nodem,   &
                        IREZ  , Atime , D2201 , D2211 , D3210 , D3222 ,  &
                        D4410 , D4422 , D5220 , D5232 , D5421 , D5433 ,  &
                        Dedt  , Didt  , DMDT  , DNDT  , DNODT , DOMDT ,  &
                        Del1  , Del2  , Del3  , Xfact , Xlamo , Xli   ,  &
                        Xni )

          IMPLICIT NONE

        INTEGER  :: IRez, whichconst
        REAL(r8) ::   Cosim , Emsq  , Argpo , S1    , S2    , S3    , S4    , &
                S5    , Sinim , Ss1   , Ss2   , Ss3   , Ss4   , Ss5   , &
                Sz1   , Sz3   , Sz11  , Sz13  , Sz21  , Sz23  , Sz31  , &
                Sz33  , T     , Tc    , GSTo  , Mo    , MDot  , No    , &
                nodeo ,nodeDot,XPIDOT , Z1    , Z3    , Z11   , Z13   , &
                Z21   , Z23   , Z31   , Z33   , Eccm  , Argpm , Inclm , &
                Mm    , Xn    , nodem , Atime , D2201 , D2211 , D3210 , &
                D3222 , D4410 , D4422 , D5220 , D5232 , D5421 , D5433 , &
                Dedt  , Didt  , DMDT  , DNDT  , DNODT , DOMDT , Del1  , &
                Del2  , Del3  , Xfact , Xlamo , Xli   , Xni   , Ecco  , &
                Eccsq

!* -------------------------- Local Variables --------------------------
        REAL(r8) ::  ainv2 , aonv  , cosisq, eoc   , f220  , f221  , f311  ,  &
               f321  , f322  , f330  , f441  , f442  , f522  , f523  ,  &
               f542  , f543  , g200  , g201  , g211  , g300  , g310  ,  &
               g322  , g410  , g422  , g520  , g521  , g532  , g533  ,  &
               ses   , sgs   , sghl  , sghs  , shs   , shl   , sis   ,  &
               sini2 , sls   , temp  , temp1 , Theta , xno2           
        REAL(r8) ::  Q22   , Q31   , Q33   , ROOT22, ROOT44, ROOT54,          &
               RPTim , Root32, Root52, X2o3  , XKe   , Znl   ,          &
               Zns,  Emo, emsqo , tumin, mu, radiusearthkm, j2, j3, j4, &
               j3oj2

        Q22    = 1.7891679D-6
        Q31    = 2.1460748D-6
        Q33    = 2.2123015D-7
        ROOT22 = 1.7891679D-6
        ROOT44 = 7.3636953D-9
        ROOT54 = 2.1765803D-9
        RPTim  = 4.37526908801129966D-3 ! this equates to 7.29211514668855e-5 rad/sec
        Root32 = 3.7393792D-7
        Root52 = 1.1428639D-7
        X2o3   = 2.0D0 / 3.0D0
        ZNL    = 1.5835218D-4
        ZNS    = 1.19459D-5

        ! sgp4fix identify constants and allow alternate values
        CALL getgravconst( whichconst, tumin, mu, radiusearthkm, xke,j2, j3, j4, j3oj2 )

!* ------------------------ DEEP SPACE INITIALIZATION ------------------
        IREZ = 0
        IF ((XN.lt.0.0052359877D0).AND.(XN.GT.0.0034906585D0)) THEN
            IREZ = 1
          ENDIF
        IF ((XN.ge.8.26D-3).AND.(XN.LE.9.24D-3).AND.(Eccm.GE.0.5D0))THEN
            IREZ = 2
          ENDIF

!* ---------------------------- DO SOLAR TERMS -------------------------
        SES  =  SS1*ZNS*SS5
        SIS  =  SS2*ZNS*(SZ11 + SZ13)
        SLS  = -ZNS*SS3*(SZ1 + SZ3 - 14.0D0 - 6.0D0*EMSQ)
        SGHS =  SS4*ZNS*(SZ31 + SZ33 - 6.0D0)
        SHS  = -ZNS*SS2*(SZ21 + SZ23)
!c       sgp4fix for 180 deg incl
        IF ((Inclm.lt.5.2359877D-2).or.(Inclm.gt.pi-5.2359877D-2)) THEN
            SHS = 0.0D0
          ENDIF
        IF (SINIM.ne.0.0D0) THEN
            SHS = SHS/SINIM
          ENDIF
        SGS  = SGHS - COSIM*SHS

!* ----------------------------- DO LUNAR TERMS ------------------------
        DEDT = SES + S1*ZNL*S5
        DIDT = SIS + S2*ZNL*(Z11 + Z13)
        DMDT = SLS - ZNL*S3*(Z1 + Z3 - 14.0D0 - 6.0D0*EMSQ)
        SGHL = S4*ZNL*(Z31 + Z33 - 6.0D0)
        SHL  = -ZNL*S2*(Z21 + Z23)
!c       sgp4fix for 180 deg incl
        IF ((Inclm.lt.5.2359877D-2).or.(Inclm.gt.pi-5.2359877D-2)) THEN
            SHL = 0.0D0
          ENDIF
        DOMDT= SGS+SGHL
        DNODT= SHS
        IF (SINIM .ne. 0.0D0) THEN
            DOMDT = DOMDT-COSIM/SINIM*SHL
            DNODT = DNODT+SHL/SINIM
        ENDIF

!* --------------- CALCULATE DEEP SPACE RESONANCE EFFECTS --------------
        DNDT  = 0.0D0
        THETA = DMOD(GSTo + TC*RPTIM,TwoPi)
        Eccm  = Eccm + DEDT*T
        emsq  = eccm**2
        Inclm = Inclm + DIDT*T
        Argpm = Argpm + DOMDT*T
        nodem = nodem + DNODT*T
        Mm    = Mm + DMDT*T
!c   sgp4fix for negative inclinations
!c   the following if statement should be commented out
!c           IF(Inclm .lt. 0.0D0) THEN
!c             Inclm  = -Inclm
!c             Argpm  = Argpm-PI
!c             nodem = nodem+PI
!c           ENDIF

!* ------------------ Initialize the resonance terms -------------------
        IF (IREZ .ne. 0) THEN
            AONV = (XN/XKE)**X2O3

!* -------------- GEOPOTENTIAL RESONANCE FOR 12 HOUR ORBITS ------------
        IF (IREZ .eq. 2) THEN
            COSISQ = COSIM*COSIM
            emo    = Eccm
            emsqo  = emsq
            Eccm   = ecco
            emsq   = eccsq
            EOC    = Eccm*EMSQ
            G201   = -0.306D0-(Eccm-0.64D0)*0.440D0
            IF (Eccm.le.0.65D0) THEN
                G211 =   3.616D0 -  13.2470D0*Eccm +  16.2900D0*EMSQ
                G310 = -19.302D0 + 117.3900D0*Eccm - 228.4190D0*EMSQ + 156.591D0*EOC
                G322 = -18.9068D0+ 109.7927D0*Eccm - 214.6334D0*EMSQ + 146.5816D0*EOC
                G410 = -41.122D0 + 242.6940D0*Eccm - 471.0940D0*EMSQ + 313.953D0*EOC
                G422 =-146.407D0 + 841.8800D0*Eccm - 1629.014D0*EMSQ + 1083.435D0*EOC
                G520 =-532.114D0 + 3017.977D0*Eccm - 5740.032D0*EMSQ + 3708.276D0*EOC
              ELSE
                G211 =  -72.099D0 +  331.819D0*Eccm -  508.738D0*EMSQ + 266.724D0*EOC
                G310 = -346.844D0 + 1582.851D0*Eccm - 2415.925D0*EMSQ + 1246.113D0*EOC
                G322 = -342.585D0 + 1554.908D0*Eccm - 2366.899D0*EMSQ + 1215.972D0*EOC
                G410 =-1052.797D0 + 4758.686D0*Eccm - 7193.992D0*EMSQ + 3651.957D0*EOC
                G422 =-3581.690D0 + 16178.11D0*Eccm - 24462.77D0*EMSQ + 12422.52D0*EOC
                IF (Eccm.gt.0.715D0) THEN
                    G520 =-5149.66D0 + 29936.92D0*Eccm -54087.36D0*EMSQ + 31324.56D0*EOC
                  ELSE
                    G520 = 1464.74D0 -  4664.75D0*Eccm + 3763.64D0*EMSQ
                  ENDIF
              ENDIF
            IF (Eccm.lt.0.7D0) THEN
                G533 = -919.22770D0 + 4988.6100D0*Eccm-9064.7700D0*EMSQ + 5542.21D0*EOC
                G521 = -822.71072D0 + 4568.6173D0*Eccm-8491.4146D0*EMSQ + 5337.524D0*EOC
                G532 = -853.66600D0 + 4690.2500D0*Eccm-8624.7700D0*EMSQ + 5341.4D0*EOC
              ELSE
                G533 =-37995.780D0 + 161616.52D0*Eccm-229838.20D0*EMSQ + 109377.94D0*EOC
                G521 =-51752.104D0 + 218913.95D0*Eccm-309468.16D0*EMSQ + 146349.42D0*EOC
                G532 =-40023.880D0 + 170470.89D0*Eccm-242699.48D0*EMSQ + 115605.82D0*EOC
              ENDIF
            SINI2 =  SINIM*SINIM
            F220  =  0.75D0* (1.0D0+2.0D0*COSIM+COSISQ)
            F221  =  1.5D0*SINI2
            F321  =  1.875D0*SINIM * (1.0D0-2.0D0*COSIM-3.0D0*COSISQ)
            F322  = -1.875D0*SINIM * (1.0D0+2.0D0*COSIM-3.0D0*COSISQ)
            F441  = 35.0D0*SINI2*F220
            F442  = 39.3750D0*SINI2*SINI2
            F522  =  9.84375D0*SINIM * (SINI2* (1.0D0-2.0D0*COSIM-         &
                     5.0D0*COSISQ)+0.33333333D0 * (-2.0D0+4.0D0*COSIM+     &
                     6.0D0*COSISQ) )
            F523  =  SINIM * (4.92187512D0*SINI2 * (-2.0D0-4.0D0*COSIM+    &
                     10.0D0*COSISQ) + 6.56250012D0*                        &
                    (1.0D0+2.0D0*COSIM-3.0D0*COSISQ))
            F542  =  29.53125D0*SINIM * (2.0D0-8.0D0*COSIM+COSISQ*         &
                     (-12.0D0+8.0D0*COSIM+10.0D0*COSISQ) )
            F543  = 29.53125D0*SINIM * (-2.0D0-8.0D0*COSIM+COSISQ*         &
                     (12.0D0+8.0D0*COSIM-10.0D0*COSISQ) )

            XNO2   =  XN * XN
            AINV2  =  AONV * AONV
            TEMP1  =  3.0D0*XNO2*AINV2
            TEMP   =  TEMP1*ROOT22
            D2201  =  TEMP*F220*G201
            D2211  =  TEMP*F221*G211
            TEMP1  =  TEMP1*AONV
            TEMP   =  TEMP1*ROOT32
            D3210  =  TEMP*F321*G310
            D3222  =  TEMP*F322*G322
            TEMP1  =  TEMP1*AONV
            TEMP   =  2.0D0*TEMP1*ROOT44
            D4410  =  TEMP*F441*G410
            D4422  =  TEMP*F442*G422
            TEMP1  =  TEMP1*AONV
            TEMP   =  TEMP1*ROOT52
            D5220  =  TEMP*F522*G520
            D5232  =  TEMP*F523*G532
            TEMP   =  2.0D0*TEMP1*ROOT54
            D5421  =  TEMP*F542*G521
            D5433  =  TEMP*F543*G533
            XLAMO  =  DMOD(Mo+nodeo+nodeo-THETA-THETA,TwoPi)
            XFACT  = MDot + DMDT + 2.0D0 * (nodeDot+DNODT-RPTIM) - No

            Eccm = emo
            emsq = emsqo
          ENDIF

        IF (Irez .eq. 1) THEN
!* -------------------- SYNCHRONOUS RESONANCE TERMS --------------------
            G200  = 1.0D0 + EMSQ * (-2.5D0+0.8125D0*EMSQ)
            G310  = 1.0D0 + 2.0D0*EMSQ
            G300  = 1.0D0 + EMSQ * (-6.0D0+6.60937D0*EMSQ)
            F220  = 0.75D0 * (1.0D0+COSIM) * (1.0D0+COSIM)
            F311  = 0.9375D0*SINIM*SINIM*                       &
                     (1.0D0+3.0D0*COSIM) - 0.75D0*(1.0D0+COSIM)
            F330  = 1.0D0+COSIM
            F330  = 1.875D0*F330*F330*F330
            DEL1  = 3.0D0*XN*XN*AONV*AONV
            DEL2  = 2.0D0*DEL1*F220*G200*Q22
            DEL3  = 3.0D0*DEL1*F330*G300*Q33*AONV
            DEL1  = DEL1*F311*G310*Q31*AONV
            XLAMO = DMOD(Mo+nodeo+Argpo-THETA,TwoPi)
            XFACT = MDot + XPIDOT - RPTIM + DMDT + DOMDT + DNODT - No
          ENDIF

!* ---------------- FOR SGP4, INITIALIZE THE INTEGRATOR ----------------
         XLI   = XLAMO
         XNI   = No
         ATIME = 0.0D0
         XN    = No + DNDT
      ENDIF ! Ires non-zero

      RETURN
      END SUBROUTINE dsinit

!* -----------------------------------------------------------------------------
!*
!*                           SUBROUTINE DSPACE
!*
!*  This Subroutine provides deep space contributions to mean elements for
!*    perturbing third body.  these effects have been averaged over one
!*    revolution of the sun and moon.  for earth resonance effects, the
!*    effects have been averaged over no revolutions of the satellite.
!    (mean motion)
!*
!*  author        : david vallado                  719-573-2600   28 jun 2005
!*
!*  inputs        :
!*    d2201, d2211, d3210, d3222, d4410, d4422, d5220, d5232, d5421, d5433       -
!*    dedt        -
!*    del1, del2, del3  -
!*    didt        -
!*    dmdt        -
!*    dnodt       -
!*    domdt       -
!*    irez        - flag for resonance           0-none, 1-one day, 2-half day
!*    argpo       - argument of perigee
!*    argpdot     - argument of perigee dot (rate)
!*    t           - time
!*    tc          -
!*    gsto        - gst
!*    xfact       -
!*    xlamo       -
!*    no          - mean motion
!*    atime       -
!*    em          - eccentricity
!*    ft          -
!*    argpm       - argument of perigee
!*    inclm       - inclination
!*    xli         -
!*    mm          - mean anomaly
!*    xni         - mean motion
!*    nodem       - right ascension of ascending node
!*
!*  outputs       :
!*    atime       -
!*    em          - eccentricity
!*    argpm       - argument of perigee
!*    inclm       - inclination
!*    xli         -
!*    mm          - mean anomaly
!*    xni         -
!*    nodem       - right ascension of ascending node
!*    dndt        -
!*    nm          - mean motion
!*
!*  locals        :
!*    delt        -
!*    ft          -
!*    theta       -
!*    x2li        -
!*    x2omi       -
!*    xl          -
!*    xldot       -
!*    xnddt       -
!*    xndt        -
!*    xomi        -
!*
!*  coupling      :
!*    none        -
!*
!*  references    :
!*    hoots, roehrich, norad spacetrack report #3 1980
!*    hoots, norad spacetrack report #6 1986
!*    hoots, schumacher and glover 2004
!*    vallado, crawford, hujsak, kelso  2006
!*------------------------------------------------------------------------------

SUBROUTINE DSPACE( IRez  , D2201 , D2211 , D3210 , D3222 , D4410 ,        &
                        D4422 , D5220 , D5232 , D5421 , D5433 , Dedt  ,   &
                        Del1  , Del2  , Del3  , Didt  , Dmdt  , Dnodt ,   &
                        Domdt , Argpo , ArgpDot, T    , TC    , GSTo  ,   &
                        Xfact , Xlamo , No    ,                           &
                        Atime , Eccm  , Argpm , Inclm , Xli   , Mm  ,     &
                        XNi   , nodem, Dndt  , XN  )
          IMPLICIT NONE

        INTEGER  :: IRez
        REAL(r8) ::   D2201 , D2211 , D3210 , D3222 , D4410 , D4422 , D5220 , &
                 D5232 , D5421 , D5433 , Dedt  , Del1  , Del2  , Del3  ,  &
                 Didt  , Dmdt  , Dnodt , Domdt , Argpo , ArgpDot,T     ,  &
                 TC    , GSTo  , Xfact , Xlamo , No    , Atime , Eccm  ,  &
                 Argpm , Inclm , Xli   , Mm    , Xni   , nodem, Dndt  ,   &
                 XN

!* -------------------------- Local Variables --------------------------
        INTEGER  :: iretn , iret
        REAL(r8) ::   Delt  , Ft    , theta , x2li  , x2omi , xl    , xldot , &
                 xnddt , xndt  , xomi
        REAL(r8) ::   G22   , G32   , G44   , G52   , G54   , Fasx2 ,         &
                 Fasx4 , Fasx6 , RPtim , Step2 , Stepn , Stepp


!* ----------------------------- Constants -----------------------------
        FASX2 = 0.13130908D0
        FASX4 = 2.8843198D0
        FASX6 = 0.37448087D0
        G22   = 5.7686396D0
        G32   = 0.95240898D0
        G44   = 1.8014998D0
        G52   = 1.0508330D0
        G54   = 4.4108898D0
        RPTIM = 4.37526908801129966D-3
        STEPP =    720.0D0
        STEPN =   -720.0D0
        STEP2 = 259200.0D0

!* --------------- CALCULATE DEEP SPACE RESONANCE EFFECTS --------------
        DNDT  = 0.0D0
        THETA = DMOD(GSTo + TC*RPTIM,TwoPi)
        Eccm  = Eccm + DEDT*T

        Inclm = Inclm + DIDT*T
        Argpm = Argpm + DOMDT*T
        nodem = nodem + DNODT*T
        Mm    = Mm + DMDT*T

!c   sgp4fix for negative inclinations
!c   the following if statement should be commented out
!c        IF(Inclm .lt. 0.0D0) THEN
!c            Inclm  = -Inclm
!c            Argpm  = Argpm-PI
!c            nodem = nodem+PI
!c          ENDIF

!c   sgp4fix for propagator problems
!c   the following integration works for negative time steps and periods
!c   the specific changes are unknown because the original code was so convoluted
!c      sgp4fix take out atime = 0.0 and fix for faster operation
        Ft    = 0.0D0      ! Just in case - should be set in loops if used.

        IF (IREZ .ne. 0) THEN
!* ----- UPDATE RESONANCES : NUMERICAL (EULER-MACLAURIN) INTEGRATION ---
!* ---------------------------- EPOCH RESTART --------------------------
!         ! sgp4fix streamline check
         IF ((atime .eq. 0.0D0) .or. (t * atime .le. 0.0D0) .or.   & 
             (dabs(t) .lt. dabs(atime)) ) THEN
               atime  = 0.0D0
               xni    = no
               xli    = xlamo
            ENDIF
           ! sgp4fix move check outside loop
           IF (t .gt. 0.0D0) THEN
               delt = stepp
             else
               delt = stepn
             ENDIF

            iretn = 381 ! added for do loop
            iret  =   0 ! added for loop
            DO WHILE (IRetn.eq.381)

!* --------------------------- DOT TERMS CALCULATED --------------------
!* ------------------- NEAR - SYNCHRONOUS RESONANCE TERMS --------------
            IF (IREZ .ne. 2) THEN
                XNDT  = DEL1*DSIN(XLI-FASX2) + DEL2*DSIN(2.0D0*(XLI-FASX4)) +   &
                        DEL3*DSIN(3.0D0*(XLI-FASX6))
                XLDOT = XNI + XFACT
                XNDDT = DEL1*DCOS(XLI-FASX2) +                                  &
                  2.0D0*DEL2*DCOS(2.0D0*(XLI-FASX4)) +                          &
                  3.0D0*DEL3*DCOS(3.0D0*(XLI-FASX6))
                XNDDT = XNDDT*XLDOT
              ELSE

!* --------------------- NEAR - HALF-DAY RESONANCE TERMS ---------------
                XOMI = Argpo + ArgpDot*ATIME
                X2OMI= XOMI + XOMI
                X2LI = XLI + XLI
                XNDT = D2201*DSIN(X2OMI+XLI-G22) + D2211*DSIN(XLI-G22) +       &
                      D3210*DSIN( XOMI+XLI-G32) +                             &
                      D3222*DSIN(-XOMI+XLI-G32) +                             &
                      D4410*DSIN(X2OMI+X2LI-G44)+ D4422*DSIN(X2LI-G44)+       &
                      D5220*DSIN( XOMI+XLI-G52) +                             &
                      D5232*DSIN(-XOMI+XLI-G52) +                             &
                      D5421*DSIN( XOMI+X2LI-G54)+                             &
                      D5433*DSIN(-XOMI+X2LI-G54)
                XLDOT = XNI+XFACT
                XNDDT = D2201*DCOS(X2OMI+XLI-G22) + D2211*DCOS(XLI-G22)+     &
                       D3210*DCOS( XOMI+XLI-G32) +                          &
                       D3222*DCOS(-XOMI+XLI-G32) +                          &
                       D5220*DCOS( XOMI+XLI-G52) +                          &
                       D5232*DCOS(-XOMI+XLI-G52) +                          &
                       2.0D0*(D4410*DCOS(X2OMI+X2LI-G44) +                  &
                       D4422*DCOS(X2LI-G44) +                               &
                       D5421*DCOS( XOMI+X2LI-G54) +                         &
                       D5433*DCOS(-XOMI+X2LI-G54))
                XNDDT = XNDDT*XLDOT
              ENDIF

!* ------------------------------- INTEGRATOR --------------------------
              !  sgp4fix move end checks to end of routine
              IF (DABS(T-ATIME).ge.STEPP) THEN
                  IRET  = 0
                  IRETN = 381
                ELSE
                  FT    = T-ATIME
                  IRETN = 0
                ENDIF

              IF (IRETN.EQ.381) THEN
                  XLI   = XLI + XLDOT*DELT + XNDT*STEP2
                  XNI   = XNI + XNDT*DELT + XNDDT*STEP2
                  ATIME = ATIME + DELT
                ENDIF

              ENDDO

            XN = XNI + XNDT*FT  + XNDDT*FT*FT*0.5D0
            XL = XLI + XLDOT*FT + XNDT*FT*FT*0.5D0
            IF(IREZ .ne. 1) THEN
                Mm   = XL-2.0D0*nodem+2.0D0*THETA
                DNDT = XN-No
              ELSE
                Mm   = XL-nodem-Argpm+THETA
                DNDT = XN-No
              ENDIF

            XN = No + DNDT
          ENDIF

      RETURN
      END SUBROUTINE dspace


!* -----------------------------------------------------------------------------


!*
!*                           SUBROUTINE INITL
!*
!*  this subroutine initializes the spg4 propagator. all the initialization is
!*    consolidated here instead of having multiple loops inside other routines.
!*
!*  author        : david vallado                  719-573-2600   28 jun 2005
!*
!*  inputs        :
!*    ecco        - eccentricity                           0.0 - 1.0
!*    epoch       - epoch time in days from jan 0, 1950. 0 hr
!*    inclo       - inclination of satellite
!*    no          - mean motion of satellite
!*    satn        - satellite number
!*
!*  outputs       :
!*    ainv        - 1.0 / a
!*    ao          - semi major axis
!*    con41       -
!*    con42       - 1.0 - 5.0 cos(i)
!*    cosio       - cosine of inclination
!*    cosio2      - cosio squared
!*    eccsq       - eccentricity squared
!*    method      - flag for deep space                    'd', 'n'
!*    omeosq      - 1.0 - ecco * ecco
!*    posq        - semi-parameter squared
!*    rp          - radius of perigee
!*    rteosq      - square root of (1.0 - ecco*ecco)
!*    sinio       - sine of inclination
!*    gsto        - gst at time of observation               rad
!*    no          - mean motion of satellite
!*
!*  locals        :
!*    ak          -
!*    d1          -
!*    del         -
!*    adel        -
!*    po          -
!*
!*  coupling      :
!*    getgravconst-
!*
!*  references    :
!*    hoots, roehrich, norad spacetrack report #3 1980
!*    hoots, norad spacetrack report #6 1986
!*    hoots, schumacher and glover 2004
!*    vallado, crawford, hujsak, kelso  2006
!*------------------------------------------------------------------------------

      SUBROUTINE INITL( Satn , whichconst, Ecco  , EPOCH , Inclo , No,  &
               Method, AINV  , AO    , CON41 , CON42 , COSIO , COSIO2,  &
               Eccsq , OMEOSQ, POSQ  , rp    , RTEOSQ, SINIO ,          &
               GSTo, operationmode )

           IMPLICIT NONE

        CHARACTER :: Method, operationmode
        INTEGER :: Satn, whichconst
        REAL(r8) :: Ecco  , EPOCH , Inclo , No   ,                          &
               AINV  , AO    , CON41 , CON42 , COSIO , COSIO2,         &
               Eccsq , OMEOSQ, POSQ  , rp    , RTEOSQ, SINIO , GSTo    


!* -------------------------- Local Variables --------------------------
!c        sgp4fix use old way of finding gst
        Integer :: ids70
        REAL(r8) :: ts70, ds70, tfrac, c1, thgr70, fk5r, c1p2p, thgr, thgro

        REAL(r8) ::  RadPerDay, Temp, TUT1
        REAL(r8) ::  ak, d1, del, adel, po
        REAL(r8) ::  X2o3, J2, XKE, tumin, mu, radiusearthkm, j3, j4, j3oj2

!* ------------------------ WGS-72 EARTH CONSTANTS ---------------------
        X2o3   = 2.0D0/3.0D0
        ! sgp4fix identify constants and allow alternate values
        CALL getgravconst( whichconst, tumin, mu, radiusearthkm, xke, j2, j3, j4, j3oj2 )

!* ----------------- CALCULATE AUXILLARY EPOCH QUANTITIES --------------
        Eccsq  = Ecco*Ecco
        OMEOSQ = 1.0D0 - Eccsq
        RTEOSQ = DSQRT(OMEOSQ)
        COSIO  = DCOS(Inclo)
        COSIO2 = COSIO*COSIO

!* ---------------------- UN-KOZAI THE MEAN MOTION ---------------------
        AK   =  (XKE/No)**X2O3
        D1   =  0.75D0*J2* (3.0D0*COSIO2-1.0D0) / (RTEOSQ*OMEOSQ)
        DEL  =  D1/(AK*AK)
        ADEL =  AK * ( 1.0D0 - DEL*DEL - DEL* (1.0D0/3.0D0 + 134.0D0*DEL*DEL / 81.0D0) )
        DEL  =  D1/(ADEL*ADEL)
        No   =  No/(1.0D0 + DEL)

        AO   =  (XKE/No)**X2O3
        SINIO=  DSIN(Inclo)
        PO   =  AO*OMEOSQ
        CON42=  1.0D0-5.0D0*COSIO2
        CON41=  -CON42-COSIO2-COSIO2
        AINV =  1.0D0/AO
        POSQ =  PO*PO
        rp   =  AO*(1.0D0-Ecco)
        METHOD = 'n'

!* ----------------- CALCULATE GREENWICH LOCATION AT EPOCH -------------
!c       sgp4fix modern approach to finding sidereal time
        IF (operationmode .ne. 'a') THEN
            RadPerDay  = twopi * 1.002737909350795D0  !6.30038809866574D0
            Temp = Epoch + 2433281.5D0
            TUT1= ( DINT(Temp-0.5D0) + 0.5D0 - 2451545.0D0 ) / 36525.0D0
            Gsto= 1.75336855923327D0 + 628.331970688841D0*TUT1      &
                  + 6.77071394490334D-06*TUT1*TUT1                 &
                  - 4.50876723431868D-10*TUT1*TUT1*TUT1            &
                  + RadPerDay*( Temp-0.5D0-DINT(Temp-0.5D0) )
          ELSE
            ! sgp4fix use old way of finding gst
            ! count integer number of days from 0 jan 1970
           TS70  = EPOCH-7305.0D0
           IDS70 = TS70 + 1.0D-8
           TFRAC = TS70-IDS70
            ! find greenwich location at epoch
           C1    = 1.72027916940703639D-2
           THGR70= 1.7321343856509374D0
            FK5R  = 5.07551419432269442D-15
           C1P2P = C1+TWOPI
           gsto  = THGR70+C1*IDS70+C1P2P*TFRAC+TS70*TS70*FK5R
         ENDIF
         
        ! ------------------------ Check quadrants ---------------------
        Gsto = DMOD( Gsto,TwoPi )
        IF ( Gsto .lt. 0.0D0 ) THEN
            Gsto= Gsto + TwoPi
          ENDIF

!      write(*,*) Satn,'  gst delta ', gsto-gsto1

      RETURN
      END SUBROUTINE initl

!* -----------------------------------------------------------------------------
!*
!*                             SUBROUTINE SGP4INIT
!*
!*  This subroutine initializes variables for SGP4.
!*
!*  author        : david vallado                  719-573-2600   28 jun 2005
!*
!*  inputs        :
!*    satn        - satellite number
!*    bstar       - sgp4 type drag coefficient              kg/m2er
!*    ecco        - eccentricity
!*    epoch       - epoch time in days from jan 0, 1950. 0 hr
!*    argpo       - argument of perigee (output if ds)
!*    inclo       - inclination
!*    mo          - mean anomaly (output if ds)
!*    no          - mean motion
!*    nodeo      - right ascension of ascending node
!*
!*  outputs       :
!*    satrec      - common block values for subsequent calls
!*    return code - non-zero on error.
!*                   1 - mean elements, ecc >= 1.0 or ecc < -0.001 or a < 0.95 er
!*                   2 - mean motion less than 0.0
!*                   3 - pert elements, ecc < 0.0  or  ecc > 1.0
!*                   4 - semi-latus rectum < 0.0
!*                   5 - epoch elements are sub-orbital
!*                   6 - satellite has decayed
!*
!*  locals        :
!*    CNODM  , SNODM  , COSIM  , SINIM  , COSOMM , SINOMM
!*    Cc1sq  , Cc2    , Cc3
!*    Coef   , Coef1
!*    cosio4      -
!*    day         -
!*    dndt        -
!*    em          - eccentricity
!*    emsq        - eccentricity squared
!*    eeta        -
!*    etasq       -
!*    gam         -
!*    argpm       - argument of perigee
!*    ndem        -
!*    inclm       - inclination
!*    mm          - mean anomaly
!*    nm          - mean motion
!*    perige      - perigee
!*    pinvsq      -
!*    psisq       -
!*    qzms24      -
!*    rtemsq      -
!*    s1, s2, s3, s4, s5, s6, s7          -
!*    sfour       -
!*    ss1, ss2, ss3, ss4, ss5, ss6, ss7         -
!*    sz1, sz2, sz3
!*    sz11, sz12, sz13, sz21, sz22, sz23, sz31, sz32, sz33        -
!*    tc          -
!*    temp        -
!*    temp1, temp2, temp3       -
!*    tsi         -
!*    xpidot      -
!*    xhdot1      -
!*    z1, z2, z3          -
!*    z11, z12, z13, z21, z22, z23, z31, z32, z33         -
!*
!*  coupling      :
!*    getgravconst-
!*    initl       -
!*    dscom       -
!*    dpper       -
!*    dsinit      -
!*
!*  references    :
!*    hoots, roehrich, norad spacetrack report #3 1980
!*    hoots, norad spacetrack report #6 1986
!*    hoots, schumacher and glover 2004
!*    vallado, crawford, hujsak, kelso  2006
!* ---------------------------------------------------------------------------- }


      SUBROUTINE SGP4Init ( whichconst,                              &
                            Satn,   xBStar, xEcco,  Epoch, xArgpo,   &
                            xInclo, xMo,    xNo,    xnodeo, Error )
           IMPLICIT NONE

        INTEGER :: Satn, error, whichconst
        REAL(r8) ::  xBStar, xEcco, Epoch, xArgpo, xInclo, xMo, xNo, xnodeo
        REAL(r8) :: T, r(3), v(3)

!* -------------------------- Local Variables --------------------------

        REAL(r8) ::  Ao,ainv,con42,cosio,sinio,cosio2,Eccsq,omeosq,           &
                posq,rp,rteosq, CNODM , SNODM , COSIM , SINIM , COSOMM,  &
                SINOMM, Cc1sq ,                                          &
                Cc2   , Cc3   , Coef  , Coef1 , Cosio4, DAY   , Dndt  ,  &
                Eccm  , EMSQ  , Eeta  , Etasq , GAM   , Argpm , nodem,   &
                Inclm , Mm  , Xn    , Perige, Pinvsq, Psisq , Qzms24,    &
                RTEMSQ, S1    , S2    , S3    , S4    , S5    , S6    ,  &
                S7    , SFour , SS1   , SS2   , SS3   , SS4   , SS5   ,  &
                SS6   , SS7   , SZ1   , SZ2   , SZ3   , SZ11  , SZ12  ,  &
                SZ13  , SZ21  , SZ22  , SZ23  , SZ31  , SZ32  , SZ33  ,  &
                Tc    , Temp  , Temp1 , Temp2 , Temp3 , Tsi   , XPIDOT,  &
                Xhdot1, Z1    , Z2    , Z3    , Z11   , Z12   , Z13   ,  &
                Z21   , Z22   , Z23   , Z31   , Z32   , Z33            
        REAL(r8) ::  qzms2t, SS, mu, RadiusEarthKm, J2, j3oJ2,J4,X2o3,        &
                temp4, j3, xke, tumin

!* ---------------------------- INITIALIZATION -------------------------
        method = 'n'
!c       clear sgp4 flag
        Error = 0

!c      sgp4fix - note the following variables are also passed directly via sgp4 common. 
!c      it is possible to streamline the sgp4init call by deleting the "x"
!c      variables, but the user would need to set the common values first. we
!c      include the additional assignment in case twoline2rv is not used. 
 
        bstar  = xbstar
        ecco   = xecco
        argpo  = xargpo
        inclo  = xinclo
        mo     = xmo
        no     = xno
        nodeo  = xnodeo

        ! sgp4fix identify constants and allow alternate values
        CALL getgravconst( whichconst, tumin, mu, radiusearthkm, xke, j2, j3, j4, j3oj2 )

        SS     = 78.0D0/RadiusEarthKm + 1.0D0
        QZMS2T = ((120.0D0-78.0D0)/RadiusEarthKm) ** 4
        X2o3   =  2.0D0 / 3.0D0
!c     sgp4fix divisor for divide by zero check on inclination
!c     the old check used 1.0D0 + cos(pi-1.0D-9), but then compared it to
!c     1.5D-12, so the threshold was changed to 1.5D-12 for consistency
        temp4    =   1.5D-12

        Init = 'y'
        T = 0.0D0

        CALL INITL( Satn , whichconst, Ecco  , EPOCH , Inclo , No,  &
           Method, AINV  , AO    , CON41 , CON42 , COSIO , COSIO2,  &
           Eccsq , OMEOSQ, POSQ  , rp    , RTEOSQ, SINIO ,          &
           GSTo, Opsmode )

        IF(rp .lt. 1.0D0) THEN
!c            Write(*,*) '# *** SATN',Satn,' EPOCH ELTS SUB-ORBITAL *** '
            Error = 5
          ENDIF

        IF(OMEOSQ .ge. 0.0D0 .OR. No .ge. 0.0D0) THEN
            ISIMP = 0
            IF (rp .lt. (220.0D0/RadiusEarthKm+1.0D0)) THEN
                ISIMP = 1
              ENDIF
            SFour  = SS
            QZMS24 = QZMS2T
            PERIGE = (rp-1.0D0)*RadiusEarthKm

!* ----------- For perigees below 156 km, S and Qoms2t are altered -----
            IF(PERIGE .lt. 156.0D0) THEN
                SFour = PERIGE-78.0D0
                IF(PERIGE .le. 98.0D0) THEN
                    SFour = 20.0D0
                  ENDIF
                QZMS24 = ( (120.0D0-SFour)/RadiusEarthKm )**4
                SFour  = SFour/RadiusEarthKm + 1.0D0
              ENDIF
            PINVSQ = 1.0D0/POSQ

            TSI    = 1.0D0/(AO-SFour)
            ETA    = AO*Ecco*TSI
            ETASQ  = ETA*ETA
            EETA   = Ecco*ETA
            PSISQ  = DABS(1.0D0-ETASQ)
            COEF   = QZMS24*TSI**4
            COEF1  = COEF/PSISQ**3.5D0
            CC2    = COEF1*No* (AO* (1.0D0+1.5D0*ETASQ+EETA*           &
                     (4.0D0+ETASQ) )+0.375D0*                          &
               J2*TSI/PSISQ*CON41*(8.0D0+3.0D0*ETASQ*(8.0D0+ETASQ)))   
            CC1    = BSTAR*CC2
            CC3    = 0.0D0
            IF(Ecco .GT. 1.0D-4) THEN
                CC3 = -2.0D0*COEF*TSI*J3OJ2*No*SINIO/Ecco
              ENDIF
            X1MTH2 = 1.0D0-COSIO2
            CC4    = 2.0D0*No*COEF1*AO*OMEOSQ*(ETA*(2.0D0+0.5D0*ETASQ)  &
                    +Ecco*(0.5D0 + 2.0D0*ETASQ) - J2*TSI / (AO*PSISQ)*  &
                    (-3.0D0*CON41*(1.0D0-2.0D0*                         &
             EETA+ETASQ*(1.5D0-0.5D0*EETA))+0.75D0*X1MTH2*(2.0D0*ETASQ  &
            -EETA*(1.0D0+ETASQ))*DCOS(2.0D0*Argpo)))                   
            CC5    = 2.0D0*COEF1*AO*OMEOSQ* (1.0D0 + 2.75D0*            &
                     (ETASQ + EETA) + EETA*ETASQ )
            COSIO4 = COSIO2*COSIO2
            TEMP1  = 1.5D0*J2*PINVSQ*No
            TEMP2  = 0.5D0*TEMP1*J2*PINVSQ
            TEMP3  = -0.46875D0*J4*PINVSQ*PINVSQ*No
            MDot   = No + 0.5D0*TEMP1*RTEOSQ*CON41 + 0.0625D0*TEMP2*   &
                     RTEOSQ*(13.0D0 - 78.0D0*COSIO2 + 137.0D0*COSIO4)
            ArgpDot= -0.5D0*TEMP1*CON42 + 0.0625D0*TEMP2*              &
                     (7.0D0 - 114.0D0*COSIO2 +                         &
              395.0D0*COSIO4)+TEMP3*(3.0D0-36.0D0*COSIO2+49.0D0*COSIO4)
            XHDOT1 = -TEMP1*COSIO
            nodeDot = XHDOT1+(0.5D0*TEMP2*(4.0D0-19.0D0*COSIO2)+       &
                       2.0D0*TEMP3*(3.0D0 - 7.0D0*COSIO2))*COSIO
            XPIDOT = ArgpDot+nodeDot
            OMGCOF = BSTAR*CC3*DCOS(Argpo)
            XMCOF  = 0.0D0
            IF(Ecco .GT. 1.0D-4) THEN
                XMCOF = -X2O3*COEF*BSTAR/EETA
              ENDIF
            XNODCF = 3.5D0*OMEOSQ*XHDOT1*CC1
            T2COF  = 1.5D0*CC1
!c           sgp4fix for divide by zero with xinco = 180 deg
            if (dabs(cosio+1.0).gt. 1.5d-12) THEN
                XLCOF  = -0.25D0*J3OJ2*SINIO*                 &
                         (3.0D0+5.0D0*COSIO)/(1.0D0+COSIO)
              else
                XLCOF  = -0.25D0*J3OJ2*SINIO*                 &
                         (3.0D0+5.0D0*COSIO)/temp4
              ENDIF
            AYCOF  = -0.5D0*J3OJ2*SINIO
            DELMO  = (1.0D0+ETA*DCOS(Mo))**3
            SINMAO = DSIN(Mo)
            X7THM1 = 7.0D0*COSIO2-1.0D0

!* ------------------------ Deep Space Initialization ------------------
            IF ((TWOPI/No) .ge. 225.0D0) THEN
                METHOD = 'd'
                ISIMP  = 1
                TC     = 0.0D0
                Inclm  = Inclo
                CALL DSCOM( EPOCH     , Ecco  , Argpo , Tc    , Inclo ,   &
                       nodeo, No    ,                                    &
                       SNODM , CNODM , SINIM , COSIM , SINOMM, COSOMM,   &
                       DAY   , E3    , Ee2   , Eccm  , EMSQ  , GAM   ,   &
                       Peo   , Pgho  , Pho   , PInco , Plo   ,           &
                       RTemSq, Se2   , Se3   , Sgh2  , Sgh3  , Sgh4  ,   &
                       Sh2   , Sh3   , Si2   , Si3   , Sl2   , Sl3   ,   &
                       Sl4   , S1    , S2    , S3    , S4    , S5    ,   &
                       S6    , S7    , SS1   , SS2   , SS3   , SS4   ,   &
                       SS5   , SS6   , SS7   , SZ1   , SZ2   , SZ3   ,   &
                       SZ11  , SZ12  , SZ13  , SZ21  , SZ22  , SZ23  ,   &
                       SZ31  , SZ32  , SZ33  , Xgh2  , Xgh3  , Xgh4  ,   &
                       Xh2   , Xh3   , Xi2   , Xi3   , Xl2   , Xl3   ,   &
                       Xl4   , Xn    , Z1    , Z2    , Z3    , Z11   ,   &
                       Z12   , Z13   , Z21   , Z22   , Z23   , Z31   ,   &
                       Z32   , Z33   , Zmol  , Zmos )
                CALL DPPER( e3, ee2   , peo   , pgho  , pho   , pinco ,  &
                       plo   , se2   , se3   , sgh2  , sgh3  , sgh4  ,  &
                       sh2   , sh3   , si2   , si3   , sl2   , sl3   ,  &
                       sl4   , T     , xgh2  , xgh3  , xgh4  , xh2   ,  &
                       xh3   , xi2   , xi3   , xl2   , xl3   , xl4   ,  &
                       zmol  , zmos  , Inclm , init  ,                  &
                       Ecco  , Inclo , nodeo, Argpo , Mo, Opsmode )

                Argpm  = 0.0D0 ! add for DS to work initial
                nodem  = 0.0D0
                Mm     = 0.0D0

                CALL DSINIT( whichconst,                                 &
                        Cosim ,Emsq, Argpo, S1    , S2    , S3    ,      &
                        S4    , S5    , Sinim , Ss1   , Ss2   , Ss3   ,  &
                        Ss4   , Ss5   , Sz1   , Sz3   , Sz11  , Sz13  ,  &
                        Sz21  , Sz23  , Sz31  , Sz33  , T     , Tc    ,  &
                        GSTo  , Mo    , MDot  , No    ,nodeo,nodeDot,    &
                        XPIDOT, Z1    , Z3    , Z11   , Z13   , Z21   ,  &
                        Z23   , Z31   , Z33   , ecco  , eccsq,           &
                        Eccm  , Argpm , Inclm , Mm    , Xn    , nodem,   &
                        IREZ  , Atime , D2201 , D2211 , D3210 , D3222 ,  &
                        D4410 , D4422 , D5220 , D5232 , D5421 , D5433 ,  &
                        Dedt  , Didt  , DMDT  , DNDT  , DNODT , DOMDT ,  &
                        Del1  , Del2  , Del3  , Xfact , Xlamo , Xli   ,  &
                        Xni )
            ENDIF

!* ------------ Set variables if not deep space or rp < 220 -------------
            IF (ISIMP .ne. 1) THEN
                CC1SQ = CC1*CC1
                D2    = 4.0D0*AO*TSI*CC1SQ
                TEMP  = D2*TSI*CC1 / 3.0D0
                D3    = (17.0D0*AO + SFour) * TEMP
                D4    = 0.5D0*TEMP*AO*TSI*(221.0D0*AO + 31.0D0*SFour)*CC1
                T3COF = D2 + 2.0D0*CC1SQ
                T4COF = 0.25D0* (3.0D0*D3+CC1*(12.0D0*D2+10.0D0*CC1SQ) )
                T5COF = 0.2D0* (3.0D0*D4 + 12.0D0*CC1*D3 + 6.0D0*D2*D2 +     &
                        15.0D0*CC1SQ* (2.0D0*D2 + CC1SQ) )
              ENDIF

          ENDIF ! ------ if nodeo and No are gtr 0

      init = 'n'

      CALL SGP4(whichconst, 0.0D0, r, v, error)

      RETURN
      END SUBROUTINE sgp4init

!* -----------------------------------------------------------------------------
!*
!*                             SUBROUTINE SGP4
!*
!*  this procedure is the sgp4 prediction model from space command. this is an
!*    updated and combined version of sgp4 and sdp4, which were originally
!*    published separately in spacetrack report #3. this version follows the
!*    methodology from the aiaa paper (2006) describing the history and
!*    development of the code.
!*
!*  author        : david vallado                  719-573-2600   28 jun 2005
!*
!*  inputs        :
!*    satrec	 - initialised structure from sgp4init() call.
!*    tsince	 - time eince epoch (minutes)
!*
!*  outputs       :
!*    r           - position vector                     km
!*    v           - velocity                            km/sec
!*  return code - non-zero on error.
!*                   1 - mean elements, ecc >= 1.0 or ecc < -0.001 or a < 0.95 er
!*                   2 - mean motion less than 0.0
!*                   3 - pert elements, ecc < 0.0  or  ecc > 1.0
!*                   4 - semi-latus rectum < 0.0
!*                   5 - epoch elements are sub-orbital
!*                   6 - satellite has decayed
!*
!*  locals        :
!*    am          -
!*    axnl, aynl        -
!*    betal       -
!*    COSIM   , SINIM   , COSOMM  , SINOMM  , Cnod    , Snod    , Cos2u   ,
!*    Sin2u   , Coseo1  , Sineo1  , Cosi    , Sini    , Cosip   , Sinip   ,
!*    Cosisq  , Cossu   , Sinsu   , Cosu    , Sinu
!*    Delm        -
!*    Delomg      -
!*    Dndt        -
!*    Eccm        -
!*    EMSQ        -
!*    Ecose       -
!*    El2         -
!*    Eo1         -
!*    Eccp        -
!*    Esine       -
!*    Argpm       -
!*    Argpp       -
!*    Omgadf      -
!*    Pl          -
!*    R           -
!*    RTEMSQ      -
!*    Rdotl       -
!*    Rl          -
!*    Rvdot       -
!*    Rvdotl      -
!*    Su          -
!*    T2  , T3   , T4    , Tc
!*    Tem5, Temp , Temp1 , Temp2  , Tempa  , Tempe  , Templ
!*    U   , Ux   , Uy    , Uz     , Vx     , Vy     , Vz
!*    inclm       - inclination
!*    mm          - mean anomaly
!*    nm          - mean motion
!*    nodem       - longi of ascending node
!*    xinc        -
!*    xincp       -
!*    xl          -
!*    xlm         -
!*    mp          -
!*    xmdf        -
!*    xmx         -
!*    xmy         -
!*    nodedf     -
!*    xnode       -
!*    nodep      -
!*    np          -
!*
!*  coupling      :
!*    getgravconst-
!*    dpper
!*    dpspace
!*
!*  references    :
!*    hoots, roehrich, norad spacetrack report #3 1980
!*    hoots, norad spacetrack report #6 1986
!*    hoots, schumacher and glover 2004
!*    vallado, crawford, hujsak, kelso  2006
!*------------------------------------------------------------------------------

 SUBROUTINE SGP4 ( whichconst, T, r, v, Error )

           IMPLICIT NONE

        INTEGER  :: Error, whichconst
        REAL(r8) ::   T, r(3), v(3)

!* -------------------------- Local Variables --------------------------
        REAL(r8) :: AM    , Axnl  , Aynl  , Betal , COSIM , Cnod  ,           &
               Cos2u , Coseo1, Cosi  , Cosip , Cosisq, Cossu , Cosu  ,   &
               Delm  , Delomg, Eccm  , EMSQ  , Ecose , El2   , Eo1   ,   &
               Eccp  , Esine , Argpm , Argpp , Omgadf, Pl    ,           &
               Rdotl , Rl    , Rvdot , Rvdotl, SINIM ,                   &
               Sin2u , Sineo1, Sini  , Sinip , Sinsu , Sinu  ,           &
               Snod  , Su    , T2    , T3    , T4    , Tem5  , Temp  ,   &
               Temp1 , Temp2 , Tempa , Tempe , Templ , U     , Ux    ,   &
               Uy    , Uz    , Vx    , Vy    , Vz    , Inclm , Mm  ,     &
               XN    , nodem , Xinc  , Xincp , Xl    , Xlm   , Mp  ,     &
               Xmdf  , Xmx   , Xmy   , Xnoddf, Xnode , nodep,            &
               Tc    , Dndt

        REAL(r8) :: X2O3, J2,J3,XKE,J3OJ2, mr,mv,                             &
               mu, RadiusEarthkm, VKmPerSec, temp4, tumin, j4
	INTEGER :: iter

!* ------------------------ WGS-72 EARTH CONSTANTS ---------------------
!* ---------------------- SET MATHEMATICAL CONSTANTS -------------------
      X2O3   = 2.0D0/3.0D0

!c     Keep compiler ok for warnings on uninitialized variables
      mr = 0.0D0
      Coseo1 = 1.0D0
      Sineo1 = 0.0D0

      ! sgp4fix identify constants and allow alternate values
        CALL getgravconst( whichconst, tumin, mu, radiusearthkm, xke, j2, j3, j4, j3oj2 )
!     sgp4fix divisor for divide by zero check on inclination
!     the old check used 1.0D0 + cos(pi-1.0D-9), but then compared it to
!     1.5D-12, so the threshold was changed to 1.5D-12 for consistency
      temp4    =   1.5D-12
      VKmPerSec     =  RadiusEarthKm * xke/60.0D0

!* ------------------------- CLEAR SGP4 ERROR FLAG ---------------------
      Error = 0

!* ----------- UPDATE FOR SECULAR GRAVITY AND ATMOSPHERIC DRAG ---------
      XMDF   = Mo + MDot*T
      OMGADF = Argpo + ArgpDot*T
      XNODDF = nodeo + nodeDot*T
      Argpm  = OMGADF
      Mm     = XMDF
      T2     = T*T
      nodem  = XNODDF + XNODCF*T2
      TEMPA  = 1.0D0 - CC1*T
      TEMPE  = BSTAR*CC4*T
      TEMPL  = T2COF*T2
      IF (ISIMP .ne. 1) THEN
          DELOMG = OMGCOF*T
          DELM   = XMCOF*(( 1.0D0+ETA*DCOS(XMDF) )**3-DELMO)
          TEMP   = DELOMG + DELM
          Mm     = XMDF + TEMP
          Argpm  = OMGADF - TEMP
          T3     = T2*T
          T4     = T3*T
          TEMPA  = TEMPA - D2*T2 - D3*T3 - D4*T4
          TEMPE  = TEMPE + BSTAR*CC5*(DSIN(Mm) - SINMAO)
          TEMPL  = TEMPL + T3COF*T3 + T4*(T4COF + T*T5COF)
        ENDIF
      XN    = No
      Eccm  = Ecco
      Inclm = Inclo
      IF(METHOD .EQ. 'd') THEN
          TC     = T
          CALL DSPACE( IRez  , D2201 , D2211 , D3210 , D3222 , D4410 ,  &
                       D4422 , D5220 , D5232 , D5421 , D5433 , Dedt  ,  &
                       Del1  , Del2  , Del3  , Didt  , Dmdt  , Dnodt ,  &
                       Domdt , Argpo , ArgpDot, T    , TC    , GSTo ,   &
                       Xfact , Xlamo , No   ,                           &
                       Atime , Eccm  , Argpm, Inclm , Xli   , Mm  ,     &
                       XNi   , nodem, Dndt  , XN  )
        ENDIF

!c     mean motion less than 0.0
      IF(XN .LE. 0.0D0) THEN
          Error = 2
        ENDIF
      AM = (XKE/XN)**X2O3*TEMPA**2
      XN = XKE/AM**1.5D0
      Eccm = Eccm-TEMPE
!c   fix tolerance for error recognition
      IF (Eccm .GE. 1.0D0 .or. Eccm.lt.-0.001D0 .or. AM .lt. 0.95) THEN
!c	  write(6,*) '# Error 1, Eccm = ',  Eccm, ' AM = ', AM
          Error = 1
        ENDIF
!c   sgp4fix change test condition for eccentricity   
      IF (Eccm .lt. 1.0D-6) Eccm = 1.0D-6
      Mm     = Mm+No*TEMPL
      XLM    = Mm+Argpm+nodem
      EMSQ   = Eccm*Eccm
      TEMP   = 1.0D0 - EMSQ
      nodem  = DMOD(nodem,TwoPi)
      Argpm  = DMOD(Argpm,TwoPi)
      XLM    = DMOD(XLM,TwoPi)
      Mm     = DMOD(XLM - Argpm - nodem,TwoPi)

!* --------------------- COMPUTE EXTRA MEAN QUANTITIES -----------------
      SINIM  = DSIN(Inclm)
      COSIM  = DCOS(Inclm)

!* ------------------------ ADD LUNAR-SOLAR PERIODICS ------------------
      Eccp   = Eccm
      XINCP  = Inclm
      Argpp  = Argpm
      nodep = nodem
      Mp     = Mm
      SINIP  = SINIM
      COSIP  = COSIM
      IF(METHOD .EQ. 'd') THEN
          CALL DPPER( e3    , ee2   , peo   , pgho  , pho   , pinco ,  &
                      plo   , se2   , se3   , sgh2  , sgh3  , sgh4  ,  &
                      sh2   , sh3   , si2   , si3   , sl2   , sl3   ,  &
                      sl4   , T     , xgh2  , xgh3  , xgh4  , xh2   ,  &
                      xh3   , xi2   , xi3   , xl2   , xl3   , xl4   ,  &
                      zmol  , zmos  , Inclo , 'n'   ,                  &
                      Eccp  , XIncp , nodep, Argpp, Mp, Opsmode )
          IF(XINCP .lt. 0.0D0) THEN
              XINCP  = -XINCP
              nodep  = nodep + PI
              Argpp  = Argpp - PI
            ENDIF
          IF(Eccp .lt. 0.0D0 .OR. Eccp .GT. 1.0D0) THEN
              Error = 3
            ENDIF
        ENDIF

!* ------------------------ LONG PERIOD PERIODICS ----------------------
      IF(METHOD .EQ. 'd') THEN
          SINIP =  DSIN(XINCP)
          COSIP =  DCOS(XINCP)
          AYCOF = -0.5D0*J3OJ2*SINIP
!c         sgp4fix for divide by zero with xincp = 180 deg
          if (dabs(cosip+1.0).gt. 1.5d-12) THEN
              XLCOF  = -0.25D0*J3OJ2*SINIP*                         &
                       (3.0D0+5.0D0*COSIP)/(1.0D0+COSIP)
            else
              XLCOF  = -0.25D0*J3OJ2*SINIP*                         &
                       (3.0D0+5.0D0*COSIP)/temp4
            ENDIF
        ENDIF
      AXNL = Eccp*DCOS(Argpp)
      TEMP = 1.0D0 / (AM*(1.0D0-Eccp*Eccp))
      AYNL = Eccp*DSIN(Argpp) + TEMP*AYCOF
      XL   = Mp + Argpp + nodep + TEMP*XLCOF*AXNL

!* ------------------------- SOLVE KEPLER'S EQUATION -------------------
      U    = DMOD(XL-nodep,TwoPi)
      EO1  = U
      ITER=0
!c   sgp4fix for kepler iteration
!c   the following iteration needs better limits on corrections
      Temp = 9999.9D0
      DO WHILE ((Temp.ge.1.0D-12).and.(ITER.lt.10))
          ITER=ITER+1
          SINEO1= DSIN(EO1)
          COSEO1= DCOS(EO1)
          TEM5  = 1.0D0 - COSEO1*AXNL - SINEO1*AYNL
          TEM5  = (U - AYNL*COSEO1 + AXNL*SINEO1 - EO1) / TEM5
          Temp  = DABS(Tem5)
          IF(Temp.gt.1.0D0) Tem5=Tem5/Temp ! Stop excessive correction
          EO1   = EO1+TEM5
        ENDDO

!* ----------------- SHORT PERIOD PRELIMINARY QUANTITIES ---------------
      ECOSE = AXNL*COSEO1+AYNL*SINEO1
      ESINE = AXNL*SINEO1-AYNL*COSEO1
      EL2   = AXNL*AXNL+AYNL*AYNL
      PL    = AM*(1.0D0-EL2)
!c     semi-latus rectum < 0.0
      IF ( PL .lt. 0.0D0 ) THEN
          Error = 4
        ELSE
          RL    = AM*(1.0D0-ECOSE)
          RDOTL = DSQRT(AM)*ESINE/RL
          RVDOTL= DSQRT(PL)/RL
          BETAL = DSQRT(1.0D0-EL2)
          TEMP  = ESINE/(1.0D0+BETAL)
          SINU  = AM/RL*(SINEO1-AYNL-AXNL*TEMP)
          COSU  = AM/RL*(COSEO1-AXNL+AYNL*TEMP)
          SU    = DATAN2(SINU,COSU)
          SIN2U = (COSU+COSU)*SINU
          COS2U = 1.0D0-2.0D0*SINU*SINU
          TEMP  = 1.0D0/PL
          TEMP1 = 0.5D0*J2*TEMP
          TEMP2 = TEMP1*TEMP

!* ------------------ UPDATE FOR SHORT PERIOD PERIODICS ----------------
          IF(METHOD .EQ. 'd') THEN
              COSISQ = COSIP*COSIP
              CON41  = 3.0D0*COSISQ - 1.0D0
              X1MTH2 = 1.0D0 - COSISQ
              X7THM1 = 7.0D0*COSISQ - 1.0D0
            ENDIF
          mr   = RL*(1.0D0 - 1.5D0*TEMP2*BETAL*CON41) + 0.5D0*TEMP1*X1MTH2*COS2U
          SU   = SU - 0.25D0*TEMP2*X7THM1*SIN2U
          XNODE= nodep + 1.5D0*TEMP2*COSIP*SIN2U
          XINC = XINCP + 1.5D0*TEMP2*COSIP*SINIP*COS2U
          mv   = RDOTL - XN*TEMP1*X1MTH2*SIN2U / XKE
          RVDOT= RVDOTL + XN*TEMP1* (X1MTH2*COS2U+1.5D0*CON41) / XKE

!* ------------------------- ORIENTATION VECTORS -----------------------
          SINSU=  DSIN(SU)
          COSSU=  DCOS(SU)
          SNOD =  DSIN(XNODE)
          CNOD =  DCOS(XNODE)
          SINI =  DSIN(XINC)
          COSI =  DCOS(XINC)
          XMX  = -SNOD*COSI
          XMY  =  CNOD*COSI
          UX   =  XMX*SINSU + CNOD*COSSU
          UY   =  XMY*SINSU + SNOD*COSSU
          UZ   =  SINI*SINSU
          VX   =  XMX*COSSU - CNOD*SINSU
          VY   =  XMY*COSSU - SNOD*SINSU
          VZ   =  SINI*COSSU

!* ----------------------- POSITION AND VELOCITY -----------------------
          r(1) = mr*UX * RadiusEarthkm
          r(2) = mr*UY * RadiusEarthkm
          r(3) = mr*UZ * RadiusEarthkm
          v(1) = (mv*UX + RVDOT*VX) * VKmPerSec
          v(2) = (mv*UY + RVDOT*VY) * VKmPerSec
          v(3) = (mv*UZ + RVDOT*VZ) * VKmPerSec
        ENDIF

!* --------------------------- ERROR PROCESSING ------------------------
!c     sgp4fix for decaying satellites
      if (mr .lt. 1.0D0) THEN
!c          write(*,*) '# decay condition ',mr
          error = 6
        ENDIF

      RETURN
END SUBROUTINE SGP4

!* -----------------------------------------------------------------------------
!*
!*                           FUNCTION GSTIME
!*
!*  This function finds the Greenwich SIDEREAL time.  Notice just the INTEGER
!*    part of the Julian Date is used for the Julian centuries calculation.
!*    We use radper Solar day because we're multiplying by 0-24 solar hours.
!*
!*  Author        : David Vallado                  719-573-2600    1 Mar 2001
!*
!*  Inputs          Description                    Range / Units
!*    JD          - Julian Date                    days from 4713 BC
!*
!*  OutPuts       :
!*    GSTIME      - Greenwich SIDEREAL Time        0 to 2Pi rad
!*
!*  Locals        :
!*    Temp        - Temporary variable for reals   rad
!*    TUT1        - Julian Centuries from the
!*                  Jan 1, 2000 12 h epoch (UT1)
!*
!*  Coupling      :
!*
!*  References    :
!*    Vallado       2007, 194, Eq 3-45
!* -----------------------------------------------------------------------------

      REAL(r8) FUNCTION GSTIME1(JD)
        IMPLICIT NONE
        REAL(r8) JD
!* ----------------------------  Locals  -------------------------------
        REAL(r8) Temp, TUT1

        ! --------------------  Implementation   ----------------------

        TUT1= ( JD - 2451545.0D0 ) / 36525.0D0
        Temp= - 6.2D-6*TUT1*TUT1*TUT1 + 0.093104D0*TUT1*TUT1     &
             + (876600.0D0*3600.0D0 + 8640184.812866D0)*TUT1     &
             + 67310.54841D0
        Temp= DMOD( Temp*Deg2Rad/240.0D0,TwoPi ) ! 360/86400 = 1/240, to deg, to rad

        ! ------------------------ Check quadrants --------------------
        IF ( Temp .lt. 0.0D0 ) THEN
            Temp= Temp + TwoPi
          ENDIF

        GSTIME1= Temp

      RETURN
      END  FUNCTION gstime1


!* -----------------------------------------------------------------------------
!*
!*                           function getgravconst
!*
!*  this function gets constants for the propagator. note that mu is identified to
!*    facilitiate comparisons with newer models.
!*
!*  author        : david vallado                  719-573-2600   21 jul 2006
!*
!*  inputs        :
!*    whichconst  - which set of constants to use  721, 72, 84
!*
!*  outputs       :
!*    tumin       - minutes in one time unit
!*    mu          - earth gravitational parameter
!*    radiusearthkm - radius of the earth in km
!*    xke         - reciprocal of tumin
!*    j2, j3, j4  - un-normalized zonal harmonic values
!*    j3oj2       - j3 divided by j2
!*
!*  locals        :
!*
!*  coupling      :
!*
!*  references    :
!*    norad spacetrack report #3
!*    vallado, crawford, hujsak, kelso  2006
!*  ---------------------------------------------------------------------------- 

SUBROUTINE getgravconst(whichconst, tumin, mu1, radiusearthkm, xke, j2, j3, j4, j3oj2 )

         IMPLICIT NONE  
   
       REAL(r8), intent(out):: radiusearthkm, xke, j2, j3, j4, j3oj2, mu1, tumin
       INTEGER, INTENT(in)  :: whichconst

       if (whichconst.eq.721) THEN
           ! -- wgs-72 low precision str#3 constants --
           radiusearthkm = 6378.135D0     ! km
           xke    = 0.0743669161D0
           mu1     = 398600.79964D0            ! in km3 / s2
           tumin  = 1.0D0 / xke
           j2     =   0.001082616D0
           j3     =  -0.00000253881D0
           j4     =  -0.00000165597D0
           j3oj2  =  j3 / j2
         ENDIF
       if (whichconst.eq.72) THEN
           ! ------------ wgs-72 constants ------------
           mu1     = 398600.8D0            ! in km3 / s2
           radiusearthkm = 6378.135D0     ! km
           xke    = 60.0D0 / dsqrt(radiusearthkm**3/mu1)
           tumin  = 1.0D0 / xke
           j2     =   0.001082616D0
           j3     =  -0.00000253881D0
           j4     =  -0.00000165597D0
           j3oj2  =  j3 / j2
         ENDIF  
       if (whichconst.eq.84) THEN
           ! ------------ wgs-84 constants ------------
           mu1     = 398600.5D0            ! in km3 / s2
           radiusearthkm = 6378.137D0     ! km
           xke    = 60.0D0 / dsqrt(radiusearthkm**3/mu1)
           tumin  = 1.0D0 / xke
           j2     =   0.00108262998905D0
           j3     =  -0.00000253215306D0
           j4     =  -0.00000161098761D0
           j3oj2  =  j3 / j2
         ENDIF

       RETURN
END SUBROUTINE getgravconst !  SUBROUTINE getgravconst


SUBROUTINE XYZ2LL ( X, Y, Z, JD, Latgc, Latgd, Lon )

        IMPLICIT NONE

        REAL(r8), intent(in)  ::  X, Y, Z, JD
        REAL(r8), intent(out) ::  Latgc, Latgd, Lon

        REAL(r8) ::  R(3), Hellp

        R(1) = X
        R(2) = Y
        R(3) = Z
        call ijk2llE ( R, JD, Latgc, Latgd, Lon, Hellp )

end SUBROUTINE XYZ2LL

SUBROUTINE ijk2llE ( R, JD, Latgc, Latgd, Lon, Hellp )

        IMPLICIT NONE

        REAL(r8), intent(in)  ::  R(3), JD
        REAL(r8), intent(out) ::  Latgc, Latgd, Lon, Hellp
        !EXTERNAL GSTIME, MAG

! * -----------------------------  Locals  ------------------------------
        INTEGER :: i
        Real(r8) :: rsite, DeltaLat, RSqrd
        REAL(r8) :: RtAsc, OldDelta, Decl, Temp, GST, SinTemp, GSTime, OneMinusE2, MAG, magr
        CHARACTER :: Show

        REAL(r8), PARAMETER :: rekm       = 6378.137D0 
        REAL(r8), PARAMETER :: muzz       = 398600.4418D0
        REAL(r8), PARAMETER :: omegaearth = 7.2921158553D-5
        REAL(r8), PARAMETER :: flat       = 0.003352810665D0 ! f = 1.0/298.257223563
        REAL(r8), PARAMETER :: EESqrd     = 0.006694379990D0 ! 2f - f**2
        REAL(r8), PARAMETER :: auer       = 23454.79095228D0 ! 149597870.0/6378.137

        ! --------------------  Implementation   ----------------------
        Show = 'N'

       ! -------------------  Initialize values   --------------------
        magr = MAG1(R)
        OneMinuse2 = 1.0D0 - EeSqrd

       ! ---------------- Find Longitude value  ----------------------
        Temp = DSQRT( R(1)*R(1) + R(2)*R(2) )
        IF ( DABS( Temp ) .lt. Small ) THEN
            RtAsc= DSIGN(1.0D0, R(3))*Pi*0.5D0
        ELSE
            RtAsc= DATAN2( R(2) / Temp , R(1) / Temp )
        ENdif
        GST  = GSTIME1(JD)
        Lon  = RtAsc - GST 

        IF ( DABS(Lon) .ge. Pi ) THEN
            IF ( Lon .lt. 0.0D0 ) THEN
                Lon= TwoPi + Lon
            ELSE
                Lon= Lon - TwoPi 
            ENDIF
        ENDIF
       ! -------------- Set up initial latitude value  ---------------  
        Decl    = DASIN( R(3) / magr )
        Latgc= Decl 
        DeltaLat= 100.0D0 
        RSqrd   = magr**2

       ! ---- Iterate to find Geocentric .and. Geodetic Latitude  -----  
        i= 1 
        DO WHILE ( ( DABS( OldDelta - DeltaLat ) .ge. Small ) .and. &
                  ( i .lt. 10 ))
            OldDelta = DeltaLat 
            rsite    = DSQRT( OneMinuse2 / (1.0D0 - &
                      EeSqrd*(DCOS(Latgc))**2 ) )
            Latgd = DATAN( DTAN(Latgc) / OneMinuse2 ) 
            Temp     = Latgd-Latgc 
            SinTemp  = DSIN( Temp ) 
            Hellp    = DSQRT( RSqrd - rsite*rsite*SinTemp*SinTemp ) -   &
                      rsite*DCOS(Temp)
            DeltaLat = DASIN( Hellp*SinTemp / magr )
            Latgc = Decl - DeltaLat 
            i = i + 1
            IF ( Show .eq. 'Y' ) THEN
                write(*, *) 'E loops gc gd ', Latgc*57.29578D0,  &
                          Latgd*57.29578D0
              ENDIF
          ENDDO

        IF ( i .ge. 10 ) THEN
           Write(*, *) 'ijk2ll did NOT converge '
        ENDIF

      RETURN

END SUBROUTINE ijk2llE 

!* ------------------------------------------------------------------------------
!*
!*                           SUBROUTINE MAG
!*
!*  this subroutine finds the magnitude of a vector.  The tolerance is set to
!*    0.00000001D0, thus the 1.0D0E-16 for the squared test of underflows.
!*
!*  Author        : David Vallado                  719-573-2600    1 Mar 2001
!*
!*  Inputs          Description                    Range / Units
!*    Vec       - Vector
!*
!*  OutPuts       :
!*    Vec       - Answer stored in fourth component
!*
!*  Locals        :
!*    None.
!*
!*  Coupling      :
!*    None.
!*
!* ------------------------------------------------------------------------------  

REAL(r8) FUNCTION MAG1(Vec)

          IMPLICIT NONE
        REAL(r8) Vec(3)
!* -----------------------------  Locals  ------------------------------
        Real(r8) Temp

        ! --------------------  Implementation   ----------------------
        Temp= Vec(1)*Vec(1) + Vec(2)*Vec(2) + Vec(3)*Vec(3)

        IF ( DABS( Temp ) .ge. 1.0D-16 ) THEN
            MAG1 = DSQRT( Temp )
          ELSE
            MAG1 = 0.0D0
          ENDIF
      RETURN
END FUNCTION MAG1




! -----------------------------------------------------------------------------
!
!                           SUBROUTINE FINDDAYS
!
!  this subroutine finds the fractional days through a year given the year,
!    month, day, hour, Minute and second.
!
!  Algorithm     : Set up array for the Number of days per month
!                  Find Leap Year - be sure to account for the 400 years 
!                  Check for a leap year
!                  Loop to find the elapsed days in the year
!
!  Author        : David Vallado                  719-573-2600    1 Mar 2001
!
!  Inputs          Description                    Range / Units
!    Year        - Year                           1900 .. 2100
!    Mon         - Month                          1 .. 12
!    Day         - Day                            1 .. 28,29,30,31
!    Hr          - Hour                           0 .. 23
!    Min         - Minute                         0 .. 59
!    Sec         - Second                         0.0D0 .. 59.999D0
!
!  OutPuts       :
!    Days        - Day of year plus fraction of a
!                    day                          days
!
!  Locals        :
!    LMonth      - Length of months of year
!    i           - Index
!
!  Coupling      :
!    None.
!
!  References    :
!    Vallado       2007, 207, Ex 3-12
!
! -----------------------------------------------------------------------------

      SUBROUTINE FINDDAYS    ( Year,Month,Day,Hr,Min, Sec,  Days )
        IMPLICIT NONE
        INTEGER Year, Month, Day, Hr, Min
        REAL*8 Sec, Days
! ----------------------------  Locals  -------------------------------
        INTEGER i, LMonth(12)

        ! --------------------  Implementation   ----------------------
        DO i = 1,12
            LMonth(i) = 31
          ENDDO
        LMonth( 2) = 28
        LMonth( 4) = 30
        LMonth( 6) = 30
        LMonth( 9) = 30
        LMonth(11) = 30
        IF (MOD(Year,4).eq.0) THEN
            LMonth(2)= 29
          ENDIF

        i   = 1
        Days= 0.0D0
        DO WHILE ((i .lt. Month) .and. ( i .lt. 12 ))
            Days= Days + LMonth(i)
            i= i + 1
          ENDDO

        Days= Days + Day + Hr/24.0D0 + Min/1440.0D0 + Sec/86400.0D0
      RETURN
      END SUBROUTINE FINDDAYS


 ! ............................................................................
 

!  NOTE: This routine is no longer used because it is suspected to have a bug.

#if 0

   SUBROUTINE FindGroundTrackRepeat(repeatday,nymdnew, nhmsnew, p, nymd, nhms)

        IMPLICIT NONE

! !INPUT PARAMETERS:

        integer,  intent(in) :: nymd(2)  ! Beginning/ending date: YYYYMMDD
        integer,  intent(in) :: nhms(2)  ! Beginning/ending time: HHMMSS

! 
! !OUTPUT PARAMETERS:

        real(r8), intent(out)  :: repeatday
        integer,  intent(inout) :: nymdnew(2)  ! Beginning/ending date: YYYYMMDD
        integer,  intent(inout) :: nhmsnew(2)  ! Beginning/ending time: HHMMSS

!---
       real(r8)    :: NewJDs
       real(r8), pointer :: lonst(:)  ! Ground track longitudes [degrees]  
       real(r8), pointer :: latst(:)  ! Ground track latitudes  [degrees]  
       real(r8)          :: deltat
       INTEGER           :: YrPer, DayPer,MonPer,HrPer,MinutePer
       INTEGER           :: Topmin, YearPer2,MonPer2,DayPer2,HrPer2,MinPer2
       INTEGER           :: Yeartmp,Montmp,Daytmp,Hrtmp,Mintmp
       REAL(r8)          :: Sectmp
       REAL(r8)          :: SecPer, SecPer2, ttemp, JDSatEpochPer, JDPER
       INTEGER           :: nymdPer(2), nhmsPer(2)
       type(TLE)         :: p
       integer           :: rc
       integer           :: searchstartday = 15

       real(r8) :: alat(1:100), alon(1:100)
       integer  :: i, ilk, son, temp 
       REAL(r8), DIMENSION(:),  ALLOCATABLE :: blat, blon, dist_m
       REAL(r8) :: dist_l, holdonVal
       INTEGER  :: holdonLoc
       INTEGER, DIMENSION(1:6)   :: timevec_st, timevec_end, timevec_rt
       real(r8)    :: s_fraction2day, e_fraction2day
       integer     :: Sat
       real(r8)    :: JDs, JDe, norm, differ

!      Calculate Epoch time to JD (Reference date)
!      ------------------------------------------
       rc = 0
       deltat   = 0.1

!      Temporary year fix
!      ------------------
        IF (p%TEpochYr.lt.57) THEN
            YrPer = p%TEpochYr + 2000
        ELSE
            YrPer = p%TEpochYr + 1900
        ENDIF
        CALL Days2MDHMS( YrPer,p%TEpDay, MonPer,DayPer,HrPer,MinutePer,SecPer )
        CALL JDAY ( YrPer, MonPer,DayPer,HrPer,MinutePer,SecPer,  JDSatEpochPer )
        
!       Calculate Epoch time + 17 days to JD (Reference date)
        Topmin = 17 !*24*60
        JDPER = JDSatEpochPer + Topmin!/1440.0D0
        CALL INVJDAY( JDPER, YearPer2,MonPer2,DayPer2,HrPer2,MinPer2, SecPer2 )

!       normal formata cevir
          nymdPer(1)   = YrPer * 10000 + MonPer *100 + DayPer
          nhmsPer(1)   = HrPer * 10000 + MinutePer *100 + int(SecPer)
          nymdPer(2)   = YearPer2 * 10000 + MonPer2 *100 + DayPer2
          nhmsPer(2)   = HrPer2 * 10000 + MinPer2 *100 + int(SecPer2)

!       create the data for 17 days and find exact track repeat period 
        CALL GroundTrackTLE(lonst, latst, nymdPer, nhmsPer, deltat, p, rc)
!       Get the first 100 as a reference
        alat(1:100) = latst(1:100)
        alon(1:100) = lonst(1:100)
        i=1;
        ilk = searchstartday*24*60/deltat !30 !(deltat*60) !carresponds to 15 days time steps for 0.5 in sec
        temp = size(alat,1)-1
        son = ilk + temp 
        holdonVal = 999999999.9999999999
        holdonLoc = -1

        do while (son < size(latst,1))      !//check for end of the tail versus end of array 
            !print*, son-ilk, ilk, son
            ALLOCATE(blat(1:(son-ilk+1)), blon(1:(son-ilk+1))) ! deallocate in the driver
           blat(1:(son-ilk+1)) = latst(ilk:son)
           blon(1:(son-ilk+1)) = lonst(ilk:son)
           dist_l =  sum(sqrt((alat(:)-blat(:))**2 + (alon(:)-blon(:))**2)) 
           if (holdonVal >= dist_l) then
                   holdonVal = dist_l
                   holdonLoc = i
           end  if
           i=i+1
           ilk = ilk + 1
           son = son + 1
           DEALLOCATE(blat, blon)
        end do

        repeatday = searchstartday + holdonLoc*deltat/(24*60)

!       ********************************
!       ********************************
!       shift original time interval to refeence time interval

!       Step 1 -- nymd format to vector format and then to JD
        CALL get_time(Sat, nymd(1), nhms(1), s_fraction2day,  timevec_st) ! handle time 
        CALL JDay ( timevec_st(1), timevec_st(2), timevec_st(3), timevec_st(4) , timevec_st(5), real(timevec_st(6),r8), JDs )
        CALL get_time(Sat, nymd(2), nhms(2), e_fraction2day,  timevec_end)
        CALL JDay ( timevec_end(1), timevec_end(2), timevec_end(3), timevec_end(4) , timevec_end(5), real(timevec_end(6),r8), JDe )

!print*, "ilk-", JDSatEpochPer, JDs, JDe, differ

!       Step 2 -- Find difference between start and end JDs
        differ = JDe - JDs
!       Step 3 -- Normalize JD start according to reference date to 0
        norm = -JDSatEpochPer ! normalization factor
        JDs = JDs + norm
        JDSatEpochPer = JDSatEpochPer + norm
!        print*, "after norm-", JDSatEpochPer, JDs, JDe, differ
!       Step 4 -- Take the mod for JD start to shift to reference date
!       out(newJD) and add normalization factor ...
        
        if ( (JDs-JDSatEpochPer) >= 0 ) then
           newJDs =  JDSatEpochPer + MOD(JDs,repeatday )
        else
           newJDs =  JDSatEpochPer + (repeatday -  ABS(MOD(JDs,repeatday ) ))
        end if


        newJDs        = newJDs - norm
        JDSatEpochPer = JDSatEpochPer - norm
        JDs           = JDs - norm
!       Step 5 -- From NewJD calculate shifted start day
        CALL INVJDAY( newJDs, Yeartmp,Montmp,Daytmp,Hrtmp,Mintmp, Sectmp)
        nymdnew(1)   = Yeartmp * 10000 + Montmp *100 + Daytmp
        nhmsnew(1)   = Hrtmp * 10000 + Mintmp *100 + int(Sectmp)
!       Step 6 -- Do the same for the end date. Just add the difference
        CALL INVJDAY( (newJDs+differ), Yeartmp,Montmp,Daytmp,Hrtmp,Mintmp, Sectmp)
        nymdnew(2)   = Yeartmp * 10000 + Montmp *100 + Daytmp
        nhmsnew(2)   = Hrtmp * 10000 + Mintmp *100 + int(Sectmp)


!        print*, nhmsnew(1), nhmsnew(2)
!        print*, " ++++++ "



   END SUBROUTINE FindGroundTrackRepeat

#endif


END MODULE sgp4_mod


