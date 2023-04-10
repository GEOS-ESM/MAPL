#if 0

                  TLE File Structure
                  ------------------          

                       ------
                       Line 1
                       ------

Column               Description
------  ----------------------------------------------------
01      Line Number of Element Data
03-07   Satellite Number
08      Classification (U=Unclassified)
10-11   International Designator, last 2 digits of launch year, 2000+ if < 57.
12-14   International Designator, launch number of the year
15-17   International Designator, piece of the launch
19-20   Epoch Year, last two digits of year,  2000+ if < 57
21-32   Epoch Day of the year and fractional portion of the day
34-43   First Time Derivative of the Mean Motion
45-52   Second Time Derivative of Mean Motion (decimal point assumed)
54-61   BSTAR drag term (decimal point assumed)
63      Ephemeris type
65-68   Element number
69      Checksum (Modulo 10)
        (Letters, blanks, periods, plus signs = 0; minus signs = 1)

                       ------
                       Line 2
                       ------

Column               Description
------  ----------------------------------------------------
01      Line Number of Element Data
03-07   Satellite Number
09-16   Inclination [Degrees]
18-25   Right Ascension of the Ascending Node [Degrees]
27-33   Eccentricity (decimal point assumed)
35-42   Argument of Perigee [Degrees]
44-51   Mean Anomaly [Degrees]
53-63   Mean Motion [Revs per day]
64-68   Revolution number at epoch [Revs]
69      Checksum (Modulo 10)

#endif

  MODULE TLE_mod

      IMPLICIT NONE

      PRIVATE
      PUBLIC TLE
      PUBLIC TLE_Read
      PUBLIC R8

      REAL*8 R8_
      INTEGER, PARAMETER :: R8=kind(R8_)

      TYPE TLE

          Character(len=12) :: TSatName
          INTEGER           :: TICrdno, Tnexp, Tbexp
          REAL(r8)          :: TEPDay
          
          INTEGER           :: TSatNum, TELNO, TEpochYr, TEPHTYP, TREVI
          REAL(r8)          :: TBStar , TEcco  , TInclo , Tnodeo, TArgpo , TNo, TMo, TNDot, TNDDot
          REAL(r8)          :: Tstartmfe, Tstopmfe, TDeltaMin

       END TYPE TLE

CONTAINS

  ! This code is awful; needs rewriting

  SUBROUTINE TLE_Read(InFileName, p, Code)

    implicit none

! !INPUT PARAMETERS:
       Character(len=*), intent(in) :: InFileName

! !OUTPUT PARAMETERS:
       type(TLE), intent(out)       :: p
       Integer, intent(out)         :: Code


       Character(len=130) :: LongStr1, LongStr2

! Assumed only one tle data exist in the file
! print*, InFileName, "   --- right before reading ----"

       Code = 0

       OPEN(11,FILE = InFileName ,STATUS='OLD', ACCESS = 'SEQUENTIAL', err=994 )

!* ----------------- READ THE FIRST LINE OF ELEMENT SET ----------------
        LongStr1 = ' '
   50   READ(11,'(a130)',END=999,err=995) LongStr1
        if ( code /= 0 ) return
        IF(LongStr1(1:1) .eq. '#') GOTO 50 ! Commented line of text, skip
        READ(LongStr1,500,err=996) p%TICRDNO,p%TSatNum,p%TSatName,p%TEpochYr,p%TEpDay,       &
                            p%TNDot,p%TNDDot,p%Tnexp,p%TBStar,p%Tbexp,p%TEPHTYP,p%TELNO
  500   FORMAT( I1,1X,I5,1X,A10,I2,D12.0,1X,D10.0,1X,  &
                F6.5,I2,1X,F6.5,I2,1X,I1,1X,I4 )

!* ----------- READ THE SECOND LINE OF ELEMENT SET AND TIME ------------
        LongStr2 = ' '
   51   READ(11,'(a130)',END=999,err=997) LongStr2
        if ( code /= 0 ) return
        IF(LongStr2(1:1) .eq. '#') GOTO 51 ! Commented line of text, skip

        READ(LongStr2,501,err=998) p%TICRDNO,p%TInclo,p%Tnodeo,p%TEcco,p%TArgpo,p%TMo,p%TNo,p%TREVI
        if ( code /= 0 ) return
  501   FORMAT( I1,7X,D8.0,1X,D8.0,1X,F7.7,1X,D8.0,1X,D8.0,1X,D11.0,I5)

!!!      print *, 'p%TICRDNO,p%TInclo,p%Tnodeo,p%TEcco,p%TArgpo,p%TMo,p%TNo,p%TREVI'
!!!      print *, p%TICRDNO,p%TInclo,p%Tnodeo,p%TEcco,p%TArgpo,p%TMo,p%TNo,p%TREVI

      GOTO 1000

999   Code = 999

1000 CONTINUE

     CLOSE(11)

     return

994  Code=994; return
995  Code=995; return
996  Code=996; return
997  Code=997; return
998  Code=998; return

   END SUBROUTINE TLE_Read

END MODULE TLE_mod

