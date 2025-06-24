#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
#include "MAPL_Generic.h"
module MAPL_StateMaskMod
   use ESMF
   use MAPL_KeywordEnforcerMod
   use ESMFL_Mod
   use MAPL_BaseMod
   use MAPL_ExceptionHandling
   use gFTL_StringVector
   use MAPL_StateArithmeticParserMod
   use MAPL_Constants
   implicit none
   private

   type, public :: StateMask
      character(len=:), allocatable :: mask_type
      character(len=:), allocatable :: mask_arguments
      contains
         procedure :: get_mask_variables
         procedure :: evaluate_mask
         procedure :: evaluate_region_mask
         procedure :: evaluate_zone_mask
         procedure :: evaluate_box_mask
   end type StateMask

   interface StateMask
      module procedure new_StateMask
   end interface StateMask

   contains

   function new_StateMask(mask_expression,rc) result(new_mask)
      type(StateMask) :: new_mask
      character(len=*), intent(in) :: mask_expression
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=:), allocatable :: function_name
      character(len=:), allocatable :: arguments
      integer :: i1,len

      i1 = index(mask_expression,"(")
      __ASSERT(i1 > 0,'Incorrect format for function expression: missing "("')
      function_name = adjustl(mask_expression(:i1-1))
      function_name = ESMF_UtilStringLowerCase(function_name, __RC)

      if (index(function_name,"regionmask") /= 0) then
         new_mask%mask_type = "regionmask"
      else if (index(function_name,"zonemask") /= 0) then
         new_mask%mask_type = "zonemask"
      else if (index(function_name,"boxmask") /= 0) then
         new_mask%mask_type = "boxmask"
      else
         __FAIL("Invalid mask type")
      end if

      len = len_trim(mask_expression)
      arguments = adjustl(mask_expression(i1+1:len-1))
      i1 = index(arguments,",")
      __ASSERT(i1 > 0,'Incorrect format for function expression: missing ","')
      new_mask%mask_arguments = arguments
      __RETURN(__SUCCESS)
   end function

   function get_mask_variables(this,rc) result(variables_in_mask)
      class(StateMask), intent(inout) :: this
      type(StringVector) :: variables_in_mask
      integer, intent(out), optional :: rc

      integer                         :: i1,i2
      logical                         :: twovar
      character(len=:), allocatable   :: tmpstring1,tmpstring2

      if (this%mask_type == "regionmask") twovar = .true.
      if (this%mask_type == "zonemask") twovar = .false.
      if (this%mask_type == "boxmask") twovar = .false.
      i1 = index(this%mask_arguments,",")
      i2 = index(this%mask_arguments,";")
      if (twovar) then
         tmpstring1 = this%mask_arguments(1:i1-1)
         variables_in_mask = parser_variables_in_expression(tmpstring1)
         tmpstring2 = this%mask_arguments(i1+1:i2-1)
         call variables_in_mask%push_back(trim(tmpstring2))
      else
       tmpstring1 = this%mask_arguments(1:i1-1)
         variables_in_mask = parser_variables_in_expression(tmpstring1)
      end if
      __RETURN(__SUCCESS)

   end function

   subroutine evaluate_mask(this,state,field,rc)
      class(StateMask), intent(inout) :: this
      type(ESMF_State), intent(inout) :: state
      type(ESMF_Field), intent(inout) :: field
      integer, optional, intent(out) :: rc

      integer :: status
      select case(this%mask_type)
      case("regionmask")
         call this%evaluate_region_mask(state,field,__RC)
      case("zonemask")
         call this%evaluate_zone_mask(state,field,__RC)
      case("boxmask")
         call this%evaluate_box_mask(state,field,__RC)
      end select
      __RETURN(__SUCCESS)
   end subroutine evaluate_mask

   subroutine evaluate_region_mask(this,state,field,rc)
      class(StateMask), intent(inout) :: this
      type(ESMF_State), intent(inout) :: state
      type(ESMF_Field), intent(inout)   :: field
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: k,i
      character(len=:), allocatable :: maskString,maskname,vartomask
      integer, allocatable :: regionNumbers(:), flag(:)
      integer, allocatable :: mask(:,:)
      real, pointer        :: rmask(:,:)
      real, pointer        :: var2d(:,:)
      real, pointer        :: var3d(:,:,:)
      real, pointer        :: out_var2d(:,:)
      real, pointer        :: out_var3d(:,:,:)
      integer              :: rank,ib,ie

      call ESMF_FieldGet(field,rank=rank,__RC)

       ! get mask string
       ib = index(this%mask_arguments,";")
       maskString = this%mask_arguments(ib+1:)
       ! get mask name
       ie = index(this%mask_arguments,";")
       ib = index(this%mask_arguments,",")
       vartomask = this%mask_arguments(:ib-1)
       maskname = this%mask_arguments(ib+1:ie-1)

       call MAPL_GetPointer(state,rmask,maskName,__RC)
       if (rank == 2) then
          call ESMF_FieldGet(field,0,farrayPtr=out_var2d,__RC)
          call MAPL_GetPointer(state,var2d, vartomask, __RC)
       else if (rank == 3) then
          call ESMF_FieldGet(field,0,farrayPtr=out_var3d,__RC)
          call MAPL_GetPointer(state,var3d, vartomask, __RC)
       else
          __FAIL('Rank must be 2 or 3')
       end if

       k=32
       allocate(regionNumbers(k), flag(k), stat=status)
       __VERIFY(STATUS)
       regionNumbers = 0
       call ExtDataExtractIntegers(maskString,k,regionNumbers,rc=status)
       __VERIFY(STATUS)
       flag(:) = 1
       WHERE(regionNumbers(:) == 0) flag(:) = 0
       k = SUM(flag)
       deallocate(flag,stat=status)
       __VERIFY(STATUS)

   !   Set local mask to 1 where gridMask matches each integer (within precision!)
   !   ---------------------------------------------------------------------------
       allocate(mask(size(rmask,1),size(rmask,2)),stat=status)
       __VERIFY(STATUS)
       mask = 0
       DO i=1,k
        WHERE(regionNumbers(i)-0.01 <= rmask .AND. &
              rmask <= regionNumbers(i)+0.01) mask = 1
       END DO

       if (rank == 2) then
          out_var2d = var2d
          where(mask == 0) out_var2d = 0.0
       else if (rank == 3) then
          out_var3d = var3d
          do i=1,size(var3d,3)
             where(mask == 0) out_var3d(:,:,i) = 0.0
          enddo
       end if
       deallocate( mask)

      __RETURN(__SUCCESS)
   end subroutine evaluate_region_mask

   subroutine evaluate_zone_mask(this,state,field,rc)
      class(StateMask), intent(inout) :: this
      type(ESMF_State), intent(inout) :: state
      type(ESMF_Field), intent(inout) :: field
      integer, optional, intent(out) :: rc

      integer :: status

       integer :: i
       character(len=:), allocatable :: vartomask,clatS,clatN
       real, pointer        :: var2d(:,:)
       real, pointer        :: var3d(:,:,:)
       real, pointer        :: out_var2d(:,:)
       real, pointer        :: out_var3d(:,:,:)
       real(REAL64), pointer :: lats(:,:)
       real(REAL64)         :: limitS, limitN
       type(ESMF_Grid)      :: grid
       integer              :: rank,ib,is
       type(ESMF_CoordSys_Flag) :: coordSys

       call ESMF_FieldGet(field,rank=rank,grid=grid,__RC)

       ib = index(this%mask_arguments,",")
       vartomask = this%mask_arguments(:ib-1)
       is = index(this%mask_arguments,",",back=.true.)
       clatS = this%mask_arguments(ib+1:is-1)
       clatN = this%mask_arguments(is+1:)
       READ(clatS,*,IOSTAT=status) limitS
       __VERIFY(status)
       READ(clatN,*,IOSTAT=status) limitN
       __VERIFY(status)

       call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
            staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=lats, rc=status)
       __VERIFY(status)
       call ESMF_GridGet(grid,coordsys=coordsys,__RC)
       if (coordsys == ESMF_COORDSYS_SPH_RAD) then
          limitN=limitN*MAPL_PI_R8/180.0d0
          limitS=limitS*MAPL_PI_R8/180.0d0
       end if

       if (rank == 2) then
          call ESMF_FieldGet(field,0,farrayPtr=out_var2d,__RC)
          call MAPL_GetPointer(state,var2d, vartomask, __RC)
       else if (rank == 3) then
          call ESMF_FieldGet(field,0,farrayPtr=out_var3d,__RC)
          call MAPL_GetPointer(state,var3d, vartomask, __RC)
       else
          __FAIL('Rank must be 2 or 3')
       end if

       if (rank == 2) then
          out_var2d = 0.0
          where(limitS <= lats .and. lats <=limitN) out_var2d = var2d
       else if (rank == 3) then
          out_var3d = 0.0
          do i=1,size(var3d,3)
             where(limitS <= lats .and. lats <=limitN) out_var3d(:,:,i) = var3d(:,:,i)
          enddo
       end if

      __RETURN(__SUCCESS)
   end subroutine evaluate_zone_mask

   subroutine evaluate_box_mask(this,state,field,rc)
      class(StateMask), intent(inout) :: this
      type(ESMF_State), intent(inout) :: state
      type(ESMF_Field), intent(inout)   :: field
      integer, optional, intent(out) :: rc

      integer :: status

       integer :: i
       character(len=:), allocatable :: vartomask,strtmp
       real, pointer        :: out_var2d(:,:)
       real, pointer        :: out_var3d(:,:,:)
       real, pointer        :: var2d(:,:)
       real, pointer        :: var3d(:,:,:)
       real(REAL64), pointer :: lats(:,:)
       real(REAL64), pointer :: lons(:,:)
       real(REAL64)         :: limitS, limitN, limitE, limitW
       real(REAL64)         :: limitE1, limitW1
       real(REAL64)         :: limitE2, limitW2
       type(ESMF_Grid)      :: grid
       integer              :: rank,is,nargs
       integer              :: counts(3)
       logical              :: isCube, twoBox
       real, allocatable    :: temp2d(:,:)
       character(len=ESMF_MAXSTR) :: args(5)
       type(ESMF_CoordSys_Flag) :: coordSys

       call ESMF_FieldGet(field,rank=rank,grid=grid,__RC)
       call ESMF_GridGet(grid,coordsys=coordsys,__RC)

       strtmp = this%mask_arguments
       do nargs=1,5
          is = index(strtmp,',')
          if (is >0) then
             args(nargs) = strtmp(:is-1)
          else
             args(nargs) = strtmp
          end if
          strtmp = strtmp(is+1:)
       end do

       varToMask=args(1)

       READ(args(2),*,IOSTAT=status) limitS
       __VERIFY(status)
       READ(args(3),*,IOSTAT=status) limitN
       __VERIFY(status)
       READ(args(4),*,IOSTAT=status) limitW
       __VERIFY(status)
       READ(args(5),*,IOSTAT=status) limitE
       __VERIFY(status)
       __ASSERT(limitE > limitW,'LimitE must be greater than limitW')
       __ASSERT(limitE /= limitW,'LimitE cannot equal limitW')
       __ASSERT(limitN /= limitS,'LimitN cannot equal LimitS')
       __ASSERT((limitE-limitW)<=360.0d0,'(LimitE - LimitW) must be less than or equal to 360')

       call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
            staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=lons, rc=status)
       __VERIFY(status)
       call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
            staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=lats, rc=status)
       __VERIFY(status)

       ! do some tests if cube goes from 0 to 360, lat-lon -180 to 180
       call MAPL_GridGet(grid, globalCellCountPerDim=COUNTS,rc=status)
       __VERIFY(STATUS)
       if (counts(2)==6*counts(1)) then
          isCube=.true.
       else
          isCube=.false.
       end if
       twoBox = .false.
       if (isCube) then
          if (limitW < 0.0d0 .and. limitE >=0.0d0) then
             ! need two boxes
             twoBox=.true.
             limitW1=0.0d0
             limitE1=limitE
             limitW2=limitW+360.0d0
             limitE2=360.0d0

          else if (limitW <0.0d0 .and. limitE <0.0d0) then
             ! just shift
             limitW1=limitW+360.d0
             limitE1=limitE+360.d0

          else
             ! normal case
             limitW1=limitW
             limitE1=limitE
          end if

       else

          if (limitW  <= 180.0d0 .and. limitE > 180.0d0) then
             ! need two boxes
             twoBox=.true.
             limitW1=limitW
             limitE1=180.0d0
             limitW2=-180.d0
             limitE2=limitE-360.0d0
          else if (limitW > 180.0d0 .and. limitE > 180.0d0) then
             ! just shift
             limitW1=limitW-360.d0
             limitE1=limitE-360.d0
          else
             ! normal case
             limitW1=limitW
             limitE1=limitE
          end if

       end if
       if (coordSys == ESMF_COORDSYS_SPH_RAD) then
          limitE1=limitE1*MAPL_PI_R8/180.0d0
          limitW1=limitW1*MAPL_PI_R8/180.0d0
          if (twoBox) then
             limitE2=limitE2*MAPL_PI_R8/180.0d0
             limitW2=limitW2*MAPL_PI_R8/180.0d0
          end if

          limitN=limitN*MAPL_PI_R8/180.0d0
          limitS=limitS*MAPL_PI_R8/180.0d0
       end if

       if (rank == 2) then
          call ESMF_FieldGet(field,0,farrayPtr=out_var2d,__RC)
          call MAPL_GetPointer(state,var2d, vartomask, __RC)
       else if (rank == 3) then
          call ESMF_FieldGet(field,0,farrayPtr=out_var3d,__RC)
          call MAPL_GetPointer(state,var3d, vartomask, __RC)
       else
          __FAIL('Rank must be 2 or 3')
       end if

       if (rank == 2) then
          out_var2d = 0.0
          where(limitS <= lats .and. lats <=limitN .and. limitW1 <= lons .and. lons <= limitE1 ) out_var2d = var2d
       else if (rank == 3) then
          out_var3d = 0.0
          do i=1,size(var3d,3)
             where(limitS <= lats .and. lats <=limitN .and. limitW1 <= lons .and. lons <= limitE1 ) out_var3d(:,:,i) = var3d(:,:,i)
          enddo
       end if

       if (twoBox) then
          allocate(temp2d(size(var2d,1),size(var2d,2)),stat=status)
          __VERIFY(STATUS)
          if (rank == 2) then
             out_var2d = 0.0
             temp2d = 0.0
             where(limitS <= lats .and. lats <=limitN .and. limitW2 <= lons .and. lons <= limitE2 ) temp2d = var2d
             out_var2d=out_var2d+temp2d
          else if (rank == 3) then
             out_var3d = 0.0
             do i=1,size(var3d,3)
                temp2d = 0.0
                where(limitS <= lats .and. lats <=limitN .and. limitW2 <= lons .and. lons <= limitE2 ) temp2d = var3d(:,:,i)
                out_var3d(:,:,i)=out_var3d(:,:,i)+temp2d
             enddo
          end if
          deallocate(temp2d)
       end if

       __RETURN(__SUCCESS)
  end subroutine evaluate_box_mask

!------------------------------------------------------------------------------
!>
! Extract integers from a character-delimited string, for example, "-1,45,256,7,10".  In the context
! of Chem_Util, this is provided for determining the numerically indexed regions over which an
! emission might be applied.
!
! In multiple passes, the string is parsed for the delimiter, and the characters up to, but not
! including the delimiter are taken as consecutive digits of an integer.  A negative sign ("-") is
! allowed.  After the first pass, each integer and its trailing delimiter are lopped of the head of
! the (local copy of the) string, and the process is started over.
!
! The default delimiter is a comma (",").
!
! "Unfilled" iValues are zero.
!
! Return codes:
!1. Zero-length string.
!2. iSize needs to be increased.
!
!#### Assumptions/bugs:
!
! A non-zero return code does not stop execution.
! Allowed numerals are: 0,1,2,3,4,5,6,7,8,9.
! A delimiter must be separated from another delimiter by at least one numeral.
! The delimiter cannot be a numeral or a negative sign.
! The character following a negative sign must be an allowed numeral.
! The first character must be an allowed numeral or a negative sign.
! The last character must be an allowed numeral.
! The blank character (" ") cannot serve as a delimiter.
!
!#### Examples of strings that will work:
!```
!  "1"
!  "-1"
!  "-1,2004,-3"
!  "1+-2+3"
!  "-1A100A5"
!```
!
!#### Examples of strings that will not work:
!```
!  "1,--2,3"
!  "1,,2,3"
!  "1,A,3"
!  "1,-,2"
!  "1,2,3,4,"
!  "+1"
!  "1 3 6"
!```
!
  SUBROUTINE ExtDataExtractIntegers(string,iSize,iValues,delimiter,verbose,rc)

  IMPLICIT NONE

  CHARACTER(LEN=*), INTENT(IN)   :: string         !! Character-delimited string of integers
  INTEGER, INTENT(IN)            :: iSize
  INTEGER, INTENT(INOUT)         :: iValues(iSize) !! Space allocated for extracted integers
  CHARACTER(LEN=*), OPTIONAL     :: delimiter      !! 1-character delimiter
  LOGICAL, OPTIONAL, INTENT(IN)  :: verbose        !! Let me know iValues as they are found.
                                                   !! DEBUG directive turns on the message even
                                                   !! if verbose is not present or if
                                                   !! verbose = .FALSE.
  INTEGER, OPTIONAL, INTENT(OUT) :: rc             !! Return code

 INTEGER :: base,count,i,iDash,last,lenStr
 INTEGER :: multiplier,pos,posDelim,sign
 CHARACTER(LEN=255) :: str
 CHARACTER(LEN=1) :: char,delimChar
 LOGICAL :: Done
 LOGICAL :: tellMe

! Initializations
! ---------------
 count = 1
 Done = .FALSE.
 iValues(:) = 0
 base = ICHAR("0")
 iDash = ICHAR("-")

! Determine verbosity, letting the DEBUG
! directive override local specification
! --------------------------------------
  tellMe = .FALSE.
  IF(PRESENT(verbose)) THEN
   IF(verbose) tellMe = .TRUE.
 END IF
#ifdef DEBUG
  tellMe = .TRUE.
#endif
! Check for zero-length string
! ----------------------------
 lenStr = LEN_TRIM(string)
 IF(lenStr == 0) THEN
  __FAIL("ERROR - Found zero-length string.")
 END IF

! Default delimiter is a comma
! ----------------------------
 delimChar = ","
 IF(PRESENT(delimiter)) delimChar(1:1) = delimiter(1:1)

! Work on a local copy
! --------------------
 str = TRIM(string)

! One pass for each delimited integer
! -----------------------------------
 Parse: DO

  lenStr = LEN_TRIM(str)

! Parse the string for the delimiter
! ----------------------------------
  posDelim = INDEX(TRIM(str),TRIM(delimChar))

! If the delimiter does not exist,
! one integer remains to be extracted.
! ------------------------------------
  IF(posDelim == 0) THEN
   Done = .TRUE.
   last = lenStr
  ELSE
   last = posDelim-1
  END IF
  multiplier = 10**last

! Examine the characters of this integer
! --------------------------------------
  Extract: DO pos=1,last

   char = str(pos:pos)
   i = ICHAR(char)

! Account for a leading "-"
! -------------------------
   IF(pos == 1) THEN
    IF(i == iDash) THEN
     sign = -1
    ELSE
     sign = 1
    END IF
   END IF

! "Power" of 10 for this character
! --------------------------------
   multiplier = multiplier/10

   IF(pos == 1 .AND. sign == -1) CYCLE Extract

! Integer comes from remaining characters
! ---------------------------------------
   i = (i-base)*multiplier
   iValues(count) = iValues(count)+i
   IF(pos == last) THEN
    iValues(count) = iValues(count)*sign
   END IF

  END DO Extract
  IF(Done) EXIT

! Lop off the leading integer and try again
! -----------------------------------------
  str(1:lenStr-posDelim) = str(posDelim+1:lenStr)
  str(lenStr-posDelim+1:255) = " "
  count = count+1

! Check size
! ----------
  IF(count > iSize) THEN
   __FAIL("ERROR - iValues does not have enough elements.")
  END IF

 END DO Parse

 __RETURN(ESMF_SUCCESS)

 END SUBROUTINE ExtDataExtractIntegers
end module MAPL_StateMaskMod
