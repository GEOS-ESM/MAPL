
!  $Id$ 

#include "MAPL_ErrLog.h"

!BOP

! !MODULE: MAPL_Profiler -- A Module to instrument codes for profiling


! !INTERFACE:

  module MAPL_ProfMod

! !USES:

  use ESMF
  use MAPL_BaseMod
  use MAPL_IOMod
  use MAPL_CommsMod
  use MAPL_ErrorHandlingMod
#ifdef _CUDA
  use cudafor
#endif
  use, intrinsic :: ISO_FORTRAN_ENV, only: REAL64
  implicit none
  private

! !PUBLIC TYPES:

  type, public :: MAPL_Prof
    private
    character(len=ESMF_MAXSTR) :: NAME=""
    integer        :: START_TIME
    real  (kind=REAL64) :: CUMM_TIME   
  end type MAPL_Prof

! !PUBLIC MEMBER FUNCTIONS:

  public MAPL_ProfClockOn
  public MAPL_ProfClockOff
  public MAPL_ProfSet
  public MAPL_ProfDisable
  public MAPL_ProfEnable
  public MAPL_ProfWrite
  public MAPL_ProfIsDisabled
  public MAPL_TimerModeSet

!EOP

  integer, public, parameter  :: MAPL_TimerModeOld = 0
  integer, public, parameter  :: MAPL_TimerModeRootOnly = 1
  integer, public, parameter  :: MAPL_TimerModeMax = 2
  integer, public, parameter  :: MAPL_TimerModeMinMax = 3

  type(ESMF_VM), save :: VM
  integer,       save :: COUNT_MAX, COUNT_RATE
  real(kind=REAL64),  save :: CRI
  logical,       save :: FIRSTTIME = .true.
  logical,       save :: DISABLED  = .false.
  integer,       save :: timerMode = MAPL_TimerModeMax

  contains

!********************************************************
    logical function MAPL_ProfIsDisabled()
      MAPL_ProfIsDisabled = DISABLED

    end function MAPL_ProfIsDisabled

!********************************************************

    subroutine MAPL_ProfClockOn(TIMES, NAME, RC)
      type (MAPL_Prof),  intent(INOUT) :: TIMES(:)
      character(len=*),  intent(IN   ) :: NAME      
      integer, optional, intent(OUT  ) :: RC

      character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_ProfClockOn"

      integer :: I, NN
      integer :: status

      if(.not.DISABLED) then

         NN = size(TIMES)

         I=1
         do while (I<=NN)
            if(TIMES(I)%NAME==NAME) then
               exit
            endif
            I=I+1
         enddo

         if(I>NN) then
            print *, 'ERROR: Timer '//trim(NAME)//' needs to be set first'
            _RETURN(ESMF_FAILURE)
         end if
     
#ifdef _CUDA
         status = cudaDeviceSynchronize()
#endif
         if (timerMode == MAPL_TimerModeOld) then
            call ESMF_VMBarrier(VM, rc=status)
         end if
         call SYSTEM_CLOCK(TIMES(I)%START_TIME)  

      end if

      _RETURN(ESMF_SUCCESS)
    
    end subroutine MAPL_ProfClockOn

!********************************************************

    subroutine MAPL_ProfClockOff(TIMES, NAME, RC)
      type (MAPL_Prof),  intent(INOUT) :: TIMES(:)
      character(len=*),  intent(IN   ) :: NAME
      integer, optional, intent(OUT  ) :: RC

      character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_ProfClockOff"

      integer :: COUNTS
      integer :: I, NN
      integer :: status

      if(.not.DISABLED) then

         NN = size(TIMES)

         I=1
         do while (I<=NN)
            if(TIMES(I)%NAME==NAME) then
               exit
            endif
            I=I+1
         enddo

         if(I>NN) then
            print *, 'ERROR: Timer '//trim(NAME)//' needs to be set first'
            _RETURN(ESMF_FAILURE)
         end if

#ifdef _CUDA
         status = cudaDeviceSynchronize()
#endif
         if (timerMode == MAPL_TimerModeOld) then
            call ESMF_VMBarrier(VM, rc=status)
         end if
         call SYSTEM_CLOCK(COUNTS)

         COUNTS = COUNTS-TIMES(I)%START_TIME

         if(COUNTS<0) then
            COUNTS = COUNTS + COUNT_MAX
         endif

         TIMES(I)%CUMM_TIME = TIMES(I)%CUMM_TIME + real(COUNTS,kind=REAL64)*CRI

      end if

      _RETURN(ESMF_SUCCESS)

      
    end subroutine MAPL_ProfClockOff

!********************************************************

    subroutine MAPL_ProfSet(TIMES, NAME, RC)
      type (MAPL_Prof),  pointer       :: TIMES(:)
      character(len=*),  intent(IN   ) :: NAME
      integer, optional, intent(OUT  ) :: RC

      character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_ProfSet"
      type (MAPL_Prof), pointer :: TMP(:)
      integer :: I, STATUS

      if (FIRSTTIME) then
         FIRSTTIME = .false.
         call ESMF_VMGetCurrent(VM, rc=STATUS)
         _VERIFY(STATUS)
         call SYSTEM_CLOCK(COUNT_RATE=COUNT_RATE,COUNT_MAX=COUNT_MAX)
         CRI = 1.d0/real(COUNT_RATE,kind=REAL64)
      end if

      if (.not.associated(TIMES)) then
         I = 0
      else
         I = size(TIMES)
      endif

      allocate(TMP(I+1))
      if(associated(TIMES)) then
         TMP(1:I) = TIMES
         deallocate(TIMES)
      endif
      TMP(I+1) = MAPL_Prof(trim(NAME),0,0.D0)
      TIMES => TMP

      _RETURN(ESMF_SUCCESS)
      
    end subroutine MAPL_ProfSet

!********************************************************

    subroutine MAPL_ProfWrite(TIMES, RC)
      type (MAPL_Prof),  pointer       :: TIMES(:)
      integer, optional, intent(OUT)   :: RC

      character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_ProfWrite"
      integer :: status

      integer :: I
      integer :: N
      logical :: amIroot
      logical :: writing
      real(kind=REAL64), allocatable :: MAX_CUMM_TIME(:)  
      real(kind=REAL64), allocatable :: MIN_CUMM_TIME(:) 
      real(kind=REAL64), allocatable :: MEAN_CUMM_TIME(:) 
      real(kind=REAL64), allocatable :: TEMP_TIME(:)
      integer :: nPet 

      amIroot = MAPL_AM_I_Root(vm)
      call ESMF_VMGet(VM,petCount=nPet,rc=status)
      _VERIFY(STATUS)

!ALT: Currently, only root PE writes the Prof report
!     If we adopt other modes, we need to change next line
      writing = amIroot

      if (associated(TIMES)) then
         if (timerMode == MAPL_TimerModeMax .or. timerMode == MAPL_TimerModeMinMax) then
            N=size(TIMES)
            allocate(MAX_CUMM_TIME(N), stat=status, source=0.0_REAL64)
            allocate(TEMP_TIME(N), stat=status, source=0.0_REAL64)
            _VERIFY(status)

            TEMP_TIME = TIMES(:)%CUMM_TIME
            call ESMF_VmReduce(vm, sendData=TEMP_TIME, &
                 recvData=MAX_CUMM_TIME, count=size(TIMES), &
                 reduceFlag=ESMF_Reduce_Max, RootPet=MAPL_Root, RC=status)
            _VERIFY(STATUS)
            if (timerMode == MAPL_TimerModeMinMax) then
               allocate(MIN_CUMM_TIME(N), stat=status, source=0.0_REAL64)
               allocate(MEAN_CUMM_TIME(N), stat=status, source=0.0_REAL64)
               call ESMF_VmReduce(vm, sendData=TEMP_TIME, &
                    recvData=MIN_CUMM_TIME, count=size(TIMES), &
                    reduceFlag=ESMF_Reduce_Min, RootPet=MAPL_Root, RC=status)
               _VERIFY(STATUS)
               call ESMF_VmReduce(vm, sendData=TEMP_TIME, &
                    recvData=MEAN_CUMM_TIME, count=size(TIMES), &
                    reduceFlag=ESMF_Reduce_Sum, RootPet=MAPL_Root, RC=status)
               _VERIFY(STATUS)
               MEAN_CUMM_TIME=MEAN_CUMM_TIME/real(nPet)
            end if
            DEALLOCATE(TEMP_TIME)
         end if

         if (writing .and. (timerMode == MAPL_TimerModeRootOnly .or. timerMode == MAPL_TimerModeOld) ) then
            ! We do the loop twice to make sure TOTAL is reported first
            do I=1,size(TIMES)
               if(trim(TIMES(I)%name)=='TOTAL') then
                  WRITE(*,'("'//TIMES(I)%NAME(1:24)//':",F12.3)') TIMES(I)%CUMM_TIME
                  exit
               end if
            enddo
            do I=1,size(TIMES)
               if(trim(TIMES(I)%name)=='TOTAL') then
                  cycle
               else
                  WRITE(*,'("'//TIMES(I)%NAME(1:24)//':",F12.3)') TIMES(I)%CUMM_TIME
               end if
            enddo
         else if (writing .and. timerMode == MAPL_TimerModeMax) then
            ! We do the loop twice to make sure TOTAL is reported first
            do I=1,size(TIMES)
               if(trim(TIMES(I)%name)=='TOTAL') then
                  WRITE(*,'("'//TIMES(I)%NAME(1:24)//':",F12.3)') MAX_CUMM_TIME(I)
                  exit
               end if
            enddo
            do I=1,size(TIMES)
               if(trim(TIMES(I)%name)=='TOTAL') then
                  cycle
               else
                  WRITE(*,'("'//TIMES(I)%NAME(1:24)//':",F12.3)') MAX_CUMM_TIME(I)
               end if
            enddo
         else if (writing .and. timerMode == MAPL_TimerModeMinMax) then
            ! We do the loop twice to make sure TOTAL is reported first
            do I=1,size(TIMES)
               if(trim(TIMES(I)%name)=='TOTAL') then
                  WRITE(*,'("'//TIMES(I)%NAME(1:24)//':",F12.3,F12.3,F12.3)') MIN_CUMM_TIME(I),MEAN_CUMM_TIME(I), &
                       MAX_CUMM_TIME(I)
                  exit
               end if
            enddo
            do I=1,size(TIMES)
               if(trim(TIMES(I)%name)=='TOTAL') then
                  cycle
               else
                  WRITE(*,'("'//TIMES(I)%NAME(1:24)//':",F12.3,F12.3,F12.3)') MIN_CUMM_TIME(I),MEAN_CUMM_TIME(I), &
                       MAX_CUMM_TIME(I)
               end if
            enddo
         end if
          

         if (timerMode == MAPL_TimerModeMax .or. timerMode == MAPL_TimerModeMinMax) then
            deallocate(MAX_CUMM_TIME)
            if (timerMode == MAPL_TimerModeMinMax) deallocate(MIN_CUMM_TIME)
            if (timerMode == MAPL_TimerModeMinMax) deallocate(MEAN_CUMM_TIME)
         end if
      end if

      _RETURN(ESMF_SUCCESS)
      
    end subroutine MAPL_ProfWrite

!********************************************************

    subroutine MAPL_ProfDisable(RC)
      integer, optional, intent(OUT)   :: RC

      character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_ProfDisable"

      DISABLED = .true.

      _RETURN(ESMF_SUCCESS)
      
    end subroutine MAPL_ProfDisable

!********************************************************

    subroutine MAPL_ProfEnable(RC)
      integer, optional, intent(OUT)   :: RC

      character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_ProfEnable"

      DISABLED = .false.

      _RETURN(ESMF_SUCCESS)
      
    end subroutine MAPL_ProfEnable

!********************************************************

    subroutine MAPL_TimerModeSet(MODE, RC)
      integer,           intent(IN )   :: MODE
      integer, optional, intent(OUT)   :: RC

      character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_ProfModeSet"

      ! Sanity check
      _ASSERT(timerMode >= MAPL_TimerModeOld .and. timerMode <= MAPL_TimerModeMax,'needs informative message')
      timerMode = mode

      _RETURN(ESMF_SUCCESS)
      
    end subroutine MAPL_TimerModeSet

!********************************************************


  end module MAPL_ProfMod

