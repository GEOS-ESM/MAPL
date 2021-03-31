
#include "MAPL_Generic.h"
#include "unused_dummy.H"

!=============================================================================
!BOP

! !MODULE: MAPL_GenericCplCompMod

! !DESCRIPTION:
!
!  This is a generic coupler component used by \ggn\ to instantiate 
!  the automatic couplers it needs.
!  \newline

! !INTERFACE:

module MAPL_GenericCplCompMod

! !USES:

  use ESMF
  use ESMFL_Mod
  use MAPL_BaseMod
  use MAPL_ConstantsMod
  use MAPL_IOMod
  use MAPL_CommsMod
  use MAPL_ProfMod
  use MAPL_SunMod
  use MAPL_VarSpecMod
  use MAPL_ExceptionHandling

  implicit none
  private

! !PUBLIC MEMBER FUNCTIONS:

  public GenericCplSetServices
  public MAPL_CplCompSetVarSpecs
  public MAPL_CplCompSetAlarm

!EOP

  type MAPL_CplCnt
     integer, pointer  :: PTR1C(:)     => null()
     integer, pointer  :: PTR2C(:,:)   => null()
     integer, pointer  :: PTR3C(:,:,:) => null()
  end type MAPL_CplCnt

  type MAPL_GenericCplState
     private
! These are done in set services
     type (ESMF_Config)           :: CF
     logical                      :: ACTIVE
     type (ESMF_Alarm), pointer   :: TIME2CPL_ALARM => null()
     character(LEN=ESMF_MAXSTR)   :: NAME
     type (MAPL_VarSpec), pointer :: SRC_SPEC(:) => null()
     type (MAPL_VarSpec), pointer :: DST_SPEC(:) => null()
! These are done in init
     integer          , pointer   :: CLEAR_INTERVAL(:)
     integer          , pointer   :: COUPLE_INTERVAL(:)
     type (ESMF_Alarm), pointer   :: TIME_TO_CLEAR(:)
     type (ESMF_Alarm), pointer   :: TIME_TO_COUPLE(:)
     type (ESMF_LocalArray), pointer :: ACCUMULATORS(:)
     type(MAPL_CplCnt), pointer   :: ARRAY_COUNT(:) => null()
     integer          , pointer   :: ACCUM_COUNT(:)
     integer          , pointer   :: ACCUM_RANK(:)
     integer          , pointer   :: couplerType(:) => null()
  end type MAPL_GenericCplState

  type MAPL_GenericCplWrap
     type (MAPL_GenericCplState), pointer :: INTERNAL_STATE
  end type MAPL_GenericCplWrap

contains

!=============================================================================
!=============================================================================
!=============================================================================

!BOPI

! !IROUTINE: GenericCplSetServices

! !DESCRIPTION: \ssv\  for generic couplers.

! !INTERFACE:

  subroutine GenericCplSetServices ( CC, RC )

! !ARGUMENTS:

    type (ESMF_CplComp  )                          :: CC  
    integer,                         intent(  OUT) :: RC
    
!EOPI

! ErrLog Variables

    character(len=ESMF_MAXSTR)    :: IAm
    character(len=ESMF_MAXSTR)    :: COMP_NAME
    integer                       :: STATUS

! Locals

    type (MAPL_GenericCplState), pointer :: STATE
    type (MAPL_GenericCplWrap )          :: CCWRAP

! Begin...

! Get this instance's name and set-up traceback handle.
! -----------------------------------------------------

    call ESMF_CplCompGet( CC, name=COMP_NAME, RC=STATUS )
    _VERIFY(STATUS)
    Iam = trim(COMP_NAME) // "GenericCplSetServices"

! Allocate this instance of the internal state and put it in wrapper.
! -------------------------------------------------------------------

    allocate(STATE, STAT=STATUS)
    _VERIFY(STATUS)

    CCWRAP%INTERNAL_STATE => STATE

! Have ESMF save pointer to the wrapped internal state in the C.C.
! ----------------------------------------------------------------

    call ESMF_CplCompSetInternalState(CC, CCWRAP, STATUS)
    _VERIFY(STATUS)

! Register services for this component
! ------------------------------------

    call ESMF_CplCompSetEntryPoint ( CC, ESMF_METHOD_INITIALIZE,  Initialize, &
                                     rc=STATUS )
    _VERIFY(STATUS)

    call ESMF_CplCompSetEntryPoint ( CC, ESMF_METHOD_RUN,   Run,        &
                                     rc=STATUS )
    _VERIFY(STATUS)

    call ESMF_CplCompSetEntryPoint ( CC, ESMF_METHOD_FINALIZE, Finalize,   &
                                     rc=STATUS )
    _VERIFY(STATUS)

!ALT: Add these 2 IO methods to facilitate transparent checkpointing
!     to support monthly averages
    call ESMF_CplCompSetEntryPoint ( CC, ESMF_METHOD_READRESTART, ReadRestart,   &
                                     rc=STATUS )
    _VERIFY(STATUS)
    call ESMF_CplCompSetEntryPoint ( CC, ESMF_METHOD_WRITERESTART, WriteRestart,   &
                                     rc=STATUS )
    _VERIFY(STATUS)
! Put the inherited configuration in the internal state
! -----------------------------------------------------

!ALT not_used    call ESMF_CplCompGet( CC, CONFIG=STATE%CF, RC=STATUS )
!ALT not_used    _VERIFY(STATUS)

    _RETURN(ESMF_SUCCESS)
  end subroutine GenericCplSetServices

  subroutine MAPL_CplCompSetVarSpecs ( CC, SRC_SPEC, DST_SPEC, RC )
    type (ESMF_CplComp  ),           intent(INOUT) :: CC  
    type (MAPL_VarSpec  ), target,   intent(IN   ) :: SRC_SPEC(:)
    type (MAPL_VarSpec  ), target,   intent(IN   ) :: DST_SPEC(:)
    integer, optional,               intent(  OUT) :: RC
    
! ErrLog Variables

    character(len=ESMF_MAXSTR)    :: IAm
    character(len=ESMF_MAXSTR)    :: COMP_NAME
    integer                       :: STATUS

! Locals

    type (MAPL_GenericCplState), pointer :: STATE
    type (MAPL_GenericCplWrap )          :: WRAP
    character(len=ESMF_MAXSTR)           :: SRC_NAME
    character(len=ESMF_MAXSTR)           :: DST_NAME

    integer                              :: I

! Begin...

! Get this instance's name and set-up traceback handle.
! -----------------------------------------------------

    call ESMF_CplCompGet( CC, name=COMP_NAME, RC=STATUS )
    _VERIFY(STATUS)
    Iam = trim(COMP_NAME) // "MAPL_CplCompSetVarSpecs"

! Retrieve the pointer to the internal state. It comes in a wrapper.
! ------------------------------------------------------------------

    call ESMF_CplCompGetInternalState ( CC, WRAP, STATUS )
    _VERIFY(STATUS)

    STATE  =>  WRAP%INTERNAL_STATE

! Make sure the specs match
!--------------------------

    _ASSERT(size(SRC_SPEC)==size(DST_SPEC),'needs informative message')

    do I=1,size(SRC_SPEC)
       call MAPL_VarSpecGet(SRC_SPEC(I),SHORT_NAME=SRC_NAME,RC=STATUS)
       _VERIFY(STATUS)
       call MAPL_VarSpecGet(DST_SPEC(I),SHORT_NAME=DST_NAME,RC=STATUS)
       _VERIFY(STATUS)

!ALT       _ASSERT(SRC_NAME==DST_NAME,'needs informative message')
    end do

! Put miscellaneous info in the internal state
!---------------------------------------------

    STATE%SRC_SPEC => SRC_SPEC
    STATE%DST_SPEC => DST_SPEC

    STATE%ACTIVE = .true.
    STATE%NAME   = COMP_NAME

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_CplCompSetVarSpecs

!=============================================================================

!BOPI

! !IROUTINE: INITIALIZE

! !DESCRIPTION: Initialize method for generic couplers.

! !INTERFACE:

  subroutine Initialize(CC, SRC, DST, CLOCK, RC)

! !ARGUMENTS:

    type (ESMF_CplComp)      :: CC
    type (ESMF_State)        :: SRC
    type (ESMF_State)        :: DST
    type (ESMF_Clock)        :: CLOCK
    integer, intent(  OUT)   :: RC

!EOPI

! ErrLog Variables

    character(len=ESMF_MAXSTR)    :: IAm
    character(len=ESMF_MAXSTR)    :: COMP_NAME
    integer                       :: STATUS

! Locals

    character (len=ESMF_MAXSTR)           :: NAME
    type (MAPL_GenericCplState), pointer  :: STATE
    type (MAPL_GenericCplWrap )           :: WRAP
    type (ESMF_TimeInterval   )           :: TCPL
    type (ESMF_TimeInterval   )           :: TCLR
    type (ESMF_TimeInterval   )           :: TS
    type (ESMF_TimeInterval   )           :: TOFF ! offset for alarms
    type (ESMF_Time           )           :: TM0 
    type (ESMF_Time           )           :: currTime ! current time of the clock
    type (ESMF_Time           )           :: rTime 
    type (ESMF_Calendar       )           :: cal
    integer                               :: J, L1, LN
    integer                               :: NCPLS
    integer                               :: DIMS
    real, pointer                         :: PTR1 (:    )
    real, pointer                         :: PTR2 (:,:  )
    real, pointer                         :: PTR3 (:,:,:)
    real, pointer                         :: PTR10(:    )
    real, pointer                         :: PTR20(:,:  )
    real, pointer                         :: PTR30(:,:,:)
    integer                               :: OFFSET
    integer, pointer                      :: ungrd(:)
    logical                               :: has_ungrd
    type(ESMF_Field)                      :: field
    integer                               :: cplfunc
    logical                               :: isPresent

! Begin...

    _UNUSED_DUMMY(SRC)
    _UNUSED_DUMMY(DST)
    _UNUSED_DUMMY(CLOCK)

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

    Iam = "MAPL_GenericCplCompInitialize"
    call ESMF_CplCompGet( CC, NAME=COMP_NAME, RC=STATUS )
    _VERIFY(STATUS)
    Iam = trim(COMP_NAME) // Iam

! Retrieve the pointer to the internal state. It comes in a wrapper.
! ------------------------------------------------------------------

    call ESMF_CplCompGetInternalState ( CC, WRAP, STATUS )
    _VERIFY(STATUS)

    STATE  =>  WRAP%INTERNAL_STATE

! The number of couplings for this pair
!--------------------------------------

    NCPLS = size(STATE%DST_SPEC)

    _ASSERT(NCPLS == size(STATE%SRC_SPEC),'needs informative message')

! Allocate arrays of ESMF arrays for accumulators
!-------------------------------------------------

    allocate(STATE%ACCUMULATORS(NCPLS), STAT=STATUS)
    _VERIFY(STATUS)
    allocate(STATE%ARRAY_COUNT (NCPLS), STAT=STATUS)
    _VERIFY(STATUS)
    allocate(STATE%ACCUM_RANK (NCPLS), STAT=STATUS)
    _VERIFY(STATUS)
    allocate(STATE%couplerType(NCPLS), STAT=STATUS)
    _VERIFY(STATUS)

! Allocate internal state objects
! -------------------------------

    allocate (STATE%CLEAR_INTERVAL (NCPLS), stat=status)
    _VERIFY(STATUS)
    allocate (STATE%COUPLE_INTERVAL(NCPLS), stat=status)
    _VERIFY(STATUS)
    allocate (STATE%TIME_TO_CLEAR  (NCPLS), stat=status)
    _VERIFY(STATUS)
    allocate (STATE%TIME_TO_COUPLE (NCPLS), stat=status)
    _VERIFY(STATUS)
    allocate(STATE%ACCUM_COUNT     (NCPLS), stat=status)
    _VERIFY(STATUS)

! Get clock info
!---------------

    call ESMF_ClockGet(CLOCK, calendar=cal, currTime=currTime, timeStep=TS, rc=STATUS)
    _VERIFY(STATUS)

    TM0 = currTime

! Initialize the counters to 0. This may do some unnecessary
!   accumulations immediately after initialize
!-----------------------------------------------------------

    STATE%ACCUM_COUNT = 0

    DO J = 1, NCPLS

! Get info from the DST spec
!---------------------------

       call MAPL_VarSpecGet(STATE%DST_SPEC(J),        &
            ACCMLT_INTERVAL = STATE%CLEAR_INTERVAL(J), &
            COUPLE_INTERVAL = STATE%COUPLE_INTERVAL(J), &
            OFFSET          = OFFSET, &
            SHORT_NAME      = NAME, &
                                            RC = STATUS )
       _VERIFY(STATUS)

! Initalize COUPLE ALARM from destination properties
!---------------------------------------------------

       call ESMF_TimeIntervalSet(TCPL, S=STATE%COUPLE_INTERVAL(J), &
            calendar=cal, RC=STATUS)
       _VERIFY(STATUS)

       call ESMF_TimeIntervalSet(TOFF, S=OFFSET, &
            calendar=cal, RC=STATUS)
       _VERIFY(STATUS)

       rTime = TM0 + TOFF

       if (associated(STATE%TIME2CPL_ALARM)) then
          STATE%TIME_TO_COUPLE(J) = STATE%TIME2CPL_ALARM
          STATE%TIME_TO_CLEAR(J) = STATE%TIME2CPL_ALARM
       else
          STATE%TIME_TO_COUPLE(J) = ESMF_AlarmCreate(NAME='TIME2COUPLE_' // trim(COMP_NAME) &
               // '_' // trim(NAME),   &
               clock        = CLOCK,   &
               ringInterval = TCPL,    &
               ringTime     = rTime,   &
               sticky       = .false., &
               rc=STATUS   )
          _VERIFY(STATUS)

          if(rTime == currTime) then
             call ESMF_AlarmRingerOn(STATE%TIME_TO_COUPLE(J), rc=status); _VERIFY(STATUS)
          end if

! initalize CLEAR ALARM from destination properties
!--------------------------------------------------

          call ESMF_TimeIntervalSet(TCLR, S=STATE%CLEAR_INTERVAL(J), &
               calendar=cal, RC=STATUS)
          _VERIFY(STATUS)

          if (TCLR < TS) TCLR = TS

          rTime = TM0 + TOFF - TCLR

          do while (rTime < currTime) 
             rTime = rTime + TCPL
          end do

          STATE%TIME_TO_CLEAR(J) = ESMF_AlarmCreate(NAME='TIME2CLEAR_' // trim(COMP_NAME) &
               // '_' // trim(NAME),   &
               clock        = CLOCK,   &
               ringInterval = TCPL,    & 
               ringTime     = rTime,   &
               sticky       = .false., &
               rc=STATUS   )
          _VERIFY(STATUS)

          if(rTime == currTime) then
             call ESMF_AlarmRingerOn(STATE%TIME_TO_CLEAR(J), rc=status); _VERIFY(STATUS)
          end if
       end if

! Get info from the SRC spec
!---------------------------

       call MAPL_VarSpecGet(STATE%SRC_SPEC(J),      &
                            DIMS       = DIMS,      &
                            SHORT_NAME = NAME,      &
                            UNGRIDDED_DIMS=UNGRD,   &
                                           RC=STATUS)
       _VERIFY(STATUS)

! We currently make these d1mension assumptions
!----------------------------------------------

       if(DIMS==MAPL_DIMSHORZVERT) then
          DIMS=3
       elseif(DIMS==MAPL_DIMSHORZONLY .or. DIMS==MAPL_DIMSTILETILE) then
          DIMS=2
       elseif(DIMS==MAPL_DIMSVERTONLY .or. DIMS==MAPL_DIMSTILEONLY) then
          DIMS=1
       else
          _RETURN(ESMF_FAILURE)
       end if


       has_ungrd = associated(UNGRD)
       if (has_ungrd) then
          DIMS = DIMS + size(UNGRD)
       end if
       _ASSERT(DIMS < 4,'needs informative message') ! ALT: due to laziness we are supporting only 3 dims

       STATE%ACCUM_RANK(J) = DIMS

       call ESMF_StateGet(src, NAME, field, rc=status)
       _VERIFY(STATUS)
       call ESMF_AttributeGet(field, NAME="CPLFUNC", isPresent=isPresent, RC=STATUS)
       _VERIFY(STATUS)
       if (isPresent) then
          call ESMF_AttributeGet(field, NAME="CPLFUNC", VALUE=cplfunc, RC=STATUS)
          _VERIFY(STATUS)
       else
          cplfunc = MAPL_CplAverage
       end if
       state%couplerType(J) = cplfunc

! Create Accumulators for 3 dimensions
!-------------------------------------

       select case(DIMS)

       case(3)
! Get SRC pointer, making sure it is allocated.
          call MAPL_GetPointer(SRC, PTR3, NAME, ALLOC=.TRUE., RC=STATUS)
          _VERIFY(STATUS)
! Allocate space for accumulator
          L1 = LBOUND(PTR3,3)
          LN = UBOUND(PTR3,3)
          allocate(PTR30(size(PTR3,1),size(PTR3,2),L1:LN), STAT=STATUS)
          _VERIFY(STATUS)
          if (STATE%couplerType(J) /= MAPL_CplAverage .and. STATE%couplerType(J) /= MAPL_CplAccumulate) then
             PTR30 = MAPL_UNDEF
          else
! Set accumulator values to zero
             PTR30 = 0.0
          endif
! Put pointer in accumulator
          STATE%ACCUMULATORS(J)=ESMF_LocalArrayCreate( PTR30, RC=STATUS)
          _VERIFY(STATUS)
          
       case(2)
          call MAPL_GetPointer(SRC, PTR2, NAME, ALLOC=.TRUE., RC=STATUS)
          _VERIFY(STATUS)
          allocate(PTR20(size(PTR2,1),size(PTR2,2)), STAT=STATUS)
          _VERIFY(STATUS)
          if (STATE%couplerType(J) /= MAPL_CplAverage .and. STATE%couplerType(J) /= MAPL_CplAccumulate) then
             PTR20 = MAPL_UNDEF
          else
             PTR20 = 0.0
          end if
          STATE%ACCUMULATORS(J)=ESMF_LocalArrayCreate( PTR20, RC=STATUS)
          _VERIFY(STATUS)

       case(1)
          call MAPL_GetPointer(SRC, PTR1, NAME, ALLOC=.TRUE., RC=STATUS)
          _VERIFY(STATUS)
          allocate(PTR10(size(PTR1)), STAT=STATUS)
          _VERIFY(STATUS)
          if (STATE%couplerType(J) /= MAPL_CplAverage .and. STATE%couplerType(J) /= MAPL_CplAccumulate) then
             PTR10 = MAPL_UNDEF
          else
             PTR10 = 0.0
          end if
          STATE%ACCUMULATORS(J)=ESMF_LocalArrayCreate( PTR10, RC=STATUS)
          _VERIFY(STATUS)

       case default
          _RETURN(ESMF_FAILURE)

       end select
    end do

    _RETURN(ESMF_SUCCESS)

  end subroutine Initialize


!BOPI

! !IROUTINE: RUN

! !DESCRIPTION: {Run method for the generic coupler.}

! !INTERFACE:

  subroutine Run(CC, SRC, DST, CLOCK, RC)
    
! !ARGUMENTS:

    type (ESMF_CplComp)      :: CC
    type (ESMF_State)        :: SRC
    type (ESMF_State)        :: DST
    type (ESMF_Clock)        :: CLOCK
    integer, intent(  OUT)   :: RC

!EOPI

! ErrLog Variables

    character(len=ESMF_MAXSTR)    :: IAm
    character(len=ESMF_MAXSTR)    :: COMP_NAME
    integer                       :: STATUS

! Locals

    type (MAPL_GenericCplState), pointer  :: STATE
    type (MAPL_GenericCplWrap )           :: WRAP

! Begin...

    _UNUSED_DUMMY(CLOCK)

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

    Iam = "MAPL_GenericCplCompRun"
    call ESMF_CplCompGet( CC, NAME=COMP_NAME, RC=STATUS )
    _VERIFY(STATUS)
    Iam = trim(COMP_NAME) // Iam

! Retrieve the pointer to the internal state. It comes in a wrapper.
! ------------------------------------------------------------------

    call ESMF_CplCompGetInternalState ( CC, WRAP, STATUS )
    _VERIFY(STATUS)

    STATE     =>  WRAP%INTERNAL_STATE

! If the state is inactive, src and dst are the same
! --------------------------------------------------
    
    if(STATE%ACTIVE) then

! Make sure SRC and DST descriptors exist 
!----------------------------------------

       _ASSERT(associated(STATE%SRC_SPEC),'needs informative message')
       _ASSERT(associated(STATE%DST_SPEC),'needs informative message')

! Update accumulators on SRC grid.
!---------------------------------

       call  ACCUMULATE(SRC, STATE, RC=STATUS)
       _VERIFY(STATUS)

! Periodically transfer accumulators to DST arrays
!-------------------------------------------------

       call  COUPLE    (DST, STATE, RC=STATUS)
       _VERIFY(STATUS)

! Zero accumulators when next averaging interval starts
!------------------------------------------------------

       call ZERO_CLEAR_COUNT(STATE, RC=STATUS)
       _VERIFY(STATUS)
    end if

    _RETURN(ESMF_SUCCESS)

  contains

  subroutine ACCUMULATE(SRC, STATE, RC)
    type (ESMF_State)           :: SRC
    type (MAPL_GenericCplState) :: STATE
    integer, optional           :: RC 

! local vars

    integer                               :: J
    integer                               :: I1, I2, I3
    integer                               :: couplerType
    character (len=ESMF_MAXSTR)           :: NAME
    integer                               :: DIMS
    real, pointer                         :: PTR1 (:)
    real, pointer                         :: PTR2 (:,:)
    real, pointer                         :: PTR3 (:,:,:)
    real, pointer                         :: PTR10(:)
    real, pointer                         :: PTR20(:,:)
    real, pointer                         :: PTR30(:,:,:)
    integer, pointer                      :: PTR1c(:)     => NULL()
    integer, pointer                      :: PTR2c(:,:)   => NULL()
    integer, pointer                      :: PTR3c(:,:,:) => NULL()

    character(*), parameter       :: IAm="ACCUMULATE"
    integer                       :: STATUS

    do J = 1, size(STATE%SRC_SPEC)
       couplerType = state%couplerType(J)

! Accumulate only if we are in the couplings averaging interval
!--------------------------------------------------------------
       if(STATE%ACCUM_COUNT(J) < 0) cycle

! Get info from the SRC spec
!---------------------------

       call MAPL_VarSpecGet(STATE%SRC_SPEC(J), SHORT_NAME=NAME, RC=STATUS)
       _VERIFY(STATUS)

       DIMS = STATE%ACCUM_RANK(J)

! Process the 3 dimensions
!------------------------- 

       select case(DIMS)

       case(3)
          call MAPL_GetPointer  (SRC, PTR3, NAME,            RC=STATUS)
          _VERIFY(STATUS)
          call ESMF_LocalArrayGet(STATE%ACCUMULATORS(J),farrayPtr=PTR30,RC=STATUS)
          _VERIFY(STATUS)
          PTR3c => STATE%ARRAY_COUNT(J)%PTR3C

          if(.not.associated(PTR3C)) then
             if(  any( PTR3==MAPL_UNDEF ) ) then
                allocate(PTR3C(size(PTR3,1), size(PTR3,2), size(PTR3,3)),STAT=STATUS)
                _VERIFY(STATUS)
                PTR3C = STATE%ACCUM_COUNT(J)
!               put it back into array
                STATE%ARRAY_COUNT(J)%PTR3C => PTR3c
                _VERIFY(STATUS)
             end if
          end if

          if (couplerType == MAPL_CplAverage .or. couplerType == MAPL_CplAccumulate) then
             if(associated(PTR3C)) then
                where (PTR3 /= MAPL_Undef)
                   PTR30 = PTR30 + PTR3
                   PTR3c = PTR3c + 1
                end where
             else
                PTR30 = PTR30 + PTR3
             end if
          else
             DO I1=1,size(PTR3,1)
                DO I2=1,size(PTR3,2)
                   DO I3=1,size(PTR3,3)
                      if (PTR30(I1,I2,I3)== MAPL_Undef) then
                         PTR30(I1,I2,I3) = PTR3(I1,I2,I3)
                      else 
                         if (couplerType == MAPL_CplMax) then
                            PTR30(I1,I2,I3) = max(PTR30(I1,I2,I3),PTR3(I1,I2,I3))
                         else if (couplerType == MAPL_CplMin) then
                            PTR30(I1,I2,I3) = min(PTR30(I1,I2,I3),PTR3(I1,I2,I3))
                         end if
                      end if
                   end DO
                end DO
             end DO
          end if

       case(2)
          call MAPL_GetPointer  (SRC, PTR2, NAME,            RC=STATUS)
          _VERIFY(STATUS)
          call ESMF_LocalArrayGet(STATE%ACCUMULATORS(J),farrayPtr=PTR20,RC=STATUS)
          _VERIFY(STATUS)
          PTR2c => STATE%ARRAY_COUNT(J)%PTR2C

          if(.not.associated(PTR2C)) then
             if(  any( PTR2==MAPL_UNDEF ) ) then
                allocate(PTR2C(size(PTR2,1), size(PTR2,2)), STAT=STATUS)
                _VERIFY(STATUS)
                PTR2C = STATE%ACCUM_COUNT(J)
!               put it back into array
                STATE%ARRAY_COUNT(J)%PTR2C => PTR2c
                _VERIFY(STATUS)
             end if
          end if

          if (couplerType == MAPL_CplAverage .or. couplerType == MAPL_CplAccumulate) then
             if(associated(PTR2C)) then
                where (PTR2 /= MAPL_Undef)
                   PTR20 = PTR20 + PTR2
                   PTR2c = PTR2c + 1
                end where
             else
                PTR20 = PTR20 + PTR2
             end if
          else
             DO I1=1,size(PTR2,1)
                DO I2=1,size(PTR2,2)
                   if (PTR20(I1,I2)== MAPL_Undef) then
                      PTR20(I1,I2) = PTR2(I1,I2)
                   else 
                      if (couplerType == MAPL_CplMax) then
                         PTR20(I1,I2) = max(PTR20(I1,I2),PTR2(I1,I2))
                      else if (couplerType == MAPL_CplMin) then
                         PTR20(I1,I2) = min(PTR20(I1,I2),PTR2(I1,I2))
                      end if
                   end if
                end DO
             end DO
          endif

       case(1)
          call MAPL_GetPointer  (SRC, PTR1, NAME,            RC=STATUS)
          _VERIFY(STATUS)
          call ESMF_LocalArrayGet(STATE%ACCUMULATORS(J),farrayPtr=PTR10,RC=STATUS)
          _VERIFY(STATUS)
          PTR1c => STATE%ARRAY_COUNT(J)%PTR1C

          if(.not.associated(PTR1C)) then
             if(  any( PTR1==MAPL_UNDEF ) ) then
                allocate(PTR1C(size(PTR1,1)), STAT=STATUS)
                _VERIFY(STATUS)
                PTR1C = STATE%ACCUM_COUNT(J)
!               put it back into array
                STATE%ARRAY_COUNT(J)%PTR1C => PTR1c
                _VERIFY(STATUS)
             end if
          end if

          if (couplerType == MAPL_CplAverage .or. couplerType == MAPL_CplAccumulate) then
             if(associated(PTR1C)) then
                where (PTR1 /= MAPL_Undef)
                   PTR10 = PTR10 + PTR1
                   PTR1c = PTR1c + 1
                end where
             else
                PTR10 = PTR10 + PTR1
             end if
          else
             DO I1=1,size(PTR1,1)
                if (PTR10(I1)== MAPL_Undef) then
                   PTR10(I1) = PTR1(I1)
                else 
                   if (couplerType == MAPL_CplMax) then
                      PTR10(I1) = max(PTR10(I1),PTR1(I1))
                   else if (couplerType == MAPL_CplMin) then
                      PTR10(I1) = min(PTR10(I1),PTR1(I1))
                   end if
                end if
             end DO
          endif

       case default
          _RETURN(ESMF_FAILURE)

       end select

       if(couplerType == MAPL_CplMax .or. couplerType == MAPL_CplMin) then 
        STATE%ACCUM_COUNT(J) = 1
       else
        STATE%ACCUM_COUNT(J) = STATE%ACCUM_COUNT(J) + 1
       endif

    end do

    _RETURN(ESMF_SUCCESS)
  end subroutine ACCUMULATE


  subroutine ZERO_CLEAR_COUNT(STATE, RC)
    type (MAPL_GenericCplState) :: STATE
    integer, optional           :: RC 

! local vars

    integer                               :: J
    integer                               :: DIMS
    logical                               :: RINGING
    real, pointer                         :: PTR1 (:)
    real, pointer                         :: PTR2 (:,:)
    real, pointer                         :: PTR3 (:,:,:)
    real, pointer                         :: PTR10(:)
    real, pointer                         :: PTR20(:,:)
    real, pointer                         :: PTR30(:,:,:)

    character(*), parameter       :: IAm="ZERO_CLEAR_COUNT"
    integer                       :: STATUS

    do J = 1, size(STATE%SRC_SPEC)

       RINGING = ESMF_AlarmIsRinging(STATE%TIME_TO_CLEAR(J), RC=STATUS)
       _VERIFY(STATUS)
       
       if (RINGING) then
          if(.not.associated(STATE%TIME2CPL_ALARM)) then
             call ESMF_AlarmRingerOff(STATE%TIME_TO_CLEAR(J), RC=STATUS)
             _VERIFY(STATUS)
          end if

          DIMS = STATE%ACCUM_RANK(J)

! Process the 3 dimension possibilities
!--------------------------------------

          select case(DIMS)

          case(3)
             call ESMF_LocalArrayGet(STATE%ACCUMULATORS(J),farrayPtr=PTR30,RC=STATUS)
             _VERIFY(STATUS)
             if (STATE%couplerType(J) /= MAPL_CplAverage .and. STATE%couplerType(J) /= MAPL_CplAccumulate) then
                PTR30 = MAPL_UNDEF
             else
                PTR30 = 0.0
             endif
             if (associated(STATE%ARRAY_COUNT(J)%PTR3C)) STATE%ARRAY_COUNT(J)%PTR3C = 0

          case(2)
             call ESMF_LocalArrayGet(STATE%ACCUMULATORS(J),farrayPtr=PTR20,RC=STATUS)
             _VERIFY(STATUS)
             if (STATE%couplerType(J) /= MAPL_CplAverage .and. STATE%couplerType(J) /= MAPL_CplAccumulate) then
                PTR20 = MAPL_UNDEF
             else
                PTR20 = 0.0
             endif
             if (associated(STATE%ARRAY_COUNT(J)%PTR2C)) STATE%ARRAY_COUNT(J)%PTR2C = 0

          case(1)
             call ESMF_LocalArrayGet(STATE%ACCUMULATORS(J),farrayPtr=PTR10,RC=STATUS)
             _VERIFY(STATUS)
             if (STATE%couplerType(J) /= MAPL_CplAverage .and. STATE%couplerType(J) /= MAPL_CplAccumulate) then
                PTR10 = MAPL_UNDEF
             else
                PTR10 = 0.0
             endif
             if (associated(STATE%ARRAY_COUNT(J)%PTR1C)) STATE%ARRAY_COUNT(J)%PTR1C = 0

          case default
             _RETURN(ESMF_FAILURE)

          end select

          STATE%ACCUM_COUNT(J) = 0
          if (associated(STATE%ARRAY_COUNT(J)%PTR1C)) then
             deallocate(STATE%ARRAY_COUNT(J)%PTR1C)
             nullify(STATE%ARRAY_COUNT(J)%PTR1C)
          end if
          if (associated(STATE%ARRAY_COUNT(J)%PTR2C)) then
             deallocate(STATE%ARRAY_COUNT(J)%PTR2C)
             nullify(STATE%ARRAY_COUNT(J)%PTR2C)
          end if
          if (associated(STATE%ARRAY_COUNT(J)%PTR3C)) then
             deallocate(STATE%ARRAY_COUNT(J)%PTR3C)
             nullify(STATE%ARRAY_COUNT(J)%PTR3C)
          end if

       end if
    end do

    _RETURN(ESMF_SUCCESS)
  end subroutine ZERO_CLEAR_COUNT


  subroutine COUPLE(SRC, STATE, RC)
    type (ESMF_State)           :: SRC
    type (MAPL_GenericCplState) :: STATE
    integer, optional           :: RC 

! local vars

    integer                               :: J
    character (len=ESMF_MAXSTR)           :: NAME
    integer                               :: DIMS
    real, pointer                         :: PTR1 (:)
    real, pointer                         :: PTR2 (:,:)
    real, pointer                         :: PTR3 (:,:,:)
    real, pointer                         :: PTR10(:)
    real, pointer                         :: PTR20(:,:)
    real, pointer                         :: PTR30(:,:,:)
    integer, pointer                      :: PTR1c(:)
    integer, pointer                      :: PTR2c(:,:)
    integer, pointer                      :: PTR3c(:,:,:)
    logical                               :: RINGING
    integer                               :: couplerType

    character(*), parameter       :: IAm="COUPLE"
    integer                       :: STATUS

    do J = 1, size(STATE%SRC_SPEC)

       couplerType = state%couplerType(J)
       RINGING = ESMF_AlarmIsRinging(STATE%TIME_TO_COUPLE(J), RC=STATUS)
       _VERIFY(STATUS)
       
       if (RINGING) then

          if(.not.associated(STATE%TIME2CPL_ALARM)) then
             call ESMF_AlarmRingerOff(STATE%TIME_TO_COUPLE(J), RC=STATUS)
             _VERIFY(STATUS)
          end if
          call MAPL_VarSpecGet(STATE%DST_SPEC(J), SHORT_NAME=NAME, RC=STATUS)
          _VERIFY(STATUS)

          DIMS = STATE%ACCUM_RANK(J)

! Process the three dimension possibilities
!------------------------------------------

          select case(DIMS)

          case(3)
             call ESMF_LocalArrayGet(STATE%ACCUMULATORS(J),farrayPtr=PTR30,RC=STATUS)
             _VERIFY(STATUS)
             call MAPL_GetPointer  (DST, PTR3, NAME,            RC=STATUS)
             _VERIFY(STATUS)
             PTR3c => STATE%ARRAY_COUNT(J)%PTR3C
             if(associated(PTR3C)) then
                if (couplerType /= MAPL_CplAccumulate) then
                   where (PTR3C /= 0) 
                      PTR30 = PTR30 / PTR3C
                   elsewhere
                      PTR30 = MAPL_Undef
                   end where
                else
                   where (PTR3C /= 0) 
                      PTR30 = PTR30
                   elsewhere
                      PTR30 = MAPL_Undef
                   end where
                end if
             elseif(STATE%ACCUM_COUNT(J)>0) then
                if (couplerType /= MAPL_CplAccumulate) then
                   PTR30 = PTR30 / STATE%ACCUM_COUNT(J)
                else
                   PTR30 = PTR30
                end if
             else
                PTR30 = MAPL_Undef
             end if

! Regrid stubbed

             PTR3 = PTR30

          case(2)
             call ESMF_LocalArrayGet(STATE%ACCUMULATORS(J),farrayPtr=PTR20,RC=STATUS)
             _VERIFY(STATUS)
             call MAPL_GetPointer  (DST, PTR2, NAME,            RC=STATUS)
             _VERIFY(STATUS)
             PTR2c => STATE%ARRAY_COUNT(J)%PTR2C
             if(associated(PTR2C)) then
                if (couplerType /= MAPL_CplAccumulate) then
                   where (PTR2C /= 0)
                      PTR20 = PTR20 / PTR2C
                   elsewhere
                      PTR20 = MAPL_Undef
                   end where
                else
                   where (PTR2C /= 0)
                      PTR20 = PTR20
                   elsewhere
                      PTR20 = MAPL_Undef
                   end where
                end if
             elseif(STATE%ACCUM_COUNT(J)>0) then
                if (couplerType /= MAPL_CplAccumulate) then
                   PTR20 = PTR20 / STATE%ACCUM_COUNT(J)
                else
                   PTR20 = PTR20
                end if
             else
                PTR20 = MAPL_Undef
             end if

! Regrid stubbed

             PTR2 = PTR20

          case(1)
             call ESMF_LocalArrayGet(STATE%ACCUMULATORS(J),farrayPtr=PTR10,RC=STATUS)
             _VERIFY(STATUS)
             call MAPL_GetPointer  (DST, PTR1, NAME,            RC=STATUS)
             _VERIFY(STATUS)
             PTR1c => STATE%ARRAY_COUNT(J)%PTR1C
             if(associated(PTR1C)) then
                if (couplerType /= MAPL_CplAccumulate) then
                   where (PTR1C /= 0) 
                      PTR10 = PTR10 / PTR1C
                   elsewhere
                      PTR10 = MAPL_Undef
                   end where
                else
                   where (PTR1C /= 0) 
                      PTR10 = PTR10
                   elsewhere
                      PTR10 = MAPL_Undef
                   end where
                end if
             elseif(STATE%ACCUM_COUNT(J)>0) then
                if (couplerType /= MAPL_CplAccumulate) then
                   PTR10 = PTR10 / STATE%ACCUM_COUNT(J)
                else
                   PTR10 = PTR10
                end if
             else
                PTR10 = MAPL_Undef
             end if

! Regrid stubbed

             PTR1 = PTR10

          case default
             _RETURN(ESMF_FAILURE)

          end select

          STATE%ACCUM_COUNT(J) = -1

       end if


    end do

    _RETURN(ESMF_SUCCESS)
  end subroutine COUPLE

 end subroutine Run

!---------------------------

!BOPI

! !IROUTINE: FINALIZE

! !DESCRIPTION: {Finalize method for the generic coupler.}

! !INTERFACE:

  subroutine Finalize(CC, SRC, DST, CLOCK, RC)

! !ARGUMENTS:

    type (ESMF_CplComp)      :: CC
    type (ESMF_State)        :: SRC
    type (ESMF_State)        :: DST
    type (ESMF_Clock)        :: CLOCK
    integer, intent(  OUT)   :: RC

!EOPI
! ErrLog Variables

    character(len=ESMF_MAXSTR)    :: IAm
    character(len=ESMF_MAXSTR)    :: COMP_NAME
    integer                       :: STATUS

! Locals

    type (MAPL_GenericCplState), pointer  :: STATE
    type (MAPL_GenericCplWrap )           :: WRAP

! Begin...

    _UNUSED_DUMMY(SRC)
    _UNUSED_DUMMY(DST)
    _UNUSED_DUMMY(CLOCK)

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

    IAm = "MAPL_GenericCplCompFinalize"
    call ESMF_CplCompGet( CC, NAME=COMP_NAME, RC=STATUS )
    _VERIFY(STATUS)
    Iam = trim(COMP_NAME) // Iam

! Retrieve the pointer to the internal state. It comes in a wrapper.
! ------------------------------------------------------------------

    call ESMF_CplCompGetInternalState ( CC, WRAP, STATUS )
    _VERIFY(STATUS)

    STATE     =>  WRAP%INTERNAL_STATE


    call write_parallel('STUBBED in CPL finalize')
    _RETURN(ESMF_SUCCESS)
  end subroutine Finalize

  subroutine ReadRestart(CC, SRC, DST, CLOCK, RC)

! !ARGUMENTS:

    type (ESMF_CplComp)      :: CC
    type (ESMF_State)        :: SRC
    type (ESMF_State)        :: DST
    type (ESMF_Clock)        :: CLOCK
    integer, intent(  OUT)   :: RC

!EOPI
! ErrLog Variables

    character(len=ESMF_MAXSTR)    :: IAm
    character(len=ESMF_MAXSTR)    :: COMP_NAME
    integer                       :: STATUS

! Locals

    type (MAPL_GenericCplState), pointer  :: STATE
    type (MAPL_GenericCplWrap )           :: WRAP
    type(ESMF_VM)                         :: VM
    type(ESMF_Grid)                       :: grid
    type(ESMF_Field)                      :: field
    character(len=ESMF_MAXSTR)            :: name
    character(len=ESMF_MAXSTR)            :: filename
    logical                               :: file_exists
    logical                               :: am_i_root
    integer                               :: unit
    integer                               :: n_vars
    integer                               :: n_count
    integer                               :: n_undefs
    integer                               :: rank
    integer                               :: i
    integer                               :: dims
    integer, pointer                      :: mask(:) => null()
    real, allocatable                     :: buf1(:), buf2(:,:), buf3(:,:,:)
    real, pointer                         :: ptr1(:), ptr2(:,:), ptr3(:,:,:)

! Begin...

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

    _UNUSED_DUMMY(dst)
    _UNUSED_DUMMY(clock)
    IAm = "MAPL_GenericCplComReadRestart"
    call ESMF_CplCompGet( CC, NAME=COMP_NAME, RC=STATUS )
    _VERIFY(STATUS)
    Iam = trim(COMP_NAME) // Iam

! Retrieve the pointer to the internal state. It comes in a wrapper.
! ------------------------------------------------------------------

    call ESMF_CplCompGetInternalState ( CC, WRAP, STATUS )
    _VERIFY(STATUS)

    STATE     =>  WRAP%INTERNAL_STATE


!ALT remove this line when done
    call write_parallel('STUBBED in CPL ReadRestart')
!ALT: Uncomment when done
!strategy
!root tries to open the restart (or inquire)
!if the file is there 
! read the restart:
!==================
!    call ESMF_CplCompGet(CC, vm=vm, name=name, rc=status)
!    _VERIFY(STATUS)

!    filename = trim(name) // '_rst' ! following Andrea's suggestion

    call ESMF_CplCompGet(CC, vm=vm, rc=status)
    _VERIFY(STATUS)
    filename = trim(state%name) // '_rst' ! following Andrea's suggestion
    am_i_root = MAPL_AM_I_ROOT(vm)
    if (am_i_root) then
       ! check if file exists
       inquire(file=filename, exist=file_exists)
    end if

    call MAPL_CommsBcast(vm, file_exists, n=1, ROOT=MAPL_Root, rc=status)
    _VERIFY(status)

    if (file_exists) then
       !ALT: ideally, we should check the monthly alarm: read only when not ringing.
       ! read metadata: grid info, number of vars
       unit=0 ! just to initialize
       if (am_i_root) then
          UNIT = GETFILE(filename, rc=status)
          _VERIFY(status)
          read(unit) n_vars
          _ASSERT(size(state%src_spec) == n_vars, "Number of variables on the restart does not agree with spec")
       end if

       ! for each var
       n_vars = size(state%src_spec)
       do i = 1, n_vars
          ! varname we can get from query SHORT_NAME in state%src_spec(i)
          call MAPL_VarSpecGet(state%src_spec(i), SHORT_NAME=name, rc=status)
          _VERIFY(status)
          call ESMF_StateGet(SRC, name, field=field, rc=status) 
          _VERIFY(status)
          call ESMF_FieldGet(field, grid=grid, rc=status)
          _VERIFY(status)

          rank = state%accum_rank(i)
          call ESMF_AttributeGet(field, name='DIMS', value=DIMS, rc=status)
          _VERIFY(STATUS)
          mask => null()
          if (DIMS == MAPL_DimsTileOnly .or. DIMS == MAPL_DimsTileTile) then
             call MAPL_TileMaskGet(grid,  mask, rc=status)
             _VERIFY(STATUS)
          end if
          ! ALT note: calling a procedure with optional argument, and passing NULL pointer to indicate "absent", needs ifort16 or newer
          
          if (am_i_root) then
             read(unit) n_count
          end if
          call MAPL_CommsBcast(vm, n_count, n=1, ROOT=MAPL_Root, rc=status)
          _VERIFY(status)
          state%accum_count(i) = n_count

          if (am_i_root) then
             read(unit) n_undefs
          end if
          call MAPL_CommsBcast(vm, n_undefs, n=1, ROOT=MAPL_Root, rc=status)
          _VERIFY(status)

          select case(rank)
          case (3)
             call ESMF_LocalArrayGet(STATE%ACCUMULATORS(i), &
                  farrayPtr=ptr3, RC=status)
             _VERIFY(status)
             
             call MAPL_VarRead(unit, grid, ptr3, rc=status)
             _VERIFY(STATUS)
             if (n_undefs /=0) then
                allocate(buf3(size(ptr3,1),size(ptr3,2),size(ptr3,3)), stat=status)
                _VERIFY(STATUS)
                call MAPL_VarRead(unit, grid, buf3, rc=status)
                _VERIFY(STATUS)
                if (.not. associated(state%array_count(i)%ptr3c)) then
                   allocate(state%array_count(i)%ptr3c(size(ptr3,1),size(ptr3,2),size(ptr3,3)), stat=status)
                   _VERIFY(STATUS)
                end if
                state%array_count(i)%ptr3c = buf3
                deallocate(buf3)
             end if
          case (2)
             call ESMF_LocalArrayGet(STATE%ACCUMULATORS(i), &
                  farrayPtr=ptr2, RC=status)
             _VERIFY(status)
             
             call MAPL_VarRead(unit, grid, ptr2, mask=mask, rc=status)
             _VERIFY(STATUS)
             if (n_undefs /=0) then
                allocate(buf2(size(ptr2,1),size(ptr2,2)), stat=status)
                _VERIFY(STATUS)
                call MAPL_VarRead(unit, grid, buf2, mask=mask, rc=status)
                _VERIFY(STATUS)
                if (.not. associated(state%array_count(i)%ptr2c)) then
                   allocate(state%array_count(i)%ptr2c(size(ptr2,1),size(ptr2,2)), stat=status)
                   _VERIFY(STATUS)
                end if
                state%array_count(i)%ptr2c = buf2
                deallocate(buf2)
             end if
          case (1)
             call ESMF_LocalArrayGet(STATE%ACCUMULATORS(i), &
                  farrayPtr=ptr1, RC=status)
             _VERIFY(status)
             
             call MAPL_VarRead(unit, grid, ptr1, mask=mask, rc=status)
             _VERIFY(STATUS)
             if (n_undefs /=0) then
                allocate(buf1(size(ptr1,1)), stat=status)
                _VERIFY(STATUS)
                call MAPL_VarRead(unit, grid, buf1, mask=mask, rc=status)
                _VERIFY(STATUS)
                if (.not. associated(state%array_count(i)%ptr1c)) then
                   allocate(state%array_count(i)%ptr1c(size(ptr1,1)), stat=status)
                   _VERIFY(STATUS)
                end if
                state%array_count(i)%ptr1c = buf1
                deallocate(buf1)
             end if
          case default
             _ASSERT(.false., "Unsupported rank")
          end select
          if(associated(mask)) deallocate(mask)
       end do

       if (am_i_root) call Free_File(unit = UNIT, rc=STATUS)

    else
       RC = ESMF_RC_FILE_READ
       return
    end if

    _RETURN(ESMF_SUCCESS)
  end subroutine ReadRestart

  subroutine WriteRestart(CC, SRC, DST, CLOCK, RC)

! !ARGUMENTS:

    type (ESMF_CplComp)      :: CC
    type (ESMF_State)        :: SRC
    type (ESMF_State)        :: DST
    type (ESMF_Clock)        :: CLOCK
    integer, intent(  OUT)   :: RC

!EOPI
! ErrLog Variables

    character(len=ESMF_MAXSTR)    :: IAm
    character(len=ESMF_MAXSTR)    :: COMP_NAME
    integer                       :: STATUS

! Locals

    type (MAPL_GenericCplState), pointer  :: STATE
    type (MAPL_GenericCplWrap )           :: WRAP
    type(ESMF_VM)                         :: VM
    type(ESMF_Grid)                       :: grid
    type(ESMF_Field)                      :: field
    character(len=ESMF_MAXSTR)            :: name
    character(len=ESMF_MAXSTR)            :: filename
    logical                               :: am_i_root
    logical                               :: local_undefs
    integer                               :: unit
    integer                               :: n_vars
    integer                               :: n_count
    integer                               :: n_undefs
    integer                               :: rank
    integer                               :: i
    integer                               :: dims
    integer                               :: have_undefs
    integer, pointer                      :: mask(:) => null()
    real, allocatable                     :: buf1(:), buf2(:,:), buf3(:,:,:)
    real, pointer                         :: ptr1(:), ptr2(:,:), ptr3(:,:,:)

! Begin...

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------
    _UNUSED_DUMMY(dst)
    _UNUSED_DUMMY(clock)

    IAm = "MAPL_GenericCplComWriteRestart"
    call ESMF_CplCompGet( CC, NAME=COMP_NAME, RC=STATUS )
    _VERIFY(STATUS)
    Iam = trim(COMP_NAME) // Iam

! Retrieve the pointer to the internal state. It comes in a wrapper.
! ------------------------------------------------------------------

    call ESMF_CplCompGetInternalState ( CC, WRAP, STATUS )
    _VERIFY(STATUS)

    STATE     =>  WRAP%INTERNAL_STATE

    call ESMF_CplCompGet(CC, vm=vm, rc=status)
    _VERIFY(STATUS)

    filename = trim(comp_name) // '_checkpoint' ! following Andrea's and Larry's suggestions
    am_i_root = MAPL_AM_I_ROOT(vm)

    unit=0 ! just to initialize
    n_vars = size(state%src_spec)
    if (am_i_root) then
       UNIT = GETFILE(filename, rc=status)
       _VERIFY(status)
       write(unit) n_vars
    end if

       ! for each var
    do i = 1, n_vars
       ! varname we can get from query SHORT_NAME in state%src_spec(i)
       call MAPL_VarSpecGet(state%src_spec(i), SHORT_NAME=name, rc=status)
       _VERIFY(status)
       call ESMF_StateGet(SRC, name, field=field, rc=status) 
       _VERIFY(status)
       call ESMF_FieldGet(field, grid=grid, rc=status)
       _VERIFY(status)

       rank = state%accum_rank(i)
       call ESMF_AttributeGet(field, name='DIMS', value=DIMS, rc=status)
       _VERIFY(STATUS)
       mask => null()
       if (DIMS == MAPL_DimsTileOnly .or. DIMS == MAPL_DimsTileTile) then
          call MAPL_TileMaskGet(grid,  mask, rc=status)
          _VERIFY(STATUS)
       end if

       !we need to get the MAX n_count         
       call MAPL_CommsAllReduceMax(vm, sendbuf=state%accum_count(i), &
            recvbuf=n_count, cnt=1, RC=status)
       _VERIFY(status)
       if (am_i_root) then
          write(unit) n_count
       end if
       select case (rank)
          case(1)
             local_undefs = associated(state%array_count(i)%ptr1c)
          case(2)
             local_undefs = associated(state%array_count(i)%ptr2c)
          case(3)
             local_undefs = associated(state%array_count(i)%ptr3c)
          case default
             _ASSERT(.false., "Unsupported rank")
          end select
       have_undefs = 0
       n_undefs = 0
       if (local_undefs) have_undefs = 1
       call MAPL_CommsAllReduceMax(vm, sendbuf=have_undefs, &
            recvbuf=n_undefs, cnt=1, RC=status)
       _VERIFY(status)
       if (am_i_root) then
          write(unit) n_undefs
       end if

       select case(rank)
          case (3)
             call ESMF_LocalArrayGet(STATE%ACCUMULATORS(i), &
                  farrayPtr=ptr3, RC=status)
             _VERIFY(status)
             
             call MAPL_VarWrite(unit, grid, ptr3, rc=status)
             _VERIFY(STATUS)
             if (n_undefs /=0) then
                allocate(buf3(size(ptr3,1),size(ptr3,2),size(ptr3,3)), stat=status)
                _VERIFY(STATUS)
                if (associated(state%array_count(i)%ptr3c)) then
                   buf3 = state%array_count(i)%ptr3c
                else
                   buf3 = state%accum_count(i)
                end if
                call MAPL_VarWrite(unit, grid, buf3, rc=status)
                _VERIFY(STATUS)
                deallocate(buf3)
             end if
          case (2)
             call ESMF_LocalArrayGet(STATE%ACCUMULATORS(i), &
                  farrayPtr=ptr2, RC=status)
             _VERIFY(status)
             
             call MAPL_VarWrite(unit, grid, ptr2, mask=mask, rc=status)
             _VERIFY(STATUS)
             if (n_undefs /=0) then
                allocate(buf2(size(ptr2,1),size(ptr2,2)), stat=status)
                _VERIFY(STATUS)
                if (associated(state%array_count(i)%ptr2c)) then
                   buf2 = state%array_count(i)%ptr2c
                else
                   buf2 = state%accum_count(i)
                end if
                call MAPL_VarWrite(unit, grid, buf2, mask=mask, rc=status)
                _VERIFY(STATUS)
                deallocate(buf2)
             end if
          case (1)
             call ESMF_LocalArrayGet(STATE%ACCUMULATORS(i), &
                  farrayPtr=ptr1, RC=status)
             _VERIFY(status)
             
             call MAPL_VarWrite(unit, grid, ptr1, mask=mask, rc=status)
             _VERIFY(STATUS)
             if (n_undefs /=0) then
                allocate(buf1(size(ptr1,1)), stat=status)
                _VERIFY(STATUS)
                if (associated(state%array_count(i)%ptr1c)) then
                   buf1 = state%array_count(i)%ptr1c
                else
                   buf1 = state%accum_count(i)
                end if
                call MAPL_VarWrite(unit, grid, buf1, mask=mask, rc=status)
                _VERIFY(STATUS)
                deallocate(buf1)
             end if
          case default
             _ASSERT(.false.," Unsupported rank")
          end select
          if(associated(mask)) deallocate(mask)
       end do

       if(am_i_root) call Free_File(unit = UNIT, rc=STATUS)


    _RETURN(ESMF_SUCCESS)
  end subroutine WriteRestart

  subroutine MAPL_CplCompSetAlarm ( CC, ALARM, RC )
    type (ESMF_CplComp  ),           intent(INOUT) :: CC  
    type (ESMF_Alarm), target,       intent(IN   ) :: ALARM
    integer, optional,               intent(  OUT) :: RC
    
! ErrLog Variables

    character(len=ESMF_MAXSTR)    :: IAm
    character(len=ESMF_MAXSTR)    :: COMP_NAME
    integer                       :: STATUS

! Locals

    type (MAPL_GenericCplState), pointer :: STATE
    type (MAPL_GenericCplWrap )          :: WRAP


! Begin...

! Get this instance's name and set-up traceback handle.
! -----------------------------------------------------

    call ESMF_CplCompGet( CC, name=COMP_NAME, RC=STATUS )
    _VERIFY(STATUS)
    Iam = trim(COMP_NAME) // "MAPL_CplCompSetAlarm"

! Retrieve the pointer to the internal state. It comes in a wrapper.
! ------------------------------------------------------------------

    call ESMF_CplCompGetInternalState ( CC, WRAP, STATUS )
    _VERIFY(STATUS)

    STATE  =>  WRAP%INTERNAL_STATE

    if (.not.associated(STATE%TIME2CPL_ALARM)) then
       STATE%TIME2CPL_ALARM => ALARM
    else
       _ASSERT(.false., "Alarm is already associated! Cannot set it again!")
    end if
    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_CplCompSetAlarm

end module MAPL_GenericCplCompMod
