

#define LDB_SUCCESS  0
#include "MAPL_ErrLog.h"
!------------------------------------------------------------------------------
!>
!### MODULE: `MAPL_LoadBalanceMod`
!
module MAPL_LoadBalanceMod

  use MAPL_Constants, only : MAPL_R8
  use MAPL_SortMod
  use MAPL_ExceptionHandling
  use mpi
  implicit none
  private

  public MAPL_BalanceWork
  public MAPL_BalanceCreate
  public MAPL_BalanceDestroy
  public MAPL_BalanceGet

  interface MAPL_BalanceWork
     module procedure MAPL_BalanceWork4
     module procedure MAPL_BalanceWork8
  end interface MAPL_BalanceWork
  integer, public, parameter :: MAPL_Distribute = 1
  integer, public, parameter :: MAPL_Retrieve   = 2

  type TBalanceStrategy
     integer :: UNBALANCED_LENGTH=-1
     integer :: BALANCED_LENGTH  =-1
     integer :: BUFFER_LENGTH    =-1
     integer :: PASSES           =-1
     integer :: COMM             =-1
     integer, pointer :: NOP(:,:)=>Null()
     ! Pre-computed cursor positions for each pass (distribute and retrieve)
     integer, pointer :: CURSOR_DIST(:)=>Null()
     integer, pointer :: CURSOR_RETR(:)=>Null()
  end type TBalanceStrategy

  integer,           parameter :: MAX_NUM_STRATEGIES=1000
  type(TBalanceStrategy), save :: THE_STRATEGIES(0:MAX_NUM_STRATEGIES)

!---------------------------------------------------------------------------
!>
!### EXAMPLE
!```fortran
!      REAL A(IM,JM,LM), B(IM,JM), C(IM,JM,LM)
!      REAL, allocatable :: AT(:,:), BT(:), CT(:,:)
!      LOGICAL MASK(IM,JM)
!      ...
!      LENGTH = COUNT(MASK)
!      IRUN   = MAPL_BalanceCreate(LENGTH)
!      IDIM   = max(length,irun)
!
!      allocate(AT(IDIM,LM),BT(IDIM),CT(IDIM,LM)
!
!      BT(1:LENGTH) = PACK(B,MASK)
!
!      DO L=1,LM
!       AT(1:LENGTH,L) = PACK(A(:,:,L),MASK)
!      ENDDO
!
!!! DISTRIBUTE THE INPUTS
!
!      CALL MAPL_BalanceWork(AT,IDIM,LM,Direction=MAPL_Distribute)
!      CALL MAPL_BalanceWork(BT,IDIM,1 ,Direction=MAPL_Distribute)
!
!!! PLUG COMPATIBLE ROUTINE AT(IN), BT(INOUT), CT(OUT)
!
!      CALL WORKSUB(IRUN,AT,BT,CT)
!
!!! RETRIEVE THE OUTPUTS
!
!      CALL MAPL_BalanceWork(CT,IDIM,LM,Direction=MAPL_Retrieve)
!      CALL MAPL_BalanceWork(BT,IDIM, 1,Direction=MAPL_Retrieve)
!
!      B = UNPACK(BT(1:LENGTH),MASK,B)
!
!      DO L=1,LM
!       C(:,:,L) = UNPACK(CT(1:LENGTH,L),MASK,0)
!      ENDDO
!      ...
!```
!---------------------------------------------------------------------------

contains

!---------------------------------------------------------------------------
!>
! Depending on the argument "Direction", this performs the actual distribution
! of work or the gathering of results for a given strategy. The strategy has to
! have been predefined by a call to MAPL_BalanceCreate. A strategy "Handle"
! obtained from that call can be optionally used to specify the strategy. Otherwise,
! a default strategy is assumed (see MAPL_BalanceCreate for details).
! Work (Results) is distributed (retrieved) using the buffer A, which is assumed
! to consist of Jdim contiguous blocks of size Idim. Of course, Jdim can be 1.
! The blocksize of A (Idim) must be at least as large as the BufLen associated
! with the strategy. This size can be obtained by quering the strategy using
! its handle or be saving it from the MAPL_BalanceCreate call. Again, see
! MAPL_BalanceCreate for details.
!
! Non-blocking MPI is used: all MPI_ISEND/MPI_IRECV calls are posted in a
! single pass over the NOP table, then a single MPI_WAITALL completes all
! transfers. For multi-level arrays (Jdim>1), MPI derived vector types are
! created once per pass before posting and freed after MPI_WAITALL.

  subroutine MAPL_BalanceWork4(A, Idim, Direction, Handle, rc)
    real,              intent(INOUT) :: A(:)
    integer,           intent(IN   ) :: Idim, Direction
    integer, optional, intent(IN   ) :: Handle
    integer, optional, intent(  OUT) :: rc

    integer :: PASS, LENGTH, PROCESSOR, CURSOR, ISTRAT, KPASS
    integer :: COMM, STATUS, Jdim
    integer :: nreq, Vtype, VLength
    logical :: SEND, RECV
    integer, pointer :: NOP(:,:), CURSORS(:)
    integer, allocatable :: requests(:), statuses(:,:), vtypes(:)

    Jdim = size(A)/Idim

    if(present(Handle)) then
       ISTRAT = Handle
    else
       ISTRAT = 0
    endif

    if(THE_STRATEGIES(ISTRAT)%PASSES>0) then ! We have a defined strategy
       _ASSERT(associated(THE_STRATEGIES(ISTRAT)%NOP),'needs informative message')

       NOP   => THE_STRATEGIES(ISTRAT)%NOP
       COMM  =  THE_STRATEGIES(ISTRAT)%COMM
       KPASS =  THE_STRATEGIES(ISTRAT)%PASSES

       ! Select pre-computed cursor array for this direction
       if (Direction==MAPL_Distribute) then
          CURSORS => THE_STRATEGIES(ISTRAT)%CURSOR_DIST
       else
          CURSORS => THE_STRATEGIES(ISTRAT)%CURSOR_RETR
       end if

       ! Allocate request array and, for Jdim>1, a slot to track
       ! any derived types so they can be freed after WAITALL.
       allocate(requests(KPASS), statuses(MPI_STATUS_SIZE, KPASS))
       if (Jdim > 1) allocate(vtypes(KPASS), source=MPI_DATATYPE_NULL)
       nreq = 0

       do PASS=1,KPASS
          LENGTH    = abs(NOP(1,PASS))
          PROCESSOR = NOP(2,PASS)
          CURSOR    = CURSORS(PASS)

          if (LENGTH == 0) cycle

          if (Direction==MAPL_Distribute) then
             SEND = NOP(1,PASS)>0
             RECV = NOP(1,PASS)<0
          else
             SEND = NOP(1,PASS)<0
             RECV = NOP(1,PASS)>0
          end if

          if (Jdim==1) then
             Vtype   = MPI_REAL
             VLength = LENGTH
          else
             call MPI_Type_VECTOR(Jdim, Length, Idim, MPI_REAL, Vtype, STATUS)
             _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')
             call MPI_TYPE_COMMIT(Vtype, STATUS)
             _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')
             vtypes(PASS) = Vtype
             VLength = 1
          end if

          nreq = nreq + 1
          if (SEND) then
             call MPI_ISEND(A(CURSOR), VLength, Vtype, PROCESSOR, PASS, COMM, &
                            requests(nreq), STATUS)
             _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')
          else if (RECV) then
             call MPI_IRECV(A(CURSOR), VLength, Vtype, PROCESSOR, PASS, COMM, &
                            requests(nreq), STATUS)
             _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')
          else
             nreq = nreq - 1
          end if
       enddo

       ! Complete all non-blocking operations in one call
       if (nreq > 0) then
          call MPI_WAITALL(nreq, requests, statuses, STATUS)
          _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')
       end if

       ! Free any derived types that were created
       if (Jdim > 1) then
          do PASS=1,KPASS
             if (vtypes(PASS) /= MPI_DATATYPE_NULL) then
                call MPI_TYPE_FREE(vtypes(PASS), STATUS)
                _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')
             end if
          end do
          deallocate(vtypes)
       end if

       deallocate(requests, statuses)
    end if

    _RETURN(LDB_SUCCESS)
  end subroutine MAPL_BalanceWork4

!---------------------------------------------------------------------------
!>
! Depending on the argument "Direction", this performs the actual distribution
! of work or the gathering of results for a given strategy. The strategy has to
! have been predefined by a call to MAPL_BalanceCreate. A strategy "Handle"
! obtained from that call can be optionally used to specify the strategy. Otherwise,
! a default strategy is assumed (see MAPL_BalanceCreate for details).
! Work (Results) is distributed (retrieved) using the buffer A, which is assumed
! to consist of Jdim contiguous blocks of size Idim. Of course, Jdim can be 1.
! The blocksize of A (Idim) must be at least as large as the BufLen associated
! with the strategy. This size can be obtained by quering the strategy using
! its handle or be saving it from the MAPL_BalanceCreate call. Again, see
! MAPL_BalanceCreate for details.
!
! Non-blocking MPI is used: all MPI_ISEND/MPI_IRECV calls are posted in a
! single pass over the NOP table, then a single MPI_WAITALL completes all
! transfers.

  subroutine MAPL_BalanceWork8(A, Idim, Direction, Handle, rc)
    real(kind=MAPL_R8), intent(INOUT) :: A(:)
    integer,            intent(IN   ) :: Idim, Direction
    integer, optional,  intent(IN   ) :: Handle
    integer, optional,  intent(  OUT) :: rc

    integer :: PASS, LENGTH, PROCESSOR, CURSOR, ISTRAT, KPASS
    integer :: COMM, STATUS, Jdim
    integer :: nreq, Vtype, VLength
    logical :: SEND, RECV
    integer, pointer :: NOP(:,:), CURSORS(:)
    integer, allocatable :: requests(:), statuses(:,:), vtypes(:)

    Jdim = size(A)/Idim

    if(present(Handle)) then
       ISTRAT = Handle
    else
       ISTRAT = 0
    endif

    if(THE_STRATEGIES(ISTRAT)%PASSES>0) then ! We have a defined strategy
       _ASSERT(associated(THE_STRATEGIES(ISTRAT)%NOP),'needs informative message')

       NOP   => THE_STRATEGIES(ISTRAT)%NOP
       COMM  =  THE_STRATEGIES(ISTRAT)%COMM
       KPASS =  THE_STRATEGIES(ISTRAT)%PASSES

       ! Select pre-computed cursor array for this direction
       if (Direction==MAPL_Distribute) then
          CURSORS => THE_STRATEGIES(ISTRAT)%CURSOR_DIST
       else
          CURSORS => THE_STRATEGIES(ISTRAT)%CURSOR_RETR
       end if

       allocate(requests(KPASS), statuses(MPI_STATUS_SIZE, KPASS))
       if (Jdim > 1) allocate(vtypes(KPASS), source=MPI_DATATYPE_NULL)
       nreq = 0

       do PASS=1,KPASS
          LENGTH    = abs(NOP(1,PASS))
          PROCESSOR = NOP(2,PASS)
          CURSOR    = CURSORS(PASS)

          if (LENGTH == 0) cycle

          if (Direction==MAPL_Distribute) then
             SEND = NOP(1,PASS)>0
             RECV = NOP(1,PASS)<0
          else
             SEND = NOP(1,PASS)<0
             RECV = NOP(1,PASS)>0
          end if

          if (Jdim==1) then
             Vtype   = MPI_DOUBLE_PRECISION
             VLength = LENGTH
          else
             call MPI_Type_VECTOR(Jdim, Length, Idim, MPI_DOUBLE_PRECISION, Vtype, STATUS)
             _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')
             call MPI_TYPE_COMMIT(Vtype, STATUS)
             _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')
             vtypes(PASS) = Vtype
             VLength = 1
          end if

          nreq = nreq + 1
          if (SEND) then
             call MPI_ISEND(A(CURSOR), VLength, Vtype, PROCESSOR, PASS, COMM, &
                            requests(nreq), STATUS)
             _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')
          else if (RECV) then
             call MPI_IRECV(A(CURSOR), VLength, Vtype, PROCESSOR, PASS, COMM, &
                            requests(nreq), STATUS)
             _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')
          else
             nreq = nreq - 1
          end if
       enddo

       ! Complete all non-blocking operations in one call
       if (nreq > 0) then
          call MPI_WAITALL(nreq, requests, statuses, STATUS)
          _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')
       end if

       ! Free any derived types that were created
       if (Jdim > 1) then
          do PASS=1,KPASS
             if (vtypes(PASS) /= MPI_DATATYPE_NULL) then
                call MPI_TYPE_FREE(vtypes(PASS), STATUS)
                _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')
             end if
          end do
          deallocate(vtypes)
       end if

       deallocate(requests, statuses)
    end if

    _RETURN(LDB_SUCCESS)
  end subroutine MAPL_BalanceWork8

!---------------------------------------------------------------------------
!>
! This routine creates a balancing strategy over an MPI communicator (Comm)
! given the work in the local rank (OrgLen). The startegy can be committed
! and used later through Handle. If a handle is not requested, the latest
! non-committed strategy is kept at Handle=0, which will be the default strategy
! for the other methods. The number of passes may be optionally controlled
! with an upper limit (MaxPasses) or a limiting criterion (BalCond).
! The amount of work resulting for the local rank can be returned (BalLen).
!
!@note
! As there may be more than one communicator, Comm is required. This
! will most likely be the communicator from the ESMF VM.
!@endnote
!
  subroutine MAPL_BalanceCreate(OrgLen, Comm, MaxPasses, BalCond, &
                                Handle, BalLen, BufLen, rc)

    integer,           intent(IN)  :: OrgLen
    integer,           intent(IN)  :: Comm
    integer, optional, intent(IN)  :: MaxPasses
    real,    optional, intent(IN)  :: BalCond
    integer, optional, intent(OUT) :: Handle, BalLen, BufLen
    integer, optional, intent(OUT) :: rc

    real    :: BalCond_
    integer :: MaxPasses_
    integer :: KPASS, STATUS, Balance, MyNewWork, MyBufSize
    integer :: NPES, MyPE, J

    integer, allocatable :: WORK(:), RANK(:), NOP(:,:)

! Defaults of optional Inputs
!----------------------------

    if(present(BalCond)) then
       BalCond_ = BalCond
    else
       BalCond_ = 0.1
    end if

    if(present(MaxPasses)) then
       MaxPasses_ = MaxPasses
    else
       MaxPasses_ = 100
    end if

! Get Communicator parameters
!----------------------------

    call MPI_COMM_RANK(Comm, MyPE, STATUS)
    _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')
    call MPI_COMM_SIZE(Comm, NPES, STATUS)
    _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')

! Allocate temporary space and zero-initialise so passes where
! this rank is not involved have NOP(1,PASS)=0 (no-op).
!-------------------------------------------------------------

    allocate(NOP(2,MaxPasses_), Work(NPES), Rank(NPES), stat=STATUS)
    _VERIFY(STATUS)
    NOP = 0

! Initialize global lists of work load and corresponding rank
!------------------------------------------------------------

    call MPI_AllGather(OrgLen,1,MPI_INTEGER,&
                       Work  ,1,MPI_INTEGER,Comm,status)
    _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')

    do concurrent (J=1:NPES)
       Rank(J) = J-1
    end do

    call CreateStrategy(Work, Rank, MyPE, BalCond_, Kpass, MyNewWork, MyBufSize, NOP)

    deallocate(Work, Rank)

! Done with balancing strategy. Prepare optional Outputs.
!--------------------------------------------------------

    if(present(Handle)) then
       do Balance=1,MAX_NUM_STRATEGIES
          if(.not.associated(THE_STRATEGIES(Balance)%NOP)) exit
       enddo

       _ASSERT(Balance <= MAX_NUM_STRATEGIES,'needs informative message')
       Handle  = Balance
    else
       Balance = 0
       if( associated(THE_STRATEGIES(Balance)%NOP) ) then
           deallocate(THE_STRATEGIES(Balance)%NOP)
           deallocate(THE_STRATEGIES(Balance)%CURSOR_DIST)
           deallocate(THE_STRATEGIES(Balance)%CURSOR_RETR)
       end if
    end if

    if(present(BalLen)) BalLen =  MyNewWork
    if(present(BufLen)) BufLen =  MyBufSize

! Save the Strategy
!------------------

    allocate(THE_STRATEGIES(Balance)%NOP(2,KPASS))
    allocate(THE_STRATEGIES(Balance)%CURSOR_DIST(KPASS))
    allocate(THE_STRATEGIES(Balance)%CURSOR_RETR(KPASS))

    THE_STRATEGIES(Balance)%BALANCED_LENGTH   = MyNewWork
    THE_STRATEGIES(Balance)%BUFFER_LENGTH     = MyBufSize
    THE_STRATEGIES(Balance)%UNBALANCED_LENGTH = OrgLen
    THE_STRATEGIES(Balance)%PASSES            = KPASS
    THE_STRATEGIES(Balance)%COMM              = Comm
    THE_STRATEGIES(Balance)%NOP               = NOP(:,:KPASS)

    ! Pre-compute cursor positions for each pass in both directions.
    call PrecomputeCursors(THE_STRATEGIES(Balance)%NOP, KPASS, &
                           OrgLen, MyNewWork,                   &
                           THE_STRATEGIES(Balance)%CURSOR_DIST, &
                           THE_STRATEGIES(Balance)%CURSOR_RETR)

    deallocate(NOP)

    _RETURN(LDB_SUCCESS)

  contains

    !> Pre-compute cursor positions for all passes so BalanceWork can post
    !  all non-blocking communications without sequential cursor arithmetic.
    !
    !  Distribute direction: CURSOR starts at OrgLen+1. For each pass
    !    a sender decrements CURSOR by LENGTH before posting; a receiver
    !    increments CURSOR by LENGTH after posting.
    !  Retrieve direction: passes are processed in reverse (K3=-1 in the
    !    original loop). Sender decrements before posting; receiver
    !    increments after posting.
    subroutine PrecomputeCursors(NOP, KPASS, OrgLen, BalLen, CURSOR_DIST, CURSOR_RETR)
      integer, intent(IN)  :: NOP(:,:), KPASS, OrgLen, BalLen
      integer, intent(OUT) :: CURSOR_DIST(:), CURSOR_RETR(:)

      integer :: PASS, CURSOR, LENGTH

      ! Distribute direction (forward pass)
      CURSOR = OrgLen + 1
      do PASS=1,KPASS
         LENGTH = abs(NOP(1,PASS))
         if (NOP(1,PASS)>0) then       ! SEND: decrement before
            CURSOR = CURSOR - LENGTH
            CURSOR_DIST(PASS) = CURSOR
         else if (NOP(1,PASS)<0) then  ! RECV: record then increment
            CURSOR_DIST(PASS) = CURSOR
            CURSOR = CURSOR + LENGTH
         else                          ! no-op for this rank
            CURSOR_DIST(PASS) = CURSOR
         end if
      end do

      ! Retrieve direction (reverse pass)
      CURSOR = BalLen + 1
      do PASS=KPASS,1,-1
         LENGTH = abs(NOP(1,PASS))
         if (NOP(1,PASS)<0) then       ! SEND (sign reversed): decrement before
            CURSOR = CURSOR - LENGTH
            CURSOR_RETR(PASS) = CURSOR
         else if (NOP(1,PASS)>0) then  ! RECV (sign reversed): record then increment
            CURSOR_RETR(PASS) = CURSOR
            CURSOR = CURSOR + LENGTH
         else                          ! no-op for this rank
            CURSOR_RETR(PASS) = CURSOR
         end if
      end do

    end subroutine PrecomputeCursors

    subroutine CreateStrategy(Work, Rank, MyPE, BalCond, KPASS, MyNewWork, MyBufSize, NOP)
      integer, intent(INOUT) :: Work(:), Rank(:)
      integer, intent(IN   ) :: MyPE
      real   , intent(IN   ) :: BalCond
      integer, intent(  OUT) :: NOP(:,:), KPASS, MyNewWork, MyBufSize

      integer :: NPES, J, JSPARD, LEN, MaxPasses
      real    :: MEAN

      NPES      = size(Work)
      MaxPasses = size(NOP,2)

! Loop over passes until either MaxPasses or BalanceCondition is met
!-------------------------------------------------------------------

      KPASS     = 0
      MEAN      = sum(Work)/real(NPES)
      MyNewWork = OrgLen
      MyBufSize = OrgLen

      PASSES: do while(KPASS<MaxPasses)

! Sort latest work-load and rank lists in ascending order of work
!----------------------------------------------------------------

         call MAPL_Sort(Work, Rank)

! Check for balance condition on the ratio of max minus min work
!  to the ideal average work
!---------------------------------------------------------------

         if((Work(NPES)-Work(1))/MEAN < BalCond) exit

! Fold the sorted work list and compute the transfers needed
!  to balance the "least with the greatest".
!-----------------------------------------------------------

         KPASS = KPASS+1

         FOLD: do J=1,NPES/2

            ! Js partner in the fold has >= the work as J
            JSPARD = NPES + 1 - J

            ! Half the difference will be sent to J (can be zero)
            LEN    = (Work(JSPARD)-Work(J))/2

            ! New lengths that obtain after completion of this pass
            Work(J     ) = Work(J     ) + LEN
            Work(JSPARD) = Work(JSPARD) - LEN

            ! A negative length means J receives from partner
            if(Rank(J    ) == MyPE) then
               NOP(1,KPASS) = -LEN
               NOP(2,KPASS) = Rank(JSPARD) ! Partners rank
               MyNewWork    = Work(J)
               MyBufSize    = max(MyBufSize,MyNewWork)
            endif

            ! If I am the partner, I will send to J
            if(Rank(JSPARD) == MyPE) then
               NOP(1,KPASS) = LEN
               NOP(2,KPASS) = Rank(J)  ! Js rank
               MyNewWork    = Work(JSPARD)
               MyBufSize    = max(MyBufSize,MyNewWork)
            endif

         enddo FOLD

      enddo PASSES

    end subroutine CreateStrategy

  end subroutine MAPL_BalanceCreate

!---------------------------------------------------------------------------

  subroutine MAPL_BalanceDestroy(Handle, rc)
    integer, optional, intent(IN ) :: Handle
    integer, optional, intent(OUT) :: rc

    integer :: Handle_

    if (present(Handle)) then
       _ASSERT(Handle>=0, 'Handle is less than 0')
       _ASSERT(Handle<=MAX_NUM_STRATEGIES,'Handle is greater than MAX_NUM_STRATEGIES')
       Handle_ = Handle
    else
       ! If we do not pass in a Handle, assume we wish to destroy
       ! the default Strategy which has a Handle of 0
       Handle_ = 0
    end if

    if(associated(THE_STRATEGIES(Handle_)%NOP)) &
         deallocate(THE_STRATEGIES(Handle_)%NOP)

    nullify(THE_STRATEGIES(Handle_)%NOP)

    if(associated(THE_STRATEGIES(Handle_)%CURSOR_DIST)) then
       deallocate(THE_STRATEGIES(Handle_)%CURSOR_DIST)
       nullify(THE_STRATEGIES(Handle_)%CURSOR_DIST)
    end if

    if(associated(THE_STRATEGIES(Handle_)%CURSOR_RETR)) then
       deallocate(THE_STRATEGIES(Handle_)%CURSOR_RETR)
       nullify(THE_STRATEGIES(Handle_)%CURSOR_RETR)
    end if

    THE_STRATEGIES(Handle_)%UNBALANCED_LENGTH =-1
    THE_STRATEGIES(Handle_)%BALANCED_LENGTH   =-1
    THE_STRATEGIES(Handle_)%BUFFER_LENGTH     =-1
    THE_STRATEGIES(Handle_)%PASSES            =-1
    THE_STRATEGIES(Handle_)%COMM              =-1

    _RETURN(LDB_SUCCESS)
  end subroutine MAPL_BalanceDestroy

!---------------------------------------------------------------------------

  subroutine MAPL_BalanceGet(Handle, BalLen, BufLen, Passes, Comm, rc)
    integer,           intent(IN ) :: Handle
    integer, optional, intent(OUT) :: BalLen, BufLen, Passes, Comm
    integer, optional, intent(OUT) :: rc

    _ASSERT(Handle>=0, 'Handle is less than 0')
    _ASSERT(Handle<=MAX_NUM_STRATEGIES,'Handle is greater than MAX_NUM_STATEGIES')

    _ASSERT(associated(THE_STRATEGIES(Handle)%NOP),'needs informative message')

    if(present(BalLen)) &
         BalLen = THE_STRATEGIES(Handle)%BALANCED_LENGTH
    if(present(BufLen)) &
         BufLen = THE_STRATEGIES(Handle)%BUFFER_LENGTH
    if(present(Passes)) &
         Passes = THE_STRATEGIES(Handle)%PASSES
    if(present(Comm  )) &
         Comm   = THE_STRATEGIES(Handle)%COMM

    _RETURN(LDB_SUCCESS)
  end subroutine MAPL_BalanceGet

end module MAPL_LOADBALANCEMOD
