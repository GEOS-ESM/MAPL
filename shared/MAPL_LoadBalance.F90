

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

  !> Precomputed Alltoallv communication pattern for one direction.
  !  sendcounts/recvcounts are element counts; senddispls/recvdispls are
  !  0-based element displacements into the caller's buffer A.
  type TAlltoallvPlan
     integer, pointer :: sendcounts(:) => Null()
     integer, pointer :: senddispls(:) => Null()
     integer, pointer :: recvcounts(:) => Null()
     integer, pointer :: recvdispls(:) => Null()
  end type TAlltoallvPlan

  type TBalanceStrategy
     integer :: UNBALANCED_LENGTH=-1
     integer :: BALANCED_LENGTH  =-1
     integer :: BUFFER_LENGTH    =-1
     integer :: PASSES           =-1
     integer :: COMM             =-1
     integer :: NPES             =-1
     integer, pointer :: NOP(:,:)=>Null()
     type(TAlltoallvPlan) :: dist_plan  ! plan for MAPL_Distribute
     type(TAlltoallvPlan) :: retr_plan  ! plan for MAPL_Retrieve
  end type TBalanceStrategy

  integer,           parameter :: MAX_NUM_STRATEGIES=1000
  type(TBalanceStrategy), save, target :: THE_STRATEGIES(0:MAX_NUM_STRATEGIES)

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
! The redistribution is performed with a single MPI_Alltoallv call rather
! than a sequential loop of log2(N) point-to-point passes. The communication
! pattern is precomputed once in MAPL_BalanceCreate and stored in the
! strategy, so this routine is a thin wrapper around a single collective.
! Received data lands in a temporary buffer and is then copied into the
! correct positions of A (MPI_Alltoallv requires non-aliased send/recv buffers).

  subroutine MAPL_BalanceWork4(A, Idim, Direction, Handle, rc)
    real,              intent(INOUT) :: A(:)
    integer,           intent(IN   ) :: Idim, Direction
    integer, optional, intent(IN   ) :: Handle
    integer, optional, intent(  OUT) :: rc

    integer :: ISTRAT, STATUS, NPES
    integer :: Jdim, lev, r, cnt, sdisp, rdisp_flat
    integer :: total_recv, vtype
    type(TAlltoallvPlan), pointer :: plan
    integer, allocatable :: recvdispls_compact(:), rcounts_flat(:)
    integer, allocatable :: send_vtypes(:), scounts_w(:), sdispls_bytes(:)
    integer, allocatable :: rtypes(:), rdispls_bytes_w(:), rcounts_w(:)
    real,    allocatable :: recvbuf(:)
    integer(kind=MPI_ADDRESS_KIND) :: lb, extent

    Jdim = size(A)/Idim

    if (present(Handle)) then
       ISTRAT = Handle
    else
       ISTRAT = 0
    endif

    if (THE_STRATEGIES(ISTRAT)%PASSES <= 0) then
       _RETURN(LDB_SUCCESS)
    end if

    NPES = THE_STRATEGIES(ISTRAT)%NPES

    if (Direction==MAPL_Distribute) then
       plan => THE_STRATEGIES(ISTRAT)%dist_plan
    else
       plan => THE_STRATEGIES(ISTRAT)%retr_plan
    end if

    ! Total number of elements this rank will receive
    total_recv = sum(plan%recvcounts)

    if (Jdim==1) then

       ! ------------------------------------------------------------------
       ! 1-D case: simple MPI_Alltoallv into a compact temporary buffer,
       ! then scatter back into A at the precomputed displacements.
       ! ------------------------------------------------------------------
       allocate(recvbuf(total_recv))
       allocate(recvdispls_compact(0:NPES-1))
       allocate(rcounts_flat(0:NPES-1))

       ! Build compact (0-based) recv displacements into recvbuf
       rdisp_flat = 0
       do r=0,NPES-1
          recvdispls_compact(r) = rdisp_flat
          rcounts_flat(r)       = plan%recvcounts(r)
          rdisp_flat = rdisp_flat + rcounts_flat(r)
       end do

       call MPI_Alltoallv( &
            A,       plan%sendcounts, plan%senddispls,   MPI_REAL, &
            recvbuf, rcounts_flat,    recvdispls_compact, MPI_REAL, &
            THE_STRATEGIES(ISTRAT)%COMM, STATUS)
       _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')

       ! Scatter recvbuf back into the correct positions of A
       rdisp_flat = 0
       do r=0,NPES-1
          cnt = plan%recvcounts(r)
          if (cnt > 0) then
             sdisp = plan%recvdispls(r) + 1  ! convert to 1-based
             A(sdisp:sdisp+cnt-1) = recvbuf(rdisp_flat+1:rdisp_flat+cnt)
             rdisp_flat = rdisp_flat + cnt
          end if
       end do

       deallocate(recvbuf, recvdispls_compact, rcounts_flat)

    else

       ! ------------------------------------------------------------------
       ! Multi-level case (Jdim>1): send using MPI_Type_vector (strided)
       ! per partner rank, receive into a flat temporary buffer (one block
       ! per level per partner), then scatter back into strided positions.
       ! Uses MPI_Alltoallw so send and recv types can differ.
       ! ------------------------------------------------------------------
       call MPI_TYPE_GET_EXTENT(MPI_REAL, lb, extent, STATUS)
       _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')

       allocate(send_vtypes(0:NPES-1), source=MPI_DATATYPE_NULL)
       allocate(scounts_w(0:NPES-1),   source=0)
       allocate(sdispls_bytes(0:NPES-1), source=0)
       ! Receive flat: Jdim*count reals per partner
       allocate(rtypes(0:NPES-1),        source=MPI_REAL)
       allocate(rcounts_w(0:NPES-1),     source=0)
       allocate(rdispls_bytes_w(0:NPES-1), source=0)
       allocate(recvbuf(Jdim * total_recv))

       rdisp_flat = 0
       do r=0,NPES-1
          cnt = plan%sendcounts(r)
          if (cnt > 0) then
             call MPI_Type_VECTOR(Jdim, cnt, Idim, MPI_REAL, vtype, STATUS)
             _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')
             call MPI_TYPE_COMMIT(vtype, STATUS)
             _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')
             send_vtypes(r)    = vtype
             scounts_w(r)      = 1
             sdispls_bytes(r)  = plan%senddispls(r) * int(extent)
          end if
          cnt = plan%recvcounts(r)
          if (cnt > 0) then
             rcounts_w(r)        = Jdim * cnt
             rdispls_bytes_w(r)  = rdisp_flat * int(extent)
             rdisp_flat          = rdisp_flat + Jdim * cnt
          end if
       end do

       call MPI_Alltoallw( &
            A,       scounts_w, sdispls_bytes,   send_vtypes, &
            recvbuf, rcounts_w, rdispls_bytes_w, rtypes,      &
            THE_STRATEGIES(ISTRAT)%COMM, STATUS)
       _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')

       ! Scatter flat recvbuf back into strided positions of A
       ! recvbuf layout: for each partner r with recvcounts(r)>0,
       !   Jdim blocks of recvcounts(r) elements (level-major order)
       rdisp_flat = 0
       do r=0,NPES-1
          cnt = plan%recvcounts(r)
          if (cnt > 0) then
             sdisp = plan%recvdispls(r) + 1  ! 1-based column offset in A
             do lev=1,Jdim
                A(sdisp+(lev-1)*Idim : sdisp+(lev-1)*Idim+cnt-1) = &
                     recvbuf(rdisp_flat + (lev-1)*cnt + 1 : &
                             rdisp_flat + lev*cnt)
             end do
             rdisp_flat = rdisp_flat + Jdim * cnt
          end if
       end do

       do r=0,NPES-1
          if (send_vtypes(r) /= MPI_DATATYPE_NULL) then
             call MPI_TYPE_FREE(send_vtypes(r), STATUS)
             _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')
          end if
       end do

       deallocate(send_vtypes, scounts_w, sdispls_bytes)
       deallocate(rtypes, rcounts_w, rdispls_bytes_w, recvbuf)

    end if

    _RETURN(LDB_SUCCESS)
  end subroutine MAPL_BalanceWork4

!---------------------------------------------------------------------------
!>
! See MAPL_BalanceWork4 for full documentation. This variant operates on
! double-precision (real*8) arrays.

  subroutine MAPL_BalanceWork8(A, Idim, Direction, Handle, rc)
    real(kind=MAPL_R8), intent(INOUT) :: A(:)
    integer,            intent(IN   ) :: Idim, Direction
    integer, optional,  intent(IN   ) :: Handle
    integer, optional,  intent(  OUT) :: rc

    integer :: ISTRAT, STATUS, NPES
    integer :: Jdim, lev, r, cnt, sdisp, rdisp_flat
    integer :: total_recv, vtype
    type(TAlltoallvPlan), pointer :: plan
    integer, allocatable :: recvdispls_compact(:), rcounts_flat(:)
    integer, allocatable :: send_vtypes(:), scounts_w(:), sdispls_bytes(:)
    integer, allocatable :: rtypes(:), rdispls_bytes_w(:), rcounts_w(:)
    real(kind=MAPL_R8), allocatable :: recvbuf(:)
    integer(kind=MPI_ADDRESS_KIND) :: lb, extent

    Jdim = size(A)/Idim

    if (present(Handle)) then
       ISTRAT = Handle
    else
       ISTRAT = 0
    endif

    if (THE_STRATEGIES(ISTRAT)%PASSES <= 0) then
       _RETURN(LDB_SUCCESS)
    end if

    NPES = THE_STRATEGIES(ISTRAT)%NPES

    if (Direction==MAPL_Distribute) then
       plan => THE_STRATEGIES(ISTRAT)%dist_plan
    else
       plan => THE_STRATEGIES(ISTRAT)%retr_plan
    end if

    total_recv = sum(plan%recvcounts)

    if (Jdim==1) then

       allocate(recvbuf(total_recv))
       allocate(recvdispls_compact(0:NPES-1))
       allocate(rcounts_flat(0:NPES-1))

       rdisp_flat = 0
       do r=0,NPES-1
          recvdispls_compact(r) = rdisp_flat
          rcounts_flat(r)       = plan%recvcounts(r)
          rdisp_flat = rdisp_flat + rcounts_flat(r)
       end do

       call MPI_Alltoallv( &
            A,       plan%sendcounts, plan%senddispls,    MPI_DOUBLE_PRECISION, &
            recvbuf, rcounts_flat,    recvdispls_compact, MPI_DOUBLE_PRECISION, &
            THE_STRATEGIES(ISTRAT)%COMM, STATUS)
       _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')

       rdisp_flat = 0
       do r=0,NPES-1
          cnt = plan%recvcounts(r)
          if (cnt > 0) then
             sdisp = plan%recvdispls(r) + 1
             A(sdisp:sdisp+cnt-1) = recvbuf(rdisp_flat+1:rdisp_flat+cnt)
             rdisp_flat = rdisp_flat + cnt
          end if
       end do

       deallocate(recvbuf, recvdispls_compact, rcounts_flat)

    else

       call MPI_TYPE_GET_EXTENT(MPI_DOUBLE_PRECISION, lb, extent, STATUS)
       _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')

       allocate(send_vtypes(0:NPES-1), source=MPI_DATATYPE_NULL)
       allocate(scounts_w(0:NPES-1),   source=0)
       allocate(sdispls_bytes(0:NPES-1), source=0)
       allocate(rtypes(0:NPES-1),        source=MPI_DOUBLE_PRECISION)
       allocate(rcounts_w(0:NPES-1),     source=0)
       allocate(rdispls_bytes_w(0:NPES-1), source=0)
       allocate(recvbuf(Jdim * total_recv))

       rdisp_flat = 0
       do r=0,NPES-1
          cnt = plan%sendcounts(r)
          if (cnt > 0) then
             call MPI_Type_VECTOR(Jdim, cnt, Idim, MPI_DOUBLE_PRECISION, vtype, STATUS)
             _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')
             call MPI_TYPE_COMMIT(vtype, STATUS)
             _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')
             send_vtypes(r)    = vtype
             scounts_w(r)      = 1
             sdispls_bytes(r)  = plan%senddispls(r) * int(extent)
          end if
          cnt = plan%recvcounts(r)
          if (cnt > 0) then
             rcounts_w(r)        = Jdim * cnt
             rdispls_bytes_w(r)  = rdisp_flat * int(extent)
             rdisp_flat          = rdisp_flat + Jdim * cnt
          end if
       end do

       call MPI_Alltoallw( &
            A,       scounts_w, sdispls_bytes,   send_vtypes, &
            recvbuf, rcounts_w, rdispls_bytes_w, rtypes,      &
            THE_STRATEGIES(ISTRAT)%COMM, STATUS)
       _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')

       rdisp_flat = 0
       do r=0,NPES-1
          cnt = plan%recvcounts(r)
          if (cnt > 0) then
             sdisp = plan%recvdispls(r) + 1
             do lev=1,Jdim
                A(sdisp+(lev-1)*Idim : sdisp+(lev-1)*Idim+cnt-1) = &
                     recvbuf(rdisp_flat + (lev-1)*cnt + 1 : &
                             rdisp_flat + lev*cnt)
             end do
             rdisp_flat = rdisp_flat + Jdim * cnt
          end if
       end do

       do r=0,NPES-1
          if (send_vtypes(r) /= MPI_DATATYPE_NULL) then
             call MPI_TYPE_FREE(send_vtypes(r), STATUS)
             _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')
          end if
       end do

       deallocate(send_vtypes, scounts_w, sdispls_bytes)
       deallocate(rtypes, rcounts_w, rdispls_bytes_w, recvbuf)

    end if

    _RETURN(LDB_SUCCESS)
  end subroutine MAPL_BalanceWork8

!---------------------------------------------------------------------------
!>
! This routine creates a balancing strategy over an MPI communicator (Comm)
! given the work in the local rank (OrgLen). The strategy can be committed
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

! Allocate temporary space and zero-initialise so passes where this rank
! is not involved have NOP(1,PASS)=0 (no-op).
!-----------------------------------------------------------------------

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
           call DestroyPlan(THE_STRATEGIES(Balance)%dist_plan)
           call DestroyPlan(THE_STRATEGIES(Balance)%retr_plan)
           deallocate(THE_STRATEGIES(Balance)%NOP)
       end if
    end if

    if(present(BalLen)) BalLen =  MyNewWork
    if(present(BufLen)) BufLen =  MyBufSize

! Save the Strategy
!------------------

    allocate(THE_STRATEGIES(Balance)%NOP(2,KPASS))

    THE_STRATEGIES(Balance)%BALANCED_LENGTH   = MyNewWork
    THE_STRATEGIES(Balance)%BUFFER_LENGTH     = MyBufSize
    THE_STRATEGIES(Balance)%UNBALANCED_LENGTH = OrgLen
    THE_STRATEGIES(Balance)%PASSES            = KPASS
    THE_STRATEGIES(Balance)%COMM              = Comm
    THE_STRATEGIES(Balance)%NPES              = NPES
    THE_STRATEGIES(Balance)%NOP               = NOP(:,:KPASS)

! Build the Alltoallv plans for both directions from the NOP table.
!------------------------------------------------------------------

    call BuildAlltoallvPlans(THE_STRATEGIES(Balance)%NOP, KPASS, NPES, &
                             OrgLen, MyNewWork,                         &
                             THE_STRATEGIES(Balance)%dist_plan,         &
                             THE_STRATEGIES(Balance)%retr_plan)

    deallocate(NOP)

    _RETURN(LDB_SUCCESS)

  contains

    !> Simulate the sequential cursor walk for both Distribute and Retrieve
    !  directions over the NOP table to build Alltoallv send/recv count and
    !  displacement arrays (0-based element indices into buffer A).
    !
    !  Each rank sends to at most one partner and receives from at most one
    !  partner per pass. Across all passes a rank may communicate with
    !  multiple distinct partners; those are accumulated here.
    subroutine BuildAlltoallvPlans(NOP, KPASS, NPES, OrgLen, BalLen, &
                                   dist_plan, retr_plan)
      integer,             intent(IN)    :: NOP(:,:), KPASS, NPES, OrgLen, BalLen
      type(TAlltoallvPlan),intent(INOUT) :: dist_plan, retr_plan

      integer :: PASS, CURSOR, LENGTH, PARTNER

      allocate(dist_plan%sendcounts(0:NPES-1), source=0)
      allocate(dist_plan%senddispls(0:NPES-1), source=0)
      allocate(dist_plan%recvcounts(0:NPES-1), source=0)
      allocate(dist_plan%recvdispls(0:NPES-1), source=0)
      allocate(retr_plan%sendcounts(0:NPES-1), source=0)
      allocate(retr_plan%senddispls(0:NPES-1), source=0)
      allocate(retr_plan%recvcounts(0:NPES-1), source=0)
      allocate(retr_plan%recvdispls(0:NPES-1), source=0)

      ! -- Distribute direction (forward pass) --
      CURSOR = OrgLen + 1
      do PASS=1,KPASS
         LENGTH  = abs(NOP(1,PASS))
         PARTNER = NOP(2,PASS)
         if (NOP(1,PASS) > 0) then          ! SEND: cursor decrements first
            CURSOR = CURSOR - LENGTH
            dist_plan%sendcounts(PARTNER) = dist_plan%sendcounts(PARTNER) + LENGTH
            dist_plan%senddispls(PARTNER) = CURSOR - 1  ! 0-based
         else if (NOP(1,PASS) < 0) then     ! RECV: cursor increments after
            dist_plan%recvcounts(PARTNER) = dist_plan%recvcounts(PARTNER) + LENGTH
            dist_plan%recvdispls(PARTNER) = CURSOR - 1  ! 0-based
            CURSOR = CURSOR + LENGTH
         end if
      end do

      ! -- Retrieve direction (reverse pass, signs swapped) --
      CURSOR = BalLen + 1
      do PASS=KPASS,1,-1
         LENGTH  = abs(NOP(1,PASS))
         PARTNER = NOP(2,PASS)
         if (NOP(1,PASS) < 0) then          ! SEND (sign reversed)
            CURSOR = CURSOR - LENGTH
            retr_plan%sendcounts(PARTNER) = retr_plan%sendcounts(PARTNER) + LENGTH
            retr_plan%senddispls(PARTNER) = CURSOR - 1
         else if (NOP(1,PASS) > 0) then     ! RECV (sign reversed)
            retr_plan%recvcounts(PARTNER) = retr_plan%recvcounts(PARTNER) + LENGTH
            retr_plan%recvdispls(PARTNER) = CURSOR - 1
            CURSOR = CURSOR + LENGTH
         end if
      end do

    end subroutine BuildAlltoallvPlans

    subroutine DestroyPlan(plan)
      type(TAlltoallvPlan), intent(INOUT) :: plan
      if (associated(plan%sendcounts)) deallocate(plan%sendcounts)
      if (associated(plan%senddispls)) deallocate(plan%senddispls)
      if (associated(plan%recvcounts)) deallocate(plan%recvcounts)
      if (associated(plan%recvdispls)) deallocate(plan%recvdispls)
      nullify(plan%sendcounts, plan%senddispls, plan%recvcounts, plan%recvdispls)
    end subroutine DestroyPlan

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

    if (associated(THE_STRATEGIES(Handle_)%NOP)) then
       call DestroyPlan(THE_STRATEGIES(Handle_)%dist_plan)
       call DestroyPlan(THE_STRATEGIES(Handle_)%retr_plan)
       deallocate(THE_STRATEGIES(Handle_)%NOP)
    end if

    nullify(THE_STRATEGIES(Handle_)%NOP)

    THE_STRATEGIES(Handle_)%UNBALANCED_LENGTH =-1
    THE_STRATEGIES(Handle_)%BALANCED_LENGTH   =-1
    THE_STRATEGIES(Handle_)%BUFFER_LENGTH     =-1
    THE_STRATEGIES(Handle_)%PASSES            =-1
    THE_STRATEGIES(Handle_)%COMM              =-1
    THE_STRATEGIES(Handle_)%NPES              =-1

    _RETURN(LDB_SUCCESS)

  contains

    subroutine DestroyPlan(plan)
      type(TAlltoallvPlan), intent(INOUT) :: plan
      if (associated(plan%sendcounts)) deallocate(plan%sendcounts)
      if (associated(plan%senddispls)) deallocate(plan%senddispls)
      if (associated(plan%recvcounts)) deallocate(plan%recvcounts)
      if (associated(plan%recvdispls)) deallocate(plan%recvdispls)
      nullify(plan%sendcounts, plan%senddispls, plan%recvcounts, plan%recvdispls)
    end subroutine DestroyPlan

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
