#include "MAPL.h"

!> @file ShmemComms.F90
!> Module: mapl_ShmemComms_mod
!>
!> Provides shared-memory-aware broadcast operations and round-robin
!> PE assignment, layering on top of mapl_Comms_mod and mapl_Shmem_mod.
!>
!> The non-blocking request machinery (mapl_CommRequest, mapl_CreateRequest,
!> mapl_ArrayIGather, mapl_ArrayIScatter, mapl_CollectiveWait, ArrPtr) is
!> kept private here as an implementation detail used by mapl_GridComms_mod
!> (infrastructure/geom/geom/GridComms.F90) for the 3-D collective routines.
!>
!> Grid-argument collectives (mapl_CollectiveGather3D, mapl_CollectiveScatter3D)
!> live in mapl_GridComms_mod since they require mapl_GridGetGlobal_mod.

module mapl_ShmemComms_mod

   use ESMF, only: ESMF_DELayout, ESMF_DELayoutGet, &
                    ESMF_MAXSTR, ESMF_SUCCESS, &
                    ESMF_VM, ESMF_VMGet, ESMF_VMGetCurrent, &
                    ESMF_Grid, ESMF_GridGet, &
                    ESMF_DistGrid, ESMF_DistGridGet, &
                    ESMF_KIND_I4, ESMF_KIND_R4, ESMF_KIND_R8
   use mapl_Comms_mod, only: comms_gatherv, comms_scatterv, ROOT_PROCESS_ID
    use mapl_Shmem_mod, only: MAPL_ShmInitialized, SyncSharedMemory, &
                               BroadcastToNodes, MAPL_NodeRankList, &
                               GetNewRank
   use MAPL_Constants, only: MAPL_Unknown, MAPL_IsGather, MAPL_IsScatter, MAPL_UNDEF
   use mapl_ErrorHandling_mod, only: MAPL_Assert, MAPL_Verify, MAPL_Return
   use mpi
   use, intrinsic :: iso_fortran_env, only: REAL64

   implicit none
   private

   ! ----------------------------------------------------------------
   ! Public API
   ! ----------------------------------------------------------------

   !> Broadcast a scalar or array from a root PE to all PEs in a
   !> DELayout or VM communicator.
   public :: mapl_CommsBcast

   !> Broadcast from root to shared-memory nodes; falls back to
   !> MPI broadcast when shared memory is not initialised.
   public :: mapl_BcastShared

   !> Round-robin assignment of PEs across NUMA nodes.
   public :: mapl_RoundRobinPEList

   !> Convenience alias kept for backward compatibility.
   public :: MAPL_ROOT

   integer, parameter :: MAPL_ROOT = ROOT_PROCESS_ID

   ! ----------------------------------------------------------------
   ! Private implementation — request machinery used by mapl_GridComms_mod
   ! ----------------------------------------------------------------

   type ArrPtr
      real, pointer :: A(:,:)
   end type ArrPtr

   type mapl_CommRequest
      integer, pointer :: i1(:), in(:), j1(:), jn(:), im(:), jm(:)
      integer          :: im_world, jm_world, im0, jm0
      integer, pointer :: recv(:) => null()
      integer, pointer :: send(:) => null()
      real, pointer    :: var(:) => null()
      real, pointer    :: DstArray(:,:) => null()
      real, pointer    :: Local_Array(:,:) => null()
      real, pointer    :: Trans_Array(:,:,:) => null()
      real, pointer    :: Read_Array(:,:) => null()
      type(ArrPtr), pointer :: Buff(:)
      integer          :: nDEs, MYPE, comm, root
      logical          :: active = .false., amRoot = .false.
      logical          :: IsPrePosted
      integer          :: RequestType = MAPL_Unknown
      integer          :: tag, s_rqst
   end type mapl_CommRequest

   ! These are accessible to mapl_GridComms_mod via use association
   public :: mapl_CommRequest
   public :: ArrPtr
   public :: mapl_CreateRequest
   public :: mapl_ArrayIGather
   public :: mapl_ArrayIScatter
   public :: mapl_CollectiveWait

   ! ----------------------------------------------------------------
   ! Generic interfaces — bcast
   ! ----------------------------------------------------------------

   interface mapl_CommsBcast
      module procedure mapl_CommsBcast_STRING_0
      module procedure mapl_CommsBcast_L4_0
      module procedure mapl_CommsBcast_I4_0
      module procedure mapl_CommsBcast_R4_0
      module procedure mapl_CommsBcast_R8_0
      module procedure mapl_CommsBcast_I4_1
      module procedure mapl_CommsBcast_R4_1
      module procedure mapl_CommsBcast_R8_1
      module procedure mapl_CommsBcast_I4_2
      module procedure mapl_CommsBcast_R4_2
      module procedure mapl_CommsBcast_R8_2
      module procedure mapl_CommsBcastVm_STRING_0
      module procedure mapl_CommsBcastVm_L4_0
      module procedure mapl_CommsBcastVm_I4_0
      module procedure mapl_CommsBcastVm_R4_0
      module procedure mapl_CommsBcastVm_R8_0
      module procedure mapl_CommsBcastVm_I4_1
      module procedure mapl_CommsBcastVm_R4_1
      module procedure mapl_CommsBcastVm_R8_1
      module procedure mapl_CommsBcastVm_I4_2
      module procedure mapl_CommsBcastVm_R4_2
      module procedure mapl_CommsBcastVm_R8_2
   end interface mapl_CommsBcast

   interface mapl_BcastShared
      module procedure mapl_BcastShared_1DR4
      module procedure mapl_BcastShared_1DR8
      module procedure mapl_BcastShared_2DI4
      module procedure mapl_BcastShared_2DR4
      module procedure mapl_BcastShared_2DR8
   end interface mapl_BcastShared

   interface mapl_ArrayIGather
      module procedure mapl_ArrayIGather_R4_2
   end interface mapl_ArrayIGather

   interface mapl_ArrayIScatter
      module procedure mapl_ArrayIScatter_R4_2
   end interface mapl_ArrayIScatter

contains

   ! ================================================================
   ! Broadcast — layout dispatcher
   ! ================================================================

   subroutine mapl_CommsBcast_STRING_0(layout, data, N, ROOT, RC)
      type(ESMF_DELayout)              :: layout
      character(len=*), intent(INOUT)  :: data
      integer,          intent(in)     :: N
      integer,          intent(in)     :: ROOT
      integer, optional, intent(out)   :: RC

      integer       :: status
      type(ESMF_VM) :: vm

      call ESMF_DELayoutGet(layout, vm=vm, rc=status)
      _VERIFY(STATUS)
      call mapl_CommsBcast(vm, data=data, N=N, Root=Root, RC=status)
      _VERIFY(STATUS)
      _RETURN(ESMF_SUCCESS)
   end subroutine mapl_CommsBcast_STRING_0

   subroutine mapl_CommsBcastVm_STRING_0(vm, data, N, ROOT, RC)
      type(ESMF_VM)                    :: vm
      character(len=*), intent(INOUT)  :: data
      integer,          intent(in)     :: N
      integer,          intent(in)     :: ROOT
      integer, optional, intent(out)   :: RC

      character(len=N) :: tmpString
      integer          :: slen
      integer          :: status
      integer          :: comm
      integer          :: deId

      call ESMF_VMGet(vm, mpiCommunicator=COMM, localPet=deId, rc=status)
      _VERIFY(STATUS)

      tmpString = data
      if (deId == Root) slen = len_trim(tmpString)

      call MPI_Bcast(slen, 1, MPI_INTEGER, ROOT, COMM, status)
      _VERIFY(STATUS)

      _ASSERT(slen <= N, 'exceeded string length')

      call MPI_Bcast(tmpString, slen, MPI_BYTE, ROOT, COMM, STATUS)
      _VERIFY(STATUS)

      data = ""
      data = tmpString(1:slen)

      _RETURN(ESMF_SUCCESS)
   end subroutine mapl_CommsBcastVm_STRING_0

   ! ================================================================
   ! bcast.H template expansions
   ! ================================================================

! Rank 0
#define RANK_ 0
#define VARTYPE_ 1
#include "bcast.H"

#define RANK_ 0
#define VARTYPE_ 2
#include "bcast.H"

#define RANK_ 0
#define VARTYPE_ 3
#include "bcast.H"

#define RANK_ 0
#define VARTYPE_ 4
#include "bcast.H"

! Rank 1
#define RANK_ 1
#define VARTYPE_ 1
#include "bcast.H"

#define RANK_ 1
#define VARTYPE_ 3
#include "bcast.H"

#define RANK_ 1
#define VARTYPE_ 4
#include "bcast.H"

! Rank 2
#define RANK_ 2
#define VARTYPE_ 1
#include "bcast.H"

#define RANK_ 2
#define VARTYPE_ 3
#include "bcast.H"

#define RANK_ 2
#define VARTYPE_ 4
#include "bcast.H"

   ! ================================================================
   ! BcastShared — shmem-aware broadcast variants
   ! ================================================================

   subroutine mapl_BcastShared_1DR4(VM, Data, N, Root, RootOnly, rc)
      type(ESMF_VM)                  :: VM
      real, pointer, intent(INOUT)   :: Data(:)
      integer,       intent(IN)      :: N
      integer, optional, intent(IN)  :: Root
      logical,       intent(IN)      :: RootOnly
      integer, optional, intent(OUT) :: rc

      integer :: status

      if (.not. MAPL_ShmInitialized) then
         if (RootOnly) then
            _RETURN(ESMF_SUCCESS)
         end if
         call mapl_CommsBcast(vm, DATA=Data, N=N, ROOT=Root, RC=status)
         _RETURN(STATUS)
      else
         call SyncSharedMemory(RC=STATUS)
         _VERIFY(STATUS)
         call BroadcastToNodes(Data, N=N, ROOT=Root, rc=status)
         _VERIFY(STATUS)
         call SyncSharedMemory(RC=STATUS)
         _VERIFY(STATUS)
      end if

      _RETURN(ESMF_SUCCESS)
   end subroutine mapl_BcastShared_1DR4

   subroutine mapl_BcastShared_1DR8(VM, Data, N, Root, RootOnly, rc)
      type(ESMF_VM)                             :: VM
      real(kind=REAL64), pointer, intent(INOUT) :: Data(:)
      integer,           intent(IN)             :: N
      integer, optional, intent(IN)             :: Root
      logical,           intent(IN)             :: RootOnly
      integer, optional, intent(OUT)            :: rc

      integer :: status

      if (.not. MAPL_ShmInitialized) then
         if (RootOnly) then
            _RETURN(ESMF_SUCCESS)
         end if
         call mapl_CommsBcast(vm, DATA=Data, N=N, ROOT=Root, _RC)
      else
         call SyncSharedMemory(_RC)
         call BroadcastToNodes(Data, N=N, ROOT=Root, _RC)
         call SyncSharedMemory(_RC)
      end if

      _RETURN(ESMF_SUCCESS)
   end subroutine mapl_BcastShared_1DR8

   subroutine mapl_BcastShared_2DR4(VM, Data, N, Root, RootOnly, rc)
      type(ESMF_VM)                  :: VM
      real, pointer, intent(INOUT)   :: Data(:,:)
      integer,       intent(IN)      :: N
      integer, optional, intent(IN)  :: Root
      logical,       intent(IN)      :: RootOnly
      integer, optional, intent(OUT) :: rc

      integer :: status

      if (.not. MAPL_ShmInitialized) then
         if (RootOnly) then
            _RETURN(ESMF_SUCCESS)
         end if
         call mapl_CommsBcast(vm, DATA=Data, N=N, ROOT=Root, RC=status)
         _RETURN(STATUS)
      else
         call SyncSharedMemory(RC=STATUS)
         _VERIFY(STATUS)
         call BroadcastToNodes(Data, N=N, ROOT=Root, rc=status)
         _VERIFY(STATUS)
         call SyncSharedMemory(RC=STATUS)
         _VERIFY(STATUS)
      end if

      _RETURN(ESMF_SUCCESS)
   end subroutine mapl_BcastShared_2DR4

   subroutine mapl_BcastShared_2DR8(VM, Data, N, Root, RootOnly, rc)
      type(ESMF_VM)                             :: VM
      real(kind=REAL64), pointer, intent(INOUT) :: Data(:,:)
      integer,           intent(IN)             :: N
      integer, optional, intent(IN)             :: Root
      logical,           intent(IN)             :: RootOnly
      integer, optional, intent(OUT)            :: rc

      integer :: status

      if (.not. MAPL_ShmInitialized) then
         if (RootOnly) then
            _RETURN(ESMF_SUCCESS)
         end if
         call mapl_CommsBcast(vm, DATA=Data, N=N, ROOT=Root, _RC)
      else
         call SyncSharedMemory(_RC)
         call BroadcastToNodes(Data, N=N, ROOT=Root, _RC)
         call SyncSharedMemory(_RC)
      end if

      _RETURN(ESMF_SUCCESS)
   end subroutine mapl_BcastShared_2DR8

   subroutine mapl_BcastShared_2DI4(VM, Data, N, Root, RootOnly, rc)
      type(ESMF_VM)                    :: VM
      integer, pointer, intent(INOUT)  :: Data(:,:)
      integer,          intent(IN)     :: N
      integer, optional, intent(IN)    :: Root
      logical,          intent(IN)     :: RootOnly
      integer, optional, intent(OUT)   :: rc

      integer :: status

      if (.not. MAPL_ShmInitialized) then
         if (RootOnly) then
            _RETURN(ESMF_SUCCESS)
         end if
         call mapl_CommsBcast(vm, DATA=Data, N=N, ROOT=Root, _RC)
      else
         call SyncSharedMemory(_RC)
         call BroadcastToNodes(Data, N=N, ROOT=Root, _RC)
         call SyncSharedMemory(_RC)
      end if

      _RETURN(ESMF_SUCCESS)
   end subroutine mapl_BcastShared_2DI4

   ! ================================================================
   ! RoundRobinPEList
   ! ================================================================

   subroutine mapl_RoundRobinPEList(List, nNodes, Root, UseFirstRank, FirstRank, RC)
      integer,           intent(OUT)  :: List(:)
      integer,           intent(IN)   :: nNodes
      integer, optional, intent(IN)   :: Root
      logical, optional, intent(IN)   :: UseFirstRank
      integer, optional, intent(out)  :: FirstRank
      integer, optional, intent(OUT)  :: RC

      integer              :: status
      integer, allocatable :: filled(:), nPerNode(:)
      integer              :: i, n, nlist, locRoot
      logical              :: gotFirstRank, lUseFirstRank

      locRoot = 1
      if (present(Root)) locRoot = Root

      lUseFirstRank = .true.
      if (present(UseFirstRank)) lUseFirstRank = UseFirstRank

      gotFirstRank = .false.

      allocate(filled(nNodes), nPerNode(nNodes), _STAT)
      do i = 1, nNodes
         nPerNode(i) = size(MAPL_NodeRankList(locRoot+i-1)%rank)
         if (lUseFirstRank) then
            filled(i) = 0
         else
             filled(i) = GetNewRank(locRoot+i-1, rc=status) - 1
            _VERIFY(status)
         end if
      end do

      nlist = size(list)
      n = 0
      do
         do i = 1, nNodes
            if (filled(i) < size(MAPL_NodeRankList(locRoot+i-1)%rank)) then
               filled(i) = filled(i) + 1
               n = n + 1
               list(n) = MAPL_NodeRankList(locRoot+i-1)%rank(filled(i))
               if (.not. gotFirstRank .and. present(FirstRank)) then
                  gotFirstRank = .true.
                  FirstRank = list(n)
               end if
            end if
            if (n == nlist) exit
         end do
         if (n == nlist) exit
         if (All(filled == nPerNode)) filled = 0
      end do

      deallocate(filled, nPerNode)
      _RETURN(ESMF_SUCCESS)
   end subroutine mapl_RoundRobinPEList

   ! ================================================================
   ! Private request machinery
   ! ================================================================

   subroutine mapl_CreateRequest(grid, Root, request, tag, RequestType, &
                                  DstArray, PrePost, hw, rc)
      type(ESMF_Grid),        intent(IN)    :: grid
      integer,                intent(IN)    :: Root
      type(mapl_CommRequest), intent(INOUT) :: request
      integer,                intent(IN)    :: tag, RequestType
      real, target, optional, intent(IN)    :: DstArray(:,:)
      logical,      optional, intent(IN)    :: PrePost
      integer,      optional, intent(IN)    :: hw
      integer,      optional, intent(OUT)   :: rc

      integer              :: status
      type(ESMF_VM)        :: VM
      type(ESMF_DistGrid)  :: distGrid
      integer, allocatable :: AL(:,:), AU(:,:)
      integer              :: count
      integer              :: displs
      integer              :: n
      integer              :: myPE, nDEs
      integer              :: gridRank
      integer              :: comm
      integer              :: hw_

      hw_ = 0
      if (present(hw)) hw_ = hw

      _ASSERT(.not. request%active, 'request is already active')

      call ESMF_VMGetCurrent(vm,                               RC=STATUS)
      _VERIFY(STATUS)
      call ESMF_VMGet(VM, mpiCommunicator=comm,                RC=STATUS)
      _VERIFY(STATUS)
      call ESMF_VMGet(VM, localpet=MYPE, petcount=nDEs,        RC=STATUS)
      _VERIFY(STATUS)
      call ESMF_GridGet(GRID, dimCount=gridRank,               rc=status)
      _VERIFY(STATUS)

      _ASSERT(gridRank > 1, 'rank 1 is not supported')

      call ESMF_GridGet(GRID, distGrid=distGrid, RC=STATUS); _VERIFY(STATUS)

      allocate(AL(gridRank, 0:nDEs-1), _STAT)
      allocate(AU(gridRank, 0:nDEs-1), _STAT)
      call ESMF_DistGridGet(distgrid, minIndexPDe=AL, maxIndexPDe=AU, RC=STATUS)
      _VERIFY(STATUS)

      allocate(request%i1(0:nDEs-1), _STAT)
      allocate(request%in(0:nDEs-1), _STAT)
      allocate(request%j1(0:nDEs-1), _STAT)
      allocate(request%jn(0:nDEs-1), _STAT)
      allocate(request%im(0:nDEs-1), _STAT)
      allocate(request%jm(0:nDEs-1), _STAT)
      allocate(request%RECV(0:nDEs-1), _STAT)
      allocate(request%SEND(0:nDEs-1), _STAT)

      request%amRoot      = (myPE == Root)
      request%active      = .true.
      request%nDEs        = nDEs
      request%myPE        = myPE
      request%comm        = comm
      request%root        = root
      request%RequestType = RequestType
      request%tag         = tag

      request%I1 = AL(1,:) - hw_
      request%In = AU(1,:) + hw_
      request%J1 = AL(2,:) - hw_
      request%Jn = AU(2,:) + hw_
      request%IM = request%IN - request%I1 + 1
      request%JM = request%JN - request%J1 + 1

      request%IM_WORLD = request%IN(nDEs-1) - request%I1(0) + 1 - (2*hw_)
      request%JM_WORLD = request%JN(nDEs-1) - request%J1(0) + 1 - (2*hw_)
      request%IM0      = request%IN(mype)   - request%I1(mype) + 1
      request%JM0      = request%JN(mype)   - request%J1(mype) + 1

      if (present(PrePost)) then
         request%IsPrePosted = PrePost
      else
         request%IsPrePosted = .false.
      end if

      deallocate(AL, AU)

      if (requestType == MAPL_IsGather) then
         if (request%amRoot) then
            if (present(DstArray)) then
               request%DstArray => DstArray
               _ASSERT(all(shape(DstArray) == (/request%IM_WORLD, request%JM_WORLD/)), 'inconsistent shape')
            else
               allocate(request%DstArray(request%IM_WORLD, request%JM_WORLD), _STAT)
            end if
         end if
      elseif (requestType == MAPL_IsScatter) then
         if (present(DstArray)) then
            request%DstArray => DstArray
            _ASSERT(all(shape(DstArray) == (/request%IM0, request%JM0/)), 'inconsistent shape')
         else
            allocate(request%DstArray(request%IM0, request%JM0), _STAT)
         end if
      else
         _FAIL('unsupported action')
      end if

      if (requestType == MAPL_IsGather .and. request%amRoot) then
         allocate(request%Var(0:request%IM_WORLD*request%JM_WORLD-1), _STAT)
      elseif (requestType == MAPL_IsScatter) then
         allocate(request%Var(0:request%IM0*request%JM0-1), _STAT)
      else
         allocate(request%Var(1), _STAT)
      end if

      POST_REQUEST: if (request%IsPrePosted) then
         if (requestType == MAPL_IsGather) then
            if (request%amRoot) then
               displs = 0
               do n = 0, nDEs-1
                  count = request%IM(n) * request%JM(n)
                  if (n /= mype) then
                     call MPI_IRecv(request%VAR(displs), count, MPI_REAL, &
                          n, tag, comm, request%recv(n), status)
                     _VERIFY(STATUS)
                  end if
                  displs = displs + count
               end do
            end if
         else
            if (.not. request%amRoot) then
               call MPI_IRecv(request%Var, size(request%Var), MPI_REAL, &
                    request%Root, tag, comm, request%recv(0), status)
               _VERIFY(STATUS)
            end if
         end if
      end if POST_REQUEST

      _RETURN(ESMF_SUCCESS)
   end subroutine mapl_CreateRequest

   ! ================================================================

   subroutine mapl_ArrayIGather_R4_2(local_array, request, rc)
      real,                    intent(IN)    :: local_array(:,:)
      type(mapl_CommRequest),  intent(INOUT) :: request
      integer, optional,       intent(OUT)   :: rc

      integer :: status
      integer :: i1, in, j1, jn

      allocate(request%local_array(size(LOCAL_ARRAY,1), size(LOCAL_ARRAY,2)), _STAT)
      request%local_array = local_array

      if (request%amRoot) then
         i1 = request%i1(request%mype)
         in = request%in(request%mype)
         j1 = request%j1(request%mype)
         jn = request%jn(request%mype)
         request%DstArray(i1:in, j1:jn) = local_array
      else
         call MPI_ISend(request%Local_Array, size(Local_Array), MPI_REAL, &
              request%root, request%tag, request%comm, request%send(0), status)
         _VERIFY(STATUS)
      end if

      _RETURN(ESMF_SUCCESS)
   end subroutine mapl_ArrayIGather_R4_2

   ! ================================================================

   subroutine mapl_ArrayIScatter_R4_2(global_array, request, hw, rc)
      real,                   intent(IN)    :: global_array(:,:)
      type(mapl_CommRequest), intent(INOUT) :: request
      integer, optional,      intent(IN)    :: hw
      integer, optional,      intent(OUT)   :: rc

      integer           :: status
      integer           :: i1, in, j1, jn
      integer           :: n, count, hw_, j
      real, allocatable :: global_array_(:,:)

      hw_ = 0
      if (present(hw)) hw_ = hw

      if (request%amRoot) then
         if (hw_ > 0) then
            allocate(Global_Array_(1-hw_:request%im_world+hw_, 1-hw_:request%jm_world+hw_))
            Global_Array_(1:request%im_world, 1:request%jm_world) = Global_Array
            do j = 1, hw_
               Global_Array_(1-j, :) = Global_Array_(request%im_world-j+1, :)
               Global_Array_(request%im_world+j, :) = Global_Array_(j, :)
               Global_Array_(:, 1-j) = MAPL_UNDEF
               Global_Array_(:, request%jm_world+j) = MAPL_UNDEF
            end do
         end if
         allocate(request%Buff(0:request%nDEs-1))
         PEs: do n = 0, request%nDEs-1
            count = request%IM(n) * request%JM(n)
            i1 = request%i1(n); in = request%in(n)
            j1 = request%j1(n); jn = request%jn(n)
            if (n == request%mype) then
               if (hw_ > 0) then
                  request%DstArray = Global_Array_(i1:in, j1:jn)
               else
                  request%DstArray = Global_Array(i1:in, j1:jn)
               end if
            else
               allocate(request%Buff(n)%A(request%im(n), request%jm(n)))
               if (hw_ > 0) then
                  request%Buff(n)%A = Global_Array_(i1:in, j1:jn)
               else
                  request%Buff(n)%A = Global_Array(i1:in, j1:jn)
               end if
               call MPI_ISend(request%Buff(n)%A, count, MPI_REAL, &
                    n, request%tag, request%comm, request%send(n), status)
               _VERIFY(STATUS)
            end if
         end do PEs
         if (hw_ > 0) deallocate(Global_Array_)
      end if

      _RETURN(ESMF_SUCCESS)
   end subroutine mapl_ArrayIScatter_R4_2

   ! ================================================================

   subroutine mapl_CollectiveWait(request, DstArray, rc)
      type(mapl_CommRequest), intent(INOUT) :: request
      real, pointer, optional               :: DstArray(:,:)
      integer, optional,      intent(OUT)   :: rc

      integer :: status
      integer :: i, j, k, n, count

      REQUEST_TYPE: if (request%RequestType == MAPL_IsGather) then

         ROOT_GATH: if (request%amRoot) then
            k = 0
            PE_GATH: do n = 0, request%nDEs-1
               count = request%IM(n) * request%JM(n)
               if (request%mype /= n) then
                  if (request%IsPrePosted) then
                     call MPI_Wait(request%recv(n), MPI_STATUS_IGNORE, status)
                     _VERIFY(STATUS)
                  else
                     call MPI_Recv(request%var(k), count, MPI_REAL, &
                          n, request%tag, request%comm, MPI_STATUS_IGNORE, status)
                     _VERIFY(STATUS)
                  end if
                  do J = request%J1(n), request%JN(n)
                     do I = request%I1(n), request%IN(n)
                        request%DstArray(I,J) = request%var(k)
                        k = k + 1
                     end do
                  end do
               else
                  k = k + count
               end if
            end do PE_GATH
            if (present(DstArray)) DstArray => request%DstArray
         else
            call MPI_WAIT(request%send(0), MPI_STATUS_IGNORE, status)
            _VERIFY(STATUS)
         end if ROOT_GATH

      elseif (request%RequestType == MAPL_IsScatter) then

         ROOT_SCAT: if (.not. request%amRoot) then
            if (request%IsPrePosted) then
               call MPI_Wait(request%recv(0), MPI_STATUS_IGNORE, status)
               _VERIFY(STATUS)
            else
               call MPI_Recv(request%Var, size(request%Var), MPI_REAL, &
                              request%Root, request%tag, request%comm, &
                              MPI_STATUS_IGNORE, status)
               _VERIFY(status)
            end if
            k = 0
            do J = 1, request%JM0
               do I = 1, request%IM0
                  request%DstArray(I,J) = request%var(k)
                  k = k + 1
               end do
            end do
         else
            PE_SCAT: do n = 0, request%nDEs-1
               if (n /= request%mype) then
                  call MPI_Wait(request%send(n), MPI_STATUS_IGNORE, status)
                  _VERIFY(STATUS)
                  deallocate(request%buff(n)%A)
               end if
            end do PE_SCAT
            deallocate(request%Buff)
         end if ROOT_SCAT

         if (present(DstArray)) DstArray => request%DstArray

      end if REQUEST_TYPE

      deallocate(request%var, request%recv, request%send)
      deallocate(request%i1, request%in, request%j1, request%jn)
      deallocate(request%im, request%jm)

      nullify(request%var, request%send, request%recv, request%DstArray)

      if (associated(request%Local_Array)) deallocate(request%Local_Array)
      nullify(request%Local_Array)

      request%active = .false.

      _RETURN(ESMF_SUCCESS)
   end subroutine mapl_CollectiveWait

end module mapl_ShmemComms_mod
