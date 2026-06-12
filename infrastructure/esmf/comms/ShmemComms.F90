#include "MAPL.h"

!> @file ShmemComms.F90
!> Module: mapl_ShmemComms_mod
!>
!> Provides shared-memory-aware broadcast operations and round-robin
!> PE assignment, layering on top of mapl_comms_mod and mapl_shmem_mod.
!>
!> The non-blocking request machinery (CommRequest, CreateRequest,
!> ArrayIGather, ArrayIScatter, CollectiveWait, ArrPtr) is
!> kept private here as an implementation detail used by mapl_GridComms_mod
!> (infrastructure/geom/geom/GridComms.F90) for the 3-D collective routines.
!>
!> Grid-argument collectives (mapl_CollectiveGather3D, mapl_CollectiveScatter3D)
!> live in mapl_GridComms_mod since they require mapl_GridGetGlobal_mod.

module mapl_ShmemComms_mod

   ! TODO: pchakrab - we have some MAPL_ names here from mapl_shmem_mod

   use ESMF, only: ESMF_DELayout, ESMF_DELayoutGet
   use ESMF, only: ESMF_MAXSTR, ESMF_SUCCESS
   use ESMF, only: ESMF_VM, ESMF_VMGet, ESMF_VMGetCurrent
   use ESMF, only: ESMF_Grid, ESMF_GridGet
   use ESMF, only: ESMF_DistGrid, ESMF_DistGridGet
   use ESMF, only: ESMF_KIND_I4, ESMF_KIND_R4, ESMF_KIND_R8
   use mapl_comms_mod, only: comms_gatherv, comms_scatterv, ROOT_PROCESS_ID
   ! TODO: pchakrab - we have some MAPL_ names here from mapl_shmem_mod
   use mapl_shmem_mod, only: MAPL_ShmInitialized, SyncSharedMemory
   use mapl_shmem_mod, only: BroadcastToNodes, MAPL_NodeRankList, GetNewRank
   use MAPL_Constants, only: MAPL_Unknown, MAPL_IsGather, MAPL_IsScatter, MAPL_UNDEF
   use mapl_ErrorHandling_mod, only: MAPL_Assert, MAPL_Verify, MAPL_Return
   use mpi
   use, intrinsic :: iso_fortran_env, only: REAL64

   implicit none
   private

   ! ----------------------------------------------------------------
   ! Private implementation — request machinery used by mapl_GridComms_mod
   ! ----------------------------------------------------------------

   type ArrPtr
      real, pointer :: A(:, :)
   end type ArrPtr

   type CommRequest
      integer, pointer :: i1(:), in(:), j1(:), jn(:), im(:), jm(:)
      integer :: im_world, jm_world, im0, jm0
      integer, pointer :: recv(:) => null()
      integer, pointer :: send(:) => null()
      real, pointer :: var(:) => null()
      real, pointer :: DstArray(:, :) => null()
      real, pointer :: Local_Array(:, :) => null()
      real, pointer :: Trans_Array(:, :, :) => null()
      real, pointer :: Read_Array(:, :) => null()
      type(ArrPtr), pointer :: Buff(:)
      integer :: nDEs, MYPE, comm, root
      logical :: active = .false., amRoot = .false.
      logical :: IsPrePosted
      integer :: RequestType = MAPL_Unknown
      integer :: tag, s_rqst
   end type CommRequest

   ! ----------------------------------------------------------------
   ! Public API
   ! ----------------------------------------------------------------

   !> Broadcast a scalar or array from a root PE to all PEs in a
   !> DELayout or VM communicator.
   public :: CommsBcast

   !> Broadcast from root to shared-memory nodes; falls back to
   !> MPI broadcast when shared memory is not initialised.
   public :: BcastShared

   !> Round-robin assignment of PEs across NUMA nodes.
   public :: RoundRobinPEList

   ! These are accessible to mapl_GridComms_mod via use association
   public :: CommRequest
   public :: ArrPtr
   public :: CreateRequest
   public :: ArrayIGather
   public :: ArrayIScatter
   public :: CollectiveWait

   ! ----------------------------------------------------------------
   ! Generic interfaces — bcast
   ! ----------------------------------------------------------------

   interface CommsBcast
      module procedure comms_bcast_STRING_0
      module procedure comms_bcast_L4_0
      module procedure comms_bcast_I4_0
      module procedure comms_bcast_R4_0
      module procedure comms_bcast_R8_0
      module procedure comms_bcast_I4_1
      module procedure comms_bcast_R4_1
      module procedure comms_bcast_R8_1
      module procedure comms_bcast_I4_2
      module procedure comms_bcast_R4_2
      module procedure comms_bcast_R8_2
      module procedure comms_bcast_vm_STRING_0
      module procedure comms_bcast_vm_L4_0
      module procedure comms_bcast_vm_I4_0
      module procedure comms_bcast_vm_R4_0
      module procedure comms_bcast_vm_R8_0
      module procedure comms_bcast_vm_I4_1
      module procedure comms_bcast_vm_R4_1
      module procedure comms_bcast_vm_R8_1
      module procedure comms_bcast_vm_I4_2
      module procedure comms_bcast_vm_R4_2
      module procedure comms_bcast_vm_R8_2
   end interface CommsBcast

   interface BcastShared
      module procedure bcast_shared_1DR4
      module procedure bcast_shared_1DR8
      module procedure bcast_shared_2DI4
      module procedure bcast_shared_2DR4
      module procedure bcast_shared_2DR8
   end interface BcastShared

   interface ArrayIGather
      module procedure ArrayIGather_R4_2
   end interface ArrayIGather

   interface ArrayIScatter
      module procedure ArrayIScatter_R4_2
   end interface ArrayIScatter

contains

   ! ================================================================
   ! Broadcast — layout dispatcher
   ! ================================================================

   subroutine comms_bcast_STRING_0(layout, data, N, root, RC)
      type(ESMF_DELayout) :: layout
      character(len=*), intent(inout) :: data
      integer, intent(in) :: N
      integer, intent(in) :: root
      integer, optional, intent(out) :: RC

      integer :: status
      type(ESMF_VM) :: vm

      call ESMF_DELayoutGet(layout, vm=vm, _RC)
      call CommsBcast(vm, data=data, N=N, root=root, _RC)
      _RETURN(ESMF_SUCCESS)
   end subroutine comms_bcast_STRING_0

   subroutine comms_bcast_vm_STRING_0(vm, data, N, root, RC)
      type(ESMF_VM) :: vm
      character(len=*), intent(inout) :: data
      integer, intent(in) :: N
      integer, intent(in) :: root
      integer, optional, intent(out) :: RC

      character(len=N) :: tmpString
      integer :: slen
      integer :: status
      integer :: comm
      integer :: deId

      call ESMF_VMGet(vm, mpiCommunicator=comm, localPet=deId, _RC)

      tmpString = data
      if (deId == root) slen = len_trim(tmpString)

      call MPI_Bcast(slen, 1, MPI_INTEGER, root, comm, status)
      _VERIFY(status)

      _ASSERT(slen <= N, 'exceeded string length')

      call MPI_Bcast(tmpString, slen, MPI_BYTE, root, comm, status)
      _VERIFY(status)

      data = ""
      data = tmpString(1:slen)

      _RETURN(ESMF_SUCCESS)
   end subroutine comms_bcast_vm_STRING_0

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

   subroutine bcast_shared_1DR4(vm, data, N, root, RootOnly, RC)
      type(ESMF_VM) :: vm
      real, pointer, intent(inout) :: data(:)
      integer, intent(in) :: N
      integer, optional, intent(in) :: root
      logical, intent(in) :: RootOnly
      integer, optional, intent(out) :: RC

      integer :: status

      if (.not. MAPL_ShmInitialized) then
         if (RootOnly) then
            _RETURN(ESMF_SUCCESS)
         end if
         call CommsBcast(vm, data=data, N=N, root=root, _RC)
         _RETURN(status)
      else
         call SyncSharedMemory(_RC)
         call BroadcastToNodes(data, N=N, root=root, _RC)
         call SyncSharedMemory(_RC)
      end if

      _RETURN(ESMF_SUCCESS)
   end subroutine bcast_shared_1DR4

   subroutine bcast_shared_1DR8(vm, data, N, root, RootOnly, RC)
      type(ESMF_VM) :: vm
      real(kind=REAL64), pointer, intent(inout) :: data(:)
      integer, intent(in) :: N
      integer, optional, intent(in) :: root
      logical, intent(in) :: RootOnly
      integer, optional, intent(out) :: RC

      integer :: status

      if (.not. MAPL_ShmInitialized) then
         if (RootOnly) then
            _RETURN(ESMF_SUCCESS)
         end if
         call CommsBcast(vm, data=data, N=N, root=root, _RC)
      else
         call SyncSharedMemory(_RC)
         call BroadcastToNodes(data, N=N, root=root, _RC)
         call SyncSharedMemory(_RC)
      end if

      _RETURN(ESMF_SUCCESS)
   end subroutine bcast_shared_1DR8

   subroutine bcast_shared_2DR4(vm, data, N, root, RootOnly, RC)
      type(ESMF_VM) :: vm
      real, pointer, intent(inout) :: data(:, :)
      integer, intent(in) :: N
      integer, optional, intent(in) :: root
      logical, intent(in) :: RootOnly
      integer, optional, intent(out) :: RC

      integer :: status

      if (.not. MAPL_ShmInitialized) then
         if (RootOnly) then
            _RETURN(ESMF_SUCCESS)
         end if
         call CommsBcast(vm, data=data, N=N, root=root, _RC)
         _RETURN(status)
      else
         call SyncSharedMemory(_RC)
         call BroadcastToNodes(data, N=N, root=root, _RC)
         call SyncSharedMemory(_RC)
      end if

      _RETURN(ESMF_SUCCESS)
   end subroutine bcast_shared_2DR4

   subroutine bcast_shared_2DR8(vm, data, N, root, RootOnly, RC)
      type(ESMF_VM) :: vm
      real(kind=REAL64), pointer, intent(inout) :: data(:, :)
      integer, intent(in) :: N
      integer, optional, intent(in) :: root
      logical, intent(in) :: RootOnly
      integer, optional, intent(out) :: RC

      integer :: status

      if (.not. MAPL_ShmInitialized) then
         if (RootOnly) then
            _RETURN(ESMF_SUCCESS)
         end if
         call CommsBcast(vm, data=data, N=N, root=root, _RC)
      else
         call SyncSharedMemory(_RC)
         call BroadcastToNodes(data, N=N, root=root, _RC)
         call SyncSharedMemory(_RC)
      end if

      _RETURN(ESMF_SUCCESS)
   end subroutine bcast_shared_2DR8

   subroutine bcast_shared_2DI4(vm, data, N, root, RootOnly, RC)
      type(ESMF_VM) :: vm
      integer, pointer, intent(inout) :: data(:, :)
      integer, intent(in) :: N
      integer, optional, intent(in) :: root
      logical, intent(in) :: RootOnly
      integer, optional, intent(out) :: RC

      integer :: status

      if (.not. MAPL_ShmInitialized) then
         if (RootOnly) then
            _RETURN(ESMF_SUCCESS)
         end if
         call CommsBcast(vm, data=data, N=N, root=root, _RC)
      else
         call SyncSharedMemory(_RC)
         call BroadcastToNodes(data, N=N, root=root, _RC)
         call SyncSharedMemory(_RC)
      end if

      _RETURN(ESMF_SUCCESS)
   end subroutine bcast_shared_2DI4

   ! ================================================================
   ! RoundRobinPEList
   ! ================================================================

   subroutine RoundRobinPEList(List, nNodes, root, UseFirstRank, FirstRank, RC)
      integer, intent(out) :: List(:)
      integer, intent(in) :: nNodes
      integer, optional, intent(in) :: root
      logical, optional, intent(in) :: UseFirstRank
      integer, optional, intent(out) :: FirstRank
      integer, optional, intent(out) :: RC

      integer :: status
      integer, allocatable :: filled(:), nPerNode(:)
      integer :: i, N, nlist, locRoot
      logical :: gotFirstRank, lUseFirstRank

      locRoot = 1
      if (present(root)) locRoot = root

      lUseFirstRank = .true.
      if (present(UseFirstRank)) lUseFirstRank = UseFirstRank

      gotFirstRank = .false.

      allocate(filled(nNodes), nPerNode(nNodes), _STAT)
      do i = 1, nNodes
         nPerNode(i) = size(MAPL_NodeRankList(locRoot + i - 1)%rank)
         if (lUseFirstRank) then
            filled(i) = 0
         else
            filled(i) = GetNewRank(locRoot + i - 1, rc=status) - 1
            _VERIFY(status)
         end if
      end do

      nlist = size(List)
      N = 0
      do
         do i = 1, nNodes
            if (filled(i) < size(MAPL_NodeRankList(locRoot + i - 1)%rank)) then
               filled(i) = filled(i) + 1
               N = N + 1
               List(N) = MAPL_NodeRankList(locRoot + i - 1)%rank(filled(i))
               if (.not. gotFirstRank .and. present(FirstRank)) then
                  gotFirstRank = .true.
                  FirstRank = List(N)
               end if
            end if
            if (N == nlist) exit
         end do
         if (N == nlist) exit
         if (All(filled == nPerNode)) filled = 0
      end do

      deallocate(filled, nPerNode)
      _RETURN(ESMF_SUCCESS)
   end subroutine RoundRobinPEList

   ! ================================================================
   ! Private request machinery
   ! ================================================================

   subroutine CreateRequest(grid, root, request, tag, RequestType, &
        DstArray, PrePost, hw, RC)
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in) :: root
      type(CommRequest), intent(inout) :: request
      integer, intent(in) :: tag, RequestType
      real, target, optional, intent(in) :: DstArray(:, :)
      logical, optional, intent(in) :: PrePost
      integer, optional, intent(in) :: hw
      integer, optional, intent(out) :: RC

      integer :: status
      type(ESMF_VM) :: vm
      type(ESMF_DistGrid) :: distGrid
      integer, allocatable :: AL(:, :), AU(:, :)
      integer :: count
      integer :: displs
      integer :: N
      integer :: MYPE, nDEs
      integer :: gridRank
      integer :: comm
      integer :: hw_

      hw_ = 0
      if (present(hw)) hw_ = hw

      _ASSERT(.not. request%active, 'request is already active')

      call ESMF_VMGetCurrent(vm, _RC)
      call ESMF_VMGet(vm, mpiCommunicator=comm, _RC)
      call ESMF_VMGet(vm, localPet=MYPE, petcount=nDEs, _RC)
      call ESMF_GridGet(grid, dimCount=gridRank, _RC)

      _ASSERT(gridRank > 1, 'rank 1 is not supported')

      call ESMF_GridGet(grid, distGrid=distGrid, _RC)

      allocate(AL(gridRank, 0:nDEs - 1), _STAT)
      allocate(AU(gridRank, 0:nDEs - 1), _STAT)
      call ESMF_DistGridGet(distGrid, minIndexPDe=AL, maxIndexPDe=AU, _RC)

      allocate(request%i1(0:nDEs - 1), _STAT)
      allocate(request%in(0:nDEs - 1), _STAT)
      allocate(request%j1(0:nDEs - 1), _STAT)
      allocate(request%jn(0:nDEs - 1), _STAT)
      allocate(request%im(0:nDEs - 1), _STAT)
      allocate(request%jm(0:nDEs - 1), _STAT)
      allocate(request%recv(0:nDEs - 1), _STAT)
      allocate(request%send(0:nDEs - 1), _STAT)

      request%amRoot = (MYPE == root)
      request%active = .true.
      request%nDEs = nDEs
      request%MYPE = MYPE
      request%comm = comm
      request%root = root
      request%RequestType = RequestType
      request%tag = tag

      request%i1 = AL(1, :) - hw_
      request%in = AU(1, :) + hw_
      request%j1 = AL(2, :) - hw_
      request%jn = AU(2, :) + hw_
      request%im = request%in - request%i1 + 1
      request%jm = request%jn - request%j1 + 1

      request%im_world = request%in(nDEs - 1) - request%i1(0) + 1 - (2 * hw_)
      request%jm_world = request%jn(nDEs - 1) - request%j1(0) + 1 - (2 * hw_)
      request%im0 = request%in(MYPE) - request%i1(MYPE) + 1
      request%jm0 = request%jn(MYPE) - request%j1(MYPE) + 1

      if (present(PrePost)) then
         request%IsPrePosted = PrePost
      else
         request%IsPrePosted = .false.
      end if

      deallocate(AL, AU)

      if (RequestType == MAPL_IsGather) then
         if (request%amRoot) then
            if (present(DstArray)) then
               request%DstArray => DstArray
               _ASSERT(All(shape(DstArray) == (/request%im_world, request%jm_world/)), 'inconsistent shape')
            else
               allocate(request%DstArray(request%im_world, request%jm_world), _STAT)
            end if
         end if
      elseif (RequestType == MAPL_IsScatter) then
         if (present(DstArray)) then
            request%DstArray => DstArray
            _ASSERT(All(shape(DstArray) == (/request%im0, request%jm0/)), 'inconsistent shape')
         else
            allocate(request%DstArray(request%im0, request%jm0), _STAT)
         end if
      else
         _FAIL('unsupported action')
      end if

      if (RequestType == MAPL_IsGather .and. request%amRoot) then
         allocate(request%var(0:request%im_world * request%jm_world - 1), _STAT)
      elseif (RequestType == MAPL_IsScatter) then
         allocate(request%var(0:request%im0 * request%jm0 - 1), _STAT)
      else
         allocate(request%var(1), _STAT)
      end if

      POST_REQUEST: if (request%IsPrePosted) then
         if (RequestType == MAPL_IsGather) then
            if (request%amRoot) then
               displs = 0
               do N = 0, nDEs - 1
                  count = request%im(N) * request%jm(N)
                  if (N /= MYPE) then
                     call MPI_IRecv(request%var(displs), count, MPI_REAL, &
                          N, tag, comm, request%recv(N), status)
                     _VERIFY(status)
                  end if
                  displs = displs + count
               end do
            end if
         else
            if (.not. request%amRoot) then
               call MPI_IRecv(request%var, size(request%var), MPI_REAL, &
                    request%root, tag, comm, request%recv(0), status)
               _VERIFY(status)
            end if
         end if
      end if POST_REQUEST

      _RETURN(ESMF_SUCCESS)
   end subroutine CreateRequest

   ! ================================================================

   subroutine ArrayIGather_R4_2(Local_Array, request, RC)
      real, intent(in) :: Local_Array(:, :)
      type(CommRequest), intent(inout) :: request
      integer, optional, intent(out) :: RC

      integer :: status
      integer :: i1, in, j1, jn

      allocate(request%Local_Array(size(Local_Array, 1), size(Local_Array, 2)), _STAT)
      request%Local_Array = Local_Array

      if (request%amRoot) then
         i1 = request%i1(request%MYPE)
         in = request%in(request%MYPE)
         j1 = request%j1(request%MYPE)
         jn = request%jn(request%MYPE)
         request%DstArray(i1:in, j1:jn) = Local_Array
      else
         call MPI_ISend(request%Local_Array, size(Local_Array), MPI_REAL, &
              request%root, request%tag, request%comm, request%send(0), status)
         _VERIFY(status)
      end if

      _RETURN(ESMF_SUCCESS)
   end subroutine ArrayIGather_R4_2

   ! ================================================================

   subroutine ArrayIScatter_R4_2(global_array, request, hw, RC)
      real, intent(in) :: global_array(:, :)
      type(CommRequest), intent(inout) :: request
      integer, optional, intent(in) :: hw
      integer, optional, intent(out) :: RC

      integer :: status
      integer :: i1, in, j1, jn
      integer :: N, count, hw_, j
      real, allocatable :: global_array_(:, :)

      hw_ = 0
      if (present(hw)) hw_ = hw

      if (request%amRoot) then
         if (hw_ > 0) then
            allocate(global_array_(1 - hw_:request%im_world + hw_, 1 - hw_:request%jm_world + hw_))
            global_array_(1:request%im_world, 1:request%jm_world) = global_array
            do j = 1, hw_
               global_array_(1 - j, :) = global_array_(request%im_world - j + 1, :)
               global_array_(request%im_world + j, :) = global_array_(j, :)
               global_array_(:, 1 - j) = MAPL_UNDEF
               global_array_(:, request%jm_world + j) = MAPL_UNDEF
            end do
         end if
         allocate(request%Buff(0:request%nDEs - 1))
         PEs: do N = 0, request%nDEs - 1
            count = request%im(N) * request%jm(N)
            i1 = request%i1(N)
            in = request%in(N)
            j1 = request%j1(N)
            jn = request%jn(N)
            if (N == request%MYPE) then
               if (hw_ > 0) then
                  request%DstArray = global_array_(i1:in, j1:jn)
               else
                  request%DstArray = global_array(i1:in, j1:jn)
               end if
            else
               allocate(request%Buff(N)%A(request%im(N), request%jm(N)))
               if (hw_ > 0) then
                  request%Buff(N)%A = global_array_(i1:in, j1:jn)
               else
                  request%Buff(N)%A = global_array(i1:in, j1:jn)
               end if
               call MPI_ISend(request%Buff(N)%A, count, MPI_REAL, &
                    N, request%tag, request%comm, request%send(N), status)
               _VERIFY(status)
            end if
         end do PEs
         if (hw_ > 0) deallocate(global_array_)
      end if

      _RETURN(ESMF_SUCCESS)
   end subroutine ArrayIScatter_R4_2

   ! ================================================================

   subroutine CollectiveWait(request, DstArray, RC)
      type(CommRequest), intent(inout) :: request
      real, pointer, optional :: DstArray(:, :)
      integer, optional, intent(out) :: RC

      integer :: status
      integer :: i, j, k, N, count

      REQUEST_TYPE: if (request%RequestType == MAPL_IsGather) then

         ROOT_GATH: if (request%amRoot) then
            k = 0
            PE_GATH: do N = 0, request%nDEs - 1
               count = request%im(N) * request%jm(N)
               if (request%MYPE /= N) then
                  if (request%IsPrePosted) then
                     call MPI_Wait(request%recv(N), MPI_STATUS_IGNORE, status)
                     _VERIFY(status)
                  else
                     call MPI_Recv(request%var(k), count, MPI_REAL, &
                          N, request%tag, request%comm, MPI_STATUS_IGNORE, status)
                     _VERIFY(status)
                  end if
                  do j = request%j1(N), request%jn(N)
                     do i = request%i1(N), request%in(N)
                        request%DstArray(i, j) = request%var(k)
                        k = k + 1
                     end do
                  end do
               else
                  k = k + count
               end if
            end do PE_GATH
            if (present(DstArray)) DstArray => request%DstArray
         else
            call MPI_Wait(request%send(0), MPI_STATUS_IGNORE, status)
            _VERIFY(status)
         end if ROOT_GATH

      elseif (request%RequestType == MAPL_IsScatter) then

         ROOT_SCAT: if (.not. request%amRoot) then
            if (request%IsPrePosted) then
               call MPI_Wait(request%recv(0), MPI_STATUS_IGNORE, status)
               _VERIFY(status)
            else
               call MPI_Recv(request%var, size(request%var), MPI_REAL, &
                    request%root, request%tag, request%comm, &
                    MPI_STATUS_IGNORE, status)
               _VERIFY(status)
            end if
            k = 0
            do j = 1, request%jm0
               do i = 1, request%im0
                  request%DstArray(i, j) = request%var(k)
                  k = k + 1
               end do
            end do
         else
            PE_SCAT: do N = 0, request%nDEs - 1
               if (N /= request%MYPE) then
                  call MPI_Wait(request%send(N), MPI_STATUS_IGNORE, status)
                  _VERIFY(status)
                  deallocate(request%Buff(N)%A)
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
   end subroutine CollectiveWait

end module mapl_ShmemComms_mod
