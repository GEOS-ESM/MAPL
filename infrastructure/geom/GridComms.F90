#include "MAPL.h"

!> @file GridComms.F90
!> Module: mapl_GridComms_mod
!>
!> Provides collective 3-D scatter/gather operations that operate on
!> ESMF_Grid objects. These require mapl_GridGetGlobal_mod (for global
!> grid dimension queries) and the private request machinery from
!> mapl_ShmemComms_mod, which is why they live in MAPL.geom rather
!> than MAPL.esmf.

module mapl_GridComms_mod

   use ESMF, only: ESMF_Grid, ESMF_VM, ESMF_VMGet, ESMF_VMGetCurrent, ESMF_SUCCESS
   use mapl_esmf_api, only: mapl_CommRequest, mapl_CreateRequest
   use mapl_esmf_api, only: mapl_ArrayIGather, mapl_ArrayIScatter
   use mapl_esmf_api, only: mapl_CollectiveWait, mapl_RoundRobinPEList
   use mapl_GridGetGlobal_mod, only: GridGetGlobalCellCountPerDim
   use mapl_Shmem_mod, only: MAPL_NodeRankList
   use MAPL_Constants, only: MAPL_IsGather, MAPL_IsScatter
   use mapl_ErrorHandling_mod, only: MAPL_Assert, MAPL_Verify, MAPL_Return

   implicit none
   private

   public :: mapl_CollectiveGather3D
   public :: mapl_CollectiveScatter3D

contains

   subroutine mapl_CollectiveGather3D(Grid, LocArray, GlobArray, CoresPerNode, rc)
      type(ESMF_Grid),  intent(INout) :: Grid
      real,             intent(IN)    :: LocArray(:,:,:)
      real, pointer                   :: GlobArray(:,:,:)
      integer, optional, intent(In)   :: CoresPerNode
      integer, optional, intent(OUT)  :: rc

      integer                :: status
      type(mapl_CommRequest) :: reqs(size(LocArray,3))
      integer                :: root(size(LocArray,3))
      integer                :: nNodes, nn
      integer                :: LM, L, nc, npes, mype, dims(5)
      integer, allocatable   :: dims_alloc(:)
      type(ESMF_VM)          :: VM
      integer                :: comm

      _ASSERT(.not. associated(GlobArray), 'GlobalArray already associated')

      call ESMF_VMGetCurrent(VM, RC=STATUS)
      _VERIFY(STATUS)
      call ESMF_VMGet(VM, petcount=npes, localpet=MYPE, mpiCommunicator=comm, RC=STATUS)
      _VERIFY(STATUS)

      LM     = size(LocArray,3)
      nNodes = size(MAPL_NodeRankList)
      call mapl_RoundRobinPEList(Root, nNodes, RC=STATUS)
      _VERIFY(STATUS)

      if (any(root == mype)) then
         call GridGetGlobalCellCountPerDim(grid, globalCellCountPerDim=dims_alloc, RC=STATUS)
         _VERIFY(STATUS)
         dims(1:size(dims_alloc)) = dims_alloc
         nc = count(Root == mype)
         allocate(GlobArray(dims(1), dims(2), nc), _STAT)
      else
         allocate(GlobArray(1, 1, 1), _STAT)
      end if

      nn = 0
      do L = 1, LM
         if (root(L) == mype) then
            nn = nn + 1
            call mapl_CreateRequest(GRID, Root(L), reqs(L), tag=L,    &
                                 RequestType=MAPL_IsGather,           &
                                 DstArray=GlobArray(:,:,nn),          &
                                 PrePost=.true., RC=STATUS)
            _VERIFY(STATUS)
         else
            call mapl_CreateRequest(GRID, Root(L), reqs(L), tag=L,    &
                                 RequestType=MAPL_IsGather,           &
                                 DstArray=GlobArray(:,:,1),           &
                                 PrePost=.true., RC=STATUS)
            _VERIFY(STATUS)
         end if
      end do

      do L = 1, LM
         call mapl_ArrayIGather(LocArray(:,:,L), reqs(L), RC=STATUS)
         _VERIFY(STATUS)
      end do

      do L = 1, LM
         call mapl_CollectiveWait(reqs(L), rc=status)
         _VERIFY(STATUS)
      end do

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(corespernode)
   end subroutine mapl_CollectiveGather3D

   ! ================================================================

   subroutine mapl_CollectiveScatter3D(Grid, GlobArray, LocArray, hw, rc)
      type(ESMF_Grid),  intent(IN)    :: Grid
      real, target,     intent(INOUT) :: LocArray(:,:,:)
      real,             intent(IN)    :: GlobArray(:,:,:)
      integer, optional, intent(IN)   :: hw
      integer, optional, intent(OUT)  :: rc

      integer                :: status
      type(mapl_CommRequest) :: reqs(size(LocArray,3))
      integer                :: root(size(LocArray,3))
      integer                :: nNodes
      integer                :: LM, L, nc, npes, mype, nn
      type(ESMF_VM)          :: VM
      logical                :: HaveGlobal
      integer                :: comm, hw_

      call ESMF_VMGetCurrent(VM, RC=STATUS)
      _VERIFY(STATUS)
      call ESMF_VMGet(VM, petcount=npes, localpet=MYPE, mpiCommunicator=comm, RC=STATUS)
      _VERIFY(STATUS)

      hw_ = 0
      if (present(hw)) hw_ = hw

      nNodes = size(MAPL_NodeRankList)
      call mapl_RoundRobinPEList(Root, nNodes, RC=STATUS)
      _VERIFY(STATUS)

      LM = size(LocArray,3)
      NC = count(Root == mype)
      HaveGlobal = NC > 0

      do L = 1, LM
         call mapl_CreateRequest(GRID, Root(L), reqs(L), tag=L,       &
                                  RequestType=MAPL_IsScatter,          &
                                  DstArray=LocArray(:,:,L),            &
                                  PrePost=.true., hw=hw_, RC=STATUS)
         _VERIFY(STATUS)
      end do

      if (HaveGlobal) then
         _ASSERT(size(GlobArray,3) == NC, 'inconsistent rank')
         nn = 0
         do L = 1, LM
            if (Root(L) == mype) then
               nn = nn + 1
               call mapl_ArrayIScatter(GlobArray(:,:,nn), reqs(L), hw=hw_, RC=STATUS)
               _VERIFY(STATUS)
               if (nn == NC) exit
            end if
         end do
      end if

      do L = 1, LM
         call mapl_CollectiveWait(reqs(L), rc=status)
         _VERIFY(STATUS)
      end do

      _RETURN(ESMF_SUCCESS)
   end subroutine mapl_CollectiveScatter3D

end module mapl_GridComms_mod
