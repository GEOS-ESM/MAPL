#define SHM_SUCCESS  0
#include "unused_dummy.H"
#include "MAPL_ErrLog.h"

submodule (MAPL_Shmem) MAPL_FinalizeShmem_smod
  use pflogger, only: logging, Logger
  use MAPL_ExceptionHandling
  use MAPL_Constants
  implicit none

contains

 module subroutine MAPL_FinalizeShmem(rc)
       integer, optional, intent(OUT) :: rc
    integer      :: status, i
    type (c_ptr) :: buf

    if (allocated(MAPL_NodeRankList)) then
       do i=1,size(MAPL_NodeRankList)
            if (associated(MAPL_NodeRankList(i)%rank)) then
               deallocate(MAPL_NodeRankList(i)%rank)
               MAPL_NodeRankList(i)%rank=>NULL()
            end if
         end do
         deallocate(MAPL_NodeRankList)
      end if

      if (associated(Segs)) then
         do i=1,size(Segs)
            if(Segs(i)%shmid==-1) cycle

!!! Everyone detaches address from shared segment

            STATUS = shmdt(Segs(i)%addr)
            _ASSERT(STATUS /= -1,'needs informative message')

!!! Make sure everyone has finished detaching

            call MPI_Barrier(MAPL_NodeComm, STATUS)
            _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')

!!! The root processor destroys the segment

            if (MAPL_AmNodeRoot) then
               STATUS = shmctl(Segs(i)%shmid, IPC_RMID, buf)
               _ASSERT(STATUS /= -1,'needs informative message')
            end if
         end do

         deallocate(Segs,stat=STATUS)
         _ASSERT(STATUS==0,'needs informative message')
      end if

      MAPL_ShmInitialized=.false.

#ifdef DEBUG
      if(MAPL_AmNodeRoot) &
           print *, "MAPL_Shmem finalized for node ", MAPL_MyNodeNum
#endif

      call MPI_Comm_free(MAPL_NodeComm, status)
      _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')
      MAPL_NodeComm = -1

      if (MAPL_NodeRootsComm /= MPI_COMM_NULL) then
         call MPI_Comm_free(MAPL_NodeRootsComm, status)
         _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')
      end if
      MAPL_NodeRootsComm = -1

      MAPL_MyNodeNum=-1
      MAPL_AmNodeRoot=.false.

      MAPL_CoresPerNodeUsed=-1
      MAPL_CoresPerNodeMin=-1
      MAPL_CoresPerNodeMax=-1
      MAPL_NumNodes=-1

      _RETURN(SHM_SUCCESS)
    end subroutine MAPL_FinalizeShmem

  end submodule MAPL_FinalizeShmem_smod
