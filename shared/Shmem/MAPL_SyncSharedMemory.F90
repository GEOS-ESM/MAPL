#define SHM_SUCCESS  0
#include "unused_dummy.H"
#include "MAPL_ErrLog.h"

submodule (MAPL_Shmem) MAPL_SyncSharedMemory_smod
  use pflogger, only: logging, Logger
  use MAPL_ExceptionHandling
  use MAPL_Constants
  implicit none

contains

    module subroutine MAPL_SyncSharedMemory(rc)
       integer, optional, intent(  OUT) :: rc
      integer :: STATUS
      if(.not.MAPL_ShmInitialized) then
         _RETURN(SHM_SUCCESS)
      endif
!!! Make sure everyone on a node syncs
      call MPI_Barrier(MAPL_NodeComm, STATUS)
      _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')
      _RETURN(SHM_SUCCESS)
    end subroutine MAPL_SyncSharedMemory

end submodule MAPL_SyncSharedMemory_smod

