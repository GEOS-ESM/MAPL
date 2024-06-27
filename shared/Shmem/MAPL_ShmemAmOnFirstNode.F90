#define SHM_SUCCESS  0
#include "unused_dummy.H"
#include "MAPL_ErrLog.h"

submodule (MAPL_Shmem) MAPL_ShmemAmOnFirstNode_smod
  use pflogger, only: logging, Logger
  use MAPL_ExceptionHandling
  use MAPL_Constants
  implicit none

contains

    module function MAPL_ShmemAmOnFirstNode(comm, rc) result(a)
       integer,           intent(IN   ) :: comm
       integer, optional, intent(  OUT) :: RC
       logical                          :: a

      integer :: status, rank

      if ( MAPL_NodeComm == -1 ) then
           call MAPL_GetNodeInfo(comm, rc=STATUS )
           _VERIFY(STATUS)
      end if

      a = .false.
      if (MAPL_MyNodeNum == 1) then
         if (MAPL_ShmInitialized) then
            a = .true.
         else
            call MPI_Comm_rank(comm, rank, STATUS)
            _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')
            a = (rank == 0)
         end if
      end if

      _RETURN(SHM_SUCCESS)
    end function MAPL_ShmemAmOnFirstNode

end submodule MAPL_ShmemAmOnFirstNode_smod
