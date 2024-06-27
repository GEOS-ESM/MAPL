#define SHM_SUCCESS  0
#include "unused_dummy.H"
#include "MAPL_ErrLog.h"

submodule (MAPL_Shmem) MAPL_GetNodeInfo_smod
  use pflogger, only: logging, Logger
  use MAPL_ExceptionHandling
  use MAPL_Constants
  implicit none

contains

  module subroutine MAPL_GetNodeInfo(comm, rc)
       integer,           intent(IN ) :: comm
       integer, optional, intent(OUT) :: rc

     integer :: STATUS

     if (MAPL_NodeComm == -1) then ! make sure that we do this only once
        MAPL_NodeComm = getNodeComm(comm, rc=STATUS)
        _VERIFY(STATUS)
     end if

     if (MAPL_NodeRootsComm == -1) then ! make sure that we do this only once
        MAPL_NodeRootsComm = getNodeRootsComm(comm, rc=STATUS)
        _VERIFY(STATUS)
     end if

     _RETURN(SHM_SUCCESS)
  end subroutine MAPL_GetNodeInfo

  end submodule MAPL_GetNodeInfo_smod
