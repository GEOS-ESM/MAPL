#define SHM_SUCCESS  0
#include "unused_dummy.H"
#include "MAPL_ErrLog.h"

submodule (MAPL_Shmem) MAPL_CoresPerNodeGet_smod
  use pflogger, only: logging, Logger
  use MAPL_ExceptionHandling
  use MAPL_Constants
  implicit none

contains

    integer module function MAPL_CoresPerNodeGet(comm, rc)
       integer,           intent(IN   ) :: comm
       integer, optional, intent(  OUT) :: RC

      integer :: status

      if ( MAPL_NodeComm == -1 ) then
           call MAPL_GetNodeInfo(comm, rc=STATUS )
           _VERIFY(STATUS)
      end if

      MAPL_CoresPerNodeGet = MAPL_CoresPerNodeUsed

      _RETURN(SHM_SUCCESS)
    end function MAPL_CoresPerNodeGet

  end submodule MAPL_CoresPerNodeGet_smod
