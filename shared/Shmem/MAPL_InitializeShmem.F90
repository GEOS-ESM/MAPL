#define SHM_SUCCESS  0
#include "unused_dummy.H"
#include "MAPL_ErrLog.h"

submodule (MAPL_Shmem) MAPL_InitializeShmem_smod
  use pflogger, only: logging, Logger
  use MAPL_ExceptionHandling
  use MAPL_Constants
  implicit none

contains

  module subroutine MAPL_InitializeShmem(rc)
       integer, optional, intent(OUT) :: rc

     integer :: STATUS

     _ASSERT(MAPL_NodeComm /= -1,'needs informative message')

     allocate(Segs(CHUNK),stat=STATUS)
     _ASSERT(STATUS==0,'needs informative message')
     Segs(:)%shmid = -1
     Segs(:)%addr=C_NULL_PTR

     MAPL_ShmInitialized=.true.

#ifdef DEBUG
     if(MAPL_AmNodeRoot) &
          print *, "MAPL_Shmem initialized for node ", MAPL_MyNodeNum
#endif

     _RETURN(SHM_SUCCESS)
  end subroutine MAPL_InitializeShmem

  end submodule MAPL_InitializeShmem_smod
