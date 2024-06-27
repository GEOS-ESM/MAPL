#define SHM_SUCCESS  0
#include "unused_dummy.H"
#include "MAPL_ErrLog.h"

submodule (MAPL_Shmem) MAPL_DeAllocNodeArray_2DR4_smod
  use pflogger, only: logging, Logger
  use MAPL_ExceptionHandling
  use MAPL_Constants
  implicit none

contains

    module subroutine MAPL_DeAllocNodeArray_2DR4(Ptr,rc)
       real(kind=REAL32),  pointer    :: Ptr(:,:)
       integer, optional, intent(OUT) :: rc

      type(c_ptr) :: Caddr
      integer     :: STATUS

      if(.not.MAPL_ShmInitialized) then
         _RETURN(MAPL_NoShm)
      endif

      Caddr = C_Loc(Ptr(lbound(Ptr,1),lbound(Ptr,2)))

      call ReleaseSharedMemory(Caddr,rc=STATUS)
      _VERIFY(STATUS)

      _RETURN(SHM_SUCCESS)
    end subroutine MAPL_DeAllocNodeArray_2DR4

end submodule MAPL_DeAllocNodeArray_2DR4_smod

