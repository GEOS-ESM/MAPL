#define SHM_SUCCESS  0
#include "unused_dummy.H"
#include "MAPL_ErrLog.h"

submodule (MAPL_Shmem) MAPL_AllocateShared_1DL4_smod
  use pflogger, only: logging, Logger
  use MAPL_ExceptionHandling
  use MAPL_Constants
  implicit none

contains

    module subroutine MAPL_AllocateShared_1DL4(Ptr, Shp, lbd, TransRoot, rc)
       logical, pointer,  intent(INOUT) :: Ptr(:)
       integer,           intent(IN   ) :: Shp(1)
       integer, optional, intent(IN   ) :: lbd(1)
       logical,           intent(IN   ) :: TransRoot
       integer, optional, intent(  OUT) :: rc

      integer :: status

      if(MAPL_ShmInitialized) then
         call MAPL_AllocNodeArray(Ptr, Shp, lbd, rc=STATUS)
         _VERIFY(STATUS)
      else
         if (TransRoot) then
            allocate(Ptr(Shp(1)),stat=status)
         else
            allocate(Ptr(0),stat=status)
         end if
         _VERIFY(STATUS)
      endif

      _RETURN(STATUS)

    end subroutine MAPL_AllocateShared_1DL4

end submodule MAPL_AllocateShared_1DL4_smod

