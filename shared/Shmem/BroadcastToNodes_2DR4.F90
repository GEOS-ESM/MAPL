#define SHM_SUCCESS  0
#include "unused_dummy.H"
#include "MAPL_ErrLog.h"

submodule (MAPL_Shmem) BroadcastToNodes_2DR4_smod
  use pflogger, only: logging, Logger
  use MAPL_ExceptionHandling
  use MAPL_Constants
  implicit none

contains

    module subroutine BroadcastToNodes_2DR4(DATA,N,ROOT,rc)
       real(kind=REAL32), intent(INOUT) :: DATA(:,:)
       integer,           intent(IN   ) :: N
       integer,           intent(IN   ) :: ROOT
       integer, optional, intent(  OUT) :: rc

      integer :: STATUS

      real(kind=REAL32), allocatable :: ldata(:,:)

      if(.not.MAPL_ShmInitialized .or. MAPL_NodeRootsComm==MPI_COMM_NULL) THEN
         _RETURN(SHM_SUCCESS)
      end if

      allocate(ldata(size(data,1),size(data,2)),stat=status)
      _VERIFY(STATUS)
      ldata = data
      call MPI_Bcast(LDATA, N, MPI_REAL, ROOT, MAPL_NodeRootsComm, STATUS)
      _VERIFY(STATUS)
      data = ldata
      deallocate(ldata)

      _RETURN(SHM_SUCCESS)
    end subroutine BroadcastToNodes_2DR4

end submodule BroadcastToNodes_2DR4_smod
