#define SHM_SUCCESS  0
#include "unused_dummy.H"
#include "MAPL_ErrLog.h"

submodule (MAPL_Shmem) ReleaseSharedMemory_smod
  use pflogger, only: logging, Logger
  use MAPL_ExceptionHandling
  use MAPL_Constants
  implicit none

contains

    module subroutine ReleaseSharedMemory(Caddr,rc)
       type(c_ptr),       intent(INOUT) :: Caddr
       integer, optional, intent(  OUT) :: rc

      integer        :: pos
      type (c_ptr)   :: buf
      integer        :: STATUS

!!! Find the segment in the segment list

      do pos=1,size(Segs)
         if(Segs(pos)%shmid == -1) cycle
         if(c_associated(Segs(pos)%addr,Caddr)) exit
      end do

!!! Everyone exits if it is not there

      _ASSERT(pos<=size(Segs),'needs informative message')

!!! The root processor destroys the segment

      if (MAPL_AmNodeRoot) then
         STATUS = shmctl(Segs(pos)%shmid, IPC_RMID, buf)
         _ASSERT(STATUS /= -1,'needs informative message')
      end if

!!! Everyone detaches address from shared segment

      status = shmdt(Caddr)
      _ASSERT(status /= -1,'needs informative message')

!!! Make sure everyone has finished detaching

      call MPI_Barrier(MAPL_NodeComm, STATUS)
      _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')

!!! The root processor destroys the segment
!
!     if (MAPL_AmNodeRoot) then
!        STATUS = shmctl(Segs(pos)%shmid, IPC_RMID, buf)
!        _ASSERT(STATUS /= -1,'needs informative message')
!     end if

!!! Free the position in the segment list

      Segs(pos)%shmid=-1
      Segs(pos)%addr=C_NULL_PTR

      _RETURN(SHM_SUCCESS)
    end subroutine ReleaseSharedMemory

end submodule ReleaseSharedMemory_smod

