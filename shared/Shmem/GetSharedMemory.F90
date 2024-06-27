#define SHM_SUCCESS  0
#include "unused_dummy.H"
#include "MAPL_ErrLog.h"

submodule (MAPL_Shmem) GetSharedMemory_smod
  use pflogger, only: logging, Logger
  use MAPL_ExceptionHandling
  use MAPL_Constants
  implicit none

  interface
     subroutine perror(s) bind(c,name="perror")
       use, intrinsic :: ISO_C_BINDING
       character(c_char), intent(in) :: s(*)
     end subroutine perror
  end interface

contains

    module subroutine GetSharedMemory(Caddr,Len,rc)
       type(c_ptr),       intent(  OUT) :: Caddr
       integer,           intent(IN   ) :: Len
       integer, optional, intent(  OUT) :: rc

      integer                   :: status, pos
      integer(c_key_t)          :: key
      integer(c_size_t)         :: numBytes
      integer, parameter        :: WORD_SIZE = 4
      integer(c_int), parameter :: C_ZERO = 0
      integer(c_int), parameter :: myflg = int(o'666')
      integer(c_int), parameter :: shmflg = int(ior(IPC_CREAT,myflg))
      integer(c_key_t), parameter :: keypre = 456000000

!!! Get an empty spot in the list of allocated segments
!!! and use its index as the segment's key

      pos=1
      do while(pos<=size(Segs))
         if(Segs(pos)%shmid==-1) exit ! Found an available spot

         if(pos==size(Segs)) then ! Expand the segment list
            allocate(SegsNew(size(Segs)+CHUNK),stat=STATUS)
            _ASSERT(STATUS==0,'needs informative message')
            SegsNew(:)%shmid = -1
            SegsNew(:)%addr=C_NULL_PTR

            SegsNew(1:size(Segs)) = Segs

            deallocate(Segs,stat=STATUS)
            _ASSERT(STATUS==0,'needs informative message')

            Segs=>SegsNew
            nullify(SegsNew)
         end if

         pos = pos + 1
      end do

      key = keypre + pos

!!!  Create the segment in root and have other processors
!!!  get the segment id using the common key

      numBytes = WORD_SIZE*len

      if (MAPL_AmNodeRoot) then ! root process creates segment
         Segs(pos)%shmid = shmget(key, numBytes, shmflg)
         if (Segs(pos)%shmid < 0) then
            call perror('server shmget():'//C_NULL_CHAR)
            _FAIL('needs informative message')
         end if
         call MPI_Barrier(MAPL_NodeComm, STATUS)
         _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')
      else                 ! wait for create on root & get segment
         call MPI_Barrier(MAPL_NodeComm, STATUS)
         _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')
         Segs(pos)%shmid = shmget(key, numBytes, myflg)
         _ASSERT(Segs(pos)%shmid /= -1,'needs informative message')
      end if

!!! Everyone attaches the memory to their own C pointer

      Segs(pos)%addr= shmat(Segs(pos)%shmid, C_NULL_PTR, C_ZERO)

!!! Check that we have valid shared memory

      _ASSERT(c_associated(Segs(pos)%addr),'needs informative message')

!!! Return C address. It will be attached to a Fortran pointer
!!!  with rank overloads

      Caddr = Segs(pos)%addr

      _RETURN(SHM_SUCCESS)
    end subroutine GetSharedMemory

end submodule GetSharedMemory_smod

