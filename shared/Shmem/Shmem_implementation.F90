#define SHM_SUCCESS  0
#include "unused_dummy.H"
#include "MAPL_ErrLog.h"

submodule (MAPL_Shmem) Shmem_implementation
  use pflogger, only: logging, Logger
  use MAPL_ExceptionHandling
  implicit none

contains

  module procedure MAPL_GetNodeInfo

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
  end procedure MAPL_GetNodeInfo

  module procedure MAPL_InitializeShmem

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
  end procedure MAPL_InitializeShmem

 module procedure MAPL_FinalizeShmem
    integer      :: status, i
    type (c_ptr) :: buf

    if (allocated(MAPL_NodeRankList)) then
       do i=1,size(MAPL_NodeRankList)
            if (associated(MAPL_NodeRankList(i)%rank)) then
               deallocate(MAPL_NodeRankList(i)%rank)
               MAPL_NodeRankList(i)%rank=>NULL()
            end if
         end do
         deallocate(MAPL_NodeRankList)
      end if

      if (associated(Segs)) then
         do i=1,size(Segs)
            if(Segs(i)%shmid==-1) cycle

!!! Everyone detaches address from shared segment

            STATUS = shmdt(Segs(i)%addr)
            _ASSERT(STATUS /= -1,'needs informative message')

!!! Make sure everyone has finished detaching

            call MPI_Barrier(MAPL_NodeComm, STATUS)
            _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')

!!! The root processor destroys the segment

            if (MAPL_AmNodeRoot) then
               STATUS = shmctl(Segs(i)%shmid, IPC_RMID, buf)
               _ASSERT(STATUS /= -1,'needs informative message')
            end if
         end do

         deallocate(Segs,stat=STATUS)
         _ASSERT(STATUS==0,'needs informative message')
      end if

      MAPL_ShmInitialized=.false.

#ifdef DEBUG
      if(MAPL_AmNodeRoot) &
           print *, "MAPL_Shmem finalized for node ", MAPL_MyNodeNum
#endif

      call MPI_Comm_free(MAPL_NodeComm, status)
      _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')
      MAPL_NodeComm = -1

      if (MAPL_NodeRootsComm /= MPI_COMM_NULL) then
         call MPI_Comm_free(MAPL_NodeRootsComm, status)
         _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')
      end if
      MAPL_NodeRootsComm = -1

      MAPL_MyNodeNum=-1
      MAPL_AmNodeRoot=.false.

      MAPL_CoresPerNodeUsed=-1
      MAPL_CoresPerNodeMin=-1
      MAPL_CoresPerNodeMax=-1
      MAPL_NumNodes=-1

      _RETURN(SHM_SUCCESS)
    end procedure MAPL_FinalizeShmem

    module procedure MAPL_DeAllocNodeArray_1DL4

      type(c_ptr) :: Caddr
      integer     :: STATUS

      if(.not.MAPL_ShmInitialized) then
         _RETURN(MAPL_NoShm)
      endif

      Caddr = C_Loc(Ptr(lbound(Ptr,1)))

      call ReleaseSharedMemory(Caddr,rc=STATUS)
      _VERIFY(STATUS)

      _RETURN(SHM_SUCCESS)
    end procedure MAPL_DeAllocNodeArray_1DL4

    module procedure MAPL_DeAllocNodeArray_1DI4

      type(c_ptr) :: Caddr
      integer     :: STATUS

      if(.not.MAPL_ShmInitialized) then
         _RETURN(MAPL_NoShm)
      endif

      Caddr = C_Loc(Ptr(lbound(Ptr,1)))

      call ReleaseSharedMemory(Caddr,rc=STATUS)
      _VERIFY(STATUS)

      _RETURN(SHM_SUCCESS)
    end procedure MAPL_DeAllocNodeArray_1DI4

    module procedure MAPL_DeAllocNodeArray_2DI4

      type(c_ptr) :: Caddr
      integer     :: STATUS

      if(.not.MAPL_ShmInitialized) then
         _RETURN(MAPL_NoShm)
      endif

      Caddr = C_Loc(Ptr(lbound(Ptr,1),lbound(Ptr,2)))

      call ReleaseSharedMemory(Caddr,rc=STATUS)
      _VERIFY(STATUS)

      _RETURN(SHM_SUCCESS)
    end procedure MAPL_DeAllocNodeArray_2DI4

    module procedure MAPL_DeAllocNodeArray_3DI4

      type(c_ptr) :: Caddr
      integer     :: STATUS

      if(.not.MAPL_ShmInitialized) then
         _RETURN(MAPL_NoShm)
      endif

      Caddr = C_Loc(Ptr(lbound(Ptr,1),lbound(Ptr,2),lbound(Ptr,3)))

      call ReleaseSharedMemory(Caddr,rc=STATUS)
      _VERIFY(STATUS)

      _RETURN(SHM_SUCCESS)
    end procedure MAPL_DeAllocNodeArray_3DI4

    module procedure MAPL_DeAllocNodeArray_4DI4

      type(c_ptr) :: Caddr
      integer     :: STATUS

      if(.not.MAPL_ShmInitialized) then
         _RETURN(MAPL_NoShm)
      endif

      Caddr = C_Loc(Ptr(lbound(Ptr,1),lbound(Ptr,2),lbound(Ptr,3),lbound(Ptr,4)))

      call ReleaseSharedMemory(Caddr,rc=STATUS)
      _VERIFY(STATUS)

      _RETURN(SHM_SUCCESS)
    end procedure MAPL_DeAllocNodeArray_4DI4


    module procedure MAPL_DeAllocNodeArray_1DR4

      type(c_ptr) :: Caddr
      integer     :: STATUS

      if(.not.MAPL_ShmInitialized) then
         _RETURN(MAPL_NoShm)
      endif

      Caddr = C_Loc(Ptr(lbound(Ptr,1)))

      call ReleaseSharedMemory(Caddr,rc=STATUS)
      _VERIFY(STATUS)

      _RETURN(SHM_SUCCESS)
    end procedure MAPL_DeAllocNodeArray_1DR4

    module procedure MAPL_DeAllocNodeArray_2DR4

      type(c_ptr) :: Caddr
      integer     :: STATUS

      if(.not.MAPL_ShmInitialized) then
         _RETURN(MAPL_NoShm)
      endif

      Caddr = C_Loc(Ptr(lbound(Ptr,1),lbound(Ptr,2)))

      call ReleaseSharedMemory(Caddr,rc=STATUS)
      _VERIFY(STATUS)

      _RETURN(SHM_SUCCESS)
    end procedure MAPL_DeAllocNodeArray_2DR4

    module procedure MAPL_DeAllocNodeArray_3DR4

      type(c_ptr) :: Caddr
      integer     :: STATUS

      if(.not.MAPL_ShmInitialized) then
         _RETURN(MAPL_NoShm)
      endif

      Caddr = C_Loc(Ptr(lbound(Ptr,1),lbound(Ptr,2),lbound(Ptr,3)))

      call ReleaseSharedMemory(Caddr,rc=STATUS)
      _VERIFY(STATUS)

      _RETURN(SHM_SUCCESS)
    end procedure MAPL_DeAllocNodeArray_3DR4

    module procedure MAPL_DeAllocNodeArray_4DR4

      type(c_ptr) :: Caddr
      integer     :: STATUS

      if(.not.MAPL_ShmInitialized) then
         _RETURN(MAPL_NoShm)
      endif

      Caddr = C_Loc(Ptr(lbound(Ptr,1),lbound(Ptr,2),lbound(Ptr,3),lbound(Ptr,4)))
      call ReleaseSharedMemory(Caddr,rc=STATUS)
      _VERIFY(STATUS)

      _RETURN(SHM_SUCCESS)
    end procedure MAPL_DeAllocNodeArray_4DR4


    module procedure MAPL_DeAllocNodeArray_1DR8

      type(c_ptr) :: Caddr
      integer     :: STATUS

      if(.not.MAPL_ShmInitialized) then
         _RETURN(MAPL_NoShm)
      endif

      Caddr = C_Loc(Ptr(lbound(Ptr,1)))

      call ReleaseSharedMemory(Caddr,rc=STATUS)
      _VERIFY(STATUS)

      _RETURN(SHM_SUCCESS)
    end procedure MAPL_DeAllocNodeArray_1DR8

    module procedure MAPL_DeAllocNodeArray_2DR8

      type(c_ptr) :: Caddr
      integer     :: STATUS

      if(.not.MAPL_ShmInitialized) then
         _RETURN(MAPL_NoShm)
      endif

      Caddr = C_Loc(Ptr(lbound(Ptr,1),lbound(Ptr,2)))

      call ReleaseSharedMemory(Caddr,rc=STATUS)
      _VERIFY(STATUS)

      _RETURN(SHM_SUCCESS)
    end procedure MAPL_DeAllocNodeArray_2DR8

    module procedure MAPL_DeAllocNodeArray_3DR8

      type(c_ptr) :: Caddr
      integer     :: STATUS

      if(.not.MAPL_ShmInitialized) then
         _RETURN(MAPL_NoShm)
      endif

      Caddr = C_Loc(Ptr(lbound(Ptr,1),lbound(Ptr,2),lbound(Ptr,3)))

      call ReleaseSharedMemory(Caddr,rc=STATUS)
      _VERIFY(STATUS)

      _RETURN(SHM_SUCCESS)
    end procedure MAPL_DeAllocNodeArray_3DR8

    module procedure MAPL_DeAllocNodeArray_4DR8

      type(c_ptr) :: Caddr
      integer     :: STATUS

      if(.not.MAPL_ShmInitialized) then
         _RETURN(MAPL_NoShm)
      endif

      Caddr = C_Loc(Ptr(lbound(Ptr,1),lbound(Ptr,2),lbound(Ptr,3),lbound(Ptr,4)))
      call ReleaseSharedMemory(Caddr,rc=STATUS)
      _VERIFY(STATUS)

      _RETURN(SHM_SUCCESS)
    end procedure MAPL_DeAllocNodeArray_4DR8

    module procedure MAPL_DeAllocNodeArray_5DR8

      type(c_ptr) :: Caddr
      integer     :: STATUS

      if(.not.MAPL_ShmInitialized) then
         _RETURN(MAPL_NoShm)
      endif

      Caddr = C_Loc(Ptr(lbound(Ptr,1),lbound(Ptr,2),lbound(Ptr,3),lbound(Ptr,4),lbound(Ptr,5)))
      call ReleaseSharedMemory(Caddr,rc=STATUS)
      _VERIFY(STATUS)

      _RETURN(SHM_SUCCESS)
    end procedure MAPL_DeAllocNodeArray_5DR8    

    module procedure MAPL_AllocNodeArray_1DL4
    implicit none
      type(c_ptr) :: Caddr
      integer len, STATUS

      if(.not.MAPL_ShmInitialized) then
         _RETURN(MAPL_NoShm)
      endif

      len = shp(1)

      call GetSharedMemory(Caddr, len, rc=STATUS)
      _VERIFY(STATUS)

      call c_f_pointer(Caddr, Ptr, Shp) ! C ptr to Fortran ptr
      _ASSERT(size(Ptr)==len,'needs informative message')

      if(present(lbd)) Ptr(lbd(1):) => Ptr

      _RETURN(SHM_SUCCESS)
    end procedure MAPL_AllocNodeArray_1DL4

    module procedure MAPL_AllocNodeArray_1DI4

      type(c_ptr) :: Caddr
      integer len, STATUS

      if(.not.MAPL_ShmInitialized) then
         _RETURN(MAPL_NoShm)
      endif

      len = shp(1)

      call GetSharedMemory(Caddr, len, rc=STATUS)
      _VERIFY(STATUS)

      call c_f_pointer(Caddr, Ptr, Shp) ! C ptr to Fortran ptr
      _ASSERT(size(Ptr)==len,'needs informative message')

      if(present(lbd)) Ptr(lbd(1):) => Ptr

      _RETURN(SHM_SUCCESS)
    end procedure MAPL_AllocNodeArray_1DI4


    module procedure MAPL_AllocNodeArray_2DI4

      type(c_ptr) :: Caddr
      integer len, STATUS

      if(.not.MAPL_ShmInitialized) then
         _RETURN(MAPL_NoShm)
      endif

      len=product(Shp)

      call GetSharedMemory(Caddr, len, rc=STATUS)
      _VERIFY(STATUS)

      call c_f_pointer(Caddr, Ptr, Shp) ! C ptr to Fortran ptr
      _ASSERT(all(shape(Ptr)==Shp),'needs informative message')

      if(present(lbd)) Ptr(lbd(1):,lbd(2):) => Ptr

      _RETURN(SHM_SUCCESS)
    end procedure MAPL_AllocNodeArray_2DI4

    module procedure MAPL_AllocNodeArray_3DI4

      type(c_ptr) :: Caddr
      integer len, STATUS

      if(.not.MAPL_ShmInitialized) then
         _RETURN(MAPL_NoShm)
      endif

      len=product(Shp)

      call GetSharedMemory(Caddr, len, rc=STATUS)
      _VERIFY(STATUS)

      call c_f_pointer(Caddr, Ptr, Shp) ! C ptr to Fortran ptr
      _ASSERT(all(shape(Ptr)==Shp),'needs informative message')

      if(present(lbd)) Ptr(lbd(1):,lbd(2):,lbd(3):) => Ptr

      _RETURN(SHM_SUCCESS)
    end procedure MAPL_AllocNodeArray_3DI4

    module procedure MAPL_AllocNodeArray_4DI4

      type(c_ptr) :: Caddr
      integer len, STATUS

      if(.not.MAPL_ShmInitialized) then
         _RETURN(MAPL_NoShm)
      endif

      len=product(Shp)

      call GetSharedMemory(Caddr, len, rc=STATUS)
      _VERIFY(STATUS)

      call c_f_pointer(Caddr, Ptr, Shp) ! C ptr to Fortran ptr
      _ASSERT(all(shape(Ptr)==Shp),'needs informative message')

      if(present(lbd)) Ptr(lbd(1):,lbd(2):,lbd(3):,lbd(4):) => Ptr

      _RETURN(SHM_SUCCESS)
    end procedure MAPL_AllocNodeArray_4DI4


    module procedure MAPL_AllocNodeArray_1DR4

      type(c_ptr) :: Caddr
      integer len, STATUS

      if(.not.MAPL_ShmInitialized) then
         _RETURN(MAPL_NoShm)
      endif

      len = shp(1)

      call GetSharedMemory(Caddr, len, rc=STATUS)
      _VERIFY(STATUS)

      call c_f_pointer(Caddr, Ptr, Shp) ! C ptr to Fortran ptr
      _ASSERT(size(Ptr)==len,'needs informative message')

      if(present(lbd)) Ptr(lbd(1):) => Ptr

      _RETURN(SHM_SUCCESS)
    end procedure MAPL_AllocNodeArray_1DR4


    module procedure MAPL_AllocNodeArray_2DR4

      type(c_ptr) :: Caddr
      integer len, STATUS

      if(.not.MAPL_ShmInitialized) then
         _RETURN(MAPL_NoShm)
      endif

      len=product(Shp)

      call GetSharedMemory(Caddr, len, rc=STATUS)
      _VERIFY(STATUS)

      call c_f_pointer(Caddr, Ptr, Shp) ! C ptr to Fortran ptr
      _ASSERT(all(shape(Ptr)==Shp),'needs informative message')

      if(present(lbd)) Ptr(lbd(1):,lbd(2):) => Ptr

      _RETURN(SHM_SUCCESS)
    end procedure MAPL_AllocNodeArray_2DR4

    module procedure MAPL_AllocNodeArray_3DR4

      type(c_ptr) :: Caddr
      integer len, STATUS

      if(.not.MAPL_ShmInitialized) then
         _RETURN(MAPL_NoShm)
      endif

      len=product(Shp)

      call GetSharedMemory(Caddr, len, rc=STATUS)
      _VERIFY(STATUS)

      call c_f_pointer(Caddr, Ptr, Shp) ! C ptr to Fortran ptr
      _ASSERT(all(shape(Ptr)==Shp),'needs informative message')

      if(present(lbd)) Ptr(lbd(1):,lbd(2):,lbd(3):) => Ptr

      _RETURN(SHM_SUCCESS)
    end procedure MAPL_AllocNodeArray_3DR4

    module procedure MAPL_AllocNodeArray_4DR4

      type(c_ptr) :: Caddr
      integer len, STATUS

      if(.not.MAPL_ShmInitialized) then
         _RETURN(MAPL_NoShm)
      endif

      len=product(Shp)

      call GetSharedMemory(Caddr, len, rc=STATUS)
      _VERIFY(STATUS)

      call c_f_pointer(Caddr, Ptr, Shp) ! C ptr to Fortran ptr
      _ASSERT(all(shape(Ptr)==Shp),'needs informative message')

      if(present(lbd)) Ptr(lbd(1):,lbd(2):,lbd(3):,lbd(4):) => Ptr

      _RETURN(SHM_SUCCESS)
    end procedure MAPL_AllocNodeArray_4DR4


    module procedure MAPL_AllocNodeArray_1DR8

      type(c_ptr) :: Caddr
      integer len, STATUS

      if(.not.MAPL_ShmInitialized) then
         _RETURN(MAPL_NoShm)
      endif

      len = shp(1)*2

      call GetSharedMemory(Caddr, len, rc=STATUS)
      _VERIFY(STATUS)

      call c_f_pointer(Caddr, Ptr, Shp) ! C ptr to Fortran ptr
!      _ASSERT(size(Ptr)==len,'needs informative message')   ! Thomas Clune suggested that this ASSERT is unnecessary.

      if(present(lbd)) Ptr(lbd(1):) => Ptr

      _RETURN(SHM_SUCCESS)
    end procedure MAPL_AllocNodeArray_1DR8


    module procedure MAPL_AllocNodeArray_2DR8

      type(c_ptr) :: Caddr
      integer len, STATUS

      if(.not.MAPL_ShmInitialized) then
         _RETURN(MAPL_NoShm)
      endif

      len=product(Shp)*2

      call GetSharedMemory(Caddr, len, rc=STATUS)
      _VERIFY(STATUS)

      call c_f_pointer(Caddr, Ptr, Shp) ! C ptr to Fortran ptr
      _ASSERT(all(shape(Ptr)==Shp),'needs informative message')

      if(present(lbd)) Ptr(lbd(1):,lbd(2):) => Ptr

      _RETURN(SHM_SUCCESS)
    end procedure MAPL_AllocNodeArray_2DR8

    module procedure MAPL_AllocNodeArray_3DR8

      type(c_ptr) :: Caddr
      integer len, STATUS

      if(.not.MAPL_ShmInitialized) then
         _RETURN(MAPL_NoShm)
      endif

      len=product(Shp)*2

      call GetSharedMemory(Caddr, len, rc=STATUS)
      _VERIFY(STATUS)

      call c_f_pointer(Caddr, Ptr, Shp) ! C ptr to Fortran ptr
      _ASSERT(all(shape(Ptr)==Shp),'needs informative message')

      if(present(lbd)) Ptr(lbd(1):,lbd(2):,lbd(3):) => Ptr

      _RETURN(SHM_SUCCESS)
    end procedure MAPL_AllocNodeArray_3DR8

    module procedure MAPL_AllocNodeArray_4DR8

      type(c_ptr) :: Caddr
      integer len, STATUS

      if(.not.MAPL_ShmInitialized) then
         _RETURN(MAPL_NoShm)
      endif

      len=product(Shp)*2

      call GetSharedMemory(Caddr, len, rc=STATUS)
      _VERIFY(STATUS)

      call c_f_pointer(Caddr, Ptr, Shp) ! C ptr to Fortran ptr
      _ASSERT(all(shape(Ptr)==Shp),'needs informative message')

      if(present(lbd)) Ptr(lbd(1):,lbd(2):,lbd(3):,lbd(4):) => Ptr

      _RETURN(SHM_SUCCESS)
    end procedure MAPL_AllocNodeArray_4DR8

    module procedure MAPL_AllocNodeArray_5DR8

      type(c_ptr) :: Caddr
      integer len, STATUS

      _UNUSED_DUMMY(lbd)
      if(.not.MAPL_ShmInitialized) then
         _RETURN(MAPL_NoShm)
      endif

      len=product(Shp)*2

      call GetSharedMemory(Caddr, len, rc=STATUS)
      _VERIFY(STATUS)

      call c_f_pointer(Caddr, Ptr, Shp) ! C ptr to Fortran ptr
      _ASSERT(all(shape(Ptr)==Shp),'needs informative message')

      if(present(lbd)) Ptr(lbd(1):,lbd(2):,lbd(3):,lbd(4):,lbd(5):) => Ptr

      _RETURN(SHM_SUCCESS)
    end procedure MAPL_AllocNodeArray_5DR8    


    module procedure MAPL_AllocateShared_1DL4


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

    end procedure MAPL_AllocateShared_1DL4

    module procedure MAPL_AllocateShared_1DI4


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

    end procedure MAPL_AllocateShared_1DI4

    module procedure MAPL_AllocateShared_1DR4


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

    end procedure MAPL_AllocateShared_1DR4

    module procedure MAPL_AllocateShared_1DR8


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

    end procedure MAPL_AllocateShared_1DR8

    module procedure MAPL_AllocateShared_2DR4


      integer :: status

      if(MAPL_ShmInitialized) then
         call MAPL_AllocNodeArray(Ptr, Shp, lbd, rc=STATUS)
         _VERIFY(STATUS)
      else
         if (TransRoot) then
            allocate(Ptr(Shp(1),Shp(2)),stat=status)
         else
            allocate(Ptr(0,0),stat=status)
         end if
         _VERIFY(STATUS)
      endif

      _RETURN(STATUS)

    end procedure MAPL_AllocateShared_2DR4

    module procedure MAPL_AllocateShared_2DR8


      integer :: status

      if(MAPL_ShmInitialized) then
         call MAPL_AllocNodeArray(Ptr, Shp, lbd, rc=STATUS)
         _VERIFY(STATUS)
      else
         if (TransRoot) then
            allocate(Ptr(Shp(1),Shp(2)),stat=status)
         else
            allocate(Ptr(0,0),stat=status)
         end if
         _VERIFY(STATUS)
      endif

      _RETURN(STATUS)

    end procedure MAPL_AllocateShared_2DR8

    module procedure ReleaseSharedMemory
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
    end procedure ReleaseSharedMemory



    module procedure GetSharedMemory

      integer                   :: status, pos
      integer(c_key_t)          :: key
      integer(c_size_t)         :: numBytes
      integer, parameter        :: WORD_SIZE = 4
      integer(c_int), parameter :: C_ZERO = 0
      integer(c_int), parameter :: myflg = o'666'
      integer(c_int), parameter :: shmflg = ior(IPC_CREAT,myflg)
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
            _ASSERT(.false.,'needs informative message')
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
    end procedure GetSharedMemory

    module procedure BroadcastToNodes_1DR4
      integer :: STATUS

      real(kind=REAL32), allocatable :: ldata(:)

      if(.not.MAPL_ShmInitialized .or. MAPL_NodeRootsComm==MPI_COMM_NULL) THEN
         _RETURN(SHM_SUCCESS)
      end if

      allocate(ldata(size(data,1)),stat=status)
      _VERIFY(STATUS)
      ldata = data
      call MPI_Bcast(LDATA, N, MPI_REAL, ROOT, MAPL_NodeRootsComm, STATUS)
      _VERIFY(STATUS)
      data = ldata
      deallocate(ldata)

      _RETURN(SHM_SUCCESS)
    end procedure BroadcastToNodes_1DR4

    module procedure BroadcastToNodes_2DR4
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
    end procedure BroadcastToNodes_2DR4

    module procedure BroadcastToNodes_3DR4
      integer :: STATUS

      real, allocatable :: ldata(:,:,:)

      if(.not.MAPL_ShmInitialized .or. MAPL_NodeRootsComm==MPI_COMM_NULL) THEN
         _RETURN(SHM_SUCCESS)
      endif

      allocate(ldata(size(data,1),size(data,2),size(data,3)),stat=STATUS)
      _VERIFY(STATUS)
      ldata = data
      call MPI_Bcast(LDATA, N, MPI_REAL, ROOT, MAPL_NodeRootsComm, STATUS)
      _VERIFY(STATUS)
      data = ldata
      deallocate(ldata)

      _RETURN(SHM_SUCCESS)
    end procedure BroadcastToNodes_3DR4

    module procedure BroadcastToNodes_4DR4
      integer :: STATUS

      real, allocatable :: ldata(:,:,:,:)

      if(.not.MAPL_ShmInitialized .or. MAPL_NodeRootsComm==MPI_COMM_NULL) THEN
         _RETURN(SHM_SUCCESS)
      endif

      allocate(ldata(size(data,1),size(data,2),size(data,3),size(data,4)),stat=STATUS)
      _VERIFY(STATUS)
      ldata = data
      call MPI_Bcast(LDATA, N, MPI_REAL, ROOT, MAPL_NodeRootsComm, STATUS)
      _VERIFY(STATUS)
      data = ldata
      deallocate(ldata)

      _RETURN(SHM_SUCCESS)
    end procedure BroadcastToNodes_4DR4

    module procedure BroadcastToNodes_1DR8
      integer :: STATUS

      real(kind=REAL64), allocatable :: ldata(:)

      if(.not.MAPL_ShmInitialized .or. MAPL_NodeRootsComm==MPI_COMM_NULL) THEN
         _RETURN(SHM_SUCCESS)
      endif

      allocate(ldata(size(data,1)),stat=STATUS)
      _VERIFY(STATUS)
      ldata = data
      call MPI_Bcast(LDATA, N, MPI_DOUBLE_PRECISION, ROOT, MAPL_NodeRootsComm, STATUS)
      _VERIFY(STATUS)
      data = ldata
      deallocate(ldata)

      _RETURN(SHM_SUCCESS)
    end procedure BroadcastToNodes_1DR8

    module procedure BroadcastToNodes_2DR8
      integer :: STATUS

      real(kind=REAL64), allocatable :: ldata(:,:)

      if(.not.MAPL_ShmInitialized .or. MAPL_NodeRootsComm==MPI_COMM_NULL) THEN
         _RETURN(SHM_SUCCESS)
      endif

      allocate(ldata(size(data,1),size(data,2)),stat=STATUS)
      _VERIFY(STATUS)
      ldata = data
      call MPI_Bcast(LDATA, N, MPI_DOUBLE_PRECISION, ROOT, MAPL_NodeRootsComm, STATUS)
      _VERIFY(STATUS)
      data = ldata
      deallocate(ldata)

      _RETURN(SHM_SUCCESS)
    end procedure BroadcastToNodes_2DR8

    module procedure BroadcastToNodes_3DR8
      integer :: STATUS

      real(kind=REAL64), allocatable :: ldata(:,:,:)

      if(.not.MAPL_ShmInitialized .or. MAPL_NodeRootsComm==MPI_COMM_NULL) THEN
         _RETURN(SHM_SUCCESS)
      endif

      allocate(ldata(size(data,1),size(data,2),size(data,3)),stat=STATUS)
      _VERIFY(STATUS)
      ldata = data
      call MPI_Bcast(LDATA, N, MPI_DOUBLE_PRECISION, ROOT, MAPL_NodeRootsComm, STATUS)
      _VERIFY(STATUS)
      data = ldata
      deallocate(ldata)

      _RETURN(SHM_SUCCESS)
    end procedure BroadcastToNodes_3DR8

    module procedure BroadcastToNodes_4DR8
      integer :: STATUS

      real(kind=REAL64), allocatable :: ldata(:,:,:,:)

      if(.not.MAPL_ShmInitialized .or. MAPL_NodeRootsComm==MPI_COMM_NULL) THEN
         _RETURN(SHM_SUCCESS)
      endif

      allocate(ldata(size(data,1),size(data,2),size(data,3),size(data,4)),stat=STATUS)
      _VERIFY(STATUS)
      ldata = data
      call MPI_Bcast(LDATA, N, MPI_DOUBLE_PRECISION, ROOT, MAPL_NodeRootsComm, STATUS)
      _VERIFY(STATUS)
      data = ldata
      deallocate(ldata)

      _RETURN(SHM_SUCCESS)
    end procedure BroadcastToNodes_4DR8

    module procedure BroadcastToNodes_1DI4
      integer :: STATUS

      integer, allocatable :: ldata(:)

      if(.not.MAPL_ShmInitialized .or. MAPL_NodeRootsComm==MPI_COMM_NULL) THEN
         _RETURN(SHM_SUCCESS)
      endif

      allocate(ldata(size(data,1)),stat=STATUS)
      _VERIFY(STATUS)
      ldata = data
      call MPI_Bcast(LDATA, N, MPI_INTEGER, ROOT, MAPL_NodeRootsComm, STATUS)
      _VERIFY(STATUS)
      data = ldata
      deallocate(ldata)

      _RETURN(SHM_SUCCESS)
    end procedure BroadcastToNodes_1DI4

    module procedure BroadcastToNodes_2DI4
      integer :: STATUS

      integer, allocatable :: ldata(:,:)

      if(.not.MAPL_ShmInitialized .or. MAPL_NodeRootsComm==MPI_COMM_NULL) THEN
         _RETURN(SHM_SUCCESS)
      endif

      allocate(ldata(size(data,1),size(data,2)),stat=STATUS)
      _VERIFY(STATUS)
      ldata = data
      call MPI_Bcast(LDATA, N, MPI_INTEGER, ROOT, MAPL_NodeRootsComm, STATUS)
      _VERIFY(STATUS)
      data = ldata
      deallocate(ldata)

      _RETURN(SHM_SUCCESS)
    end procedure BroadcastToNodes_2DI4

    module procedure BroadcastToNodes_3DI4
      integer :: STATUS

      integer, allocatable :: ldata(:,:,:)

      if(.not.MAPL_ShmInitialized .or. MAPL_NodeRootsComm==MPI_COMM_NULL) THEN
         _RETURN(SHM_SUCCESS)
      endif

      allocate(ldata(size(data,1),size(data,2),size(data,3)),stat=STATUS)
      _VERIFY(STATUS)
      ldata = data
      call MPI_Bcast(LDATA, N, MPI_INTEGER, ROOT, MAPL_NodeRootsComm, STATUS)
      _VERIFY(STATUS)
      data = ldata
      deallocate(ldata)

      _RETURN(SHM_SUCCESS)
    end procedure BroadcastToNodes_3DI4

    module procedure MAPL_SyncSharedMemory
      integer :: STATUS
      if(.not.MAPL_ShmInitialized) then
         _RETURN(SHM_SUCCESS)
      endif
!!! Make sure everyone on a node syncs
      call MPI_Barrier(MAPL_NodeComm, STATUS)
      _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')
      _RETURN(SHM_SUCCESS)
    end procedure MAPL_SyncSharedMemory

    module procedure MAPL_GetNewRank

      rank = MAPL_NodeRankList(node)%RankLastUsed+1
      if (rank > size(MAPL_NodeRankList(node)%rank)) then
         rank = 1
      end if
      MAPL_NodeRankList(node)%rankLastUsed=rank
 
      _RETURN(SHM_SUCCESS)

    end procedure MAPL_GetNewRank

    module procedure getNodeComm
      use MAPL_SortMod

      integer, allocatable :: colors(:), ranks(:)
      integer :: last
      integer :: i, n

      character(len=MPI_MAX_PROCESSOR_NAME) :: name
      character(len=MPI_MAX_PROCESSOR_NAME), allocatable :: names(:)
    
      integer :: len, STATUS, MyColor, NumColors, npes, rank
      integer :: NumCores
      integer :: nodeRank

      integer :: i1, i2
      integer, allocatable :: newNode(:)
      class(Logger), pointer :: lgr

      NodeComm=MPI_COMM_NULL
      
      call MPI_Get_processor_name(name,len,STATUS)
      _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')

      call MPI_COMM_RANK(Comm, rank, STATUS)
      _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')
      call MPI_COMM_SIZE(Comm, npes, STATUS)
      _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')

      allocate(names(npes),stat=STATUS)
      _ASSERT(STATUS==0,'needs informative message')

      call MPI_AllGather(name ,MPI_MAX_PROCESSOR_NAME,MPI_CHARACTER,&
                         names,MPI_MAX_PROCESSOR_NAME,MPI_CHARACTER,Comm,status)
      _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')

      myColor = getColor(name, names)

      ! We are ready to split communicators

      call MPI_COMM_SPLIT(Comm, MyColor, rank, NodeComm, STATUS)
      _ASSERT(NodeComm/=MPI_COMM_NULL,'needs informative message')

      call MPI_COMM_SIZE(NodeComm, NumCores, STATUS)
      _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')

      allocate(colors(npes), ranks(npes), stat=STATUS)
      _ASSERT(STATUS==0,'needs informative message')
      do i=1,npes
         ranks(i) = i-1
      end do
      
      call MPI_AllGather(myColor, 1, MPI_INTEGER,&
                         colors,  1, MPI_INTEGER,Comm,status)
      _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')

      call MAPL_Sort(COLORS,ranks)
      last = 0
      n = 0
      allocate(newNode(npes+1),stat=status)
      _VERIFY(STATUS)
      newNode = 0
      do i=1,npes
         if(last /= colors(i)) then 
            last = colors(i)
            n = n + 1
            newNode(n) = i
         end if
         ! We are done with COLORS, now we reuse the space to store NodeId
         colors(i) = n
      end do
      NumColors = n
      MAPL_NumNodes = NumColors
      do i=1,size(ranks)
         if (ranks(i) == rank) then 
            MAPL_MyNodeNum = colors(i)
            exit
         end if
      end do

      newNode(NumColors+1) = npes+1
      allocate(MAPL_NodeRankList(NumColors), stat=status)
      _VERIFY(STATUS) 
      do i=1,NumColors
         i1=newNode(i)
         i2=newNode(i+1)-1
         allocate(MAPL_NodeRankList(i)%rank(i2-i1+1),stat=status)
         _VERIFY(STATUS)
         MAPL_NodeRankList(i)%rank=ranks(i1:i2)
         call MAPL_Sort(MAPL_NodeRankList(i)%rank)
      end do
      deallocate(newNode)

      do i=1,size(MAPL_NodeRankList)
         MAPL_NodeRankList(i)%RankLastUsed=1
      enddo

      deallocate(ranks)
      deallocate(colors)

      MAPL_CoresPerNodeUsed = NumCores

      call MPI_Comm_rank(NodeComm, nodeRank, STATUS)
      _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')

      MAPL_AmNodeRoot = nodeRank==0

      if (MAPL_AmNodeRoot) then
         _ASSERT(MAPL_NodeRankList(MAPL_MyNodeNum)%rank(1) == rank,'needs informative message')
      end if

!     we store the global Min and Max of CoresPerNode
      call MPI_AllReduce (MAPL_CoresPerNodeUsed, MAPL_CoresPerNodeMin, &
                          1, MPI_INTEGER, MPI_MIN, comm, status )
      _VERIFY(STATUS)
      call MPI_AllReduce (MAPL_CoresPerNodeUsed, MAPL_CoresPerNodeMax, &
                          1, MPI_INTEGER, MPI_MAX, comm, status )
      _VERIFY(STATUS)

      lgr => logging%get_logger('MAPL.SHMEM')
      
      if(rank==0) then
         if (MAPL_CoresPerNodeMin == MAPL_CoresPerNodeMax) then
            call lgr%info("NumCores per Node = %i0", NumCores)
         else
            call lgr%info("NumCores per Node varies from %i0 to %i0", &
                 MAPL_CoresPerNodeMin, MAPL_CoresPerNodeMax)
         end if
         call lgr%info("NumNodes in use   = %i0", NumColors)
         call lgr%info("Total PEs         = %i0", npes)
      end if

      deallocate(names,stat=STATUS)
      _ASSERT(STATUS==0,'needs informative message')
    
      _RETURN(SHM_SUCCESS)
    contains
      function getColor(name, sampleNames) result(color)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: sampleNames(:)
        integer :: color
        
        integer :: i
        
        color = 0 ! unless
        do i = 1, size(sampleNames)
           if (trim(name) == trim(sampleNames(i))) then
              color = i
              exit
           end if
        end do

      end function getColor
    
    end procedure getNodeComm

    module procedure getNodeRootsComm

      integer :: STATUS, MyColor, NumNodes, npes, rank
      class(Logger), pointer :: lgr

      NodeRootsComm=MPI_COMM_NULL

      call MPI_COMM_RANK(Comm, rank, STATUS)
      _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')
      call MPI_COMM_SIZE(Comm, npes, STATUS)
      _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')

      myColor = 0
      if (MAPL_AmNodeRoot) myColor = 1

      ! We are ready to split communicators

      call MPI_COMM_SPLIT(Comm, MyColor, rank, NodeRootsComm, STATUS)
      _ASSERT(NodeRootsComm/=MPI_COMM_NULL,'needs informative message')

      if (myColor==0) then
      ! Set nodes outside of this comm back to null
         NodeRootsComm=MPI_COMM_NULL
      else
      ! Confirm we have the proper communicator
         call MPI_COMM_SIZE(NodeRootsComm, NumNodes, STATUS)
         _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')
         ! additional sanity checks
         _ASSERT(MAPL_NumNodes == NumNodes,'needs informative message')
         call MPI_COMM_RANK(NodeRootsComm, rank, STATUS)
         _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')
         _ASSERT(MAPL_MyNodeNum == rank+1,'needs informative message')
      endif

      lgr => logging%get_logger('MAPL.SHMEM')
      
      if(rank==0) then
         call lgr%info("NumNodes in use  = %i0", NumNodes)
      end if

      _RETURN(SHM_SUCCESS)

    end procedure getNodeRootsComm



    module procedure MAPL_ShmemAmOnFirstNode

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
    end procedure MAPL_ShmemAmOnFirstNode

    module procedure MAPL_CoresPerNodeGet

      integer :: status

      if ( MAPL_NodeComm == -1 ) then
           call MAPL_GetNodeInfo(comm, rc=STATUS )
           _VERIFY(STATUS)
      end if

      MAPL_CoresPerNodeGet = MAPL_CoresPerNodeUsed

      _RETURN(SHM_SUCCESS)
    end procedure MAPL_CoresPerNodeGet

  end submodule Shmem_implementation
