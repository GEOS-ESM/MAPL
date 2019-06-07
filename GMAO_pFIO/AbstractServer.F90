module pFIO_AbstractServerMod
   use pFIO_ConstantsMod
   use pFIO_AbstractDirectoryServiceMod
   use pFIO_MemReferenceMod
   use pFIO_ShmemReferenceMod
   use pFIO_IntegerShmemReferenceMapMod
   use pFIO_IntegerIntegerMapMod
   use pFIO_AbstractMessageMod
   use pFIO_DummyMessageMod
   use mpi

   implicit none
   private
   public :: AbstractServer
   
   integer,parameter,public :: MAX_SERVER_NODES_NUM = 100000
   integer,parameter,public :: MSIZE_ID = - MAX_SERVER_NODES_NUM
   integer,parameter,public :: UNALLOCATED = -100
   integer,parameter,public :: PENDING     = -200

   type,abstract :: AbstractServer
      class (AbstractDirectoryService), pointer :: directory_service => null()
      integer :: comm   ! of all server processes
      integer :: rank   ! rank in all server processes
      integer :: npes   ! number of processes of the server
      integer :: status ! counter, UNALLOCATED, PENDING
      logical :: all_backlog_is_empty 
      integer :: num_clients
      logical :: terminate
      integer :: InNode_Comm  ! communicator in a node
      integer :: InNode_npes  ! number of processes in a node, it may differ across server nodes
      integer :: InNode_Rank  ! rank in a node
      integer :: NodeRoot_Comm! communicator of server nodes' roots
      integer :: Node_Num     ! number of server nodes
      integer :: Node_Rank    ! rank of server nodes

      ! save info about which process belongs to which node 
      ! all processes keep this info
      integer,allocatable :: Node_Ranks(:)
      type(ShmemReference) :: shmem_reference
      type (IntegerIntegerMap) :: request_offset

   contains
      procedure :: init
      procedure(start),deferred :: start
      procedure :: get_status
      procedure :: set_status
      procedure :: get_and_set_status
      procedure :: update_status
      procedure :: set_AllBacklogIsEmpty
      procedure :: get_AllBacklogIsEmpty
      procedure :: get_shmemReference
      procedure :: set_shmemReference
      procedure :: get_dmessage
      procedure :: I_am_NodeRoot
   end type AbstractServer

   abstract interface 

      subroutine start(this)
         import AbstractServer
         class(AbstractServer),target,intent(inout) :: this
      end subroutine start

   end interface

contains

   subroutine init(this,comm)
      class (AbstractServer),intent(inout) :: this
      integer, intent(in) :: comm

      integer :: ierror, MyColor

      call MPI_Comm_dup(comm, this%comm, ierror)
      call MPI_Comm_rank(this%comm, this%rank, ierror)
      call MPI_Comm_size(this%comm, this%npes, ierror)

      this%num_clients = 0
      this%all_backlog_is_empty = .true.
      this%status = UNALLOCATED

      call MPI_Comm_split_type(this%comm, MPI_COMM_TYPE_SHARED, 0, MPI_INFO_NULL, this%InNode_Comm,ierror)
      call MPI_Comm_size(this%InNode_Comm, this%InNode_npes,ierror)
      call MPI_Comm_rank(this%InNode_Comm, this%InNode_Rank, ierror)
       
      MyColor = 0
      if (this%InNode_Rank == 0) MyColor = 1

      call MPI_COMM_SPLIT( comm, MyColor, this%rank, this%NodeRoot_Comm, ierror)

      if (MyColor==0) then
         this%NodeRoot_Comm=MPI_COMM_NULL
      else
         call MPI_COMM_SIZE(this%NodeRoot_Comm, this%Node_Num, ierror)
         call MPI_COMM_RANK(this%NodeRoot_Comm, this%Node_Rank, ierror)
      endif

      call Mpi_Bcast(this%Node_Rank, 1, MPI_INTEGER, 0, this%InNode_Comm, ierror) 
      call Mpi_Bcast(this%Node_Num,  1, MPI_INTEGER, 0, this%InNode_Comm, ierror) 
     
      allocate(This%Node_Ranks(0:this%npes-1))

      call Mpi_AllGather(this%Node_Rank,  1, MPI_INTEGER, &
                         this%Node_Ranks, 1, MPI_INTEGER, comm,ierror)

   end subroutine init

   function get_and_set_status(this) result(status)
      class(AbstractServer),intent(inout) :: this
      integer :: status

      !$omp critical (counter_status)
      status = this%status
      if( this%status == UNALLOCATED) then
         this%status = PENDING 
      endif
      !$omp flush (this)
      !$omp end critical (counter_status)
   end function get_and_set_status

   function get_status(this) result(status)
      class(AbstractServer),intent(in) :: this
      integer :: status

      !$omp critical (counter_status)
      status = this%status
      !$omp end critical (counter_status)
   end function get_status

   subroutine set_status(this,status)
      class(AbstractServer),intent(inout) :: this
      integer,intent(in) :: status

      !$omp critical (counter_status)
      this%status = status
      !$omp flush (this)
      !$omp end critical (counter_status)
   end subroutine  set_status

   subroutine update_status(this) 
      class(AbstractServer),intent(inout) :: this
      integer :: status

      !$omp critical (counter_status)
      this%status = this%status -1
      status = this%status
      !$omp flush (this)
      !$omp end critical (counter_status)
      if (status /= 0) return
      ! status ==0, means the last server thread in the backlog
      call this%shmem_reference%deallocate()
      call this%set_status(UNALLOCATED)
      call this%set_AllBacklogIsEmpty(.true.)
      call this%request_offset%clear()

   end subroutine update_status

   function get_AllBacklogIsEmpty(this) result(status)
      class(AbstractServer),intent(in) :: this
      logical :: status

      !$omp critical (backlog_status)
      status = this%all_backlog_is_empty
      !$omp end critical (backlog_status)
   end function get_AllBacklogIsEmpty

   subroutine set_AllBacklogIsEmpty(this,status)
      class(AbstractServer),intent(inout) :: this
      logical :: status

      !$omp critical (backlog_status)
      this%all_backlog_is_empty = status
      !$omp flush (this)
      !$omp end critical (backlog_status)
   end subroutine set_AllBacklogIsEmpty

   function get_shmemReference(this) result(shmemRef)
      class (AbstractServer),target, intent(in) :: this
      type(ShmemReference),pointer :: shmemRef

      shmemRef => this%shmem_reference

   end function get_shmemReference

   subroutine set_shmemReference(this,shmemRef)
      class (AbstractServer), intent(inout) :: this
      type(ShmemReference),intent(in) :: shmemRef

      this%shmem_reference = shmemRef
      call this%set_status(this%num_clients)

   end subroutine set_shmemReference

   ! if it is dummy, the server will not loop the open_requests. Instead, it loops over the servertheads first.
   function get_dmessage(this) result(dmessage) 
      class (AbstractServer), target, intent(in) :: this
      class(AbstractMessage), pointer :: dmessage
      allocate(dmessage,source = DummyMessage())
   end function

   function I_am_NodeRoot(this) result (yes)
      class (AbstractServer), intent(in) :: this
      logical :: yes
      yes = (this%NodeRoot_Comm /= MPI_COMM_NULL) 
   end function

end module pFIO_AbstractServerMod
