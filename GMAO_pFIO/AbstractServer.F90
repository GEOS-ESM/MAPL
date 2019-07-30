#include "pFIO_ErrLog.h"
#include "unused_dummy.H"

module pFIO_AbstractServerMod
   use pFIO_ErrorHandlingMod
   use pFIO_ConstantsMod
   use pFIO_AbstractDataReferenceMod
   use pFIO_AbstractDataReferenceVectorMod
   use pFIO_ShmemReferenceMod
   use pFIO_StringInt64MapMod
   use pFIO_AbstractMessageMod
   use pFIO_DummyMessageMod
   use pFIO_MessageVectorMod
   use mpi

   implicit none
   private
   public :: AbstractServer
   
   integer,parameter,public :: MAX_SERVER_NODES_NUM = 100000
   integer,parameter,public :: MSIZE_ID = - MAX_SERVER_NODES_NUM
   integer,parameter,public :: UNALLOCATED = -100
   integer,parameter,public :: PENDING     = -200

   type,abstract :: AbstractServer
      integer :: comm   ! of all server processes
      integer :: rank   ! rank in all server processes
      integer :: npes   ! number of processes of the server
      integer :: status ! counter, UNALLOCATED, PENDING
      logical :: all_backlog_is_empty = .true. 
      integer :: num_clients = 0
      logical :: terminate
      integer :: InNode_Comm  ! communicator in a node
      integer :: InNode_npes  ! number of processes in a node, it may differ across server nodes
      integer :: InNode_Rank  ! rank in a node
      integer :: NodeRoot_Comm! communicator of server nodes' roots
      integer :: Node_Num     ! number of server nodes
      integer :: Node_Rank    ! rank of server nodes

      ! save info about which process belongs to which node 
      ! all processes keep this info
      integer,allocatable    :: Node_Ranks(:)
      type(StringInt64Map) :: prefetch_offset
      type(StringInt64Map) :: stage_offset
      type(MessageVector),allocatable :: output_backlog(:)
      logical , allocatable :: serverthread_done_msgs(:)
      type(AbstractDataReferenceVector) :: dataRefPtrs
   contains
      procedure :: init
      procedure(start),deferred :: start
      procedure(get_dmessage), deferred :: get_dmessage
      procedure(clear_RequestHandle), deferred :: clear_RequestHandle
      procedure :: get_status
      procedure :: set_status
      procedure :: get_and_set_status
      procedure :: update_status
      procedure :: set_AllBacklogIsEmpty
      procedure :: get_AllBacklogIsEmpty
      procedure :: get_DataReference
      procedure :: set_DataReference
      procedure :: clear_DataReference
      procedure :: I_am_NodeRoot
      procedure :: I_am_ServerRoot
      procedure :: receive_output_data
      procedure :: put_DataToFile
      procedure :: am_I_reading_PE
      procedure :: am_I_writing_PE
      procedure :: get_writing_PE
      procedure :: distribute_task
      procedure :: get_communicator

   end type AbstractServer

   abstract interface 

      subroutine start(this)
         use pFIO_AbstractSocketVectorMod
         import AbstractServer
         class(AbstractServer),target,intent(inout) :: this
      end subroutine start

      subroutine clear_RequestHandle(this)
         import AbstractServer
         class(AbstractServer),target,intent(inout) :: this
      end subroutine clear_RequestHandle

      function get_dmessage(this, rc) result(dmessage) 
         import AbstractServer
         import AbstractMessage
         class (AbstractServer), target, intent(in) :: this
         integer, optional, intent(out) :: rc
         class(AbstractMessage), pointer :: dmessage
      end function

   end interface

contains

   subroutine init(this,comm)
      class (AbstractServer),intent(inout) :: this
      integer, intent(in) :: comm

      integer :: ierror, MyColor
      integer :: i

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

      allocate(this%output_backlog(this%Node_num))
      do i = 1, this%Node_num
         this%output_backlog(i) = MessageVector()
      enddo
   end subroutine init

   function get_and_set_status(this, rc) result(status)
      class(AbstractServer),intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: status

      !$omp critical (counter_status)
      status = this%status
      if( this%status == UNALLOCATED) then
         this%status = PENDING 
      endif
      !$omp flush (this)
      !$omp end critical (counter_status)
      _RETURN(_SUCCESS)
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

   subroutine update_status(this, rc) 
      class(AbstractServer),intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: status

      !$omp critical (counter_status)
      this%status = this%status -1
      status = this%status
      !$omp flush (this)
      !$omp end critical (counter_status)
      if (status /= 0) then
        _RETURN(_SUCCESS)
      endif
      ! status ==0, means the last server thread in the backlog
      
      call this%clear_DataReference()
      call this%clear_RequestHandle()
      call this%set_status(UNALLOCATED)
      call this%set_AllBacklogIsEmpty(.true.)
      call this%prefetch_offset%clear()
      call this%stage_offset%clear()
       
      this%serverthread_done_msgs(:) = .false.
      _RETURN(_SUCCESS)
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

   function I_am_NodeRoot(this) result (yes)
      class (AbstractServer), intent(in) :: this
      logical :: yes
      yes = (this%NodeRoot_Comm /= MPI_COMM_NULL) 
   end function

   function I_am_ServerRoot(this) result (yes)
      class (AbstractServer), intent(in) :: this
      logical :: yes
      yes = (this%rank == 0) 
   end function I_am_ServerRoot

   subroutine receive_output_data(this, rc)
     class (AbstractServer),target, intent(inout) :: this
     integer, optional, intent(out) :: rc

     _ASSERT(.false.," no action of receive_output_data")
   end subroutine receive_output_data

   subroutine put_DataToFile(this, rc)
     class (AbstractServer),target, intent(inout) :: this
     integer, optional, intent(out) :: rc
     _ASSERT(.false.," no action of server_put_DataToFile")
   end subroutine put_DataToFile

   function am_I_reading_PE(this,id) result (yes)
      class(AbstractServer),intent(in) :: this
      integer, intent(in) :: id
      integer :: node_rank,innode_rank
      logical :: yes

      call this%distribute_task(id, node_rank,innode_rank)
      yes = (node_rank   == this%Node_Rank) .and. &
            (innode_rank == this%InNode_Rank)
   end function am_I_reading_PE

   ! for output writing, each node will chose one innode rank write
   function am_I_writing_PE(this,id) result (yes)
      class(AbstractServer),intent(in) :: this
      integer, intent(in) :: id
      integer :: node_rank,innode_rank
      logical :: yes

      call this%distribute_task(id, node_rank,innode_rank)
      yes = (innode_rank == this%InNode_Rank)
   end function am_I_writing_PE

   ! distribute the task (id) to a specific process (node_rank, innode_rank)
   subroutine distribute_task(this,id, node_rank, innode_rank)
      class(AbstractServer),intent(in) :: this
      integer, intent(in)   :: id
      integer,intent(out) :: node_rank,innode_rank
      integer :: rank

      innode_rank = mod(id, this%InNode_npes)
      rank        = mod(id, this%npes)
      node_rank   = this%Node_Ranks(rank)

   end subroutine distribute_task

   function get_writing_PE(this,id) result (rank)
      class(AbstractServer),intent(in) :: this
      integer, intent(in) :: id
      integer :: rank
      integer :: rank_tmp, ierror

      integer :: node_rank,innode_rank
      logical :: yes

      node_rank   = mod(id,this%node_num)
      innode_rank = mod(id,this%innode_npes)

      yes = (node_rank   == this%Node_Rank) .and. &
            (innode_rank == this%InNode_Rank)
      rank_tmp = 0
      rank = 0
      if (yes) rank_tmp = this%rank
      call Mpi_Allreduce(rank_tmp,rank,1, MPI_INTEGER, MPI_SUM, this%comm, ierror)

   end function get_writing_PE

   function get_DataReference(this, ith) result(DataRef)
      class (AbstractServer),target, intent(in) :: this
      integer,optional, intent(in) :: ith
      class(AbstractDataReference),pointer :: DataRef
      integer :: i    
      i=1
      if(present(ith)) i = ith
      DataRef => this%dataRefPtrs%at(i)

   end function get_DataReference

   subroutine set_DataReference(this,DataRef)
      class (AbstractServer), intent(inout) :: this
      class(AbstractDataReference),target,intent(in) :: DataRef

      call this%dataRefPtrs%push_back(DataRef)
      call this%set_status(this%num_clients)

   end subroutine set_DataReference

   subroutine clear_DataReference(this)
      class (AbstractServer), intent(inout) :: this
      class (AbstractDataReference), pointer :: datarefPtr
      integer :: n, i
       
      n = this%dataRefPtrs%size()
      do i = 1, n
         dataRefPtr => this%dataRefPtrs%at(i)
         call dataRefPtr%deallocate()
         deallocate(dataRefPtr)
      enddo      

      call this%dataRefPtrs%clear()

   end subroutine clear_DataReference

   integer function get_communicator(this) result(communicator)
      class (AbstractServer), intent(in) :: this

      communicator = this%comm

   end function get_communicator

end module pFIO_AbstractServerMod
