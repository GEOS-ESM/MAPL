#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_AbstractServerMod
   use MAPL_Profiler
   use MAPL_ExceptionHandling
   use pFIO_ConstantsMod
   use pFIO_AbstractDataReferenceMod
   use pFIO_AbstractDataReferenceVectorMod
   use pFIO_ShmemReferenceMod
   use gFTL_StringInteger64Map
   use pFIO_AbstractMessageMod
   use pFIO_CollectiveStageDataMessageMod
   use pFIO_RDMAReferenceMod
   use pFIO_DummyMessageMod
   use pFIO_MessageVectorMod
   use mpi

   implicit none
   private
   public :: AbstractServer
   public :: ioserver_profiler
   type (DistributedProfiler), pointer :: ioserver_profiler=>null()

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
      type(StringInteger64Map) :: prefetch_offset
      type(StringInteger64Map) :: stage_offset
      logical , allocatable :: serverthread_done_msgs(:)
      type(AbstractDataReferenceVector) :: dataRefPtrs
   contains
      procedure :: init
      procedure(start),deferred :: start
      procedure(get_dmessage), deferred :: get_dmessage
      procedure(clear_RequestHandle), deferred :: clear_RequestHandle
      procedure(set_collective_request), deferred :: set_collective_request
      procedure(create_remote_win), deferred :: create_remote_win
      procedure :: get_status
      procedure :: set_status
      procedure :: update_status
      procedure :: clean_up
      procedure :: set_AllBacklogIsEmpty
      procedure :: get_AllBacklogIsEmpty
      procedure :: get_DataReference
      procedure :: add_DataReference
      procedure :: clear_DataReference
      procedure :: I_am_NodeRoot
      procedure :: I_am_ServerRoot
      procedure :: receive_output_data
      procedure :: put_DataToFile
      procedure :: get_DataFromMem
      procedure :: am_I_reading_PE
      procedure :: get_writing_PE
      procedure :: distribute_task
      procedure :: get_communicator
      procedure :: report_profile
   end type AbstractServer

   abstract interface

      subroutine start(this, rc)
         use pFIO_AbstractSocketVectorMod
         import AbstractServer
         class(AbstractServer),target,intent(inout) :: this
         integer, optional, intent(out) :: rc
      end subroutine start

      subroutine clear_RequestHandle(this, rc)
         import AbstractServer
         class(AbstractServer),target,intent(inout) :: this
         integer, optional, intent(out) :: rc
      end subroutine clear_RequestHandle

      subroutine set_collective_request(this, request, have_done)
         import AbstractServer
         class(AbstractServer),target,intent(inout) :: this
         logical, intent(in) :: request, have_done
      end subroutine set_collective_request

      function get_dmessage(this, rc) result(dmessage)
         import AbstractServer
         import AbstractMessage
         class (AbstractServer), target, intent(in) :: this
         integer, optional, intent(out) :: rc
         class(AbstractMessage), pointer :: dmessage
      end function

      subroutine create_remote_win(this,rc)
         import AbstractServer
         class(AbstractServer),target,intent(inout) :: this
         integer, optional, intent(out) :: rc
      end subroutine create_remote_win

   end interface

contains

   subroutine init(this,comm, port_name, profiler_name, with_profiler, rc)
      class (AbstractServer),intent(inout) :: this
      integer, intent(in) :: comm
      character(*), intent(in) :: port_name
      character(*), optional, intent(in) :: profiler_name
      logical, optional, intent(in) :: with_profiler
      integer, optional, intent(out) :: rc

      integer :: ierror, MyColor
      character(len=:), allocatable :: p_name

      this%comm = comm
      call MPI_Comm_rank(this%comm, this%rank, ierror)
      _VERIFY(ierror)
      call MPI_Comm_size(this%comm, this%npes, ierror)
      _VERIFY(ierror)

      this%num_clients = 0
      this%all_backlog_is_empty = .true.
      this%status = UNALLOCATED

      call MPI_Comm_split_type(this%comm, MPI_COMM_TYPE_SHARED, 0, MPI_INFO_NULL, this%InNode_Comm,ierror)
      _VERIFY(ierror)
      call MPI_Comm_size(this%InNode_Comm, this%InNode_npes,ierror)
      _VERIFY(ierror)
      call MPI_Comm_rank(this%InNode_Comm, this%InNode_Rank,ierror)
      _VERIFY(ierror)

      MyColor = 0
      if (this%InNode_Rank == 0) MyColor = 1

      call MPI_COMM_SPLIT( comm, MyColor, this%rank, this%NodeRoot_Comm, ierror)
      _VERIFY(ierror)

      if (MyColor==0) then
         this%NodeRoot_Comm=MPI_COMM_NULL
      else
         call MPI_COMM_SIZE(this%NodeRoot_Comm, this%Node_Num, ierror)
         _VERIFY(ierror)
         call MPI_COMM_RANK(this%NodeRoot_Comm, this%Node_Rank, ierror)
         _VERIFY(ierror)
      endif

      call Mpi_Bcast(this%Node_Rank, 1, MPI_INTEGER, 0, this%InNode_Comm, ierror)
      _VERIFY(ierror)
      call Mpi_Bcast(this%Node_Num,  1, MPI_INTEGER, 0, this%InNode_Comm, ierror)
      _VERIFY(ierror)

      allocate(This%Node_Ranks(0:this%npes-1))

      call Mpi_AllGather(this%Node_Rank,  1, MPI_INTEGER, &
                         this%Node_Ranks, 1, MPI_INTEGER, comm,ierror)
      _VERIFY(ierror)

      if (present(profiler_name)) then
         p_name = profiler_name
      else
         p_name = port_name
      endif

      if (present(with_profiler)) then
         if (with_profiler) then
            if (.not. associated(ioserver_profiler)) then
               allocate(ioserver_profiler, source = DistributedProfiler(p_name, MpiTimerGauge(), comm))
               call ioserver_profiler%start()
            endif
         endif
      endif

      call MPI_Barrier(comm, ierror)
      _VERIFY(ierror)
      _RETURN(_SUCCESS)
   end subroutine init

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
      type(StringInteger64MapIterator) :: iter
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
      call this%clear_RequestHandle(_RC)
      call this%set_status(UNALLOCATED)
      call this%set_AllBacklogIsEmpty(.true.)

      iter = this%prefetch_offset%begin()
      do while (iter /= this%prefetch_offset%end())
         call this%prefetch_offset%erase(iter)
         iter = this%prefetch_offset%begin()
      enddo

      iter = this%stage_offset%begin()
      do while (iter /= this%stage_offset%end())
         call this%stage_offset%erase(iter)
         iter = this%stage_offset%begin()
      enddo
      this%serverthread_done_msgs(:) = .false.

      _RETURN(_SUCCESS)
   end subroutine update_status

   subroutine clean_up(this, rc)
      class(AbstractServer), target, intent(inout) :: this
      integer, optional, intent(out) :: rc
      type(StringInteger64MapIterator) :: iter
      integer :: status

      if (associated(ioserver_profiler)) call ioserver_profiler%start("clean_up")

      call this%clear_DataReference()
      Call this%clear_RequestHandle(_RC)
      call this%set_AllBacklogIsEmpty(.true.)
      this%serverthread_done_msgs(:) = .false.

      iter = this%prefetch_offset%begin()
      do while (iter /= this%prefetch_offset%end())
         call this%prefetch_offset%erase(iter)
         iter = this%prefetch_offset%begin()
      enddo

      iter = this%stage_offset%begin()
      do while (iter /= this%stage_offset%end())
         call this%stage_offset%erase(iter)
         iter = this%stage_offset%begin()
      enddo

      if (associated(ioserver_profiler)) call ioserver_profiler%stop("clean_up")

      _RETURN(_SUCCESS)
   end subroutine clean_up

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

     _FAIL(" no action of receive_output_data")
     _UNUSED_DUMMY(this)
   end subroutine receive_output_data

   subroutine put_DataToFile(this, rc)
     class (AbstractServer),target, intent(inout) :: this
     integer, optional, intent(out) :: rc
     _FAIL(" no action of server_put_DataToFile")
     _UNUSED_DUMMY(this)
   end subroutine put_DataToFile

   subroutine get_DataFromMem(this,multi, rc)
     class (AbstractServer),target, intent(inout) :: this
     logical, intent(in) :: multi
     integer, optional, intent(out) :: rc
     _FAIL(" no action of server_get_DataFromMem")
     _UNUSED_DUMMY(this)
     _UNUSED_DUMMY(multi)
   end subroutine get_DataFromMem

   function am_I_reading_PE(this,id) result (yes)
      class(AbstractServer),intent(in) :: this
      integer, intent(in) :: id
      integer :: node_rank,innode_rank
      logical :: yes

      call this%distribute_task(id, node_rank,innode_rank)
      yes = (node_rank   == this%Node_Rank) .and. &
            (innode_rank == this%InNode_Rank)
   end function am_I_reading_PE

   ! distribute the task (id) to a specific process (node_rank, innode_rank)
   subroutine distribute_task(this,id, node_rank, innode_rank)
      class(AbstractServer),intent(in) :: this
      integer, intent(in)   :: id
      integer,intent(out) :: node_rank,innode_rank
      integer :: rank

      innode_rank = mod(id, this%InNode_npes)
      rank        = mod(id, this%npes)
      node_rank   = this%Node_Ranks(rank)

     _UNUSED_DUMMY(this)
   end subroutine distribute_task

   function get_writing_PE(this,id) result (rank)
      class(AbstractServer),intent(in) :: this
      integer, intent(in) :: id
      integer :: rank
      integer :: rank_tmp, ierror, rc

      integer :: node_rank,innode_rank
      logical :: yes

      node_rank   = mod(id-1,this%node_num)
      innode_rank = mod(id-1,this%innode_npes)

      yes = (node_rank   == this%Node_Rank) .and. &
            (innode_rank == this%InNode_Rank)
      rank_tmp = 0
      rank = 0
      if (yes) rank_tmp = this%rank
      call Mpi_Allreduce(rank_tmp,rank,1, MPI_INTEGER, MPI_SUM, this%comm, ierror)
      _VERIFY(ierror)

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

   subroutine add_DataReference(this,DataRef)
      class (AbstractServer), intent(inout) :: this
      class(AbstractDataReference),target,intent(in) :: DataRef

      call this%dataRefPtrs%push_back(DataRef)
      call this%set_status(this%num_clients)

   end subroutine add_DataReference

   subroutine clear_DataReference(this)
      class (AbstractServer), target, intent(inout) :: this
      class (AbstractDataReference), pointer :: datarefPtr
      integer :: n, i

      n = this%dataRefPtrs%size()
      do i = 1, n
         dataRefPtr => this%dataRefPtrs%at(i)
         call dataRefPtr%deallocate()
      enddo
      call this%dataRefPtrs%erase(this%dataRefPtrs%begin(), this%dataRefPtrs%end())

   end subroutine clear_DataReference

   integer function get_communicator(this) result(communicator)
      class (AbstractServer), intent(in) :: this

      communicator = this%comm

   end function get_communicator

   subroutine report_profile(this, rc )
      class (AbstractServer), intent(inout) :: this
      integer, optional,   intent(  out) :: RC     ! Error code:
      character(:), allocatable :: report_lines(:)
      type (ProfileReporter) :: reporter
      character(1) :: empty(0)
      integer :: i, status

      if ( .not. associated(ioserver_profiler)) then
         _RETURN(_SUCCESS)
      endif

      call ioserver_profiler%stop(_RC)
      call ioserver_profiler%reduce()

      reporter = ProfileReporter(empty)
      call reporter%add_column(NameColumn(20))
      call reporter%add_column(FormattedTextColumn('Inclusive','(f10.2)', 10, InclusiveColumn('MEAN')))
      call reporter%add_column(FormattedTextColumn('% Incl','(f6.2)', 6, PercentageColumn(InclusiveColumn('MEAN'),'MAX')))
      call reporter%add_column(FormattedTextColumn('Exclusive','(f10.2)', 10, ExclusiveColumn('MEAN')))
      call reporter%add_column(FormattedTextColumn('% Excl','(f6.2)', 6, PercentageColumn(ExclusiveColumn('MEAN'))))
      call reporter%add_column(FormattedTextColumn(' Max Excl)','(f10.2)', 10, ExclusiveColumn('MAX')))
      call reporter%add_column(FormattedTextColumn(' Min Excl)','(f10.2)', 10, ExclusiveColumn('MIN')))
      call reporter%add_column(FormattedTextColumn('Max PE)','(1x,i4.4,1x)', 6, ExclusiveColumn('MAX_PE')))
      call reporter%add_column(FormattedTextColumn('Min PE)','(1x,i4.4,1x)', 6, ExclusiveColumn('MIN_PE')))
      report_lines = reporter%generate_report(ioserver_profiler)

      if (this%rank == 0) then
         write(*,'(a)')'Final io_server profile'
         write(*,'(a)')'============='
         do i = 1, size(report_lines)
            write(*,'(a)') report_lines(i)
         end do
         write(*,'(a)') ''
      end if

      deallocate(ioserver_profiler)
      _RETURN(_SUCCESS)
   end subroutine report_profile

end module pFIO_AbstractServerMod
