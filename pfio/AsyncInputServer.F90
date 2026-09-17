#include "MAPL_ErrLog.h"

module pFIO_AsyncInputServerMod
   use, intrinsic :: iso_c_binding, only: c_f_pointer, c_null_ptr, c_ptr
   use, intrinsic :: iso_fortran_env, only: INT32, INT64, REAL32, REAL64
   use mapl_ErrorHandling_mod
   use mapl_Profiler_mod
   use mapl_Sleep_mod, only: MAPL_Sleep
   use pFIO_AbstractMessageMod
   use pFIO_ConstantsMod
   use pFIO_AbstractSocketMod
   use pFIO_AbstractRequestHandleMod
   use pFIO_UtilitiesMod, only: word_size
    use pFIO_MessageVectorMod
    use pFIO_MessageVectorUtilMod
    use pFIO_CollectivePrefetchDataMessageMod
    use pFIO_NextCollectivePrefetchMessageMod
    use pFIO_LocalMemReferenceMod
     use pFIO_NetCDF4_FileFormatterMod
   use pFIO_ServerThreadMod
   use pFIO_ServerThreadVectorMod
   use pFIO_BaseServerMod
    use mpi

   implicit none
   private

   public :: AsyncInputServer

     integer, parameter :: ASYNC_INPUT_CMD_READ       = 1
     integer, parameter :: ASYNC_INPUT_CMD_NEXT_PREFETCH = 3
    integer, parameter :: ASYNC_INPUT_CMD_TERMINATE  = -1
    integer, parameter :: ASYNC_INPUT_TAG_CMD        = 4701
    integer, parameter :: ASYNC_INPUT_TAG_SIZE       = 4702
    integer, parameter :: ASYNC_INPUT_TAG_BUFFER     = 4703
      integer, parameter :: ASYNC_INPUT_TAG_WORKER_RANK = 4705
     integer, parameter :: ASYNC_INPUT_TAG_READER_CMD = 4711
     integer, parameter :: ASYNC_INPUT_TAG_READER_SIZE = 4712
     integer, parameter :: ASYNC_INPUT_TAG_READER_BUFFER = 4713
     integer, parameter :: ASYNC_INPUT_TAG_READER_DONE = 4714
     integer, parameter :: ASYNC_INPUT_TAG_READER_RESULT_SIZE = 4715
     integer, parameter :: ASYNC_INPUT_TAG_READER_SOURCE = 4716
     integer, parameter :: ASYNC_INPUT_TAG_READER_CACHE_SLOT = 4717
     integer, parameter :: ASYNC_INPUT_TAG_READER_TERMINATED = 4718
     integer, parameter :: ASYNC_INPUT_DEFAULT_CACHE_SLOTS = 2
     integer, parameter :: ASYNC_INPUT_MAILBOX_EMPTY = 0
     integer, parameter :: ASYNC_INPUT_MAILBOX_READY = 1
     integer, parameter :: ASYNC_INPUT_MAILBOX_OVERFLOW = 2
     integer, parameter :: ASYNC_INPUT_MAILBOX_HEADER_WORDS = 2
     integer, parameter :: ASYNC_INPUT_DEFAULT_MAILBOX_WORDS = 4 * 1024 * 1024

    ! -----------------------------------------------------------------------
    ! Reader-side cache slot.
    !
    ! Key: (file_name, var_name, type_kind, global_start, global_count) —
    !      the full global slab extent, identical across all model ranks.
    !
    ! Payload: a LocalMemReference holding the complete global slab.
    !
    ! When model rank 0 sends the first request for a unique global slab:
    !   cache miss → read global slab → store in slot → extract rank-0 slice.
    ! When model ranks 1..N-1 send the same global key:
    !   cache hit  → extract their slice from the cached global slab.
    !
    ! This way the file is read ONCE per unique (file, var, timestep) no
    ! matter how many model ranks exist on the node.
    ! -----------------------------------------------------------------------
    type :: AsyncInputCacheSlot
      logical :: valid = .false.
      character(len=:), allocatable :: file_name
      character(len=:), allocatable :: var_name
      integer :: type_kind = 0
      integer, allocatable :: global_start(:)
      integer, allocatable :: global_count(:)
      type(LocalMemReference), allocatable :: reference   ! holds full global slab
    end type AsyncInputCacheSlot

    type :: AsyncInputPendingRequest
       integer :: command = ASYNC_INPUT_CMD_READ
       integer :: source_rank = -1
       character(len=:), allocatable :: file_name
       integer, allocatable :: buffer(:)
    end type AsyncInputPendingRequest

    type :: AsyncInputWorkerState
       logical :: busy = .false.
       integer :: command = ASYNC_INPUT_CMD_READ
       integer :: source_rank = -1
       character(len=:), allocatable :: file_name
       integer, allocatable :: buffer(:)
    end type AsyncInputWorkerState

    type :: AsyncInputFileReadRecord
       character(len=:), allocatable :: file_name
       integer :: worker_rank = -1
    end type AsyncInputFileReadRecord

    type :: AsyncInputWarmRecord
       integer :: worker_rank = -1
       integer :: slot_index = 0
       character(len=:), allocatable :: file_name
       character(len=:), allocatable :: var_name
       integer :: type_kind = 0
       integer, allocatable :: global_start(:)
       integer, allocatable :: global_count(:)
    end type AsyncInputWarmRecord

    type, extends(BaseServer) :: AsyncInputServer
      character(len=:), allocatable :: port_name
      integer :: model_comm = MPI_COMM_NULL
      integer :: model_node_comm = MPI_COMM_NULL
      integer :: node_comm = MPI_COMM_NULL
      integer :: reader_comm = MPI_COMM_NULL
      integer :: node_npes = 0
      integer :: model_npes_on_node = 0
      integer :: model_node_rank = -1
       integer :: reader_capacity_on_node = 0
       logical :: synchronous_fallback = .true.
       integer, allocatable :: reader_ranks_on_node(:)
       integer :: reader_comm_size = 0
       integer, allocatable :: reader_global_ranks(:)
       integer, allocatable :: node_global_ranks(:)
       integer :: shared_win = MPI_WIN_NULL
       type(c_ptr) :: shared_base_address = c_null_ptr
       type(c_ptr) :: shared_cache_base_address = c_null_ptr
       integer :: shared_mailbox_words = ASYNC_INPUT_DEFAULT_MAILBOX_WORDS
       integer :: num_cache_slots = ASYNC_INPUT_DEFAULT_CACHE_SLOTS
       type(AsyncInputCacheSlot), allocatable :: cache_slots(:)
      integer :: next_cache_slot = 1
      integer :: cache_hits = 0
      integer :: cache_misses = 0
      integer :: demand_cache_misses = 0
      integer :: prefetch_cache_misses = 0
       integer :: forwarded_requests = 0
       integer :: captain_warm_hits = 0
      integer :: captain_prefetch_hits = 0
      integer :: reader_requests = 0
      integer :: reader_comm_rank = -1
       contains
       procedure :: start
       procedure :: stop_reader_pool
       procedure :: release_runtime
       procedure :: service_collective_prefetch
       procedure :: service_next_collective_prefetch
       end type AsyncInputServer

   interface AsyncInputServer
      module procedure new_AsyncInputServer
   end interface AsyncInputServer

contains

   function new_AsyncInputServer(comm, port_name, model_comm, profiler_name, with_profiler, rc) result(s)
      type(AsyncInputServer) :: s
      integer, intent(in) :: comm
      character(*), intent(in) :: port_name
      integer, optional, intent(in) :: model_comm
      character(*), optional, intent(in) :: profiler_name
      logical, optional, intent(in) :: with_profiler
      integer, optional, intent(out) :: rc
      integer :: status
      character(len=32) :: sleep_string
      integer :: sleep_length, sleep_status

      s%port_name = trim(port_name)
      s%threads = ServerThreadVector()
      s%model_comm = MPI_COMM_NULL
      if (present(model_comm)) s%model_comm = model_comm

       call get_environment_variable('MAPL_ASYNC_INPUT_SHMEM_WORDS', sleep_string, sleep_length, sleep_status)
      if (sleep_status == 0 .and. sleep_length > 0) then
         read(sleep_string(1:sleep_length), *, iostat=sleep_status) s%shared_mailbox_words
         if (sleep_status /= 0 .or. s%shared_mailbox_words < 1) &
              s%shared_mailbox_words = ASYNC_INPUT_DEFAULT_MAILBOX_WORDS
      end if

      call get_environment_variable('MAPL_ASYNC_INPUT_CACHE_SLOTS', sleep_string, sleep_length, sleep_status)
      if (sleep_status == 0 .and. sleep_length > 0) then
         read(sleep_string(1:sleep_length), *, iostat=sleep_status) s%num_cache_slots
         if (sleep_status /= 0 .or. s%num_cache_slots < 1) &
              s%num_cache_slots = ASYNC_INPUT_DEFAULT_CACHE_SLOTS
      end if
      allocate(s%cache_slots(s%num_cache_slots))

      call s%init(comm, port_name, profiler_name=profiler_name, with_profiler=with_profiler, _RC)
      call initialize_role_accounting(s, comm, _RC)

      _RETURN(_SUCCESS)
   end function new_AsyncInputServer

   subroutine initialize_role_accounting(this, comm, rc)
      class(AsyncInputServer), intent(inout) :: this
      integer, intent(in) :: comm
      integer, optional, intent(out) :: rc

      integer :: ierror, status
      integer :: reader_color

      call MPI_Comm_split_type(comm, MPI_COMM_TYPE_SHARED, 0, MPI_INFO_NULL, this%node_comm, ierror)
      _VERIFY(ierror)

      this%model_npes_on_node = 0
      this%model_node_rank = -1
      if (this%model_comm /= MPI_COMM_NULL) then
         call MPI_Comm_split_type(this%model_comm, MPI_COMM_TYPE_SHARED, 0, MPI_INFO_NULL, this%model_node_comm, ierror)
         _VERIFY(ierror)
         call MPI_Comm_size(this%model_node_comm, this%model_npes_on_node, ierror)
         _VERIFY(ierror)
         call MPI_Comm_rank(this%model_node_comm, this%model_node_rank, ierror)
         _VERIFY(ierror)
      end if

      call MPI_Comm_size(this%node_comm, this%node_npes, ierror)
      _VERIFY(ierror)

      reader_color = MPI_UNDEFINED
      if (this%model_comm == MPI_COMM_NULL) reader_color = 1
      call MPI_Comm_split(this%node_comm, reader_color, this%rank, this%reader_comm, ierror)
      _VERIFY(ierror)
       if (this%reader_comm /= MPI_COMM_NULL) then
          call MPI_Comm_rank(this%reader_comm, this%reader_comm_rank, ierror)
          _VERIFY(ierror)
          call MPI_Comm_size(this%reader_comm, this%reader_comm_size, ierror)
          _VERIFY(ierror)
          allocate(this%reader_global_ranks(this%reader_comm_size))
          call MPI_Allgather(this%rank, 1, MPI_INTEGER, this%reader_global_ranks, 1, &
               MPI_INTEGER, this%reader_comm, ierror)
          _VERIFY(ierror)
       end if

      this%reader_capacity_on_node = this%node_npes - this%model_npes_on_node
      _ASSERT(this%reader_capacity_on_node >= 0, 'reader_capacity_on_node must be non-negative')
      this%synchronous_fallback = (this%reader_capacity_on_node == 0)

      call gather_reader_ranks(this, _RC)
      if (.not. this%synchronous_fallback) call initialize_shared_mailboxes(this, _RC)

      if (this%InNode_Rank == 0) then
         write(*,'(A,1X,A,1X,A,I0,1X,A,I0,1X,A,I0,1X,A,L1)') &
              'INFO: AsyncInputServer:', trim(this%port_name), &
              'model_size_on_node=', this%model_npes_on_node, &
              'node_size=', this%node_npes, &
              'reader_capacity_on_node=', this%reader_capacity_on_node, &
              'synchronous_fallback=', this%synchronous_fallback
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(comm)
    end subroutine initialize_role_accounting

    subroutine gather_reader_ranks(this, rc)
      class(AsyncInputServer), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer, allocatable :: model_ranks(:)
      integer :: i, j, status
      logical :: is_model_rank

      allocate(this%node_global_ranks(this%node_npes))
      call MPI_Allgather(this%rank, 1, MPI_INTEGER, this%node_global_ranks, 1, MPI_INTEGER, this%node_comm, status)
      _VERIFY(status)

      allocate(model_ranks(this%model_npes_on_node))
      if (this%model_npes_on_node > 0) then
         call MPI_Allgather(this%rank, 1, MPI_INTEGER, model_ranks, 1, MPI_INTEGER, this%model_node_comm, status)
         _VERIFY(status)
      end if

      allocate(this%reader_ranks_on_node(this%reader_capacity_on_node))
      j = 0
      do i = 1, size(this%node_global_ranks)
         is_model_rank = .false.
         if (this%model_npes_on_node > 0) then
            is_model_rank = any(model_ranks == this%node_global_ranks(i))
         end if
         if (.not. is_model_rank) then
            j = j + 1
            if (j <= size(this%reader_ranks_on_node)) this%reader_ranks_on_node(j) = this%node_global_ranks(i)
         end if
      end do

      _RETURN(_SUCCESS)
    end subroutine gather_reader_ranks

    subroutine initialize_shared_mailboxes(this, rc)
       class(AsyncInputServer), intent(inout) :: this
       integer, optional, intent(out) :: rc

       integer(kind=MPI_ADDRESS_KIND) :: local_bytes
       integer :: ierr, n_workers
       integer, pointer :: shared_words(:)
#if !defined (SUPPORT_FOR_MPI_ALLOC_MEM_CPTR)
       integer(kind=MPI_ADDRESS_KIND) :: baseaddr
#endif

       n_workers = this%reader_capacity_on_node - 1
       _ASSERT(n_workers > 0, 'nonfallback AsyncInputServer requires at least one reader worker')
       local_bytes = 0_MPI_ADDRESS_KIND
       if (this%model_comm /= MPI_COMM_NULL) then
          local_bytes = int(n_workers, MPI_ADDRESS_KIND) * &
               int(ASYNC_INPUT_MAILBOX_HEADER_WORDS + this%shared_mailbox_words, MPI_ADDRESS_KIND) * &
               4_MPI_ADDRESS_KIND
       else if (this%reader_comm_rank > 0) then
          local_bytes = int(this%num_cache_slots, MPI_ADDRESS_KIND) * &
               int(this%shared_mailbox_words, MPI_ADDRESS_KIND) * 4_MPI_ADDRESS_KIND
       end if

#if defined(SUPPORT_FOR_MPI_ALLOC_MEM_CPTR)
       call MPI_Win_allocate_shared(local_bytes, 4, MPI_INFO_NULL, this%node_comm, &
            this%shared_base_address, this%shared_win, ierr)
#else
       call MPI_Win_allocate_shared(local_bytes, 4, MPI_INFO_NULL, this%node_comm, &
            baseaddr, this%shared_win, ierr)
       this%shared_base_address = transfer(baseaddr, this%shared_base_address)
#endif
       _VERIFY(ierr)

       call MPI_Win_lock_all(0, this%shared_win, ierr)
       _VERIFY(ierr)

       if (this%model_comm /= MPI_COMM_NULL) then
          call c_f_pointer(this%shared_base_address, shared_words, &
               [n_workers * (ASYNC_INPUT_MAILBOX_HEADER_WORDS + this%shared_mailbox_words)])
          shared_words = ASYNC_INPUT_MAILBOX_EMPTY
          call MPI_Win_sync(this%shared_win, ierr)
          _VERIFY(ierr)
       else if (this%reader_comm_rank > 0) then
          this%shared_cache_base_address = this%shared_base_address
       end if
       call MPI_Barrier(this%node_comm, ierr)
       _VERIFY(ierr)

       _RETURN(_SUCCESS)
    end subroutine initialize_shared_mailboxes

    ! -----------------------------------------------------------------------
    ! Main server loop.
    !
    ! Reader ranks use reader_comm rank 0 as captain. The captain schedules
    ! requests on worker ranks, which own the file cache and publish current
    ! results directly to model-rank shared-memory mailboxes. Cache-only
    ! requests populate the worker cache without publishing a result.
    !
    ! Model ranks (model_comm /= MPI_COMM_NULL):
    !   Standard ServerThread dispatch loop.
    !   Each model rank sends its full request, including global extents, to
    !   its node-local captain and waits only when it needs the current slice.
    ! -----------------------------------------------------------------------
    subroutine start(this, rc)
       class(AsyncInputServer), target, intent(inout) :: this
       integer, optional, intent(out) :: rc
       class(ServerThread), pointer :: thread_ptr => null()
       integer :: i, client_size
       logical, allocatable :: mask(:)
       integer :: status, ierr, cmd, source_rank, buffer_size, slot_index, msize_word
       integer :: mpi_status(MPI_STATUS_SIZE)
       integer, allocatable :: buffer(:), result(:)
       type(AsyncInputPendingRequest), allocatable :: pending(:)
       type(AsyncInputWorkerState), allocatable :: workers(:)
       type(AsyncInputFileReadRecord), allocatable :: active_reads(:)
       type(AsyncInputFileReadRecord), allocatable :: file_owners(:)
       type(AsyncInputWarmRecord), allocatable :: warm_records(:)
       logical :: message_available

       if (this%model_comm == MPI_COMM_NULL) then
          if (this%reader_comm_rank /= 0) then
             do while (.true.)
                 call MPI_Recv(cmd, 1, MPI_INTEGER, 0, ASYNC_INPUT_TAG_READER_CMD, &
                      this%reader_comm, mpi_status, ierr)
                 _VERIFY(ierr)
                 if (cmd == ASYNC_INPUT_CMD_TERMINATE) then
                    call MPI_Send(ASYNC_INPUT_CMD_TERMINATE, 1, MPI_INTEGER, 0, &
                         ASYNC_INPUT_TAG_READER_TERMINATED, this%reader_comm, ierr)
                    _VERIFY(ierr)
                    exit
                 end if
                _ASSERT(cmd == ASYNC_INPUT_CMD_READ .or. cmd == ASYNC_INPUT_CMD_NEXT_PREFETCH, &
                     'unknown worker command')
                 call MPI_Recv(buffer_size, 1, MPI_INTEGER, 0, ASYNC_INPUT_TAG_READER_SIZE, &
                      this%reader_comm, mpi_status, ierr)
                 _VERIFY(ierr)
                 call MPI_Recv(source_rank, 1, MPI_INTEGER, 0, ASYNC_INPUT_TAG_READER_SOURCE, &
                      this%reader_comm, mpi_status, ierr)
                 _VERIFY(ierr)
                 allocate(buffer(buffer_size))
                call MPI_Recv(buffer, buffer_size, MPI_INTEGER, 0, ASYNC_INPUT_TAG_READER_BUFFER, &
                     this%reader_comm, mpi_status, ierr)
                _VERIFY(ierr)
                 call execute_reader_request(this, buffer, buffer_size, result, msize_word, slot_index, _RC)
                 deallocate(buffer)
                 call publish_shared_cache_slot(this, slot_index, _RC)
                 if (msize_word > 0) then
                    call publish_shared_result(this, source_rank, this%reader_comm_rank, &
                         result, msize_word, _RC)
                    deallocate(result)
                 end if
                 call MPI_Send(0, 1, MPI_INTEGER, 0, ASYNC_INPUT_TAG_READER_DONE, this%reader_comm, ierr)
                _VERIFY(ierr)
                 call MPI_Send(msize_word, 1, MPI_INTEGER, 0, ASYNC_INPUT_TAG_READER_RESULT_SIZE, &
                      this%reader_comm, ierr)
                 _VERIFY(ierr)
                 call MPI_Send(slot_index, 1, MPI_INTEGER, 0, ASYNC_INPUT_TAG_READER_CACHE_SLOT, &
                      this%reader_comm, ierr)
                 _VERIFY(ierr)
              end do
              write(*,'(A,1X,A,I0,1X,A,I0,1X,A,I0,1X,A,I0,1X,A,I0,1X,A,I0,1X,A,I0)') &
                   'INFO: AsyncInputServer cache:', 'reader_rank=', this%rank, &
                   'slots=', this%num_cache_slots, 'hits=', this%cache_hits, &
                   'misses=', this%cache_misses, 'demand_misses=', this%demand_cache_misses, &
                   'prefetch_misses=', this%prefetch_cache_misses, 'requests=', this%reader_requests
              call finalize_runtime(this, _RC)
             _RETURN(_SUCCESS)
          end if

            _ASSERT(this%reader_comm_size > 1, &
                 'nonfallback AsyncInputServer requires one reader captain and at least one worker')
             allocate(workers(this%reader_comm_size - 1))
             allocate(pending(0))
             allocate(active_reads(0))
             allocate(file_owners(0))
             allocate(warm_records(0))
             do while (.true.)
                call poll_reader_completions(this, workers, active_reads, warm_records, .false., ierr)
                _VERIFY(ierr)
                call serve_warm_requests(this, pending, workers, warm_records, ierr)
                _VERIFY(ierr)
                call dispatch_pending_requests(this, pending, workers, active_reads, file_owners, ierr)
               _VERIFY(ierr)
               call MPI_Iprobe(MPI_ANY_SOURCE, ASYNC_INPUT_TAG_CMD, this%comm, message_available, mpi_status, ierr)
              _VERIFY(ierr)
              if (.not. message_available) then
                 call MAPL_Sleep(0.0001)
                 cycle
              end if
              call MPI_Recv(cmd, 1, MPI_INTEGER, MPI_ANY_SOURCE, ASYNC_INPUT_TAG_CMD, &
                   this%comm, mpi_status, ierr)
              _VERIFY(ierr)
              if (cmd == ASYNC_INPUT_CMD_TERMINATE) exit
              source_rank = mpi_status(MPI_SOURCE)
              call MPI_Recv(buffer_size, 1, MPI_INTEGER, source_rank, ASYNC_INPUT_TAG_SIZE, &
                   this%comm, mpi_status, ierr)
              _VERIFY(ierr)
              allocate(buffer(buffer_size))
              call MPI_Recv(buffer, buffer_size, MPI_INTEGER, source_rank, ASYNC_INPUT_TAG_BUFFER, &
                   this%comm, mpi_status, ierr)
              _VERIFY(ierr)
               _ASSERT(cmd == ASYNC_INPUT_CMD_READ .or. cmd == ASYNC_INPUT_CMD_NEXT_PREFETCH, &
                    'unknown reader captain command')
                call enqueue_reader_request(pending, cmd, source_rank, buffer, _RC)
                deallocate(buffer)
                call serve_warm_requests(this, pending, workers, warm_records, ierr)
                _VERIFY(ierr)
                call dispatch_pending_requests(this, pending, workers, active_reads, file_owners, ierr)
               _VERIFY(ierr)
            end do

             do while (any(workers%busy) .or. size(pending) > 0)
               call poll_reader_completions(this, workers, active_reads, warm_records, .true., ierr)
               _VERIFY(ierr)
               call serve_warm_requests(this, pending, workers, warm_records, ierr)
               _VERIFY(ierr)
               call dispatch_pending_requests(this, pending, workers, active_reads, file_owners, ierr)
                _VERIFY(ierr)
             end do
             write(*,'(A,1X,A,I0,1X,A,I0)') 'INFO: AsyncInputServer captain cache:', &
                  'warm_hits=', this%captain_warm_hits, 'prefetch_hits=', this%captain_prefetch_hits
             do i = 1, size(workers)
                call MPI_Send(ASYNC_INPUT_CMD_TERMINATE, 1, MPI_INTEGER, i, &
                     ASYNC_INPUT_TAG_READER_CMD, this%reader_comm, ierr)
                _VERIFY(ierr)
             end do
             do i = 1, size(workers)
                call MPI_Recv(cmd, 1, MPI_INTEGER, i, ASYNC_INPUT_TAG_READER_TERMINATED, &
                     this%reader_comm, MPI_STATUS_IGNORE, ierr)
                _VERIFY(ierr)
                _ASSERT(cmd == ASYNC_INPUT_CMD_TERMINATE, 'reader worker returned an invalid shutdown acknowledgment')
             end do
             deallocate(active_reads)
             deallocate(file_owners)
             deallocate(warm_records)
            deallocate(pending)
            deallocate(workers)
          call finalize_runtime(this, _RC)
          _RETURN(_SUCCESS)
       end if

      client_size = this%threads%size()

      allocate(this%serverthread_done_msgs(client_size))
      this%serverthread_done_msgs(:) = .false.

      allocate(mask(client_size))
      mask = .false.
      do while (.true.)

         do i = 1, client_size

            if (mask(i)) cycle

            thread_ptr => this%threads%at(i)
            call thread_ptr%run(_RC)
            if (thread_ptr%do_terminate()) then
               mask(i) = .true.
            end if
         end do

         if (all(mask)) exit

      end do

       call this%threads%clear()
       deallocate(mask)

       if (.not. this%synchronous_fallback .and. this%model_comm /= MPI_COMM_NULL .and. this%model_node_rank == 0) then
          write(*,'(A,1X,A,I0)') 'INFO: AsyncInputServer forwarded:', 'requests=', this%forwarded_requests
       end if

        call this%stop_reader_pool(_RC)

       call this%report_profile(_RC)
       call finalize_runtime(this, _RC)

       _RETURN(_SUCCESS)
     end subroutine start

     subroutine stop_reader_pool(this, rc)
       class(AsyncInputServer), intent(inout) :: this
       integer, optional, intent(out) :: rc

        integer :: status

        if (.not. this%synchronous_fallback .and. this%model_comm /= MPI_COMM_NULL .and. this%model_node_rank == 0) then
           call MPI_Send(ASYNC_INPUT_CMD_TERMINATE, 1, MPI_INTEGER, this%reader_ranks_on_node(1), &
                ASYNC_INPUT_TAG_CMD, this%comm, status)
           _VERIFY(status)
        end if

        _RETURN(_SUCCESS)
      end subroutine stop_reader_pool

     subroutine release_runtime(this, rc)
        class(AsyncInputServer), intent(inout) :: this
        integer, optional, intent(out) :: rc
        integer :: status

        call finalize_runtime(this, _RC)
        _RETURN(_SUCCESS)
      end subroutine release_runtime

    ! -----------------------------------------------------------------------
    ! service_collective_prefetch
    !
    ! Each model rank independently forwards its own request to the reader.
    ! The reader deduplicates on the GLOBAL key (file, var, global extents),
    ! so the file is read at most once per unique slab.  The reader then
    ! extracts and returns each rank's LOCAL slice.  No collective operations
    ! between model ranks needed here.
    ! -----------------------------------------------------------------------
    subroutine service_collective_prefetch(this, request_backlog, connection, handled, rc)
       class(AsyncInputServer), intent(inout) :: this
       type(MessageVector), intent(inout) :: request_backlog
       class(AbstractSocket), intent(inout), target :: connection
       logical, intent(out) :: handled
       integer, optional, intent(out) :: rc

       type(MessageVectorIterator) :: iter
       class(AbstractMessage), pointer :: msg
       integer :: status
       logical :: removed

       handled = .false.
        if (this%synchronous_fallback) then
           _RETURN(_SUCCESS)
        end if
        _ASSERT(size(this%reader_ranks_on_node) > 0, 'reader ranks must exist when not in synchronous fallback')

        iter = request_backlog%begin()
       do while (iter /= request_backlog%end())
          removed = .false.
          msg => iter%get()
          select type (q => msg)
          type is (CollectivePrefetchDataMessage)
             call forward_request_to_reader(this, q, connection, .true., _RC)
             call request_backlog%erase(iter)
             removed = .true.
          class default
             call iter%next()
          end select
          if (removed) iter = request_backlog%begin()
         end do

       call finish_collective_service(this, request_backlog, _RC)
       handled = .true.
       _RETURN(_SUCCESS)
    end subroutine service_collective_prefetch

      subroutine service_next_collective_prefetch(this, request_backlog, connection, handled, rc)
        class(AsyncInputServer), intent(inout) :: this
        type(MessageVector), intent(inout) :: request_backlog
        class(AbstractSocket), intent(inout), target :: connection
        logical, intent(out) :: handled
        integer, optional, intent(out) :: rc

        type(MessageVectorIterator) :: iter
        class(AbstractMessage), pointer :: msg
        integer :: status
        logical :: removed

        handled = .false.
        iter = request_backlog%begin()
       do while (iter /= request_backlog%end())
          removed = .false.
          msg => iter%get()
          select type (q => msg)
          type is (NextCollectivePrefetchMessage)
             if (.not. this%synchronous_fallback) then
                _ASSERT(size(this%reader_ranks_on_node) > 0, 'reader ranks must exist when not in synchronous fallback')
                 call forward_request_to_reader(this, q, connection, .false., ASYNC_INPUT_CMD_NEXT_PREFETCH, _RC)
             end if
             call request_backlog%erase(iter)
             removed = .true.
          class default
             call iter%next()
          end select
          if (removed) iter = request_backlog%begin()
        end do

       call finish_collective_service(this, request_backlog, _RC)
       handled = .true.
       _RETURN(_SUCCESS)
     end subroutine service_next_collective_prefetch

     subroutine execute_reader_request(this, input, input_size, result, result_size, slot_index, rc)
       class(AsyncInputServer), intent(inout) :: this
       integer, intent(in) :: input(:), input_size
       integer, allocatable, intent(out) :: result(:)
       integer, intent(out) :: result_size, slot_index
       integer, optional, intent(out) :: rc
       type(CollectivePrefetchDataMessage) :: request
        integer :: status

       call request%deserialize(input(1:input_size), _RC)
       slot_index = find_cache_slot(this, request)
       if (slot_index > 0) then
          this%cache_hits = this%cache_hits + 1
        else
           this%cache_misses = this%cache_misses + 1
           if (request%cache_only) then
              this%prefetch_cache_misses = this%prefetch_cache_misses + 1
           else
              this%demand_cache_misses = this%demand_cache_misses + 1
           end if
           slot_index = choose_cache_slot(this)
          call read_global_slab_into_slot(this, request, slot_index, _RC)
       end if

       result_size = 0
       if (.not. request%cache_only) then
          result_size = int(word_size(request%type_kind) * product(int(request%count, INT64)))
          allocate(result(result_size))
          call extract_local_slice_from_slot(this, request, slot_index, result, _RC)
       else
          allocate(result(0))
       end if
       this%reader_requests = this%reader_requests + 1
       _RETURN(_SUCCESS)
     end subroutine execute_reader_request

     subroutine publish_shared_cache_slot(this, slot_index, rc)
        class(AsyncInputServer), intent(inout) :: this
        integer, intent(in) :: slot_index
        integer, optional, intent(out) :: rc

        integer(INT64) :: cache_words, offset
        integer :: ierr
        integer, pointer :: cache_data(:), slot_data(:)

        cache_words = word_size(this%cache_slots(slot_index)%type_kind) * &
             product(int(this%cache_slots(slot_index)%global_count, INT64))
        _ASSERT(cache_words <= this%shared_mailbox_words, &
             'AsyncInputServer shared cache slot is too small; increase MAPL_ASYNC_INPUT_SHMEM_WORDS')
        call c_f_pointer(this%shared_cache_base_address, cache_data, &
             [this%num_cache_slots * this%shared_mailbox_words])
        call c_f_pointer(this%cache_slots(slot_index)%reference%base_address, slot_data, [cache_words])
        offset = int(slot_index - 1, INT64) * this%shared_mailbox_words
        cache_data(offset + 1:offset + cache_words) = slot_data
        call MPI_Win_sync(this%shared_win, ierr)
        if (ierr /= MPI_SUCCESS) return

        _RETURN(_SUCCESS)
     end subroutine publish_shared_cache_slot

     subroutine enqueue_reader_request(pending, command, source_rank, input, rc)
       type(AsyncInputPendingRequest), allocatable, intent(inout) :: pending(:)
       integer, intent(in) :: command, source_rank, input(:)
       integer, optional, intent(out) :: rc
       type(AsyncInputPendingRequest), allocatable :: expanded(:)
       type(CollectivePrefetchDataMessage) :: request
       integer :: n, status

       call request%deserialize(input, _RC)
       n = size(pending)
       allocate(expanded(n + 1))
       if (n > 0) expanded(1:n) = pending
       expanded(n + 1)%command = command
       expanded(n + 1)%source_rank = source_rank
       expanded(n + 1)%file_name = request%file_name
       allocate(expanded(n + 1)%buffer(size(input)))
       expanded(n + 1)%buffer = input
       call move_alloc(expanded, pending)
       _RETURN(_SUCCESS)
     end subroutine enqueue_reader_request

     subroutine dispatch_pending_requests(this, pending, workers, active_reads, file_owners, ierr)
       class(AsyncInputServer), intent(in) :: this
       type(AsyncInputPendingRequest), allocatable, intent(inout) :: pending(:)
       type(AsyncInputWorkerState), intent(inout) :: workers(:)
       type(AsyncInputFileReadRecord), allocatable, intent(inout) :: active_reads(:)
       type(AsyncInputFileReadRecord), allocatable, intent(inout) :: file_owners(:)
       integer, intent(out) :: ierr
       integer :: request_index, worker_rank

       ierr = MPI_SUCCESS
       do
          call select_pending_request(pending, workers, active_reads, file_owners, &
               request_index, worker_rank)
          if (request_index < 1) return

          call MPI_Send(pending(request_index)%command, 1, MPI_INTEGER, worker_rank, &
               ASYNC_INPUT_TAG_READER_CMD, this%reader_comm, ierr)
          if (ierr /= MPI_SUCCESS) return
          call MPI_Send(size(pending(request_index)%buffer), 1, MPI_INTEGER, worker_rank, &
               ASYNC_INPUT_TAG_READER_SIZE, this%reader_comm, ierr)
          if (ierr /= MPI_SUCCESS) return
          call MPI_Send(pending(request_index)%source_rank, 1, MPI_INTEGER, worker_rank, &
               ASYNC_INPUT_TAG_READER_SOURCE, this%reader_comm, ierr)
          if (ierr /= MPI_SUCCESS) return
          call MPI_Send(pending(request_index)%buffer, size(pending(request_index)%buffer), &
               MPI_INTEGER, worker_rank, ASYNC_INPUT_TAG_READER_BUFFER, this%reader_comm, ierr)
          if (ierr /= MPI_SUCCESS) return

          workers(worker_rank)%busy = .true.
          workers(worker_rank)%command = pending(request_index)%command
          workers(worker_rank)%source_rank = pending(request_index)%source_rank
          workers(worker_rank)%file_name = pending(request_index)%file_name
          allocate(workers(worker_rank)%buffer(size(pending(request_index)%buffer)))
          workers(worker_rank)%buffer = pending(request_index)%buffer
          call add_active_read(active_reads, workers(worker_rank)%file_name, worker_rank)
           if (workers(worker_rank)%command == ASYNC_INPUT_CMD_READ) then
              call MPI_Send(this%reader_global_ranks(worker_rank + 1), 1, MPI_INTEGER, &
                   workers(worker_rank)%source_rank, ASYNC_INPUT_TAG_WORKER_RANK, this%comm, ierr)
              if (ierr /= MPI_SUCCESS) return
           end if
          call remove_pending_request(pending, request_index)
       end do
     end subroutine dispatch_pending_requests

     subroutine poll_reader_completions(this, workers, active_reads, warm_records, wait_for_one, ierr)
       class(AsyncInputServer), intent(in) :: this
       type(AsyncInputWorkerState), intent(inout) :: workers(:)
       type(AsyncInputFileReadRecord), allocatable, intent(inout) :: active_reads(:)
       type(AsyncInputWarmRecord), allocatable, intent(inout) :: warm_records(:)
       logical, intent(in) :: wait_for_one
       integer, intent(out) :: ierr
       logical :: available
       integer :: result_size, slot_index, dummy, worker_rank, result_status(MPI_STATUS_SIZE)
       type(CollectivePrefetchDataMessage) :: request

       ierr = MPI_SUCCESS
       if (.not. any(workers%busy)) return
       do
          call MPI_Iprobe(MPI_ANY_SOURCE, ASYNC_INPUT_TAG_READER_DONE, this%reader_comm, &
               available, result_status, ierr)
          if (ierr /= MPI_SUCCESS) return
          if (available) exit
          if (.not. wait_for_one) return
          call MAPL_Sleep(0.0001)
       end do
       worker_rank = result_status(MPI_SOURCE)
       call MPI_Recv(dummy, 1, MPI_INTEGER, worker_rank, ASYNC_INPUT_TAG_READER_DONE, this%reader_comm, &
            result_status, ierr)
       if (ierr /= MPI_SUCCESS) return
       call MPI_Recv(result_size, 1, MPI_INTEGER, worker_rank, ASYNC_INPUT_TAG_READER_RESULT_SIZE, this%reader_comm, &
            MPI_STATUS_IGNORE, ierr)
       if (ierr /= MPI_SUCCESS) return
       call MPI_Recv(slot_index, 1, MPI_INTEGER, worker_rank, ASYNC_INPUT_TAG_READER_CACHE_SLOT, this%reader_comm, &
            MPI_STATUS_IGNORE, ierr)
       if (ierr /= MPI_SUCCESS) return
       call request%deserialize(workers(worker_rank)%buffer, ierr)
       if (ierr /= MPI_SUCCESS) return
       call update_warm_record(warm_records, request, worker_rank, slot_index)
       call remove_active_read(active_reads, workers(worker_rank)%file_name, worker_rank)
       workers(worker_rank)%busy = .false.
       workers(worker_rank)%source_rank = -1
       if (allocated(workers(worker_rank)%file_name)) deallocate(workers(worker_rank)%file_name)
       if (allocated(workers(worker_rank)%buffer)) deallocate(workers(worker_rank)%buffer)
     end subroutine poll_reader_completions

     subroutine serve_warm_requests(this, pending, workers, warm_records, ierr)
        class(AsyncInputServer), intent(inout) :: this
        type(AsyncInputPendingRequest), allocatable, intent(inout) :: pending(:)
        type(AsyncInputWorkerState), intent(in) :: workers(:)
        type(AsyncInputWarmRecord), intent(in) :: warm_records(:)
        integer, intent(out) :: ierr

        integer :: i, warm_index
        type(CollectivePrefetchDataMessage) :: request

        ierr = MPI_SUCCESS
        i = 1
        do while (i <= size(pending))
           call reset_prefetch_request(request)
           call request%deserialize(pending(i)%buffer, ierr)
           if (ierr /= MPI_SUCCESS) return
           warm_index = find_warm_record(warm_records, request)
           if (warm_index < 1) then
              i = i + 1
              cycle
           end if
           if (workers(warm_records(warm_index)%worker_rank)%busy) then
              i = i + 1
              cycle
           end if

           if (pending(i)%command == ASYNC_INPUT_CMD_READ) then
              call publish_warm_result(this, request, pending(i)%source_rank, warm_records(warm_index), ierr)
              if (ierr /= MPI_SUCCESS) return
              call MPI_Send(this%reader_global_ranks(warm_records(warm_index)%worker_rank + 1), &
                   1, MPI_INTEGER, pending(i)%source_rank, ASYNC_INPUT_TAG_WORKER_RANK, this%comm, ierr)
              if (ierr /= MPI_SUCCESS) return
              this%captain_warm_hits = this%captain_warm_hits + 1
           else
              this%captain_prefetch_hits = this%captain_prefetch_hits + 1
           end if
           call remove_pending_request(pending, i)
        end do
     end subroutine serve_warm_requests

     subroutine reset_prefetch_request(request)
        type(CollectivePrefetchDataMessage), intent(inout) :: request

        if (allocated(request%file_name)) deallocate(request%file_name)
        if (allocated(request%var_name)) deallocate(request%var_name)
        if (allocated(request%start)) deallocate(request%start)
        if (allocated(request%count)) deallocate(request%count)
        if (allocated(request%global_start)) deallocate(request%global_start)
        if (allocated(request%global_count)) deallocate(request%global_count)
     end subroutine reset_prefetch_request

     subroutine publish_warm_result(this, request, model_rank, warm_record, ierr)
        class(AsyncInputServer), intent(inout) :: this
        type(CollectivePrefetchDataMessage), intent(in) :: request
        integer, intent(in) :: model_rank
        type(AsyncInputWarmRecord), intent(in) :: warm_record
        integer, intent(out) :: ierr

        integer(kind=MPI_ADDRESS_KIND) :: segment_bytes
        integer(INT64) :: cache_words, cache_offset
        integer :: disp_unit, model_node_rank, result_size
        integer, allocatable :: result(:)
        integer, pointer :: cache_data(:)
        type(c_ptr) :: cache_base_address
#if !defined (SUPPORT_FOR_MPI_ALLOC_MEM_CPTR)
        integer(kind=MPI_ADDRESS_KIND) :: baseaddr
#endif

        ierr = MPI_SUCCESS
        model_node_rank = find_node_rank(this, model_rank)
        if (model_node_rank < 0) then
           ierr = MPI_ERR_RANK
           return
        end if
#if defined(SUPPORT_FOR_MPI_ALLOC_MEM_CPTR)
        call MPI_Win_shared_query(this%shared_win, find_node_rank(this, &
             this%reader_global_ranks(warm_record%worker_rank + 1)), segment_bytes, disp_unit, &
             cache_base_address, ierr)
#else
        call MPI_Win_shared_query(this%shared_win, find_node_rank(this, &
             this%reader_global_ranks(warm_record%worker_rank + 1)), segment_bytes, disp_unit, baseaddr, ierr)
        cache_base_address = transfer(baseaddr, cache_base_address)
#endif
        if (ierr /= MPI_SUCCESS) return
        call c_f_pointer(cache_base_address, cache_data, &
             [this%num_cache_slots * this%shared_mailbox_words])
        cache_words = word_size(request%type_kind) * product(int(request%global_count, INT64))
        cache_offset = int(warm_record%slot_index - 1, INT64) * this%shared_mailbox_words
        result_size = int(word_size(request%type_kind) * product(int(request%count, INT64)))
        allocate(result(result_size))
        call copy_subarray(cache_data(cache_offset + 1:cache_offset + cache_words), result, &
             request%global_count, request%start - request%global_start + 1, request%count, &
             size(request%global_count), word_size(request%type_kind))
        call publish_result_to_mailbox(this, model_node_rank, warm_record%worker_rank, result, result_size, ierr)
        deallocate(result)
     end subroutine publish_warm_result

     integer function find_warm_record(warm_records, request) result(record_index)
        type(AsyncInputWarmRecord), intent(in) :: warm_records(:)
        type(CollectivePrefetchDataMessage), intent(in) :: request
        integer :: i

        record_index = 0
        do i = 1, size(warm_records)
           if (warm_record_matches(warm_records(i), request)) then
              record_index = i
              return
           end if
        end do
     end function find_warm_record

     logical function warm_record_matches(record, request) result(matches)
        type(AsyncInputWarmRecord), intent(in) :: record
        type(CollectivePrefetchDataMessage), intent(in) :: request

        matches = record%type_kind == request%type_kind
        if (.not. matches) return
        matches = record%file_name == request%file_name .and. record%var_name == request%var_name
        if (.not. matches) return
        matches = size(record%global_start) == size(request%global_start) .and. &
             all(record%global_start == request%global_start)
        if (.not. matches) return
        matches = size(record%global_count) == size(request%global_count) .and. &
             all(record%global_count == request%global_count)
     end function warm_record_matches

     subroutine update_warm_record(warm_records, request, worker_rank, slot_index)
        type(AsyncInputWarmRecord), allocatable, intent(inout) :: warm_records(:)
        type(CollectivePrefetchDataMessage), intent(in) :: request
        integer, intent(in) :: worker_rank, slot_index
        type(AsyncInputWarmRecord), allocatable :: updated(:)
        integer :: i, n

        do i = 1, size(warm_records)
           if (warm_records(i)%worker_rank == worker_rank .and. &
                warm_records(i)%slot_index == slot_index) then
              warm_records(i)%file_name = request%file_name
              warm_records(i)%var_name = request%var_name
              warm_records(i)%type_kind = request%type_kind
              warm_records(i)%global_start = request%global_start
              warm_records(i)%global_count = request%global_count
              return
           end if
        end do
        n = size(warm_records)
        allocate(updated(n + 1))
        if (n > 0) updated(1:n) = warm_records
        updated(n + 1)%worker_rank = worker_rank
        updated(n + 1)%slot_index = slot_index
        updated(n + 1)%file_name = request%file_name
        updated(n + 1)%var_name = request%var_name
        updated(n + 1)%type_kind = request%type_kind
        updated(n + 1)%global_start = request%global_start
        updated(n + 1)%global_count = request%global_count
        call move_alloc(updated, warm_records)
     end subroutine update_warm_record

      subroutine select_pending_request(pending, workers, active_reads, file_owners, &
          request_index, worker_rank)
       type(AsyncInputPendingRequest), intent(in) :: pending(:)
       type(AsyncInputWorkerState), intent(in) :: workers(:)
       type(AsyncInputFileReadRecord), intent(in) :: active_reads(:)
       type(AsyncInputFileReadRecord), allocatable, intent(inout) :: file_owners(:)
       integer, intent(out) :: request_index, worker_rank
       integer :: i, owner_rank

       request_index = 0
       worker_rank = 0
       do i = 1, size(pending)
          if (file_read_is_active(active_reads, pending(i)%file_name)) cycle
          owner_rank = find_file_worker(file_owners, pending(i)%file_name)
           if (owner_rank > 0) then
              if (workers(owner_rank)%busy) cycle
              request_index = i
              worker_rank = owner_rank
              return
           end if
           worker_rank = select_file_worker(pending(i)%file_name, size(workers))
           if (workers(worker_rank)%busy) cycle
           call add_active_read(file_owners, pending(i)%file_name, worker_rank)
           request_index = i
           return
        end do
      end subroutine select_pending_request

      integer function select_file_worker(file_name, n_workers) result(worker_rank)
        character(len=*), intent(in) :: file_name
        integer, intent(in) :: n_workers
        integer :: hash_value, i

        hash_value = 0
        do i = 1, len_trim(file_name)
           hash_value = modulo(31 * hash_value + iachar(file_name(i:i)), n_workers)
        end do
        worker_rank = hash_value + 1
      end function select_file_worker

     integer function find_file_worker(records, file_name) result(worker_rank)
       type(AsyncInputFileReadRecord), intent(in) :: records(:)
       character(len=*), intent(in) :: file_name
       integer :: i

       worker_rank = 0
       do i = 1, size(records)
          if (records(i)%file_name == file_name) then
             worker_rank = records(i)%worker_rank
             return
          end if
       end do
     end function find_file_worker

     logical function file_read_is_active(active_reads, file_name) result(is_active)
       type(AsyncInputFileReadRecord), intent(in) :: active_reads(:)
       character(len=*), intent(in) :: file_name
       integer :: i

       is_active = .false.
       do i = 1, size(active_reads)
          if (active_reads(i)%file_name == file_name) then
             is_active = .true.
             return
          end if
       end do
     end function file_read_is_active

     subroutine add_active_read(active_reads, file_name, worker_rank)
       type(AsyncInputFileReadRecord), allocatable, intent(inout) :: active_reads(:)
       character(len=*), intent(in) :: file_name
       integer, intent(in) :: worker_rank
       type(AsyncInputFileReadRecord), allocatable :: expanded(:)
       integer :: n

       n = size(active_reads)
       allocate(expanded(n + 1))
       if (n > 0) expanded(1:n) = active_reads
       expanded(n + 1)%file_name = file_name
       expanded(n + 1)%worker_rank = worker_rank
       call move_alloc(expanded, active_reads)
     end subroutine add_active_read

     subroutine remove_active_read(active_reads, file_name, worker_rank)
       type(AsyncInputFileReadRecord), allocatable, intent(inout) :: active_reads(:)
       character(len=*), intent(in) :: file_name
       integer, intent(in) :: worker_rank
       type(AsyncInputFileReadRecord), allocatable :: remaining(:)
       integer :: i, j

       allocate(remaining(max(0, size(active_reads) - 1)))
       j = 0
       do i = 1, size(active_reads)
          if (active_reads(i)%worker_rank == worker_rank .and. active_reads(i)%file_name == file_name) cycle
          j = j + 1
          if (j <= size(remaining)) remaining(j) = active_reads(i)
       end do
       call move_alloc(remaining, active_reads)
     end subroutine remove_active_read

     subroutine remove_pending_request(pending, request_index)
       type(AsyncInputPendingRequest), allocatable, intent(inout) :: pending(:)
       integer, intent(in) :: request_index
       type(AsyncInputPendingRequest), allocatable :: remaining(:)
       integer :: n

       n = size(pending) - 1
       allocate(remaining(n))
       if (request_index > 1) remaining(1:request_index - 1) = pending(1:request_index - 1)
       if (request_index <= n) remaining(request_index:n) = pending(request_index + 1:)
       call move_alloc(remaining, pending)
     end subroutine remove_pending_request

     ! -----------------------------------------------------------------------
    ! forward_request_to_reader
    !
    ! Serialise the request (including global_start/global_count) and send to
     ! the reader. Current reads wait for the selected worker to publish the
     ! LOCAL slice. Cache-only lookahead returns as soon as the captain accepts
     ! the request, allowing the worker read to overlap model computation.
    ! -----------------------------------------------------------------------
      subroutine forward_request_to_reader(this, request, connection, deliver_to_client, command, rc)
       class(AsyncInputServer), intent(inout) :: this
       class(CollectivePrefetchDataMessage), intent(in) :: request
        class(AbstractSocket), intent(inout), target :: connection
        logical, intent(in) :: deliver_to_client
        integer, optional, intent(in) :: command
        integer, optional, intent(out) :: rc

       integer, allocatable :: buffer(:)
       integer :: buffer_size, reader_rank, worker_rank, ierr, status
       integer(INT64) :: local_msize_word
       integer, pointer :: i_ptr(:)
       type(LocalMemReference) :: mem_data_reference
       class(AbstractRequestHandle), allocatable :: handle

         ! Model commands always enter through the reader captain. The captain
         ! returns the selected worker's server-communicator rank before data.
        reader_rank = this%reader_ranks_on_node(1)
       this%forwarded_requests = this%forwarded_requests + 1
       local_msize_word = word_size(request%type_kind) * product(int(request%count, INT64))

       buffer_size = request%get_length()
       allocate(buffer(buffer_size))
       call request%serialize(buffer, _RC)
        if (present(command)) then
           call MPI_Send(command, 1, MPI_INTEGER, reader_rank, ASYNC_INPUT_TAG_CMD, this%comm, ierr)
        else
           call MPI_Send(ASYNC_INPUT_CMD_READ, 1, MPI_INTEGER, reader_rank, ASYNC_INPUT_TAG_CMD, this%comm, ierr)
        end if
       _VERIFY(ierr)
       call MPI_Send(buffer_size, 1, MPI_INTEGER, reader_rank, ASYNC_INPUT_TAG_SIZE, this%comm, ierr)
       _VERIFY(ierr)
        call MPI_Send(buffer, buffer_size, MPI_INTEGER, reader_rank, ASYNC_INPUT_TAG_BUFFER, this%comm, ierr)
        _VERIFY(ierr)
        deallocate(buffer)

        if (deliver_to_client) then
           call MPI_Recv(worker_rank, 1, MPI_INTEGER, reader_rank, ASYNC_INPUT_TAG_WORKER_RANK, &
                this%comm, MPI_STATUS_IGNORE, ierr)
           _VERIFY(ierr)
           mem_data_reference = LocalMemReference(request%type_kind, request%count)
           call c_f_pointer(mem_data_reference%base_address, i_ptr, [local_msize_word])
           call consume_shared_result(this, worker_rank, i_ptr, int(local_msize_word), _RC)

          handle = connection%put(request%request_id, mem_data_reference)
          call handle%wait()
          call mem_data_reference%deallocate(status)
          _VERIFY(status)
       end if

       _RETURN(_SUCCESS)
     end subroutine forward_request_to_reader

     subroutine publish_shared_result(this, model_rank, worker_rank, result, result_size, rc)
        class(AsyncInputServer), intent(inout) :: this
        integer, intent(in) :: model_rank, worker_rank, result(:), result_size
        integer, optional, intent(out) :: rc

        integer :: ierr, model_node_rank

        model_node_rank = find_node_rank(this, model_rank)
        _ASSERT(model_node_rank >= 0, 'model rank is not present in the node communicator')
        call publish_result_to_mailbox(this, model_node_rank, worker_rank, result, result_size, ierr)
        if (ierr /= MPI_SUCCESS) return

        _RETURN(_SUCCESS)
     end subroutine publish_shared_result

     subroutine publish_result_to_mailbox(this, model_node_rank, worker_rank, result, result_size, ierr)
        class(AsyncInputServer), intent(inout) :: this
        integer, intent(in) :: model_node_rank, worker_rank, result(:), result_size
        integer, intent(out) :: ierr

        integer(kind=MPI_ADDRESS_KIND) :: segment_bytes
        integer :: disp_unit, offset
        integer, pointer :: mailboxes(:)
        type(c_ptr) :: model_base_address
#if !defined (SUPPORT_FOR_MPI_ALLOC_MEM_CPTR)
        integer(kind=MPI_ADDRESS_KIND) :: baseaddr
#endif

        ierr = MPI_SUCCESS
#if defined(SUPPORT_FOR_MPI_ALLOC_MEM_CPTR)
        call MPI_Win_shared_query(this%shared_win, model_node_rank, segment_bytes, disp_unit, &
             model_base_address, ierr)
#else
        call MPI_Win_shared_query(this%shared_win, model_node_rank, segment_bytes, disp_unit, &
             baseaddr, ierr)
        model_base_address = transfer(baseaddr, model_base_address)
#endif
        if (ierr /= MPI_SUCCESS) return
        call c_f_pointer(model_base_address, mailboxes, &
             [(this%reader_comm_size - 1) * (ASYNC_INPUT_MAILBOX_HEADER_WORDS + this%shared_mailbox_words)])
        offset = (worker_rank - 1) * (ASYNC_INPUT_MAILBOX_HEADER_WORDS + this%shared_mailbox_words)

        do
           call MPI_Win_sync(this%shared_win, ierr)
           if (ierr /= MPI_SUCCESS) return
           if (mailboxes(offset + 1) == ASYNC_INPUT_MAILBOX_EMPTY) exit
           call MAPL_Sleep(0.0001)
        end do
        if (result_size > this%shared_mailbox_words) then
           mailboxes(offset + 2) = result_size
           mailboxes(offset + 1) = ASYNC_INPUT_MAILBOX_OVERFLOW
        else
           mailboxes(offset + 3:offset + 2 + result_size) = result
           mailboxes(offset + 2) = result_size
           call MPI_Win_sync(this%shared_win, ierr)
           if (ierr /= MPI_SUCCESS) return
           mailboxes(offset + 1) = ASYNC_INPUT_MAILBOX_READY
        end if
        call MPI_Win_sync(this%shared_win, ierr)
     end subroutine publish_result_to_mailbox

     subroutine consume_shared_result(this, worker_global_rank, result, expected_size, rc)
        class(AsyncInputServer), intent(inout) :: this
        integer, intent(in) :: worker_global_rank, expected_size
        integer, intent(out) :: result(:)
        integer, optional, intent(out) :: rc

        integer :: ierr, offset, result_size, worker_rank
        integer, pointer :: mailboxes(:)

        worker_rank = find_worker_rank(this, worker_global_rank)
        _ASSERT(worker_rank > 0, 'captain selected an unknown reader worker')
        call c_f_pointer(this%shared_base_address, mailboxes, &
             [(this%reader_capacity_on_node - 1) * (ASYNC_INPUT_MAILBOX_HEADER_WORDS + this%shared_mailbox_words)])
        offset = (worker_rank - 1) * (ASYNC_INPUT_MAILBOX_HEADER_WORDS + this%shared_mailbox_words)
        do
           call MPI_Win_sync(this%shared_win, ierr)
           _VERIFY(ierr)
           if (mailboxes(offset + 1) /= ASYNC_INPUT_MAILBOX_EMPTY) exit
           call MAPL_Sleep(0.0001)
        end do
        result_size = mailboxes(offset + 2)
        _ASSERT(mailboxes(offset + 1) /= ASYNC_INPUT_MAILBOX_OVERFLOW, &
             'AsyncInputServer shared mailbox is too small; increase MAPL_ASYNC_INPUT_SHMEM_WORDS')
        _ASSERT(result_size == expected_size, 'AsyncInputServer shared result has an unexpected size')
        result = mailboxes(offset + 3:offset + 2 + result_size)
        mailboxes(offset + 1) = ASYNC_INPUT_MAILBOX_EMPTY
        call MPI_Win_sync(this%shared_win, ierr)
        _VERIFY(ierr)

        _RETURN(_SUCCESS)
     end subroutine consume_shared_result

     integer function find_node_rank(this, global_rank) result(node_rank)
        class(AsyncInputServer), intent(in) :: this
        integer, intent(in) :: global_rank
        integer :: i

        node_rank = -1
        do i = 1, size(this%node_global_ranks)
           if (this%node_global_ranks(i) == global_rank) then
              node_rank = i - 1
              return
           end if
        end do
     end function find_node_rank

     integer function find_worker_rank(this, global_rank) result(worker_rank)
        class(AsyncInputServer), intent(in) :: this
        integer, intent(in) :: global_rank
        integer :: i

        worker_rank = 0
        do i = 2, size(this%reader_ranks_on_node)
           if (this%reader_ranks_on_node(i) == global_rank) then
              worker_rank = i - 1
              return
           end if
        end do
     end function find_worker_rank

     subroutine finish_collective_service(this, request_backlog, rc)
       class(AsyncInputServer), intent(inout) :: this
       type(MessageVector), intent(inout) :: request_backlog
       integer, optional, intent(out) :: rc

       if (request_backlog%empty()) then
          call this%clean_up()
       else
          call this%set_AllBacklogIsEmpty(.false.)
          this%serverthread_done_msgs(:) = .false.
       end if

       _RETURN(_SUCCESS)
     end subroutine finish_collective_service


    ! -----------------------------------------------------------------------
    ! Reader-side: read the full global slab from file into the cache slot.
    ! -----------------------------------------------------------------------
    subroutine read_global_slab_into_slot(this, request, slot_index, rc)
       class(AsyncInputServer), intent(inout) :: this
       class(CollectivePrefetchDataMessage), intent(in) :: request
       integer, intent(in) :: slot_index
       integer, optional, intent(out) :: rc

       type(NetCDF4_FileFormatter) :: formatter
       integer(INT32), pointer :: values_int32(:)
       integer(INT64), pointer :: values_int64(:)
       real(REAL32), pointer :: values_real32(:)
       real(REAL64), pointer :: values_real64(:)
       integer :: status
       ! Update cache key metadata.
       this%cache_slots(slot_index)%file_name    = request%file_name
       this%cache_slots(slot_index)%var_name     = request%var_name
       this%cache_slots(slot_index)%type_kind    = request%type_kind
       this%cache_slots(slot_index)%global_start = request%global_start
       this%cache_slots(slot_index)%global_count = request%global_count
       this%cache_slots(slot_index)%valid        = .false.

       ! Allocate (or re-use) the LocalMemReference for the global slab.
       if (allocated(this%cache_slots(slot_index)%reference)) then
          call this%cache_slots(slot_index)%reference%deallocate(status)
          _VERIFY(status)
          deallocate(this%cache_slots(slot_index)%reference)
       end if
       allocate(this%cache_slots(slot_index)%reference, &
             source=LocalMemReference(request%type_kind, request%global_count))

       status = _SUCCESS
       select case (request%type_kind)
       case (pFIO_INT32)
          call c_f_pointer(this%cache_slots(slot_index)%reference%base_address, values_int32, [product(request%global_count)])
          call formatter%open(request%file_name, pFIO_READ, rc=status)
          _VERIFY(status)
          call formatter%get_var(request%var_name, values_int32, &
               start=request%global_start, count=request%global_count, rc=status)
       case (pFIO_INT64)
          call c_f_pointer(this%cache_slots(slot_index)%reference%base_address, values_int64, [product(request%global_count)])
          call formatter%open(request%file_name, pFIO_READ, rc=status)
          _VERIFY(status)
          call formatter%get_var(request%var_name, values_int64, &
               start=request%global_start, count=request%global_count, rc=status)
       case (pFIO_REAL32)
          call c_f_pointer(this%cache_slots(slot_index)%reference%base_address, values_real32, [product(request%global_count)])
          call formatter%open(request%file_name, pFIO_READ, rc=status)
          _VERIFY(status)
          call formatter%get_var(request%var_name, values_real32, &
               start=request%global_start, count=request%global_count, rc=status)
       case (pFIO_REAL64)
          call c_f_pointer(this%cache_slots(slot_index)%reference%base_address, values_real64, [product(request%global_count)])
          call formatter%open(request%file_name, pFIO_READ, rc=status)
          _VERIFY(status)
          call formatter%get_var(request%var_name, values_real64, &
               start=request%global_start, count=request%global_count, rc=status)
       case default
          _FAIL('unsupported type kind for AsyncInputServer reader')
       end select
       _VERIFY(status)
       call formatter%close()
        this%cache_slots(slot_index)%valid = .true.
       _RETURN(_SUCCESS)
    end subroutine read_global_slab_into_slot

    ! -----------------------------------------------------------------------
    ! Reader-side: copy the local slice from the cached global slab into the
    ! output buffer (flat integer array of local_msize_word words).
    ! -----------------------------------------------------------------------
    subroutine extract_local_slice_from_slot(this, request, slot_index, out_buf, rc)
       class(AsyncInputServer), intent(inout) :: this
       class(CollectivePrefetchDataMessage), intent(in) :: request
       integer, intent(in) :: slot_index
       integer, intent(out) :: out_buf(:)
       integer, optional, intent(out) :: rc

       integer, pointer :: win_ptr(:)
       integer(INT64) :: global_words
       integer :: ndim

       global_words = word_size(request%type_kind) * product(int(request%global_count, INT64))
       call c_f_pointer(this%cache_slots(slot_index)%reference%base_address, win_ptr, [global_words])

       ndim = size(request%global_count)

       call copy_subarray(win_ptr, out_buf, &
            request%global_count, &
            request%start - request%global_start + 1, &
            request%count, &
            ndim, word_size(request%type_kind))

       _RETURN(_SUCCESS)
       _UNUSED_DUMMY(this)
    end subroutine extract_local_slice_from_slot

    ! -----------------------------------------------------------------------
    ! copy_subarray — copy a hyper-rectangular sub-array (Fortran column-major)
    ! from a global buffer (src) into a contiguous local buffer (dst).
    !
    ! src          : flat integer buffer, Fortran-order, shape = global_count
    ! dst          : flat integer buffer, contiguous, shape = sub_count
    ! global_count : element counts of each dimension in src
    ! sub_start    : 1-based start of the sub-array in each dimension
    ! sub_count    : element counts of each dimension to copy
    ! ndim         : number of dimensions (>= 1)
    ! wpe          : words per element = word_size(type_kind)
    ! -----------------------------------------------------------------------
    recursive subroutine copy_subarray(src, dst, global_count, sub_start, sub_count, ndim, wpe)
       integer, intent(in)  :: src(:)
       integer, intent(out) :: dst(:)
       integer, intent(in)  :: global_count(:)
       integer, intent(in)  :: sub_start(:)
       integer, intent(in)  :: sub_count(:)
       integer, intent(in)  :: ndim
       integer, intent(in)  :: wpe

       integer(INT64) :: src_stride, dst_stride, src_off, dst_off
       integer :: i

       if (ndim == 1) then
          src_off = int(sub_start(1) - 1, INT64) * wpe + 1
          dst(1 : int(sub_count(1), INT64) * wpe) = &
               src(src_off : src_off + int(sub_count(1), INT64) * wpe - 1)
          return
       end if

       src_stride = product(int(global_count(1:ndim-1), INT64)) * wpe
       dst_stride = product(int(sub_count(1:ndim-1),   INT64)) * wpe

       do i = 1, sub_count(ndim)
          src_off = int(sub_start(ndim) - 1 + i - 1, INT64) * src_stride + 1
          dst_off = int(i - 1, INT64) * dst_stride + 1
          call copy_subarray( &
               src(src_off : src_off + src_stride - 1), &
               dst(dst_off : dst_off + dst_stride - 1), &
               global_count(1:ndim-1), &
               sub_start(1:ndim-1), &
               sub_count(1:ndim-1), &
               ndim - 1, wpe)
       end do
    end subroutine copy_subarray

    ! -----------------------------------------------------------------------
    ! Cache helpers (reader-side, keyed on global extents).
    ! -----------------------------------------------------------------------
    integer function find_cache_slot(this, request) result(slot_index)
       class(AsyncInputServer), intent(in) :: this
       class(CollectivePrefetchDataMessage), intent(in) :: request

       integer :: i

       slot_index = 0
       do i = 1, size(this%cache_slots)
          if (cache_slot_matches(this%cache_slots(i), request)) then
             slot_index = i
             exit
          end if
       end do
    end function find_cache_slot

    logical function cache_slot_matches(slot, request) result(matches)
      type(AsyncInputCacheSlot), intent(in) :: slot
      class(CollectivePrefetchDataMessage), intent(in) :: request

      matches = slot%valid
      if (.not. matches) return
      matches = slot%type_kind == request%type_kind
      if (.not. matches) return
      matches = allocated(slot%file_name) .and. slot%file_name == request%file_name
      if (.not. matches) return
      matches = allocated(slot%var_name) .and. slot%var_name == request%var_name
      if (.not. matches) return
      matches = allocated(slot%global_start) .and. allocated(slot%global_count)
      if (.not. matches) return
      matches = size(slot%global_start) == size(request%global_start) .and. &
                all(slot%global_start == request%global_start)
      if (.not. matches) return
      matches = size(slot%global_count) == size(request%global_count) .and. &
                all(slot%global_count == request%global_count)
    end function cache_slot_matches

    integer function choose_cache_slot(this) result(slot_index)
      class(AsyncInputServer), intent(inout) :: this

      slot_index = this%next_cache_slot
      this%next_cache_slot = this%next_cache_slot + 1
      if (this%next_cache_slot > size(this%cache_slots)) this%next_cache_slot = 1
    end function choose_cache_slot

    subroutine finalize_runtime(this, rc)
      class(AsyncInputServer), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

       call finalize_cache_slots(this, _RC)

       if (this%shared_win /= MPI_WIN_NULL) then
          call MPI_Win_unlock_all(this%shared_win, status)
          _VERIFY(status)
          call MPI_Win_free(this%shared_win, status)
          _VERIFY(status)
          this%shared_win = MPI_WIN_NULL
          this%shared_base_address = c_null_ptr
       end if

      if (this%model_node_comm /= MPI_COMM_NULL) then
         call MPI_Comm_free(this%model_node_comm, status)
         _VERIFY(status)
         this%model_node_comm = MPI_COMM_NULL
      end if
      if (this%node_comm /= MPI_COMM_NULL) then
         call MPI_Comm_free(this%node_comm, status)
         _VERIFY(status)
         this%node_comm = MPI_COMM_NULL
      end if
       if (this%reader_comm /= MPI_COMM_NULL) then
          call MPI_Comm_free(this%reader_comm, status)
          _VERIFY(status)
          this%reader_comm = MPI_COMM_NULL
       end if
       if (allocated(this%reader_global_ranks)) deallocate(this%reader_global_ranks)
       if (allocated(this%node_global_ranks)) deallocate(this%node_global_ranks)
       this%reader_comm_size = 0
        this%next_cache_slot = 1

        _RETURN(_SUCCESS)
    end subroutine finalize_runtime

    subroutine finalize_cache_slots(this, rc)
      class(AsyncInputServer), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: i, status

      if (.not. allocated(this%cache_slots)) then
         _RETURN(_SUCCESS)
      end if

      do i = 1, size(this%cache_slots)
         if (allocated(this%cache_slots(i)%reference)) then
            call this%cache_slots(i)%reference%deallocate(status)
            _VERIFY(status)
            deallocate(this%cache_slots(i)%reference)
         end if
         if (allocated(this%cache_slots(i)%global_start)) deallocate(this%cache_slots(i)%global_start)
         if (allocated(this%cache_slots(i)%global_count)) deallocate(this%cache_slots(i)%global_count)
         if (allocated(this%cache_slots(i)%file_name)) deallocate(this%cache_slots(i)%file_name)
         if (allocated(this%cache_slots(i)%var_name)) deallocate(this%cache_slots(i)%var_name)
         this%cache_slots(i)%valid = .false.
         this%cache_slots(i)%type_kind = 0
      end do
      deallocate(this%cache_slots)

      _RETURN(_SUCCESS)
    end subroutine finalize_cache_slots

end module pFIO_AsyncInputServerMod
