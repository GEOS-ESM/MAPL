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

     integer, parameter :: ASYNC_INPUT_CMD_READ = 1
     integer, parameter :: ASYNC_INPUT_CMD_NEXT_PREFETCH = 3
     integer, parameter :: ASYNC_INPUT_CMD_TERMINATE = -1
     integer, parameter :: ASYNC_INPUT_TAG_REQUEST_HEADER = 4701
     integer, parameter :: ASYNC_INPUT_TAG_REQUEST_PAYLOAD = 4702
     integer, parameter :: ASYNC_INPUT_TAG_ASSIGNMENT = 4703
     integer, parameter :: ASYNC_INPUT_TAG_TERMINATE = 4704
     integer, parameter :: ASYNC_INPUT_TAG_WORKER_HEADER = 4711
     integer, parameter :: ASYNC_INPUT_TAG_WORKER_PAYLOAD = 4712
     integer, parameter :: ASYNC_INPUT_TAG_COMPLETION = 4713
     integer, parameter :: ASYNC_INPUT_TAG_WORKER_TERMINATE = 4714
     integer, parameter :: ASYNC_INPUT_TAG_WORKER_TERMINATED = 4715
     integer, parameter :: ASYNC_INPUT_REQUEST_HEADER_WORDS = 6
     integer, parameter :: ASYNC_INPUT_ASSIGNMENT_WORDS = 4
     integer, parameter :: ASYNC_INPUT_COMPLETION_WORDS = 8
     integer, parameter :: ASYNC_INPUT_DEFAULT_CACHE_SLOTS = 2
     integer, parameter :: ASYNC_INPUT_MAILBOX_EMPTY = 0
     integer, parameter :: ASYNC_INPUT_MAILBOX_READY = 1
     integer, parameter :: ASYNC_INPUT_MAILBOX_OVERFLOW = 2
     integer, parameter :: ASYNC_INPUT_MAILBOX_HEADER_WORDS = 2
     integer, parameter :: ASYNC_INPUT_DEFAULT_MAILBOX_WORDS = 4 * 1024 * 1024

     ! Rank spaces in the internal protocol are explicit:
     ! source_service_rank and worker_service_rank are ranks in this%comm;
     ! source_node_rank is a rank in topology%node_comm;
     ! source_model_index is a rank in topology%model_node_comm; and
     ! worker_reader_rank is a rank in topology%reader_comm.
     ! protocol_request_id identifies async control traffic independently of
     ! the client/socket request_id serialized in the request payload.
     type :: AsyncInputRequestMetadata
        integer(INT64) :: protocol_request_id = -1_INT64
        integer :: command = 0
        integer :: source_service_rank = -1
        integer :: source_node_rank = -1
        integer :: source_model_index = -1
        integer :: payload_words = 0
     end type AsyncInputRequestMetadata

     type :: AsyncInputAssignment
        integer(INT64) :: protocol_request_id = -1_INT64
        integer :: worker_service_rank = -1
        integer :: worker_reader_rank = -1
        integer :: status = MPI_SUCCESS
     end type AsyncInputAssignment

     type :: AsyncInputCompletion
        integer(INT64) :: protocol_request_id = -1_INT64
        integer :: worker_reader_rank = -1
        integer :: source_service_rank = -1
        integer :: source_node_rank = -1
        integer :: source_model_index = -1
        integer :: result_words = 0
        integer :: cache_slot = 0
        integer :: status = MPI_SUCCESS
     end type AsyncInputCompletion

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
    type :: AsyncInputCacheKey
      character(len=:), allocatable :: file_name
      character(len=:), allocatable :: var_name
      integer :: type_kind = 0
      integer, allocatable :: global_start(:)
      integer, allocatable :: global_count(:)
    contains
      procedure :: matches_request => cache_key_matches_request
      procedure :: set_from_request => set_cache_key_from_request
    end type AsyncInputCacheKey

    type :: AsyncInputCacheSlot
      logical :: valid = .false.
      type(AsyncInputCacheKey) :: key
      type(LocalMemReference), allocatable :: reference   ! holds full global slab
    end type AsyncInputCacheSlot

    type :: AsyncInputPendingRequest
       type(AsyncInputRequestMetadata) :: metadata
       character(len=:), allocatable :: file_name
       integer, allocatable :: buffer(:)
    end type AsyncInputPendingRequest

    type :: AsyncInputWorkerState
       logical :: busy = .false.
       type(AsyncInputRequestMetadata) :: metadata
       character(len=:), allocatable :: file_name
       integer, allocatable :: buffer(:)
    end type AsyncInputWorkerState

    type :: AsyncInputWarmRecord
       integer :: worker_rank = -1
       integer :: slot_index = 0
       type(AsyncInputCacheKey) :: key
    end type AsyncInputWarmRecord

     type :: AsyncInputTopology
        integer :: model_node_comm = MPI_COMM_NULL
        integer :: node_comm = MPI_COMM_NULL
        integer :: reader_comm = MPI_COMM_NULL
       integer :: node_size = 0
       integer :: model_size = 0
        integer :: model_node_rank = -1
        integer :: reader_size = 0
        integer :: reader_rank = -1
        integer :: captain_service_rank = -1
        integer, allocatable :: node_server_ranks(:)
        integer, allocatable :: reader_server_ranks(:)
    contains
       procedure :: node_rank => topology_node_rank
       procedure :: worker_rank => topology_worker_rank
    end type AsyncInputTopology

    type, extends(BaseServer) :: AsyncInputServer
       private
       character(len=:), allocatable :: port_name
       logical :: model_role = .false.
       logical :: reader_role = .false.
       logical :: captain_role = .false.
       logical :: worker_role = .false.
       logical :: model_node_root_role = .false.
       type(AsyncInputTopology) :: topology
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
       integer(INT64) :: next_protocol_sequence = 1_INT64
       contains
       procedure, public :: start
        procedure, public :: shutdown
        procedure, public :: is_reader_role
        procedure, public :: is_model_role
        procedure, public :: is_captain_role
        procedure, public :: is_worker_role
        procedure, public :: get_captain_service_rank
        procedure, public :: next_protocol_request_id
        procedure, public :: service_collective_prefetch
       procedure, public :: service_next_collective_prefetch
       end type AsyncInputServer

   interface AsyncInputServer
      module procedure new_AsyncInputServer
   end interface AsyncInputServer

contains

   function new_AsyncInputServer(comm, port_name, model_comm, profiler_name, with_profiler, rc) result(s)
      type(AsyncInputServer) :: s
      integer, intent(in) :: comm
      character(*), intent(in) :: port_name
      integer, intent(in) :: model_comm
      character(*), optional, intent(in) :: profiler_name
      logical, optional, intent(in) :: with_profiler
      integer, optional, intent(out) :: rc
      integer :: status
      character(len=32) :: sleep_string
      integer :: sleep_length, sleep_status

       s%port_name = trim(port_name)
       s%threads = ServerThreadVector()

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
       call initialize_role_accounting(s, comm, model_comm, _RC)

      _RETURN(_SUCCESS)
   end function new_AsyncInputServer

    subroutine initialize_role_accounting(this, comm, model_comm, rc)
       class(AsyncInputServer), intent(inout) :: this
       integer, intent(in) :: comm, model_comm
      integer, optional, intent(out) :: rc

      integer :: ierror, status, model_flag, reader_color, reader_size
      integer, allocatable :: model_flags(:)

       status = _SUCCESS
       this%model_role = model_comm /= MPI_COMM_NULL
       this%reader_role = .not. this%model_role
       call MPI_Comm_split_type(comm, MPI_COMM_TYPE_SHARED, 0, MPI_INFO_NULL, this%topology%node_comm, ierror)
      _VERIFY(ierror)

      this%topology%model_node_rank = -1
       if (this%model_role) then
          call MPI_Comm_split_type(model_comm, MPI_COMM_TYPE_SHARED, 0, MPI_INFO_NULL, &
               this%topology%model_node_comm, ierror)
         _VERIFY(ierror)
         call MPI_Comm_rank(this%topology%model_node_comm, this%topology%model_node_rank, ierror)
         _VERIFY(ierror)
      end if

      call MPI_Comm_size(this%topology%node_comm, this%topology%node_size, ierror)
      _VERIFY(ierror)
      allocate(this%topology%node_server_ranks(this%topology%node_size))
      call MPI_Allgather(this%rank, 1, MPI_INTEGER, this%topology%node_server_ranks, 1, &
           MPI_INTEGER, this%topology%node_comm, ierror)
      _VERIFY(ierror)

       model_flag = merge(1, 0, this%model_role)
      allocate(model_flags(this%topology%node_size))
      call MPI_Allgather(model_flag, 1, MPI_INTEGER, model_flags, 1, MPI_INTEGER, &
           this%topology%node_comm, ierror)
      _VERIFY(ierror)
      this%topology%model_size = count(model_flags == 1)
      this%topology%reader_size = this%topology%node_size - this%topology%model_size
      _ASSERT(this%topology%reader_size >= 2, &
           'AsyncInputServer requires one reader captain and at least one reader worker')
       allocate(this%topology%reader_server_ranks(this%topology%reader_size))
       this%topology%reader_server_ranks = pack(this%topology%node_server_ranks, model_flags == 0)
       this%topology%captain_service_rank = this%topology%reader_server_ranks(1)
       deallocate(model_flags)

       reader_color = MPI_UNDEFINED
       if (this%reader_role) reader_color = 1
       call MPI_Comm_split(this%topology%node_comm, reader_color, this%rank, this%topology%reader_comm, ierror)
      _VERIFY(ierror)
       if (this%topology%reader_comm /= MPI_COMM_NULL) then
          call MPI_Comm_rank(this%topology%reader_comm, this%topology%reader_rank, ierror)
          _VERIFY(ierror)
          call MPI_Comm_size(this%topology%reader_comm, reader_size, ierror)
          _VERIFY(ierror)
          _ASSERT(reader_size == this%topology%reader_size, 'reader communicator size does not match node topology')
       end if
       this%captain_role = this%reader_role .and. this%topology%reader_rank == 0
       this%worker_role = this%reader_role .and. this%topology%reader_rank > 0
       this%model_node_root_role = this%model_role .and. this%topology%model_node_rank == 0

      if (this%InNode_Rank == 0) then
         write(*,'(A,1X,A,1X,A,I0,1X,A,I0,1X,A,I0)') &
              'INFO: AsyncInputServer:', trim(this%port_name), &
               'model_size_on_node=', this%topology%model_size, &
               'node_size=', this%topology%node_size, &
               'reader_capacity_on_node=', this%topology%reader_size
      end if

      call initialize_shared_mailboxes(this, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(comm)
    end subroutine initialize_role_accounting

    subroutine initialize_shared_mailboxes(this, rc)
       class(AsyncInputServer), intent(inout) :: this
       integer, optional, intent(out) :: rc

       integer(kind=MPI_ADDRESS_KIND) :: local_bytes
       integer :: ierr, n_workers
       integer, pointer :: shared_words(:)
#if !defined (SUPPORT_FOR_MPI_ALLOC_MEM_CPTR)
       integer(kind=MPI_ADDRESS_KIND) :: baseaddr
#endif

       n_workers = this%topology%reader_size - 1
       _ASSERT(n_workers > 0, 'nonfallback AsyncInputServer requires at least one reader worker')
       local_bytes = 0_MPI_ADDRESS_KIND
        if (this%model_role) then
          local_bytes = int(n_workers, MPI_ADDRESS_KIND) * &
               int(ASYNC_INPUT_MAILBOX_HEADER_WORDS + this%shared_mailbox_words, MPI_ADDRESS_KIND) * &
               4_MPI_ADDRESS_KIND
       else if (this%topology%reader_rank > 0) then
          local_bytes = int(this%num_cache_slots, MPI_ADDRESS_KIND) * &
               int(this%shared_mailbox_words, MPI_ADDRESS_KIND) * 4_MPI_ADDRESS_KIND
       end if

#if defined(SUPPORT_FOR_MPI_ALLOC_MEM_CPTR)
       call MPI_Win_allocate_shared(local_bytes, 4, MPI_INFO_NULL, this%topology%node_comm, &
            this%shared_base_address, this%shared_win, ierr)
#else
       call MPI_Win_allocate_shared(local_bytes, 4, MPI_INFO_NULL, this%topology%node_comm, &
            baseaddr, this%shared_win, ierr)
       this%shared_base_address = transfer(baseaddr, this%shared_base_address)
#endif
       _VERIFY(ierr)

       call MPI_Win_lock_all(0, this%shared_win, ierr)
       _VERIFY(ierr)

        if (this%model_role) then
          call c_f_pointer(this%shared_base_address, shared_words, &
               [n_workers * (ASYNC_INPUT_MAILBOX_HEADER_WORDS + this%shared_mailbox_words)])
          shared_words = ASYNC_INPUT_MAILBOX_EMPTY
          call MPI_Win_sync(this%shared_win, ierr)
          _VERIFY(ierr)
       else if (this%topology%reader_rank > 0) then
          this%shared_cache_base_address = this%shared_base_address
       end if
       call MPI_Barrier(this%topology%node_comm, ierr)
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
     ! Model ranks:
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
       integer :: status, ierr, cmd, source_service_rank, buffer_size, slot_index, msize_word
       integer :: mpi_status(MPI_STATUS_SIZE)
       integer, allocatable :: buffer(:), result(:)
       integer(INT64) :: header_words(ASYNC_INPUT_REQUEST_HEADER_WORDS)
       integer(INT64) :: completion_words(ASYNC_INPUT_COMPLETION_WORDS)
       type(AsyncInputRequestMetadata) :: metadata
       type(AsyncInputCompletion) :: completion
       type(AsyncInputPendingRequest), allocatable :: pending(:)
       type(AsyncInputWorkerState), allocatable :: workers(:)
       type(AsyncInputWarmRecord), allocatable :: warm_records(:)
       logical :: message_available

       if (this%reader_role) then
           if (this%worker_role) then
              do while (.true.)
                  call MPI_Probe(0, MPI_ANY_TAG, this%topology%reader_comm, mpi_status, ierr)
                  _VERIFY(ierr)
                  if (mpi_status(MPI_TAG) == ASYNC_INPUT_TAG_WORKER_TERMINATE) then
                     call MPI_Recv(cmd, 1, MPI_INTEGER, 0, ASYNC_INPUT_TAG_WORKER_TERMINATE, &
                          this%topology%reader_comm, MPI_STATUS_IGNORE, ierr)
                     _VERIFY(ierr)
                     call MPI_Send(cmd, 1, MPI_INTEGER, 0, ASYNC_INPUT_TAG_WORKER_TERMINATED, &
                          this%topology%reader_comm, ierr)
                     _VERIFY(ierr)
                     exit
                  end if
                  _ASSERT(mpi_status(MPI_TAG) == ASYNC_INPUT_TAG_WORKER_HEADER, 'unknown worker protocol tag')
                  call MPI_Recv(header_words, ASYNC_INPUT_REQUEST_HEADER_WORDS, MPI_INTEGER8, 0, &
                       ASYNC_INPUT_TAG_WORKER_HEADER, this%topology%reader_comm, MPI_STATUS_IGNORE, ierr)
                  _VERIFY(ierr)
                  call unpack_request_metadata(header_words, metadata)
                  cmd = metadata%command
                  source_service_rank = metadata%source_service_rank
                  buffer_size = metadata%payload_words
                  _ASSERT(cmd == ASYNC_INPUT_CMD_READ .or. cmd == ASYNC_INPUT_CMD_NEXT_PREFETCH, &
                      'unknown worker command')
                  allocate(buffer(buffer_size))
                 call MPI_Recv(buffer, buffer_size, MPI_INTEGER, 0, ASYNC_INPUT_TAG_WORKER_PAYLOAD, &
                      this%topology%reader_comm, MPI_STATUS_IGNORE, ierr)
                 _VERIFY(ierr)
                 call execute_reader_request(this, buffer, buffer_size, result, msize_word, slot_index, _RC)
                 deallocate(buffer)
                 call publish_shared_cache_slot(this, slot_index, _RC)
                 if (msize_word > 0) then
                     call publish_shared_result(this, source_service_rank, this%topology%reader_rank, &
                         result, msize_word, _RC)
                    deallocate(result)
                 end if
                  completion%protocol_request_id = metadata%protocol_request_id
                  completion%worker_reader_rank = this%topology%reader_rank
                  completion%source_service_rank = metadata%source_service_rank
                  completion%source_node_rank = metadata%source_node_rank
                  completion%source_model_index = metadata%source_model_index
                  completion%result_words = msize_word
                  completion%cache_slot = slot_index
                  completion%status = MPI_SUCCESS
                  call pack_completion(completion, completion_words)
                  call MPI_Send(completion_words, ASYNC_INPUT_COMPLETION_WORDS, MPI_INTEGER8, 0, &
                       ASYNC_INPUT_TAG_COMPLETION, this%topology%reader_comm, ierr)
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

            _ASSERT(this%topology%reader_size > 1, &
                 'nonfallback AsyncInputServer requires one reader captain and at least one worker')
             allocate(workers(this%topology%reader_size - 1))
             allocate(pending(0))
             allocate(warm_records(0))
             do while (.true.)
                call poll_reader_completions(this, workers, warm_records, .false., ierr)
                _VERIFY(ierr)
                call serve_warm_requests(this, pending, workers, warm_records, ierr)
                _VERIFY(ierr)
                call dispatch_pending_requests(this, pending, workers, ierr)
               _VERIFY(ierr)
               call MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, this%comm, message_available, mpi_status, ierr)
              _VERIFY(ierr)
              if (.not. message_available) then
                 call MAPL_Sleep(0.0001)
                 cycle
              end if
               source_service_rank = mpi_status(MPI_SOURCE)
               if (mpi_status(MPI_TAG) == ASYNC_INPUT_TAG_TERMINATE) then
                  call MPI_Recv(cmd, 1, MPI_INTEGER, source_service_rank, ASYNC_INPUT_TAG_TERMINATE, &
                       this%comm, MPI_STATUS_IGNORE, ierr)
                  _VERIFY(ierr)
                  exit
               end if
               _ASSERT(mpi_status(MPI_TAG) == ASYNC_INPUT_TAG_REQUEST_HEADER, 'unknown captain protocol tag')
               call MPI_Recv(header_words, ASYNC_INPUT_REQUEST_HEADER_WORDS, MPI_INTEGER8, &
                    source_service_rank, ASYNC_INPUT_TAG_REQUEST_HEADER, this%comm, MPI_STATUS_IGNORE, ierr)
               _VERIFY(ierr)
               call unpack_request_metadata(header_words, metadata)
               _ASSERT(metadata%source_service_rank == source_service_rank, &
                    'request source does not match its service rank')
               _ASSERT(this%topology%node_rank(source_service_rank) == metadata%source_node_rank, &
                    'request source does not match its node rank')
               _ASSERT(metadata%source_model_index >= 0 .and. &
                    metadata%source_model_index < this%topology%model_size, &
                    'request source has an invalid node-local model index')
               cmd = metadata%command
               buffer_size = metadata%payload_words
               allocate(buffer(buffer_size))
               call MPI_Recv(buffer, buffer_size, MPI_INTEGER, source_service_rank, &
                    ASYNC_INPUT_TAG_REQUEST_PAYLOAD, this%comm, MPI_STATUS_IGNORE, ierr)
               _VERIFY(ierr)
               _ASSERT(cmd == ASYNC_INPUT_CMD_READ .or. cmd == ASYNC_INPUT_CMD_NEXT_PREFETCH, &
                    'unknown reader captain command')
                 call enqueue_reader_request(pending, metadata, buffer, _RC)
                 deallocate(buffer)
                 call send_assignment(this, metadata, &
                      select_file_worker(pending(size(pending))%file_name, size(workers)), ierr)
                 _VERIFY(ierr)
                 call serve_warm_requests(this, pending, workers, warm_records, ierr)
                _VERIFY(ierr)
                call dispatch_pending_requests(this, pending, workers, ierr)
               _VERIFY(ierr)
            end do

             do while (any(workers%busy) .or. size(pending) > 0)
               call poll_reader_completions(this, workers, warm_records, .true., ierr)
               _VERIFY(ierr)
               call serve_warm_requests(this, pending, workers, warm_records, ierr)
               _VERIFY(ierr)
               call dispatch_pending_requests(this, pending, workers, ierr)
                _VERIFY(ierr)
             end do
             write(*,'(A,1X,A,I0,1X,A,I0)') 'INFO: AsyncInputServer captain cache:', &
                  'warm_hits=', this%captain_warm_hits, 'prefetch_hits=', this%captain_prefetch_hits
             do i = 1, size(workers)
                 call MPI_Send(ASYNC_INPUT_CMD_TERMINATE, 1, MPI_INTEGER, i, &
                      ASYNC_INPUT_TAG_WORKER_TERMINATE, this%topology%reader_comm, ierr)
                _VERIFY(ierr)
             end do
             do i = 1, size(workers)
                 call MPI_Recv(cmd, 1, MPI_INTEGER, i, ASYNC_INPUT_TAG_WORKER_TERMINATED, &
                     this%topology%reader_comm, MPI_STATUS_IGNORE, ierr)
                _VERIFY(ierr)
                _ASSERT(cmd == ASYNC_INPUT_CMD_TERMINATE, 'reader worker returned an invalid shutdown acknowledgment')
             end do
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

       if (this%model_node_root_role) then
          write(*,'(A,1X,A,I0)') 'INFO: AsyncInputServer forwarded:', 'requests=', this%forwarded_requests
       end if

       call this%report_profile(_RC)
       call this%shutdown(_RC)

       _RETURN(_SUCCESS)
     end subroutine start

     subroutine shutdown(this, rc)
       class(AsyncInputServer), intent(inout) :: this
       integer, optional, intent(out) :: rc

        integer :: status

        if (this%topology%node_comm == MPI_COMM_NULL) then
           _RETURN(_SUCCESS)
        end if

        if (this%model_node_root_role) then
            call MPI_Send(ASYNC_INPUT_CMD_TERMINATE, 1, MPI_INTEGER, &
                 this%topology%captain_service_rank, &
                ASYNC_INPUT_TAG_TERMINATE, this%comm, status)
           _VERIFY(status)
        end if

        call finalize_runtime(this, _RC)
        _RETURN(_SUCCESS)
      end subroutine shutdown

     logical function is_reader_role(this)
        class(AsyncInputServer), intent(in) :: this
        is_reader_role = this%reader_role
     end function is_reader_role

     logical function is_model_role(this)
        class(AsyncInputServer), intent(in) :: this
        is_model_role = this%model_role
     end function is_model_role

     logical function is_captain_role(this)
        class(AsyncInputServer), intent(in) :: this
        is_captain_role = this%captain_role
     end function is_captain_role

     logical function is_worker_role(this)
        class(AsyncInputServer), intent(in) :: this
        is_worker_role = this%worker_role
     end function is_worker_role

     integer function get_captain_service_rank(this)
        class(AsyncInputServer), intent(in) :: this
        get_captain_service_rank = this%topology%captain_service_rank
     end function get_captain_service_rank

     integer(INT64) function next_protocol_request_id(this) result(request_id)
        class(AsyncInputServer), intent(inout) :: this

        ! Service rank makes independently generated per-rank sequences unique.
        request_id = this%next_protocol_sequence * int(this%npes, INT64) + int(this%rank, INT64)
        this%next_protocol_sequence = this%next_protocol_sequence + 1_INT64
     end function next_protocol_request_id

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
        _ASSERT(this%topology%reader_size >= 2, &
             'AsyncInputServer requires one reader captain and at least one reader worker')

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
              _ASSERT(this%topology%reader_size >= 2, &
                   'AsyncInputServer requires one reader captain and at least one reader worker')
              call forward_request_to_reader(this, q, connection, .false., ASYNC_INPUT_CMD_NEXT_PREFETCH, _RC)
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

        cache_words = word_size(this%cache_slots(slot_index)%key%type_kind) * &
             product(int(this%cache_slots(slot_index)%key%global_count, INT64))
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

     subroutine enqueue_reader_request(pending, metadata, input, rc)
       type(AsyncInputPendingRequest), allocatable, intent(inout) :: pending(:)
       type(AsyncInputRequestMetadata), intent(in) :: metadata
       integer, intent(in) :: input(:)
       integer, optional, intent(out) :: rc
       type(AsyncInputPendingRequest), allocatable :: expanded(:)
       type(CollectivePrefetchDataMessage) :: request
       integer :: n, status

       call request%deserialize(input, _RC)
       _ASSERT(metadata%payload_words == size(input), 'request payload size does not match its header')
       _ASSERT(metadata%protocol_request_id >= 0_INT64, 'request has an invalid protocol request ID')
       _ASSERT((metadata%command == ASYNC_INPUT_CMD_READ .and. .not. request%cache_only) .or. &
            (metadata%command == ASYNC_INPUT_CMD_NEXT_PREFETCH .and. request%cache_only), &
            'request command does not match cache-only metadata')
       n = size(pending)
       allocate(expanded(n + 1))
       if (n > 0) expanded(1:n) = pending
       expanded(n + 1)%metadata = metadata
       expanded(n + 1)%file_name = request%file_name
       allocate(expanded(n + 1)%buffer(size(input)))
       expanded(n + 1)%buffer = input
       call move_alloc(expanded, pending)
       _RETURN(_SUCCESS)
     end subroutine enqueue_reader_request

     subroutine dispatch_pending_requests(this, pending, workers, ierr)
       class(AsyncInputServer), intent(in) :: this
       type(AsyncInputPendingRequest), allocatable, intent(inout) :: pending(:)
       type(AsyncInputWorkerState), intent(inout) :: workers(:)
       integer, intent(out) :: ierr
       integer :: request_index, worker_rank
       integer(INT64) :: header_words(ASYNC_INPUT_REQUEST_HEADER_WORDS)

       ierr = MPI_SUCCESS
       do
          call select_pending_request(pending, workers, request_index, worker_rank)
          if (request_index < 1) return

          call pack_request_metadata(pending(request_index)%metadata, header_words)
          call MPI_Send(header_words, ASYNC_INPUT_REQUEST_HEADER_WORDS, MPI_INTEGER8, worker_rank, &
               ASYNC_INPUT_TAG_WORKER_HEADER, this%topology%reader_comm, ierr)
          if (ierr /= MPI_SUCCESS) return
          call MPI_Send(pending(request_index)%buffer, size(pending(request_index)%buffer), &
               MPI_INTEGER, worker_rank, ASYNC_INPUT_TAG_WORKER_PAYLOAD, this%topology%reader_comm, ierr)
          if (ierr /= MPI_SUCCESS) return

          workers(worker_rank)%busy = .true.
          workers(worker_rank)%metadata = pending(request_index)%metadata
          workers(worker_rank)%file_name = pending(request_index)%file_name
          allocate(workers(worker_rank)%buffer(size(pending(request_index)%buffer)))
          workers(worker_rank)%buffer = pending(request_index)%buffer
          call remove_pending_request(pending, request_index)
       end do
     end subroutine dispatch_pending_requests

     subroutine poll_reader_completions(this, workers, warm_records, wait_for_one, ierr)
       class(AsyncInputServer), intent(in) :: this
       type(AsyncInputWorkerState), intent(inout) :: workers(:)
       type(AsyncInputWarmRecord), allocatable, intent(inout) :: warm_records(:)
       logical, intent(in) :: wait_for_one
       integer, intent(out) :: ierr
       logical :: available
       integer :: worker_rank, result_status(MPI_STATUS_SIZE)
       integer(INT64) :: completion_words(ASYNC_INPUT_COMPLETION_WORDS)
       type(AsyncInputCompletion) :: completion
       type(CollectivePrefetchDataMessage) :: request

       ierr = MPI_SUCCESS
       if (.not. any(workers%busy)) return
       do
          call MPI_Iprobe(MPI_ANY_SOURCE, ASYNC_INPUT_TAG_COMPLETION, this%topology%reader_comm, &
               available, result_status, ierr)
          if (ierr /= MPI_SUCCESS) return
          if (available) exit
          if (.not. wait_for_one) return
          call MAPL_Sleep(0.0001)
       end do
       worker_rank = result_status(MPI_SOURCE)
       call MPI_Recv(completion_words, ASYNC_INPUT_COMPLETION_WORDS, MPI_INTEGER8, worker_rank, &
             ASYNC_INPUT_TAG_COMPLETION, this%topology%reader_comm, result_status, ierr)
       if (ierr /= MPI_SUCCESS) return
       call unpack_completion(completion_words, completion)
       if (.not. workers(worker_rank)%busy .or. completion%worker_reader_rank /= worker_rank .or. &
            completion%protocol_request_id /= workers(worker_rank)%metadata%protocol_request_id .or. &
            completion%source_service_rank /= workers(worker_rank)%metadata%source_service_rank .or. &
            completion%source_node_rank /= workers(worker_rank)%metadata%source_node_rank .or. &
            completion%source_model_index /= workers(worker_rank)%metadata%source_model_index .or. &
            completion%status /= MPI_SUCCESS) then
          ierr = MPI_ERR_OTHER
          return
       end if
       call request%deserialize(workers(worker_rank)%buffer, ierr)
       if (ierr /= MPI_SUCCESS) return
       call update_warm_record(warm_records, request, worker_rank, completion%cache_slot)
       workers(worker_rank)%busy = .false.
       workers(worker_rank)%metadata = AsyncInputRequestMetadata()
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

            if (pending(i)%metadata%command == ASYNC_INPUT_CMD_READ) then
               call publish_warm_result(this, request, pending(i)%metadata%source_service_rank, warm_records(warm_index), ierr)
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

     subroutine publish_warm_result(this, request, model_service_rank, warm_record, ierr)
        class(AsyncInputServer), intent(inout) :: this
        type(CollectivePrefetchDataMessage), intent(in) :: request
        integer, intent(in) :: model_service_rank
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
        model_node_rank = this%topology%node_rank(model_service_rank)
        if (model_node_rank < 0) then
           ierr = MPI_ERR_RANK
           return
        end if
#if defined(SUPPORT_FOR_MPI_ALLOC_MEM_CPTR)
        call MPI_Win_shared_query(this%shared_win, this%topology%node_rank( &
             this%topology%reader_server_ranks(warm_record%worker_rank + 1)), segment_bytes, disp_unit, &
             cache_base_address, ierr)
#else
        call MPI_Win_shared_query(this%shared_win, this%topology%node_rank( &
             this%topology%reader_server_ranks(warm_record%worker_rank + 1)), &
             segment_bytes, disp_unit, baseaddr, ierr)
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

        matches = record%key%matches_request(request)
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
              call warm_records(i)%key%set_from_request(request)
              return
           end if
        end do
        n = size(warm_records)
        allocate(updated(n + 1))
        if (n > 0) updated(1:n) = warm_records
        updated(n + 1)%worker_rank = worker_rank
        updated(n + 1)%slot_index = slot_index
        call updated(n + 1)%key%set_from_request(request)
        call move_alloc(updated, warm_records)
     end subroutine update_warm_record

      subroutine select_pending_request(pending, workers, request_index, worker_rank)
       type(AsyncInputPendingRequest), intent(in) :: pending(:)
       type(AsyncInputWorkerState), intent(in) :: workers(:)
       integer, intent(out) :: request_index, worker_rank
       integer :: i

       request_index = 0
        worker_rank = 0
        do i = 1, size(pending)
           worker_rank = select_file_worker(pending(i)%file_name, size(workers))
           if (workers(worker_rank)%busy) cycle
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
       integer :: buffer_size, reader_rank, worker_rank, ierr, status, request_command
       integer(INT64) :: local_msize_word
       integer(INT64) :: header_words(ASYNC_INPUT_REQUEST_HEADER_WORDS)
       integer(INT64) :: assignment_words(ASYNC_INPUT_ASSIGNMENT_WORDS)
       type(AsyncInputRequestMetadata) :: metadata
       type(AsyncInputAssignment) :: assignment
       integer, pointer :: i_ptr(:)
       type(LocalMemReference) :: mem_data_reference
       class(AbstractRequestHandle), allocatable :: handle

         ! Model commands always enter through the reader captain. The captain
         ! returns the selected worker's server-communicator rank before data.
         reader_rank = this%topology%captain_service_rank
        this%forwarded_requests = this%forwarded_requests + 1
        local_msize_word = word_size(request%type_kind) * product(int(request%count, INT64))

       buffer_size = request%get_length()
        allocate(buffer(buffer_size))
        call request%serialize(buffer, _RC)
        request_command = ASYNC_INPUT_CMD_READ
        if (present(command)) request_command = command
        metadata%protocol_request_id = this%next_protocol_request_id()
        metadata%command = request_command
        metadata%source_service_rank = this%rank
        metadata%source_node_rank = this%InNode_Rank
        metadata%source_model_index = this%topology%model_node_rank
        metadata%payload_words = buffer_size
        call pack_request_metadata(metadata, header_words)
        call MPI_Send(header_words, ASYNC_INPUT_REQUEST_HEADER_WORDS, MPI_INTEGER8, reader_rank, &
             ASYNC_INPUT_TAG_REQUEST_HEADER, this%comm, ierr)
        _VERIFY(ierr)
         call MPI_Send(buffer, buffer_size, MPI_INTEGER, reader_rank, ASYNC_INPUT_TAG_REQUEST_PAYLOAD, this%comm, ierr)
         _VERIFY(ierr)
         deallocate(buffer)

         call MPI_Recv(assignment_words, ASYNC_INPUT_ASSIGNMENT_WORDS, MPI_INTEGER8, reader_rank, &
              ASYNC_INPUT_TAG_ASSIGNMENT, this%comm, MPI_STATUS_IGNORE, ierr)
         _VERIFY(ierr)
         call unpack_assignment(assignment_words, assignment)
         _ASSERT(assignment%protocol_request_id == metadata%protocol_request_id, &
              'captain assignment does not match the submitted request')
         _ASSERT(assignment%status == MPI_SUCCESS, 'captain could not assign the input request')
         _ASSERT(assignment%worker_reader_rank > 0 .and. &
              assignment%worker_reader_rank < this%topology%reader_size, &
              'captain returned an invalid reader worker rank')
         _ASSERT(assignment%worker_service_rank == &
              this%topology%reader_server_ranks(assignment%worker_reader_rank + 1), &
              'captain returned inconsistent worker rank spaces')
         worker_rank = assignment%worker_service_rank

         if (deliver_to_client) then
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

     subroutine send_assignment(this, metadata, worker_reader_rank, ierr)
        class(AsyncInputServer), intent(in) :: this
        type(AsyncInputRequestMetadata), intent(in) :: metadata
        integer, intent(in) :: worker_reader_rank
        integer, intent(out) :: ierr

        integer(INT64) :: words(ASYNC_INPUT_ASSIGNMENT_WORDS)
        type(AsyncInputAssignment) :: assignment

        assignment%protocol_request_id = metadata%protocol_request_id
        assignment%worker_service_rank = this%topology%reader_server_ranks(worker_reader_rank + 1)
        assignment%worker_reader_rank = worker_reader_rank
        assignment%status = MPI_SUCCESS
        call pack_assignment(assignment, words)
        call MPI_Send(words, ASYNC_INPUT_ASSIGNMENT_WORDS, MPI_INTEGER8, metadata%source_service_rank, &
             ASYNC_INPUT_TAG_ASSIGNMENT, this%comm, ierr)
     end subroutine send_assignment

     subroutine pack_request_metadata(metadata, words)
        type(AsyncInputRequestMetadata), intent(in) :: metadata
        integer(INT64), intent(out) :: words(ASYNC_INPUT_REQUEST_HEADER_WORDS)

        words = [metadata%protocol_request_id, int(metadata%command, INT64), &
             int(metadata%source_service_rank, INT64), int(metadata%source_node_rank, INT64), &
             int(metadata%source_model_index, INT64), int(metadata%payload_words, INT64)]
     end subroutine pack_request_metadata

     subroutine unpack_request_metadata(words, metadata)
        integer(INT64), intent(in) :: words(ASYNC_INPUT_REQUEST_HEADER_WORDS)
        type(AsyncInputRequestMetadata), intent(out) :: metadata

        metadata%protocol_request_id = words(1)
        metadata%command = int(words(2))
        metadata%source_service_rank = int(words(3))
        metadata%source_node_rank = int(words(4))
        metadata%source_model_index = int(words(5))
        metadata%payload_words = int(words(6))
     end subroutine unpack_request_metadata

     subroutine pack_assignment(assignment, words)
        type(AsyncInputAssignment), intent(in) :: assignment
        integer(INT64), intent(out) :: words(ASYNC_INPUT_ASSIGNMENT_WORDS)

        words = [assignment%protocol_request_id, int(assignment%worker_service_rank, INT64), &
             int(assignment%worker_reader_rank, INT64), int(assignment%status, INT64)]
     end subroutine pack_assignment

     subroutine unpack_assignment(words, assignment)
        integer(INT64), intent(in) :: words(ASYNC_INPUT_ASSIGNMENT_WORDS)
        type(AsyncInputAssignment), intent(out) :: assignment

        assignment%protocol_request_id = words(1)
        assignment%worker_service_rank = int(words(2))
        assignment%worker_reader_rank = int(words(3))
        assignment%status = int(words(4))
     end subroutine unpack_assignment

     subroutine pack_completion(completion, words)
        type(AsyncInputCompletion), intent(in) :: completion
        integer(INT64), intent(out) :: words(ASYNC_INPUT_COMPLETION_WORDS)

        words = [completion%protocol_request_id, int(completion%worker_reader_rank, INT64), &
             int(completion%source_service_rank, INT64), int(completion%source_node_rank, INT64), &
             int(completion%source_model_index, INT64), int(completion%result_words, INT64), &
             int(completion%cache_slot, INT64), int(completion%status, INT64)]
     end subroutine pack_completion

     subroutine unpack_completion(words, completion)
        integer(INT64), intent(in) :: words(ASYNC_INPUT_COMPLETION_WORDS)
        type(AsyncInputCompletion), intent(out) :: completion

        completion%protocol_request_id = words(1)
        completion%worker_reader_rank = int(words(2))
        completion%source_service_rank = int(words(3))
        completion%source_node_rank = int(words(4))
        completion%source_model_index = int(words(5))
        completion%result_words = int(words(6))
        completion%cache_slot = int(words(7))
        completion%status = int(words(8))
     end subroutine unpack_completion

     subroutine publish_shared_result(this, model_service_rank, worker_rank, result, result_size, rc)
        class(AsyncInputServer), intent(inout) :: this
        integer, intent(in) :: model_service_rank, worker_rank, result(:), result_size
        integer, optional, intent(out) :: rc

        integer :: ierr, model_node_rank

        model_node_rank = this%topology%node_rank(model_service_rank)
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
             [(this%topology%reader_size - 1) * &
             (ASYNC_INPUT_MAILBOX_HEADER_WORDS + this%shared_mailbox_words)])
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

        worker_rank = this%topology%worker_rank(worker_global_rank)
        _ASSERT(worker_rank > 0, 'captain selected an unknown reader worker')
        call c_f_pointer(this%shared_base_address, mailboxes, &
             [(this%topology%reader_size - 1) * &
             (ASYNC_INPUT_MAILBOX_HEADER_WORDS + this%shared_mailbox_words)])
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

     integer function topology_node_rank(this, server_rank) result(node_rank)
        class(AsyncInputTopology), intent(in) :: this
        integer, intent(in) :: server_rank
        integer :: i

        node_rank = -1
        do i = 1, size(this%node_server_ranks)
           if (this%node_server_ranks(i) == server_rank) then
              node_rank = i - 1
              return
           end if
        end do
     end function topology_node_rank

     integer function topology_worker_rank(this, server_rank) result(worker_rank)
        class(AsyncInputTopology), intent(in) :: this
        integer, intent(in) :: server_rank
        integer :: i

        worker_rank = 0
        do i = 2, size(this%reader_server_ranks)
           if (this%reader_server_ranks(i) == server_rank) then
              worker_rank = i - 1
              return
           end if
        end do
     end function topology_worker_rank

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
       call this%cache_slots(slot_index)%key%set_from_request(request)
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
      matches = slot%key%matches_request(request)
    end function cache_slot_matches

    logical function cache_key_matches_request(this, request) result(matches)
      class(AsyncInputCacheKey), intent(in) :: this
      class(CollectivePrefetchDataMessage), intent(in) :: request

      matches = this%type_kind == request%type_kind
      if (.not. matches) return
      matches = allocated(this%file_name) .and. this%file_name == request%file_name
      if (.not. matches) return
      matches = allocated(this%var_name) .and. this%var_name == request%var_name
      if (.not. matches) return
      matches = allocated(this%global_start) .and. allocated(this%global_count)
      if (.not. matches) return
      matches = size(this%global_start) == size(request%global_start) .and. &
           all(this%global_start == request%global_start)
      if (.not. matches) return
      matches = size(this%global_count) == size(request%global_count) .and. &
           all(this%global_count == request%global_count)
    end function cache_key_matches_request

    subroutine set_cache_key_from_request(this, request)
      class(AsyncInputCacheKey), intent(inout) :: this
      class(CollectivePrefetchDataMessage), intent(in) :: request

      this%file_name = request%file_name
      this%var_name = request%var_name
      this%type_kind = request%type_kind
      this%global_start = request%global_start
      this%global_count = request%global_count
    end subroutine set_cache_key_from_request

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

      if (this%topology%model_node_comm /= MPI_COMM_NULL) then
         call MPI_Comm_free(this%topology%model_node_comm, status)
         _VERIFY(status)
         this%topology%model_node_comm = MPI_COMM_NULL
      end if
      if (this%topology%node_comm /= MPI_COMM_NULL) then
         call MPI_Comm_free(this%topology%node_comm, status)
         _VERIFY(status)
         this%topology%node_comm = MPI_COMM_NULL
      end if
       if (this%topology%reader_comm /= MPI_COMM_NULL) then
          call MPI_Comm_free(this%topology%reader_comm, status)
          _VERIFY(status)
          this%topology%reader_comm = MPI_COMM_NULL
       end if
       if (allocated(this%topology%reader_server_ranks)) deallocate(this%topology%reader_server_ranks)
       if (allocated(this%topology%node_server_ranks)) deallocate(this%topology%node_server_ranks)
       this%topology%reader_size = 0
       this%topology%node_size = 0
       this%topology%model_size = 0
        this%topology%reader_rank = -1
        this%topology%model_node_rank = -1
        this%topology%captain_service_rank = -1
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
         if (allocated(this%cache_slots(i)%key%global_start)) deallocate(this%cache_slots(i)%key%global_start)
         if (allocated(this%cache_slots(i)%key%global_count)) deallocate(this%cache_slots(i)%key%global_count)
         if (allocated(this%cache_slots(i)%key%file_name)) deallocate(this%cache_slots(i)%key%file_name)
         if (allocated(this%cache_slots(i)%key%var_name)) deallocate(this%cache_slots(i)%key%var_name)
         this%cache_slots(i)%valid = .false.
         this%cache_slots(i)%key%type_kind = 0
      end do
      deallocate(this%cache_slots)

      _RETURN(_SUCCESS)
    end subroutine finalize_cache_slots

end module pFIO_AsyncInputServerMod
