#include "MAPL_ErrLog.h"

module pFIO_AsyncInputServerMod
   use, intrinsic :: iso_c_binding, only: c_f_pointer
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
    use pFIO_ShmemReferenceMod
    use pFIO_NetCDF4_FileFormatterMod
   use pFIO_ServerThreadMod
   use pFIO_ServerThreadVectorMod
   use pFIO_BaseServerMod
    use mpi

   implicit none
   private

   public :: AsyncInputServer

     integer, parameter :: ASYNC_INPUT_CMD_READ       = 1
     integer, parameter :: ASYNC_INPUT_CMD_PREPARE_CACHE = 2
     integer, parameter :: ASYNC_INPUT_CMD_NEXT_PREFETCH = 3
    integer, parameter :: ASYNC_INPUT_CMD_TERMINATE  = -1
    integer, parameter :: ASYNC_INPUT_TAG_CMD        = 4701
    integer, parameter :: ASYNC_INPUT_TAG_SIZE       = 4702
    integer, parameter :: ASYNC_INPUT_TAG_BUFFER     = 4703
     integer, parameter :: ASYNC_INPUT_TAG_CACHE_SIZE = 4704
     integer, parameter :: ASYNC_INPUT_TAG_READER_CMD = 4711
     integer, parameter :: ASYNC_INPUT_TAG_READER_SIZE = 4712
     integer, parameter :: ASYNC_INPUT_TAG_READER_BUFFER = 4713
     integer, parameter :: ASYNC_INPUT_TAG_READER_DONE = 4714
     integer, parameter :: ASYNC_INPUT_TAG_READER_RESULT_SIZE = 4715
    integer, parameter :: ASYNC_INPUT_NUM_CACHE_SLOTS = 2

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
       integer :: command = ASYNC_INPUT_CMD_NEXT_PREFETCH
       integer :: source_rank = -1
       integer, allocatable :: buffer(:)
    end type AsyncInputPendingRequest


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
      type(AsyncInputCacheSlot) :: cache_slots(ASYNC_INPUT_NUM_CACHE_SLOTS)
      integer :: next_cache_slot = 1
      integer :: cache_hits = 0
      integer :: cache_misses = 0
      integer :: forwarded_requests = 0
      integer :: reader_requests = 0
      real(REAL64) :: reader_sleep_seconds = 0.0_REAL64
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

      call get_environment_variable('MAPL_PERF_READER_SLEEP_SEC', sleep_string, sleep_length, sleep_status)
      if (sleep_status == 0 .and. sleep_length > 0) then
         read(sleep_string(1:sleep_length), *, iostat=sleep_status) s%reader_sleep_seconds
         if (sleep_status /= 0) s%reader_sleep_seconds = 0.0_REAL64
      end if

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
      end if

      this%reader_capacity_on_node = this%node_npes - this%model_npes_on_node
      _ASSERT(this%reader_capacity_on_node >= 0, 'reader_capacity_on_node must be non-negative')
      this%synchronous_fallback = (this%reader_capacity_on_node == 0)

      call gather_reader_ranks(this, _RC)

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

      integer, allocatable :: node_ranks(:), model_ranks(:)
      integer :: i, j, status
      logical :: is_model_rank

      allocate(node_ranks(this%node_npes))
      call MPI_Allgather(this%rank, 1, MPI_INTEGER, node_ranks, 1, MPI_INTEGER, this%node_comm, status)
      _VERIFY(status)

      allocate(model_ranks(this%model_npes_on_node))
      if (this%model_npes_on_node > 0) then
         call MPI_Allgather(this%rank, 1, MPI_INTEGER, model_ranks, 1, MPI_INTEGER, this%model_node_comm, status)
         _VERIFY(status)
      end if

      allocate(this%reader_ranks_on_node(this%reader_capacity_on_node))
      j = 0
      do i = 1, size(node_ranks)
         is_model_rank = .false.
         if (this%model_npes_on_node > 0) then
            is_model_rank = any(model_ranks == node_ranks(i))
         end if
         if (.not. is_model_rank) then
            j = j + 1
            if (j <= size(this%reader_ranks_on_node)) this%reader_ranks_on_node(j) = node_ranks(i)
         end if
      end do

      _RETURN(_SUCCESS)
    end subroutine gather_reader_ranks

    ! -----------------------------------------------------------------------
    ! Main server loop.
    !
    ! Reader ranks (model_comm == MPI_COMM_NULL):
    !   Spin on MPI_Recv.
    !   ASYNC_INPUT_CMD_PREPARE_CACHE — no-op (reader no longer maintains shmem)
    !   ASYNC_INPUT_CMD_READ:
    !     Deserialise request.  Look up (file, var, global_start, global_count)
    !     in the 2-slot cache:
    !       Cache miss → read the full GLOBAL slab from file → store in slot.
    !       Cache hit  → data already in the slot.
    !     Extract the per-rank LOCAL slice (request%start, request%count) from
    !     the cached global slab and MPI_Send it back.
    !     If cache_only==.true., skip the MPI_Send (caller only wanted prefetch).
    !
    ! Model ranks (model_comm /= MPI_COMM_NULL):
    !   Standard ServerThread dispatch loop.
    !   Each model rank sends its full request (including global extents) to
    !   the reader and waits for its local slice.  The reader deduplicates
    !   based on the global key → only ONE file read per unique slab per node.
    ! -----------------------------------------------------------------------
    subroutine start(this, rc)
       class(AsyncInputServer), target, intent(inout) :: this
      integer, optional, intent(out) :: rc
      class(ServerThread), pointer :: thread_ptr => null()
      integer :: i, client_size
      logical, allocatable :: mask(:)
       integer :: status, ierr, cmd, source_rank, buffer_size, slot_index, msize_word
       integer :: worker_command, worker_source
       integer(INT64) :: desired_words, local_msize_word
       integer :: mpi_status(MPI_STATUS_SIZE)
       integer, allocatable :: buffer(:), result(:)
       type(AsyncInputPendingRequest), allocatable :: pending(:)
       logical :: worker_busy, message_available
      type(CollectivePrefetchDataMessage) :: request

        if (this%model_comm == MPI_COMM_NULL .and. this%synchronous_fallback) then
          ! ---- Reader rank loop ----
          do while (.true.)
             call MPI_Recv(cmd, 1, MPI_INTEGER, MPI_ANY_SOURCE, ASYNC_INPUT_TAG_CMD, this%comm, mpi_status, ierr)
            _VERIFY(ierr)
            if (cmd == ASYNC_INPUT_CMD_TERMINATE) exit

            source_rank = mpi_status(MPI_SOURCE)
            if (cmd == ASYNC_INPUT_CMD_PREPARE_CACHE) then
               ! No-op in the new design: cache is managed per-request below.
               call MPI_Recv(desired_words, 1, MPI_INTEGER8, source_rank, ASYNC_INPUT_TAG_CACHE_SIZE, this%comm, mpi_status, ierr)
               _VERIFY(ierr)
               cycle
            end if
             _ASSERT(cmd == ASYNC_INPUT_CMD_READ, 'unknown async input command')
            this%reader_requests = this%reader_requests + 1

            call MPI_Recv(buffer_size, 1, MPI_INTEGER, source_rank, ASYNC_INPUT_TAG_SIZE, this%comm, mpi_status, ierr)
            _VERIFY(ierr)
            allocate(buffer(buffer_size))
            call MPI_Recv(buffer, buffer_size, MPI_INTEGER, source_rank, ASYNC_INPUT_TAG_BUFFER, this%comm, mpi_status, ierr)
            _VERIFY(ierr)
            if (allocated(request%file_name)) deallocate(request%file_name)
            if (allocated(request%var_name)) deallocate(request%var_name)
            if (allocated(request%start)) deallocate(request%start)
            if (allocated(request%count)) deallocate(request%count)
            if (allocated(request%global_start)) deallocate(request%global_start)
            if (allocated(request%global_count)) deallocate(request%global_count)
            call request%deserialize(buffer, _RC)
            deallocate(buffer)

            ! Look up / populate the reader-side global-key cache.
             slot_index = find_cache_slot(this, request)
            if (slot_index > 0) then
               this%cache_hits = this%cache_hits + 1
            else
               this%cache_misses = this%cache_misses + 1
               slot_index = choose_cache_slot(this)
               call read_global_slab_into_slot(this, request, slot_index, _RC)
            end if

            ! Return the per-rank local slice, unless this is a cache-only request.
            if (.not. request%cache_only) then
               local_msize_word = word_size(request%type_kind) * product(int(request%count, INT64))
               msize_word = int(local_msize_word)
               allocate(buffer(msize_word))
               call extract_local_slice_from_slot(this, request, slot_index, buffer, _RC)
               call MPI_Send(buffer, msize_word, MPI_INTEGER, source_rank, ASYNC_INPUT_TAG_BUFFER, this%comm, ierr)
               _VERIFY(ierr)
               deallocate(buffer)
            end if
           end do
           write(*,'(A,1X,A,I0,1X,A,I0,1X,A,I0,1X,A,I0)') 'INFO: AsyncInputServer cache:', &
                'reader_rank=', this%rank, 'hits=', this%cache_hits, 'misses=', this%cache_misses, &
                'requests=', this%reader_requests
          call finalize_runtime(this, _RC)
           _RETURN(_SUCCESS)
       end if

       if (this%model_comm == MPI_COMM_NULL) then
          if (this%reader_comm_rank /= 0) then
             do while (.true.)
                call MPI_Recv(cmd, 1, MPI_INTEGER, 0, ASYNC_INPUT_TAG_READER_CMD, &
                     this%reader_comm, mpi_status, ierr)
                _VERIFY(ierr)
                if (cmd == ASYNC_INPUT_CMD_TERMINATE) exit
                _ASSERT(cmd == ASYNC_INPUT_CMD_READ .or. cmd == ASYNC_INPUT_CMD_NEXT_PREFETCH, &
                     'unknown worker command')
                call MPI_Recv(buffer_size, 1, MPI_INTEGER, 0, ASYNC_INPUT_TAG_READER_SIZE, &
                     this%reader_comm, mpi_status, ierr)
                _VERIFY(ierr)
                allocate(buffer(buffer_size))
                call MPI_Recv(buffer, buffer_size, MPI_INTEGER, 0, ASYNC_INPUT_TAG_READER_BUFFER, &
                     this%reader_comm, mpi_status, ierr)
                _VERIFY(ierr)
                call execute_reader_request(this, buffer, buffer_size, result, msize_word, _RC)
                deallocate(buffer)
                call MPI_Send(0, 1, MPI_INTEGER, 0, ASYNC_INPUT_TAG_READER_DONE, this%reader_comm, ierr)
                _VERIFY(ierr)
                call MPI_Send(msize_word, 1, MPI_INTEGER, 0, ASYNC_INPUT_TAG_READER_RESULT_SIZE, &
                     this%reader_comm, ierr)
                _VERIFY(ierr)
                if (msize_word > 0) then
                   call MPI_Send(result, msize_word, MPI_INTEGER, 0, ASYNC_INPUT_TAG_READER_BUFFER, &
                        this%reader_comm, ierr)
                   _VERIFY(ierr)
                   deallocate(result)
                end if
             end do
             call finalize_runtime(this, _RC)
             _RETURN(_SUCCESS)
          end if

           worker_busy = .false.
           worker_command = ASYNC_INPUT_CMD_READ
           worker_source = -1
           allocate(pending(0))
           do while (.true.)
              call poll_reader_completion(this, worker_busy, worker_command, worker_source, &
                   pending, .false., ierr)
              _VERIFY(ierr)
              if (.not. worker_busy) then
                 call dispatch_next_request(this, pending, worker_busy, worker_command, worker_source, ierr)
                 _VERIFY(ierr)
              end if
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
              if (cmd == ASYNC_INPUT_CMD_NEXT_PREFETCH) then
                 call enqueue_next_request(pending, source_rank, buffer)
                 deallocate(buffer)
                 call dispatch_next_request(this, pending, worker_busy, worker_command, worker_source, ierr)
                 _VERIFY(ierr)
                 cycle
              end if

              ! Current reads remain blocking, but cannot use a worker running next work.
              do while (worker_busy)
                 call poll_reader_completion(this, worker_busy, worker_command, worker_source, &
                      pending, .true., ierr)
                 _VERIFY(ierr)
              end do
              call send_reader_request(this, ASYNC_INPUT_CMD_READ, source_rank, buffer, ierr)
              _VERIFY(ierr)
              deallocate(buffer)
           end do

           do while (worker_busy .or. size(pending) > 0)
              call poll_reader_completion(this, worker_busy, worker_command, worker_source, &
                   pending, .true., ierr)
              _VERIFY(ierr)
              if (.not. worker_busy) then
                 call dispatch_next_request(this, pending, worker_busy, worker_command, worker_source, ierr)
                 _VERIFY(ierr)
              end if
           end do
           call MPI_Send(ASYNC_INPUT_CMD_TERMINATE, 1, MPI_INTEGER, 1, ASYNC_INPUT_TAG_READER_CMD, &
                this%reader_comm, ierr)
           _VERIFY(ierr)
           deallocate(pending)
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

       integer :: i, status

       if (.not. this%synchronous_fallback .and. this%model_comm /= MPI_COMM_NULL .and. this%model_node_rank == 0) then
          do i = 1, size(this%reader_ranks_on_node)
             call MPI_Send(ASYNC_INPUT_CMD_TERMINATE, 1, MPI_INTEGER, this%reader_ranks_on_node(i), &
                  ASYNC_INPUT_TAG_CMD, this%comm, status)
             _VERIFY(status)
          end do
       end if

        _RETURN(_SUCCESS)
      end subroutine stop_reader_pool

     subroutine release_runtime(this, rc)
        class(AsyncInputServer), intent(inout) :: this
        integer, optional, intent(out) :: rc
        integer :: status

        if (this%model_comm /= MPI_COMM_NULL .and. .not. this%synchronous_fallback .and. this%model_node_rank == 0) then
           write(*,'(A,1X,A,I0)') 'INFO: AsyncInputServer forwarded:', 'requests=', this%forwarded_requests
        end if

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

     subroutine execute_reader_request(this, input, input_size, result, result_size, rc)
       class(AsyncInputServer), intent(inout) :: this
       integer, intent(in) :: input(:), input_size
       integer, allocatable, intent(out) :: result(:)
       integer, intent(out) :: result_size
       integer, optional, intent(out) :: rc
       type(CollectivePrefetchDataMessage) :: request
        integer :: slot_index, status

       call request%deserialize(input(1:input_size), _RC)
       slot_index = find_cache_slot(this, request)
       if (slot_index > 0) then
          this%cache_hits = this%cache_hits + 1
       else
          this%cache_misses = this%cache_misses + 1
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

     subroutine enqueue_next_request(pending, source_rank, input)
       type(AsyncInputPendingRequest), allocatable, intent(inout) :: pending(:)
       integer, intent(in) :: source_rank, input(:)
       type(AsyncInputPendingRequest), allocatable :: expanded(:)
       integer :: n

       n = size(pending)
       allocate(expanded(n + 1))
       if (n > 0) expanded(1:n) = pending
       expanded(n + 1)%source_rank = source_rank
       allocate(expanded(n + 1)%buffer(size(input)))
       expanded(n + 1)%buffer = input
       call move_alloc(expanded, pending)
     end subroutine enqueue_next_request

     subroutine dispatch_next_request(this, pending, worker_busy, worker_command, worker_source, ierr)
       class(AsyncInputServer), intent(in) :: this
       type(AsyncInputPendingRequest), allocatable, intent(inout) :: pending(:)
       logical, intent(inout) :: worker_busy
       integer, intent(inout) :: worker_command, worker_source
       integer, intent(out) :: ierr
       type(AsyncInputPendingRequest), allocatable :: remaining(:)
       integer :: n

       ierr = MPI_SUCCESS
       if (worker_busy .or. size(pending) == 0) return
       call MPI_Send(ASYNC_INPUT_CMD_NEXT_PREFETCH, 1, MPI_INTEGER, 1, ASYNC_INPUT_TAG_READER_CMD, &
            this%reader_comm, ierr)
       if (ierr /= MPI_SUCCESS) return
       call MPI_Send(size(pending(1)%buffer), 1, MPI_INTEGER, 1, ASYNC_INPUT_TAG_READER_SIZE, &
            this%reader_comm, ierr)
       if (ierr /= MPI_SUCCESS) return
       call MPI_Send(pending(1)%buffer, size(pending(1)%buffer), MPI_INTEGER, 1, ASYNC_INPUT_TAG_READER_BUFFER, &
            this%reader_comm, ierr)
       if (ierr /= MPI_SUCCESS) return
       worker_busy = .true.
       worker_command = ASYNC_INPUT_CMD_NEXT_PREFETCH
       worker_source = pending(1)%source_rank
       n = size(pending) - 1
       allocate(remaining(n))
       if (n > 0) remaining = pending(2:)
       call move_alloc(remaining, pending)
     end subroutine dispatch_next_request

     subroutine send_reader_request(this, command, source_rank, input, ierr)
       class(AsyncInputServer), intent(in) :: this
       integer, intent(in) :: command, source_rank, input(:)
       integer, intent(out) :: ierr
       integer :: status, result_size
       integer, allocatable :: result(:)

       call MPI_Send(command, 1, MPI_INTEGER, 1, ASYNC_INPUT_TAG_READER_CMD, this%reader_comm, ierr)
       if (ierr /= MPI_SUCCESS) return
       call MPI_Send(size(input), 1, MPI_INTEGER, 1, ASYNC_INPUT_TAG_READER_SIZE, this%reader_comm, ierr)
       if (ierr /= MPI_SUCCESS) return
       call MPI_Send(input, size(input), MPI_INTEGER, 1, ASYNC_INPUT_TAG_READER_BUFFER, this%reader_comm, ierr)
       if (ierr /= MPI_SUCCESS) return
       call MPI_Recv(status, 1, MPI_INTEGER, 1, ASYNC_INPUT_TAG_READER_DONE, this%reader_comm, MPI_STATUS_IGNORE, ierr)
       if (ierr /= MPI_SUCCESS) return
       call MPI_Recv(result_size, 1, MPI_INTEGER, 1, ASYNC_INPUT_TAG_READER_RESULT_SIZE, &
            this%reader_comm, MPI_STATUS_IGNORE, ierr)
       if (ierr /= MPI_SUCCESS) return
       if (result_size > 0) then
          allocate(result(result_size))
          call MPI_Recv(result, result_size, MPI_INTEGER, 1, ASYNC_INPUT_TAG_READER_BUFFER, &
               this%reader_comm, MPI_STATUS_IGNORE, ierr)
          if (ierr == MPI_SUCCESS) then
             call MPI_Send(result, result_size, MPI_INTEGER, source_rank, ASYNC_INPUT_TAG_BUFFER, this%comm, ierr)
          end if
          deallocate(result)
       end if
     end subroutine send_reader_request

     subroutine poll_reader_completion(this, worker_busy, worker_command, worker_source, pending, wait_for_one, ierr)
       class(AsyncInputServer), intent(in) :: this
       logical, intent(inout) :: worker_busy
       integer, intent(inout) :: worker_command, worker_source
       type(AsyncInputPendingRequest), allocatable, intent(inout) :: pending(:)
       logical, intent(in) :: wait_for_one
       integer, intent(out) :: ierr
       logical :: available
       integer :: result_size, dummy, result_status(MPI_STATUS_SIZE)
       integer, allocatable :: result(:)

       ierr = MPI_SUCCESS
       if (.not. worker_busy) return
       do
          call MPI_Iprobe(1, ASYNC_INPUT_TAG_READER_DONE, this%reader_comm, available, result_status, ierr)
          if (ierr /= MPI_SUCCESS) return
          if (available) exit
          if (.not. wait_for_one) return
          call MAPL_Sleep(0.0001)
       end do
       call MPI_Recv(dummy, 1, MPI_INTEGER, 1, ASYNC_INPUT_TAG_READER_DONE, this%reader_comm, &
            result_status, ierr)
       if (ierr /= MPI_SUCCESS) return
       call MPI_Recv(result_size, 1, MPI_INTEGER, 1, ASYNC_INPUT_TAG_READER_RESULT_SIZE, this%reader_comm, &
            MPI_STATUS_IGNORE, ierr)
       if (ierr /= MPI_SUCCESS) return
       if (result_size > 0) then
          allocate(result(result_size))
          call MPI_Recv(result, result_size, MPI_INTEGER, 1, ASYNC_INPUT_TAG_READER_BUFFER, this%reader_comm, &
               MPI_STATUS_IGNORE, ierr)
          if (ierr == MPI_SUCCESS .and. worker_command == ASYNC_INPUT_CMD_READ) then
             call MPI_Send(result, result_size, MPI_INTEGER, worker_source, ASYNC_INPUT_TAG_BUFFER, this%comm, ierr)
          end if
          deallocate(result)
       end if
       worker_busy = .false.
       worker_source = -1
     end subroutine poll_reader_completion

     ! -----------------------------------------------------------------------
    ! forward_request_to_reader
    !
    ! Serialise the request (including global_start/global_count) and send to
    ! the reader.  If deliver_to_client is .true., wait for the reader to send
    ! back the LOCAL slice and deliver it via connection%put.
    ! -----------------------------------------------------------------------
      subroutine forward_request_to_reader(this, request, connection, deliver_to_client, command, rc)
       class(AsyncInputServer), intent(inout) :: this
       class(CollectivePrefetchDataMessage), intent(in) :: request
        class(AbstractSocket), intent(inout), target :: connection
        logical, intent(in) :: deliver_to_client
        integer, optional, intent(in) :: command
        integer, optional, intent(out) :: rc

       integer, allocatable :: buffer(:), result(:)
       integer :: buffer_size, reader_rank, ierr, status
       integer(INT64) :: local_msize_word
       integer, pointer :: i_ptr(:)
       type(LocalMemReference) :: mem_data_reference
       class(AbstractRequestHandle), allocatable :: handle

        ! Model commands always enter through the reader captain.  The captain
        ! owns worker scheduling and uses reader_comm-local rank 1 for work.
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
       if (.not. deliver_to_client) then
          call MPI_Ssend(buffer, buffer_size, MPI_INTEGER, reader_rank, ASYNC_INPUT_TAG_BUFFER, this%comm, ierr)
       else
          call MPI_Send(buffer, buffer_size, MPI_INTEGER, reader_rank, ASYNC_INPUT_TAG_BUFFER, this%comm, ierr)
       end if
       _VERIFY(ierr)
       deallocate(buffer)

       if (deliver_to_client) then
          mem_data_reference = LocalMemReference(request%type_kind, request%count)
          call c_f_pointer(mem_data_reference%base_address, i_ptr, [local_msize_word])
          call MPI_Recv(i_ptr, int(local_msize_word), MPI_INTEGER, reader_rank, ASYNC_INPUT_TAG_BUFFER, this%comm, MPI_STATUS_IGNORE, ierr)
          _VERIFY(ierr)

          handle = connection%put(request%request_id, mem_data_reference)
          call handle%wait()
          call mem_data_reference%deallocate(status)
          _VERIFY(status)
       end if

       _RETURN(_SUCCESS)
    end subroutine forward_request_to_reader

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

        call formatter%open(request%file_name, pFIO_READ, rc=status)
       _VERIFY(status)
       select case (request%type_kind)
       case (pFIO_INT32)
          call c_f_pointer(this%cache_slots(slot_index)%reference%base_address, values_int32, [product(request%global_count)])
          call formatter%get_var(request%var_name, values_int32, &
               start=request%global_start, count=request%global_count, rc=status)
       case (pFIO_INT64)
          call c_f_pointer(this%cache_slots(slot_index)%reference%base_address, values_int64, [product(request%global_count)])
          call formatter%get_var(request%var_name, values_int64, &
               start=request%global_start, count=request%global_count, rc=status)
       case (pFIO_REAL32)
          call c_f_pointer(this%cache_slots(slot_index)%reference%base_address, values_real32, [product(request%global_count)])
          call formatter%get_var(request%var_name, values_real32, &
               start=request%global_start, count=request%global_count, rc=status)
       case (pFIO_REAL64)
          call c_f_pointer(this%cache_slots(slot_index)%reference%base_address, values_real64, [product(request%global_count)])
          call formatter%get_var(request%var_name, values_real64, &
               start=request%global_start, count=request%global_count, rc=status)
       case default
          _FAIL('unsupported type kind for AsyncInputServer reader')
       end select
       _VERIFY(status)
       ! Keep the artificial delay inside the reader operation: this models a
       ! slow read and completes before the request is marked cached.
       if (this%reader_sleep_seconds > 0.0_REAL64) &
            call MAPL_Sleep(real(this%reader_sleep_seconds))
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
        this%next_cache_slot = 1

        _RETURN(_SUCCESS)
    end subroutine finalize_runtime

    subroutine finalize_cache_slots(this, rc)
      class(AsyncInputServer), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: i, status

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

      _RETURN(_SUCCESS)
    end subroutine finalize_cache_slots

end module pFIO_AsyncInputServerMod
