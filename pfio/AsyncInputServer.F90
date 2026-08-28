#include "MAPL_ErrLog.h"

module pFIO_AsyncInputServerMod
   use, intrinsic :: iso_c_binding, only: c_f_pointer
   use, intrinsic :: iso_fortran_env, only: INT32, INT64, REAL32, REAL64
   use mapl_ErrorHandling_mod
   use mapl_Profiler_mod
   use pFIO_AbstractMessageMod
   use pFIO_ConstantsMod
   use pFIO_AbstractSocketMod
   use pFIO_AbstractRequestHandleMod
   use pFIO_UtilitiesMod, only: word_size
   use pFIO_MessageVectorMod
   use pFIO_MessageVectorUtilMod
    use pFIO_CollectivePrefetchDataMessageMod
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

    integer, parameter :: ASYNC_INPUT_CMD_READ = 1
    integer, parameter :: ASYNC_INPUT_CMD_PREPARE_CACHE = 2
    integer, parameter :: ASYNC_INPUT_CMD_TERMINATE = -1
    integer, parameter :: ASYNC_INPUT_TAG_CMD = 4701
    integer, parameter :: ASYNC_INPUT_TAG_SIZE = 4702
    integer, parameter :: ASYNC_INPUT_TAG_BUFFER = 4703
    integer, parameter :: ASYNC_INPUT_TAG_CACHE_SIZE = 4704

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
      logical :: cache_valid = .false.
      character(len=:), allocatable :: cache_file_name
      character(len=:), allocatable :: cache_var_name
      integer :: cache_type_kind = 0
      integer, allocatable :: cache_start(:)
      integer, allocatable :: cache_count(:)
      type(ShmemReference), allocatable :: cache_reference
      integer(INT64) :: cache_capacity_words = 0
      integer :: cache_hits = 0
      integer :: cache_misses = 0
      contains
       procedure :: start
        procedure :: stop_reader_pool
        procedure :: release_runtime
        procedure :: service_collective_prefetch
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

      s%port_name = trim(port_name)
      s%threads = ServerThreadVector()
      s%model_comm = MPI_COMM_NULL
      if (present(model_comm)) s%model_comm = model_comm
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

    subroutine start(this, rc)
       class(AsyncInputServer), target, intent(inout) :: this
      integer, optional, intent(out) :: rc
      class(ServerThread), pointer :: thread_ptr => null()
      integer :: i, client_size
      logical, allocatable :: mask(:)
       integer :: status, ierr, cmd, source_rank, buffer_size, msize_word
       integer(INT64) :: desired_words
      integer :: mpi_status(MPI_STATUS_SIZE)
      integer, allocatable :: buffer(:)
      type(CollectivePrefetchDataMessage) :: request
      type(LocalMemReference) :: mem_data_reference
      integer, pointer :: i_ptr(:)

       if (this%model_comm == MPI_COMM_NULL) then
          do while (.true.)
            call MPI_Recv(cmd, 1, MPI_INTEGER, MPI_ANY_SOURCE, ASYNC_INPUT_TAG_CMD, this%comm, mpi_status, ierr)
            _VERIFY(ierr)
            if (cmd == ASYNC_INPUT_CMD_TERMINATE) exit

            source_rank = mpi_status(MPI_SOURCE)
            if (cmd == ASYNC_INPUT_CMD_PREPARE_CACHE) then
               call MPI_Recv(desired_words, 1, MPI_INTEGER8, source_rank, ASYNC_INPUT_TAG_CACHE_SIZE, this%comm, mpi_status, ierr)
               _VERIFY(ierr)
               call ensure_shared_cache_capacity(this, desired_words, _RC)
               cycle
            end if
            _ASSERT(cmd == ASYNC_INPUT_CMD_READ, 'unknown async input command')

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

            mem_data_reference = LocalMemReference(request%type_kind, request%count)
            write(*,'(A,1X,A,I0,1X,A,I0,1X,A,I0,1X,A,1X,A,1X,A,1X,A)') &
                 'INFO: AsyncInputServer reader:', 'reader_rank=', this%rank, &
                 'source_rank=', source_rank, 'request_id=', request%request_id, &
                 'file=', trim(request%file_name), 'var=', trim(request%var_name)

            if (cache_matches_request(this, request)) then
               this%cache_hits = this%cache_hits + 1
               call load_cache_into_mem(this, mem_data_reference, _RC)
            else
               this%cache_misses = this%cache_misses + 1
               call read_collective_request(this, request, mem_data_reference, _RC)
               call update_cache(this, request, mem_data_reference, _RC)
            end if
            msize_word = word_size(request%type_kind)*product(int(request%count, INT64))
            call c_f_pointer(mem_data_reference%base_address, i_ptr, [msize_word])
            call MPI_Send(i_ptr, msize_word, MPI_INTEGER, source_rank, ASYNC_INPUT_TAG_BUFFER, this%comm, ierr)
            _VERIFY(ierr)
            call mem_data_reference%deallocate(status)
            _VERIFY(status)
          end do
          write(*,'(A,1X,A,I0,1X,A,I0,1X,A,I0)') 'INFO: AsyncInputServer cache:', &
               'reader_rank=', this%rank, 'hits=', this%cache_hits, 'misses=', this%cache_misses
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

       call finalize_runtime(this, _RC)
       _RETURN(_SUCCESS)
     end subroutine release_runtime

    subroutine service_collective_prefetch(this, request_backlog, connection, handled, rc)
       class(AsyncInputServer), intent(inout) :: this
       type(MessageVector), intent(inout) :: request_backlog
       class(AbstractSocket), intent(inout), target :: connection
       logical, intent(out) :: handled
       integer, optional, intent(out) :: rc

       type(MessageVectorIterator) :: iter
       class(AbstractMessage), pointer :: msg
       type(CollectivePrefetchDataMessage) :: request
       integer, allocatable :: buffer(:)
        integer :: buffer_size, reader_rank, ierr, status
        integer(INT64) :: msize_word
       integer, pointer :: i_ptr(:)
       type(LocalMemReference) :: mem_data_reference
       class(AbstractRequestHandle), allocatable :: handle

       handled = .false.
       if (this%synchronous_fallback) then
          _RETURN(_SUCCESS)
       end if
       _ASSERT(size(this%reader_ranks_on_node) > 0, 'reader ranks must exist when not in synchronous fallback')

       iter = request_backlog%begin()
       do while (iter /= request_backlog%end())
          msg => iter%get()
          select type (q => msg)
          type is (CollectivePrefetchDataMessage)
             request = q
              reader_rank = this%reader_ranks_on_node(mod(this%model_node_rank, size(this%reader_ranks_on_node)) + 1)
              msize_word = word_size(q%type_kind)*product(int(q%count, INT64))
              call prepare_shared_cache(this, msize_word, _RC)
              buffer_size = request%get_length()
              allocate(buffer(buffer_size))
              call request%serialize(buffer, _RC)
             call MPI_Send(ASYNC_INPUT_CMD_READ, 1, MPI_INTEGER, reader_rank, ASYNC_INPUT_TAG_CMD, this%comm, ierr)
             _VERIFY(ierr)
             call MPI_Send(buffer_size, 1, MPI_INTEGER, reader_rank, ASYNC_INPUT_TAG_SIZE, this%comm, ierr)
             _VERIFY(ierr)
             call MPI_Send(buffer, buffer_size, MPI_INTEGER, reader_rank, ASYNC_INPUT_TAG_BUFFER, this%comm, ierr)
              _VERIFY(ierr)
              deallocate(buffer)

              mem_data_reference = LocalMemReference(q%type_kind, q%count)
              call c_f_pointer(mem_data_reference%base_address, i_ptr, [msize_word])
              call MPI_Recv(i_ptr, msize_word, MPI_INTEGER, reader_rank, ASYNC_INPUT_TAG_BUFFER, this%comm, MPI_STATUS_IGNORE, ierr)
              _VERIFY(ierr)

             handle = connection%put(q%request_id, mem_data_reference)
             call handle%wait()
             call mem_data_reference%deallocate(status)
             _VERIFY(status)
             call request_backlog%erase(iter)
          class default
             _FAIL('AsyncInputServer only supports CollectivePrefetchDataMessage in service_collective_prefetch')
          end select
          iter = request_backlog%begin()
       end do

       call this%clean_up()
       handled = .true.
       _RETURN(_SUCCESS)
    end subroutine service_collective_prefetch

     subroutine read_collective_request(this, request, mem_data_reference, rc)
       class(AsyncInputServer), intent(inout) :: this
       type(CollectivePrefetchDataMessage), intent(in) :: request
       type(LocalMemReference), intent(inout) :: mem_data_reference
       integer, optional, intent(out) :: rc

       type(NetCDF4_FileFormatter) :: formatter
       integer(INT32), pointer :: values_int32(:)
       integer(INT64), pointer :: values_int64(:)
       real(REAL32), pointer :: values_real32(:)
       real(REAL64), pointer :: values_real64(:)
       integer :: status

       call formatter%open(request%file_name, pFIO_READ, rc=status)
       _VERIFY(status)
       select case (request%type_kind)
       case (pFIO_INT32)
          call c_f_pointer(mem_data_reference%base_address, values_int32, [product(request%count)])
          call formatter%get_var(request%var_name, values_int32, start=request%start, count=request%count, rc=status)
       case (pFIO_INT64)
          call c_f_pointer(mem_data_reference%base_address, values_int64, [product(request%count)])
          call formatter%get_var(request%var_name, values_int64, start=request%start, count=request%count, rc=status)
       case (pFIO_REAL32)
          call c_f_pointer(mem_data_reference%base_address, values_real32, [product(request%count)])
          call formatter%get_var(request%var_name, values_real32, start=request%start, count=request%count, rc=status)
       case (pFIO_REAL64)
          call c_f_pointer(mem_data_reference%base_address, values_real64, [product(request%count)])
          call formatter%get_var(request%var_name, values_real64, start=request%start, count=request%count, rc=status)
       case default
          _FAIL('unsupported type kind for AsyncInputServer reader')
       end select
       _VERIFY(status)
       call formatter%close()
        _RETURN(_SUCCESS)
        _UNUSED_DUMMY(this)
     end subroutine read_collective_request

     logical function cache_matches_request(this, request) result(matches)
       class(AsyncInputServer), intent(in) :: this
       type(CollectivePrefetchDataMessage), intent(in) :: request

       matches = this%cache_valid
       if (.not. matches) return

       matches = this%cache_type_kind == request%type_kind
       if (.not. matches) return
       matches = allocated(this%cache_file_name) .and. this%cache_file_name == request%file_name
       if (.not. matches) return
       matches = allocated(this%cache_var_name) .and. this%cache_var_name == request%var_name
       if (.not. matches) return
       matches = allocated(this%cache_start) .and. allocated(this%cache_count)
       if (.not. matches) return
       matches = size(this%cache_start) == size(request%start) .and. all(this%cache_start == request%start)
       if (.not. matches) return
       matches = size(this%cache_count) == size(request%count) .and. all(this%cache_count == request%count)
     end function cache_matches_request

     subroutine update_cache(this, request, mem_data_reference, rc)
       class(AsyncInputServer), intent(inout) :: this
       type(CollectivePrefetchDataMessage), intent(in) :: request
       type(LocalMemReference), intent(in) :: mem_data_reference
       integer, optional, intent(out) :: rc

       integer(INT64) :: msize_word
       integer, pointer :: i_ptr(:), cache_ptr(:)

        this%cache_file_name = request%file_name
        this%cache_var_name = request%var_name
        this%cache_type_kind = request%type_kind
        this%cache_start = request%start
        this%cache_count = request%count

        msize_word = word_size(request%type_kind)*product(int(request%count, INT64))
        _ASSERT(allocated(this%cache_reference), 'shared cache must be allocated before update_cache')
        _ASSERT(msize_word <= this%cache_capacity_words, 'shared cache capacity too small in update_cache')
        call c_f_pointer(mem_data_reference%base_address, i_ptr, [msize_word])
        call c_f_pointer(this%cache_reference%base_address, cache_ptr, [this%cache_capacity_words])
        cache_ptr(1:msize_word) = i_ptr
        this%cache_valid = .true.

        _RETURN(_SUCCESS)
     end subroutine update_cache

      subroutine load_cache_into_mem(this, mem_data_reference, rc)
       class(AsyncInputServer), intent(inout) :: this
        type(LocalMemReference), intent(inout) :: mem_data_reference
        integer, optional, intent(out) :: rc

        integer, pointer :: i_ptr(:), cache_ptr(:)

        _ASSERT(this%cache_valid, 'cache must be valid before load_cache_into_mem')
        _ASSERT(allocated(this%cache_reference), 'shared cache must be allocated before load_cache_into_mem')
        _ASSERT(allocated(this%cache_count), 'cache count must be allocated before load_cache_into_mem')
        call c_f_pointer(mem_data_reference%base_address, i_ptr, [product(int(this%cache_count, INT64))*word_size(this%cache_type_kind)])
        call c_f_pointer(this%cache_reference%base_address, cache_ptr, [this%cache_capacity_words])
        i_ptr = cache_ptr(1:size(i_ptr))

        _RETURN(_SUCCESS)
     end subroutine load_cache_into_mem

     subroutine prepare_shared_cache(this, desired_words, rc)
       class(AsyncInputServer), intent(inout) :: this
       integer(INT64), intent(in) :: desired_words
       integer, optional, intent(out) :: rc

       integer :: i, status

       if (desired_words <= this%cache_capacity_words) then
          _RETURN(_SUCCESS)
       end if

       _ASSERT(this%model_comm /= MPI_COMM_NULL, 'prepare_shared_cache should only be called on model/front ranks')

       do i = 1, size(this%reader_ranks_on_node)
          call MPI_Send(ASYNC_INPUT_CMD_PREPARE_CACHE, 1, MPI_INTEGER, this%reader_ranks_on_node(i), &
               ASYNC_INPUT_TAG_CMD, this%comm, status)
          _VERIFY(status)
          call MPI_Send(desired_words, 1, MPI_INTEGER8, this%reader_ranks_on_node(i), &
               ASYNC_INPUT_TAG_CACHE_SIZE, this%comm, status)
          _VERIFY(status)
       end do

       _RETURN(_SUCCESS)
     end subroutine prepare_shared_cache

     subroutine ensure_shared_cache_capacity(this, desired_words, rc)
       class(AsyncInputServer), intent(inout) :: this
       integer(INT64), intent(in) :: desired_words
       integer, optional, intent(out) :: rc

       integer :: status

       if (desired_words <= this%cache_capacity_words) then
          _RETURN(_SUCCESS)
       end if

       _ASSERT(this%reader_comm /= MPI_COMM_NULL, 'shared cache allocation should only happen on reader ranks')

       if (allocated(this%cache_reference)) then
          call this%cache_reference%deallocate(status)
          _VERIFY(status)
          deallocate(this%cache_reference)
       end if
       allocate(this%cache_reference, source=ShmemReference(pFIO_INT32, desired_words, this%reader_comm, rc=status))
       _VERIFY(status)
       this%cache_capacity_words = desired_words
       this%cache_valid = .false.

       _RETURN(_SUCCESS)
     end subroutine ensure_shared_cache_capacity

     subroutine finalize_runtime(this, rc)
       class(AsyncInputServer), intent(inout) :: this
       integer, optional, intent(out) :: rc

       integer :: status

       if (allocated(this%cache_reference)) then
          call this%cache_reference%deallocate(status)
          _VERIFY(status)
          deallocate(this%cache_reference)
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
        if (allocated(this%cache_start)) deallocate(this%cache_start)
        if (allocated(this%cache_count)) deallocate(this%cache_count)
        if (allocated(this%cache_file_name)) deallocate(this%cache_file_name)
        if (allocated(this%cache_var_name)) deallocate(this%cache_var_name)
        this%cache_valid = .false.
        this%cache_capacity_words = 0

        _RETURN(_SUCCESS)
     end subroutine finalize_runtime

end module pFIO_AsyncInputServerMod
