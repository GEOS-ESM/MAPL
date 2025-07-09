#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_MultiGroupServerMod
   use, intrinsic :: iso_c_binding, only: c_ptr
   use, intrinsic :: iso_c_binding, only: c_loc
   use, intrinsic :: iso_fortran_env, only: REAL64, INT32, INT64
   use, intrinsic :: iso_c_binding, only: c_f_pointer
   use mapl_KeywordEnforcerMod
   use MAPL_Profiler
   use MAPL_ErrorHandlingMod
   use pFIO_ConstantsMod
   use pFIO_CollectiveStageDataMessageMod
   use pFIO_UtilitiesMod
   use pFIO_AbstractDirectoryServiceMod
   use pFIO_ServerThreadMod
   use pFIO_ServerThreadVectorMod
   use pFIO_AbstractSocketMod
   use pFIO_AbstractSocketVectorMod
   use pFIO_AbstractServerMod
   use gFTL_StringInteger64Map
   use pFIO_AbstractMessageMod
   use pFIO_AbstractCollectiveDataMessageMod
   use pFIO_AbstractDataMessageMod
   use pFIO_AbstractDataReferenceMod
   use pFIO_AbstractDataReferenceVectorMod
   use pFIO_ShmemReferenceMod
   use pFIO_RDMAReferenceMod
   use pFIO_LocalMemReferenceMod
   use pFIO_MessageVectorMod
   use pFIO_MessageVectorUtilMod
   use pFIO_ForwardDataAndMessageMod
   use pFIO_NetCDF4_FileFormatterMod
   use pFIO_HistoryCollectionMod
   use pFIO_HistoryCollectionVectorMod
   use pFIO_HistoryCollectionVectorUtilMod
   use pFIO_BaseServerMod
   use pFIO_IntArrayMod
   use pFIO_StringIntArrayMapMod
   use MAPL_SplitCommunicatorMod
   use MAPL_SimpleCommSplitterMod
   use pFIO_MpiSocketMod
   use gFTL_IntegerVector
   use pFIO_AbstractRequestHandleMod
   use pFIO_FileMetadataMod
   use pFIO_IntegerMessageMapMod
   use gFTL2_StringSet
   use mpi
   use pFlogger, only: logging, Logger

   implicit none
   private

   public :: MultiGroupServer

   type :: vector_array
      integer :: request
      integer, allocatable :: buffer(:)
   end type

   type,extends (BaseServer) :: MultiGroupServer
      character(len=:), allocatable :: port_name
      integer :: front_Comm
      integer :: back_Comm
      integer :: server_Comm
      integer :: nwriter ! nback or number of back end
      integer :: nfront  ! number of front end
      logical :: I_am_front_root
      logical :: I_am_back_root
      integer, allocatable :: back_ranks(:)
      integer, allocatable :: front_ranks(:)
      class(vector_array), allocatable :: buffers(:)
      type(SimpleCommSplitter) :: splitter
   contains
      procedure :: start
      procedure :: start_back
      procedure :: get_communicator
      procedure :: receive_output_data
      procedure :: create_remote_win
      procedure :: put_dataToFile
      procedure :: clean_up
      procedure :: terminate_backend_server
   end type MultiGroupServer

   interface MultiGroupServer
      module procedure new_MultiGroupServer
   end interface MultiGroupServer

   integer, parameter :: FNAME_LEN = 512

contains

   function new_MultiGroupServer(server_comm, port_name, nwriter_per_node, with_profiler, rc) result(s)
      type (MultiGroupServer) :: s
      integer, intent(in) :: server_comm
      character(*), intent(in) :: port_name
      integer, intent (in) :: nwriter_per_node
      logical, optional, intent(in) :: with_profiler
      integer, optional, intent(out) :: rc

      integer :: s_rank, s_size
      integer :: ierror, status, local_rank
      type (SplitCommunicator)   ::  s_comm
      character(len=:), allocatable :: s_name
      integer :: MPI_STAT(MPI_STATUS_SIZE)
      type (MpiSocket), target :: dummy_socket
      integer :: nwriter
      integer, allocatable :: node_sizes(:)

      s%server_comm = server_comm
      call MPI_Comm_size(s%server_comm, s_size , ierror)
      _verify(ierror)
      s%splitter = SimpleCommsplitter(s%server_comm)
      node_sizes = s%splitter%get_node_sizes()

      ! if oserver size is smaller than one-node, than it means oserver and model coexist in one node
      if (s_size < node_sizes(1)) then
         call s%splitter%add_group(npes = s_size - nwriter_per_node, name="o_server_front", isolate_nodes=.false.)
         call s%splitter%add_group(npes = nwriter_per_node,          name="o_server_back",  isolate_nodes=.false.)
      else
         call s%splitter%add_group(npes_per_node = node_sizes(1)-nwriter_per_node, name="o_server_front", isolate_nodes=.false.)
         call s%splitter%add_group(npes_per_node = nwriter_per_node,               name="o_server_back",  isolate_nodes=.false.)
      endif

      s_comm = s%splitter%split(_rc)

      nwriter = nwriter_per_node*size(node_sizes)
      s%front_comm = MPI_COMM_NULL
      s%back_comm  = MPI_COMM_NULL
      s%nwriter    = nwriter
      s%nfront     = s_size - nwriter
      s%threads = ServerThreadVector()
      allocate(s%back_ranks(nwriter))
      allocate(s%front_ranks(s%nfront))
      call MPI_Comm_rank(s%server_comm, s_rank, ierror)
      _verify(ierror)
      s_name = s_comm%get_name()
      s%I_am_front_root = .false.
      s%I_am_back_root = .false.
      if (index(s_name, 'o_server_front') /=0) then
         s%front_comm = s_comm%get_subcommunicator()
         call s%init(s%front_comm, s_name, with_profiler = with_profiler, _rc)
         s%port_name = trim(port_name)
         call MPI_Comm_rank(s%front_comm, local_rank, ierror)
         _verify(ierror)
         if (s_rank == 0) then
           _assert( local_rank == 0, "re-arrange the rank of the server_comm")
           s%I_am_front_root = .true.
           call MPI_recv(s%back_ranks, nwriter, MPI_INTEGER, MPI_ANY_SOURCE, 666, s%server_comm, MPI_STAT,ierror)
           _verify(ierror)
         endif
         call MPI_Bcast(s%back_ranks, nwriter, MPI_INTEGER, 0, s%front_comm, ierror)
         _verify(ierror)

         call MPI_AllGather(s_rank, 1, MPI_INTEGER, s%front_ranks, 1, MPI_INTEGER, s%front_comm, ierror)
         _verify(ierror)
         if (local_rank ==0 ) then
            call MPI_Send(s%front_ranks, s_size-nwriter, MPI_INTEGER, s%back_ranks(1), 777, s%server_comm, ierror)
            _verify(ierror)
         endif
         allocate(s%buffers(s%nwriter))
      endif

      if (index(s_name, 'o_server_back') /=0) then
         s%back_comm = s_comm%get_subcommunicator()
         call MPI_AllGather(s_rank, 1, MPI_INTEGER, s%back_ranks, 1, MPI_INTEGER, s%back_comm, ierror)
         _verify(ierror)
         call MPI_Comm_rank(s%back_comm, local_rank, ierror)
         _verify(ierror)
         if (local_rank ==0 ) then
            s%I_am_back_root = .true.
            call MPI_Send(s%back_ranks, nwriter, MPI_INTEGER, 0, 666, s%server_comm, ierror)
            _verify(ierror)
         endif

         if (s_rank == s%back_ranks(1)) then
            _assert( local_rank == 0, "re-arrange the rank of the server_comm")
            call MPI_recv(s%front_ranks, s%nfront, MPI_INTEGER, MPI_ANY_SOURCE, 777, s%server_comm, MPI_STAT,ierror)
            _verify(ierror)
         endif

         call MPI_Bcast(s%front_ranks, s%nfront, MPI_INTEGER, 0, s%back_comm, ierror)
         _verify(ierror)
         call s%set_status(1)
         call s%add_connection(dummy_socket)
         allocate(s%buffers(s%nfront))
         call s%init(s%back_comm, s_name, _rc)
      endif

      if (s_rank == 0) print*, "MultiServer Start: nfront, nwriter", s%nfront, s%nwriter-1
      _return(_success)
   end function new_MultiGroupServer

   subroutine start(this, rc)
      class (MultiGroupServer), target, intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: status

      if ( this%front_comm /= MPI_COMM_NULL) then
         call start_front(_rc)
      endif

      if ( this%back_comm /= MPI_COMM_NULL) then
         call this%start_back(_rc)
      endif
      call this%splitter%free_sub_comm()
      _return(_success)
   contains

      subroutine start_front(rc)
         integer, optional, intent(out) :: rc

         class (ServerThread), pointer :: thread_ptr => null()
         integer :: i,client_size, status
         logical, allocatable :: mask(:)

         client_size = this%threads%size()

         allocate(this%serverthread_done_msgs(client_size))
         this%serverthread_done_msgs(:) = .false.

         allocate(mask(client_size))
         mask = .false.
         ! loop untill terminate
         do while (.true.)

            do i = 1,client_size

               if ( mask(i)) cycle

               thread_ptr=>this%threads%at(i)
               !handle the message
               call thread_ptr%run(_rc)
               !delete the thread object if it terminates
               if(thread_ptr%do_terminate()) then
                  mask(i) = .true.
               endif
            enddo

            if (all(mask)) exit

         enddo

         call this%threads%clear()
         call this%terminate_backend_server(_rc)

         call this%report_profile(_rc)

         deallocate(mask)
         _return(_success)
      end subroutine start_front

   end subroutine start

   subroutine create_remote_win(this, rc)
      class (MultiGroupServer), target, intent(inout) :: this
      integer, optional, intent(out) :: rc
      _return(_success)
      _unused_dummy(this)
   end subroutine create_remote_win

   subroutine put_DataToFile(this, rc)
     class (MultiGroupServer),target, intent(inout) :: this
     integer, optional, intent(out) :: rc
     if (this%front_Comm == MPI_COMM_NULL) then
        _fail("hey backend does not call this")
     else
        _return(_success)
     endif
   end subroutine put_DataToFile

   subroutine clean_up(this, rc)
      class(MultiGroupServer), target, intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: num_clients, n
      integer :: status
      class (ServerThread),pointer :: thread_ptr

      if (this%front_Comm == MPI_COMM_NULL) then
         _return(_success)
      endif

      if (associated(ioserver_profiler)) call ioserver_profiler%start("clean up")

      num_clients = this%threads%size()

      do n = 1, num_clients
         thread_ptr=>this%threads%at(n)
         call thread_ptr%clear_backlog()
         call thread_ptr%clear_hist_collections()
      enddo ! threads

      call this%clear_RequestHandle(_rc)
      call this%set_AllBacklogIsEmpty(.true.)
      this%serverthread_done_msgs(:) = .false.

      call this%prefetch_offset%clear()
      call this%stage_offset%clear()

      if (associated(ioserver_profiler)) call ioserver_profiler%stop("clean up")
      _return(_success)
   end subroutine clean_up

   integer function get_communicator(this) result(communicator)
      class (MultiGroupServer), intent(in) :: this

      communicator = this%front_comm

   end function get_communicator

  subroutine receive_output_data(this, rc)
     class (MultiGroupServer),target, intent(inout) :: this
     integer, optional, intent(out) :: rc

     integer :: i, client_num, status
     class (ServerThread),pointer :: thread_ptr

     type (MessageVectorIterator) :: iter
     type (StringInteger64MapIterator) :: request_iter
     class (AbstractMessage), pointer :: msg
     integer :: collection_counter, collection_total, collection_id, ierror
     integer :: MPI_STAT(MPI_STATUS_SIZE)
     integer :: msg_size, words, local_size, back_local_rank
     integer, allocatable :: buffer(:)
     type (IntegerVector) :: collection_ids
     type (ForwardDataAndMessage), allocatable :: f_d_ms(:)
     type (HistoryCollection), pointer :: hist_collection
     integer, pointer :: i_ptr(:)
     class (AbstractRequestHandle), pointer :: handle
     character(len=FNAME_LEN) :: FileName

     if (associated(ioserver_profiler)) call ioserver_profiler%start("receive_data")
     client_num = this%threads%size()
     this%stage_offset = StringInteger64map()

     !(1) loop to get the total number of collection
     collection_counter = 0
     collection_total   = 0

     thread_ptr=> this%threads%at(1)
     iter   = thread_ptr%request_backlog%begin()
     do while (iter /= thread_ptr%request_backlog%end())
        msg => iter%get()
        select type (q=>msg)
        type is (CollectiveStageDataMessage)
           request_iter = this%stage_offset%find(i_to_string(q%collection_id))
           if ( request_iter == this%stage_offset%end()) then
              collection_total = collection_total + 1
              call this%stage_offset%insert(i_to_string(q%collection_id),int(collection_total, kind=INT64))
              call collection_ids%push_back(q%collection_id)
           endif
        end select
        call iter%next()
     end do

     ! pack the data and message for each collection id
     allocate(f_d_ms(collection_total))
     do i = 1, client_num
        thread_ptr=>this%threads%at(i)
        iter   = thread_ptr%request_backlog%begin()
        do while (iter /= thread_ptr%request_backlog%end())
           msg => iter%get()
           select type (q=>msg)
           class is (AbstractCollectiveDataMessage)
              if (associated(ioserver_profiler)) call ioserver_profiler%start("collection_"//i_to_string(q%collection_id))
              handle => thread_ptr%get_RequestHandle(q%request_id)
              call handle%wait()
              words = word_size(q%type_kind)
              local_size = product(q%count)*words
              if (local_size > 0 .or. this%I_am_front_root) then
                 collection_counter = this%stage_offset%at(i_to_string(q%collection_id))
                 call c_f_pointer(handle%data_reference%base_address, i_ptr, shape=[local_size])
                 call f_d_ms(collection_counter)%add_data_message(q, i_ptr)
              endif
              if (associated(ioserver_profiler)) call ioserver_profiler%stop("collection_"//i_to_string(q%collection_id))
           class default
              _fail( "yet to implemented")
           end select
           call iter%next()
        end do ! iter
     enddo ! client_num

     if (associated(ioserver_profiler)) call ioserver_profiler%stop("receive_data")
     if (associated(ioserver_profiler)) call ioserver_profiler%start("forward_data")
     ! serializes and send data_and_message to writer
     do collection_counter = 1, collection_total
        ! root asks for idle writer and sends axtra file metadata
        if (this%I_am_front_root) then
           collection_id = collection_ids%at(collection_counter)
           call Mpi_Send(collection_id, 1, MPI_INTEGER, this%back_ranks(1), this%back_ranks(1), this%server_comm, ierror)
           _verify(ierror)
           msg =>f_d_ms(collection_counter)%msg_vec%at(1) ! just pick first one. All messages should have the same filename
           select type (q=>msg)
           class is (AbstractCollectiveDataMessage)
              Filename = q%file_name
              call Mpi_Send(FileName, FNAME_LEN, MPI_CHARACTER, this%back_ranks(1), this%back_ranks(1), this%server_comm, ierror)
              _verify(ierror)
           class default
              _fail( "yet to implemented")
           end select

           ! here thread_ptr can point to any thread
           hist_collection => thread_ptr%hist_collections%at(collection_id)
           call hist_collection%fmd%serialize(buffer)
        endif

        call Mpi_Bcast( collection_id, 1, MPI_INTEGER, 0, this%front_comm, ierror)
        _verify(ierror)
        if (associated(ioserver_profiler)) call ioserver_profiler%start("collection_"//i_to_string(collection_id))

        if (this%I_am_front_root) then

           call Mpi_Recv(back_local_rank, 1, MPI_INTEGER, this%back_ranks(1), &
                this%front_ranks(1), this%server_comm, MPI_STAT, ierror)
           _verify(ierror)

           msg_size= size(buffer)
           call Mpi_send(msg_size,1, MPI_INTEGER, this%back_ranks(back_local_rank+1), &
               this%back_ranks(back_local_rank+1), this%server_comm, ierror)
           _verify(ierror)
           call Mpi_send(buffer,msg_size, MPI_INTEGER, this%back_ranks(back_local_rank+1), &
               this%back_ranks(back_local_rank+1), this%server_comm, ierror)
           _verify(ierror)
        endif

        call Mpi_Bcast( back_local_rank, 1, MPI_INTEGER, 0, this%front_comm, ierror)
        _verify(ierror)
        if (allocated(this%buffers(back_local_rank+1)%buffer)) then
           call MPI_Wait(this%buffers(back_local_rank+1)%request, MPI_STAT, ierror)
           _verify(ierror)
        endif
        call f_d_ms(collection_counter)%serialize(this%buffers(back_local_rank+1)%buffer)
        call f_d_ms(collection_counter)%destroy(_rc)
        msg_size= size(this%buffers(back_local_rank+1)%buffer)
        call Mpi_send(msg_size,1, MPI_INTEGER, this%back_ranks(back_local_rank+1), &
             this%back_ranks(back_local_rank+1), this%server_comm, ierror)
        _verify(ierror)
        call Mpi_Isend(this%buffers(back_local_rank+1)%buffer, msg_size, MPI_INTEGER, this%back_ranks(back_local_rank+1), &
            this%back_ranks(back_local_rank+1), this%server_comm, this%buffers(back_local_rank+1)%request,ierror)
        if (associated(ioserver_profiler)) call ioserver_profiler%stop("collection_"//i_to_string(collection_id))
     enddo
     if (associated(ioserver_profiler)) call ioserver_profiler%stop("forward_data")
     deallocate(f_d_ms)
     _return(_success)
   end subroutine receive_output_data

   ! the logic is similar to pfio_writer
   subroutine start_back(this, rc)
     class (MultiGroupServer),target, intent(inout) :: this
     integer, optional, intent(out) :: rc
     integer, parameter :: stag = 6782

     integer :: status
     type (StringSet), target :: FilesBeingWritten

     allocate(this%serverthread_done_msgs(1))
     this%serverthread_done_msgs(:) = .false.

     if ( this%I_am_back_root ) then
        call start_back_captain(_rc)
     else
        call start_back_writers(_rc)
     endif

     _return(_success)

   contains

     subroutine start_back_captain(rc)
       integer, optional, intent(out) :: rc
       integer :: collection_id
       integer :: nwriter_per_node
       integer, allocatable :: idleRank(:,:)   ! idle processors
       integer, allocatable :: num_idlePEs(:) ! how many idle processors in each node of backend server
       integer :: local_rank, node_rank, nth_writer
       integer :: terminate, ierr
       integer :: MPI_STAT(MPI_STATUS_SIZE)
       character(len=FNAME_LEN)         :: FileName

       nwriter_per_node = this%nwriter/this%Node_Num
       allocate(num_idlePEs(0:this%Node_Num-1))
       allocate(idleRank(0:this%Node_Num-1, 0:nwriter_per_node-1), source=-1)

        ! all local ranks are idle at the beginning
       do local_rank = 1, this%nwriter-1
          node_rank = this%node_ranks(local_rank)
          nth_writer = mod(local_rank, nwriter_per_node)
          idleRank(node_rank, nth_writer) = local_rank
       enddo

       num_idlePEs = count(idleRank /=-1, dim = 2)

       do while (.true.)
         ! 1) captain node of back_comm is waiting command from front_comm
         call MPI_recv( collection_id, 1, MPI_INTEGER, &
              this%front_ranks(1), this%back_ranks(1), this%server_comm, &
              MPI_STAT, ierr)
         _verify(ierr)
         if (collection_id == -1) exit

         call MPI_recv( FileName, FNAME_LEN , MPI_CHARACTER, &
              this%front_ranks(1), this%back_ranks(1), this%server_comm, &
              MPI_STAT, ierr)
         _verify(ierr)
         ! 2) get an idle processor and notify front root
         call dispatch_work(collection_id, idleRank, num_idlePEs, FileName, rc=status)
         _verify(status)
       enddo ! while .true.

       ! terminate writers ( or soldiers) first
       call terminate_back_writers(idleRank, rc=status)
       _verify(status)
       ! at the end , send done message to root of oserver
       ! this serves the syncronization with oserver
       terminate = -1
       call MPI_send(terminate, 1, MPI_INTEGER, 0, 0, this%server_comm, ierr)
       _verify(ierr)
       deallocate(num_idlePEs, idleRank)
       _return(_success)
     end subroutine start_back_captain

     subroutine dispatch_work(collection_id, idleRank, num_idlePEs, FileName, rc)
       integer, intent(in) :: collection_id
       integer, intent(inout) :: idleRank(0:,0:)
       integer, intent(inout) :: num_idlePEs(0:)
       character(*), intent(in) :: FileName
       integer, optional, intent(out) :: rc

       integer :: MPI_STAT(MPI_STATUS_SIZE)
       integer :: local_rank, idle_writer, nth_writer, node_rank
       integer :: i, ierr, nwriter_per_node
       logical :: flag
       character(len=FNAME_LEN) :: FileDone
       type (StringSetIterator) :: iter
       logical :: found

       ! 2.1)  try to retrieve idle writers
       !       keep looping (waiting) until there are idle processors
       nwriter_per_node = size(idleRank,2)
       found = .false.
       do while (.not. found)
          ! non block probe writers
          do local_rank = 1, this%nwriter-1
             flag = .false.
             call MPI_Iprobe( local_rank, stag, this%back_comm, flag, MPI_STAT, ierr)
             _verify(ierr)
             if (flag) then
               call MPI_recv(idle_writer, 1, MPI_INTEGER, &
                             local_rank, stag, this%back_comm, &
                             MPI_STAT, ierr)
               _verify(ierr)
               _assert(local_rank == idle_writer, "local_rank and idle_writer should match")
               node_rank = this%node_ranks(local_rank)
               num_idlePEs(node_rank) = num_idlePEs(node_rank) + 1
               nth_writer = mod(local_rank, nwriter_per_node)
               idleRank(node_rank, nth_writer) = local_rank

               call MPI_recv(FileDone, FNAME_LEN, MPI_CHARACTER, &
                             local_rank, stag+1, this%back_comm, &
                             MPI_STAT, ierr)
               _verify(ierr)

               iter = FilesBeingWritten%find(FileDone)
               _assert( iter /= FilesBeingWritten%end(), "FileDone should be in the set")
               iter = FilesBeingWritten%erase(iter)
             endif
          enddo
          ! if there is no idle processor, get back to probe
          if (all(num_idlePEs == 0)) cycle
          ! if this file is still being written, get back to probe
          iter = FilesBeingWritten%find(FileName)
          if (iter /= FilesBeingWritten%end()) cycle

          ! get the node with the most idle processors
          node_rank = maxloc(num_idlePEs, dim=1) - 1
          do i = 0, nwriter_per_node -1
            if (idleRank(node_rank,i) /= -1) then
               idle_writer = idleRank(node_rank,i)
               idleRank(node_rank,i) = -1 ! set to -1 when it becomes busy
               num_idlePEs(node_rank) = num_idlePEs(node_rank)-1
               exit
            end if
          enddo
          _assert(1<= idle_writer .and. idle_writer <= this%nwriter-1, "wrong local rank of writer")
          call FilesBeingWritten%insert(FileName)
          found = .true. ! exit the loop after get one idle processor and the file is done
       enddo ! while,  get one idle writer

       ! 2.2) tell front comm which idel_worker is ready
       call MPI_send(idle_writer, 1, MPI_INTEGER, this%front_ranks(1), &
                     this%front_ranks(1), this%server_comm, ierr)
       _verify(ierr)

       ! 2.3) forward the collection_id to the idle_writer
       call MPI_send(collection_id, 1, MPI_INTEGER, idle_writer, idle_writer,this%back_comm, ierr)
       _verify(ierr)
       _return(_success)
     end subroutine dispatch_work

     subroutine terminate_back_writers(idleRank, rc)
       integer, intent(in) :: idleRank(0:,0:)
       integer, optional, intent(out) :: rc
       integer :: MPI_STAT(MPI_STATUS_SIZE)
       integer :: node_rank, local_rank, nth_writer
       integer :: ierr, no_job, nwriter_per_node, idle_writer
       character(len=FNAME_LEN) :: FileDone

       no_job = -1
       nwriter_per_node = size(idleRank, 2)
       do local_rank = 1, this%nwriter -1
          node_rank = this%node_ranks(local_rank)
          nth_writer = mod(local_rank, nwriter_per_node)

          if (idleRank(node_rank, nth_writer) >=1) then
             ! send no_job directly to terminate
             call MPI_send(no_job, 1, MPI_INTEGER, local_rank, local_rank, this%back_comm, ierr)
             _verify(ierr)
          else
             ! For busy worker, wait to receive idle_writer and the send no_job
             call MPI_recv( idle_writer, 1, MPI_INTEGER, &
                            local_rank, stag, this%back_comm, &
                            MPI_STAT, ierr)
             _verify(ierr)
             call MPI_recv( FileDone, FNAME_LEN, MPI_CHARACTER, &
                            local_rank, stag+1, this%back_comm, &
                            MPI_STAT, ierr)
             _verify(ierr)
             _assert(local_rank == idle_writer, "local_rank and idle_writer should match")
             call MPI_send(no_job, 1, MPI_INTEGER, local_rank, local_rank, this%back_comm, ierr)
             _verify(ierr)
          endif
       enddo
       _return(_success)
     end subroutine terminate_back_writers

     subroutine start_back_writers(rc)
       integer, optional, intent(out) :: rc
       integer :: collection_id
       integer :: i, j
       integer :: msg_size, back_local_rank, status

       integer :: MPI_STAT(MPI_STATUS_SIZE), ierr
       class (AbstractMessage), pointer :: msg
       class(ServerThread), pointer :: thread_ptr
       integer, allocatable :: buffer_fmd(:)

       integer, pointer :: g_1d(:), l_1d(:), g_2d(:,:), l_2d(:,:), g_3d(:,:,:), l_3d(:,:,:)
       integer, pointer :: g_4d(:,:,:,:), l_4d(:,:,:,:), g_5d(:,:,:,:,:), l_5d(:,:,:,:,:)
       integer :: d_rank, request_id
       integer(kind=INT64) :: msize_word, s0, e0, s1, e1, s2, e2, s3, e3, s4, e4, s5, e5
       type (StringIntArrayMap), target :: vars_map
       type (StringIntArrayMapIterator) :: var_iter
       type (IntegerMessageMap), target  :: msg_map
       type (IntegerMessageMapIterator)  :: msg_iter

       integer, pointer :: x_ptr(:)
       integer , allocatable :: buffer_v(:)
       type (IntArray), pointer :: array_ptr
       type (IntArray) :: array_tmp
       type (c_ptr) :: address
       type (ForwardDataAndMessage), target :: f_d_m
       type (FileMetaData) :: fmd
       type(AdvancedMeter) :: file_timer
       real(kind=REAL64) :: time
       character(len=:), allocatable :: filename
       character(len=FNAME_LEN) :: FileDone
       real(kind=REAL64) :: file_size, speed

       class(Logger), pointer :: lgr
       back_local_rank = this%rank
       thread_ptr => this%threads%at(1)
       file_timer = AdvancedMeter(MpiTimerGauge())
       do while (.true.)

         ! 1) get collection id from captain
         call MPI_recv( collection_id, 1, MPI_INTEGER, &
               0, back_local_rank, this%back_comm, &
               MPI_STAT, ierr)
         _verify(ierr)
         if (collection_id == -1 ) exit ! exit when get terminate signal
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! sync with create_remote_win from front_com
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         do i = 1, this%nfront
            ! receive extra metadata from front root
            if (i == 1) then
               call MPI_recv( msg_size, 1, MPI_INTEGER,    &
                    this%front_ranks(i), this%back_ranks(back_local_rank+1), this%server_comm, &
                    MPI_STAT, ierr)
              _verify(ierr)
               allocate(buffer_fmd(msg_size))
               call MPI_recv( buffer_fmd(1), msg_size, MPI_INTEGER, &
                   this%front_ranks(i), this%back_ranks(back_local_rank+1), this%server_comm, &
                   MPI_STAT, ierr)
              _verify(ierr)
            endif

            call MPI_recv( msg_size, 1, MPI_INTEGER,    &
                   this%front_ranks(i), this%back_ranks(back_local_rank+1), this%server_comm, &
                   MPI_STAT, ierr)
              _verify(ierr)
            if (allocated(this%buffers(i)%buffer)) deallocate (this%buffers(i)%buffer)
              allocate(this%buffers(i)%buffer(msg_size))
              call MPI_Irecv( this%buffers(i)%buffer(1), msg_size, MPI_INTEGER, &
                   this%front_ranks(i), this%back_ranks(back_local_rank+1), this%server_comm, this%buffers(i)%request, &
                   ierr)
              _verify(ierr)
         enddo ! nfront

         ! re-org data
         vars_map = StringIntArrayMap()
         msg_map  = IntegerMessageMap()
         file_size = 0.

         do i = 1, this%nfront
            s0 = 1
            f_d_m = ForwardDataAndMessage()
            call MPI_Wait(this%buffers(i)%request, MPI_STAT, ierr)
            _verify(ierr)
            call f_d_m%deserialize(this%buffers(i)%buffer)
            deallocate(this%buffers(i)%buffer)
            if (size(f_d_m%idata) ==0) cycle
            file_size = file_size + size(f_d_m%idata)

            do j = 1, f_d_m%msg_vec%size()
               msg => f_d_m%msg_vec%at(j)
               select type (q=>msg)
               type is (CollectiveStageDataMessage)
                  msize_word = word_size(q%type_kind)*product(int(q%global_count, INT64))
                  var_iter = vars_map%find(i_to_string(q%request_id))
                  if (var_iter == vars_map%end()) then
                     msize_word = word_size(q%type_kind)*product(int(q%global_count, INT64))
                     array_tmp = IntArray(msize_word)
                     call vars_map%insert(i_to_string(q%request_id),array_tmp)
                     var_iter = vars_map%find(i_to_string(q%request_id))
                     call msg_map%insert(q%request_id, q)
                  endif
                  array_ptr => var_iter%value()
                  x_ptr => array_ptr%get_values()
                  address = c_loc(x_ptr(1))
                  d_rank = size(q%global_count)
                  ! first dimension increases
                  q%global_count(1) = word_size(q%type_kind)*q%global_count(1)
                  q%count(1) = word_size(q%type_kind)*q%count(1)
                  q%start(1) = word_size(q%type_kind)*(q%start(1)-1)+1
                  select case (d_rank)
                  case (0)
                     _fail( "scalar ?? ")
                  case (1)
                     call c_f_pointer(address, g_1d, shape=q%global_count)
                     msize_word = product(q%count)
                     e0 = s0 + msize_word - 1
                     l_1d(1:q%count(1))=> f_d_m%idata(s0:e0)
                     s1 = q%start(1)
                     e1 = s1 + q%count(1) - 1
                     g_1d(s1:e1) = l_1d(:)
                     s0 = e0 + 1
                  case (2)
                     call c_f_pointer(address, g_2d, shape=q%global_count)
                     msize_word = product(q%count)
                     e0 = s0 + msize_word - 1
                     l_2d(1:q%count(1),1:q%count(2))=> f_d_m%idata(s0:e0)
                     s1 = q%start(1)
                     e1 = s1 + q%count(1) - 1

                     s2 = q%start(2)
                     e2 = s2 + q%count(2) - 1

                     g_2d(s1:e1,s2:e2) = l_2d(:,:)

                     s0 = e0 + 1

                  case (3)
                     call c_f_pointer(address, g_3d, shape=q%global_count)
                     msize_word = product(q%count)
                     e0 = s0 + msize_word - 1
                     l_3d(1:q%count(1),1:q%count(2),1:q%count(3))=> f_d_m%idata(s0:e0)
                     s1 = q%start(1)
                     e1 = s1 + q%count(1) - 1

                     s2 = q%start(2)
                     e2 = s2 + q%count(2) - 1

                     s3 = q%start(3)
                     e3 = s3 + q%count(3) - 1

                     g_3d(s1:e1,s2:e2,s3:e3) = l_3d(:,:,:)

                     s0 = e0 + 1
                  case (4)
                     call c_f_pointer(address, g_4d, shape=q%global_count)
                     msize_word = product(q%count)
                     e0 = s0 + msize_word - 1
                     l_4d(1:q%count(1),1:q%count(2),1:q%count(3),1:q%count(4))=> f_d_m%idata(s0:e0)
                     s1 = q%start(1)
                     e1 = s1 + q%count(1) - 1

                     s2 = q%start(2)
                     e2 = s2 + q%count(2) - 1

                     s3 = q%start(3)
                     e3 = s3 + q%count(3) - 1

                     s4 = q%start(4)
                     e4 = s4 + q%count(4) - 1

                     g_4d(s1:e1,s2:e2,s3:e3,s4:e4) = l_4d(:,:,:,:)

                     s0 = e0 + 1
                  case (5)
                     call c_f_pointer(address, g_5d, shape=q%global_count)
                     msize_word = product(q%count)
                     e0 = s0 + msize_word - 1
                     l_5d(1:q%count(1),1:q%count(2),1:q%count(3),1:q%count(4), 1:q%count(5))=> f_d_m%idata(s0:e0)
                     s1 = q%start(1)
                     e1 = s1 + q%count(1) - 1

                     s2 = q%start(2)
                     e2 = s2 + q%count(2) - 1

                     s3 = q%start(3)
                     e3 = s3 + q%count(3) - 1

                     s4 = q%start(4)
                     e4 = s4 + q%count(4) - 1

                     s5 = q%start(5)
                     e5 = s5 + q%count(5) - 1
                     g_5d(s1:e1,s2:e2,s3:e3,s4:e4,s5:e5) = l_5d(:,:,:,:,:)

                     s0 = e0 + 1
                  end select
                  ! return to its orignal value
                  q%global_count(1) = q%global_count(1)/word_size(q%type_kind)
                  q%count(1) = q%count(1)/word_size(q%type_kind)
                  q%start(1) = (q%start(1)-1)/word_size(q%type_kind)+1
               end select
             end do ! message vec size
             if(allocated(f_d_m%idata))deallocate(f_d_m%idata)
         enddo ! nfront

         call FileMetadata_deserialize(buffer_fmd, fmd)
         deallocate (buffer_fmd)

         call thread_ptr%hist_collections%push_back(HistoryCollection(fmd))

         msg_iter = msg_map%begin()
         do while (msg_iter /= msg_map%end())
            request_id = msg_iter%key()
            msg =>msg_iter%value()
            var_iter = vars_map%find(i_to_string(request_id))
            array_ptr =>var_iter%value()
            x_ptr => array_ptr%get_values()
            address = c_loc(x_ptr(1))
            select type (q=>msg)
            class is (AbstractDataMessage)
               filename =q%file_name
               call file_timer%start()
               call thread_ptr%put_dataToFile(q, address, _rc)
               call file_timer%stop()
            end select
            call msg_iter%next()
            call array_ptr%destroy(_rc)
            call vars_map%erase(var_iter)
         enddo
         msg_iter = msg_map%begin()
         do while (msg_iter /= msg_map%end())
            call msg_map%erase(msg_iter)
            msg_iter = msg_map%begin()
         enddo

         call thread_ptr%clear_hist_collections()
         call thread_ptr%hist_collections%clear()

         time = file_timer%get_total()
         file_size = file_size*4./1024./1024. ! 4-byte integer, unit is converted to MB
         speed = file_size/time
         lgr => logging%get_logger('MAPL.pfio')
         call lgr%info(" Writing time: %f9.3 s, speed: %f9.3 MB/s, size: %f9.3 MB, at server node: %i0~:%i0~, file: %a", time, speed, file_size, this%node_rank, this%innode_rank, filename)
         call file_timer%reset()


         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         ! telling captain it is idle by sending its own rank
         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         call MPI_send(back_local_rank, 1, MPI_INTEGER, 0, stag, this%back_comm , ierr)
         _verify(ierr)
         FileDone = Filename
         call MPI_send(FileDone, FNAME_LEN, MPI_CHARACTER, 0, stag+1, this%back_comm , ierr)
         _verify(ierr)
       enddo
       _return(_success)
     end subroutine start_back_writers

   end subroutine start_back

   subroutine terminate_backend_server(this, rc)
      class (MultiGroupServer), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: terminate
      integer :: ierr, i, status
      integer :: MPI_STAT(MPI_STATUS_SIZE)

      terminate = -1
      ! starting from 2, no backend root
      do i = 2, this%nwriter
        if (allocated(this%buffers(i)%buffer)) then
           call MPI_Wait(this%buffers(i)%request, MPI_STAT, ierr)
           _verify(ierr)
        endif
      enddo

      ! The front root rank sends termination signal to the back root
      ! The back root send terminate back for synchronization
      if (this%I_am_front_root) then
         call MPI_send(terminate, 1, MPI_INTEGER, this%back_ranks(1), &
              this%back_ranks(1),  this%server_comm, ierr)
         _verify(ierr)
         call MPI_recv(terminate, 1, MPI_INTEGER, this%back_ranks(1), &
              this%front_ranks(1), this%server_comm, MPI_STAT, ierr)
         _verify(ierr)
      endif
      _return(_success)
   end subroutine terminate_backend_server

end module pFIO_MultiGroupServerMod
