#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_MultiGroupServerMod
   use, intrinsic :: iso_c_binding, only: c_ptr
   use, intrinsic :: iso_c_binding, only: C_NULL_PTR
   use, intrinsic :: iso_c_binding, only: c_loc
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64, INT32, INT64
   use, intrinsic :: iso_c_binding, only: c_f_pointer
   use pFIO_KeywordEnforcerMod
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
   use pFIO_AttributeMod
   use pFIO_StringAttributeMapMod
   use pFIO_StringAttributeMapUtilMod
   use MAPL_SplitCommunicatorMod
   use MAPL_SimpleCommSplitterMod
   use pFIO_MpiSocketMod
   use gFTL_IntegerVector
   use pFIO_AbstractRequestHandleMod
   use pFIO_FileMetadataMod
   use pFIO_IntegerMessageMapMod
   use mpi

   implicit none
   private

   public :: MultiGroupServer

   type :: vector_array
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
   contains
      procedure :: start
      procedure :: start_back
      procedure :: get_communicator
      procedure :: receive_output_data
      procedure :: create_remote_win
      procedure :: put_dataToFile
      procedure :: clean_up
      procedure :: terminate_back
   end type MultiGroupServer

   interface MultiGroupServer
      module procedure new_MultiGroupServer
   end interface MultiGroupServer

contains

   function new_MultiGroupServer(server_comm, port_name, nwriter_per_node, rc) result(s)
      type (MultiGroupServer) :: s
      integer, intent(in) :: server_comm
      character(*), intent(in) :: port_name
      integer, intent (in) :: nwriter_per_node
      integer, optional, intent(out) :: rc
      integer :: s_rank, s_size
      integer :: ierror, status, local_rank
      type (SimpleCommSplitter) :: splitter
      type (SplitCommunicator)   ::  s_comm
      character(len=:), allocatable :: s_name
      integer :: MPI_STAT(MPI_STATUS_SIZE)
      type (MpiSocket), target :: dummy_socket
      integer :: nwriter
      integer, allocatable :: ranks(:)
      integer, allocatable :: node_sizes(:)

      call MPI_Comm_dup(server_comm, s%server_comm, ierror)
      call MPI_Comm_size(s%server_comm, s_size , ierror)

      splitter = SimpleCommsplitter(s%server_comm)
      node_sizes = splitter%get_node_sizes()
      call splitter%add_group(npes_per_node = node_sizes(1)-nwriter_per_node, name="o_server_front", isolate_nodes=.false.)
      call splitter%add_group(npes_per_node = nwriter_per_node,               name="o_server_back",  isolate_nodes=.false.)
 
      s_comm = splitter%split(rc=status); _VERIFY(status)

      nwriter = nwriter_per_node*size(node_sizes)
      s%front_comm = MPI_COMM_NULL
      s%back_comm  = MPI_COMM_NULL
      s%nwriter    = nwriter
      s%nfront     = s_size - nwriter
      s%threads = ServerThreadVector()
      allocate(s%back_ranks(nwriter))
      allocate(s%front_ranks(s%nfront))
      call MPI_Comm_rank(s%server_comm, s_rank, ierror)
      s_name = s_comm%get_name()
      s%I_am_front_root = .false.
      s%I_am_back_root = .false.
      if (index(s_name, 'o_server_front') /=0) then
         s%front_comm = s_comm%get_subcommunicator()
         call s%init(s%front_comm)
         s%port_name = trim(port_name)
         call MPI_Comm_rank(s%front_comm, local_rank, ierror)
         if (s_rank == 0) then 
           _ASSERT( local_rank == 0, "re-arrange the rank of the server_comm") 
           s%I_am_front_root = .true.          
           call MPI_recv(s%back_ranks, nwriter, MPI_INTEGER, MPI_ANY_SOURCE, 666, s%server_comm, MPI_STAT,ierror)
         endif
         call MPI_Bcast(s%back_ranks, nwriter, MPI_INTEGER, 0, s%front_comm, ierror)

         call MPI_AllGather(s_rank, 1, MPI_INTEGER, s%front_ranks, 1, MPI_INTEGER, s%front_comm, ierror)
         if (local_rank ==0 ) then
            call MPI_Send(s%front_ranks, s_size-nwriter, MPI_INTEGER, s%back_ranks(1), 777, s%server_comm, ierror)
         endif
         
      endif

      if (index(s_name, 'o_server_back') /=0) then
         s%back_comm = s_comm%get_subcommunicator()
         call MPI_AllGather(s_rank, 1, MPI_INTEGER, s%back_ranks, 1, MPI_INTEGER, s%back_comm, ierror)
         call MPI_Comm_rank(s%back_comm, local_rank, ierror)
         if (local_rank ==0 ) then
            s%I_am_back_root = .true.          
            call MPI_Send(s%back_ranks, nwriter, MPI_INTEGER, 0, 666, s%server_comm, ierror)
         endif

         if (s_rank == s%back_ranks(1)) then
            _ASSERT( local_rank == 0, "re-arrange the rank of the server_comm")           
            call MPI_recv(s%front_ranks, s%nfront, MPI_INTEGER, MPI_ANY_SOURCE, 777, s%server_comm, MPI_STAT,ierror)
         endif
         
         call MPI_Bcast(s%front_ranks, s%nfront, MPI_INTEGER, 0, s%back_comm, ierror)
         call s%set_status(1)
         call s%add_connection(dummy_socket)
      endif

      if (s_rank == 0) print*, "MultiServer Start: nfront, nback", s%nfront, s%nwriter

   end function new_MultiGroupServer

   subroutine start(this)
      class (MultiGroupServer), target, intent(inout) :: this

      if ( this%front_comm /= MPI_COMM_NULL) then
         call start_front()
      endif

      if ( this%back_comm /= MPI_COMM_NULL) then
         call this%start_back()      
      endif

   contains

      subroutine start_front()
         class (ServerThread), pointer :: thread_ptr => null()
         integer :: i,client_size,ierr
         logical, allocatable :: mask(:)
         integer :: terminate = -1
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
               call thread_ptr%run()
               !delete the thread object if it terminates 
               if(thread_ptr%do_terminate()) then
                  mask(i) = .true.
               endif
            enddo
   
            if (all(mask)) exit
   
         enddo
   
         call this%threads%clear()
         call this%terminate_back()
         deallocate(mask)
   
      end subroutine start_front

   end subroutine start

   subroutine create_remote_win(this, rc)
      class (MultiGroupServer), target, intent(inout) :: this
      integer, optional, intent(out) :: rc
      _RETURN(_SUCCESS)
   end subroutine create_remote_win

   subroutine put_DataToFile(this, rc)
     class (MultiGroupServer),target, intent(inout) :: this
     integer, optional, intent(out) :: rc
     if (this%front_Comm == MPI_COMM_NULL) then 
        _ASSERT(.false. , "hey backend does not call this")
     else
        _RETURN(_SUCCESS)
     endif  
   end subroutine put_DataToFile

   subroutine clean_up(this, rc)
      class(MultiGroupServer),intent(inout) :: this
      integer, optional, intent(out) :: rc
      type(StringInteger64MapIterator) :: iter
      integer :: num_clients, n
      class (ServerThread),pointer :: thread_ptr

      if (this%front_Comm == MPI_COMM_NULL) then 
         _RETURN(_SUCCESS)
      endif

      num_clients = this%threads%size()

      do n = 1, num_clients
         thread_ptr=>this%threads%at(n)
         call thread_ptr%clear_backlog()
         call thread_ptr%clear_hist_collections()
      enddo ! threads

      call this%clear_RequestHandle()
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

      _RETURN(_SUCCESS)
   end subroutine clean_up

   integer function get_communicator(this) result(communicator)
      class (MultiGroupServer), intent(in) :: this

      communicator = this%front_comm

   end function get_communicator

  subroutine receive_output_data(this, rc)
     class (MultiGroupServer),target, intent(inout) :: this
     integer, optional, intent(out) :: rc

     integer :: i, client_num
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
              handle => thread_ptr%get_RequestHandle(q%request_id)
              call handle%wait()
              words = word_size(q%type_kind)
              local_size = product(q%count)*words
              if (local_size > 0) then
                 collection_counter = this%stage_offset%at(i_to_string(q%collection_id))
                 call c_f_pointer(handle%data_reference%base_address, i_ptr, shape=[local_size])
                 call f_d_ms(collection_counter)%add_data_message(q, i_ptr)
              endif
           class default
              _ASSERT(.false., "yet to implemented")
           end select
           call iter%next()
        end do ! iter
     enddo ! client_num

     ! serializes and send data_and_message to writer
     do collection_counter = 1, collection_total
        ! root asks for idle writer and sends axtra file metadata
        if (this%I_am_front_root) then

           collection_id = collection_ids%at(collection_counter)
           call Mpi_Send(collection_id, 1, MPI_INTEGER, this%back_ranks(1), this%back_ranks(1), this%server_comm, ierror)
           ! here thread_ptr can point to any thread
           hist_collection => thread_ptr%hist_collections%at(collection_id)
           call hist_collection%fmd%serialize(buffer)

           call Mpi_Recv(back_local_rank, 1, MPI_INTEGER, this%back_ranks(1), &
                this%front_ranks(1), this%server_comm, MPI_STAT, ierror)

           msg_size= size(buffer)
           call Mpi_send(msg_size,1, MPI_INTEGER, this%back_ranks(back_local_rank+1), &
               this%back_ranks(back_local_rank+1), this%server_comm, ierror)
           call Mpi_send(buffer,msg_size, MPI_INTEGER, this%back_ranks(back_local_rank+1), &
               this%back_ranks(back_local_rank+1), this%server_comm, ierror)
        endif

        call Mpi_Bcast( back_local_rank, 1, MPI_INTEGER, 0, this%front_comm, ierror)

        call f_d_ms(collection_counter)%serialize(buffer)
        msg_size= size(buffer)
        call Mpi_send(msg_size,1, MPI_INTEGER, this%back_ranks(back_local_rank+1), &
             this%back_ranks(back_local_rank+1), this%server_comm, ierror)
        call Mpi_send(buffer,msg_size, MPI_INTEGER, this%back_ranks(back_local_rank+1), &
            this%back_ranks(back_local_rank+1), this%server_comm, ierror)
     enddo

     deallocate(f_d_ms)
     _RETURN(_SUCCESS)
   end subroutine receive_output_data

   ! the logic is similar to pfio_writer
   subroutine start_back(this, rc)
     class (MultiGroupServer),target, intent(inout) :: this
     integer, optional, intent(out) :: rc
     integer :: ierr

     integer :: MPI_STAT(MPI_STATUS_SIZE)
     integer :: i, j, idle, no_job, idle_worker
     integer :: collection_id 
     integer, allocatable :: busy(:)
     integer :: msg_size, back_local_rank
     
     type (MessageVectorIterator) :: iter
     class (AbstractMessage), pointer :: msg
     class (ServerThread),pointer :: thread_ptr
     integer, parameter :: stag = 6782
     class (vector_array), allocatable :: buffers(:)
     integer, allocatable :: buffer_fmd(:)

     integer, pointer :: g_1d(:), l_1d(:), g_2d(:,:), l_2d(:,:), g_3d(:,:,:), l_3d(:,:,:)
     integer, pointer :: g_4d(:,:,:,:), l_4d(:,:,:,:), g_5d(:,:,:,:,:), l_5d(:,:,:,:,:)
     integer :: msize_word, d_rank, request_id
     integer :: s0, e0, s1, e1, s2, e2, s3, e3, s4, e4, s5, e5
     type (StringAttributeMap) :: vars_map
     type (StringAttributeMapIterator) :: var_iter
     type (IntegerMessageMap) :: msg_map
     type (IntegerMessageMapIterator) :: msg_iter

     class (*), pointer :: x_ptr(:) 
     integer , allocatable :: buffer_v(:)
     type (Attribute), pointer :: attr_ptr
     type (c_ptr) :: address
     type (ForwardDataAndMessage), target :: f_d_m
     type (FileMetaData) :: fmd

     call MPI_Comm_rank(this%back_comm, back_local_rank, ierr)
     thread_ptr => this%threads%at(1)

     allocate(busy(this%nwriter-1), source =0)
    
     allocate(this%serverthread_done_msgs(1))
     this%serverthread_done_msgs(:) = .false.
 
     if ( this%I_am_back_root ) then ! captain node is distributing work
        do while (.true.)
           ! 1) captain node of back_comm is waiting command from front_comm
           call MPI_recv( collection_id, 1, MPI_INTEGER, &
                this%front_ranks(1), this%back_ranks(1), this%server_comm, &
                MPI_STAT, ierr)
           if (collection_id >= 1) then ! front_comm is asking for a writing node 
  
               ! check idle woker
               idle_worker = 0
               do i = 1, this%nwriter -1
                  if (busy(i) == 0) then
                     idle_worker = i
                     exit
                  endif
               enddo

               ! if all workers are busy, wait for one
               if (idle_worker == 0) then 
                  call MPI_recv( idle_worker, 1, MPI_INTEGER, &
                      MPI_ANY_SOURCE, stag, this%back_comm, &
                      MPI_STAT, ierr)
               endif

               ! tell front comm which idel_worker is ready
               call MPI_send(idle_worker, 1, MPI_INTEGER, this%front_ranks(1), &
                             this%front_ranks(1), this%server_comm, ierr)
               busy(idle_worker) = 1
               ! forward the collection_id to the idle_worker 
               call MPI_send(collection_id, 1, MPI_INTEGER, idle_worker, idle_worker,this%back_comm, ierr)

            else ! command /=1, notify the worker to quit and finalize

               no_job = -1
               do i = 1, this%nwriter -1
                  if ( busy(i) == 0) then
                     call MPI_send(no_job, 1, MPI_INTEGER, i, i, this%back_comm, ierr)
                  else
                     call MPI_recv( idle, 1, MPI_INTEGER, &
                          i, stag, this%back_comm, &
                          MPI_STAT, ierr)
                     if (idle /= i ) stop ("idle should be i")
                     call MPI_send(no_job, 1, MPI_INTEGER, i, i, this%back_comm, ierr)
                  endif  
               enddo  
               exit
            endif
         enddo
      else ! not root but writers 
        do while (.true.)

           ! 1) get collection id from captain
           call MPI_recv( collection_id, 1, MPI_INTEGER, &
                  0, back_local_rank, this%back_comm, &
                  MPI_STAT, ierr)
           if (collection_id == -1 ) exit
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! sync with create_remote_win from front_com
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

           allocate(buffers(this%nfront))

           do i = 1, this%nfront
              ! receive extra metadata from front root
              if (i == 1) then
                 call MPI_recv( msg_size, 1, MPI_INTEGER,    &
                      this%front_ranks(i), this%back_ranks(back_local_rank+1), this%server_comm, &
                      MPI_STAT, ierr)
                 allocate(buffer_fmd(msg_size))
                 call MPI_recv( buffer_fmd(1), msg_size, MPI_INTEGER, &
                   this%front_ranks(i), this%back_ranks(back_local_rank+1), this%server_comm, &
                   MPI_STAT, ierr)
              endif

              call MPI_recv( msg_size, 1, MPI_INTEGER,    &
                   this%front_ranks(i), this%back_ranks(back_local_rank+1), this%server_comm, &
                   MPI_STAT, ierr)
              allocate(buffers(i)%buffer(msg_size))
              call MPI_recv( buffers(i)%buffer(1), msg_size, MPI_INTEGER, &
                   this%front_ranks(i), this%back_ranks(back_local_rank+1), this%server_comm, &
                   MPI_STAT, ierr)
           enddo

           ! re-org data
           vars_map = StringAttributeMap()
           msg_map  = IntegerMessageMap()

           do i = 1, this%nfront
              s0 = 1
              f_d_m = ForwardDataAndMessage()
              call f_d_m%deserialize(buffers(i)%buffer)
              deallocate(buffers(i)%buffer)
              if (size(f_d_m%idata) ==0) cycle
              iter = f_d_m%msg_vec%begin()
              do j = 1, f_d_m%msg_vec%size()
                 msg => f_d_m%msg_vec%at(j)
                 select type (q=>msg)
                 type is (CollectiveStageDataMessage)
                    var_iter = vars_map%find(i_to_string(q%request_id))
                    if (var_iter == vars_map%end()) then
                       msize_word = word_size(q%type_kind)*product(q%global_count)
                       allocate(buffer_v(msize_word), source = -1)
                       call vars_map%insert(i_to_string(q%request_id), Attribute(buffer_v))
                       var_iter = vars_map%find(i_to_string(q%request_id))
                       deallocate(buffer_v)
                       call msg_map%insert(q%request_id, q)
                    endif
                    attr_ptr => var_iter%value()
                    x_ptr => attr_ptr%get_values()
                    select type (ptr=>x_ptr)
                    type is (integer(INT32))
                       address = c_loc(ptr(1))
                    end select
                    d_rank = size(q%global_count)
                    ! first dimension increases
                    q%global_count(1) = word_size(q%type_kind)*q%global_count(1)
                    q%count(1) = word_size(q%type_kind)*q%count(1)
                    q%start(1) = word_size(q%type_kind)*(q%start(1)-1)+1
                    select case (d_rank)
                    case (0)
                       _ASSERT(.false., "scalar ?? ")
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
               end do
               if(allocated(f_d_m%idata))deallocate(f_d_m%idata)
           enddo
          
           call FileMetadata_deserialize(buffer_fmd, fmd)

           thread_ptr => this%threads%at(1) ! backend only has one thread
           call thread_ptr%hist_collections%push_back(HistoryCollection(fmd))

           msg_iter = msg_map%begin()
           do while (msg_iter /= msg_map%end())
              request_id = msg_iter%key()
              msg =>msg_iter%value()

              var_iter = vars_map%find(i_to_string(request_id))
              attr_ptr =>var_iter%value()
              x_ptr => attr_ptr%get_values()
              select type (ptr=>x_ptr)
              type is (integer(INT32))
                   address = c_loc(ptr(1))
              end select
              select type (q=>msg)
              class is (AbstractDataMessage) 
                 call thread_ptr%put_dataToFile(q, address) 
              end select
              call msg_iter%next()
           enddo
           call thread_ptr%clear_hist_collections()
           call thread_ptr%hist_collections%clear()
           deallocate (buffers)
           deallocate (buffer_fmd) 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! telling captain, I am the soldier that is ready to have more work
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

           call MPI_send(back_local_rank, 1, MPI_INTEGER, 0, stag, this%back_comm , ierr)

         enddo
      endif

      if (this%I_am_back_root) then
         ! send done message to server
         ! this serves the syncronization with oserver
         collection_id = -1
         call MPI_send(collection_id, 1, MPI_INTEGER, 0, 0, this%server_comm, ierr)
      endif

   end subroutine start_back

   subroutine terminate_back(this)
      class (MultiGroupServer), intent(inout) :: this
      integer :: terminate = -1
      integer :: ierr
      integer :: MPI_STAT(MPI_STATUS_SIZE)
      ! The front root rank sends termination signal to the back root 
      ! The back root send terminate back for synchronization
      if (this%I_am_front_root) then
         call MPI_send(terminate, 1, MPI_INTEGER, this%back_ranks(1), &
              this%back_ranks(1),  this%server_comm, ierr)
         call MPI_recv(terminate, 1, MPI_INTEGER, this%back_ranks(1), &
              this%front_ranks(1), this%server_comm, MPI_STAT, ierr)
      endif

   end subroutine terminate_back

end module pFIO_MultiGroupServerMod
