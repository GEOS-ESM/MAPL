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
   use pFIO_AbstractDataReferenceMod
   use pFIO_AbstractServerMod
   use gFTL_StringInteger64Map
   use pFIO_AbstractMessageMod
   use pFIO_AbstractDataReferenceMod
   use pFIO_AbstractDataReferenceVectorMod
   use pFIO_ShmemReferenceMod
   use pFIO_RDMAReferenceMod
   use pFIO_LocalMemReferenceMod
   use pFIO_MessageVectorMod
   use pFIO_MessageVectorUtilMod
   use pFIO_ForwardDataMessageMod
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
   use mpi

   implicit none
   private

   public :: MultiGroupServer

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
      integer, allocatable :: group_comms(:)

      type(AbstractDataReferenceVector) :: MemdataRefPtrs
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
      integer :: server_group, tmp_group, n, nwriter
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


     ! create a group of comms
      call MPI_Comm_group(s%server_comm, server_group, ierror)

      allocate(s%group_comms(s%nwriter))
      do n = 1, s%nwriter
         ranks =[ s%front_ranks, s%back_ranks(n)]
         call MPI_Group_incl(server_group, s%nfront+1, ranks, tmp_group, ierror)
         call MPI_Comm_create_group(s%server_comm, tmp_group, s%front_ranks(n)+1, s%group_comms(n), ierror)
      enddo

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
      class (AbstractDataReference), pointer :: remotePtr
      integer :: rank
      integer(KIND=INT64) :: offset, msize_word
      integer(KIND=INT64),allocatable :: offsets(:), msize_words(:)
      type (MessageVectorIterator) :: iter
      type (StringInteger64MapIterator) :: request_iter
      class (AbstractMessage), pointer :: msg
      integer :: collection_counter, collection_total, collection_id, ierror
      integer :: MPI_STAT(MPI_STATUS_SIZE)
      character(len=*),parameter :: Iam = 'create_remote_win'
      type (ServerThread),pointer :: thread_ptr
      integer :: msg_size, hist_size, back_local_rank
      integer, allocatable :: bufferm(:), bufferh(:), groups(:)
      type (IntegerVector) :: collection_ids

      if (this%front_comm == MPI_COMM_NULL) then
         _RETURN(_SUCCESS)
      endif

      this%stage_offset = StringInteger64map()

      thread_ptr=> this%threads%at(1)

      !(1) loop to get the total number of collection
      collection_counter = 0
      collection_total   = 0

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

      ! send request to get the writer for each collection
      allocate(groups(collection_total))
      if (this%I_am_front_root) then
         call serialize_message_vector(thread_ptr%request_backlog, bufferm)
         msg_size  = size(bufferm)
         call HistoryCollectionVector_serialize(thread_ptr%hist_collections, bufferh)
         hist_size = size(bufferh)

         do collection_counter = 1, collection_total
            collection_id = collection_ids%at(collection_counter)
            call Mpi_Send(collection_id, 1, MPI_INTEGER, this%back_ranks(1), this%back_ranks(1), this%server_comm, ierror)
            call Mpi_Recv(back_local_rank, 1, MPI_INTEGER, this%back_ranks(1), &
                 this%front_ranks(1), this%server_comm, MPI_STAT, ierror)

            groups(collection_counter) =  back_local_rank
            call Mpi_send(msg_size,1, MPI_INTEGER, this%back_ranks(back_local_rank+1), &
                 this%back_ranks(back_local_rank+1), this%server_comm, ierror) 
            call Mpi_send(bufferm,msg_size, MPI_INTEGER, this%back_ranks(back_local_rank+1), &
                 this%back_ranks(back_local_rank+1), this%server_comm, ierror) 

            call Mpi_send(hist_size,1, MPI_INTEGER, this%back_ranks(back_local_rank+1), &
                 this%back_ranks(back_local_rank+1), this%server_comm, ierror) 
            call Mpi_send(bufferh,hist_size, MPI_INTEGER, this%back_ranks(back_local_rank+1), &
                 this%back_ranks(back_local_rank+1), this%server_comm, ierror) 
         enddo
      endif

      call Mpi_Bcast(groups, collection_total, MPI_INTEGER, 0, this%front_comm, ierror)

     !(2) loop to get the total size and offset of each collection and request
      allocate(offsets(collection_total), msize_words(collection_total))
      offsets = 0
      offset = 0
      iter   = thread_ptr%request_backlog%begin()
      do while (iter /= thread_ptr%request_backlog%end())
         msg => iter%get()
   
         select type (q=>msg)
         type is (CollectiveStageDataMessage)
            collection_counter = this%stage_offset%of(i_to_string(q%collection_id))
            request_iter = this%stage_offset%find(i_to_string(q%request_id))
            if ( request_iter == this%stage_offset%end()) then
               ! insert local offset for each node
               call this%stage_offset%insert(i_to_string(q%request_id),offsets(collection_counter))
               msize_word = word_size(q%type_kind)*product(int(q%global_count,INT64))
               offsets(collection_counter) = offsets(collection_counter) + msize_word
            endif
         end select
         call iter%next()
      end do
      msize_words = offsets

      do collection_counter = 1, collection_total

         back_local_rank = groups(collection_counter)
         rank = this%nfront
         msize_word = msize_words(collection_counter)
         call this%stage_offset%insert(i_to_string(MSIZE_ID + collection_counter ),msize_word)
         allocate(remotePtr, source  = RDMAReference(pFIO_INT32,msize_word, this%group_comms(back_local_rank+1), rank ))
         call this%add_DataReference(remotePtr)
         remotePtr=>null()
      enddo

      deallocate(groups)

      _RETURN(_SUCCESS)
   end subroutine create_remote_win

   subroutine put_DataToFile(this, rc)
     class (MultiGroupServer),target, intent(inout) :: this
     integer, optional, intent(out) :: rc

     integer :: num_clients, n
     type (ServerThread),pointer :: threadPtr

     if (this%front_comm == MPI_COMM_NULL) then
        _RETURN(_SUCCESS)
     endif

     num_clients = this%threads%size()

     do n = 1, num_clients
        threadPtr=>this%threads%at(n)
        call threadPtr%clear_backlog()
        call threadPtr%clear_hist_collections()
        call threadPtr%clear_subarray()
     enddo ! threads
     _RETURN(_SUCCESS)
   end subroutine put_DataToFile

  subroutine clean_up(this, rc)
      class(MultiGroupServer),intent(inout) :: this
      integer, optional, intent(out) :: rc
      type(StringInteger64MapIterator) :: iter


      if (this%front_Comm == MPI_COMM_NULL) then 
         _RETURN(_SUCCESS)
      endif

      call this%clear_DataReference()
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

     integer :: i, client_num, status
     type (ServerThread),pointer :: threadPtr
     class (AbstractDataReference), pointer :: dataRefPtr

     client_num = this%threads%size()

     if (this%front_comm /= MPI_COMM_NULL) then
       do i = 1, client_num
         threadPtr=>this%threads%at(i)
         call threadPtr%receive_output_data(rc=status)
         _VERIFY(status)
       enddo
     endif

     do i = 1, this%dataRefPtrs%size()
        dataRefPtr => this%get_dataReference(i)
        call dataRefPtr%fence(rc=status)
         _VERIFY(status)
     enddo
     _RETURN(_SUCCESS)
   end subroutine receive_output_data

   ! the logic is similar to pfio_writer
   subroutine start_back(this, rc)
     class (MultiGroupServer),target, intent(inout) :: this
     integer, optional, intent(out) :: rc
     integer :: ierr

     integer :: MPI_STAT(MPI_STATUS_SIZE)
     integer :: i, idle, no_job, idle_worker
     integer :: collection_id 
     integer, allocatable :: busy(:)
     integer :: msg_size, hist_size, back_local_rank
     integer, allocatable :: bufferm(:), bufferh(:)
     integer, pointer :: i_ptr(:)
     integer(kind=MPI_ADDRESS_KIND) :: offset, msize_word
     type (RDMAReference), pointer :: remotePtr
     type (LocalMemReference), pointer :: memDataPtr
     
     type (MessageVectorIterator) :: iter
     type (StringInteger64MapIterator) :: offset_iter
     class (AbstractMessage), pointer :: msg
     type (ServerThread),pointer :: threadPtr
     type(c_ptr) :: offset_address
     integer, parameter :: stag = 6782

     call MPI_Comm_rank(this%back_comm, back_local_rank, ierr)
     threadPtr => this%threads%at(1)

     allocate(busy(this%nwriter-1), source =0)
     
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
           call MPI_recv( msg_size, 1, MPI_INTEGER,    &
                this%front_ranks(1), this%back_ranks(back_local_rank+1), this%server_comm, &
                  MPI_STAT, ierr)
           allocate(bufferm(msg_size))
           call MPI_recv( bufferm, msg_size, MPI_INTEGER, &
                this%front_ranks(1), this%back_ranks(back_local_rank+1), this%server_comm, &
                  MPI_STAT, ierr)

           call MPI_recv( hist_size, 1, MPI_INTEGER,&
                this%front_ranks(1), this%back_ranks(back_local_rank+1), this%server_comm, &
                MPI_STAT, ierr)

           allocate(bufferh(hist_size))
           call MPI_recv( bufferh, hist_size, MPI_INTEGER, &
                this%front_ranks(1), this%back_ranks(back_local_rank+1), this%server_comm, &
                MPI_STAT, ierr)

           ! deserilize msg and  hist collection
           call deserialize_message_vector(bufferm, threadPtr%request_backlog)
           deallocate (bufferm)

           call HistoryCollectionVector_deserialize(bufferh, threadPtr%hist_collections)
           deallocate (bufferh)
         
           !(2) loop to get the total size and offset of each collection and request
           offset = 0
           iter   = threadPtr%request_backlog%begin()
           do while (iter /= threadPtr%request_backlog%end())
              msg => iter%get()

              select type (q=>msg)
              type is (CollectiveStageDataMessage)
                 if ( q%collection_id == collection_id) then
                       ! insert local offset for each node
                    call this%stage_offset%insert(i_to_string(q%request_id),offset)
                    msize_word = word_size(q%type_kind)*product(int(q%global_count,INT64))
                    offset = offset + msize_word
                 endif
              end select
              call iter%next()
           end do

           msize_word = offset
           call this%stage_offset%insert(i_to_string(MSIZE_ID + collection_id ),msize_word)

           allocate(remotePtr, source  = RDMAReference(pFIO_INT32,msize_word, this%group_comms(back_local_rank+1), this%nfront))

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! front_comm receive and put data, back_comecopy the data after fence
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

           ! fence after get the data
           call remotePtr%fence()
           call c_f_pointer(remotePtr%base_address,i_ptr,shape=[msize_word])
           allocate(memdataPtr, source = LocalMemReference(i_ptr))

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! cleanup with front_comm to release shared remote win
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

           call remotePtr%deallocate()
           deallocate(remotePtr)

!!!!!!!!!!!!
! write File
!!!!!!!!!!!!           
           iter   = threadPtr%request_backlog%begin()
           do while (iter /= threadPtr%request_backlog%end())
              msg => iter%get()

              select type (q=>msg)
              type is (CollectiveStageDataMessage)
                 if ( q%collection_id == collection_id) then
           
                    i_ptr =>memDataPtr%i_ptr

                    offset  = this%stage_offset%at(i_to_string(q%request_id))
                    offset_address = c_loc(i_ptr(offset + 1))
                    call threadPtr%put_DataToFile(q, offset_address)
                 endif
              end select
              call iter%next()
           end do
           call memDataPtr%deallocate()
           deallocate(memDataPtr)

           call threadPtr%clear_backlog()
           call threadPtr%clear_hist_collections()

           offset_iter = this%stage_offset%begin()
           do while (offset_iter /= this%stage_offset%end())
              call this%stage_offset%erase(offset_iter)
              offset_iter = this%stage_offset%begin()
           enddo 

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
