#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_MultiCommServerMod
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
   use mpi

   implicit none
   private

   public :: MultiCommServer

   type,extends (BaseServer) :: MultiCommServer
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
      type(AbstractDataReferenceVector) :: MemdataRefPtrs
   contains
      procedure :: start
      procedure :: get_writing_PE
      procedure :: get_communicator
      procedure :: receive_output_data
      procedure :: create_remote_win
      procedure :: put_dataToFile
      procedure :: clean_up
   end type MultiCommServer

   interface MultiCommServer
      module procedure new_MultiCommServer
   end interface MultiCommServer

contains

   function new_MultiCommServer(server_comm, port_name, nwriter, rc) result(s)
      type (MultiCommServer) :: s
      integer, intent(in) :: server_comm
      character(*), intent(in) :: port_name
      integer, intent (in) :: nwriter
      integer, optional, intent(out) :: rc
      integer :: s_rank, s_size
      integer :: ierror, status, local_rank
      type (SimpleCommSplitter) :: splitter
      type (SplitCommunicator)   ::  s_comm
      character(len=:), allocatable :: s_name
      integer :: MPI_STAT(MPI_STATUS_SIZE)
      type (MpiSocket), target :: dummy_socket


      call MPI_Comm_dup(server_comm, s%server_comm, ierror)
      call MPI_Comm_size(s%server_comm, s_size , ierror)

      splitter = SimpleCommsplitter(s%server_comm)
      call splitter%add_group(npes = s_size-nwriter, name="o_server_front", isolate_nodes=.false.)
      call splitter%add_group(npes = nwriter,        name="o_server_back",  isolate_nodes=.false.)
 
      s_comm = splitter%split(rc=status); _VERIFY(status)

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

   end function new_MultiCommServer

   subroutine start(this)
      class (MultiCommServer), target, intent(inout) :: this

      if ( this%front_comm /= MPI_COMM_NULL) then
         call start_front()
      endif

      if ( this%back_comm /= MPI_COMM_NULL) then
         call start_back()      
      endif

   contains

      subroutine start_back()
         integer :: collection_counter, collection_total  
         integer :: ierr, rank
         integer :: my_rank, cmd, status
         integer(kind=INT64) :: msize_word

         call MPI_Comm_rank(this%server_comm, my_rank, ierr)
         allocate(this%serverthread_done_msgs(1))
         this%serverthread_done_msgs(:) = .false.

         do while (.true.)

            call MPI_Bcast(cmd, 1, MPI_INTEGER, 0, this%server_comm, ierr)
            if (cmd == -1) exit
            call this%create_remote_win(rc=status) 
            call this%receive_output_data()
            call this%put_dataToFile()

            call this%clean_up() 
           
         enddo

      end subroutine start_back
      
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
         call MPI_Bcast(terminate, 1, MPI_INTEGER, 0, this%server_comm, ierr)
         deallocate(mask)
   
      end subroutine start_front

   end subroutine start

   function get_writing_PE(this,id) result (rank)
      class(MultiCommServer),intent(in) :: this
      integer, intent(in) :: id
      integer :: rank, ith

      ith  = mod(id-1,this%nwriter)
      rank = this%back_ranks(ith+1)

   end function get_writing_PE

   subroutine create_remote_win(this, rc)
      class (MultiCommServer), target, intent(inout) :: this
      integer, optional, intent(out) :: rc
      class (AbstractDataReference), pointer :: remotePtr
      integer :: rank
      integer(KIND=INT64) :: offset, msize_word
      integer(KIND=INT64),allocatable :: offsets(:), msize_words(:)
      type (MessageVectorIterator) :: iter
      type (StringInteger64MapIterator) :: request_iter
      class (AbstractMessage), pointer :: msg
      integer :: collection_counter, collection_total, ierror
      integer :: MPI_STAT(MPI_STATUS_SIZE)
      character(len=*),parameter :: Iam = 'create_remote_win'
      type (ServerThread),pointer :: thread_ptr
      integer :: bsize, ierr
      integer :: cmd = 1
      integer, allocatable :: buffer(:)

      if (this%front_comm /= MPI_COMM_NULL) then
         call MPI_Bcast(cmd, 1, MPI_INTEGER, 0, this%server_comm, ierr)
      endif

      this%stage_offset = StringInteger64map()

      thread_ptr=> this%threads%at(1)
 
      if (this%I_am_front_root) then
         call serialize_message_vector(thread_ptr%request_backlog,buffer)
         bsize = size(buffer)
         call MPI_send(bsize,  1,     MPI_INTEGER, this%back_ranks(1), this%back_ranks(1), this%server_Comm, ierr)
         call MPI_send(buffer, bsize, MPI_INTEGER, this%back_ranks(1), this%back_ranks(1), this%server_Comm, ierr)

         call HistoryCollectionVector_serialize(thread_ptr%hist_collections, buffer)
         bsize = size(buffer)
         call MPI_send(bsize,  1,     MPI_INTEGER, this%back_ranks(1), this%back_ranks(1), this%server_Comm, ierr)
         call MPI_send(buffer, bsize, MPI_INTEGER, this%back_ranks(1), this%back_ranks(1), this%server_Comm, ierr)

      endif
      
      if (this%back_comm /= MPI_COMM_NULL) then
         if (this%I_am_back_root) then
           call MPI_recv( bsize, 1, MPI_INTEGER,    &
             this%front_ranks(1), this%back_ranks(1), this%server_comm, &
             MPI_STAT, ierr)
           allocate(buffer(bsize))
           call MPI_recv( buffer,bsize, MPI_INTEGER,    &
             this%front_ranks(1), this%back_ranks(1), this%server_comm, &
             MPI_STAT, ierr)
         endif
         call MPI_Bcast(bsize, 1, MPI_INTEGER, 0, this%back_comm, ierr)
         if (.not. allocated(buffer)) allocate(buffer(bsize))
         call MPI_Bcast(buffer, bsize, MPI_INTEGER, 0, this%back_comm, ierr)
         call deserialize_message_vector(buffer, thread_ptr%request_backlog)
         deallocate (buffer)

         if (this%I_am_back_root) then
           call MPI_recv( bsize, 1, MPI_INTEGER,    &
             this%front_ranks(1), this%back_ranks(1), this%server_comm, &
             MPI_STAT, ierr)
           allocate(buffer(bsize))
           call MPI_recv( buffer,bsize, MPI_INTEGER,    &
             this%front_ranks(1), this%back_ranks(1), this%server_comm, &
             MPI_STAT, ierr)
         endif
         call MPI_Bcast(bsize, 1, MPI_INTEGER, 0, this%back_comm, ierr)
         if (.not. allocated(buffer)) allocate(buffer(bsize))
         call MPI_Bcast(buffer, bsize, MPI_INTEGER, 0, this%back_comm, ierr)
         call HistoryCollectionVector_deserialize(buffer, thread_ptr%hist_collections)
         deallocate (buffer)
      endif

      collection_counter = 0
      collection_total   = 0
      !(1) loop to get the total number of collection
      iter   = thread_ptr%request_backlog%begin()
      do while (iter /= thread_ptr%request_backlog%end())
         msg => iter%get()
   
         select type (q=>msg)
         type is (CollectiveStageDataMessage)
            request_iter = this%stage_offset%find(i_to_string(q%collection_id))
            if ( request_iter == this%stage_offset%end()) then
               collection_total = collection_total + 1
               call this%stage_offset%insert(i_to_string(q%collection_id),int(collection_total, kind=INT64))
            endif
         end select
         call iter%next()
      end do
   
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

         rank = this%get_writing_PE(collection_counter)
         msize_word = msize_words(collection_counter)
         call this%stage_offset%insert(i_to_string(MSIZE_ID + collection_counter ),msize_word)

         allocate(remotePtr, source  = RDMAReference(pFIO_INT32,msize_word, this%server_comm, rank ))
         call this%add_DataReference(remotePtr)
         remotePtr=>null()
      enddo

      _RETURN(_SUCCESS)
   end subroutine create_remote_win

   subroutine put_DataToFile(this, rc)
     class (MultiCommServer),target, intent(inout) :: this
     integer, optional, intent(out) :: rc

     integer ::  n
     type (ServerThread),pointer :: threadPtr
     integer,pointer :: i_ptr(:)
     integer :: collection_counter
     class (AbstractDataReference), pointer :: dataRefPtr
     type (LocalMemReference), pointer :: memdataPtr=>null()
     integer(kind=MPI_ADDRESS_KIND) :: msize
     integer :: num_clients, l_rank, w_rank, ierr, empty(0)
     !real(KIND=REAL64) :: t0, t1

     !t0 = 0.0d0
     !t1 = -1.0d0
     num_clients = this%threads%size()
     if (num_clients == 0) then
        _RETURN(_SUCCESS)
     endif


     if (this%back_comm /= MPI_COMM_NULL) then

        call MPI_comm_rank(this%back_comm, l_rank, ierr)
        ! copy and save the data
        do collection_counter = 1, this%dataRefPtrs%size()
              dataRefPtr => this%get_dataReference(collection_counter)
              msize  = this%stage_offset%of(i_to_string(MSIZE_ID+collection_counter))
              call c_f_pointer(dataRefPtr%base_address,i_ptr,shape=[msize])
              w_rank = this%get_writing_PE(collection_counter)
              if ( w_rank == this%back_ranks(l_rank+1)) then ! 
                 allocate(memdataPtr, source = LocalMemReference(i_ptr))
                 call this%MemdataRefPtrs%push_back(memdataPtr)
              else
                 ! push null ptr, make sure the total is collection_total
                 allocate(memdataPtr, source = LocalMemReference(empty))
                 call this%MemdataRefPtrs%push_back(memdataPtr)
              endif
              memdataPtr => null()
         enddo

     endif

     if (this%front_comm /= MPI_COMM_NULL) then
        do n = 1, num_clients
           threadPtr=>this%threads%at(n)
           call threadPtr%clear_backlog()
           call threadPtr%clear_hist_collections()
           call threadPtr%clear_subarray()
        enddo ! threads
     endif
     !if( t1-t0 > 0) then
     !   print*, "this rank",this%rank,"spending ", t1-t0, " seconds writing"
     !endif
     _RETURN(_SUCCESS)
   end subroutine put_DataToFile

  subroutine clean_up(this, rc)
      class(MultiCommServer),intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer ::  n
      type (ServerThread),pointer :: threadPtr
      class (AbstractMessage),pointer :: msg
      type (MessageVectorIterator) :: msg_iter
      integer,pointer :: i_ptr(:)
      type(c_ptr) :: offset_address
      integer :: collection_counter
      class (AbstractDataReference), pointer :: dataRefPtr
      integer(kind=MPI_ADDRESS_KIND) :: offset
      integer :: w_rank, l_rank, ierr
      type(StringInteger64MapIterator) :: iter


      call this%clear_DataReference()
      call this%clear_RequestHandle()
      call this%set_AllBacklogIsEmpty(.true.)
      this%serverthread_done_msgs(:) = .false.

      iter = this%prefetch_offset%begin()
      do while (iter /= this%prefetch_offset%end())
         call this%prefetch_offset%erase(iter)
         iter = this%prefetch_offset%begin()
      enddo

      if (this%front_Comm /= MPI_COMM_NULL) then 
         iter = this%stage_offset%begin()
         do while (iter /= this%stage_offset%end())
            call this%stage_offset%erase(iter)
            iter = this%stage_offset%begin()
         enddo
      endif

      if (this%back_Comm /= MPI_COMM_NULL) then 
        ! time to write file
         call MPI_comm_rank(this%back_comm, l_rank, ierr)

         threadPtr=>this%threads%at(1)
         msg_iter = threadPtr%request_backlog%begin()
         do while (msg_iter /= threadPtr%request_backlog%end())
            msg => msg_iter%get()
            select type (q=>msg)
            type is (CollectiveStageDataMessage)
               collection_counter = this%stage_offset%of(i_to_string(q%collection_id))
               dataRefPtr => this%MemdataRefPtrs%at(collection_counter)

               select type(dataRefPtr)
               type is (LocalMemReference)
                 i_ptr =>dataRefPtr%i_ptr
               class default
                  _ASSERT(.false., "I expect localmemReference")
               end select

               iter = this%stage_offset%find(i_to_string(q%request_id)//'done')
               w_rank = this%get_writing_PE(collection_counter)
               if (iter == this%stage_offset%end() .and.  w_rank == this%back_ranks(l_rank+1)) then 
                  ! (1) get address where data should put
                  offset     = this%stage_offset%at(i_to_string(q%request_id))
                  offset_address   = c_loc(i_ptr(offset+1))
                  ! (2) write data
                  call threadPtr%put_DataToFile(q,offset_address)
                  !  (3) leave a mark, it has been written
                  call this%stage_offset%insert(i_to_string(q%request_id)//'done',0_MPI_ADDRESS_KIND)
               endif
            class default
               _ASSERT(.false., "I expect CollectiveStageDataMessage")
            end select
            call msg_iter%next()
         enddo

         call threadPtr%clear_backlog()
         call threadPtr%clear_hist_collections()

         iter = this%stage_offset%begin()
         do while (iter /= this%stage_offset%end())
            call this%stage_offset%erase(iter)
            iter = this%stage_offset%begin()
         enddo

         do collection_counter =  1, this%MemdataRefPtrs%size()
            dataRefPtr => this%MemdataRefPtrs%at(collection_counter)
            call dataRefPtr%deallocate()
         enddo
         call this%MemdataRefPtrs%erase(this%MemdataRefPtrs%begin(), this%MemdataRefPtrs%end())
      end if

      _RETURN(_SUCCESS)
   end subroutine clean_up

   integer function get_communicator(this) result(communicator)
      class (MultiCommServer), intent(in) :: this

      communicator = this%front_comm

   end function get_communicator

  subroutine receive_output_data(this, rc)
     class (MultiCommServer),target, intent(inout) :: this
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


end module pFIO_MultiCommServerMod
