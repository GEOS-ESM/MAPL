#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_ServerThreadMod
   use, intrinsic :: iso_c_binding, only: c_ptr
   use, intrinsic :: iso_c_binding, only: C_NULL_PTR
   use, intrinsic :: iso_c_binding, only: c_loc
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64, INT32, INT64
   use, intrinsic :: iso_c_binding, only: c_f_pointer
   use MAPL_ExceptionHandling
   use pFIO_UtilitiesMod, only: word_size, i_to_string 
   use pFIO_AbstractSocketMod
   use pFIO_AbstractMessageMod
   use pFIO_AbstractServerMod
   use pFIO_SimpleSocketMod
   use pFIO_MessageVisitorMod
   use pFIO_BaseThreadMod
   use pFIO_ExtDataCollectionMod
   use pFIO_ExtCollectionVectorMod
   use pFIO_AbstractRequestHandleMod
   use pFIO_IntegerRequestMapMod
   use pFIO_IntegerSocketMapMod
   use gFTL_IntegerVector
   use pFIO_FileMetadataMod

   use pFIO_TerminateMessageMod
   use pFIO_DoneMessageMod
   use pFIO_PrefetchDoneMessageMod
   use pFIO_CollectivePrefetchDoneMessageMod
   use pFIO_StageDoneMessageMod
   use pFIO_CollectiveStageDoneMessageMod
   use pFIO_AddExtCollectionMessageMod
   use pFIO_DummyMessageMod
   use pFIO_HandShakeMessageMod
   use pFIO_IDMessageMod
   use pFIO_AddHistCollectionMessageMod
   use pFIO_AbstractDataMessageMod
   use pFIO_PrefetchDataMessageMod
   use pFIO_CollectivePrefetchDataMessageMod
   use pFIO_StageDataMessageMod
   use pFIO_CollectiveStageDataMessageMod
   use pFIO_ModifyMetadataMessageMod

   use pFIO_NetCDF4_FileFormatterMod
   use pFIO_HistoryCollectionMod
   use pFIO_HistoryCollectionVectorMod
   use pFIO_AbstractDataReferenceMod
   use pFIO_RDMAReferenceMod
   use pFIO_LocalMemReferenceMod
   use pFIO_ShmemReferenceMod
   use pFIO_ConstantsMod
   use pFIO_MessageVectorMod
   use pFIO_MessageVectorUtilMod
   use gFTL_StringInteger64Map
   use mpi

   implicit none
   private

   public :: ServerThread
   
   type, extends(BaseThread) :: ServerThread
      private

      type (ExtCollectionVector)        :: ext_collections
      type (HistoryCollectionVector), public :: hist_collections

      logical                  :: there_is_collective_request = .false.
      logical,public           :: terminate = .false.
      type (MessageVector),public     :: request_backlog
      logical                  :: have_done = .true.
      class(AbstractServer),pointer :: containing_server=>null()
      integer :: thread_rank
      type (IntegerVector) :: sub_array_types
   contains
      procedure :: init
      procedure :: run
      procedure :: set_terminate
      procedure :: set_rank
      procedure :: set_collective_request
      procedure :: do_terminate
      procedure :: clear_terminate

      procedure :: handle_Terminate
      procedure :: handle_Done
      procedure :: handle_Done_prefetch
      procedure :: handle_Done_collective_prefetch
      procedure :: handle_Done_stage
      procedure :: handle_Done_collective_stage
      procedure :: handle_AddExtCollection
      procedure :: handle_AddHistCollection
      procedure :: handle_PrefetchData
      procedure :: handle_CollectivePrefetchData
      procedure :: handle_StageData
      procedure :: handle_CollectiveStageData
      procedure :: handle_ModifyMetadata
      procedure :: handle_HandShake

      procedure :: get_hist_collection
      procedure :: get_DataFromFile
      procedure :: get_DataFromMem
      procedure :: put_DataToFile
      procedure :: read_and_gather
      procedure :: read_and_share
      procedure :: receive_output_data
      procedure :: clear_hist_collections
      procedure :: clear_backlog
      procedure :: clear_subarray
 
   end type ServerThread

   interface ServerThread
      module procedure new_ServerThread
   end interface ServerThread

   ! Just for convenience. Will be removed after the better strategy is chosen.

   logical ::  multi_data_read = .true.  !: each node will choose one pe to read all data. No exchange between nodes 
   !logical :: multi_data_read = .false. !: each node will choose one pe to read part of the data, then exchange between nodes 

   ! WY notes:
   ! output strategy (1) ( deleted after the test ) :
   ! Each node chooses one PE and takes turns to write part of the data to the file. No data exchange among nodes. It is very slow
   ! output strategy (2) ( implemented ):
   ! Choose one PE among  servers to write. The other PE in the servers use MPI_put to forward the data the the chosen one. 

contains

   function new_ServerThread(sckt, server, rc) result(s)
      class (AbstractSocket), target, intent(in) :: sckt
      class (AbstractServer), target,optional, intent(in) :: server
      integer, optional, intent(out) :: rc

      type (ServerThread) :: s
      integer :: status

      call s%set_connection(sckt, status)
      _VERIFY(status)
      if(present(server)) s%containing_server=>server
      _RETURN(_SUCCESS)
   end function new_ServerThread

   subroutine init(this, sckt, server, rc) 
      class (ServerThread), intent(inout) :: this
      class (AbstractSocket), target, intent(in) :: sckt
      class (AbstractServer), target, intent(in) :: server
      integer, optional, intent(out) :: rc

      integer :: status

      call this%set_connection(sckt, status)
      _VERIFY(status)
      this%containing_server=>server

      _RETURN(_SUCCESS)
   end subroutine init

   subroutine run(this, rc)
      class (ServerThread), intent(inout) :: this
      integer, optional, intent(out) :: rc

      class (AbstractMessage), pointer :: message
      class(AbstractSocket),pointer :: connection
      integer :: status

      connection=>this%get_connection()
      message => connection%receive()
      if (associated(message)) then
         call message%dispatch(this, status)
         _VERIFY(status)
         deallocate(message)
      end if
      _RETURN(_SUCCESS)
   end subroutine run

   subroutine run_done(this, rc)
      class (ServerThread), intent(inout) :: this
      integer, optional, intent(out) :: rc

      class (AbstractMessage), pointer :: message
      type(DoneMessage) :: dMessage
      class(AbstractSocket),pointer :: connection
      logical :: all_backlog_is_empty
      integer :: status

      if ( .not. this%have_done) then
        call dMessage%dispatch(this)
        _RETURN(_SUCCESS)
      endif
      
      ! wait until all the backlogs are empty
      all_backlog_is_empty = this%containing_server%get_AllBacklogIsEmpty() 
      if (.not. all_backlog_is_empty) then
        _RETURN(_SUCCESS)
      endif

      connection=>this%get_connection()
      message => connection%receive()
      if (associated(message)) then
         call message%dispatch(this, status)
         _VERIFY(status)
         deallocate(message)
      end if
      _RETURN(_SUCCESS)
   end subroutine run_done

   subroutine handle_Terminate(this, message, rc)
      class (ServerThread), intent(inout) :: this
      type (TerminateMessage), intent(in) :: message
      integer, optional, intent(out) :: rc

      call this%set_terminate()

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(message)
   end subroutine handle_Terminate

   recursive subroutine handle_Done(this, message, rc)
      class (ServerThread), target, intent(inout) :: this
      type (DoneMessage), intent(in) :: message
      integer, optional, intent(out) :: rc

      type (LocalMemReference) :: mem_data_reference
      class(AbstractDataReference),pointer :: dataRefPtr
      class(AbstractMessage),pointer :: dMessage
      integer :: data_status, node_rank, innode_rank
      integer(kind=INT64) :: g_offset, offset,msize_word
      type(c_ptr) :: offset_address
      integer,pointer :: i_ptr(:)
      type (MessageVectorIterator) :: iter
      class (AbstractMessage), pointer :: msg
      class(AbstractSocket),pointer :: connection
      class (AbstractRequestHandle), pointer :: handle
      integer :: status, cmd

      ! first time handling the "Done" message, simple return
      this%containing_server%serverthread_done_msgs(this%thread_rank) = .true. 
      if ( this%have_done) then
         this%have_done = .false.
          ! Simple server will continue, but no effect for other server type
         dMessage=>this%containing_server%get_dmessage()
         call dmessage%dispatch(this)
         deallocate(dmessage)
         _RETURN(_SUCCESS)
      endif

      if ( this%request_backlog%empty()) then
        ! done issued more than once 
        this%have_done = .true.
        call this%containing_server%set_AllBacklogIsEmpty(.true.)
         _RETURN(_SUCCESS)
      endif

      iter = this%request_backlog%begin()
      msg => iter%get()
      connection=>this%get_connection(status)
      _VERIFY(status)

      select type (q=>msg)
      type is (PrefetchDataMessage)
         _ASSERT(.false., "please use done_prefetch")
         _RETURN(_SUCCESS)  
      type is (CollectivePrefetchDataMessage)
         _ASSERT(.false., "please use done_collective_prefetch")
         _RETURN(_SUCCESS)  
      type is (StageDataMessage)
         _ASSERT(.false., "please use done_stage")
         _RETURN(_SUCCESS)  
      type is (CollectiveStageDataMessage)
         _ASSERT(.false., "please use done_collective_stage")
         _RETURN(_SUCCESS)  
      class default
         _ASSERT(.false., "Wrong message type")
      end select


      if ( this%request_backlog%empty()) then 

          this%have_done = .true.
          call this%containing_server%set_AllBacklogIsEmpty(.true.) 

          if (this%there_is_collective_request) then
            !prepare for the next round of "done" message
             this%there_is_collective_request = .false.
            !! make sure this server thread will wait for all the other threads to finsh their backlog
             call this%containing_server%set_AllBacklogIsEmpty(.false.) 
            ! It reduces counter untill no threads need the shared mem 
            ! then it  set backlog flag to empty and status to UNALLOCATED, deallocate share mem, erase prefetch_offset
             call this%containing_server%update_status()
          endif

          _RETURN(_SUCCESS)  
       endif ! empty no collective request

       !if it is SimpleServer, DoneMessge will recursively call handle_done until the back_log is empty.
       !if it is Not SimpleServer, DummyMessage will do nothing and leave the subroutine
       dMessage=>this%containing_server%get_dmessage()
       call dmessage%dispatch(this) 
       deallocate(dmessage)
       _RETURN(_SUCCESS)  
       _UNUSED_DUMMY(message)
   end subroutine handle_Done

   ! W.J note: This subroutine is different for "read_and_share"
   ! To avoid duplicate reading, each node choose a PE to read part of the file, then gather and share
   function read_and_gather(this, rc) result(dataRefPtr)
      class (ServerThread), target, intent(inout) :: this
      integer, optional, intent(out) :: rc
      class (AbstractDataReference), pointer :: dataRefPtr
      integer :: node_rank,innode_rank, local_size
      integer(KIND=INT64) ::  msize_word
      integer(KIND=MPI_OFFSET_KIND) :: g_offset, offset
      integer(kind=MPI_OFFSET_KIND),allocatable :: offsets(:),g_offsets(:)
      integer,allocatable :: locals(:)
      type (MessageVectorIterator)  :: iter
      type (StringInteger64MapIterator) :: request_iter
      class (AbstractMessage), pointer :: msg
      integer,pointer :: i_ptr(:)
      type(c_ptr) :: address
      integer :: status


      allocate(offsets(  0:this%containing_server%Node_Num-1))
      allocate(g_offsets(0:this%containing_server%Node_Num-1))
      offsets   = 0
      g_offsets = 0
      this%containing_server%prefetch_offset = StringInteger64map()

      !(1) loop to get the total size and offset of each request
      iter = this%request_backlog%begin()
      do while (iter /= this%request_backlog%end())
         msg => iter%get()

         select type (q=>msg)
         type is (CollectivePrefetchDataMessage)
            ! each server may have mutiple clients with same requst_id
            request_iter = this%containing_server%prefetch_offset%find(i_to_string(q%request_id))
            if ( request_iter == this%containing_server%prefetch_offset%end()) then
               msize_word = word_size(q%type_kind)*product(int(q%global_count,INT64))
               ! which node this message will be handled
               call this%containing_server%distribute_task(q%request_id,node_rank,innode_rank)
               ! insert local offset for each node
               call this%containing_server%prefetch_offset%insert(i_to_string(q%request_id),offsets(node_rank))
               offsets(node_rank) = offsets(node_rank) + msize_word
            endif
         end select

         call iter%next()
      end do
      ! save the global offset for each node
      g_offsets(0) = 0
      call this%containing_server%prefetch_offset%insert(i_to_string(0),g_offsets(0))
      do node_rank = 1,this%containing_server%Node_Num-1
         g_offsets(node_rank) = g_offsets(node_rank-1)+offsets(node_rank-1)
         call this%containing_server%prefetch_offset%insert(i_to_string(-node_rank),g_offsets(node_rank))
      enddo

      ! (2) allocate the memory
      msize_word = sum(offsets)
      call this%containing_server%prefetch_offset%insert(i_to_string(MSIZE_ID),msize_word)
      allocate(dataRefPtr, source = ShmemReference(pFIO_INT32,msize_word, this%containing_server%InNode_Comm))

      ! (3) loop to read file into the its slot of memory
      call c_f_pointer(dataRefPtr%base_address,i_ptr,shape=[msize_word])
      iter = this%request_backlog%begin()
      do while (iter /= this%request_backlog%end())
         msg => iter%get()

         select type (q=>msg)
         type is (CollectivePrefetchDataMessage)
            request_iter = this%containing_server%prefetch_offset%find(i_to_string(q%request_id)//'done')
            if (request_iter == this%containing_server%prefetch_offset%end() .and. &
               this%containing_server%am_I_reading_PE(q%request_id)) then
              ! get address where data should put
               node_rank = this%containing_server%Node_Rank
               g_offset  = g_offsets(node_rank)
               offset    = this%containing_server%prefetch_offset%at(i_to_string(q%request_id))
               offset    = g_offset + offset 
               address   = c_loc(i_ptr(offset+1))
                ! (2) read data
               call this%get_DataFromFile(q, address, status)
               _VERIFY(status)
               call this%containing_server%prefetch_offset%insert(i_to_string(q%request_id)//'done',0_Int64)
            endif
         end select
         call iter%next()
      enddo 

      call dataRefPtr%fence(rc=status)
      _VERIFY(status)
      ! (4) root nodes exchange the shared data 
      if(this%containing_server%I_am_NodeRoot() .and. this%containing_server%Node_Num > 1 ) then

        node_rank = this%containing_server%Node_Rank
        local_size = int(offsets(node_rank))
        allocate(locals(local_size))
        locals = i_ptr(g_offsets(node_rank)+1:g_offsets(node_rank)+ local_size)
        call MPI_AllGATHERV(locals, local_size,            MPI_INTEGER, &
                            i_ptr,  int(offsets),  int(g_offsets),  MPI_INTEGER, &
                            this%containing_server%NodeRoot_Comm,status)
        deallocate(locals)

      endif 

      call dataRefPtr%fence(rc=status)
      _VERIFY(status)

      deallocate(g_offsets,offsets)
      _RETURN(_SUCCESS)
   end function read_and_gather

   ! W.J note: This subroutine is different for "read_and_gather"
   ! To avoid exchanging data among nodes, each node choose a PE to read the file
   function read_and_share(this, rc) result(dataRefPtr)
      class (ServerThread), target, intent(inout) :: this
      integer, optional, intent(out) :: rc
      class (AbstractDataReference), pointer :: dataRefPtr

      integer :: node_rank, innode_rank, status
      integer(KIND=INT64) :: offset, msize_word
      type (MessageVectorIterator) :: iter
      type (StringInteger64MapIterator) :: request_iter
      class (AbstractMessage), pointer :: msg
      integer,pointer :: i_ptr(:)
      type(c_ptr) :: address
      character(len=*),parameter :: Iam = 'read_and_share'

      this%containing_server%prefetch_offset = StringInteger64map()

      !(1) loop to get the total size and offset of each request
      offset = 0
      iter   = this%request_backlog%begin()
      do while (iter /= this%request_backlog%end())
         msg => iter%get()

         select type (q=>msg)
         type is (CollectivePrefetchDataMessage)
            request_iter = this%containing_server%prefetch_offset%find(i_to_string(q%request_id))
            if ( request_iter == this%containing_server%prefetch_offset%end()) then
               ! insert local offset for each node
               call this%containing_server%prefetch_offset%insert(i_to_string(q%request_id),offset)
               msize_word = word_size(q%type_kind)*product(int(q%global_count,INT64))
               offset = offset + msize_word
            endif
         end select
         call iter%next()
      end do

      ! (2) allocate the memory
      msize_word = offset  ! last offset is the total size
      call this%containing_server%prefetch_offset%insert(i_to_string(MSIZE_ID),msize_word)
      allocate(dataRefPtr, source = ShmemReference(pFIO_INT32,msize_word, this%containing_server%InNode_Comm))

      ! (3) loop to read file into the its slot of memory
      call c_f_pointer(dataRefPtr%base_address,i_ptr,shape=[msize_word])
      iter = this%request_backlog%begin()
      do while (iter /= this%request_backlog%end())
         msg => iter%get()

         select type (q=>msg)
         type is (CollectivePrefetchDataMessage)

           request_iter = this%containing_server%prefetch_offset%find(i_to_string(q%request_id)//'done')
           if (request_iter == this%containing_server%prefetch_offset%end()) then ! not read yet

              call this%containing_server%distribute_task(q%request_id,node_rank,innode_rank)

              if (this%containing_server%Innode_Rank == innode_rank) then ! pick a rank from each node to read
                 ! (1) get address where data should put
                 offset    = this%containing_server%prefetch_offset%at(i_to_string(q%request_id))
                 address   = c_loc(i_ptr(offset+1))
                 ! (2) read data
                 call this%get_DataFromFile(q, address, rc=status)
                 _VERIFY(status)
                 ! (3) leave a mark, it has been read
                 call this%containing_server%prefetch_offset%insert(i_to_string(q%request_id)//'done',0_Int64)
              endif
            endif

         end select
         call iter%next()
      enddo 

      call dataRefPtr%fence(rc=status)
      _VERIFY(status)
      _RETURN(_SUCCESS)
   end function read_and_share

   subroutine handle_AddExtCollection(this, message, rc)
      class (ServerThread), target, intent(inout) :: this
      type (AddExtCollectionMessage), intent(in) :: message
      integer, optional, intent(out) :: rc

      integer :: n
      logical :: found
      type (ExtCollectionVectorIterator) :: iter
      type (ExtDataCollection), pointer :: collection
      type (ExtDataCollection) :: c
      class(AbstractSocket),pointer :: connection

      iter = this%ext_collections%begin()
      n = 1

      ! Is it a new collection?
      found = .false.
      do while (iter /= this%ext_collections%end())
         collection => iter%get()
         if (message%template == collection%template) then
            found = .true.
            exit
         end if
         n = n + 1
         call iter%next()
      end do

      if (.not. found) then
         c = new_ExtDataCollection(message%template)
         call this%ext_collections%push_back(c)
      end if
      connection=>this%get_connection()      
      call connection%send(IdMessage(n))         
      
      _RETURN(_SUCCESS)
   end subroutine handle_AddExtCollection

   subroutine handle_AddHistCollection(this, message, rc)
      class (ServerThread), target, intent(inout) :: this
      type (AddHistCollectionMessage), intent(in) :: message
      integer, optional, intent(out) :: rc

      integer :: n
      type (HistoryCollection) :: hist_collection
      class(AbstractSocket),pointer :: connection
     
      if ( message%collection_id == -1 ) then 
         n = this%hist_collections%size()+1
      else
         n = message%collection_id
      endif

      hist_collection = HistoryCollection(message%fmd)
      if ( message%collection_id == -1) then
         call this%hist_collections%push_back(hist_collection)
      else
         call this%hist_collections%set(n,hist_collection)
      endif

      connection=>this%get_connection()      
      call connection%send(IdMessage(n))
      _RETURN(_SUCCESS)
   end subroutine handle_AddHistCollection

   subroutine handle_PrefetchData(this, message, rc)
      class (ServerThread), intent(inout) :: this
      type (PrefetchDataMessage), intent(in) :: message
      integer, optional, intent(out) :: rc

      class(AbstractSocket),pointer :: connection
      type (DummyMessage) :: handshake_msg

      connection=>this%get_connection()
      call connection%send(handshake_msg)
      call this%request_backlog%push_back(message)

      _RETURN(_SUCCESS)
   end subroutine handle_PrefetchData

   subroutine handle_CollectivePrefetchData(this, message, rc)
      class (ServerThread), intent(inout) :: this
      type (CollectivePrefetchDataMessage), intent(in) :: message
      integer, optional, intent(out) :: rc

      class(AbstractSocket),pointer :: connection
      type (DummyMessage) :: handshake_msg

      connection=>this%get_connection()
      call connection%send(handshake_msg)
      call this%request_backlog%push_back(message)

      _RETURN(_SUCCESS)
   end subroutine handle_CollectivePrefetchData

   subroutine handle_ModifyMetadata(this, message, rc)
      class (ServerThread), intent(inout) :: this
      type (ModifyMetadataMessage), intent(in) :: message
      integer, optional, intent(out) :: rc

      type (HistoryCollection),pointer :: hist_collection
      class(AbstractSocket),pointer :: connection
      type (DummyMessage) :: handshake_msg

      hist_collection=>this%hist_collections%at(message%collection_id) 
      call hist_collection%ModifyMetadata(message%var_map)

      connection=>this%get_connection()
      call connection%send(handshake_msg)

      _RETURN(_SUCCESS)
   end subroutine handle_ModifyMetadata

   subroutine handle_HandShake(this, message, rc)
      class (ServerThread), target, intent(inout) :: this
      type (HandShakeMessage), intent(in) :: message
      integer, optional, intent(out) :: rc
     
      class(AbstractSocket),pointer :: connection
      type (DummyMessage) :: handshake_msg

      _UNUSED_DUMMY(message)
 
      connection=>this%get_connection()
      call connection%send(handshake_msg)
      _RETURN(_SUCCESS)

   end subroutine handle_HandShake

   subroutine set_terminate(this)
      class (ServerThread), intent(inout) :: this
      this%terminate = .true.
   end subroutine set_terminate

   subroutine set_rank(this,rank)
      class (ServerThread), intent(inout) :: this
      integer, intent(in) :: rank
      this%thread_rank  = rank
   end subroutine set_rank

   subroutine set_collective_request(this,request, have_done)
      class (ServerThread), intent(inout) :: this
      logical, intent(in) :: request
      logical, intent(in) :: have_done
     
      this%there_is_collective_request = request
      this%have_done = have_done
   end subroutine set_collective_request

   subroutine clear_terminate(this)
      class (ServerThread), intent(inout) :: this
      this%terminate = .false.
   end subroutine clear_terminate

   logical function do_terminate(this)
      class (ServerThread), intent(in) :: this
      do_terminate = this%terminate
   end function do_terminate

   subroutine get_DataFromFile(this,message,address, rc)
      class (ServerThread), intent(inout)    :: this
      class (AbstractDataMessage), intent(in) ::  message
      type (c_ptr), intent(in) :: address
      integer, optional, intent(out) :: rc

      type (NetCDF4_FileFormatter), pointer :: formatter

      integer(kind=INT32), pointer :: values_int32_0d
      integer(kind=INT32), pointer :: values_int32_1d(:)
      real(kind=REAL32), pointer :: values_real32_0d
      real(kind=REAL32), pointer :: values_real32_1d(:)
      integer(kind=INT64), pointer :: values_int64_0d
      integer(kind=INT64), pointer :: values_int64_1d(:)
      real(kind=REAL64), pointer :: values_real64_0d
      real(kind=REAL64), pointer :: values_real64_1d(:)

      type (ExtDataCollection), pointer :: collection

      integer, allocatable :: start(:),count(:)

      collection => this%ext_collections%at(message%collection_id)
      formatter => collection%find(message%file_name)

      select type (message)
      type is (PrefetchDataMessage)
        start = message%start
        count = message%count
      type is (CollectivePrefetchDataMessage)
        start = message%global_start
        count = message%global_count
      class default
        _ASSERT(.false., "wrong PrefetchDataMessage type")  
      end select

!      if (product(count) /= product(file_data_reference%shape)) stop "memory size not match"
      select case (size(count)) ! rank
      case (0)
          select case (message%type_kind)
          case (pFIO_INT32)
              call c_f_pointer(address, values_int32_0d)
              call formatter%get_var(message%var_name, values_int32_0d)
          case (pFIO_REAL32)
              call c_f_pointer(address, values_real32_0d)
              call formatter%get_var(message%var_name, values_real32_0d)
          case (pFIO_INT64)
              call c_f_pointer(address, values_int64_0d)
              call formatter%get_var(message%var_name, values_int64_0d)
          case (pFIO_REAL64)
              call c_f_pointer(address, values_real64_0d)
              call formatter%get_var(message%var_name, values_real64_0d)
          case default
              _ASSERT(.false., "Not supported type")
          end select
      case (1:)
          select case (message%type_kind)
          case (pFIO_INT32)
              call c_f_pointer(address, values_int32_1d, [product(count)])
              call formatter%get_var(message%var_name, values_int32_1d, start=start, count=count)
          case (pFIO_REAL32)
              call c_f_pointer(address, values_real32_1d, [product(count)])
              call formatter%get_var(message%var_name, values_real32_1d, start=start, count=count)
          case (pFIO_INT64)
              call c_f_pointer(address, values_int64_1d, [product(count)])
              call formatter%get_var(message%var_name, values_int64_1d, start=start, count=count)
          case (pFIO_REAL64)
              call c_f_pointer(address, values_real64_1d, [product(count)])
              call formatter%get_var(message%var_name, values_real64_1d, start=start, count=count)
          case default
              _ASSERT(.false., "Not supported type")
          end select
       end select
       _RETURN(_SUCCESS)
   end subroutine get_DataFromFile

   subroutine handle_StageData(this, message, rc)
      class (ServerThread), intent(inout) :: this
      type (StageDataMessage), intent(in) :: message
      integer, optional, intent(out) :: rc

      class(AbstractSocket),pointer :: connection
      type(LocalMemReference) :: mem_data_reference
      type(DummyMessage) :: handshake_msg

      connection=>this%get_connection()
      call connection%send(handshake_msg)
      call this%request_backlog%push_back(message)

      mem_data_reference=LocalMemReference(message%type_kind,message%count)
      !iRecv
      call this%insert_RequestHandle(message%request_id, &
              & connection%get(message%request_id, mem_data_reference))

       _RETURN(_SUCCESS)
   end subroutine handle_StageData

   subroutine handle_CollectiveStageData(this, message, rc)
      class (ServerThread), intent(inout) :: this
      type (CollectiveStageDataMessage), intent(in) :: message
      integer, optional, intent(out) :: rc

      class(AbstractSocket),pointer :: connection
      type(LocalMemReference) :: mem_data_reference
      type(DummyMessage) :: handshake_msg

      connection=>this%get_connection()
      call connection%send(handshake_msg)
      call this%request_backlog%push_back(message)

      mem_data_reference=LocalMemReference(message%type_kind,message%count)
      !iRecv
      call this%insert_RequestHandle(message%request_id, &
              & connection%get(message%request_id, mem_data_reference))
      _RETURN(_SUCCESS)
   end subroutine handle_CollectiveStageData

   subroutine put_DataToFile(this, message, address, rc)
      class (ServerThread), intent(inout)    :: this
      class (AbstractDataMessage), intent(in) ::  message
      integer, optional, intent(out) :: rc

      type (c_ptr), intent(in) :: address

      type (NetCDF4_FileFormatter),pointer :: formatter
      type (HistoryCollection),pointer :: hist_collection

      integer(kind=INT32), pointer :: values_int32_0d
      integer(kind=INT32), pointer :: values_int32_1d(:)
      integer(kind=INT64), pointer :: values_int64_0d
      integer(kind=INT64), pointer :: values_int64_1d(:)
      real(kind=REAL32), pointer :: values_real32_0d
      real(kind=REAL32), pointer :: values_real32_1d(:)
      real(kind=REAL64), pointer :: values_real64_0d
      real(kind=REAL64), pointer :: values_real64_1d(:)

      integer, allocatable :: start(:),count(:)

      hist_collection=>this%hist_collections%at(message%collection_id)
      formatter =>hist_collection%find(message%file_name)
 
      select type (message)
      type is (StageDataMessage)
        start = message%start
        start = 1
        count = message%count
      type is (CollectiveStageDataMessage)

        start = message%global_start
        count = message%global_count

      class default
        _ASSERT(.false., "wrong StageDataMessage type")
      end select
!      if (product(count) /= product(file_data_reference%shape)) stop "memory size not match"
      select case (size(count)) ! rank
      case (0)
          select case (message%type_kind)
          case (pFIO_INT32)
              call c_f_pointer(address, values_int32_0d)
              call formatter%put_var(message%var_name, values_int32_0d)
          case (pFIO_INT64)
              call c_f_pointer(address, values_int64_0d)
              call formatter%put_var(message%var_name, values_int64_0d)
          case (pFIO_REAL32)
              call c_f_pointer(address, values_real32_0d)
              call formatter%put_var(message%var_name, values_real32_0d)
          case (pFIO_REAL64)
              call c_f_pointer(address, values_real64_0d)
              call formatter%put_var(message%var_name, values_real64_0d)
          case default
              _ASSERT(.false., "not supported type")
          end select
      case (1:)
          select case (message%type_kind)
          case (pFIO_INT32)
              call c_f_pointer(address, values_int32_1d, [product(count)])
              call formatter%put_var(message%var_name, values_int32_1d, start=start, count=count)
          case (pFIO_INT64)
              call c_f_pointer(address, values_int64_1d, [product(count)])
              call formatter%put_var(message%var_name, values_int64_1d, start=start, count=count)
          case (pFIO_REAL32)
              call c_f_pointer(address, values_real32_1d, [product(count)])
              call formatter%put_var(message%var_name, values_real32_1d, start=start, count=count)
          case (pFIO_REAL64)
              call c_f_pointer(address, values_real64_1d, [product(count)])
              call formatter%put_var(message%var_name, values_real64_1d, start=start, count=count)
          case default
              _ASSERT(.false., "not supported type")
          end select
       end select

       _RETURN(_SUCCESS)
   end subroutine put_DataToFile

   subroutine receive_output_data(this, rc)
     class (ServerThread),target,intent(inout) :: this
     integer, optional, intent(out) :: rc

     class (AbstractDataReference), pointer :: dataRefPtr
     type (RDMAReference), pointer :: remotePtr
     integer(kind=MPI_ADDRESS_KIND) :: msize_word, offset
     integer :: local_size
     integer, pointer :: k_ptr(:)
     type (MessageVectorIterator) :: iter
     class (AbstractRequestHandle), pointer :: handle
     class (AbstractMessage), pointer :: msg
     integer :: sub_arr_type
     integer :: ndims,ierror, words
     integer :: collection_counter, rank
     character(*), parameter :: Iam="receive_output_data"
     integer, allocatable :: gcount(:),lcount(:), lstart(:)

     iter = this%request_backlog%begin()
     do while (iter /= this%request_backlog%end())
        msg=>iter%get()
        select type(msg)
        type is (CollectiveStageDataMessage)
            handle => this%get_RequestHandle(msg%request_id)
            call handle%wait()

            words = word_size(msg%type_kind)
            local_size = product(msg%count)*words

            if (local_size > 0) then
               call c_f_pointer(handle%data_reference%base_address, k_ptr, shape=[local_size])
               collection_counter = this%containing_server%stage_offset%at(i_to_string(msg%collection_id))
               msize_word  = this%containing_server%stage_offset%of(i_to_string(MSIZE_ID+collection_counter))

               ndims = size(msg%start)
               offset  = this%containing_server%stage_offset%at(i_to_string(msg%request_id))
               gcount = msg%global_count
               lcount = msg%count
               lstart = msg%start - 1
               gcount(1) = gcount(1)* words
               lcount(1) = lcount(1)* words
               lstart(1) = lstart(1)* words
               call MPI_type_create_subarray(ndims,gcount,lcount, lstart , &
                    MPI_ORDER_FORTRAN, MPI_INTEGER,sub_arr_type,ierror)
               _VERIFY(ierror)
               call MPI_type_commit(sub_arr_type,ierror)
               _VERIFY(ierror)

               dataRefPtr => this%containing_server%get_dataReference(collection_counter)
               select type(dataRefPtr)
               type is (RDMAReference)
                  remotePtr=>dataRefPtr
               class default
                  _ASSERT(.false., " need a remote pointer")
               end select

               rank = remotePtr%mem_rank
               call MPI_put(k_ptr, local_size, MPI_INTEGER, rank, offset, 1, sub_arr_type, remotePtr%win, ierror)
               _VERIFY(ierror)

               call this%sub_array_types%push_back(sub_arr_type)
               nullify(k_ptr)

            endif ! local_size > 0   
        class default
            _ASSERT(.false., "receive_output_data")
        end select
        call iter%next()
     enddo

     _RETURN(_SUCCESS)
  endsubroutine receive_output_data

  subroutine clear_hist_collections(this)
    class (ServerThread),intent(inout) :: this
    integer :: i
    type(HistoryCollection),pointer :: hist
  
    do i = 1, this%hist_collections%size()
      hist=>this%hist_collections%at(i)
      call hist%clear()
    enddo

  end subroutine clear_hist_collections

  subroutine clear_backlog(this)
    class (ServerThread),intent(inout) :: this
    type (MessageVectorIterator) :: iter

    iter = this%request_backlog%begin()
    do while (iter /= this%request_backlog%end())
       call this%request_backlog%erase(iter)
       iter = this%request_backlog%begin()
    enddo 

  end subroutine clear_backlog

  subroutine clear_subarray(this, rc)
    class (ServerThread),intent(inout) :: this
    integer, optional, intent(out) :: rc
    integer:: i, sub_arr_type, ierror

    do i = 1, this%sub_array_types%size()
      sub_arr_type = this%sub_array_types%at(i)
      call MPI_type_free(sub_arr_type,ierror)
      _VERIFY(ierror)
    enddo
    call this%sub_array_types%erase(this%sub_array_types%begin(), this%sub_array_types%end())

    _RETURN(_SUCCESS)
  end subroutine clear_subarray

  recursive subroutine handle_Done_collective_stage(this, message, rc)
      class (ServerThread), target, intent(inout) :: this
      type (CollectiveStageDoneMessage), intent(in) :: message
      integer, optional, intent(out) :: rc

      integer :: status, data_status, cmd

      _UNUSED_DUMMY(message)

      this%containing_server%serverthread_done_msgs(this%thread_rank) = .true. 
      if ( .not. all(this%containing_server%serverthread_done_msgs)) then
         _RETURN(_SUCCESS)
      endif

      _ASSERT( associated(this%containing_server), "need server") 

      call this%containing_server%create_remote_win(rc=status)
      _VERIFY(status)

      call this%containing_server%receive_output_data(rc=status) 
      _VERIFY(status)

      call this%containing_server%put_dataToFile(rc=status)
      _VERIFY(status)

      call this%containing_server%clean_up()

      _RETURN(_SUCCESS)  
   end subroutine handle_Done_collective_stage

   recursive subroutine handle_Done_stage(this, message, rc)
      class (ServerThread), target, intent(inout) :: this
      type (StageDoneMessage), intent(in) :: message
      integer, optional, intent(out) :: rc

      type (MessageVectorIterator) :: iter
      class (AbstractMessage), pointer :: msg
      class(AbstractSocket),pointer :: connection
      class (AbstractRequestHandle), pointer :: handle
      integer :: status

      _UNUSED_DUMMY(message)

      if ( this%request_backlog%empty()) then
         _RETURN(_SUCCESS)
      endif

      iter = this%request_backlog%begin()
      do while ( iter /= this%request_backlog%end())
         msg => iter%get()
         connection=>this%get_connection(status)
         _VERIFY(status)

         select type (q=>msg)
         type is (StageDataMessage)

            handle => this%get_RequestHandle(q%request_id)
            call handle%wait()

            call this%put_DataToFile(q,handle%data_reference%base_address,rc=status)
            _VERIFY(status)

            call this%request_backlog%erase(iter)

         class default
            _ASSERT(.false., "Wrong message type")
         end select
         iter = this%request_backlog%begin()
     enddo

     call this%clear_RequestHandle()
     call this%clear_hist_collections()

   end subroutine handle_Done_stage

   recursive subroutine handle_Done_prefetch(this, message, rc)
      class (ServerThread), target, intent(inout) :: this
      type (PrefetchDoneMessage), intent(in) :: message
      integer, optional, intent(out) :: rc

      type (LocalMemReference) :: mem_data_reference
      type (MessageVectorIterator) :: iter
      class (AbstractMessage), pointer :: msg
      class(AbstractSocket),pointer :: connection
      integer :: status

      iter = this%request_backlog%begin()
      do while ( iter /= this%request_backlog%end())
         msg => iter%get()
         connection=>this%get_connection(status)
         _VERIFY(status)

         select type (q=>msg)
         type is (PrefetchDataMessage)
             mem_data_reference=LocalMemReference(q%type_kind,q%count)

             call this%get_DataFromFile(q,mem_data_reference%base_address, status)
             _VERIFY(status)

             call this%insert_RequestHandle(q%request_id, &
              & connection%put(q%request_id, mem_data_reference))
             call this%request_backlog%erase(iter)

         class default
            _ASSERT(.false., "Wrong message type")
         end select
         iter = this%request_backlog%begin()
       enddo

       call this%clear_RequestHandle()

       _RETURN(_SUCCESS)  
       _UNUSED_DUMMY(message)
   end subroutine handle_Done_prefetch

   recursive subroutine handle_Done_collective_prefetch(this, message, rc)
      class (ServerThread), target, intent(inout) :: this
      type (CollectivePrefetchDoneMessage), intent(in) :: message
      integer, optional, intent(out) :: rc

      class(AbstractDataReference),pointer :: dataRefPtr
      integer :: status

      ! first time handling the "Done" message, simple return
      this%containing_server%serverthread_done_msgs(this%thread_rank) = .true. 
      if ( .not. all(this%containing_server%serverthread_done_msgs)) then
         _RETURN(_SUCCESS)
      endif

      if( .not. multi_data_read) then
        ! each node read part of a file, then exchange
         dataRefPtr=> this%read_and_gather(rc=status)
         _VERIFY(status)
      else
         dataRefPtr=> this%read_and_share(rc=status)
         _VERIFY(status)
      endif
      ! now dataRefPtr on each node has all the data
      call this%containing_server%add_DataReference(dataRefPtr)

      call this%containing_server%get_DataFromMem(multi_data_read, rc=status)

      call this%containing_server%clean_up()
 
      _RETURN(_SUCCESS)  
      _UNUSED_DUMMY(message)
   end subroutine handle_Done_collective_prefetch

   subroutine get_DataFromMem( this, multi_data_read, rc)
      class (ServerThread), target, intent(inout) :: this
      logical, intent(in) :: multi_data_read
      integer, optional, intent(out) :: rc
      type (LocalMemReference) :: mem_data_reference
      class(AbstractDataReference),pointer :: dataRefPtr
      integer :: node_rank, innode_rank
      integer(kind=INT64) :: g_offset, offset,msize_word
      type(c_ptr) :: offset_address
      integer,pointer :: i_ptr(:)
      type (MessageVectorIterator) :: iter
      class (AbstractMessage), pointer :: msg
      class(AbstractSocket),pointer :: connection
      integer :: status      

      connection=>this%get_connection(status)
      _VERIFY(status)

      iter = this%request_backlog%begin()
      do while (iter /= this%request_backlog%end())
         msg => iter%get()
         select type (q=>msg)
         type is (CollectivePrefetchDataMessage)
            mem_data_reference = LocalMemReference(q%type_kind,q%count)
            offset = this%containing_server%prefetch_offset%at(i_to_string(q%request_id))

           !W.J node: these three lines are necessar for read_and_gather call
           if ( .not. multi_data_read) then
              call this%containing_server%distribute_task(q%request_id,node_rank,innode_rank)
              g_offset = this%containing_server%prefetch_offset%at(i_to_string(-node_rank))
              offset = offset+g_offset
           endif

           msize_word  = this%containing_server%prefetch_offset%at(i_to_string(MSIZE_ID))

           dataRefPtr => this%containing_server%get_dataReference()
           call c_f_pointer(dataRefPtr%base_address,i_ptr,shape=[msize_word])

           offset_address = c_loc(i_ptr(offset+1))

           call mem_data_reference%fetch_data(offset_address,q%global_count,q%start-q%global_start+1)

           call this%insert_RequestHandle(q%request_id, &
              & connection%put(q%request_id, mem_data_reference))

           call this%request_backlog%erase(iter)
         class default
           _ASSERT(.false., "Message type should be CollectivePrefetchDataMessage ")
         end select
         iter = this%request_backlog%begin()
      enddo

      _RETURN(_SUCCESS)
   end subroutine get_DataFromMem

   function get_hist_collection(this, collection_id) result(c)
      class (ServerThread), target, intent(inout) :: this
      integer, intent(in) :: collection_id
      type (HistoryCollection), pointer :: c
      c=>this%hist_collections%at(collection_id)
   end function

end module pFIO_ServerThreadMod
