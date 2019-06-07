module pFIO_ServerThreadMod
   use, intrinsic :: iso_c_binding, only: c_ptr
   use, intrinsic :: iso_c_binding, only: C_NULL_PTR
   use, intrinsic :: iso_c_binding, only: c_loc
   use, intrinsic :: iso_fortran_env, only: REAL32, INT32
   use, intrinsic :: iso_c_binding, only: c_f_pointer
   use pFIO_UtilitiesMod, only: word_size 
   use pFIO_AbstractSocketMod
   use pFIO_AbstractMessageMod
   use pFIO_AbstractServerMod
   use pFIO_SimpleSocketMod
   use pFIO_MessageVisitorMod
   use pFIO_BaseThreadMod
   use pFIO_ExtDataCollectionMod
   use pFIO_CollectionVectorMod
   use pFIO_AbstractRequestHandleMod
   use pFIO_IntegerRequestMapMod
   use pFIO_IntegerSocketMapMod

   use pFIO_TerminateMessageMod
   use pFIO_DoneMessageMod
   use pFIO_AddCollectionMessageMod
   use pFIO_CollectionIdMessageMod
   use pFIO_RequestIdMessageMod
   use pFIO_RequestDataMessageMod
   use pFIO_CollectiveRequestDataMessageMod
   use pFIO_WaitRequestDataMessageMod

   use pFIO_NetCDF4_FileFormatterMod
   use pFIO_AbstractDataReferenceMod
   use pFIO_MemReferenceMod
   use pFIO_ShmemReferenceMod
   use pFIO_ConstantsMod
   use pFIO_MessageQueueMod
   use pFIO_IntegerIntegerMapMod
   use mpi

   implicit none
   private

   public :: ServerThread
   
   type, extends(BaseThread) :: ServerThread
      private

      type (CollectionVector)  :: collections
      logical                  :: there_is_collective_request = .false.
      logical,public           :: terminate = .false.
      type (MessageQueue)      :: request_backlog
      logical                  :: have_done = .true.
      class(AbstractServer),pointer :: containing_server=>null()

   contains
      procedure :: run
      procedure :: set_terminate
      procedure :: do_terminate
      procedure :: clear_terminate

      procedure :: has_message
      procedure :: handle_Terminate
      procedure :: handle_Done
      procedure :: handle_AddCollection
      procedure :: handle_RequestData
      procedure :: handle_CollectiveRequestData
      procedure :: handle_WaitRequestData

      procedure :: get_DataFromFile
      procedure :: IamReading
      procedure :: distribute_task
      procedure :: allocate_and_read
   end type ServerThread

   interface ServerThread
      module procedure new_ServerThread
   end interface ServerThread


contains


   function new_ServerThread(sckt,server) result(s)
      class (AbstractSocket), target, intent(in) :: sckt
      class (AbstractServer), target,optional, intent(in) :: server
      type (ServerThread) :: s

      call s%set_connection(sckt)
      if(present(server)) s%containing_server=>server

   end function new_ServerThread

   function has_message(this) result(has)
      class (ServerThread), intent(inout) :: this
      logical :: has
      class(AbstractSocket),pointer :: connection

      connection=>this%get_connection()
      has = connection%has_message()
   end function

   subroutine run(this)
      class (ServerThread), intent(inout) :: this
      class (AbstractMessage), pointer :: message
      type(DoneMessage) :: dMessage
      class(AbstractSocket),pointer :: connection
      logical :: all_backlog_is_empty

      if ( .not. this%have_done) then
        call dMessage%dispatch(this)
        return
      endif
   
      ! wait until all the backlogs are empty
      all_backlog_is_empty = this%containing_server%get_AllBacklogIsEmpty() 

      if (.not. all_backlog_is_empty) return

      connection=>this%get_connection()
      message => connection%receive()
      if (associated(message)) then
         call message%dispatch(this)
         deallocate(message)
      end if

   end subroutine run

   subroutine handle_Terminate(this, message)
      class (ServerThread), intent(inout) :: this
      type (TerminateMessage), intent(in) :: message

      call this%set_terminate()

   end subroutine handle_Terminate

   subroutine handle_Done(this, message)
      class (ServerThread), target, intent(inout) :: this
      type (DoneMessage), intent(in) :: message
      type (MemReference) :: mem_data_reference

      type (MemReference) :: local_memdata
      type (ShmemReference) :: shmem_reference
      type (ShmemReference),pointer :: shmemPtr
      class(AbstractMessage),pointer :: dMessage
      integer :: data_status, offset,msize
      integer :: g_offset, node_rank, innode_rank
      type(c_ptr) :: offset_address
      integer,pointer :: i_ptr(:)
      type (MessageQueueIterator) :: iter
      class (AbstractMessage), pointer :: msg
      class(AbstractSocket),pointer :: connection
      
      ! first time handling the "Done" message, simple return
      if ( this%have_done) then
         this%have_done = .false.
         ! Simple server will continue, but no effect for other server type
         dMessage=>this%containing_server%get_dmessage()
         call dmessage%dispatch(this) 
         deallocate(dmessage)
         return
      endif

      if ( this%request_backlog%empty()) then 
        ! done issued more than once 
        this%have_done = .true.
        call this%containing_server%set_AllBacklogIsEmpty(.true.) 
        return
      endif 

      iter = this%request_backlog%begin()
      msg => iter%get()
      connection=>this%get_connection()

      select type (q=>msg)
      type is (RequestDataMessage)

            mem_data_reference=MemReference(q%type_kind,q%count)

            call this%get_DataFromFile(q,mem_data_reference%base_address)

            call this%insert_RequestHandle(q%request_id, &
              & connection%put(q%request_id, mem_data_reference))

      type is (CollectiveRequestDataMessage)

         this%there_is_collective_request = .true.

         if(.not. associated(this%containing_server)) stop "need server"

         data_status = this%containing_server%get_and_set_status()

         if (data_status == UNALLOCATED ) then

            shmem_reference = this%allocate_and_read()
            call this%containing_server%set_shmemReference(shmem_reference)
         endif
         
         do while (data_status == PENDING )
            !$omp taskyield
            data_status = this%containing_server%get_status()
         enddo

         local_memdata = MemReference(q%type_kind,q%count)
     
         call this%distribute_task(q%request_id,node_rank,innode_rank)

         g_offset = this%containing_server%request_offset%at(-node_rank)
         offset = this%containing_server%request_offset%at(q%request_id)
         offset = offset+g_offset
         msize  = this%containing_server%request_offset%at(MSIZE_ID)

         shmemPtr => this%containing_server%get_shmemReference()
         call c_f_pointer(shmemPtr%base_address,i_ptr,shape=[msize])

         offset_address = c_loc(i_ptr(offset+1))

         call local_memdata%fetch_data(offset_address,q%global_count,q%start-q%global_start+1)

         call this%insert_RequestHandle(q%request_id, &
              & connection%put(q%request_id, local_memdata))


       end select

       call this%request_backlog%erase(iter)

       if ( this%request_backlog%empty()) then 

          this%have_done = .true.
          call this%containing_server%set_AllBacklogIsEmpty(.true.) 

          if (this%there_is_collective_request) then
            !!prepare for the next round of "done" message
             this%there_is_collective_request = .false.
            !! make sure this server thread will wait for all the other threads to finsh their backlog
             call this%containing_server%set_AllBacklogIsEmpty(.false.) 
            ! It reduces counter untill no threads need the shared mem 
            ! then it  set backlog flag to empty and status to UNALLOCATED, deallocate share mem, erase request_offset
             call this%containing_server%update_status()
          endif

          return  
       endif ! empty no collective request

       !if it is SimpleServer, DoneMessge will recursively call handle_done until the back_log is empty.
       !if it is Not SimpleServer, DummyMessage will do nothing and leave the subroutine
       dMessage=>this%containing_server%get_dmessage()
       call dmessage%dispatch(this) 
       deallocate(dmessage)

   end subroutine handle_Done

   function allocate_and_read(this) result(shmem_reference)
      class (ServerThread), target, intent(inout) :: this
      type (ShmemReference) :: shmem_reference
      integer :: msize,node_rank,innode_rank
      integer :: g_offset,offset
      integer,allocatable :: offsets(:),g_offsets(:),locals(:)
      type (MessageQueueIterator) :: iter
      class (AbstractMessage), pointer :: msg
      integer,pointer :: i_ptr(:)
      type(c_ptr) :: address
      integer :: status,start0,end0


      allocate(offsets(  0:this%containing_server%Node_Num-1))
      allocate(g_offsets(0:this%containing_server%Node_Num-1))
      offsets   = 0
      g_offsets = 0
      this%containing_server%request_offset = IntegerIntegermap()

      !(1) loop to get the total size and offset of each request
      iter = this%request_backlog%begin()
      do while (iter /= this%request_backlog%end())
         msg => iter%get()

         select type (q=>msg)
         type is (CollectiveRequestDataMessage)
             msize = word_size(q%type_kind)*product(q%global_count)
             ! which node this message will be handled
             call this%distribute_task(q%request_id,node_rank,innode_rank)
             ! insert local offset for each node
             call this%containing_server%request_offset%insert(q%request_id,offsets(node_rank))
             offsets(node_rank) = offsets(node_rank) + msize
         end select

         call iter%next()
      end do
      ! save the global offset for each node
      g_offsets(0) = 0
      call this%containing_server%request_offset%insert(0,g_offsets(0))
      do node_rank = 1,this%containing_server%Node_Num-1
         g_offsets(node_rank) = g_offsets(node_rank-1)+offsets(node_rank-1)
         call this%containing_server%request_offset%insert(-node_rank,g_offsets(node_rank))
      enddo

      ! (2) allocate the memory
      msize = sum(offsets)
      call this%containing_server%request_offset%insert(MSIZE_ID,msize)
      shmem_reference = ShmemReference(pFIO_INT32,[msize], this%containing_server%InNode_Comm)

      ! (3) loop to read file into the its slot of memory
      call c_f_pointer(shmem_reference%base_address,i_ptr,shape=[msize])
      iter = this%request_backlog%begin()
      do while (iter /= this%request_backlog%end())
         msg => iter%get()

         select type (q=>msg)
         type is (CollectiveRequestDataMessage)

           if (this%IamReading(q%request_id)) then
             ! get address where data should put
              node_rank = this%containing_server%Node_Rank
              g_offset  = g_offsets(node_rank)
              offset    = this%containing_server%request_offset%at(q%request_id)
              offset    = g_offset + offset 
              address   = c_loc(i_ptr(offset+1))
             ! (2) read data
              call this%get_DataFromFile(q, address)
           endif

         end select
         call iter%next()
      enddo 

      call shmem_reference%fence()
 
      ! (4) root nodes exchange the shared data 
      if(this%containing_server%I_am_NodeRoot() .and. this%containing_server%Node_Num > 1 ) then

        node_rank = this%containing_server%Node_Rank
        start0 = g_offsets(node_rank)+1
        end0   = g_offsets(node_rank)+offsets(node_rank)
        allocate(locals(offsets(node_rank)))
        locals = i_ptr(start0:end0)
        call MPI_AllGATHERV(locals, offsets(node_rank),    MPI_INTEGER, &
                            i_ptr,  offsets,   g_offsets,  MPI_INTEGER, &
                            this%containing_server%NodeRoot_Comm,status)
        deallocate(locals)

      endif 

      call shmem_reference%fence()

      deallocate(g_offsets,offsets)

   end function allocate_and_read

   subroutine handle_AddCollection(this, message)
      class (ServerThread), target, intent(inout) :: this
      type (AddCollectionMessage), intent(in) :: message

      integer :: n
      logical :: found
      type (CollectionVectorIterator) :: iter
      type (ExtDataCollection), pointer :: collection
      type (ExtDataCollection) :: c
      class(AbstractSocket),pointer :: connection

      iter = this%collections%begin()
      n = 1

      ! Is it a new collection?
      found = .false.
      do while (iter /= this%collections%end())
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
         call this%collections%push_back(c)
      end if

      connection=>this%get_connection()      
      call connection%send(CollectionIdMessage(n))         
      
   end subroutine handle_AddCollection

   subroutine handle_RequestData(this, message)
      use, intrinsic :: iso_fortran_env, only: REAL32
      class (ServerThread), intent(inout) :: this
      type (RequestDataMessage), intent(in) :: message
      class(AbstractSocket),pointer :: connection

      connection=>this%get_connection()
      call connection%send(RequestIdMessage(message%request_id))
      call this%request_backlog%push_back(message)

   end subroutine handle_RequestData

   subroutine handle_CollectiveRequestData(this, message)
      class (ServerThread), intent(inout) :: this
      type (CollectiveRequestDataMessage), intent(in) :: message
      class(AbstractSocket),pointer :: connection

      connection=>this%get_connection()
      call connection%send(RequestIdMessage(message%request_id))
      call this%request_backlog%push_back(message)

   end subroutine handle_CollectiveRequestData

   subroutine handle_WaitRequestData(this, message)
      class (ServerThread), target, intent(inout) :: this
      type (WaitRequestDataMessage), intent(in) :: message

      class (AbstractRequestHandle), pointer :: handle

      handle => this%get_RequestHandle(message%request_id)
      call handle%wait()
      call handle%data_reference%deallocate()
      call this%erase_RequestHandle(message%request_id)

   end subroutine handle_WaitRequestData
   
   subroutine set_terminate(this)
      class (ServerThread), intent(inout) :: this
      this%terminate = .true.
   end subroutine set_terminate

   subroutine clear_terminate(this)
      class (ServerThread), intent(inout) :: this
      this%terminate = .false.
   end subroutine clear_terminate

   logical function do_terminate(this)
      class (ServerThread), intent(in) :: this
      do_terminate = this%terminate
   end function do_terminate

   subroutine get_DataFromFile(this,message,address)
      class (ServerThread), intent(inout)    :: this
      class (RequestDataMessage), intent(in) ::  message
      type (c_ptr), intent(in) :: address

      type (NetCDF4_FileFormatter), pointer :: formatter

      integer(kind=INT32), pointer :: values_int32_0d
      integer(kind=INT32), pointer :: values_int32_1d(:)
      real(kind=REAL32), pointer :: values_real32_0d
      real(kind=REAL32), pointer :: values_real32_1d(:)

      type (ExtDataCollection), pointer :: collection

      integer, allocatable :: start(:),count(:)

      collection => this%collections%at(message%collection_id)
      formatter => collection%find(message%file_name)

      select type (message)
      type is (RequestDataMessage)
        start = message%start
        count = message%count
      type is (CollectiveRequestDataMessage)
        start = message%global_start
        count = message%global_count
      class default
        stop "wrong RequestDataMessage type"  
      end select

!      if (product(count) /= product(file_data_reference%shape)) stop "memory size not match"
      select case (size(count)) ! rank
      case (0)
          select case (message%type_kind)
          case (pFIO_INT32)
              !call c_f_pointer(file_data_reference%base_address, values_int32_0d)
              call c_f_pointer(address, values_int32_0d)
              call formatter%get_var(message%var_name, values_int32_0d)
          case (pFIO_REAL32)
              !call c_f_pointer(file_data_reference%base_address, values_real32_0d)
              call c_f_pointer(address, values_real32_0d)
              call formatter%get_var(message%var_name, values_real32_0d)
          end select
      case (1:)
          select case (message%type_kind)
          case (pFIO_INT32)
              call c_f_pointer(address, values_int32_1d, [product(count)])
              call formatter%get_var(message%var_name, values_int32_1d, start=start, count=count)
          case (pFIO_REAL32)
              call c_f_pointer(address, values_real32_1d, [product(count)])
              call formatter%get_var(message%var_name, values_real32_1d, start=start, count=count)
          end select
       end select
   end subroutine get_DataFromFile

   function IamReading(this,id) result (yes)
      class(ServerThread),intent(in) :: this
      integer, intent(in) :: id
      integer :: node_rank,innode_rank
      logical :: yes

      call this%distribute_task(id, node_rank,innode_rank)
      yes = (node_rank   == this%containing_server%Node_Rank) .and. &
            (innode_rank == this%containing_server%InNode_Rank)
   end function IamReading

   ! distribute the task (id) to a specific process (node_rank, innode_rank)
   subroutine distribute_task(this,id, node_rank, innode_rank)
      class(ServerThread),intent(in) :: this
      integer, intent(in)   :: id
      integer,intent(out) :: node_rank,innode_rank
      integer :: rank

      innode_rank = mod(id, this%containing_server%InNode_npes)
      rank        = mod(id, this%containing_server%npes)
      node_rank   = this%containing_server%Node_Ranks(rank)

   end subroutine distribute_task

end module pFIO_ServerThreadMod
