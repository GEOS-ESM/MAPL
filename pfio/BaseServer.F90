#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_BaseServerMod
   use, intrinsic :: iso_c_binding, only: c_ptr
   use, intrinsic :: iso_c_binding, only: C_NULL_PTR
   use, intrinsic :: iso_c_binding, only: c_loc
   use, intrinsic :: iso_fortran_env, only: REAL32, INT32, INT64, REAL64
   use, intrinsic :: iso_c_binding, only: c_f_pointer
   use MAPL_ExceptionHandling
   use pFIO_UtilitiesMod, only: word_size, i_to_string
   use pFIO_ConstantsMod
   use pFIO_ServerThreadMod
   use pFIO_ServerThreadVectorMod
   use pFIO_AbstractSocketMod
   use pFIO_AbstractSocketVectorMod
   use pFIO_AbstractServerMod
   use gFTL_StringInteger64Map
   use pFIO_AbstractMessageMod
   use pFIO_AbstractDataReferenceMod
   use pFIO_ShmemReferenceMod
   use pFIO_RDMAReferenceMod
   use pFIO_AbstractDataMessageMod
   use pFIO_AbstractSocketMod
   use pFIO_MpiSocketMod
   use pFIO_SimpleSocketMod
   use pFIO_MessageVectorMod
   use pFIO_MessageVectorUtilMod
   use pFIO_DummyMessageMod
   use pFIO_DoneMessageMod
   use pFIO_CollectiveStageDataMessageMod
   use mpi
!   use pfio_base

   implicit none
   private

   public :: BaseServer

   type,extends (AbstractServer), abstract :: BaseServer
      type (ServerThreadVector) :: threads
   contains

      procedure :: receive_output_data
      procedure :: put_DataToFile
      procedure :: get_DataFromMem
      procedure :: add_connection
      procedure :: clear_RequestHandle
      procedure :: get_dmessage ! get done or dummy message
      procedure :: set_collective_request ! 
      procedure :: create_remote_win
   end type BaseServer

contains

   subroutine receive_output_data(this, rc)
     class (BaseServer),target, intent(inout) :: this
     integer, optional, intent(out) :: rc

     integer :: i, client_num, status
     type (ServerThread),pointer :: threadPtr
     class (AbstractDataReference), pointer :: dataRefPtr

     client_num = this%threads%size()

     do i = 1, client_num
         threadPtr=>this%threads%at(i)
         ! receive output data and save them on Shmem_reference
         call threadPtr%receive_output_data(rc=status)
         _VERIFY(status)
     enddo

     do i = 1, this%dataRefPtrs%size()   
        dataRefPtr => this%get_dataReference(i)
        call dataRefPtr%fence(rc=status)
         _VERIFY(status)
     enddo

     _RETURN(_SUCCESS)
   end subroutine receive_output_data

   subroutine put_DataToFile(this, rc)
     class (BaseServer),target, intent(inout) :: this
     integer, optional, intent(out) :: rc

     integer ::  n
     type (ServerThread),pointer :: threadPtr
     class (AbstractMessage),pointer :: msg
     type (MessageVectorIterator) :: iter
     type (StringInteger64MapIterator) :: request_iter
     integer,pointer :: i_ptr(:)
     type(c_ptr) :: offset_address
     integer :: collection_counter
     class (AbstractDataReference), pointer :: dataRefPtr
     class (RDMAReference), pointer :: remotePtr
     integer(kind=MPI_ADDRESS_KIND) :: offset, msize
     integer :: num_clients 
     !real(KIND=REAL64) :: t0, t1

     !t0 = 0.0d0
     !t1 = -1.0d0
     num_clients = this%threads%size()
     if (num_clients == 0) then
        _RETURN(_SUCCESS)
     endif

     threadPtr=>this%threads%at(1)

     iter = threadPtr%request_backlog%begin()
     ! t0 = mpi_wtime()         
     do while (iter /= threadPtr%request_backlog%end())
        msg => iter%get()
        select type (q=>msg)
        type is (CollectiveStageDataMessage)
           collection_counter = this%stage_offset%of(i_to_string(q%collection_id))
           dataRefPtr => this%get_dataReference(collection_counter)
           msize  = this%stage_offset%of(i_to_string(MSIZE_ID+collection_counter))
           call c_f_pointer(dataRefPtr%base_address,i_ptr,shape=[msize])

           select type(dataRefPtr)
           type is (RDMAReference)
              remotePtr=>dataRefPtr
           class default
              _ASSERT(.false., "remote is a must")
           end select

           request_iter = this%stage_offset%find(i_to_string(q%request_id)//'done')
           if (request_iter == this%stage_offset%end() .and. this%rank == remotePtr%mem_rank ) then ! not read yet
             !print*, this%rank , " is wrinting this collection ", collection_counter
             ! (1) get address where data should put
              offset     = this%stage_offset%at(i_to_string(q%request_id))
              offset_address   = c_loc(i_ptr(offset+1))
             ! (2) write data
              call threadPtr%put_DataToFile(q,offset_address)
             ! (3) leave a mark, it has been written
              call this%stage_offset%insert(i_to_string(q%request_id)//'done',0_MPI_ADDRESS_KIND)
              !t1 = mpi_wtime()
           endif ! rank = mem_rank
        end select
        call iter%next()
     enddo ! do backlog loop

     do n = 1, num_clients
        threadPtr=>this%threads%at(n)
        call threadPtr%clear_backlog()
        call threadPtr%clear_hist_collections()
        call threadPtr%clear_subarray()
     enddo ! threads
     !if( t1-t0 > 0) then
     !   print*, "this rank",this%rank,"spending ", t1-t0, " seconds writing"
     !endif
     _RETURN(_SUCCESS)
   end subroutine put_DataToFile

   subroutine get_DataFromMem(this, multi, rc)
     class (BaseServer),target, intent(inout) :: this
     logical, intent(in) :: multi
     integer, optional, intent(out) :: rc

     integer :: i, client_num, status
     type (ServerThread),pointer :: threadPtr

     client_num = this%threads%size()

     do i = 1, client_num
         threadPtr=>this%threads%at(i)
         ! get data from Shmem_reference and send it back to app
         call threadPtr%get_DataFromMem(multi, rc=status)
         _VERIFY(status)
     enddo

     _RETURN(_SUCCESS)
   end subroutine get_DataFromMem

   subroutine add_connection(this, socket)
      class (BaseServer), target, intent(inout) :: this
      class (AbstractSocket), intent(in) :: socket

      class(ServerThread), pointer :: thread_ptr
      integer :: k

      allocate(thread_ptr, source=ServerThread(socket, this))
      k = this%threads%size() + 1
      call thread_ptr%set_rank(k)
      call this%threads%push_Back(thread_ptr)
      nullify(thread_ptr)

      this%num_clients = this%num_clients + 1
      
   end subroutine add_connection

   ! done message of dummy message

   function get_dmessage(this, rc) result(dmessage)
      class (BaseServer), target, intent(in) :: this
      integer, optional, intent(out) :: rc

      class(AbstractMessage), pointer :: dmessage
      class(ServerThread), pointer :: thread_ptr
      integer :: n

      n = this%threads%size()
      if (n <= 0 ) then
         allocate(dmessage,source = DummyMessage())
         print*, "WARNING: no serverthread" 
         _RETURN(_SUCCESS)
      endif

      thread_ptr=>this%threads%at(1)
      select type (socket => thread_ptr%get_connection())
      type is (SimpleSocket)
         allocate(dmessage,source = DoneMessage())
      type is (MpiSocket)
         allocate(dmessage,source = DummyMessage())
      class default
         _ASSERT(.false., "wrong socket type")
      end select

      _RETURN(_SUCCESS)
   end function

   subroutine clear_RequestHandle(this)
      class (BaseServer), target, intent(inout) :: this
      class(ServerThread), pointer :: thread_ptr
      integer :: i,n

      n = this%threads%size()

      do i = 1, n
         thread_ptr=>this%threads%at(i)
         call thread_ptr%clear_RequestHandle()
      enddo

   end subroutine clear_RequestHandle

   subroutine set_collective_request(this, request, have_done)
      class (BaseServer), target, intent(inout) :: this
      logical, intent(in) :: request, have_done

      class(ServerThread), pointer :: thread_ptr
      integer :: i,n

      n = this%threads%size()

      do i = 1, n
         thread_ptr=>this%threads%at(i)
         call thread_ptr%set_collective_request(request, have_done)
      enddo

   end subroutine set_collective_request

   subroutine create_remote_win(this, rc)
      class (BaseServer), target, intent(inout) :: this
      integer, optional, intent(out) :: rc
      class (AbstractDataReference), pointer :: remotePtr
      integer :: rank
      integer(KIND=INT64) :: offset, msize_word
      integer(KIND=INT64),allocatable :: offsets(:), msize_words(:)
      type (MessageVectorIterator) :: iter
      type (StringInteger64MapIterator) :: request_iter
      class (AbstractMessage), pointer :: msg
      integer :: collection_counter, collection_total
      character(len=*),parameter :: Iam = 'create_remote_win'
      type (ServerThread) , pointer :: thread_ptr

      this%stage_offset = StringInteger64map()

      collection_counter = 0
      collection_total   = 0

      thread_ptr => this%threads%at(1)

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
      ! (3) allocate the memory
      msize_words = offsets
      do collection_counter = 1, collection_total
         rank = this%get_writing_PE(collection_counter)
         msize_word = msize_words(collection_counter)
         call this%stage_offset%insert(i_to_string(MSIZE_ID + collection_counter ),msize_word)
         allocate(remotePtr, source  = RDMAReference(pFIO_INT32,msize_word, this%comm, rank ))
         call this%add_DataReference(remotePtr)
         remotePtr=>null()
      enddo
      _RETURN(_SUCCESS)

   end subroutine create_remote_win

end module pFIO_BaseServerMod
