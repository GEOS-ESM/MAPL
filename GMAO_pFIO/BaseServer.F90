#include "pFIO_ErrLog.h"
#include "unused_dummy.H"

module pFIO_BaseServerMod
   use, intrinsic :: iso_c_binding, only: c_ptr
   use, intrinsic :: iso_c_binding, only: C_NULL_PTR
   use, intrinsic :: iso_c_binding, only: c_loc
   use, intrinsic :: iso_fortran_env, only: REAL32, INT32, INT64, REAL64
   use, intrinsic :: iso_c_binding, only: c_f_pointer
   use pFIO_ErrorHandlingMod
   use pFIO_UtilitiesMod, only: word_size, i_to_string
   use pFIO_ConstantsMod
   use pFIO_ServerThreadMod
   use pFIO_ServerThreadVectorMod
   use pFIO_AbstractSocketMod
   use pFIO_AbstractSocketVectorMod
   use pFIO_AbstractServerMod
   use pFIO_StringInt64MapMod
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
      procedure :: add_connection
      procedure :: clear_RequestHandle
      procedure :: get_dmessage ! get done or dummy message

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

     integer :: i, n
     type (ServerThread),pointer :: threadPtr
     class (AbstractMessage),pointer :: msg
     type (MessageVectorIterator) :: iter
     type (StringInt64MapIterator) :: request_iter
     integer,pointer :: i_ptr(:)
     character(len=:), allocatable :: key
     type(c_ptr) :: offset_address
     integer :: f_id,ierr, collection_counter
     class (AbstractDataReference), pointer :: dataRefPtr
     class (RDMAReference), pointer :: remotePtr
     integer(kind=MPI_ADDRESS_KIND) :: offset, msize
 
     real(KIND=REAL64) :: t0, t1

     t0 = 0.0d0
     t1 = -1.0d0
     do n= 1, this%threads%size()
        threadPtr=>this%threads%at(n)
        if( n == 1) then ! only need to check one thread
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
                   ! (2) read data
                    call threadPtr%put_DataToFile(q,offset_address)
                   ! (3) leave a mark, it has been written
                    call this%stage_offset%insert(i_to_string(q%request_id)//'done',0_MPI_ADDRESS_KIND)
                    !t1 = mpi_wtime()
                 endif ! rank = mem_rank
              end select
              call iter%next()
           enddo ! do backlog loop
        endif ! first thread n==1 
        call threadPtr%clear_backlog()
        call threadPtr%clear_hist_collections()
     enddo ! threads
     !if( t1-t0 > 0) then
     !   print*, "this rank",this%rank,"spending ", t1-t0, " seconds writing"
     !endif
     _RETURN(_SUCCESS)
   end subroutine put_DataToFile

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

end module pFIO_BaseServerMod
