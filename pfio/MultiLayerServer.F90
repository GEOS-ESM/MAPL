#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_MultiLayerServerMod
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
   use pFIO_ShmemReferenceMod
   use pFIO_RDMAReferenceMod
   use pFIO_MessageVectorMod
   use pFIO_MessageVectorUtilMod
   use pFIO_ForwardDataMessageMod
   use pFIO_NetCDF4_FileFormatterMod
   use pFIO_HistoryCollectionMod
   use pFIO_HistoryCollectionVectorMod
   use pFIO_BaseServerMod
   use pFIO_AttributeMod
   use pFIO_StringAttributeMapMod
   use pFIO_StringAttributeMapUtilMod
   use mpi

   implicit none
   private

   public :: MultiLayerServer

   type,extends (BaseServer) :: MultiLayerServer
      character(len=:), allocatable :: port_name 
      integer :: nwriters
      integer :: Inter_Comm

   contains
      procedure :: start
      procedure :: terminate_writers
      procedure :: put_DataToFile
   end type MultiLayerServer

   interface MultiLayerServer
      module procedure new_MultiLayerServer
   end interface MultiLayerServer

contains

   function new_MultiLayerServer(comm, port_name, nwriters, pfio_writer, rc) result(s)
      type (MultiLayerServer) :: s
      integer, intent(in) :: comm
      character(*), intent(in) :: port_name
      integer, intent(in) :: nwriters
      character(*), intent(in) :: pfio_writer
      integer, optional, intent(out) :: rc
      integer :: ierror
 
      call s%init(comm)

      s%Inter_Comm = MPI_COMM_NULL
      s%nwriters = nwriters
      _ASSERT(s%nwriters > 1 ,' nwriters should be >=2. pfio_writer.x has captain-soldier structure')

      call MPI_Comm_spawn( pfio_writer , MPI_ARGV_NULL, s%nwriters, MPI_INFO_NULL, 0, &
                   s%comm, s%Inter_Comm, MPI_ERRCODES_IGNORE, ierror)

      s%port_name = trim(port_name)
      s%threads = ServerThreadVector()

   end function new_MultiLayerServer

   subroutine start(this)
      class (MultiLayerServer), target, intent(inout) :: this

      class (ServerThread), pointer :: thread_ptr => null()
      integer :: i,client_size
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
            call thread_ptr%run()
            !delete the thread object if it terminates 
            if(thread_ptr%do_terminate()) then
               mask(i) = .true.
            endif
         enddo

         if (all(mask)) exit

      enddo

      call this%threads%clear()
      call this%terminate_writers()
      deallocate(mask)

   end subroutine start

   subroutine terminate_writers(this)
      class (MultiLayerServer), intent(inout) :: this
      integer :: terminate = -1
      integer :: ierr
      integer :: MPI_STAT(MPI_STATUS_SIZE)
      ! The root rank sends termination signal to the root of the spawned children which would 
      ! send terminate back for synchronization
      ! if no syncrohization, the writer may be still writing while the main testing node is comparing
      if( this%rank == 0 .and. this%nwriters > 1 ) then
        call MPI_send(terminate, 1, MPI_INTEGER, 0, pFIO_s_tag, this%Inter_Comm, ierr)
        call MPI_recv(terminate, 1, MPI_INTEGER, 0, pFIO_s_tag, this%Inter_Comm, MPI_STAT, ierr)
      endif

   end subroutine terminate_writers

   subroutine put_DataToFile(this, rc)
     class (MultiLayerServer),target, intent(inout) :: this
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
     integer :: status
     type (ForwardDataMessage) :: forMSG
     type (MessageVector) :: forwardVec
     type (StringAttributeMap) :: forData
     type (StringAttributeMapIterator) :: fiter
     type (NetCDF4_FileFormatter),pointer :: formatter
     type (HistoryCollection),pointer :: hist_collection
     logical :: f_exist
     !real(KIND=REAL64) :: t0, t1
     !t0 = 0.0d0
     !t1 = -1.0d0

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
                   ! (1) get address where data should put
                    offset     = this%stage_offset%at(i_to_string(q%request_id))
                    offset_address   = c_loc(i_ptr(offset+1))
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                   !2) forward data to writer
                    forMSG = ForwardDataMessage(q%request_id, q%collection_id, q%file_name, q%var_name, &
                         q%type_kind, q%global_count, offset)
                    call forwardVec%push_back(forMSG)
                    fiter = forData%find(i_to_string(q%collection_id))
                    if ( fiter == forData%end()) then
                       call forData%insert(i_to_string(q%collection_id), Attribute(i_ptr(1:msize)))
                    endif

                    hist_collection=>threadPtr%get_hist_collection(q%collection_id)
                       ! create file with meta data
                    inquire(file=q%file_name, exist=f_exist)
                    if (.not. f_exist) then
                       allocate(formatter)
                       call formatter%create(trim(q%file_name),rc=status)
                       _VERIFY(status)
                       call formatter%write(hist_collection%fmd, rc=status)
                       _VERIFY(status)
                       call formatter%close()
                       deallocate(formatter)
                    endif
                   ! (3) leave a mark, it has been written
                    call this%stage_offset%insert(i_to_string(q%request_id)//'done',0_MPI_ADDRESS_KIND)
                    !t1 = mpi_wtime()
                 endif ! rank = mem_rank
              end select
              call iter%next()
           enddo ! do backlog loop

           if (.not. forwardVec%empty()) then
              call Forward_dataToWriter(forwardVec, forData, rc=status)
              call forwardVec%clear()
              call forData%clear()
           endif
           call MPI_Barrier(this%comm, status)
        endif ! first thread n==1 
        call threadPtr%clear_backlog()
        call threadPtr%clear_hist_collections()
        call threadPtr%clear_subarray()
     enddo ! threads
     !if( t1-t0 > 0) then
     !   print*, "this rank",this%rank,"spending ", t1-t0, " seconds writing"
     !endif
     _RETURN(_SUCCESS)
   contains 

      subroutine forward_DataToWriter(forwardVec, forwardData, rc)
         type (MessageVector), intent(in) ::      forwardVec
         type (StringAttributeMap), intent(in) :: forwardData
         integer, optional, intent(out) :: rc
   
         integer :: writer_rank, bsize, ksize, k, rank
         integer :: command, ierr, MPI_STAT(MPI_STATUS_SIZE)
         integer, allocatable :: buffer(:)
         integer :: status
         type (MessageVectorIterator) :: iter
   
   
         command = 1
         call serialize_message_vector(forwardVec,buffer)
         bsize = size(buffer)
   
         call MPI_send(command, 1, MPI_INTEGER, 0, pFIO_s_tag, this%Inter_Comm, ierr)
         call MPI_recv(writer_rank, 1, MPI_INTEGER, &
                   0, pFIO_s_tag, this%Inter_Comm , &
                   MPI_STAT, ierr)
         !forward Message 
         call MPI_send(bsize,  1,     MPI_INTEGER, writer_rank, pFIO_s_tag, this%Inter_Comm, ierr)
         call MPI_send(buffer, bsize, MPI_INTEGER, writer_rank, pFIO_s_tag, this%Inter_Comm, ierr)
         !send number of collections
         call StringAttributeMap_serialize(forwardData,buffer)
         bsize = size(buffer)
         call MPI_send(bsize,  1, MPI_INTEGER, writer_rank, pFIO_s_tag, this%Inter_Comm, ierr)
         call MPI_send(buffer, bsize, MPI_INTEGER, writer_rank, pFIO_s_tag, this%Inter_Comm, ierr)
         !2) send the data
   
         _RETURN(_SUCCESS)
      end subroutine forward_dataToWriter

   end subroutine put_DataToFile

end module pFIO_MultiLayerServerMod
