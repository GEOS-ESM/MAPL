#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

program main
   use, intrinsic :: iso_c_binding, only: c_ptr
   use, intrinsic :: iso_c_binding, only: c_loc
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64, INT32, INT64 
   use, intrinsic :: iso_c_binding, only: c_f_pointer
   use pFIO_ConstantsMod
   use pFIO_AbstractMessageMod
   use pFIO_MessageVectorMod
   use pFIO_MessageVectorUtilMod
   use pFIO_ForwardDataMessageMod
   use pFIO_AttributeMod
   use pFIO_StringAttributeMapMod
   use pFIO_StringAttributeMapUtilMod
   use pFIO_NetCDF4_FileFormatterMod
   use pFIO_StringNetCDF4_FileFormatterMapMod
   use MAPL_ExceptionHandling
   use pFIO_UtilitiesMod 
   use mpi 

   implicit none
   integer :: Inter_Comm
   integer :: ierr
   integer :: rank
   integer :: server_rank

   integer :: MPI_STAT(MPI_STATUS_SIZE)
   integer :: n_workers, i, idle, no_job, idle_worker
   integer :: command 
   integer, allocatable :: busy(:)
   integer :: msg_size,data_size, status
   integer, allocatable :: bufferm(:), bufferd(:)
   type(MessageVector)  :: forwardVec
   type(StringAttributeMap)   :: forwardData
   type (Attribute), pointer :: attr
   type (NetCDF4_FileFormatter), pointer :: formatter
   type (NetCDF4_FileFormatter) :: fm
   type (StringNetCDF4_FileFormatterMap) :: formatters
   type (StringNetCDF4_FileFormatterMapIterator) :: iter
   class (AbstractMessage), pointer :: msg
 
   call MPI_Init(ierr)
   call MPI_Comm_get_parent(Inter_Comm, ierr);
   call MPI_Comm_rank(MPI_COMM_WORLD,rank, ierr)
   call MPI_Comm_size(MPI_COMM_WORLD,n_workers, ierr)

   allocate(busy(n_workers-1), source =0)

   if ( rank == 0 ) then ! captain node is distributing work
      do while (.true.)
        
         ! 1) captain node is waiting command from server
         call MPI_recv( command, 1, MPI_INTEGER, &
                MPI_ANY_SOURCE, pFIO_s_tag, Inter_Comm, &
                MPI_STAT, ierr)
         server_rank = MPI_STAT(MPI_SOURCE)

         if (command == 1) then ! server is asking for a writing node 
  
            ! check idle woker
            idle_worker = 0
            do i = 1, n_workers -1
               if (busy(i) == 0) then
                  idle_worker = i
                  exit
               endif
            enddo

            ! if all workers are busy, wait for one
            if (idle_worker == 0) then 

               call MPI_recv( idle, 1, MPI_INTEGER, &
                   MPI_ANY_SOURCE, pFIO_w_m_tag , MPI_COMM_WORLD, &
                   MPI_STAT, ierr)
               idle_worker = idle
            endif

            ! tell server the idel_worker
            call MPI_send(idle_worker, 1, MPI_INTEGER, server_rank, pFIO_s_tag, Inter_Comm, ierr)
            busy(idle_worker) = 1
            ! tell the idle_worker which server has work
            call MPI_send(server_rank, 1, MPI_INTEGER, idle_worker, pFIO_m_w_tag, MPI_COMM_WORLD, ierr)

         else ! command /=1, notify the worker to quit and finalize
            no_job = -1
            do i = 1, n_workers -1
               if ( busy(i) == 0) then
                  call MPI_send(no_job, 1, MPI_INTEGER, i, pFIO_m_w_tag, MPI_COMM_WORLD, ierr)
               else
                  call MPI_recv( idle, 1, MPI_INTEGER, &
                       i, pFIO_w_m_tag, MPI_COMM_WORLD, &
                       MPI_STAT, ierr)
                   if (idle /= i ) stop ("idle should be i")
                   call MPI_send(no_job, 1, MPI_INTEGER, i, pFIO_m_w_tag, MPI_COMM_WORLD, ierr)
               endif  
            enddo  
            exit
         endif
      enddo

   else 

     do while (.true.)

        ! 1) get server_rank from captain
        call MPI_recv( server_rank, 1, MPI_INTEGER, &
               0, pFIO_m_w_tag, MPI_COMM_WORLD, &
               MPI_STAT, ierr)

        if (server_rank == -1 ) exit
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! do somthing with server ( should match with server)
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        call MPI_recv( msg_size, 1, MPI_INTEGER,    &
            server_rank, pFIO_s_tag, Inter_comm, &
               MPI_STAT, ierr)
        allocate(bufferm(msg_size))
        call MPI_recv( bufferm, msg_size, MPI_INTEGER, &
             server_rank, pFIO_s_tag, Inter_comm,   &
               MPI_STAT, ierr)

        call MPI_recv( data_size, 1, MPI_INTEGER,&
             server_rank, pFIO_s_tag, Inter_comm,     &
             MPI_STAT, ierr)

        allocate(bufferd(data_size))
        call MPI_recv( bufferd, data_size, MPI_INTEGER, &
             server_rank, pFIO_s_tag, Inter_comm,   &
               MPI_STAT, ierr)

        ! deserilize message and data
        call deserialize_message_vector(bufferm, forwardVec)
        call StringAttributeMap_deserialize(bufferd, forwardData)

        ! loop over message vectors and write the file
        do i = 1, forwardVec%size()
           msg => forwardVec%at(i)
           select type(q=>msg)
           type is (ForwardDataMessage)
              iter = formatters%find(trim(q%file_name))
              if (iter == formatters%end()) then
                 call fm%open(trim(q%file_name), pFIO_WRITE)
                 call formatters%insert( trim(q%file_name),fm)
              endif
              formatter => formatters%at(trim(q%file_name))
     
              attr => forwardData%at(i_to_string(q%collection_id))
              call write_data(q, formatter, attr)
           end select
        enddo

        ! cleanup formatters
        iter = formatters%begin()
        do while (iter /= formatters%end())
           formatter => iter%value()
           call formatter%close(rc=status)
           call formatters%erase(iter)
           iter = formatters%begin()
        enddo
        
        ! clean up mssage ves amd data map 
        call forwardVec%clear()
        call forwardData%clear()

        deallocate(bufferd, bufferm)
 
        ! telling captain, I am the soldier that is ready to have more work
        call MPI_send(rank, 1, MPI_INTEGER, 0, pFIO_w_m_tag, MPI_COMM_WORLD , ierr)

      enddo
   endif

   call MPI_Barrier(MPI_COMM_WORLD, ierr)

   if ( rank == 0) then
      ! send done message to server
      ! this serves the syncronization with oserver
      command = -1
      call MPI_send(command, 1, MPI_INTEGER, 0, pFIO_s_tag, Inter_Comm, ierr)
   endif
     
   call MPI_Finalize(ierr)

contains

   subroutine write_data(message, formatter, attr, rc)
      type (ForwardDataMessage), intent(in) :: message
      type (NetCDF4_FileFormatter), intent(in) :: formatter
      type (Attribute), intent(in) :: attr
      integer, optional, intent(out) :: rc


      integer(kind=INT32), pointer :: values_int32_0d
      integer(kind=INT32), pointer :: values_int32_1d(:)
      integer(kind=INT64), pointer :: values_int64_0d
      integer(kind=INT64), pointer :: values_int64_1d(:)
      real(kind=REAL32), pointer :: values_real32_0d
      real(kind=REAL32), pointer :: values_real32_1d(:)
      real(kind=REAL64), pointer :: values_real64_0d
      real(kind=REAL64), pointer :: values_real64_1d(:)

      integer, allocatable :: start(:),count(:)
      class(*), pointer :: i_ptr(:)
      type (c_ptr) :: address


      count = message%count
      start = count
      start = 1

      i_ptr=> attr%get_values()
      select type (i_ptr)
      type is (integer(INT32))
         address = c_loc(i_ptr(message%offset+1))
      end select

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
   end subroutine

end program
