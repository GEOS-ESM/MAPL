!usage
#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

program main
   use, intrinsic :: iso_fortran_env, only: REAL32
   use mpi
   use MAPL
   implicit none
   type (MAPL_FlapCapOptions) :: cap_options
   type(ServerManager) :: ioserver_manager   
   type(SplitCommunicator) :: split_comm
   integer :: client_comm,rank, npes, ierror, provided,required
   integer :: status
   type(ArrayReference) :: ref
   type(FileMetadata) :: fmd
   Type(Variable) :: T
   integer :: dim1, i, i1, i2, width, hist_id, stage_id, n, step
   real, allocatable :: x(:)

   cap_options = MAPL_FlapCapOptions(description = 'GEOS AGCM', &
                                     authors     = 'GMAO')
   call MPI_init(ierror)
   
   call ioserver_manager%initialize(MPI_COMM_WORLD, &
         application_size=cap_options%npes_model, &
         nodes_input_server=cap_options%nodes_input_server, &
         nodes_output_server=cap_options%nodes_output_server, &
         npes_input_server=cap_options%npes_input_server, &
         npes_output_server=cap_options%npes_output_server, &
         oserver_type=cap_options%oserver_type, &
         npes_backend_pernode=cap_options%npes_backend_pernode, &
         isolate_nodes = cap_options%isolate_nodes, &
         fast_oclient  = cap_options%fast_oclient, &
         rc=status)

   call ioserver_manager%get_splitcomm(split_comm)
   select case(split_comm%get_name())
   case('model')
      client_comm = split_comm%get_subcommunicator()
      call MPi_Comm_size(client_comm, npes, ierror)
      call MPi_Comm_rank(client_comm, rank, ierror)
      if (npes /= cap_options%npes_model) stop "sanity check failed"

block

      ! if there are multiple oserver, split it into large and small pool
      call o_clients%split_server_pools()


      ! define file meta data
      call fmd%add_dimension('first',dim1)
      ! define varaible
      T = Variable(type=pFIO_REAL32, dimensions='first')

      call fmd%add_variable("temperature", T)
    
      hist_id = o_clients%add_hist_collection(fmd)

      ! variable in model
      dim1 = 1000
      width = npes/dim1
      i1 = rank*width +1
      i2 = (rank+1)*width
      if (rank == npes -1) i2 = dim1
      allocate(x(i1:i2))
      do i = i1,i2
         x(i) = i
      enddo
      ref =  ArrayReference(x)

      ! model steping
      step = 1
      do n = 1, step
         !sleep(1) ! model is working
         call o_clients%set_optimal_server(nwriting=1)
         call o_clients%collective_stage_data(hist_id, 'test_out.nc4', "temperature", ref, &
                 & start=[i1], &
                 & global_start=[1],global_count=[dim1])

         call o_clients%done_collective_stage()
         call o_clients%post_wait()
      enddo

      deallocate(x)

end block

      call i_Clients%terminate()
      call o_Clients%terminate()
   end select
   call ioserver_manager%finalize()

   call MPI_finalize(ierror)

end program main
