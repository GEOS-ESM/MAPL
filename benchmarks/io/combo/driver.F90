#define I_AM_MAIN
#include "MAPL_ErrLog.h"
program main
   use mapl_ComboSpec
   use mapl_GathervKernel
   use mapl_BW_Benchmark
   use mapl_ErrorHandlingMod
   use Kernel_mod
   use mpi
   implicit none

   type(ComboSpec) :: spec
   integer :: status
      
   call mpi_init(status)
   _VERIFY(status)
   spec = make_ComboSpec() ! CLI

   call run(spec, _RC)

   call MPI_Barrier(MPI_COMM_WORLD, status)
   _VERIFY(status)
   call mpi_finalize(status)

   stop

contains


#undef I_AM_MAIN
#include "MAPL_ErrLog.h"
   subroutine run(spec, rc)
      type(ComboSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: status

      real :: tot_time
      real :: tot_time_write
      real :: tot_time_gather
      real :: avg_time
      real :: avg_time_write
      real :: avg_time_gather
      type(GathervKernel) :: kernel
      type(BW_Benchmark) :: benchmark
      integer :: writer_comm
      integer :: gather_comm
      integer :: i
      real :: ta, tb

      integer :: color, rank, npes
      call MPI_Comm_rank(MPI_COMM_WORLD, rank, status)
      _VERIFY(status)
      call MPI_Comm_size(MPI_COMM_WORLD, npes, status)
      _VERIFY(status)

      color = (rank*spec%n_writers) / npes
      call MPI_Comm_split(MPI_COMM_WORLD, color, 0, gather_comm, status)
      _VERIFY(status)
      
      call MPI_Comm_rank(gather_comm, rank, status)
      _VERIFY(status)
      call MPI_Comm_split(MPI_COMM_WORLD, rank, 0, writer_comm, status)
      _VERIFY(status)
      if (rank /= 0) then
         writer_comm = MPI_COMM_NULL
      end if

      kernel = make_GathervKernel(spec, gather_comm, _RC)
      if (rank == 0) then
         benchmark = make_BW_Benchmark(spec, writer_comm, _RC)
      end if

      call write_header(MPI_COMM_WORLD, _RC)

      tot_time = 0
      tot_time_gather = 0
      tot_time_write = 0
      associate (n => spec%n_tries)
        do i = 1, n
           ta = time(kernel, gather_comm, _RC)
           if (writer_comm /= MPI_COMM_NULL) then
              tb = time(benchmark, writer_comm, _RC)
           end if
           tot_time_gather = tot_time_gather + ta
           tot_time_write = tot_time_write + tb
           tot_time = tot_time + ta + tb
        end do
        avg_time = tot_time / n
        avg_time_gather = tot_time_gather / n
        avg_time_write = tot_time_write / n

      end associate

      call report(spec, avg_time, avg_time_gather, avg_time_write, MPI_COMM_WORLD, _RC)

      _RETURN(_SUCCESS)
   end subroutine run


   real function time(kernel, comm, rc)
      class(Kernel_T), intent(in) :: kernel
      integer, intent(in) :: comm
      integer, optional, intent(out) :: rc

      integer :: status
      real :: t0, t1

      call MPI_Barrier(comm, status)
      _VERIFY(status)
      t0 = MPI_Wtime()
      call kernel%run(_RC)
      call MPI_Barrier(comm, status)
      _VERIFY(status)
      t1 = MPI_Wtime()

      time = t1 - t0

      _RETURN(_SUCCESS)
   end function time

   subroutine write_header(comm, rc)
      integer, intent(in) :: comm
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: rank

      call MPI_Comm_rank(comm, rank, status)
      _VERIFY(status)
      _RETURN_UNLESS(rank == 0)
      
      write(*,'(4(a6,","),4(a15,:,","))',iostat=status) 'NX', '# levs', '# writers', 'group size', 'Time (s)', 'G Time (s)', 'W Time (s)', 'BW (GB/sec)'

      _RETURN(status)
   end subroutine write_header


   subroutine report(spec, avg_time, avg_time_gather, avg_time_write, comm, rc)
      type(ComboSpec), intent(in) :: spec
      real, intent(in) :: avg_time
      real, intent(in) :: avg_time_gather
      real, intent(in) :: avg_time_write

      integer, intent(in) :: comm
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: rank
      integer :: npes
      integer :: group
      real :: bw_gb
      integer, parameter :: WORD=4

      call MPI_Comm_rank(comm, rank, status)
      _VERIFY(status)
      _RETURN_UNLESS(rank == 0)

      call MPI_Comm_size(MPI_COMM_WORLD, npes, status)
      _VERIFY(status)
      group = npes /spec%n_writers

      bw_gb = 1.e-9 * WORD * (spec%nx**2)*6*spec%n_levs / avg_time
      write(*,'(4(i6.0,","),4(f15.4,:,","))') spec%nx, spec%n_levs, spec%n_writers, group, avg_time, avg_time_gather, avg_time_write, bw_gb
 
      _RETURN(_SUCCESS)
   end subroutine report


end program main
   
