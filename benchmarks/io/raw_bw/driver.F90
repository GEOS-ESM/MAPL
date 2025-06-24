#define I_AM_MAIN
#include "MAPL_ErrLog.h"
program main
   use mapl_BW_BenchmarkSpec
   use mapl_BW_Benchmark
   use mapl_ErrorHandlingMod
   use mpi
   use, intrinsic :: iso_fortran_env, only: INT64
   implicit none

   type(BW_BenchmarkSpec) :: spec
   integer :: status
      
   call mpi_init(status)
   _verify(status)
   spec = make_BW_BenchmarkSpec() ! CLI

   call run(spec, _rc)

   call MPI_Barrier(MPI_COMM_WORLD, status)
   _verify(status)
   call mpi_finalize(status)
   stop


contains


#undef I_AM_MAIN
#include "MAPL_ErrLog.h"
   subroutine run(spec, rc)
      type(BW_BenchmarkSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: status

      real :: tot_time
      real :: tot_time_sq
      real :: avg_time
      real :: std_time
      type(BW_Benchmark) :: benchmark
      integer :: writer_comm
      integer :: gather_comm
      integer :: i
      real :: t

      integer :: color, rank, npes
      call MPI_Comm_rank(MPI_COMM_WORLD, rank, status)
      _verify(status)
      call MPI_Comm_size(MPI_COMM_WORLD, npes, status)
      _verify(status)

      color = (rank*spec%n_writers) / npes
      call MPI_Comm_split(MPI_COMM_WORLD, color, 0, gather_comm, status)
      _verify(status)
      
      call MPI_Comm_rank(gather_comm, rank, status)
      _verify(status)
      call MPI_Comm_split(MPI_COMM_WORLD, rank, 0, writer_comm, status)
      _verify(status)
      if (rank /= 0) writer_comm = MPI_COMM_NULL
      _return_if(writer_comm == MPI_COMM_NULL)

      benchmark = make_BW_Benchmark(spec, writer_comm, _rc)

      call write_header(writer_comm, spec%file_type, _rc)

      tot_time = 0
      tot_time_sq = 0
      associate (n => spec%n_tries)
        do i = 1, n
           t = time(benchmark, writer_comm, _rc)
           tot_time = tot_time + t
           tot_time_sq = tot_time_sq + t**2
        end do
        avg_time = tot_time / n

        std_time = -1 ! unless
        if (n > 1) then
           std_time = sqrt((tot_time_sq - spec%n_tries*avg_time**2)/(n-1))
        end if
      end associate

      call report(spec, avg_time, std_time, writer_comm, _rc)

      _return(_success)
   end subroutine run


   real function time(benchmark, comm, rc)
      type(BW_Benchmark), intent(in) :: benchmark
      integer, intent(in) :: comm
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: rank
      integer(kind=INT64) :: c0, c1, count_rate

      call MPI_Barrier(comm, status)
      _verify(status)

      call system_clock(c0)
      call benchmark%run(_rc)
      call MPI_Barrier(comm, status)
      _verify(status)
      call system_clock(c1, count_rate=count_rate)

      time = real(c1-c0)/count_rate

      _return(_success)
   end function time


   subroutine write_header(comm, file_type, rc)
      integer, intent(in) :: comm
      character(len=*), intent(in) :: file_type
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: rank

      call MPI_Comm_rank(comm, rank, status)
      _verify(status)
      _return_unless(rank == 0)

      write(*,*)'Tested with file type: '//trim(file_type)
      write(*,'(3(a10,","),6(a15,:,","))',iostat=status) &
           'NX', '# levs', '# writers', 'write (GB)', 'packet (GB)', &
           'Time (s)', 'Eff. BW (GB/s)', 'Avg. BW (GB/s)', 'Rel. Std. Dev.'

      _return(status)
   end subroutine write_header


   subroutine report(spec, avg_time, std_time, comm, rc)
      type(BW_BenchmarkSpec), intent(in) :: spec
      real, intent(in) :: avg_time
      real, intent(in) :: std_time
      integer, intent(in) :: comm
      integer, optional, intent(out) :: rc

      integer :: status
      real :: packet_gb
      real :: total_gb
      real :: bw
      integer :: npes
      integer :: rank
      integer, parameter :: WORD_SIZE = 4
      integer(kind=INT64) :: packet_size

      call MPI_Comm_size(comm, npes, status)
      _verify(status)
      call MPI_Comm_rank(comm, rank, status)
      _verify(status)
      _return_unless(rank == 0)

      packet_size = int(spec%nx,kind=INT64)**2 * 6 * spec%n_levs / spec%n_writers
      packet_gb = 1.e-9*(WORD_SIZE * packet_size)
      total_gb = packet_gb * npes
      bw = total_gb / avg_time

      call MPI_Comm_size(comm, npes, status)
      _verify(status)

      write(*,'(3(1x,i9.0,","),6(f15.4,:,","))') &
           spec%nx, spec%n_levs, spec%n_writers, &
           total_gb, packet_gb, avg_time, bw, bw/npes, std_time/avg_time

      _return(_success)
   end subroutine report


end program main
   
