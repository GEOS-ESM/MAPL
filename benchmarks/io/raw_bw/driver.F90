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
      
   call mpi_init(_IERROR)
   spec = make_BW_BenchmarkSpec() ! CLI

   call run(spec, _RC)

   call MPI_Barrier(MPI_COMM_WORLD, _IERROR)
   call mpi_finalize(_IERROR)
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
      integer :: i
      real :: t

      writer_comm = split_comm(MPI_COMM_WORLD, spec%n_streams, _RC)
      _RETURN_IF(writer_comm == MPI_COMM_NULL)

      benchmark = make_BW_Benchmark(spec, writer_comm, _RC)

      call write_header(writer_comm, _RC)

      tot_time = 0
      tot_time_sq = 0
      associate (n => spec%n_tries)
        do i = 1, n
           t = time(benchmark, writer_comm, _RC)
           tot_time = tot_time + t
           tot_time_sq = tot_time_sq + t**2
        end do
        avg_time = tot_time / n

        std_time = -1 ! unless
        if (n > 1) then
           std_time = sqrt((tot_time_sq - spec%n_tries*avg_time**2)/(n-1))
        end if
      end associate

      call report(spec, avg_time, std_time, writer_comm, _RC)

      _RETURN(_SUCCESS)
   end subroutine run


   real function time(benchmark, comm, rc)
      type(BW_Benchmark), intent(in) :: benchmark
      integer, intent(in) :: comm
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: rank
      integer(kind=INT64) :: c0, c1, count_rate

      call MPI_Barrier(comm, _IERROR)

      call system_clock(c0)
      call benchmark%run(_RC)
      call MPI_Barrier(comm, _IERROR)
      call system_clock(c1, count_rate=count_rate)

      time = real(c1-c0)/count_rate

      _RETURN(_SUCCESS)
   end function time


   ! Spread writers apart in rank rather than clump on any given node.
   function split_comm(comm_world, n_streams, rc) result(new_comm)
      integer :: new_comm
      integer, intent(in) :: comm_world
      integer, intent(in) :: n_streams
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: color
      integer :: team_size
      integer :: npes, rank

      call MPI_Comm_size(comm_world, npes, _IERROR)
      call MPI_Comm_rank(comm_world, rank, _IERROR)
      team_size = 1 + (npes-1)/n_streams
      color = mod(rank, team_size) + (rank/(team_size*n_streams))
      if (color /= 0) color = MPI_UNDEFINED

      call MPI_Comm_split(comm_world, color, 0, new_comm, _IERROR)

      _RETURN(_SUCCESS)
   end function split_comm

   subroutine write_header(comm, rc)
      integer, intent(in) :: comm
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: rank

      call MPI_Comm_rank(comm, rank, _IERROR)
      _RETURN_UNLESS(rank == 0)

      write(*,'(4(a10,","),6(a15,:,","))',iostat=status) &
           'NX', '# levs', '# writers', '# packets', 'write (GB)', 'packet (GB)', &
           'Time (s)', 'Eff. BW (GB/s)', 'Avg. BW (GB/s)', 'Rel. Std. Dev.'

      _RETURN(status)
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

      call MPI_Comm_size(comm, npes, _IERROR)
      call MPI_Comm_rank(comm, rank, _IERROR)
      _RETURN_UNLESS(rank == 0)

      packet_size = int(spec%nx,kind=INT64)**2 * spec%n_levs / spec%n_packets / spec%n_streams
      packet_gb = 1.e-9*(WORD_SIZE * packet_size)
      total_gb = packet_gb * spec%n_packets * npes
      bw = total_gb / avg_time

      call MPI_Comm_size(comm, npes, _IERROR)

      write(*,'(4(1x,i9.0,","),6(f15.4,:,","))') &
           spec%nx, spec%n_levs, spec%n_streams, spec%n_packets, &
           total_gb, packet_gb, avg_time, bw, bw/npes, std_time/avg_time

      _RETURN(_SUCCESS)
   end subroutine report


end program main
   
