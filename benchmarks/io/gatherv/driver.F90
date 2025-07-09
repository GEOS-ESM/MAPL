#define I_AM_MAIN
#include "MAPL_ErrLog.h"
program main
   use mapl_GathervSpec
   use mapl_GathervKernel
   use mapl_ErrorHandlingMod
   use mpi
   implicit none

   type(GathervSpec) :: spec
   integer :: status
      
   call mpi_init(status)
   _verify(status)
   spec = make_GathervSpec() ! CLI

   call run(spec, _rc)

   call MPI_Barrier(MPI_COMM_WORLD, status)
   _verify(status)
   call mpi_finalize(status)

   stop

contains


#undef I_AM_MAIN
#include "MAPL_ErrLog.h"
   subroutine run(spec, rc)
      type(GathervSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: status

      real :: tot_time
      real :: tot_time_sq
      real :: avg_time
      real :: rel_std_time
      type(GathervKernel) :: kernel
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

      kernel = make_GathervKernel(spec, gather_comm, _rc)

      call write_header(MPI_COMM_WORLD, _rc)

      tot_time = 0
      tot_time_sq = 0
      associate (n => spec%n_tries)
        do i = 1, n
           t = time(kernel, writer_comm, _rc)
           tot_time = tot_time + t
           tot_time_sq = tot_time_sq + t**2
        end do
        avg_time = tot_time / n

        rel_std_time = -1 ! unless
        if (n > 1) then
           rel_std_time = sqrt((tot_time_sq - spec%n_tries*avg_time**2)/(n-1))/avg_time
        end if
      end associate

      call report(spec, avg_time, rel_std_time, MPI_COMM_WORLD, _rc)

      _return(_success)
   end subroutine run


   real function time(kernel, comm, rc)
      type(GathervKernel), intent(in) :: kernel
      integer, intent(in) :: comm
      integer, optional, intent(out) :: rc

      integer :: status
      real :: t0, t1

      call MPI_Barrier(comm, status)
      _verify(status)
      t0 = MPI_Wtime()
      call kernel%run(_rc)
      call MPI_Barrier(comm, status)
      _verify(status)
      t1 = MPI_Wtime()

      time = t1 - t0

      _return(_success)
   end function time

   subroutine write_header(comm, rc)
      integer, intent(in) :: comm
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: rank

      call MPI_Comm_rank(comm, rank, status)
      _verify(status)
      _return_unless(rank == 0)
      
      write(*,'(4(a6,","),3(a15,:,","))',iostat=status) 'NX', '# levs', '# writers', 'group size', 'Time (s)', 'Rel. Std. dev.', 'BW (GB/sec)'

      _return(status)
   end subroutine write_header


   subroutine report(spec, avg_time, rel_std_time, comm, rc)
      type(GathervSpec), intent(in) :: spec
      real, intent(in) :: avg_time
      real, intent(in) :: rel_std_time
      integer, intent(in) :: comm
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: rank
      integer :: npes
      integer :: group
      real :: bw_gb
      integer, parameter :: WORD=4

      call MPI_Comm_rank(comm, rank, status)
      _verify(status)
      _return_unless(rank == 0)

      call MPI_Comm_size(MPI_COMM_WORLD, npes, status)
      _verify(status)
      group = npes /spec%n_writers

      bw_gb = 1.e-9 * WORD * (spec%nx**2)*6*spec%n_levs / avg_time
      write(*,'(4(i6.0,","),3(f15.4,:,","))') spec%nx, spec%n_levs, spec%n_writers, group, avg_time, rel_std_time, bw_gb
 
      _return(_success)
   end subroutine report


end program main
   
