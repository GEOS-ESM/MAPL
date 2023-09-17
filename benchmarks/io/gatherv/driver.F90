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
      
   call mpi_init(_IERROR)
   spec = make_GathervSpec() ! CLI

   call run(spec, _RC)

   call MPI_Barrier(MPI_COMM_WORLD, _IERROR)
   call mpi_finalize(_IERROR)

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
      integer :: i
      real :: t

      writer_comm = split_comm(MPI_COMM_WORLD, spec%n_writers, _RC)
      _RETURN_IF(writer_comm == MPI_COMM_NULL)

      kernel = make_GathervKernel(spec, writer_comm, _RC)

      call write_header(writer_comm, _RC)

      tot_time = 0
      tot_time_sq = 0
      associate (n => spec%n_tries)
        do i = 1, n
           t = time(kernel, writer_comm, _RC)
           tot_time = tot_time + t
           tot_time_sq = tot_time_sq + t**2
        end do
        avg_time = tot_time / n

        rel_std_time = -1 ! unless
        if (n > 1) then
           rel_std_time = sqrt((tot_time_sq - spec%n_tries*avg_time**2)/(n-1))/avg_time
        end if
      end associate

      call report(spec, avg_time, rel_std_time, writer_comm, _RC)

      _RETURN(_SUCCESS)
   end subroutine run


   real function time(kernel, comm, rc)
      type(GathervKernel), intent(in) :: kernel
      integer, intent(in) :: comm
      integer, optional, intent(out) :: rc

      integer :: status
      real :: t0, t1

      call MPI_Barrier(comm, _IERROR)
      t0 = MPI_Wtime()
      call kernel%run(_RC)
      call MPI_Barrier(comm, _IERROR)
      t1 = MPI_Wtime()

      time = t1 - t0

      _RETURN(_SUCCESS)
   end function time


   ! Spread writers apart in rank rather than clump on any given node.
   function split_comm(comm_world, n_writers, rc) result(new_comm)
      integer :: new_comm
      integer, intent(in) :: comm_world
      integer, intent(in) :: n_writers
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: color
      integer :: team_size
      integer :: npes, rank

      call MPI_Comm_size(comm_world, npes, _IERROR)
      call MPI_Comm_rank(comm_world, rank, _IERROR)
      team_size = 1 + (npes-1)/n_writers
      color = mod(rank, team_size) + (rank/(team_size*n_writers))
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
      
      write(*,'(4(a6,","),2(a15,:,","))',iostat=status) 'NX', '# levs', '# writers', 'group size', 'Time (s)', 'Rel. Std. dev.'

      _RETURN(status)
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

      call MPI_Comm_rank(comm, rank, _IERROR)
      _RETURN_UNLESS(rank == 0)

      call MPI_Comm_size(comm, npes, _IERROR)

      write(*,'(4(i6.0,","),2(f15.4,:,","))') spec%nx, spec%n_levs, spec%n_writers, npes, avg_time, rel_std_time
 
      _RETURN(_SUCCESS)
   end subroutine report


end program main
   
