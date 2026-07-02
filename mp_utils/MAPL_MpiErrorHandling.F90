! MPI-aware error and abort handlers for MAPL_ThrowMod and mapl_ErrorHandling.
! Call MAPL_initialize_error_handling() once after MPI_Init to replace the
! serial (error stop) defaults with MPI-aware implementations.
! Serial programs do not need to call this.
module mapl_MpiErrorHandling_mod
   use mapl_Throw_mod,       only: MAPL_set_throw_method
   use mapl_ErrorHandling_mod,  only: MAPL_set_abort_handler
   implicit none
   private

   public :: MAPL_initialize_error_handling

contains

   subroutine MAPL_initialize_error_handling()
      call MAPL_set_throw_method(MAPL_mpi_fail)
      call MAPL_set_abort_handler(MAPL_mpi_abort)
   end subroutine MAPL_initialize_error_handling


   ! MPI-aware throw: prints PE rank then aborts via MPI_Abort
   subroutine MAPL_mpi_fail(filename, line, message)
      use MPI
      use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
      character(*), intent(in) :: filename
      integer, intent(in) :: line
      character(*), optional, intent(in) :: message

      integer, parameter :: FIELD_WIDTH = 40
      character(FIELD_WIDTH) :: use_name
      character(3) :: prefix
      character(:), allocatable :: base_name

      integer :: rank, ierror

      base_name = get_base_name(filename)
      if (len(base_name) > FIELD_WIDTH) then
         prefix = '...'
         use_name = base_name(2:)
      else
         prefix = '   '
         use_name = base_name
      end if

      call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)
      !$omp critical (MAPL_MpiError1)
      write(ERROR_UNIT,'(a,i5.5,1x,a,i5.5,1x,a3,a40,1x,a)') &
           & 'pe=', rank, 'FAIL at line=', line, prefix, use_name, &
           & '<'//adjustl(trim(message))//'>'
      !$omp end critical (MAPL_MpiError1)

   end subroutine MAPL_mpi_fail


   ! MPI-aware abort: calls MPI_Abort on MPI_COMM_WORLD
   subroutine MAPL_mpi_abort()
      use MPI
      integer :: ierror
      call MPI_Abort(MPI_COMM_WORLD, -1, ierror)
   end subroutine MAPL_mpi_abort


   function get_base_name(filename) result(base_name)
      character(:), allocatable :: base_name
      character(*), intent(in) :: filename

      integer :: idx, idx2

      idx = scan(filename, '/', back=.true.)
      if (idx /= 0) then
         idx2 = scan(filename(:idx-1), '/', back=.true.)
      else
         idx2 = idx
      end if

      base_name = filename(idx2+1:)

   end function get_base_name

end module mapl_MpiErrorHandling_mod
