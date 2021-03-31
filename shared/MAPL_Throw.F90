module MAPL_ThrowMod
   implicit none
   private

   public :: MAPL_throw_exception
   public :: MAPL_set_throw_method

   abstract interface
      subroutine throw(filename, line_number, message)
         character(len=*), intent(in) :: filename
         integer, intent(in) :: line_number
         character(len=*), optional, intent(in) :: message
      end subroutine throw
   end interface

   procedure (throw), pointer :: throw_method => null()
   logical, save :: initialized = .false.

contains

   subroutine MAPL_set_throw_method(method)
      procedure (throw) :: method
      if (.not. initialized) call initialize()
      throw_method => method
   end subroutine MAPL_set_throw_method

   subroutine initialize()
      throw_method => MAPL_Fail
      initialized = .true.
   end subroutine initialize

   
   subroutine MAPL_throw_exception(filename, line_number, message)
      character(len=*), intent(in) :: filename
      integer, intent(in) :: line_number
      character(len=*), optional, intent(in) :: message

      if (.not. initialized) then
         call initialize()
      end if

      call throw_method(filename, line_number, message=message)
      
   end subroutine MAPL_throw_exception


   subroutine MAPL_Fail(filename, line, message)
      use MPI
      use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
      character(*), intent(in) :: filename
      integer, intent(in) :: line
      character(*), optional, intent(in) :: message

      integer, parameter :: FIELD_WIDTH=40
      character(FIELD_WIDTH) :: use_name
      character(3) :: prefix
      character(:), allocatable :: base_name
      
      integer :: rank, ierror
      logical :: is_mpi_initialized

      call MPI_Initialized(is_mpi_initialized,ierror)
  
      base_name = get_base_name(filename)
      if (len(base_name) > FIELD_WIDTH) then
         prefix = '...'
         use_name = base_name(2:)
      else
         prefix = '   '
         use_name = base_name
      end if

      ! Could use ADVANCE='no', but this may increase the chance
      ! that the output lines are not interrupted by messages from
      ! other PEs
      if (is_mpi_initialized) then
         call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)
         write(ERROR_UNIT,'(a,i5.5,1x,a,i5.5,1x,a3,a40,1x,a)') &
              & 'pe=', rank, 'FAIL at line=', line, prefix, use_name, &
              & '<'//adjustl(trim(message))//'>'
      else
         write(ERROR_UNIT,'(a,i5.5,1x,a3,a40,1x,a)') &
              & 'FAIL at line=', line, prefix, use_name, &
              & '<'//adjustl(trim(message))//'>'
      end if


      
   end subroutine MAPL_Fail



   ! TODO: Rather than taking the last N characters, it might make
   ! more sense for the following procedure to omit just the middle
   ! characters in a long string.  The trick is to intelligently split on
   ! directories.    Maybe specify a max depth at both ends?
   function get_short_name(filename, maxlen) result(short_name)
      character(:), allocatable :: short_name
      character(*), intent(in) :: filename
      integer, optional, intent(in) :: maxlen
      integer, parameter :: MAX_LEN_SHORT_NAME = 60

      integer :: maxlen_
      integer :: n

      maxlen_ = MAX_LEN_SHORT_NAME
      if (present(maxlen)) maxlen_ = maxlen
      
      n = len_trim(filename)
      short_name = filename(max(1,n+1-maxlen_):)

   end function get_short_name


   function get_base_name(filename) result(base_name)
      character(:), allocatable :: base_name
      character(*), intent(in) :: filename

      integer :: idx

      idx = scan(filename, '/', back=.true.)

      base_name = filename(idx+1:)

   end function get_base_name



end module MAPL_ThrowMod
