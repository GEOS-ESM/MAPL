module pFIO_ErrorHandlingMod
   use pFIO_ThrowMod
   implicit none
   private

   public :: pFIO_Assert
   public :: pFIO_Verify
   public :: pFIO_Return
   include "mpif.h"
contains


   logical function pFIO_Assert(condition, message, filename, line, rc) result(fail)
      logical, intent(in) :: condition
      character(*), intent(in) :: message
      character(*), intent(in) :: filename
      integer, intent(in) :: line
      integer, optional, intent(out) :: rc ! Not present in MAIN

      fail = .not. condition

      if (fail) then
         call pFIO_throw_exception(filename, line, message=message)
         if (present(rc)) rc = 1
      end if

   end function pFIO_Assert


   logical function pFIO_Verify(status, filename, line, rc) result(fail)
      integer, intent(in) :: status
      character(*), intent(in) :: filename
      integer, intent(in) :: line
      integer, optional, intent(out) :: rc ! Not present in MAIN

      logical :: condition
      character(:), allocatable :: message
      character(16) :: status_string

      condition = (status == 0)
      fail = .not. condition

      if (fail) then
         write(status_string,'(i0)') status
         message = 'status=' // status_string
         call pFIO_throw_exception(filename, line, message=message)
         if (present(rc)) rc = status
      end if
      
   end function pFIO_Verify


   subroutine pFIO_Return(status, filename, line, rc) 
      integer, intent(in) :: status
      character(*), intent(in) :: filename
      integer, intent(in) :: line
      integer, intent(out), optional :: rc

      logical :: condition, fail
      character(:), allocatable :: message
      character(8) :: status_string

      condition = (status == 0)
      fail = .not. condition

      if (fail) then
         write(status_string,'(i0)') status
         message = 'status=' // status_string
         call pFIO_throw_exception(filename, line, message=message)
      end if
      ! Regardless of error:
      if (present(rc)) rc = status 
      
   end subroutine pFIO_Return
   
   subroutine pFIO_Abort
      integer :: status
      call MPI_Abort(MPI_COMM_WORLD,status)
   end subroutine pFIO_Abort   

end module pFIO_ErrorHandlingMod
