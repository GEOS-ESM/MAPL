module MAPL_ErrorHandlingMod
   use MAPL_ThrowMod
   implicit none
   private

   public :: MAPL_Assert
   public :: MAPL_Verify
   public :: MAPL_Return

contains


   logical function MAPL_Assert(condition, message, filename, line, rc) result(fail)
      logical, intent(in) :: condition
      character(*), intent(in) :: message
      character(*), intent(in) :: filename
      integer, intent(in) :: line
      integer, optional, intent(out) :: rc ! Not present in MAIN

      fail = .not. condition

      if (fail) then
         call MAPL_throw_exception(filename, line, message=message)
         if (present(rc)) rc = 1
      end if

   end function MAPL_Assert


   logical function MAPL_Verify(status, filename, line, rc) result(fail)
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
         call MAPL_throw_exception(filename, line, message=message)
         if (present(rc)) rc = status
      end if
      
   end function MAPL_Verify


   subroutine MAPL_Return(status, filename, line, rc) 
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
         call MAPL_throw_exception(filename, line, message=message)
      end if
      ! Regardless of error:
      if (present(rc)) rc = status 
      
   end subroutine MAPL_Return
   
   

end module MAPL_ErrorHandlingMod
