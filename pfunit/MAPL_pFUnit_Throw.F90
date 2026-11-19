module MAPL_pFUnit_ThrowMod
   implicit none
   private

   public :: throw
   public :: propagate

contains

   subroutine throw(file_name, line_number, message)
      use pFUnit, only: SourceLocation
      use pFUnit, only: pFUnit_throw => throw
      character(len=*), intent(in) :: file_name
      integer, intent(in) :: line_number
      character(len=*), optional, intent(in) :: message

      call pFUnit_throw(message_or_default(message), SourceLocation(file_name, line_number))

   end subroutine throw

   ! A failure passed up from a callee is added to the traceback of the
   ! pending exception, so a test asserts only the original message.
   subroutine propagate(file_name, line_number, message)
      use pFUnit, only: SourceLocation
      use pFUnit, only: pFUnit_propagate => propagate
      character(len=*), intent(in) :: file_name
      integer, intent(in) :: line_number
      character(len=*), optional, intent(in) :: message

      call pFUnit_propagate(message_or_default(message), SourceLocation(file_name, line_number))

   end subroutine propagate

   function message_or_default(message) result(message_)
      character(len=:), allocatable :: message_
      character(len=*), optional, intent(in) :: message

      if (present(message)) then
         message_ = message
      else
         message_ = '<no message>'
      end if

   end function message_or_default

end module MAPL_pFUnit_ThrowMod
