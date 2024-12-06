module MAPL_pFUnit_ThrowMod
   implicit none
   private

   public :: throw

contains

   subroutine throw(file_name, line_number, message)
      use pFUnit, only: SourceLocation
      use pFUnit, only: pFUnit_throw => throw
      character(len=*), intent(in) :: file_name
      integer, intent(in) :: line_number
      character(len=*), optional, intent(in) :: message

      character(len=:), allocatable :: message_

      if (present(message)) then
         message_ = message
      else
         message_ = '<no message>'
      end if
      call pFUnit_throw(message_, SourceLocation(file_name, line_number))

   end subroutine throw
   
end module MAPL_pFUnit_ThrowMod
