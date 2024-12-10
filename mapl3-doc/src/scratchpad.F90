module scratchpad
   implicit none
   private

   public :: log
   public :: append_message
   public :: clear_log

   character(:), allocatable :: log

contains

   subroutine clear_log()
      if (allocated(log)) deallocate(log)
   end subroutine clear_log

   subroutine append_message(msg)
      character(len=*), intent(in) :: msg

      if (.not. allocated(log)) then
         log = msg
      else
         log = log // ' :: ' // msg
      end if

   end subroutine append_message

end module scratchpad
