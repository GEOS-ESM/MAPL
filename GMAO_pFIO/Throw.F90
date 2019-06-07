module pFIO_ThrowMod
   implicit none
   private

   public :: pFIO_throw_exception
   public :: pFIO_set_throw_method

   abstract interface
      subroutine throw(file_name, line_number, message)
         character(len=*), intent(in) :: file_name
         integer, intent(in) :: line_number
         character(len=*), optional, intent(in) :: message
      end subroutine throw
   end interface

   procedure (throw), pointer :: throw_method => null()
   logical, save :: initialized = .false.

contains

   subroutine pFIO_set_throw_method(method)
      procedure (throw) :: method
      if (.not. initialized) call initialize()
      throw_method => method
   end subroutine pFIO_set_throw_method

   subroutine initialize()
      throw_method => throw_print
      initialized = .true.
   end subroutine initialize

   
   subroutine pFIO_throw_exception(file_name, line_number, message)
      character(len=*), intent(in) :: file_name
      integer, intent(in) :: line_number
      character(len=*), optional, intent(in) :: message

      if (.not. initialized) then
         call initialize()
      end if

      call throw_method(file_name, line_number, message=message)
      
   end subroutine pFIO_throw_exception


   subroutine throw_print(file_name, line_number, message)
      character(len=*), intent(in) :: file_name
      integer, intent(in) :: line_number
      character(len=*), optional, intent(in) :: message

      print*,' '
      print*,'Found problem in file: < ',file_name,' >'
      print*,'              at line: < ',line_number,' >'
      if (present(message)) then
         print*,'              message: < ',message,' >'
      end if

   end subroutine throw_print

end module pFIO_ThrowMod
