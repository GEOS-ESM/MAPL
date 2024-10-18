module mapl3g_Validation
   implicit none
   private
  
   public :: is_valid_name
  
  
   character(*), parameter :: LOWER = 'abcdefghijklmnopqrstuvwxyz'
   character(*), parameter :: UPPER = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
   character(*), parameter :: DIGITS = '0123456789'
   character(*), parameter :: ALPHANUMERIC = LOWER//UPPER//DIGITS

contains


   ! Component names and short names must:
   !  1. Have at least one character
   !  2. Begin with a letter
   !  3. Only consist of letters, digits, and underscores

   pure logical function is_valid_name(name) result(is_valid)
      character(len=*), intent(in) :: name

      is_valid =  len(name) > 0
      if (.not. is_valid) return

      is_valid = (verify(name(1:1), LOWER // UPPER) == 0)
      if (.not. is_valid) return
      
      is_valid = (verify(name(2:), LOWER // UPPER // DIGITS // '_') == 0)

   end function is_valid_name
  
end module mapl3g_Validation
