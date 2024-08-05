module strings

   implicit none

   private

   public :: String
   public :: assignment(=)

   type :: String
      character(len=:), allocatable :: characters
   end type String

   interface String
      module procedure :: construct_string
   end interface String

   interface assignment(=)
      module procedure :: assign_characters_to_string
      module procedure :: assign_string_to_characters
   end interface assignment(=)

   character(len=*), parameter :: BLANK = ''

contains

   function construct_string(ch) result(s)
      type(String) :: s
      character(len=*), optional, intent(in) :: ch

      s%characters = BLANK
      if(present(ch)) s%characters = trim(ch)

   end function construct_string

   subroutine assign_characters_to_string(s, ch)
      type(String), intent(out) :: s
      character(len=*), intent(in) :: ch

      s = String(ch)

   end subroutine assign_characters_to_string

   subroutine assign_string_to_characters(ch, s)
      character(len=*), intent(out) :: ch
      type(String), intent(in) :: s

      ch = s%characters

   end subroutine assign_string_to_characters

end module strings

