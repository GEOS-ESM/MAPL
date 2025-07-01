module mapl3g_StringCommon
   implicit none(type, external)
   private

   public :: ASCII_UPPER_SHIFT
   public :: to_string
   public :: to_character_array
   public :: is_lower
   public :: is_upper
   public :: is_alpha
   public :: is_digit
   public :: is_alphanum_character
   public :: is_alpha_only
   public :: is_numeric
   public :: is_alphanumeric
   public :: lowercase
   public :: uppercase
   public :: to_lower
   public :: to_upper
   public :: capitalize
   public :: get_ascii_range

   interface to_lower
      module procedure :: to_lower_string
   end interface to_lower

   interface to_upper
      module procedure :: to_upper_string
   end interface to_upper

   interface capitalize
      module procedure :: capitalize_string
   end interface capitalize

   interface get_ascii_range
      module procedure :: get_ascii_range_array
      module procedure :: get_ascii_range_string
   end interface get_ascii_range

   ! ASCII_UPPER_SHIFT = iachar('A')-iachar('a')
   integer, parameter :: ASCII_UPPER_SHIFT = -32
   character(len=*), parameter :: DIGITS = '0123456789'

contains

   !===============================================================================
   ! Utility functions to convert strings (arbitrary character variables)
   !===============================================================================

   function to_string(array) result(string)
      character(len=:), allocatable :: string
      character, intent(in) :: array(:)
      integer :: i

      allocate(character(len=size(array)) :: string)
      do i = 1, size(array)
         string(i:i) = array(i)
      end do

   end function to_string

   function to_character_array(s) result(ch)
      character, allocatable :: ch(:)
      character(len=*), intent(in) :: s
      integer :: i

      allocate(ch(len(s)))
      do i= 1, size(ch)
         ch(i) = s(i:i)
      end do

   end function to_character_array

   !===============================================================================
   ! Inquiry functions - character
   !===============================================================================

   elemental logical function is_lower(ch)
      character, intent(in) :: ch

      is_lower = ch >= 'a' .and. ch <= 'z'

   end function is_lower

   elemental logical function is_upper(ch)
      character, intent(in) :: ch

      is_upper = ch >= 'A' .and. ch <= 'Z'

   end function is_upper

   elemental logical function is_alpha(ch)
      character, intent(in) :: ch

      is_alpha = is_lower(ch) .or. is_upper(ch)

   end function is_alpha

   elemental logical function is_digit(ch)
      character, intent(in) :: ch

      is_digit = (index(DIGITS, ch) > 0)

   end function is_digit

   elemental logical function is_alphanum_character(ch, exclude_underscore)
      character, intent(in) :: ch
      logical, optional, intent(in) :: exclude_underscore
      logical :: incl_und_

      incl_und_ = .TRUE.
      if(present(exclude_underscore)) incl_und_ = .not. exclude_underscore
      is_alphanum_character = is_alpha(ch) .or. is_digit(ch) .or. (incl_und_ .and. ch == '_')

   end function is_alphanum_character

   !===============================================================================
   ! Inquiry functions - string (character with any length)
   !===============================================================================

   logical function is_alpha_only(s)
      character(len=*), intent(in) :: s

      is_alpha_only = len(s) > 0 .and. all(is_alpha(to_character_array(s)))

   end function is_alpha_only

   logical function is_numeric(s)
      character(len=*), intent(in) :: s

      is_numeric = len(s) > 0 .and. all(is_digit(to_character_array(s)))

   end function is_numeric

   logical function is_alphanumeric(s, exclude_underscore)
      character(len=*), intent(in) :: s
      logical, optional, intent(in) :: exclude_underscore
      logical :: exclude_underscore_

      exclude_underscore_ = .FALSE.
      if(present(exclude_underscore)) exclude_underscore_ = exclude_underscore
      is_alphanumeric = len(s) > 0 .and. all(is_alphanum_character(to_character_array(s), exclude_underscore_))

   end function is_alphanumeric

   !===============================================================================
   ! Character conversion utilities
   !===============================================================================

   elemental function lowercase(ch) result(th)
      character :: th
      character, intent(in) :: ch

      th = ch
      if(is_upper(th)) th = achar(iachar(th)-ASCII_UPPER_SHIFT)

   end function lowercase

   elemental function uppercase(ch) result(th)
      character :: th
      character, intent(in) :: ch

      th = ch
      if(is_lower(th)) th = achar(iachar(th)+ASCII_UPPER_SHIFT)

   end function uppercase

   !===============================================================================
   ! String (arbitrary length character) conversion utilities
   !===============================================================================

   function to_lower_string(s) result(t)
      character(len=:), allocatable :: t
      character(len=*), intent(in) :: s
      
      t = to_string(lowercase(to_character_array(s)))

   end function to_lower_string

   function to_upper_string(s) result(t)
      character(len=:), allocatable :: t
      character(len=*), intent(in) :: s

      t = to_string(uppercase(to_character_array(s)))

   end function to_upper_string

   function capitalize_string(s) result(t)
      character(len=:), allocatable :: t
      character(len=*), intent(in) :: s

      t = to_upper(s(1:1)) // to_lower(s(2:))

   end function capitalize_string

   !===============================================================================
   ! Utilities to get ranges of ASCII characters
   !===============================================================================

   function get_ascii_range_array(bounds) result(range)
      character, allocatable :: range(:)
      character, intent(in) :: bounds(2)
      integer :: ibounds(2)
      integer :: i

      ibounds = iachar([bounds(1), bounds(2)])
      range = [(achar(i), i=minval(ibounds), maxval(ibounds))]

   end function get_ascii_range_array

   function get_ascii_range_string(bounds) result(range)
      character(len=:), allocatable :: range
      character(len=2), intent(in) :: bounds
      
      range = to_string(get_ascii_range(to_character_array(bounds)))

   end function get_ascii_range_string

end module mapl3g_StringCommon
