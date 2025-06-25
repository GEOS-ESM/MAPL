module mapl3g_StringCommon
   implicit none(type, external)
   private

   public :: ASCII_UPPER_SHIFT
   public :: to_lower
   public :: to_upper
   public :: capitalize
   public :: is_alpha
   public :: is_alpha_only
   public :: is_numeric
   public :: is_alphanumeric
   public :: to_string
   public :: to_char_array
   public :: lowercase
   public :: uppercase
   public :: is_digit
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

   integer, parameter :: ASCII_UPPER_SHIFT = iachar('A') - iachar('a')
   character(len=*), parameter :: DIGITS = '0123456789'

contains

   function to_string(array) result(string)
      character, intent(in) :: array(:)
      character(len=size(array)) :: string
      integer :: i

      do i = 1, size(array)
         string(i:i) = array(i)
      end do

   end function to_string

   function to_char_array(s) result(ch)
      character(len=*), intent(in) :: s
      character, allocatable :: ch(:)
      integer :: i

      allocate(ch(len(s)))
      do i= 1, size(ch)
         ch(i) = s(i:i)
      end do

   end function to_char_array

   function to_lower_string(s) result(t)
      character(len=:), allocatable :: t
      character(len=*), intent(in) :: s
      
      t = to_string(lowercase(to_char_array(s)))

   end function to_lower_string

   elemental function lowercase(ch) result(th)
      character :: th
      character, intent(in) :: ch

      th = ch
      if(is_upper(th)) th = achar(iachar(th)-ASCII_UPPER_SHIFT)

   end function lowercase

   function to_upper_string(s) result(t)
      character(len=:), allocatable :: t
      character(len=*), intent(in) :: s

      t = to_string(uppercase(to_char_array(s)))

   end function to_upper_string

   elemental function uppercase(ch) result(th)
      character :: th
      character, intent(in) :: ch

      th = ch
      if(is_lower(th)) th = achar(iachar(th)+ASCII_UPPER_SHIFT)

   end function uppercase

   elemental logical function is_lower(ch)
      character, intent(in) :: ch

      is_lower = ch >= 'a' .and. ch <= 'z'

   end function is_lower

   elemental logical function is_upper(ch)
      character, intent(in) :: ch

      is_upper = ch >= 'A' .and. ch <= 'Z'

   end function is_upper

   elemental logical function is_digit(ch)
      character, intent(in) :: ch

      is_digit = (index(ch, DIGITS) > 0)

   end function is_digit

   logical function is_numeric(s)
      character(len=*), intent(in) :: s

      is_numeric = all(is_digit(to_char_array(s)))

   end function is_numeric

   function capitalize_string(s) result(t)
      character(len=:), allocatable :: t
      character(len=*), intent(in) :: s

      t = to_upper(s(1:1)) // to_lower(s(2:))

   end function capitalize_string

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
      
      range = to_string(get_ascii_range(to_char_array(bounds)))

   end function get_ascii_range_string

   logical function is_alphanumeric(s, exclude_underscore)
      character(len=*), intent(in) :: s
      logical, optional, intent(in) :: exclude_underscore
      logical :: incl_und_

      incl_und_ = .TRUE.
      if(present(exclude_underscore)) incl_und_ = .not. exclude_underscore

      is_alphanumeric = all(predicate(to_char_array(s), incl_und_))

   contains

      elemental logical function predicate(ch, incl_und)
         character, intent(in) :: ch
         logical, intent(in) :: incl_und

         predicate = is_alpha(ch) .or. is_digit(ch) .or. (incl_und .and. ch == '_')

      end function predicate

   end function is_alphanumeric

   elemental logical function is_alpha(ch)
      character, intent(in) :: ch

      is_alpha = is_lower(ch) .or. is_upper(ch)

   end function is_alpha

   logical function is_alpha_only(s)
      character(len=*), intent(in) :: s

      is_alpha_only = all(is_alpha(to_char_array(s)))

   end function is_alpha_only

end module mapl3g_StringCommon
