! NOTE:: It is vital that all functions in this file do not have any
! potential error conditions.  These are function that we want to use
! in expressions and as actual arguments to other procedures.

module mapl_StringUtilities
   use gftl2_StringVector
   use mapl_KeywordEnforcer
   implicit none(type,external)
   private

   public :: split
   public :: to_lower
   public :: to_upper
   public :: capitalize
   public :: is_alpha
   public :: is_alpha_only
   public :: is_numeric
   public :: is_alphanumeric
   public :: to_string
   public :: to_character_array
   public :: lowercase
   public :: uppercase
   public :: is_digit
   public :: get_ascii_interval
   public :: is_alphanum_character
   public :: is_lower_character
   public :: is_upper_character

   interface split
      procedure :: split_string
   end interface split

   interface to_lower
      module procedure :: to_lower_string
   end interface to_lower

   interface to_upper
      module procedure :: to_upper_string
   end interface to_upper

   interface capitalize
      module procedure :: capitalize_string
   end interface capitalize

   interface get_ascii_interval
      module procedure :: get_ascii_interval_array
      module procedure :: get_ascii_interval_string
   end interface get_ascii_interval

   ! This is a constant completely determined by the ASCII standard. So it
   ! is a compile constant that will never change despite the function calls.
   integer, parameter :: ASCII_UPPER_SHIFT = iachar('A')-iachar('a')
   character(len=*), parameter :: DIGITS = '0123456789'

contains

   ! The following function takes a delimited string (default
   ! comma-delimited) and returns a StringVector whose elements are
   ! the strings between delimiters.  There is a potential ambiguity
   ! as to what to do with empty strings.  E.g. what is the size of
   ! Split('')?  Here we have decided that it will have size 1: a
   ! vector whose sole element is an empty string.  If the user wants
   ! an empty vector, they can pass instead an unallocated string.


   function split_string(s, unusable, delim, preserve_whitespace) result(list)
      type(StringVector) :: list
      character(*), optional, intent(in) :: s
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(1), optional, intent(in) :: delim
      logical, optional, intent(in) :: preserve_whitespace


      character(1) :: delim_
      character(:), allocatable :: tmp
      character(:), allocatable :: item
      integer :: idx
      logical :: preserve_whitespace_

      if (.not. present(s)) return

      preserve_whitespace_ = .false.
      if (present(preserve_whitespace)) preserve_whitespace_ = preserve_whitespace
      
      delim_ = ','
      if (present(delim)) delim_ = delim

      tmp = s
      if (.not. preserve_whitespace_) tmp = adjustl(tmp)

      ! Proof that the following loop terminates:
      ! 1. If delimiter is found (idx > 0), then next iteration is on
      ! a shorter string
      ! 2. If delimiter is not found, the loop explicitly exits
      do
         idx = index(tmp, delim_)
         if (idx == 0) then
            item = tmp
            if (.not. preserve_whitespace_) item = trim(item)
            call list%push_back(item)
            exit
         end if

         item = tmp(:idx-1)
         if (.not. preserve_whitespace_) item = trim(item)
         call list%push_back(item)

         tmp = tmp(idx+1:)
         if (.not. preserve_whitespace_) tmp = adjustl(tmp)
      end do

   end function split_string

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

   elemental logical function is_lower_character(ch)
      character, intent(in) :: ch

      is_lower_character = ch >= 'a' .and. ch <= 'z'

   end function is_lower_character

   elemental logical function is_upper_character(ch)
      character, intent(in) :: ch

      is_upper_character = ch >= 'A' .and. ch <= 'Z'

   end function is_upper_character

   elemental logical function is_alpha(ch)
      character, intent(in) :: ch

      is_alpha = is_lower_character(ch) .or. is_upper_character(ch)

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

      is_alpha_only = all(is_alpha(to_character_array(s)))

   end function is_alpha_only

   logical function is_numeric(s)
      character(len=*), intent(in) :: s

      is_numeric = all(is_digit(to_character_array(s)))

   end function is_numeric

   logical function is_alphanumeric(s, exclude_underscore)
      character(len=*), intent(in) :: s
      logical, optional, intent(in) :: exclude_underscore
      logical :: exclude_underscore_

      exclude_underscore_ = .FALSE.
      if(present(exclude_underscore)) exclude_underscore_ = exclude_underscore
      is_alphanumeric = all(is_alphanum_character(to_character_array(s), exclude_underscore_))

   end function is_alphanumeric

   !===============================================================================
   ! Character conversion utilities
   !===============================================================================

   elemental function lowercase(ch) result(th)
      character :: th
      character, intent(in) :: ch

      th = ch
      if(is_upper_character(th)) th = achar(iachar(th)-ASCII_UPPER_SHIFT)

   end function lowercase

   elemental function uppercase(ch) result(th)
      character :: th
      character, intent(in) :: ch

      th = ch
      if(is_lower_character(th)) th = achar(iachar(th)+ASCII_UPPER_SHIFT)

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
      
      t = ''
      if(len(s) > 0) t = to_upper(s(1:1)) // to_lower(s(2:))

   end function capitalize_string

   !===============================================================================
   ! Utilities to get intervals of ASCII characters
   !===============================================================================

   function get_ascii_interval_array(bounds) result(interval)
      character, allocatable :: interval(:)
      character, intent(in) :: bounds(2)
      integer :: ibounds(2)
      integer :: i

      ibounds = iachar([bounds(1), bounds(2)])
      interval = [(achar(i), i=minval(ibounds), maxval(ibounds))]

   end function get_ascii_interval_array

   function get_ascii_interval_string(bounds) result(interval)
      character(len=:), allocatable :: interval
      character(len=2), intent(in) :: bounds
      
      interval = to_string(get_ascii_interval(to_character_array(bounds)))

   end function get_ascii_interval_string

end module mapl_StringUtilities
