! Primary purpose is to wrap allocatable strings and thereby
! improve usability of such strings in generic interfaces.

#include "unused_dummy.H"
module MAPL_String
   use mapl_StringUtilities, only: to_upper, to_lower, capitalize_string => capitalize

   implicit none
   private

   public :: String

   type :: String
      private
      character(:), allocatable :: string
   contains
      ! ASSIGNMENT(=)
      procedure :: assign_string_from_string
      procedure :: assign_string_from_char
      procedure, pass(from) :: assign_char_from_string
      generic :: assignment(=) => assign_string_from_string
      generic :: assignment(=) => assign_string_from_char
      generic :: assignment(=) => assign_char_from_string

      ! OPERATOR(==)
      procedure :: string_is_equal_to_string
      procedure :: string_is_equal_to_char
      procedure, pass(this) :: char_is_equal_to_string
      generic :: operator(==) => string_is_equal_to_string
      generic :: operator(==) => string_is_equal_to_char
      generic :: operator(==) => char_is_equal_to_string

      ! OPERATOR(<)
      procedure :: string_is_less_than_string
      procedure :: string_is_less_than_char
      procedure, pass(this) :: char_is_less_than_string
      generic :: operator(<) => string_is_less_than_string
      generic :: operator(<) => string_is_less_than_char
      generic :: operator(<) => char_is_less_than_string

      ! OPERATOR(<=)
      procedure :: string_is_less_than_or_equal_to_string
      procedure :: string_is_less_than_or_equal_to_char
      procedure, pass(this) :: char_is_less_than_or_equal_to_string
      generic :: operator(<=) => string_is_less_than_or_equal_to_string
      generic :: operator(<=) => string_is_less_than_or_equal_to_char
      generic :: operator(<=) => char_is_less_than_or_equal_to_string

      ! OPERATOR(>)
      procedure :: string_is_greater_than_string
      procedure :: string_is_greater_than_char
      procedure, pass(this) :: char_is_greater_than_string
      generic :: operator(>) => string_is_greater_than_string
      generic :: operator(>) => string_is_greater_than_char
      generic :: operator(>) => char_is_greater_than_string

      ! OPERATOR(>=)
      procedure :: string_is_greater_than_or_equal_to_string
      procedure :: string_is_greater_than_or_equal_to_char
      procedure, pass(this) :: char_is_greater_than_or_equal_to_string
      generic :: operator(>=) => string_is_greater_than_or_equal_to_string
      generic :: operator(>=) => string_is_greater_than_or_equal_to_char
      generic :: operator(>=) => char_is_greater_than_or_equal_to_string

      ! OPERATOR(/=)
      procedure :: string_is_not_equal_to_string
      procedure :: string_is_not_equal_to_char
      procedure, pass(this) :: char_is_not_equal_to_string
      generic :: operator(/=) => string_is_not_equal_to_string
      generic :: operator(/=) => string_is_not_equal_to_char
      generic :: operator(/=) => char_is_not_equal_to_string

      ! OPERATOR(//)
      procedure :: concatenate_string_string
      procedure :: concatenate_string_char
      procedure, pass(this) :: concatenate_char_string
      generic :: operator(//) => concatenate_string_string
      generic :: operator(//) => concatenate_string_char
      generic :: operator(//) => concatenate_char_string

      ! WRITE(FORMATTED)
      procedure :: write_formatted
      generic :: write(formatted) => write_formatted

      ! INTRINSICS
      procedure :: len => len_string
      procedure :: len_trim => len_trim_string

      procedure :: index_string
      procedure :: index_char
      generic :: index => index_string
      generic :: index => index_char

      procedure :: scan_string
      procedure :: scan_char
      generic :: scan => scan_string
      generic :: scan => scan_char

      procedure :: verify_string
      procedure :: verify_char
      generic :: verify => verify_string
      generic :: verify => verify_char

      ! Supplemental
      procedure :: get => get_fixed_length_string
      procedure :: is_allocated
      procedure :: lower
      procedure :: upper
      procedure :: capitalize

   end type String

   interface String
      module procedure new_String
   end interface String

contains

   function new_String(s) result(str)
      type(String) :: str
      character(*), intent(in) :: s
      str%string = s
   end function new_String


   subroutine assign_string_from_string(to, from)
      class(String), intent(out) :: to
      class(String), intent(in) :: from
      to%string = from%string
   end subroutine assign_string_from_string

   subroutine assign_string_from_char(to, from)
      class(String), intent(out) :: to
      character(*), intent(in) :: from
      to%string = from
   end subroutine assign_string_from_char

   subroutine assign_char_from_string(to, from)
      character(:), allocatable, intent(out) :: to
      class(String), intent(in) :: from
      to = from%string
   end subroutine assign_char_from_string



   logical function string_is_equal_to_string(this, rhs) result(are_equal)
      class(String), intent(in) :: this
      class(String), intent(in) :: rhs
      are_equal = (this%string == rhs%string)
   end function string_is_equal_to_string

   logical function string_is_equal_to_char(this, rhs) result(are_equal)
      class(String), intent(in) :: this
      character(*), intent(in) :: rhs
      are_equal = (this%string == rhs)
   end function string_is_equal_to_char
   
   logical function char_is_equal_to_string(lhs, this) result(are_equal)
      character(*), intent(in) :: lhs
      class(String), intent(in) :: this
      are_equal = (lhs == this%string)
   end function char_is_equal_to_string
   

   logical function string_is_less_than_string(this, rhs) result(is_less_than)
      class(String), intent(in) :: this
      class(String), intent(in) :: rhs
      is_less_than = (this%string < rhs%string)
   end function string_is_less_than_string

   logical function string_is_less_than_char(this, rhs) result(is_less_than)
      class(String), intent(in) :: this
      character(*), intent(in) :: rhs
      is_less_than = (this%string < rhs)
   end function string_is_less_than_char
   
   logical function char_is_less_than_string(lhs, this) result(is_less_than)
      character(*), intent(in) :: lhs
      class(String), intent(in) :: this
      is_less_than = (lhs < this%string)
   end function char_is_less_than_string
   

   logical function string_is_less_than_or_equal_to_string(this, rhs) result(is_less_than_or_equal)
      class(String), intent(in) :: this
      class(String), intent(in) :: rhs
      is_less_than_or_equal = (this%string <= rhs%string)
   end function string_is_less_than_or_equal_to_string

   logical function string_is_less_than_or_equal_to_char(this, rhs) result(is_less_than_or_equal)
      class(String), intent(in) :: this
      character(*), intent(in) :: rhs
      is_less_than_or_equal = (this%string <= rhs)
   end function string_is_less_than_or_equal_to_char
   
   logical function char_is_less_than_or_equal_to_string(lhs, this) result(is_less_than_or_equal)
      character(*), intent(in) :: lhs
      class(String), intent(in) :: this
      is_less_than_or_equal = (lhs <= this%string)
   end function char_is_less_than_or_equal_to_string
   

   logical function string_is_greater_than_string(this, rhs) result(is_greater_than)
      class(String), intent(in) :: this
      class(String), intent(in) :: rhs
      is_greater_than = (this%string < rhs%string)
   end function string_is_greater_than_string

   logical function string_is_greater_than_char(this, rhs) result(is_greater_than)
      class(String), intent(in) :: this
      character(*), intent(in) :: rhs
      is_greater_than = (this%string < rhs)
   end function string_is_greater_than_char
   
   logical function char_is_greater_than_string(lhs, this) result(is_greater_than)
      character(*), intent(in) :: lhs
      class(String), intent(in) :: this
      is_greater_than = (lhs < this%string)
   end function char_is_greater_than_string
   

   logical function string_is_greater_than_or_equal_to_string(this, rhs) result(is_greater_than_or_equal)
      class(String), intent(in) :: this
      class(String), intent(in) :: rhs
      is_greater_than_or_equal = (this%string <= rhs%string)
   end function string_is_greater_than_or_equal_to_string

   logical function string_is_greater_than_or_equal_to_char(this, rhs) result(is_greater_than_or_equal)
      class(String), intent(in) :: this
      character(*), intent(in) :: rhs
      is_greater_than_or_equal = (this%string <= rhs)
   end function string_is_greater_than_or_equal_to_char
   
   logical function char_is_greater_than_or_equal_to_string(lhs, this) result(is_greater_than_or_equal)
      character(*), intent(in) :: lhs
      class(String), intent(in) :: this
      is_greater_than_or_equal = (lhs <= this%string)
   end function char_is_greater_than_or_equal_to_string
   

   logical function string_is_not_equal_to_string(this, rhs) result(are_not_equal)
      class(String), intent(in) :: this
      class(String), intent(in) :: rhs
      are_not_equal = .not. (this == rhs)
   end function string_is_not_equal_to_string
   
   logical function string_is_not_equal_to_char(this, rhs) result(are_not_equal)
      class(String), intent(in) :: this
      character(*), intent(in) :: rhs
      are_not_equal = .not. (this == rhs)
   end function string_is_not_equal_to_char
   
   logical function char_is_not_equal_to_string(lhs, this) result(are_not_equal)
      character(*), intent(in) :: lhs
      class(String), intent(in) :: this
      are_not_equal = .not. (lhs == this)
   end function char_is_not_equal_to_string
   

   subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
      class(String), intent(in) :: this
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      _UNUSED_DUMMY(iotype)
      _UNUSED_DUMMY(v_list)

      write(unit,'(a)') this%string
      iostat = 0
      iomsg=iomsg
   end subroutine write_formatted


   subroutine get_fixed_length_string(this, s)
      class(String), intent(in) :: this
      character(*), intent(out) :: s
      s = this%string
   end subroutine get_fixed_length_string

   logical function is_allocated(this)
      class(String), intent(in) :: this
      is_allocated = allocated(this%string)
   end function is_allocated

   function concatenate_string_string(this, rhs) result(str)
      type(String) :: str
      class(String), intent(in) :: this
      class(String), intent(in) :: rhs
      str = this%string // rhs%string
   end function concatenate_string_string

   function concatenate_string_char(this, rhs) result(str)
      type(String) :: str
      class(String), intent(in) :: this
      character(*), intent(in) :: rhs
      str = this%string // rhs
   end function concatenate_string_char

   function concatenate_char_string(lhs, this) result(str)
      type(String) :: str
      character(*), intent(in) :: lhs
      class(String), intent(in) :: this
      str = lhs //this%string
   end function concatenate_char_string


   integer function len_string(this)
      class(String), intent(in) :: this
      len_string = len(this%string)
   end function len_string


   integer function len_trim_string(this)
      class(String), intent(in) :: this
      len_trim_string = len_trim(this%string)
   end function len_trim_string


   function index_string(this, substring, back) result(idx)
      integer :: idx
      class(String), intent(in) :: this
      class(String), intent(in) :: substring
      logical, optional :: back

      idx = index(this%string, substring%string, back)
   end function index_string

   function index_char(this, substring, back) result(idx)
      integer :: idx
      class(String), intent(in) :: this
      character(*), intent(in) :: substring
      logical, optional :: back

      idx = index(this%string, substring, back)
   end function index_char

   function scan_string(this, set, back) result(idx)
      integer :: idx
      class(String), intent(in) :: this
      class(String), intent(in) :: set
      logical, optional :: back

      idx = scan(this%string, set%string, back)
   end function scan_string

   function scan_char(this, set, back) result(idx)
      integer :: idx
      class(String), intent(in) :: this
      character(*), intent(in) :: set
      logical, optional :: back

      idx = scan(this%string, set, back)
   end function scan_char

   function verify_string(this, set, back) result(idx)
      integer :: idx
      class(String), intent(in) :: this
      class(String), intent(in) :: set
      logical, optional :: back

      idx = verify(this%string, set%string, back)
   end function verify_string

   function verify_char(this, set, back) result(idx)
      integer :: idx
      class(String), intent(in) :: this
      character(*), intent(in) :: set
      logical, optional :: back

      idx = verify(this%string, set, back)
   end function verify_char


   function lower(this)
      type(String) :: lower
      class(String), intent(in) :: this

      lower%string = to_lower(this%string)

   end function lower
   
   function upper(this)
      type(String) :: upper
      class(String), intent(in) :: this

      upper%string = to_upper(this%string)

   end function upper
   
   function capitalize(this)
      type(String) :: capitalize
      class(String), intent(in) :: this
         
      capitalize%string = capitalize_string(this%string)

   end function capitalize

   
end module MAPL_String
