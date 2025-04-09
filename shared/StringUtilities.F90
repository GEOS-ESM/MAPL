! NOTE:: It is vital that all functions in this file do not have any
! potential error conditions.  These are function that we want to use
! in expressions and as actual arguments to other procedures.

module mapl_StringUtilities
   use gftl2_StringVector
   use mapl_KeywordEnforcer
   implicit none(type,external)
   private

   public :: to_lower
   public :: to_upper
   public :: split

   interface split
      procedure :: split_string
   end interface split

   integer, parameter :: ASCI_UPPER_SHIFT = iachar('A') - iachar('a')

contains

   function to_lower(s) result(lower)
      character(*), intent(in) :: s
      character(:), allocatable :: lower

      integer :: i
      integer :: n

      n = len(s)

      allocate(character(len=n) :: lower)
      do i = 1, n
         lower(i:i) = s(i:i)
         if (s(i:i) >= 'A' .and. s(i:i) <= 'Z') then
            lower(i:i) = achar(iachar(s(i:i)) - ASCI_UPPER_SHIFT)
         end if
      end do

   end function to_lower

   function to_upper(s) result(upper)
      character(*), intent(in) :: s
      character(:), allocatable :: upper

      integer :: i
      integer :: n

      n = len(s)

      allocate(character(len=n) :: upper)
      do i = 1, n
         upper(i:i) = s(i:i)
         if (s(i:i) >= 'a' .and. s(i:i) <= 'z') then
            upper(i:i) = achar(iachar(s(i:i)) + ASCI_UPPER_SHIFT)
         end if
      end do

   end function to_upper


   ! The following function takes a delimited string (default
   ! comma-delimited) and returns a StringVector whose elements are
   ! the strings between delimiters.  There is a potential ambiguity
   ! as to what to do with empty strings.  E.g. what is the size of
   ! Split('')?  Here we have decided that it will have size 1: a
   ! vector whose sole element is an empty string.  If the user wants
   ! an empty vector, they can pass instead an unallocated string.


   function split_string(s, unusable, delim) result(list)
      type(StringVector) :: list
      character(*), optional, intent(in) :: s
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(1), optional, intent(in) :: delim


      character(1) :: delim_
      character(:), allocatable :: tmp
      integer :: idx

      if (.not. present(s)) return
      
      delim_ = ','
      if (present(delim)) delim_ = delim
      
      tmp = s

      ! Proof that the following loop terminates:
      ! 1. If delimiter is found (idx > 0), then next iteration is on
      ! a shorter string
      ! 2. If delimiter is not found, the loop explicitly exits
      do
         idx = index(tmp, delim_)
         if (idx == 0) then
            call list%push_back(tmp)
            exit
         end if
         call list%push_back(tmp(:idx-1))
         tmp = tmp(idx+1:)
      end do

   end function split_string

end module mapl_StringUtilities
