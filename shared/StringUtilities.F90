! NOTE:: It is vital that all functions in this file do not have any
! potential error conditions.  These are function that we want to use
! in expressions and as actual arguments to other procedures.

module mapl_StringUtilities
   use gftl2_StringVector
   use mapl3g_StringCommon
   use mapl_KeywordEnforcer
   implicit none(type,external)
   private

   public :: split
   ! These are imported from mapl3g_StringCommon.
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

   interface split
      procedure :: split_string
   end interface split

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

end module mapl_StringUtilities
