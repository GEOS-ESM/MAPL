module grid_comp_creation_shared

   use, intrinsic :: iso_fortran_env, only: R64 => real64

   implicit none
   private
   public :: to_characters
   public :: MAXSTR
   public :: JOIN

   integer, parameter :: MAXSTR = 256
   character(len=*), parameter :: JOIN = ', '

   interface to_characters
      module procedure :: real_to_characters
      module procedure :: integer_to_characters
   end interface to_characters
   
contains

   function real_to_characters(t, fixed, rc) result(chars)
      character(len=:), allocatable :: chars
      real(R64), intent(in) :: t
      logical, optional, intent(in) :: fixed
      integer, optional, intent(out) :: rc
      integer :: status
      integer, parameter :: TW = 12
      character(len=TW), parameter :: SF = '(  ES24.16 )'
      character(len=TW), parameter :: FD = '(     F8.4 )'
      character(len=TW) :: FMT_
      character(len=MAXSTR) :: raw

      FMT_ = SF
      if(present(fixed)) then
         if(fixed) FMT_ = FD
      end if
      write(raw, fmt=FMT_, iostat=status) t
      if(present(rc)) rc = status
      chars = trim(adjustl(raw))

   end function real_to_characters

   function integer_to_characters(n, rc) result(chars)
      character(len=:), allocatable :: chars
      integer, intent(in) :: n
      integer, optional, intent(out) :: rc
      integer :: status
      character(len=MAXSTR) :: raw

      write(raw, fmt='(I0)', iostat=status) n
      if(present(rc)) rc = status
      chars = trim(adjustl(raw))

   end function integer_to_characters
   
end module grid_comp_creation_shared
