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

   function real_to_characters(t, rc) result(chars)
      character(len=:), allocatable :: chars
      real(R64), intent(in) :: t
      integer, optional, intent(out) :: rc
      integer :: status
      character(len=MAXSTR) :: raw

      write(raw, fmt='(ES24.16)', iostat=status) t
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
