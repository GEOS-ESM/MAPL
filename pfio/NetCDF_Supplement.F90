#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pfio_NetCDF_Supplement
   use, intrinsic :: iso_c_binding
   implicit none
   private

   public :: pfio_get_att_string
   interface
      function c_f_pfio_get_att_string(ncid, varid, name, characters, attlen) &
           & result(stat) bind(C, name='pfio_get_att_string')
         use, intrinsic :: iso_c_binding
         implicit none
         integer :: stat
         integer(kind=C_INT), value, intent(in) :: ncid
         integer(kind=C_INT), value, intent(in) :: varid
         character(kind=C_CHAR), intent(in) :: name(*)
         type(C_PTR), intent(in) :: characters
         type(C_PTR), intent(in) :: attlen
      end function c_f_pfio_get_att_string
      subroutine c_f_pfio_free_string(value) bind(C, name='pfio_free_string')
         use, intrinsic :: iso_c_binding
         implicit none
         type(C_PTR), intent(in) :: value
      end subroutine c_f_pfio_free_string

   end interface

contains

   function pfio_get_att_string(ncid, varid, name, string) result(status)
      integer :: status
      integer(kind=C_INT), intent(in) :: ncid
      integer(kind=C_INT), intent(in) :: varid
      character(*), intent(in) :: name
      character(:), allocatable, intent(out) :: string
      
      integer :: i
      character(kind=C_CHAR), pointer :: characters(:)
      integer(kind=C_SIZE_T), target :: attlen
      integer :: name_len
      character(kind=C_CHAR, len=:), target, allocatable :: c_name
      type(C_PTR) :: str

      ! C requires null termination
      name_len = len_trim(name)
      allocate(character(kind=C_CHAR,len=name_len+1) :: c_name)
      c_name(1:name_len) = name
      c_name(name_len+1:name_len+1) = C_NULL_CHAR

      status = c_f_pfio_get_att_string(ncid, varid, c_name, str, c_loc(attlen))

      ! need to convert str to fortran string

      ! Must copy C char array into Fortran string and then free the C memory
      allocate(character(len=attlen) :: string)
      do i = 1, attlen
         string(i:i) = characters(i)
      end do

      call c_f_pfio_free_string(c_loc(characters))
      
   end function pfio_get_att_string

end module pfio_NetCDF_Supplement
