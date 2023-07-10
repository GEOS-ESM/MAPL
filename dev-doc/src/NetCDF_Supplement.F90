#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pfio_NetCDF_Supplement
   use, intrinsic :: iso_c_binding
   implicit none
   private

   public :: pfio_get_att_string
   interface
      function c_f_pfio_get_att_string(ncid, name, string, attlen) &
           & result(stat) bind(C, name='pfio_get_att_string')
         use, intrinsic :: iso_c_binding
         implicit none
         integer :: stat
         integer(kind=C_INT), value, intent(in) :: ncid
         character(kind=C_CHAR), intent(in) :: name(*)
         character(kind=C_CHAR), intent(inout) :: string(*)
         integer(kind=C_INT), intent(inout) :: attlen
      end function c_f_pfio_get_att_string
   end interface

contains

   function pfio_get_att_string(ncid, name, string) result(status)
      integer :: status
      integer(kind=C_INT), intent(in) :: ncid
      character(*), intent(in) :: name
      character(:), allocatable, intent(out) :: string
      
      integer :: name_len
      integer(kind=C_INT),target :: attlen
      character(kind=C_CHAR, len=:), target, allocatable :: c_name
      character(len=512) :: tmp_str

      ! C requires null termination
      name_len = len_trim(name)
      allocate(character(kind=C_CHAR,len=name_len+1) :: c_name)
      c_name(1:name_len) = name(1:name_len)
      c_name(name_len+1:name_len+1) = C_NULL_CHAR
      tmp_str = ''
      ! This c-call would fill tmp_str with the global attribute
      status = c_f_pfio_get_att_string(ncid, c_name, tmp_str, attlen)
      allocate(character(len=attlen) :: string)
      string = trim(tmp_str)
      deallocate(c_name)
   end function pfio_get_att_string

end module pfio_NetCDF_Supplement
