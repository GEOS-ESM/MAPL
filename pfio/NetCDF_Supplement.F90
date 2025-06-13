#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pfio_NetCDF_Supplement
   use, intrinsic :: iso_c_binding
   implicit none
   private

   public :: pfio_get_att_string
   public :: pfio_nf90_put_var_string
   public :: pfio_nf90_get_var_string
   public :: pfio_nf90_get_var_string_len

   interface
      function c_f_pfio_get_att_string(ncid, varid, name, string, attlen) &
           & result(stat) bind(C, name='pfio_get_att_string')
         use, intrinsic :: iso_c_binding
         implicit none
         integer :: stat
         integer(kind=C_INT), value, intent(in) :: ncid
         integer(kind=C_INT), value, intent(in) :: varid
         character(kind=C_CHAR), intent(in) :: name(*)
         character(kind=C_CHAR), intent(inout) :: string(*)
         integer(kind=C_INT), intent(inout) :: attlen
      end function c_f_pfio_get_att_string

      function c_f_pfio_get_var_string_len(ncid, varid, str_len_ptr, size) &
           & result(stat) bind(C, name='pfio_get_var_string_len')
         use, intrinsic :: iso_c_binding
         implicit none
         integer :: stat
         integer(kind=C_INT), value, intent(in) :: ncid
         integer(kind=C_INT), value, intent(in) :: varid
         type(c_ptr),         value, intent(in) :: str_len_ptr
         integer(kind=C_INT), value, intent(in) :: size
      end function c_f_pfio_get_var_string_len

      function c_f_pfio_get_var_string(ncid, varid, string_ptr, str_len,  start_ptr, count_ptr) &
           & result(stat) bind(C, name='pfio_get_var_string')
         use, intrinsic :: iso_c_binding
         implicit none
         integer :: stat
         integer(kind=C_INT), value, intent(in) :: ncid
         integer(kind=C_INT), value, intent(in) :: varid
         type(c_ptr), intent(in), value         :: string_ptr
         integer(kind=C_INT), value, intent(in) :: str_len
         type(c_ptr), intent(in), value         :: start_ptr
         type(c_ptr), intent(in), value         :: count_ptr
      end function c_f_pfio_get_var_string

      function c_f_pfio_put_vara_string(ncid, varid, string_ptr, str_len, str_size, start_ptr, count_ptr) &
           & result(stat) bind(C, name='pfio_put_vara_string')
         use, intrinsic :: iso_c_binding
         implicit none
         integer :: stat
         integer(kind=C_INT), value, intent(in) :: ncid
         integer(kind=C_INT), value, intent(in) :: varid
         type(c_ptr), intent(in), value         :: string_ptr
         integer(kind=C_INT), value, intent(in) :: str_len
         integer(kind=C_INT), value, intent(in) :: str_size
         type(c_ptr), intent(in), value         :: start_ptr
         type(c_ptr), intent(in), value         :: count_ptr
      end function c_f_pfio_put_vara_string

      function c_f_pfio_put_var_string(ncid, varid, string_ptr, str_len) &
           & result(stat) bind(C, name='pfio_put_var_string')
         use, intrinsic :: iso_c_binding
         implicit none
         integer :: stat
         integer(kind=C_INT), value, intent(in) :: ncid
         integer(kind=C_INT), value, intent(in) :: varid
         type(c_ptr), intent(in), value         :: string_ptr
         integer(kind=C_INT), value, intent(in) :: str_len
      end function c_f_pfio_put_var_string

   end interface

   interface pfio_nf90_get_var_string
     module procedure pfio_nf90_get_var_string_0d
     module procedure pfio_nf90_get_var_string_1d
   end interface
   interface pfio_nf90_put_var_string
     module procedure pfio_nf90_put_var_string_0d
     module procedure pfio_nf90_put_var_string_1d
   end interface

contains

   function pfio_get_att_string(ncid, varid, name, string) result(status)
      integer :: status
      integer(kind=C_INT), intent(in) :: ncid
      integer(kind=C_INT), intent(in) :: varid
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
      status = c_f_pfio_get_att_string(ncid, varid, c_name, tmp_str, attlen)
      allocate(character(len=attlen) :: string)
      string = trim(tmp_str)
      deallocate(c_name)
   end function pfio_get_att_string

   function pfio_nf90_get_var_string_1d(ncid, varid, string, start, count) result(status)
      integer :: status
      integer(kind=C_INT), intent(in)   :: ncid
      integer(kind=C_INT), intent(in)   :: varid
      character(*), target,intent(inout):: string(:)
      integer, optional,   intent(in)   :: start(:)
      integer, optional,   intent(in)   :: count(:)
      integer, target, allocatable :: start_(:), count_(:)
      integer :: str_len, str_size

      str_len  = len(string(1))
      str_size = size(string)
      if (.not. present(start) .or. .not. present(count)) then
        allocate(start_(1), count_(1))
        start_(1) = 1
        count_(1) = str_size
      else
        start_ = start
        count_ = count
      endif
      status = c_f_pfio_get_var_string(ncid, varid, c_loc(string), str_len,  c_loc(start_), c_loc(count_))
      deallocate(start_, count_)

   end function pfio_nf90_get_var_string_1d

   function pfio_nf90_get_var_string_0d(ncid, varid, string) result(status)
      integer :: status
      integer(kind=C_INT), intent(in)   :: ncid
      integer(kind=C_INT), intent(in)   :: varid
      character(*), target,intent(inout):: string

      integer, target, allocatable :: start_(:), count_(:)
      integer :: str_len

      str_len  = len(string)
      allocate(start_(1), count_(1))
      start_(1) = 1
      count_(1) = 1
      status = c_f_pfio_get_var_string(ncid, varid, c_loc(string), str_len,  c_loc(start_), c_loc(count_))

   end function pfio_nf90_get_var_string_0d

   function pfio_nf90_put_var_string_1d(ncid, varid, string, start, count) result(status)
      integer :: status
      integer(kind=C_INT), intent(in) :: ncid
      integer(kind=C_INT), intent(in) :: varid
      character(*), target,intent(in):: string(:)
      integer, optional,   intent(in) :: start(:)
      integer, optional,   intent(in) :: count(:)
      integer, target, allocatable :: start_(:), count_(:)
      integer :: max_len, str_size, k
      character(len=:),allocatable, target :: string_C(:)

      max_len  = len(string(1)) + 1
      str_size = size(string)
      if (.not. present(start) .or. .not. present(count)) then
        allocate(start_(1), count_(1))
        start_(1) = 1
        count_(1) = str_size
      else
        start_ = start
        count_ = count
      endif

      allocate(character(len=max_len) :: string_C(str_size))
      do k = 1, str_size
        string_C(k) = trim(adjustl(string(k)))//c_null_char
      enddo

      status = c_f_pfio_put_vara_string(ncid, varid, c_loc(string_C), max_len, str_size, c_loc(start_), c_loc(count_))
      deallocate(start_, count_)
      deallocate(string_C)
   end function pfio_nf90_put_var_string_1d

   function pfio_nf90_put_var_string_0d(ncid, varid, string) result(status)
      integer :: status
      integer(kind=C_INT), intent(in) :: ncid
      integer(kind=C_INT), intent(in) :: varid
      character(*), target,intent(in):: string
      integer :: max_len
      character(len=:),allocatable, target :: string_C(:)

      max_len  = len(string) + 1

      allocate(character(len=max_len) :: string_C(1))
      string_C(1) = string//c_null_char
      status = c_f_pfio_put_var_string(ncid, varid, c_loc(string_C), max_len)
      deallocate(string_C)
   end function pfio_nf90_put_var_string_0d

   function pfio_nf90_get_var_string_len(ncid, varid, str_len) result(status)
      use netcdf
      integer :: status
      integer, intent(in) :: ncid
      integer, intent(in) :: varid
      integer, intent(out):: str_len
      integer, allocatable  :: dimids(:)
      integer  :: size
      integer, target  :: length

      allocate(dimids(1))
      status = nf90_inquire_variable(ncid,  varid, dimids=dimids)
      status = nf90_inquire_dimension(ncid, dimids(1), len=size)
      status = c_f_pfio_get_var_string_len(ncid, varid, c_loc(length), size)
      str_len = length
   end function pfio_nf90_get_var_string_len

end module pfio_NetCDF_Supplement
