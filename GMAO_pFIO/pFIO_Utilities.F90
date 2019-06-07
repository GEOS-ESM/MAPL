#include "unused_dummy.H"
module pFIO_UtilitiesMod
   use iso_fortran_env, only: INT32,REAL32
   use pFIO_ConstantsMod
   implicit none
   private

   public :: serialize_buffer_length
   public :: serialize_intrinsic
   public :: deserialize_intrinsic
   public :: word_size

   interface serialize_buffer_length
      module procedure serialize_buffer_length_string
      module procedure serialize_buffer_length_int32_0d
      module procedure serialize_buffer_length_int32_1d
      module procedure serialize_buffer_length_real32_0d
      module procedure serialize_buffer_length_real32_1d
      module procedure serialize_buffer_length_logical_0d
      module procedure serialize_buffer_length_logical_1d
   end interface serialize_buffer_length

   interface serialize_intrinsic
      module procedure serialize_string
      module procedure serialize_int32_0d
      module procedure serialize_int32_1d
      module procedure serialize_real32_0d
      module procedure serialize_real32_1d
      module procedure serialize_logical_0d
      module procedure serialize_logical_1d
   end interface serialize_intrinsic

   interface deserialize_intrinsic
      module procedure deserialize_string
      module procedure deserialize_int32_0d
      module procedure deserialize_int32_1d
      module procedure deserialize_real32_0d
      module procedure deserialize_real32_1d
      module procedure deserialize_logical_0d
      module procedure deserialize_logical_1d
   end interface deserialize_intrinsic

   integer, parameter :: CHARS_PER_INT32 = 4

contains

!!! length

   integer function serialize_buffer_length_string(str) result(length)
      character(len=*), intent(in) :: str

      integer :: n

      n = len(str)
      ! two words in header
      length = 1 + 1 + (1 + (n-1)/CHARS_PER_INT32)

   end function serialize_buffer_length_string


   integer function serialize_buffer_length_int32_0d(scalar) result(length)
      integer(kind=INT32), intent(in) :: scalar

      _UNUSED_DUMMY(scalar)
      length = 1
   end function serialize_buffer_length_int32_0d


   integer function serialize_buffer_length_int32_1d(array) result(length)
      integer(kind=INT32), intent(in) :: array(:)

      length = 1 + size(array)
   end function serialize_buffer_length_int32_1d


   integer function serialize_buffer_length_real32_0d(scalar) result(length)
      real(kind=REAL32), intent(in) :: scalar

      _UNUSED_DUMMY(scalar)
      length = word_size(pFIO_REAL32)
   end function serialize_buffer_length_real32_0d


   integer function serialize_buffer_length_real32_1d(array) result(length)
      real(kind=REAL32), intent(in) :: array(:)

      length = 1 + size(array)*word_size(pFIO_REAL32)

   end function serialize_buffer_length_real32_1d

   integer function serialize_buffer_length_logical_0d(scalar) result(length)
      logical, intent(in) :: scalar

      _UNUSED_DUMMY(scalar)
      length = word_size(pFIO_LOGICAL)
   end function serialize_buffer_length_logical_0d


   integer function serialize_buffer_length_logical_1d(array) result(length)
      logical, intent(in) :: array(:)

      length = 1 + size(array)*word_size(pFIO_LOGICAL)

   end function serialize_buffer_length_logical_1d

!!! serializing

   function serialize_string(str) result(buffer)
      integer(kind=INT32), allocatable :: buffer(:)
      character(len=*), intent(in) :: str

      integer(kind=INT32) :: str_len

      str_len = len(str)
      buffer = [0, str_len, transfer(str,[1])]
      buffer(1) = size(buffer)

   end function serialize_string


   function serialize_int32_0d(scalar) result(buffer)
      integer(kind=INT32), allocatable :: buffer(:)
      integer(kind=INT32), intent(in) :: scalar

      integer(kind=INT32) :: n

      n = 1
      buffer = [scalar]

   end function serialize_int32_0d


   function serialize_int32_1d(array) result(buffer)
      integer(kind=INT32), allocatable :: buffer(:)
      integer(kind=INT32), intent(in) :: array(:)

      integer(kind=INT32) :: n

      n = size(array)
      buffer = [n+1, array]

   end function serialize_int32_1d


   function serialize_real32_0d(scalar) result(buffer)
      integer(kind=INT32), allocatable :: buffer(:)
      real(kind=REAL32), intent(in) :: scalar

      integer(kind=INT32) :: n

      n = 1
      buffer = [transfer(scalar,1)]

   end function serialize_real32_0d


   function serialize_real32_1d(array) result(buffer)
      integer(kind=INT32), allocatable :: buffer(:)
      real(kind=REAL32), intent(in) :: array(:)

      integer(kind=INT32) :: n

      n = size(array)*word_size(pFIO_REAL32)
      buffer = [n+1, transfer(array,[1])]

   end function serialize_real32_1d

   function serialize_logical_0d(scalar) result(buffer)
      integer(kind=INT32), allocatable :: buffer(:)
      logical, intent(in) :: scalar

      integer(kind=INT32) :: n

      n = 1
      buffer = [transfer(scalar,1)]

   end function serialize_logical_0d


   function serialize_logical_1d(array) result(buffer)
      integer(kind=INT32), allocatable :: buffer(:)
      logical, intent(in) :: array(:)

      integer(kind=INT32) :: n

      n = size(array)*word_size(pFIO_LOGICAL)
      buffer = [n+1, transfer(array,[1])]

   end function serialize_logical_1d

!!! deserializing

   subroutine deserialize_string(buffer, str)
      integer(kind=INT32), intent(in) :: buffer(:)
      character(len=:), allocatable :: str

      integer(kind=INT32) :: buf_size, str_len

      buf_size = buffer(1)
      str_len = buffer(2)
      allocate(character(str_len) :: str)
      str = transfer(buffer(3:buf_size), str)

   end subroutine deserialize_string

   
   subroutine deserialize_int32_0d(buffer, scalar)
      integer(kind=INT32), intent(in) :: buffer(:)
      integer(kind=INT32), intent(out) :: scalar

      scalar = buffer(1)

   end subroutine deserialize_int32_0d


   subroutine deserialize_int32_1d(buffer, array)
      integer(kind=INT32), intent(in) :: buffer(:)
      integer(kind=INT32), allocatable, intent(out) :: array(:)

      integer(kind=INT32) :: n

      n = buffer(1)
      array = buffer(2:n)

   end subroutine deserialize_int32_1d

   subroutine deserialize_real32_0d(buffer, scalar)
      integer(kind=INT32), intent(in) :: buffer(:)
      real(kind=REAL32), intent(out) :: scalar

      scalar = transfer(buffer(1),scalar)

   end subroutine deserialize_real32_0d


   subroutine deserialize_real32_1d(buffer, array)
      integer(kind=INT32), intent(in) :: buffer(:)
      real(kind=REAL32), allocatable, intent(out) :: array(:)

      integer(kind=INT32) :: n

      n = buffer(1)
      allocate(array(n-1))
      array = transfer(buffer(2:n),array)

   end subroutine deserialize_real32_1d

   subroutine deserialize_logical_0d(buffer, scalar)
      integer(kind=INT32), intent(in) :: buffer(:)
      logical, intent(out) :: scalar

      scalar = transfer(buffer(1),scalar)

   end subroutine deserialize_logical_0d


   subroutine deserialize_logical_1d(buffer, array)
      integer(kind=INT32), intent(in) :: buffer(:)
      logical, allocatable, intent(out) :: array(:)

      integer(kind=INT32) :: n

      n = buffer(1)
      allocate(array(n-1))
      array = transfer(buffer(2:n),array)

   end subroutine deserialize_logical_1d

   ! In multiples of default integer ...
   integer function word_size(type_kind)
      integer, intent(in) :: type_kind

      select case(type_kind)
      case (pFIO_INT32, pFIO_REAL32, pFIO_LOGICAL)
         word_size = 1
      case (pFIO_REAL64)
         word_size = 2
      case default
         word_size = -1 ! unsupported  (should throw an exception)
      end select
         
   end function word_size


end module pFIO_UtilitiesMod
