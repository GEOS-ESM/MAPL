#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_UtilitiesMod
   use, intrinsic :: iso_c_binding, only: c_sizeof 
   use, intrinsic :: iso_c_binding, only: c_bool
   use, intrinsic :: iso_fortran_env, only: INT32,REAL32,INT64,REAL64
   use pFIO_ConstantsMod
   use MAPL_ExceptionHandling
   implicit none
   private

   public :: serialize_buffer_length
   public :: serialize_intrinsic
   public :: deserialize_intrinsic
   public :: nearlyEqual
   public :: word_size
   public :: i_to_string

   interface serialize_buffer_length
      module procedure serialize_buffer_length_string
      module procedure serialize_buffer_length_int32_0d
      module procedure serialize_buffer_length_int32_1d
      module procedure serialize_buffer_length_int64_0d
      module procedure serialize_buffer_length_int64_1d
      module procedure serialize_buffer_length_real32_0d
      module procedure serialize_buffer_length_real32_1d
      module procedure serialize_buffer_length_real64_0d
      module procedure serialize_buffer_length_real64_1d
      module procedure serialize_buffer_length_logical_0d
      module procedure serialize_buffer_length_logical_1d
   end interface serialize_buffer_length

   interface serialize_intrinsic
      module procedure serialize_string
      module procedure serialize_int32_0d
      module procedure serialize_int32_1d
      module procedure serialize_int64_0d
      module procedure serialize_int64_1d
      module procedure serialize_real32_0d
      module procedure serialize_real32_1d
      module procedure serialize_real64_0d
      module procedure serialize_real64_1d
      module procedure serialize_logical_0d
      module procedure serialize_logical_1d
   end interface serialize_intrinsic

   interface deserialize_intrinsic
      module procedure deserialize_string
      module procedure deserialize_int32_0d
      module procedure deserialize_int32_1d
      module procedure deserialize_int64_0d
      module procedure deserialize_int64_1d
      module procedure deserialize_real32_0d
      module procedure deserialize_real32_1d
      module procedure deserialize_real64_0d
      module procedure deserialize_real64_1d
      module procedure deserialize_logical_0d
      module procedure deserialize_logical_1d
   end interface deserialize_intrinsic

   interface nearlyEqual
     module procedure nearlyEqual_real32
     module procedure nearlyEqual_real64
   end interface nearlyEqual

   integer, parameter :: CHARS_PER_INT32 = 4

contains

!!! length

   integer function serialize_buffer_length_string(str, rc) result(length)
      character(len=*),  intent(in) :: str
      integer, optional, intent(out) :: rc

      integer :: n

      n = len(str)
      ! two words in header
      length = 1 + 1 + (1 + (n-1)/CHARS_PER_INT32)

      _RETURN(_SUCCESS)
   end function serialize_buffer_length_string

   integer function serialize_buffer_length_int32_0d(scalar, rc) result(length)
      integer(kind=INT32), intent(in) :: scalar
      integer, optional, intent(out) :: rc

      length = 1
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(scalar)
   end function serialize_buffer_length_int32_0d

   integer function serialize_buffer_length_int32_1d(array, rc) result(length)
      integer(kind=INT32), intent(in) :: array(:)
      integer, optional, intent(out) :: rc

      length = 1 + size(array)
      _RETURN(_SUCCESS)
   end function serialize_buffer_length_int32_1d

   integer function serialize_buffer_length_int64_0d(scalar, rc) result(length)
      integer(kind=INT64), intent(in) :: scalar
      integer, optional, intent(out) :: rc

      length = 2
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(scalar)
   end function serialize_buffer_length_int64_0d

   integer function serialize_buffer_length_int64_1d(array, rc) result(length)
      integer(kind=INT64), intent(in) :: array(:)
      integer, optional, intent(out) :: rc

      length = 1 + size(array)*word_size(pFIO_INT64)
      _RETURN(_SUCCESS)
   end function serialize_buffer_length_int64_1d

   integer function serialize_buffer_length_real32_0d(scalar,rc) result(length)
      real(kind=REAL32), intent(in) :: scalar
      integer, optional, intent(out) :: rc

      length = word_size(pFIO_REAL32)
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(scalar)
   end function serialize_buffer_length_real32_0d

   integer function serialize_buffer_length_real32_1d(array, rc) result(length)
      real(kind=REAL32), intent(in) :: array(:)
      integer, optional, intent(out) :: rc

      length = 1 + size(array)*word_size(pFIO_REAL32)

      _RETURN(_SUCCESS)
   end function serialize_buffer_length_real32_1d

   integer function serialize_buffer_length_real64_0d(scalar, rc) result(length)
      real(kind=REAL64), intent(in) :: scalar
      integer, optional, intent(out) :: rc

      length = word_size(pFIO_REAL64)
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(scalar)
   end function serialize_buffer_length_real64_0d


   integer function serialize_buffer_length_real64_1d(array, rc) result(length)
      real(kind=REAL64), intent(in) :: array(:)
      integer, optional, intent(out) :: rc

      length = 1 + size(array)*word_size(pFIO_REAL64)
      _RETURN(_SUCCESS)
   end function serialize_buffer_length_real64_1d

   integer function serialize_buffer_length_logical_0d(scalar, rc) result(length)
      logical, intent(in) :: scalar
      integer, optional, intent(out) :: rc

      length = word_size(pFIO_LOGICAL)
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(scalar)
   end function serialize_buffer_length_logical_0d


   integer function serialize_buffer_length_logical_1d(array, rc) result(length)
      logical, intent(in) :: array(:)
      integer, optional, intent(out) :: rc

      length = 1 + size(array)*word_size(pFIO_LOGICAL)
      _RETURN(_SUCCESS)
   end function serialize_buffer_length_logical_1d

!!! serializing

   function serialize_string(str, rc) result(buffer)
      integer(kind=INT32), allocatable :: buffer(:)
      character(len=*), intent(in) :: str
      integer, optional, intent(out) :: rc

      integer(kind=INT32) :: str_len

      str_len = len(str)
      buffer = [0, str_len, transfer(str,[1])]
      buffer(1) = size(buffer)

      _RETURN(_SUCCESS)
   end function serialize_string


   function serialize_int32_0d(scalar, rc) result(buffer)
      integer(kind=INT32), allocatable :: buffer(:)
      integer(kind=INT32), intent(in) :: scalar
      integer, optional, intent(out) :: rc

      integer(kind=INT32) :: n

      n = 1
      buffer = [scalar]

      _RETURN(_SUCCESS)
   end function serialize_int32_0d


   function serialize_int32_1d(array, rc) result(buffer)
      integer(kind=INT32), allocatable :: buffer(:)
      integer(kind=INT32), intent(in) :: array(:)
      integer, optional, intent(out) :: rc

      integer(kind=INT32) :: n

      n = size(array)
      buffer = [n+1, array]
      _RETURN(_SUCCESS)
   end function serialize_int32_1d

   function serialize_int64_0d(scalar, rc) result(buffer)
      integer(kind=INT32), allocatable :: buffer(:)
      integer(kind=INT64), intent(in) :: scalar
      integer, optional, intent(out) :: rc

      buffer = [transfer(scalar,[1])]

      _RETURN(_SUCCESS)
   end function serialize_int64_0d

   function serialize_int64_1d(array, rc) result(buffer)
      integer(kind=INT32), allocatable :: buffer(:)
      integer(kind=INT64), intent(in) :: array(:)
      integer, optional, intent(out) :: rc

      integer(kind=INT32) :: n

      n = size(array)*word_size(pFIO_INT64)
      buffer = [n+1, transfer(array,[1])]

      _RETURN(_SUCCESS)
   end function serialize_int64_1d

   function serialize_real32_0d(scalar, rc) result(buffer)
      integer(kind=INT32), allocatable :: buffer(:)
      real(kind=REAL32), intent(in) :: scalar
      integer, optional, intent(out) :: rc

      buffer = [transfer(scalar,1)]

      _RETURN(_SUCCESS)
   end function serialize_real32_0d


   function serialize_real32_1d(array, rc) result(buffer)
      integer(kind=INT32), allocatable :: buffer(:)
      real(kind=REAL32), intent(in) :: array(:)
      integer, optional, intent(out) :: rc

      integer(kind=INT32) :: n

      n = size(array)*word_size(pFIO_REAL32)
      buffer = [n+1, transfer(array,[1])]

      _RETURN(_SUCCESS)
   end function serialize_real32_1d

   function serialize_real64_0d(scalar, rc) result(buffer)
      integer(kind=INT32), allocatable :: buffer(:)
      real(kind=REAL64), intent(in) :: scalar
      integer, optional, intent(out) :: rc

      buffer = [transfer(scalar,[1])]

      _RETURN(_SUCCESS)
   end function serialize_real64_0d


   function serialize_real64_1d(array, rc) result(buffer)
      integer(kind=INT32), allocatable :: buffer(:)
      real(kind=REAL64), intent(in) :: array(:)
      integer, optional, intent(out) :: rc

      integer(kind=INT32) :: n

      n = size(array)*word_size(pFIO_REAL64)
      buffer = [n+1, transfer(array,[1])]

      _RETURN(_SUCCESS)
   end function serialize_real64_1d

   function serialize_logical_0d(scalar, rc) result(buffer)
      integer(kind=INT32), allocatable :: buffer(:)
      logical, intent(in) :: scalar
      integer, optional, intent(out) :: rc
     
      allocate(buffer(1), source = 0)
      if(scalar) buffer(1) = 1

      _RETURN(_SUCCESS)
   end function serialize_logical_0d


   function serialize_logical_1d(array, rc) result(buffer)
      integer(kind=INT32), allocatable :: buffer(:)
      logical, intent(in) :: array(:)
      integer, optional, intent(out) :: rc

      integer, allocatable :: itmp(:)
      integer(kind=INT32) :: n

      n = size(array)*word_size(pFIO_LOGICAL)
      allocate(itmp(n), source = 0)
      where(array)
         itmp = 1
      endwhere
      buffer = [n+1, itmp]

      _RETURN(_SUCCESS)
   end function serialize_logical_1d

!!! deserializing

   subroutine deserialize_string(buffer, str, rc)
      integer(kind=INT32), intent(in) :: buffer(:)
      character(len=:), allocatable :: str
      integer, optional, intent(out) :: rc

      integer(kind=INT32) :: buf_size, str_len
      
      _ASSERT(size(buffer) >= 2, "wrong buffer")      

      buf_size = buffer(1)
      str_len = buffer(2)
      allocate(character(str_len) :: str)
      str = transfer(buffer(3:buf_size), str)

      _RETURN(_SUCCESS)
   end subroutine deserialize_string

   
   subroutine deserialize_int32_0d(buffer, scalar, rc)
      integer(kind=INT32), intent(in) :: buffer(:)
      integer(kind=INT32), intent(out) :: scalar
      integer, optional, intent(out) :: rc

      _ASSERT(size(buffer) >= 1, "wrong buffer")      
      scalar = buffer(1)

      _RETURN(_SUCCESS)
   end subroutine deserialize_int32_0d


   subroutine deserialize_int32_1d(buffer, array, rc)
      integer(kind=INT32), intent(in) :: buffer(:)
      integer(kind=INT32), allocatable, intent(out) :: array(:)
      integer, optional, intent(out) :: rc

      integer(kind=INT32) :: n

      _ASSERT(size(buffer) >= 1, "wrong buffer")      

      n = buffer(1)
      array = buffer(2:n)

      _RETURN(_SUCCESS)
   end subroutine deserialize_int32_1d

   subroutine deserialize_int64_0d(buffer, scalar, rc)
      integer(kind=INT32), intent(in) :: buffer(:)
      integer(kind=INT64), intent(out) :: scalar
      integer, optional, intent(out) :: rc

      _ASSERT(size(buffer) >= 1, "wrong buffer")      

      scalar = transfer(buffer(1:), scalar)

      _RETURN(_SUCCESS)
   end subroutine deserialize_int64_0d


   subroutine deserialize_int64_1d(buffer, array, rc)
      integer(kind=INT32), intent(in) :: buffer(:)
      integer(kind=INT64), allocatable, intent(out) :: array(:)
      integer, optional, intent(out) :: rc

      integer(kind=INT32) :: n

      _ASSERT(size(buffer) >= 1, "wrong buffer")      

      n = buffer(1)
      allocate(array(n-1))
      array = transfer(buffer(2:n),array)

      _RETURN(_SUCCESS)
   end subroutine deserialize_int64_1d

   subroutine deserialize_real32_0d(buffer, scalar, rc)
      integer(kind=INT32), intent(in) :: buffer(:)
      real(kind=REAL32), intent(out) :: scalar
      integer, optional, intent(out) :: rc

      _ASSERT(size(buffer) >= 1, "wrong buffer")      
      scalar = transfer(buffer(1),scalar)
      _RETURN(_SUCCESS)
   end subroutine deserialize_real32_0d


   subroutine deserialize_real32_1d(buffer, array, rc)
      integer(kind=INT32), intent(in) :: buffer(:)
      real(kind=REAL32), allocatable, intent(out) :: array(:)
      integer, optional, intent(out) :: rc

      integer(kind=INT32) :: n

      _ASSERT(size(buffer) >= 1, "wrong buffer")      

      n = buffer(1)
      allocate(array(n-1))
      array = transfer(buffer(2:n),array)

      _RETURN(_SUCCESS)
   end subroutine deserialize_real32_1d

   subroutine deserialize_real64_0d(buffer, scalar, rc)
      integer(kind=INT32), intent(in) :: buffer(:)
      real(kind=REAL64), intent(out) :: scalar
      integer, optional, intent(out) :: rc

      _ASSERT(size(buffer) >= 1, "wrong buffer")      
      scalar = transfer(buffer(1:),scalar)
      _RETURN(_SUCCESS)
   end subroutine deserialize_real64_0d


   subroutine deserialize_real64_1d(buffer, array, rc)
      integer(kind=INT32), intent(in) :: buffer(:)
      real(kind=REAL64), allocatable, intent(out) :: array(:)
      integer, optional, intent(out) :: rc

      integer(kind=INT32) :: n


      _ASSERT(size(buffer) >= 1, "wrong buffer")      
      n = buffer(1)
      allocate(array(n-1))
      array = transfer(buffer(2:n),array)

      _RETURN(_SUCCESS)
   end subroutine deserialize_real64_1d

   subroutine deserialize_logical_0d(buffer, scalar, rc)
      integer(kind=INT32), intent(in) :: buffer(:)
      logical, intent(out) :: scalar
      integer, optional, intent(out) :: rc

      _ASSERT(size(buffer) >= 1, "wrong buffer")      
      scalar = buffer(1) /= 0

      _RETURN(_SUCCESS)
   end subroutine deserialize_logical_0d


   subroutine deserialize_logical_1d(buffer, array, rc)
      integer(kind=INT32), intent(in) :: buffer(:)
      logical, allocatable, intent(out) :: array(:)
      integer, optional, intent(out) :: rc

      integer(kind=INT32) :: n

      _ASSERT(size(buffer) >= 1, "wrong buffer")      
      n = buffer(1)
      allocate(array(n-1))
     
      array = buffer(2:n) /= 0

      _RETURN(_SUCCESS)
   end subroutine deserialize_logical_1d

   elemental function nearlyEqual_real32(r1, r2) result(yes)
      real(kind=REAL32), intent(in) :: r1
      real(kind=REAL32), intent(in) :: r2
      logical :: yes
      real(kind=REAL32) :: eps
      ! WY note: relax the nearly equal by 5.0* epsilon  ?
      eps = epsilon(r1) ! *5.0
      yes = (abs(r1-r2) <=  maxval([abs(r1),abs(r2)])*eps)
   end function

   elemental function nearlyEqual_real64(r1, r2) result(yes)
      real(kind=REAL64), intent(in) :: r1
      real(kind=REAL64), intent(in) :: r2
      logical :: yes
      real(kind=REAL64) :: eps
      ! WY note: relax the nearly equal by 5.0* epsilon ?
      eps = epsilon(r1) ! *5.0
      yes = (abs(r1-r2) <=  maxval([abs(r1),abs(r2)])*eps)
   end function

   ! In multiples of default integer ...
   integer function word_size(type_kind,rc)
      integer, intent(in) :: type_kind
      integer, optional, intent(out) :: rc
      integer(kind=INT32) :: i32
      integer(kind=INT64) :: i64
      real (kind=REAL32) :: r32
      real (kind=REAL64) :: r64
      logical(kind=C_BOOL) :: l

      select case(type_kind)
      case (pFIO_INT32)
         word_size = 1
      case (pFIO_REAL32)
         word_size = c_sizeof(r32)/c_sizeof(i32)
      case (pFIO_LOGICAL)
         word_size = 1
      case (pFIO_REAL64)
         word_size = c_sizeof(r64)/c_sizeof(i32)
      case (pFIO_INT64)
         word_size = c_sizeof(i64)/c_sizeof(i32)
      case default
         _ASSERT(.false., "unsupported type kind")
      end select
         
      _RETURN(_SUCCESS)
   end function word_size

   function i_to_string(count, rc) result(str)
      character(len=:), allocatable :: str
      integer, intent(in) :: count
      integer, optional, intent(out) :: rc
      character(len=9)    :: buffer
      _ASSERT( count <= 10**8, "too big to hold")
      write(buffer,'(i0)') count
      str = trim(buffer)

      _RETURN(_SUCCESS)
   end function i_to_string

end module pFIO_UtilitiesMod
