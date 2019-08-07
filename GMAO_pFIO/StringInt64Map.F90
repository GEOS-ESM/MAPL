module pFIO_StringInt64MapMod
   use, intrinsic :: iso_fortran_env, only: INT64
   use pFIO_ThrowMod
   use ESMF

   ! Create a map (associative array) between names and integers.

 
#include "types/key_deferredLengthString.inc"   
#define _value integer(kind=INT64)
#define _value_equal_defined
#define _value_less_than_defined
#define _map StringInt64Map
#define _iterator StringInt64MapIterator

#define _alt
#define _FTL_THROW pFIO_throw_exception

#include "templates/map.inc"

end module pFIO_StringInt64MapMod

module pFIO_StringInt64MapUtilMod
   use, intrinsic :: iso_fortran_env, only: INT64
   use pFIO_UtilitiesMod
   use pFIO_StringInt64MapMod
   implicit none
   private
   public :: StringInt64Map_serialize
   public :: StringInt64Map_deserialize

contains

    subroutine StringInt64Map_serialize(map,buffer)
       type(StringInt64Map), intent(in):: map
       integer, allocatable, intent(inout) :: buffer(:)
       type(StringInt64MapIterator) :: iter
       character(len=:), pointer :: key
       integer(kind=INT64), pointer :: value
       integer :: length
 
       if (allocated(buffer)) deallocate(buffer)
       allocate(buffer(0))
       iter = map%begin()
       do while (iter /= map%end())
          key => iter%key()
          buffer=[buffer,serialize_intrinsic(key)]
          value => iter%value()
          buffer = [buffer, serialize_intrinsic(value)]
          call iter%next() 
       enddo
       length = serialize_buffer_length(length) + size(buffer)
       buffer = [serialize_intrinsic(length),buffer]

    end subroutine StringInt64Map_serialize  

    function StringInt64Map_deserialize(buffer) result(map)
       type (StringInt64Map) :: map
       integer, intent(in) :: buffer(:)

       character(len=:),allocatable :: key
       integer(kind=INT64) :: value
       integer :: length,n,n0,n1,n2

       n = 1
       call deserialize_intrinsic(buffer(n:),length)
       n0 = serialize_buffer_length(length)
       n = n + n0
       length = length - n0

       do while (length > 0)
          call deserialize_intrinsic(buffer(n:),key)
          n1 = serialize_buffer_length(key)
          n = n + n1
          call deserialize_intrinsic(buffer(n:n+1),value)
          n2 = serialize_buffer_length(value)
          n = n + n2
          length = length - n1 - n2
          call map%insert(key,value)
          deallocate(key)
       enddo
    end function StringInt64Map_deserialize

end module pFIO_StringInt64MapUtilMod
