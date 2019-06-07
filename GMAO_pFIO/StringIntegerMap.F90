module pFIO_StringIntegerMapMod
   use pFIO_ThrowMod
   use ESMF

   ! Create a map (associative array) between names and integers.

 
#include "types/key_deferredLengthString.inc"   
#include "types/value_integer.inc"

#define _map StringIntegerMap
#define _iterator StringIntegerMapIterator

#define _alt
#define _FTL_THROW pFIO_throw_exception

#include "templates/map.inc"

end module pFIO_StringIntegerMapMod

module pFIO_StringIntegerMapUtilMod
   use pFIO_UtilitiesMod
   use pFIO_StringIntegerMapMod
   implicit none
   private
   public :: StringIntegerMap_serialize
   public :: StringIntegerMap_deserialize

contains

    subroutine StringIntegerMap_serialize(map,buffer)
       type (StringIntegerMap) ,intent(in):: map
       integer, allocatable,intent(inout) :: buffer(:)
       type (StringIntegerMapIterator) :: iter
       character(len=:),pointer :: key
       integer,pointer :: value
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

    end subroutine StringIntegerMap_serialize  

    function StringIntegerMap_deserialize(buffer) result(map)
       type (StringIntegerMap) :: map
       integer, intent(in) :: buffer(:)

       character(len=:),allocatable :: key
       integer :: value,length,n,n0,n1,n2

       n = 1
       call deserialize_intrinsic(buffer(n:),length)
       n0 = serialize_buffer_length(length)
       n = n + n0
       length = length - n0

       do while (length > 0)
          call deserialize_intrinsic(buffer(n:),key)
          n1 = serialize_buffer_length(key)
          n = n + n1
          call deserialize_intrinsic(buffer(n:),value)
          n2 = serialize_buffer_length(value)
          n = n + n2
          length = length - n1 - n2
          call map%insert(key,value)
          deallocate(key)
       enddo
    end function StringIntegerMap_deserialize

end module pFIO_StringIntegerMapUtilMod
