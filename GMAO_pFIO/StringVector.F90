module pFIO_StringVectorMod
#  include "types/deferredLengthString.inc"

#  define _vector StringVector
#  define _iterator StringVectorIterator
#  include "templates/vector.inc"
end module pFIO_StringVectorMod

module pFIO_StringVectorUtilMod
   use pFIO_UtilitiesMod
   use pFIO_AttributeMod
   use pFIO_StringVectorMod
   implicit none
   private
   public :: StringVector_serialize
   public :: StringVector_deserialize

contains

   subroutine StringVector_serialize(strVec,buffer)
       type (StringVector) ,intent(in):: strVec
       integer, allocatable,intent(inout) :: buffer(:)
       type (StringVectorIterator) :: iter
       character(len=:),pointer :: str
       integer :: length
      
       if (allocated(buffer)) deallocate(buffer)
       allocate(buffer(0))
       iter = strVec%begin()
       do while (iter /= strVec%end())
          str => iter%get()
          buffer=[buffer,serialize_intrinsic(str)]
          call iter%next()
       enddo
       length = serialize_buffer_length(length) + size(buffer)
       buffer = [serialize_intrinsic(length),buffer]
    end subroutine StringVector_serialize

    function StringVector_deserialize(buffer) result(strVec)
       type (StringVector) :: strVec
       integer, intent(in) :: buffer(:)

       character(len=:),allocatable :: str
       integer :: length,n,n1,n0

       n = 1
       call deserialize_intrinsic(buffer(n:),length)
       n0 =  serialize_buffer_length(length)
       n = n + n0
       length = length - n0

       do while (length > 0)
          call deserialize_intrinsic(buffer(n:),str)
          call strVec%push_back(str)
          n1 = serialize_buffer_length(str)
          n = n + n1
          length = length - n1
          deallocate(str)
       enddo
   end function StringVector_deserialize

end module pFIO_StringVectorUtilMod
