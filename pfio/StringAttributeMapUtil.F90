#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_StringAttributeMapUtilMod
   use pFIO_UtilitiesMod
   use pFIO_AttributeMod
   use pFIO_StringAttributeMapMod
   use MAPL_ExceptionHandling
   implicit none
   private

   public :: StringAttributeMap_serialize
   public :: StringAttributeMap_deserialize

contains

    subroutine StringAttributeMap_serialize(map,buffer, rc)
       type (StringAttributeMap), target, intent(in):: map
       integer, allocatable,intent(inout) :: buffer(:)
       integer, optional, intent(out) :: rc

       type (StringAttributeMapIterator) :: iter
       character(len=:),pointer :: key
       type(Attribute),pointer :: attr_ptr
       integer :: length
       integer, allocatable :: tmp_buffer(:)

       if (allocated(buffer)) deallocate(buffer)
       allocate(buffer(0))
       iter = map%ftn_begin()
       do while (iter /= map%ftn_end())
          call iter%next()

          key => iter%first()
          buffer=[buffer,serialize_intrinsic(key)]
          attr_ptr => iter%second()
          call attr_ptr%serialize(tmp_buffer)
          buffer = [buffer, tmp_buffer]
          deallocate(tmp_buffer)
       enddo
       length = serialize_buffer_length(length)+size(buffer)
       buffer = [serialize_intrinsic(length),buffer]

       _RETURN(_SUCCESS)
    end subroutine StringAttributeMap_serialize

    subroutine StringAttributeMap_deserialize(buffer, map, rc)
       integer, intent(in) :: buffer(:)
       type (StringAttributeMap), intent(inout) :: map
       integer, optional, intent(out) :: rc

       character(len=:),allocatable :: key
       integer :: length,n,n0,n1,n2
       type (Attribute) :: attr
       integer :: status

       n = 1
       call deserialize_intrinsic(buffer(n:),length)
       _ASSERT(length == size(buffer), "length does not match")

       n0 = serialize_buffer_length(length)
       n = n + n0
       length = length - n0

       map = StringAttributeMap()
       do while (length > 0)
          call deserialize_intrinsic(buffer(n:),key)
          n1 = serialize_buffer_length(key)
          n = n + n1
          !allocate(attr)
          call deserialize_intrinsic(buffer(n:),n2)
          call Attribute_deserialize(buffer(n:n+n2-1), attr, status)
          _VERIFY(status)
          n = n + n2
          length = length - n1 - n2
          call map%insert(key,attr)
          deallocate(key)
          !deallocate(attr)
       enddo
       _RETURN(_SUCCESS)
    end subroutine StringAttributeMap_deserialize

end module pFIO_StringAttributeMapUtilMod
