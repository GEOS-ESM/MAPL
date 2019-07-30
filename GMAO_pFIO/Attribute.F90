#include "pFIO_ErrLog.h"
#include "unused_dummy.H"

module pFIO_AttributeMod

! limit the dimesion to 0 or 1
!
   use pFIO_UnlimitedEntityMod
   use pFIO_ErrorHandlingMod

   implicit none
   private

   public :: Attribute
   public :: StringWrap

   type,extends(UnlimitedEntity) :: Attribute
   contains
      generic :: operator(==) => equal_attr
      generic :: operator(/=) => not_equal_attr
      procedure :: equal_attr
      procedure :: not_equal_attr
   end type Attribute

   interface Attribute
      module procedure new_Attribute_0d ! scalar constructor
      module procedure new_Attribute_1d ! vector constructor
   end interface Attribute

contains


   function new_Attribute_0d(value, rc) result(attr)
      type (Attribute) :: attr
      integer, optional, intent(out) :: rc
      integer :: status
      class (*), intent(in) :: value
      attr%UnlimitedEntity = UnlimitedEntity(value, status)
      _VERIFY(status)
      _RETURN(_SUCCESS)
   end function new_Attribute_0d
   
   function new_Attribute_1d(values, rc) result(attr)
      type (Attribute) :: attr
      integer, optional, intent(out) :: rc
      integer :: status
      class (*), intent(in) :: values(:)

      attr%UnlimitedEntity = UnlimitedEntity(values,status)
      _VERIFY(status)
      _RETURN(_SUCCESS)

   end function new_Attribute_1d

   logical function equal_attr(a, b)
      class (Attribute), target, intent(in) :: a
      type (Attribute),  target, intent(in) :: b
    
      equal_attr = (a%UnlimitedEntity == b%UnlimitedEntity)

   end function equal_attr

   logical function not_equal_attr(a, b)

      class (Attribute), target, intent(in) :: a
      type (Attribute),  target, intent(in) :: b
    
      not_equal_attr = .not. (a == b)

   end function not_equal_attr

end module pFIO_AttributeMod


! The following module defines an FTL map (associative array) with keys that are deferred
! length strings and values that are Attributes.

module pFIO_StringAttributeMapMod
   use pFIO_ThrowMod
   use ESMF
   use pFIO_AttributeMod
   
#include "types/key_deferredLengthString.inc"   
#define _value type (Attribute)
#define _value_equal_defined

#define _map StringAttributeMap
#define _iterator StringAttributeMapIterator

#define _alt
#define _FTL_THROW pFIO_throw_exception

#include "templates/map.inc"
   
end module pFIO_StringAttributeMapMod

module pFIO_StringAttributeMapUtilMod
   use pFIO_UtilitiesMod
   use pFIO_AttributeMod
   use pFIO_StringAttributeMapMod
   use pFIO_ErrorHandlingMod
   implicit none
   private

   public :: StringAttributeMap_serialize
   public :: StringAttributeMap_deserialize

contains

    subroutine StringAttributeMap_serialize(map,buffer, rc)
       type (StringAttributeMap) ,intent(in):: map
       integer, allocatable,intent(inout) :: buffer(:)
       integer, optional, intent(out) :: rc

       type (StringAttributeMapIterator) :: iter
       character(len=:),pointer :: key
       type(Attribute),pointer :: attr_ptr
       integer :: length
       integer, allocatable :: tmp_buffer(:)

       if (allocated(buffer)) deallocate(buffer)
       allocate(buffer(0))
       iter = map%begin()
       do while (iter /= map%end())
          key => iter%key()
          buffer=[buffer,serialize_intrinsic(key)]
          attr_ptr => iter%value()
          call attr_ptr%serialize(tmp_buffer)
          buffer = [buffer, tmp_buffer]
          deallocate(tmp_buffer)
          call iter%next()
       enddo
       length = serialize_buffer_length(length)+size(buffer)
       buffer = [serialize_intrinsic(length),buffer]
       _RETURN(_SUCCESS)
    end subroutine StringAttributeMap_serialize

    function StringAttributeMap_deserialize(buffer, rc) result(map)
       type (StringAttributeMap) :: map
       integer, intent(in) :: buffer(:)
       integer, optional, intent(out) :: rc

       character(len=:),allocatable :: key
       integer :: length,n,n0,n1,n2
       type (Attribute), allocatable :: attr
       integer :: status

       n = 1
       call deserialize_intrinsic(buffer(n:),length)
       _ASSERT(length == size(buffer), "length does not match")

       n0 = serialize_buffer_length(length)
       n = n + n0
       length = length - n0

       do while (length > 0)
          call deserialize_intrinsic(buffer(n:),key)
          n1 = serialize_buffer_length(key)
          n = n + n1
          allocate(attr)
          call deserialize_intrinsic(buffer(n:),n2)
          call attr%deserialize(buffer(n:n+n2-1), status)
          _VERIFY(status)
          n = n + n2
          length = length - n1 - n2
          call map%insert(key,attr)
          deallocate(key)
          deallocate(attr)
       enddo
       _RETURN(_SUCCESS)
    end function StringAttributeMap_deserialize

end module pFIO_StringAttributeMapUtilMod
