#include "pFIO_ErrLog.h"
#include "unused_dummy.H"

module pFIO_StringVariableMapMod
   use pFIO_ThrowMod
   use ESMF
   use pFIO_VariableMod
   use pFIO_CoordinateVariableMod 

   ! Create a map (associative array) between names and pFIO_Variables.

#include "types/key_deferredLengthString.inc"
#define _value class (Variable)
#define _value_allocatable
#define _value_equal_defined

! Workarounds for Intel 18 - does not correctly assign to polymorphic subcomponents
#define _ASSIGN(dest,src) allocate(dest%key,source=src%key); if(allocated(src%value)) allocate(dest%value,source=src%value)
#define _MOVE(dest,src) call move_alloc(from=src%key,to=dest%key); if (allocated(src%value)) call move_alloc(from=src%value,to=dest%value)
#define _FREE(x) deallocate(x%key,x%value)
#define _map StringVariableMap
#define _iterator StringVariableMapIterator

#define _alt
#define _FTL_THROW pFIO_throw_exception

#include "templates/map.inc"

#undef _alt
#undef _map
#undef _iterator
#undef _value
#undef _value_allocatable
#undef _value_equal_defined
end module pFIO_StringVariableMapMod

module pFIO_StringVariableMapUtilMod
   use pFIO_ErrorHandlingMod
   use pFIO_UtilitiesMod
   use pFIO_VariableMod
   use pFIO_CoordinateVariableMod
   use pFIO_StringVariableMapMod
   implicit none
   private
   public :: StringVariableMap_get_length
   public :: StringVariableMap_serialize
   public :: StringVariableMap_deserialize

contains
 
    integer function StringVariableMap_get_length(this) result(length)
      type (StringVariableMap), intent(in) :: this
      integer, allocatable :: buffer(:)

      call StringVariableMap_serialize(this, buffer)
      length = size(buffer)

    end function StringVariableMap_get_length

    subroutine StringVariableMap_serialize(map, buffer, rc)
       type (StringVariableMap) ,intent(in):: map
       integer, allocatable,intent(inout) :: buffer(:)
       integer, optional, intent(out) :: rc

       type (StringVariableMapIterator) :: iter
       character(len=:),pointer :: key
       class(Variable),pointer :: var_ptr
       integer :: length, status
       integer, allocatable :: tmp_buffer(:)

       if (allocated(buffer)) deallocate(buffer)
       allocate(buffer(0))
       iter = map%begin()
       do while (iter /= map%end())
          key => iter%key()
          buffer=[buffer,serialize_intrinsic(key)]
          var_ptr => iter%value()
          call var_ptr%serialize(tmp_buffer, status)
          _VERIFY(status)
          buffer = [buffer, tmp_buffer]
          deallocate(tmp_buffer)
          call iter%next()
       enddo
       length = serialize_buffer_length(length)+size(buffer)
       buffer = [serialize_intrinsic(length),buffer]
       _RETURN(_SUCCESS)
    end subroutine StringVariableMap_serialize

    function StringVariableMap_deserialize(buffer, rc) result(map)
       type (StringVariableMap) :: map
       integer, intent(in) :: buffer(:)
       integer, optional, intent(out) :: rc

       character(len=:),allocatable :: key
       integer :: length,n,n0,n1,n2, v_type
       class(Variable), allocatable :: var
       type (Variable) :: v
       type (CoordinateVariable) :: c 
       integer :: status

       n = 1
       call deserialize_intrinsic(buffer(n:),length)
       _ASSERT(length <= size(buffer), "stringVarmap length does not match")

       n0 = serialize_buffer_length(length)
       n = n + n0
       length = length - n0

       do while (length > 0)
          call deserialize_intrinsic(buffer(n:),key)
          n1 = serialize_buffer_length(key)
          n = n + n1

          ! the first one is length, the second one is type          
          call deserialize_intrinsic(buffer(n+1:),v_type)

          if (v_type == Variable_SERIALIZE_TYPE) then
             allocate(var, source = v)
          else if (v_type == Coord_SERIALIZE_TYPE) then
             allocate(var, source = c )
          endif

          call deserialize_intrinsic(buffer(n:),n2)
          call var%deserialize(buffer(n:n+n2-1), status)
          _VERIFY(status)

          n = n + n2
          length = length - n1 - n2
          call map%insert(key,var)
          deallocate(key)
          deallocate(var)
       enddo
       _RETURN(_SUCCESS)
    end function StringVariableMap_deserialize

end module pFIO_StringVariableMapUtilMod
