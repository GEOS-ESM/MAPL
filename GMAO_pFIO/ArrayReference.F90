#include "pFIO_ErrLog.h"
#include "unused_dummy.H"

module pFIO_ArrayReferenceMod
   use, intrinsic :: iso_c_binding, only: c_ptr
   use, intrinsic :: iso_c_binding, only: C_NULL_PTR
   use, intrinsic :: iso_c_binding, only: c_loc
   use, intrinsic :: iso_c_binding, only: c_f_pointer
   use, intrinsic :: iso_c_binding, only: c_associated
   use, intrinsic :: iso_fortran_env, only: INT32
   use, intrinsic :: iso_fortran_env, only: INT64
   use, intrinsic :: iso_fortran_env, only: REAL32
   use, intrinsic :: iso_fortran_env, only: REAL64
   use pFIO_UtilitiesMod, only: word_size
   use pFIO_ErrorHandlingMod
   use pFIO_ConstantsMod
   use pFIO_AbstractDataReferenceMod

   implicit none
   private

   public :: ArrayReference

   type,extends(AbstractDataReference) :: ArrayReference
   contains
      procedure :: get_length
      procedure :: serialize
      procedure :: deserialize
   end type ArrayReference

   interface ArrayReference
      module procedure new_ArrayReference_0d
      module procedure new_ArrayReference_1d
      module procedure new_ArrayReference_2d
      module procedure new_ArrayReference_3d
      module procedure new_ArrayReference_4d
      module procedure new_ArrayReference_5d
   end interface ArrayReference

contains


   function new_ArrayReference_0d(scalar, rc) result(reference)
      type (ArrayReference) :: reference
      class(*), target, intent(in) :: scalar
      integer, optional, intent(out) :: rc

      select type (scalar)
      type is (real(kind=REAL32))
         reference%base_address = c_loc(scalar)
      type is (real(kind=REAL64))
         reference%base_address = c_loc(scalar)
      type is (integer(kind=INT32))
         reference%base_address = c_loc(scalar)
      type is (integer(kind=INT64))
         reference%base_address = c_loc(scalar)
      class default
         _ASSERT(.false., "ArrayRef does not support this type")
      end select
      reference%shape = shape(scalar)
      reference%type_kind = type_kind(scalar)
      _RETURN(_SUCCESS)
   end function new_ArrayReference_0d

   function new_ArrayReference_1d(array, rc) result(reference)
      type (ArrayReference) :: reference
      class(*), target, intent(in) :: array(:)
      integer, optional, intent(out) :: rc

      select type (array)
      type is (real(kind=REAL32))
         reference%base_address = c_loc(array)
      type is (real(kind=REAL64))
         reference%base_address = c_loc(array)
      type is (integer(kind=INT32))
         reference%base_address = c_loc(array)
      type is (integer(kind=INT64))
         reference%base_address = c_loc(array)
      class default
         _ASSERT(.false., "ArrayRef does not support this type")
      end select

      reference%shape = shape(array)
      reference%type_kind = type_kind(array(1))
      _RETURN(_SUCCESS)
      
   end function new_ArrayReference_1d

   function new_ArrayReference_2d(array, rc) result(reference)
      type (ArrayReference) :: reference
      class(*), target, intent(in) :: array(:,:)
      integer, optional, intent(out) :: rc

      select type (array)
      type is (real(kind=REAL32))
         reference%base_address = c_loc(array)
      type is (real(kind=REAL64))
         reference%base_address = c_loc(array)
      type is (integer(kind=INT32))
         reference%base_address = c_loc(array)
      type is (integer(kind=INT64))
         reference%base_address = c_loc(array)
      class default
         _ASSERT(.false., "ArrayRef does not support this type")
      end select

      reference%shape = shape(array)
      reference%type_kind = type_kind(array(1,1))
      _RETURN(_SUCCESS)
   end function new_ArrayReference_2d

   function new_ArrayReference_3d(array, rc) result(reference)
      type (ArrayReference) :: reference
      class(*), target, intent(in) :: array(:,:,:)
      integer, optional, intent(out) :: rc

      select type (array)
      type is (real(kind=REAL32))
         reference%base_address = c_loc(array)
      type is (real(kind=REAL64))
         reference%base_address = c_loc(array)
      type is (integer(kind=INT32))
         reference%base_address = c_loc(array)
      type is (integer(kind=INT64))
         reference%base_address = c_loc(array)
      class default
         _ASSERT(.false., "ArrayRef does not support this type")
      end select

      reference%shape = shape(array)
      reference%type_kind = type_kind(array(1,1,1))
      _RETURN(_SUCCESS)
      
   end function new_ArrayReference_3d


   function new_ArrayReference_4d(array, rc) result(reference)
      type (ArrayReference) :: reference
      class(*), target, intent(in) :: array(:,:,:,:)
      integer, optional, intent(out) :: rc

      select type (array)
      type is (real(kind=REAL32))
         reference%base_address = c_loc(array)
      type is (real(kind=REAL64))
         reference%base_address = c_loc(array)
      type is (integer(kind=INT32))
         reference%base_address = c_loc(array)
      type is (integer(kind=INT64))
         reference%base_address = c_loc(array)
      class default
         _ASSERT(.false., "ArrayRef does not support this type")
      end select

      reference%shape = shape(array)
      reference%type_kind = type_kind(array(1,1,1,1))
      _RETURN(_SUCCESS)
      
   end function new_ArrayReference_4d

   function new_ArrayReference_5d(array, rc) result(reference)
      type (ArrayReference) :: reference
      class(*), target, intent(in) :: array(:,:,:,:,:)
      integer, optional, intent(out) :: rc

      select type (array)
      type is (real(kind=REAL32))
         reference%base_address = c_loc(array)
      type is (real(kind=REAL64))
         reference%base_address = c_loc(array)
      type is (integer(kind=INT32))
         reference%base_address = c_loc(array)
      type is (integer(kind=INT64))
         reference%base_address = c_loc(array)
      class default
         _ASSERT(.false., "ArrayRef does not support this type")
      end select

      reference%shape = shape(array)
      reference%type_kind = type_kind(array(1,1,1,1,1))
      _RETURN(_SUCCESS)
      
   end function new_ArrayReference_5d

   integer function type_kind(element, rc)
      class(*), intent(in) :: element
      integer, optional, intent(out) :: rc

      select type (element)
      type is (integer(kind=INT32))
         type_kind = pFIO_INT32
      type is (integer(kind=INT64))
         type_kind = pFIO_INT64
      type is (real(kind=REAL32))
         type_kind = pFIO_REAL32
      type is (real(kind=REAL64))
         type_kind = pFIO_REAL64
      class default
         _ASSERT(.false.,'kind error')
      end select
      _RETURN(_SUCCESS)
   end function type_kind

   integer function get_length(this) result(length)
      class (ArrayReference), intent(in) :: this

      length = this%get_length_base()

   end function get_length

   subroutine serialize(this, buffer, rc)
      class (ArrayReference), intent(in) :: this
      integer, allocatable :: buffer(:)
      integer, optional, intent(out) :: rc
      integer :: status

      call this%serialize_base(buffer, rc=status)
      _VERIFY(status)
      _RETURN(_SUCCESS)
   end subroutine serialize

   subroutine deserialize(this, buffer, rc)
      class (ArrayReference), intent(inout) :: this
      integer, intent(in) :: buffer(:)
      integer, optional, intent(out) :: rc
      integer :: status

      call this%deserialize_base(buffer, rc=status)
      _VERIFY(status)
      _RETURN(_SUCCESS)
   end subroutine deserialize

end module pFIO_ArrayReferenceMod
