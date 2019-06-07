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
   use pFIO_ConstantsMod
   use pFIO_AbstractDataReferenceMod

   implicit none
   private

   public :: ArrayReference

   type,extends(AbstractDataReference) :: ArrayReference
   contains
   end type ArrayReference

   interface ArrayReference
      module procedure new_ArrayReference_0d
      module procedure new_ArrayReference_1d
      module procedure new_ArrayReference_2d
      module procedure new_ArrayReference_3d
      module procedure new_ArrayReference_4d
   end interface ArrayReference

contains


   function new_ArrayReference_0d(scalar) result(reference)
      type (ArrayReference) :: reference
      class(*), target, intent(in) :: scalar

      select type (scalar)
      type is (real(kind=REAL32))
         reference%base_address = c_loc(scalar)
      type is (real(kind=REAL64))
         reference%base_address = c_loc(scalar)
      type is (integer(kind=INT32))
         reference%base_address = c_loc(scalar)
      type is (integer(kind=INT64))
         reference%base_address = c_loc(scalar)
      end select
      reference%shape = shape(scalar)
      reference%type_kind = type_kind(scalar)
      
   end function new_ArrayReference_0d

   function new_ArrayReference_1d(array) result(reference)
      type (ArrayReference) :: reference
      class(*), target, intent(in) :: array(:)

      select type (array)
      type is (real(kind=REAL32))
         reference%base_address = c_loc(array)
      type is (real(kind=REAL64))
         reference%base_address = c_loc(array)
      type is (integer(kind=INT32))
         reference%base_address = c_loc(array)
      type is (integer(kind=INT64))
         reference%base_address = c_loc(array)
      end select

      reference%shape = shape(array)
      reference%type_kind = type_kind(array(1))
      
   end function new_ArrayReference_1d

   function new_ArrayReference_2d(array) result(reference)
      type (ArrayReference) :: reference
      class(*), target, intent(in) :: array(:,:)

      select type (array)
      type is (real(kind=REAL32))
         reference%base_address = c_loc(array)
      type is (real(kind=REAL64))
         reference%base_address = c_loc(array)
      type is (integer(kind=INT32))
         reference%base_address = c_loc(array)
      type is (integer(kind=INT64))
         reference%base_address = c_loc(array)
      end select

      reference%shape = shape(array)
      reference%type_kind = type_kind(array(1,1))
      
   end function new_ArrayReference_2d

   function new_ArrayReference_3d(array) result(reference)
      type (ArrayReference) :: reference
      class(*), target, intent(in) :: array(:,:,:)

      select type (array)
      type is (real(kind=REAL32))
         reference%base_address = c_loc(array)
      type is (real(kind=REAL64))
         reference%base_address = c_loc(array)
      type is (integer(kind=INT32))
         reference%base_address = c_loc(array)
      type is (integer(kind=INT64))
         reference%base_address = c_loc(array)
      end select

      reference%shape = shape(array)
      reference%type_kind = type_kind(array(1,1,1))
      
   end function new_ArrayReference_3d


   function new_ArrayReference_4d(array) result(reference)
      type (ArrayReference) :: reference
      class(*), target, intent(in) :: array(:,:,:,:)

      select type (array)
      type is (real(kind=REAL32))
         reference%base_address = c_loc(array)
      type is (real(kind=REAL64))
         reference%base_address = c_loc(array)
      type is (integer(kind=INT32))
         reference%base_address = c_loc(array)
      type is (integer(kind=INT64))
         reference%base_address = c_loc(array)
      end select

      reference%shape = shape(array)
      reference%type_kind = type_kind(array(1,1,1,1))
      
   end function new_ArrayReference_4d

   integer function type_kind(element)
      class(*), intent(in) :: element

      select type (element)
      type is (integer(kind=INT32))
         type_kind = pFIO_INT32
      type is (real(kind=REAL32))
         type_kind = pFIO_REAL32
      type is (real(kind=REAL64))
         type_kind = pFIO_REAL64
      class default
         print*,'kind error'
         stop
      end select
   end function type_kind

end module pFIO_ArrayReferenceMod
