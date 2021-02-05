#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_LocalMemReferenceMod
   use, intrinsic :: iso_c_binding, only: C_NULL_PTR, c_associated
   use, intrinsic :: iso_c_binding, only: c_loc
   use, intrinsic :: iso_c_binding, only: c_f_pointer
   use, intrinsic :: iso_fortran_env, only: INT32
   use, intrinsic :: iso_fortran_env, only: INT64
   use, intrinsic :: iso_fortran_env, only: REAL32
   use, intrinsic :: iso_fortran_env, only: REAL64
   use MAPL_ExceptionHandling
   use pFIO_UtilitiesMod, only: word_size
   use pFIO_ConstantsMod
   use pFIO_AbstractDataReferenceMod

   implicit none
   private

   public :: LocalMemReference

   type,extends(AbstractDataReference) :: LocalMemReference
      integer, pointer :: i_ptr(:)
   contains
      procedure :: get_length
      procedure :: serialize
      procedure :: deserialize

      procedure :: allocate
      procedure :: deallocate
   end type LocalMemReference

   interface LocalMemReference
      module procedure new_LocalMemReference
      module procedure new_LocalMemReference_0d
      module procedure new_LocalMemReference_1d
      module procedure new_LocalMemReference_2d
      module procedure new_LocalMemReference_3d
      module procedure new_LocalMemReference_4d
      module procedure new_LocalMemReference_5d
   end interface LocalMemReference

contains


   function new_LocalMemReference(type_kind, shp, rc) result(reference)
      type (LocalMemReference) :: reference
      integer, intent(in) :: type_kind
      integer, intent(in) :: shp(:)
      integer, optional, intent(out) :: rc
      integer :: status

      reference%shape = shp
      reference%type_kind = type_kind
      call reference%allocate(rc=status)
      _VERIFY(status)
      _RETURN(_SUCCESS)
   end function new_LocalMemReference


   function new_LocalMemReference_0d(scalar, rc) result(reference)
      type (LocalMemReference) :: reference
      class(*), target, intent(in) :: scalar
      integer, optional, intent(out) :: rc
      integer (kind=INT32), pointer :: int32Ptr
      integer (kind=INT64), pointer :: int64Ptr
      real (kind=REAL32)  , pointer :: real32Ptr
      real (kind=REAL64)  , pointer :: real64Ptr
      integer :: status
 
      select type (scalar)
      type is (integer(kind=INT32))
         reference = LocalMemReference(pFIO_INT32, shape(scalar), rc=status)
         _VERIFY(status)
         call c_f_pointer(reference%base_address, int32Ptr)
         int32Ptr = scalar
      type is (integer(kind=INT64))
         reference = LocalMemReference(pFIO_INT64, shape(scalar), rc=status)
         _VERIFY(status)
         call c_f_pointer(reference%base_address, int64Ptr)
         int64Ptr = scalar
      type is (real(kind=REAL32))
         reference = LocalMemReference(pFIO_REAL32, shape(scalar), rc=status)
         _VERIFY(status)
         call c_f_pointer(reference%base_address, real32Ptr)
         real32Ptr = scalar
      type is (real(kind=REAL64))
         reference = LocalMemReference(pFIO_REAL64, shape(scalar), rc=status)
         _VERIFY(status)
         call c_f_pointer(reference%base_address, real64Ptr)
         real64Ptr = scalar
      class default
         _ASSERT(.false., "LocalMemRef does not support this type")
      end select

      _RETURN(_SUCCESS)
   end function new_LocalMemReference_0d

   function new_LocalMemReference_1d(array, rc) result(reference)
      type (LocalMemReference) :: reference
      class(*), target, intent(in) :: array(:)
      integer, optional, intent(out) :: rc
      integer (kind=INT32), pointer :: int32Ptr(:)
      integer (kind=INT64), pointer :: int64Ptr(:)
      real (kind=REAL32)  , pointer :: real32Ptr(:)
      real (kind=REAL64)  , pointer :: real64Ptr(:)
      integer :: status

      select type (array)
      type is (integer(kind=INT32))
         reference = LocalMemReference( pFIO_INT32, shape(array), rc=status)
         _VERIFY(status)
         if (.not. c_associated(reference%base_address)) then
            _RETURN(_SUCCESS)
         endif
         call c_f_pointer(reference%base_address, int32Ptr, shape=shape(array))
         int32Ptr = array
      type is (integer(kind=INT64))
         reference = LocalMemReference( pFIO_INT64, shape(array), rc=status)
         _VERIFY(status)
         if (.not. c_associated(reference%base_address)) then
            _RETURN(_SUCCESS)
         endif
         call c_f_pointer(reference%base_address, int64Ptr, shape=shape(array))
         int64Ptr = array
      type is (real(kind=REAL32))
         reference = LocalMemReference( pFIO_REAL32, shape(array), rc=status)
         _VERIFY(status)
         if (.not. c_associated(reference%base_address)) then
            _RETURN(_SUCCESS)
         endif
         call c_f_pointer(reference%base_address, real32Ptr, shape=shape(array))
         real32Ptr = array
      type is (real(kind=REAL64))
         reference = LocalMemReference( pFIO_REAL64, shape(array), rc=status)
         _VERIFY(status)
         if (.not. c_associated(reference%base_address)) then
            _RETURN(_SUCCESS)
         endif
         call c_f_pointer(reference%base_address, real64Ptr, shape=shape(array))
         real64Ptr = array
      class default
         _ASSERT(.false., "LocalMemRef does not support this type")
      end select

      _RETURN(_SUCCESS)

   end function new_LocalMemReference_1d

   function new_LocalMemReference_2d(array, rc) result(reference)
      type (LocalMemReference) :: reference
      class(*), target, intent(in) :: array(:,:)
      integer, optional, intent(out) :: rc
      integer (kind=INT32), pointer :: int32Ptr(:,:)
      integer (kind=INT64), pointer :: int64Ptr(:,:)
      real (kind=REAL32)  , pointer :: real32Ptr(:,:)
      real (kind=REAL64)  , pointer :: real64Ptr(:,:)
      integer :: status

      select type (array)
      type is (integer(kind=INT32))
         reference = LocalMemReference( pFIO_INT32, shape(array), rc=status)
         _VERIFY(status)
         if (.not. c_associated(reference%base_address)) then
            _RETURN(_SUCCESS)
         endif
         call c_f_pointer(reference%base_address, int32Ptr, shape=shape(array))
         int32Ptr = array
      type is (integer(kind=INT64))
         reference = LocalMemReference( pFIO_INT64, shape(array), rc=status)
         _VERIFY(status)
         if (.not. c_associated(reference%base_address)) then
            _RETURN(_SUCCESS)
         endif
         call c_f_pointer(reference%base_address, int64Ptr, shape=shape(array))
         int64Ptr = array
      type is (real(kind=REAL32))
         reference = LocalMemReference( pFIO_REAL32, shape(array), rc=status)
         _VERIFY(status)
         if (.not. c_associated(reference%base_address)) then
            _RETURN(_SUCCESS)
         endif
         call c_f_pointer(reference%base_address, real32Ptr, shape=shape(array))
         real32Ptr = array
      type is (real(kind=REAL64))
         reference = LocalMemReference( pFIO_REAL64, shape(array), rc=status)
         _VERIFY(status)
         if (.not. c_associated(reference%base_address)) then
            _RETURN(_SUCCESS)
         endif
         call c_f_pointer(reference%base_address, real64Ptr, shape=shape(array))
         real64Ptr = array
      class default
         _ASSERT(.false., "LocalMemRef does not support this type")
      end select

      _RETURN(_SUCCESS)

   end function new_LocalMemReference_2d

   function new_LocalMemReference_3d(array, rc) result(reference)
      type (LocalMemReference) :: reference
      class(*), target, intent(in) :: array(:,:,:)
      integer, optional, intent(out) :: rc
      integer (kind=INT32), pointer :: int32Ptr(:,:,:)
      integer (kind=INT64), pointer :: int64Ptr(:,:,:)
      real (kind=REAL32)  , pointer :: real32Ptr(:,:,:)
      real (kind=REAL64)  , pointer :: real64Ptr(:,:,:)
      integer :: status

      select type (array)
      type is (integer(kind=INT32))
         reference = LocalMemReference( pFIO_INT32, shape(array), rc=status)
         _VERIFY(status)
         if (.not. c_associated(reference%base_address)) then
            _RETURN(_SUCCESS)
         endif
         call c_f_pointer(reference%base_address, int32Ptr, shape=shape(array))
         int32Ptr = array
      type is (integer(kind=INT64))
         reference = LocalMemReference( pFIO_INT64, shape(array), rc=status)
         _VERIFY(status)
         if (.not. c_associated(reference%base_address)) then
            _RETURN(_SUCCESS)
         endif
         call c_f_pointer(reference%base_address, int64Ptr, shape=shape(array))
         int64Ptr = array
      type is (real(kind=REAL32))
         reference = LocalMemReference( pFIO_REAL32, shape(array), rc=status)
         _VERIFY(status)
         if (.not. c_associated(reference%base_address)) then
            _RETURN(_SUCCESS)
         endif
         call c_f_pointer(reference%base_address, real32Ptr, shape=shape(array))
         real32Ptr = array
      type is (real(kind=REAL64))
         reference = LocalMemReference( pFIO_REAL64, shape(array), rc=status)
         _VERIFY(status)
         if (.not. c_associated(reference%base_address)) then
            _RETURN(_SUCCESS)
         endif
         call c_f_pointer(reference%base_address, real64Ptr, shape=shape(array))
         real64Ptr = array
      class default
         _ASSERT(.false., "LocalMemRef does not support this type")
      end select

      _RETURN(_SUCCESS)

   end function new_LocalMemReference_3d

   function new_LocalMemReference_4d(array, rc) result(reference)
      type (LocalMemReference) :: reference
      class(*), target, intent(in) :: array(:,:,:,:)
      integer, optional, intent(out) :: rc
      integer (kind=INT32), pointer :: int32Ptr(:,:,:,:)
      integer (kind=INT64), pointer :: int64Ptr(:,:,:,:)
      real (kind=REAL32)  , pointer :: real32Ptr(:,:,:,:)
      real (kind=REAL64)  , pointer :: real64Ptr(:,:,:,:)
      integer :: status

      select type (array)
      type is (integer(kind=INT32))
         reference = LocalMemReference( pFIO_INT32, shape(array), rc=status)
         _VERIFY(status)
         if (.not. c_associated(reference%base_address)) then
            _RETURN(_SUCCESS)
         endif
         call c_f_pointer(reference%base_address, int32Ptr, shape=shape(array))
         int32Ptr = array
      type is (integer(kind=INT64))
         reference = LocalMemReference( pFIO_INT64, shape(array), rc=status)
         _VERIFY(status)
         if (.not. c_associated(reference%base_address)) then
            _RETURN(_SUCCESS)
         endif
         call c_f_pointer(reference%base_address, int64Ptr, shape=shape(array))
         int64Ptr = array
      type is (real(kind=REAL32))
         reference = LocalMemReference( pFIO_REAL32, shape(array), rc=status)
         _VERIFY(status)
         if (.not. c_associated(reference%base_address)) then
            _RETURN(_SUCCESS)
         endif
         call c_f_pointer(reference%base_address, real32Ptr, shape=shape(array))
         real32Ptr = array
      type is (real(kind=REAL64))
         reference = LocalMemReference( pFIO_REAL64, shape(array), rc=status)
         _VERIFY(status)
         if (.not. c_associated(reference%base_address)) then
            _RETURN(_SUCCESS)
         endif
         call c_f_pointer(reference%base_address, real64Ptr, shape=shape(array))
         real64Ptr = array
      class default
         _ASSERT(.false., "LocalMemRef does not support this type")
      end select

      _RETURN(_SUCCESS)

   end function new_LocalMemReference_4d

   function new_LocalMemReference_5d(array, rc) result(reference)
      type (LocalMemReference) :: reference
      class(*), target, intent(in) :: array(:,:,:,:,:)
      integer, optional, intent(out) :: rc
      integer (kind=INT32), pointer :: int32Ptr(:,:,:,:,:)
      integer (kind=INT64), pointer :: int64Ptr(:,:,:,:,:)
      real (kind=REAL32)  , pointer :: real32Ptr(:,:,:,:,:)
      real (kind=REAL64)  , pointer :: real64Ptr(:,:,:,:,:)
      integer :: status

      select type (array)
      type is (integer(kind=INT32))
         reference = LocalMemReference( pFIO_INT32, shape(array), rc=status)
         _VERIFY(status)
         if (.not. c_associated(reference%base_address)) then
            _RETURN(_SUCCESS)
         endif
         call c_f_pointer(reference%base_address, int32Ptr, shape=shape(array))
         int32Ptr = array
      type is (integer(kind=INT64))
         reference = LocalMemReference( pFIO_INT64, shape(array), rc=status)
         _VERIFY(status)
         if (.not. c_associated(reference%base_address)) then
            _RETURN(_SUCCESS)
         endif
         call c_f_pointer(reference%base_address, int64Ptr, shape=shape(array))
         int64Ptr = array
      type is (real(kind=REAL32))
         reference = LocalMemReference( pFIO_REAL32, shape(array), rc=status)
         _VERIFY(status)
         if (.not. c_associated(reference%base_address)) then
            _RETURN(_SUCCESS)
         endif
         call c_f_pointer(reference%base_address, real32Ptr, shape=shape(array))
         real32Ptr = array
      type is (real(kind=REAL64))
         reference = LocalMemReference( pFIO_REAL64, shape(array), rc=status)
         _VERIFY(status)
         if (.not. c_associated(reference%base_address)) then
            _RETURN(_SUCCESS)
         endif
         call c_f_pointer(reference%base_address, real64Ptr, shape=shape(array))
         real64Ptr = array
      class default
         _ASSERT(.false., "LocalMemRef does not support this type")
      end select

      _RETURN(_SUCCESS)

   end function new_LocalMemReference_5d

   subroutine allocate(this, rc)
      class (LocalMemReference), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer(kind=INT64) :: n_words
      integer(kind=INT64) :: n
      integer :: status

      n = product(int(this%shape,INT64))
      n_words = n * word_size(this%type_kind)
      allocate(this%i_ptr(n_words), stat=status)
      _VERIFY(status)
      if (n > 0) then
         this%base_address = c_loc(this%i_ptr)
      else
         this%base_address =  C_NULL_PTR 
      endif
      _RETURN(_SUCCESS)
   end subroutine allocate

   subroutine deallocate(this, rc)
      class (LocalMemReference), intent(inout) :: this
      integer, optional, intent(out) :: rc

      if (associated(this%i_ptr)) deallocate(this%i_ptr)
      this%base_address = C_NULL_PTR
      _RETURN(_SUCCESS)

   end subroutine deallocate

   integer function get_length(this) result(length)
      class (LocalMemReference), intent(in) :: this

      length = this%get_length_base()

   end function get_length


   subroutine serialize(this, buffer, rc)
      class (LocalMemReference), intent(in) :: this
      integer, optional, intent(out) :: rc

      integer, allocatable :: buffer(:)
      integer :: status
      call this%serialize_base(buffer, rc=status)
      _VERIFY(status)
      _RETURN(_SUCCESS)
   end subroutine serialize

   subroutine deserialize(this, buffer, rc)
      class (LocalMemReference), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer, intent(in) :: buffer(:)
      integer :: status
      call this%deserialize_base(buffer, rc=status)
      _VERIFY(status)
      _RETURN(_SUCCESS)
   end subroutine deserialize

end module pFIO_LocalMemReferenceMod
