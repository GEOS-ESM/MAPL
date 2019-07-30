#include "pFIO_ErrLog.h"
#include "unused_dummy.H"

module pFIO_LocalMemReferenceMod
   use, intrinsic :: iso_c_binding, only: c_ptr
   use, intrinsic :: iso_c_binding, only: C_NULL_PTR
   use, intrinsic :: iso_c_binding, only: c_loc
   use, intrinsic :: iso_c_binding, only: c_f_pointer
   use, intrinsic :: iso_c_binding, only: c_associated
   use, intrinsic :: iso_fortran_env, only: INT32
   use, intrinsic :: iso_fortran_env, only: INT64
   use, intrinsic :: iso_fortran_env, only: REAL32
   use, intrinsic :: iso_fortran_env, only: REAL64
   use pFIO_ErrorHandlingMod
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
      this%base_address = c_loc(this%i_ptr)
      _RETURN(_SUCCESS)
   end subroutine allocate

   subroutine deallocate(this, rc)
      class (LocalMemReference), intent(inout) :: this
      integer, optional, intent(out) :: rc

      if( c_associated(this%base_address, C_NULL_PTR)) then
         _RETURN(_SUCCESS)
      endif
      deallocate(this%i_ptr)
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
