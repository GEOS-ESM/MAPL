module pFIO_MemReferenceMod
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

   public :: MemReference

   type,extends(AbstractDataReference) :: MemReference
      integer,pointer :: ptr(:)
   contains
      procedure :: allocate
      procedure :: deallocate
   end type MemReference

   interface MemReference
      module procedure new_MemReference
   end interface MemReference

contains


   function new_MemReference(type_kind, shp) result(reference)
      type (MemReference) :: reference
      integer, intent(in) :: type_kind
      integer, intent(in) :: shp(:)

      reference%shape = shp
      reference%type_kind = type_kind

      call reference%allocate()

   end function new_MemReference

   subroutine allocate(this)
      class (MemReference), intent(inout) :: this
      integer :: n_words
      integer :: n

      n = product(this%shape)
      n_words = n * word_size(this%type_kind)
      allocate(this%ptr(n_words))
      this%base_address = c_loc(this%ptr)

   end subroutine allocate

   subroutine deallocate(this)
      class (MemReference), intent(inout) :: this
      integer :: n_words
      integer :: n
      integer,pointer :: ptr(:)

    ! if( c_associated(a%base_address, C_NULL_PTR)) return

      deallocate(this%ptr)
      this%base_address = C_NULL_PTR

   end subroutine deallocate

end module pFIO_MemReferenceMod
