#include "MAPL_Generic.h"

module MAPL_FieldPointerUtilities
   use ESMF
   use MAPL_ExceptionHandling
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
   use, intrinsic :: iso_fortran_env, only: INT8, INT16, INT32, INT64
   implicit none
   private

   public :: assign_fptr
   public :: FieldGetLocalElementCount
   public :: FieldGetLocalSize
   public :: FieldGetCptr

   interface assign_fptr
      module procedure assign_fptr_r4_rank1
      module procedure assign_fptr_r8_rank1
      module procedure assign_fptr_r4_rank2
      module procedure assign_fptr_r8_rank2
   end interface assign_fptr

   interface FieldGetCptr
      procedure get_cptr
   end interface

   interface FieldGetLocalSize
      procedure get_local_size
   end interface FieldGetLocalSize

   interface FieldGetLocalElementCount
      procedure get_local_element_count
   end interface FieldGetLocalElementCount

contains


   subroutine assign_fptr_r4_rank1(x, fptr, rc)
      type(ESMF_Field), intent(inout) :: x
      real(kind=ESMF_KIND_R4), pointer, intent(out) :: fptr(:)
      integer, optional, intent(out) :: rc

      ! local declarations
      type(c_ptr) :: cptr
      integer(ESMF_KIND_I8), allocatable :: fp_shape(:)
      integer(ESMF_KIND_I8) :: local_size
      integer :: status

      local_size = FieldGetLocalSize(x, _RC)
      fp_shape = [ local_size ]
      call FieldGetCptr(x, cptr, _RC)
      call c_f_pointer(cptr, fptr, fp_shape)

      _RETURN(_SUCCESS)
   end subroutine assign_fptr_r4_rank1

   subroutine assign_fptr_r8_rank1(x, fptr, rc)
      type(ESMF_Field), intent(inout) :: x
      real(kind=ESMF_KIND_R8), pointer, intent(out) :: fptr(:)
      integer, optional, intent(out) :: rc

      ! local declarations
      type(c_ptr) :: cptr
      integer(ESMF_KIND_I8), allocatable :: fp_shape(:)
      integer(ESMF_KIND_I8) :: local_size
      integer :: status

      local_size = FieldGetLocalSize(x, _RC)
      fp_shape = [ local_size ]
      call FieldGetCptr(x, cptr, _RC)
      call c_f_pointer(cptr, fptr, fp_shape)

      _RETURN(_SUCCESS)
   end subroutine assign_fptr_r8_rank1

   subroutine assign_fptr_r4_rank2(x, fp_shape, fptr, rc)
      type(ESMF_Field), intent(inout) :: x
      integer(ESMF_KIND_I8), intent(in) :: fp_shape(:)
      real(kind=ESMF_KIND_R4), pointer, intent(out) :: fptr(:,:)
      integer, optional, intent(out) :: rc

      ! local declarations
      type(c_ptr) :: cptr
      integer :: status

      call FieldGetCptr(x, cptr, _RC)
      call c_f_pointer(cptr, fptr, fp_shape)

      _RETURN(_SUCCESS)
   end subroutine assign_fptr_r4_rank2

   subroutine assign_fptr_r8_rank2(x, fp_shape, fptr, rc)
      type(ESMF_Field), intent(inout) :: x
      integer(ESMF_KIND_I8), intent(in) :: fp_shape(:)
      real(kind=ESMF_KIND_R8), pointer, intent(out) :: fptr(:,:)
      integer, optional, intent(out) :: rc

      ! local declarations
      type(c_ptr) :: cptr
      integer :: status

      call FieldGetCptr(x, cptr, _RC)
      call c_f_pointer(cptr, fptr, fp_shape)

      _RETURN(_SUCCESS)
   end subroutine assign_fptr_r8_rank2

   subroutine get_cptr(x, cptr, rc)
      type(ESMF_Field), intent(inout) :: x
      type(c_ptr), intent(out) :: cptr
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_TypeKind_Flag) :: tk_x

      call ESMF_FieldGet(x, typekind=tk_x, _RC)

      if (tk_x == ESMF_TYPEKIND_R4) then
         call get_cptr_r4(x, cptr, _RC)
      elseif (tk_x == ESMF_TYPEKIND_R8) then
         call get_cptr_r8(x, cptr, _RC)
      elseif (tk_x == ESMF_TYPEKIND_I4) then
         call get_cptr_i4(x, cptr, _RC)
      elseif (tk_x == ESMF_TYPEKIND_I8) then
         call get_cptr_i8(x, cptr, _RC)
      else
         _FAIL('Unsupported typekind in FieldGetCptr().')
      end if

      _RETURN(_SUCCESS)
   end subroutine get_cptr

   subroutine get_cptr_r4(x, cptr, rc)
      type(ESMF_Field), intent(inout) :: x
      type(c_ptr), intent(out) :: cptr
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: rank
      real(kind=ESMF_KIND_R4), pointer :: x_1d(:)
      real(kind=ESMF_KIND_R4), pointer :: x_2d(:,:)
      real(kind=ESMF_KIND_R4), pointer :: x_3d(:,:,:)
      real(kind=ESMF_KIND_R4), pointer :: x_4d(:,:,:,:)
      real(kind=ESMF_KIND_R4), pointer :: x_5d(:,:,:,:,:)

      call ESMF_FieldGet(x, rank=rank, _RC)

      select case (rank)
      case (1)
         call ESMF_FieldGet(x, farrayPtr = x_1d, _RC)
         cptr = c_loc(x_1d)
      case (2)
         call ESMF_FieldGet(x, farrayPtr = x_2d, _RC)
         cptr = c_loc(x_2d)
      case (3)
         call ESMF_FieldGet(x, farrayPtr = x_3d, _RC)
         cptr = c_loc(x_3d)
      case (4)
         call ESMF_FieldGet(x, farrayPtr = x_4d, _RC)
         cptr = c_loc(x_4d)
      case (5)
         call ESMF_FieldGet(x, farrayPtr = x_5d, _RC)
         cptr = c_loc(x_5d)
      case default
         _FAIL('Unsupported rank in FieldGetCptr().')
      end select

      _RETURN(_SUCCESS)
   end subroutine get_cptr_r4

   subroutine get_cptr_r8(x, cptr, rc)
      type(ESMF_Field), intent(inout) :: x
      type(c_ptr), intent(out) :: cptr
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: rank
      real(kind=ESMF_KIND_R8), pointer :: x_1d(:)
      real(kind=ESMF_KIND_R8), pointer :: x_2d(:,:)
      real(kind=ESMF_KIND_R8), pointer :: x_3d(:,:,:)
      real(kind=ESMF_KIND_R8), pointer :: x_4d(:,:,:,:)
      real(kind=ESMF_KIND_R8), pointer :: x_5d(:,:,:,:,:)

      call ESMF_FieldGet(x, rank=rank, _RC)

      select case (rank)
      case (1)
         call ESMF_FieldGet(x, farrayPtr = x_1d, _RC)
         cptr = c_loc(x_1d)
      case (2)
         call ESMF_FieldGet(x, farrayPtr = x_2d, _RC)
         cptr = c_loc(x_2d)
      case (3)
         call ESMF_FieldGet(x, farrayPtr = x_3d, _RC)
         cptr = c_loc(x_3d)
      case (4)
         call ESMF_FieldGet(x, farrayPtr = x_4d, _RC)
         cptr = c_loc(x_4d)
      case (5)
         call ESMF_FieldGet(x, farrayPtr = x_5d, _RC)
         cptr = c_loc(x_5d)
      case default
         _FAIL('Unsupported rank in FieldGetCptr().')
      end select

      _RETURN(_SUCCESS)
   end subroutine get_cptr_r8

   subroutine get_cptr_i4(x, cptr, rc)
      type(ESMF_Field), intent(inout) :: x
      type(c_ptr), intent(out) :: cptr
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: rank
      integer(kind=ESMF_KIND_I4), pointer :: x_1d(:)
      integer(kind=ESMF_KIND_I4), pointer :: x_2d(:,:)
      integer(kind=ESMF_KIND_I4), pointer :: x_3d(:,:,:)
      integer(kind=ESMF_KIND_I4), pointer :: x_4d(:,:,:,:)
      integer(kind=ESMF_KIND_I4), pointer :: x_5d(:,:,:,:,:)

      call ESMF_FieldGet(x, rank=rank, _RC)

      select case (rank)
      case (1)
         call ESMF_FieldGet(x, farrayPtr = x_1d, _RC)
         cptr = c_loc(x_1d)
      case (2)
         call ESMF_FieldGet(x, farrayPtr = x_2d, _RC)
         cptr = c_loc(x_2d)
      case (3)
         call ESMF_FieldGet(x, farrayPtr = x_3d, _RC)
         cptr = c_loc(x_3d)
      case (4)
         call ESMF_FieldGet(x, farrayPtr = x_4d, _RC)
         cptr = c_loc(x_4d)
      case (5)
         call ESMF_FieldGet(x, farrayPtr = x_5d, _RC)
         cptr = c_loc(x_5d)
      case default
         _FAIL('Unsupported rank in FieldGetCptr().')
      end select

      _RETURN(_SUCCESS)
   end subroutine get_cptr_i4

   subroutine get_cptr_i8(x, cptr, rc)
      type(ESMF_Field), intent(inout) :: x
      type(c_ptr), intent(out) :: cptr
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: rank
      integer(kind=ESMF_KIND_I8), pointer :: x_1d(:)
      integer(kind=ESMF_KIND_I8), pointer :: x_2d(:,:)
      integer(kind=ESMF_KIND_I8), pointer :: x_3d(:,:,:)
      integer(kind=ESMF_KIND_I8), pointer :: x_4d(:,:,:,:)
      integer(kind=ESMF_KIND_I8), pointer :: x_5d(:,:,:,:,:)

      call ESMF_FieldGet(x, rank=rank, _RC)

      select case (rank)
      case (1)
         call ESMF_FieldGet(x, farrayPtr = x_1d, _RC)
         cptr = c_loc(x_1d)
      case (2)
         call ESMF_FieldGet(x, farrayPtr = x_2d, _RC)
         cptr = c_loc(x_2d)
      case (3)
         call ESMF_FieldGet(x, farrayPtr = x_3d, _RC)
         cptr = c_loc(x_3d)
      case (4)
         call ESMF_FieldGet(x, farrayPtr = x_4d, _RC)
         cptr = c_loc(x_4d)
      case (5)
         call ESMF_FieldGet(x, farrayPtr = x_5d, _RC)
         cptr = c_loc(x_5d)
      case default
         _FAIL('Unsupported rank in FieldGetCptr().')
      end select

      _RETURN(_SUCCESS)
   end subroutine get_cptr_i8

   function get_local_element_count(x, rc) result(element_count)
      type(ESMF_Field), intent(inout) :: x
      integer, optional, intent(out) :: rc
      integer, allocatable :: element_count(:)

      integer :: status
      integer :: rank

!      element_count = [integer :: ] ! default

      call ESMF_FieldGet(x, rank=rank, _RC)
      allocate(element_count(rank))
      call ESMF_FieldGet(x, localElementCount=element_count, _RC)

      _RETURN(_SUCCESS)
   end function get_local_element_count

   function get_local_size(x, rc) result(sz)
      integer(kind=ESMF_KIND_I8) :: sz
      type(ESMF_Field), intent(inout) :: x
      integer, optional, intent(out) :: rc

      integer :: status
      integer, allocatable :: element_count(:)

      sz = 0
      element_count = FieldGetLocalElementCount(x, _RC)
      sz = int(product(element_count), kind=ESMF_KIND_I8)

      _RETURN(_SUCCESS)
   end function get_local_size

end module
