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
   public :: FieldClone
   public :: FieldsAreConformable
   public :: FieldsAreSameTypeKind
   public :: FieldCopy

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

   interface FieldsAreConformable
      procedure are_conformable_scalar
      procedure are_conformable_array
   end interface

   interface FieldClone
      module procedure clone
   end interface FieldClone

   interface FieldsAreSameTypeKind
      module procedure are_same_type_kind
   end interface FieldsAreSameTypeKind

   interface verify_typekind
      module procedure verify_typekind_scalar
      module procedure verify_typekind_array
   end interface verify_typekind

   ! call FieldCOPY(x, ddy, rc): y = x
   interface FieldCOPY
      procedure copy
   end interface FieldCOPY

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

   subroutine clone(x, y, rc)
      type(ESMF_Field), intent(inout) :: x
      type(ESMF_Field), intent(inout) :: y
      integer, optional, intent(out) :: rc

      character(len=*), parameter :: CLONE_TAG = '_clone'
      type(ESMF_ArraySpec) :: arrayspec
      type(ESMF_Grid) :: grid
      type(ESMF_StaggerLoc) :: staggerloc
      integer, allocatable :: gridToFieldMap(:)
      integer, allocatable :: ungriddedLBound(:)
      integer, allocatable :: ungriddedUBound(:)
      integer, allocatable :: totalLWidth(:,:)
      integer, allocatable :: totalUWidth(:,:)
      character(len=:), allocatable :: name
      integer :: status

      call ESMF_FieldGet(x, arrayspec=arrayspec, grid=grid, &
         staggerloc=staggerloc, gridToFieldMap=gridToFieldMap, &
         ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
         totalLWidth=totalLWidth, totalUWidth=totalUWidth, _RC)

      name = name // CLONE_TAG

      y = ESMF_FieldCreate(grid, arrayspec, staggerloc=staggerloc, &
         gridToFieldMap=gridToFieldMap, ungriddedLBound=ungriddedLBound, &
         ungriddedUBound=ungriddedUBound, name=name, _RC)

      _RETURN(_SUCCESS)
   end subroutine clone

   logical function are_conformable_scalar(x, y, rc) result(conformable)
      type(ESMF_Field), intent(inout) :: x
      type(ESMF_Field), intent(inout) :: y
      integer, optional, intent(out) :: rc
      integer :: rank_x, rank_y
      integer, dimension(:), allocatable :: count_x, count_y
      integer :: status

      conformable = .false.

      call ESMF_FieldGet(x, rank=rank_x, _RC)
      call ESMF_FieldGet(y, rank=rank_y, _RC)

      if(rank_x == rank_y) then
         count_x = FieldGetLocalElementCount(x, _RC)
         count_y = FieldGetLocalElementCount(y, _RC)
         conformable = all(count_x == count_y)
      end if

      _RETURN(_SUCCESS)
   end function are_conformable_scalar

   logical function are_conformable_array(x, y, rc) result(conformable)
      type(ESMF_Field), intent(inout) :: x
      type(ESMF_Field), intent(inout) :: y(:)
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: j
      logical :: element_not_conformable

      conformable = .false.
      element_not_conformable = .false.

      do j = 1, size(y)
         element_not_conformable = .not. FieldsAreConformable(x, y(j), _RC)
         if(element_not_conformable) return
      end do

      conformable = .true.

      _RETURN(_SUCCESS)
   end function are_conformable_array

   logical function are_same_type_kind(x, y, rc) result(same_tk)
      type(ESMF_Field), intent(inout) :: x
      type(ESMF_Field), intent(inout) :: y
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_TypeKind_Flag) :: tk_x, tk_y

      same_tk = .false.
      call ESMF_FieldGet(x, typekind=tk_x, _RC)
      call ESMF_FieldGet(y, typekind=tk_y, _RC)

      same_tk = (tk_x == tk_y)

      _RETURN(_SUCCESS)
   end function are_same_type_kind

    subroutine verify_typekind_scalar(x, expected_tk, rc)
      type(ESMF_Field), intent(inout) :: x
      type(ESMF_TypeKind_Flag), intent(in) :: expected_tk
      integer, optional, intent(out) :: rc

      integer :: status

      type(ESMF_TypeKind_Flag) :: found_tk

      call ESMF_FieldGet(x, typekind=found_tk, _RC)

      _ASSERT((found_tk == expected_tk), 'Found incorrect typekind.')
      _RETURN(_SUCCESS)
   end subroutine verify_typekind_scalar

   subroutine verify_typekind_array(x, expected_tk, rc)
      type(ESMF_Field), intent(inout) :: x(:)
      type(ESMF_TypeKind_Flag), intent(in) :: expected_tk
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i

      do i = 1, size(x)
         call verify_typekind(x(i), expected_tk, _RC)
      end do
      _RETURN(_SUCCESS)
   end subroutine verify_typekind_array

   function is_valid_typekind(actual_tk, valid_tks) result(is_valid)
      type(ESMF_TypeKind_Flag), intent(in) :: actual_tk
      type(ESMF_TypeKind_Flag), intent(in) :: valid_tks(:)
      logical :: is_valid
      integer :: i

      is_valid = .FALSE.
      do i = 1, size(valid_tks)
         is_valid = (actual_tk == valid_tks(i))
         if(is_valid) return
      end do

   end function is_valid_typekind

   subroutine copy(x, y, rc)
      type(ESMF_Field), intent(inout) :: x
      type(ESMF_Field), intent(inout) :: y
      integer, optional, intent(out) :: rc

      type(ESMF_TypeKind_Flag) :: tk_x, tk_y
      type(c_ptr) :: cptr_x, cptr_y
      integer(kind=ESMF_KIND_I8) :: n
      integer :: status
      logical :: conformable
      logical :: x_is_double
      logical :: y_is_double
      character(len=*), parameter :: UNSUPPORTED_TK = &
         'Unsupported typekind in FieldCOPY() for '

      conformable = FieldsAreConformable(x, y)
      !wdb fixme need to pass RC
      _ASSERT(conformable, 'FieldCopy() - fields not conformable.')
      call FieldGetCptr(x, cptr_x, _RC)
      call ESMF_FieldGet(x, typekind = tk_x, _RC)

      n  = FieldGetLocalSize(x, _RC)

      call FieldGetCptr(y, cptr_y, _RC)
      call ESMF_FieldGet(y, typekind = tk_y, _RC)

     !wdb  fixme convert between precisions ? get rid of extra cases
      y_is_double = (tk_y == ESMF_TYPEKIND_R8)
      _ASSERT(y_is_double .or. (tk_y == ESMF_TYPEKIND_R4), UNSUPPORTED_TK//'y.')

      x_is_double = (tk_x == ESMF_TYPEKIND_R8)
      _ASSERT(x_is_double .or. (tk_x == ESMF_TYPEKIND_R4), UNSUPPORTED_TK//'x.')

      if (y_is_double) then
         if (x_is_double) then
            call copy_r8_r8(cptr_x, cptr_y, n)
         else
            call copy_r4_r8(cptr_x, cptr_y, n)
         end if
      else
         if (x_is_double) then
            call copy_r8_r4(cptr_x, cptr_y, n)
         else
            call copy_r4_r4(cptr_x, cptr_y, n)
         end if
      end if

      _RETURN(_SUCCESS)
   end subroutine copy

   subroutine copy_r4_r4(cptr_x, cptr_y, n)
      type(c_ptr), intent(in) :: cptr_x, cptr_y
      integer(ESMF_KIND_I8), intent(in) :: n

      real(kind=ESMF_KIND_R4), pointer :: x_ptr(:)
      real(kind=ESMF_KIND_R4), pointer :: y_ptr(:)

      call c_f_pointer(cptr_x, x_ptr, [n])
      call c_f_pointer(cptr_y, y_ptr, [n])

      y_ptr=x_ptr
   end subroutine copy_r4_r4

   subroutine copy_r4_r8(cptr_x, cptr_y, n)
      type(c_ptr), intent(in) :: cptr_x, cptr_y
      integer(ESMF_KIND_I8), intent(in) :: n

      real(kind=ESMF_KIND_R4), pointer :: x_ptr(:)
      real(kind=ESMF_KIND_R8), pointer :: y_ptr(:)

      call c_f_pointer(cptr_x, x_ptr, [n])
      call c_f_pointer(cptr_y, y_ptr, [n])

      y_ptr=x_ptr
   end subroutine copy_r4_r8

   subroutine copy_r8_r4(cptr_x, cptr_y, n)
      type(c_ptr), intent(in) :: cptr_x, cptr_y
      integer(ESMF_KIND_I8), intent(in) :: n

      real(kind=ESMF_KIND_R8), pointer :: x_ptr(:)
      real(kind=ESMF_KIND_R4), pointer :: y_ptr(:)

      call c_f_pointer(cptr_x, x_ptr, [n])
      call c_f_pointer(cptr_y, y_ptr, [n])

      y_ptr=x_ptr
   end subroutine copy_r8_r4

   subroutine copy_r8_r8(cptr_x, cptr_y, n)
      type(c_ptr), intent(in) :: cptr_x, cptr_y
      integer(ESMF_KIND_I8), intent(in) :: n

      real(kind=ESMF_KIND_R8), pointer :: x_ptr(:)
      real(kind=ESMF_KIND_R8), pointer :: y_ptr(:)

      call c_f_pointer(cptr_x, x_ptr, [n])
      call c_f_pointer(cptr_y, y_ptr, [n])

      y_ptr=x_ptr
   end subroutine copy_r8_r8





end module
