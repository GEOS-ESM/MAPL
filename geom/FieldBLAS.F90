#include "MAPL_Generic.h"

module mapl3g_FieldBLAS
   use ESMF
   use MAPL_ExceptionHandling
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
   use, intrinsic :: iso_fortran_env, only: INT8, INT16, INT32, INT64
   implicit none
   private

   ! Level 1 BLAS
   public :: FieldCOPY
   public :: FieldSCAL
   public :: FieldAXPY

   ! Level 2 BLAS
   public :: FieldGEMV

!   ! Fortran intrinsics applied to fields
!   public :: Sin
!   public :: Cos
!   public :: Tan
!   public :: ASin
!   public :: ACos
!   public :: ATan
!   public :: Pow
!   public :: Abs
!   public :: Log
!   public :: Exp
!   public :: Log10
!   public :: Sqrt
!   public :: Sinh
!   public :: Cosh
!   public :: Tanh
!   public :: ASinh
!   public :: ACosh
!   public :: ATanh
!   public :: Heavyside
   

   ! Misc utiliities
   public :: FieldSpread
   public :: FieldClone
   public :: FieldConvertPrec
   public :: FieldGetLocalElementCount
   public :: FieldGetLocalSize
   public :: FieldGetCptr
   public :: FieldsAreConformable
   public :: FieldsAreSameTypeKind
!wdb  fixme temporary to test out this helper function
   public :: assign_fptr

   ! call FieldCOPY(x, y, rc): y = x
   interface FieldCOPY
      procedure copy
   end interface FieldCOPY

!wdb  fixme This acts on y in-place. Do we need a form that acts more like a function: y = FieldSCAL(a, x)?
   ! call FieldSCAL(a, x, rc): x = a*x (multiply x in-place)
   interface FieldSCAL
      procedure scale_r4
      procedure scale_r8
   end interface

   ! call FieldAXPY(a, x, y, rc): y = a*x + y (add a*x to y in-place)
   interface FieldAXPY
      procedure axpy_r4
      procedure axpy_r8
   end interface

   ! call FieldGEMV(alpha, A, x, beta, y, rc) (multiply y in-place, then add a*A*x to y in-place)
   interface FieldGEMV
      procedure gemv_r4
      procedure gemv_r8
   end interface

   interface FieldGetCptr
      procedure get_cptr
   end interface

   interface FieldsAreConformable
      procedure are_conformable_scalar
      procedure are_conformable_array
   end interface

   interface FieldGetLocalSize
      procedure get_local_size
   end interface FieldGetLocalSize

   interface FieldGetLocalElementCount
      procedure get_local_element_count
   end interface FieldGetLocalElementCount

   interface FieldConvertPrec
      module procedure convert_prec
   end interface FieldConvertPrec
   
   interface FieldSpread
      module procedure spread_scalar
   end interface FieldSpread

   interface FieldClone
      module procedure clone
   end interface FieldClone

   interface assign_fptr 
      module procedure assign_fptr_r4_rank1
      module procedure assign_fptr_r8_rank1
      module procedure assign_fptr_r4_rank2
      module procedure assign_fptr_r8_rank2
   end interface assign_fptr 

   interface FieldsAreSameTypeKind
      module procedure are_same_type_kind
   end interface FieldsAreSameTypeKind

   interface verify_typekind
      module procedure verify_typekind_scalar
      module procedure verify_typekind_array
   end interface verify_typekind

contains

   !wdb fixme  Is this a deep copy?
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

   subroutine scale_r4(a, x, rc)
      real(kind=ESMF_KIND_R4), intent(in) :: a
      type(ESMF_Field), intent(inout) :: x
      integer, optional, intent(out) :: rc

      real(kind=ESMF_KIND_R4), pointer :: x_ptr(:)
      integer :: status

      call assign_fptr(x, x_ptr, _RC)
      x_ptr = a * x_ptr

      _RETURN(_SUCCESS)
   end subroutine scale_r4
   
   subroutine scale_r8(a, x, rc)
      real(kind=ESMF_KIND_R8), intent(in) :: a
      type(ESMF_Field), intent(inout) :: x
      integer, optional, intent(out) :: rc 

      real(kind=ESMF_KIND_R8), pointer :: x_ptr(:)
      integer :: status

      call assign_fptr(x, x_ptr, _RC)
      x_ptr = a * x_ptr

      _RETURN(_SUCCESS)
   end subroutine scale_r8

   subroutine axpy_r4(a, x, y, rc)
      real(kind=ESMF_KIND_R4), intent(in) :: a
      type(ESMF_Field), intent(inout) :: x
      type(ESMF_Field), intent(inout) :: y
      integer, optional, intent(out) :: rc

      real(kind=ESMF_KIND_R4), pointer :: x_ptr(:), y_ptr(:)
      logical :: conformable
      integer :: status

      call verify_typekind(x, ESMF_TYPEKIND_R4)
      call verify_typekind(y, ESMF_TYPEKIND_R4)

      conformable = FieldsAreConformable(x, y)
      _ASSERT(conformable, 'FieldAXPY() - fields not conformable.')
      
      call assign_fptr(x, x_ptr, _RC)
      call assign_fptr(y, y_ptr, _RC)

      y_ptr = y_ptr + a * x_ptr

      _RETURN(_SUCCESS)
   end subroutine axpy_r4

   subroutine axpy_r8(a, x, y, rc)
      real(kind=ESMF_KIND_R8), intent(in) :: a
      type(ESMF_Field), intent(inout) :: x
      type(ESMF_Field), intent(inout) :: y
      integer, optional, intent(out) :: rc

      real(kind=ESMF_KIND_R8), pointer :: x_ptr(:), y_ptr(:)
      logical :: conformable
      integer :: status

      call verify_typekind(x, ESMF_TYPEKIND_R8)
      call verify_typekind(y, ESMF_TYPEKIND_R8)

      conformable = FieldsAreConformable(x, y)
      _ASSERT(conformable, 'FieldAXPY() - fields not conformable.')
      
      call assign_fptr(x, x_ptr, _RC)
      call assign_fptr(y, y_ptr, _RC)

      y_ptr = y_ptr + a * x_ptr

      _RETURN(_SUCCESS)
   end subroutine axpy_r8

   ! Assumes gridded dimensions are first, and that the "vector" dim
   ! is last ungridded dim of fields.
   ! Computes y = alpha * A * x + beta * y

   ! [x,y,z] = A * [u,v]
   ! single precision (R4) gemv
   subroutine gemv_r4(alpha, A, x, beta, y, rc)
      real(kind=ESMF_KIND_R4), intent(in) :: alpha
      real(kind=ESMF_KIND_R4), intent(in) :: A(:,:,:)
      type(ESMF_Field), intent(inout) :: x(:)
      real(kind=ESMF_KIND_R4), intent(in) :: beta
      type(ESMF_Field), intent(inout) :: y(:)
      integer, optional, intent(out) :: rc

      logical :: conformable
      integer :: dimcount
      integer, allocatable :: local_element_count(:)
      integer(kind=ESMF_KIND_I8) :: n_gridded, n_ungridded
      integer(kind=ESMF_KIND_I8) :: fp_shape(2)
      real(kind=ESMF_KIND_R4), pointer :: x_ptr(:,:), y_ptr(:,:)
      integer :: ix, jy, kv
      integer :: status

      _ASSERT(size(A,3) == size(x), 'FieldGEMV() - array A not nonformable with x.')
      _ASSERT(size(A,2) == size(y), 'FieldGEMV() - array A not nonformable with y.')

      call verify_typekind(x, ESMF_TYPEKIND_R4)
      call verify_typekind(y, ESMF_TYPEKIND_R4)

      conformable = FieldsAreConformable(x(1), x(2:))
      _ASSERT(conformable, 'FieldGEMV() - fields not conformable.')
      conformable = FieldsAreConformable(x(1), y)
      _ASSERT(conformable, 'FieldGEMV() - fields not conformable.')

      ! Reference dimensions
      local_element_count = FieldGetLocalElementCount(x(1), _RC)
      call ESMF_FieldGet(x(1), dimcount=dimcount, _RC)

      n_gridded = product(local_element_count(1:dimcount))
      n_ungridded = product(local_element_count(dimcount+1:))
      _ASSERT(size(A,1) == n_gridded, 'FieldGEMV() - array A not nonformable with gridded dims.')
      fp_shape = [n_gridded, n_ungridded]

!      y = matmul(A, x)
      do jy = 1, size(y)
         call assign_fptr(y(jy), fp_shape, y_ptr, _RC)
         y_ptr(:,jy) = beta * y_ptr(:,jy)
!         call FieldSCAL(beta, y_ptr(:,jy), _RC)

         do ix = 1, size(x)
            call assign_fptr(x(ix), fp_shape, x_ptr, _RC)
            do kv = 1, n_ungridded
               y_ptr(:,jy) = y_ptr(:,jy) + alpha * A(:,ix,jy) * x_ptr(:,kv)
            end do
         end do
      end do

      _RETURN(_SUCCESS)
   end subroutine gemv_r4

   ! Double precision version (R8) of gemv. See gemv_r4 (single precision)

   subroutine gemv_r8(alpha, A, x, beta, y, rc)
      real(kind=ESMF_KIND_R8), intent(in) :: alpha
      real(kind=ESMF_KIND_R8), intent(in) :: A(:,:,:)
      type(ESMF_Field), intent(inout) :: x(:)
      real(kind=ESMF_KIND_R8), intent(in) :: beta
      type(ESMF_Field), intent(inout) :: y(:)
      integer, optional, intent(out) :: rc

      logical :: conformable
      integer :: dimcount
      integer, allocatable :: local_element_count(:)
      integer(kind=ESMF_KIND_I8) :: n_gridded, n_ungridded
      integer(kind=ESMF_KIND_I8) :: fp_shape(2)
      real(kind=ESMF_KIND_R8), pointer :: x_ptr(:,:), y_ptr(:,:)
      integer :: ix, jy, kv
      integer :: status

      _ASSERT(size(A,3) == size(x), 'FieldGEMV() - array A not nonformable with x.')
      _ASSERT(size(A,2) == size(y), 'FieldGEMV() - array A not nonformable with y.')

      call verify_typekind(x, ESMF_TYPEKIND_R8)
      call verify_typekind(y, ESMF_TYPEKIND_R8)

      conformable = FieldsAreConformable(x(1), x(2:))
      _ASSERT(conformable, 'FieldGEMV() - fields not conformable.')
      conformable = FieldsAreConformable(x(1), y)
      _ASSERT(conformable, 'FieldGEMV() - fields not conformable.')

      ! Reference dimensions
      local_element_count = FieldGetLocalElementCount(x(1), _RC)
      call ESMF_FieldGet(x(1), dimcount=dimcount, _RC)

      n_gridded = product(local_element_count(1:dimcount))
      n_ungridded = product(local_element_count(dimcount+1:))
      _ASSERT(size(A,1) == n_gridded, 'FieldGEMV() - array A not nonformable with gridded dims.')
      fp_shape = [n_gridded, n_ungridded]

!      y = matmul(A, x)
      do jy = 1, size(y)
         call assign_fptr(y(jy), fp_shape, y_ptr, _RC)
         y_ptr(:,jy) = beta * y_ptr(:,jy)
!         call FieldSCAL(beta, y_ptr(:,jy), _RC)

         do ix = 1, size(x)
            call assign_fptr(x(ix), fp_shape, x_ptr, _RC)
            do kv = 1, n_ungridded
               y_ptr(:,jy) = y_ptr(:,jy) + alpha * A(:,ix,jy) * x_ptr(:,kv)
            end do
         end do
      end do

      _RETURN(_SUCCESS)
   end subroutine gemv_r8

   function spread_scalar(source, ncopies, rc) result(vector)
      type(ESMF_Field), intent(inout) :: source
      integer, intent(in) :: ncopies
      integer, optional, intent(out) :: rc
      type(ESMF_Field), allocatable :: vector(:)
      integer :: i
      integer :: status

      _ASSERT(ncopies > 0, 'ncopies must be positive')
      
      allocate(vector(ncopies))

      do i=1, ncopies
         call FieldCOPY(source, vector(i), _RC)
      end do

      _RETURN(_SUCCESS)
   end function spread_scalar

   subroutine clone(x, y, rc)
      type(ESMF_Field), intent(inout) :: x
      type(ESMF_Field), intent(inout) :: y
      integer, optional, intent(out) :: rc
      
      character(len=*), parameter :: CLONE='_clone'
      type(ESMF_ArraySpec) :: arrayspec
      type(ESMF_Grid) :: grid
      type(ESMF_StaggerLoc) :: staggerloc
      integer, allocatable :: gridToFieldMap(:)
      integer, allocatable :: ungriddedLBound(:)
      integer, allocatable :: ungriddedUBound(:)
      integer, allocatable :: totalLWidth(:,:)
      integer, allocatable :: totalUWidth(:,:)
      character(len=:), allocatable :: name

      call ESMF_FieldGet(x, arrayspec=arrayspec, grid=grid, &
         staggerloc=staggerloc, gridToFieldMap=gridToFieldMap, &
         ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
         totalLWidth=totalLWidth, totalUWidth=totalUWidth, _RC)

      name = name // CLONE

      y = ESMF_FieldCreate(grid, arrayspec, staggerloc=staggerloc, &
         gridToFieldMap=gridToFieldMap, ungriddedLBound=ungriddedLBound, &
         ungriddedUBound=ungriddedUBound, totalLWidth=totalLWidth, &
         totalUWidth=totalUWidth, name=name, _RC)

      _RETURN(_SUCCESS)
   end subroutine clone

   subroutine get_typekind(x, expected_tks, actual_tk, rc)
      type(ESMF_Field), intent(inout) :: x
      type(ESMF_TypeKind_Flag), intent(in) :: expected_tks(:)
      type(ESMF_TypeKind_Flag), intent(out) :: actual_tk
      type(ESMF_TypeKind_Flag) :: found_tk
      integer, optional, intent(out) :: rc 
      integer :: status
      integer :: i
      
      do i = 1, size(expected_tks)
         actual_tk = expected_tks(i)
         call ESMF_FieldGet(x, typekind=found_tk, _RC)
         if(actual_tk == found_tk) return
      end do

      _FAIL('Does not match any expected typekind')

   end subroutine get_typekind

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

!   subroutine verify_typekind_rank1(x, expected_tk, rc)
!      type(ESMF_Field), intent(inout) :: x(:)
!      type(ESMF_TypeKind_Flag), intent(in) :: expected_tk
!      integer, optional, intent(out) :: rc
!      
!      integer :: status
!      integer :: i
!      
!      do i = 1, size(x)
!         call verify_typekind(x(i), expected_tk, _RC)
!      end do
!      
!      _RETURN(_SUCCESS)
!   end subroutine verify_typekind_rank1

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

   subroutine convert_prec(x, y, rc)
      type(ESMF_Field), intent(inout) :: x
      type(ESMF_Field), intent(inout) :: y
      integer, optional, intent(out) :: rc

      type(ESMF_TypeKind_Flag), parameter :: expected_tks(2) = [ESMF_TYPEKIND_R4, ESMF_TYPEKIND_R8]
      type(ESMF_TypeKind_Flag) :: tk_x, tk_y
      integer :: status

      call ESMF_FieldGet(x, typekind=tk_x, _RC)
      _ASSERT(is_valid_typekind(tk_x, expected_tks), 'Unexpected typekind')
      call ESMF_FieldGet(y, typekind=tk_y, _RC)
      _ASSERT(is_valid_typekind(tk_y, expected_tks), 'Unexpected typekind')

      if(tk_x == tk_y) then
         call FieldCOPY(x, y, _RC)
      else if(tk_x == ESMF_TYPEKIND_R4) then
         call convert_prec_R4_to_R8(x, y, _RC)
      else
         call convert_prec_R8_to_R4(x, y, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine convert_prec
   
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

   subroutine convert_prec_R4_to_R8(original, converted, rc)
      type(ESMF_Field), intent(inout) :: original
      type(ESMF_Field), intent(inout) :: converted
      integer, optional, intent(out) :: rc
      integer :: status

      real(kind=ESMF_KIND_R4), pointer :: original_ptr(:)
      real(kind=ESMF_KIND_R8), pointer :: converted_ptr(:)

      call assign_fptr(original, original_ptr, _RC)
      call assign_fptr(converted, converted_ptr, _RC)

      converted_ptr = original_ptr

      _RETURN(_SUCCESS)
   end subroutine convert_prec_R4_to_R8

   subroutine convert_prec_R8_to_R4(original, converted, rc)
      type(ESMF_Field), intent(inout) :: original
      type(ESMF_Field), intent(inout) :: converted
      integer, optional, intent(out) :: rc
      integer :: status

      real(kind=ESMF_KIND_R8), pointer :: original_ptr(:)
      real(kind=ESMF_KIND_R4), pointer :: converted_ptr(:)
      
      call assign_fptr(original, original_ptr, _RC)
      call assign_fptr(converted, converted_ptr, _RC)

      converted_ptr = original_ptr

      _RETURN(_SUCCESS)
   end subroutine convert_prec_R8_to_R4

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
!   logical function are_conformable_array_array(x, y) result(conformable)
!      type(ESMF_Field), intent(inout) :: x(:)
!      type(ESMF_Field), intent(inout) :: y(:)
!
!      integer :: status
!      integer :: j
!
!      conformable = .false.
!
!      if(size(x) == 1) then
!         do j = 1, size(y)
!            if(.not. FieldsAreConformable(x(1), y(j))) return
!         end do
!      elseif(size(x) == size(y)) then
!         do j = 1, size(y)
!            if(.not. FieldsAreConformable(x(j), y(j))) return
!         end do
!      else
!         return
!      end if
!
!      conformable = .true.
!
!   end function are_conformable_array_array
!
!   logical function are_conformable_scalar_array(x, y, rc) result(conformable)
!      type(ESMF_Field), intent(inout) :: x
!      type(ESMF_Field), intent(inout) :: y(:)
!      integer, optional, intent(out) :: rc
!
!      integer :: status
!      integer :: j
!
!      do j = 1, size(y)
!         conformable = FieldsAreConformable(x, y(j))
!      end do
!
!      _RETURN(_SUCCESS)
!   end function are_conformable_scalar_array
!
!   logical function are_conformable_array_scalar(x, y, rc) result(conformable)
!      type(ESMF_Field), intent(inout) :: x(:)
!      type(ESMF_Field), intent(inout) :: y
!      integer, optional, intent(out) :: rc
!
!      integer :: status
!
!      conformable = FieldsAreConformable(y, x)
!
!      _RETURN(_SUCCESS)
!   end function are_conformable_array_scalar

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

!wdb   fixme Not implemented
!   function sin(f, rc) result(sin_f)
!      type(ESMF_Field), intent(inout) ::f
!      integer, optional, intent(out) :: rc
!      type(ESMF_Field) :: sin_f
!   end function sin

end module mapl3g_FieldBLAS
