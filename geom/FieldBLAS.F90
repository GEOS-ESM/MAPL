#include "MAPL_Generic.h"

module mapl3g_FieldBLAS
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
   public :: FieldConvertPrec
   public :: FieldClone

   public :: FieldGetLocalElementCount
   public :: FieldGetLocalSize
   public :: FieldGetCptr !wdb Should this be public? or any of these utils?
   public :: FieldsAreConformable
   public :: FieldsAreSameTypeKind

   interface FieldCOPY
      procedure copy
   end interface FieldCOPY

   interface FieldSCAL
      procedure scale_r4
      procedure scale_r8
   end interface

   interface FieldAXPY
      procedure axpy_r4
      procedure axpy_r8
   end interface

   interface FieldGEMV
      procedure gemv_r4
      procedure gemv_r8
   end interface

   interface FieldGetCptr
      procedure get_cptr
   end interface

   interface FieldsAreConformable
      procedure areConformable_scalar_scalar
      procedure areConformable_scalar_vector
   end interface

   interface verify_typekind
      procedure verify_typekind_scalar
      procedure verify_typekind_1d
   end interface verify_typekind

   interface FieldGetLocalSize
      procedure getLocalSize
   end interface FieldGetLocalSize

   interface FieldGetLocalElementCount
      procedure getLocalElementCount
   end interface FieldGetLocalElementCount

   interface is_identity
      module procedure is_r4_identity
      module procedure is_r8_identity
   end interface is_identity

   interface assign_fptr 
      module procedure assign_fptr_r4
      module procedure assign_fptr_r8
   end interface assign_fptr 

contains

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
   
      conformable = FieldsAreConformable(x, y,_RC)
      _ASSERT(conformable, 'FieldCopy() - fields not conformable.')
     !wdb  todo Could I use assign_fptr? Pros: No need to define cptr__ or n; Cons: Have to duplicate code, or do I? 
      call FieldGetCptr(x, cptr_x, _RC)
      call ESMF_FieldGet(x, typekind = tk_x, _RC) 

      n  = FieldGetLocalSize(x, _RC)

      call FieldGetCptr(y, cptr_y, _RC)
      call ESMF_FieldGet(y, typekind = tk_y, _RC) 
      
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

      call c_f_pointer(cptr_x, x_ptr, shape = [n])
      call c_f_pointer(cptr_y, y_ptr, shape = [n])

      y_ptr=x_ptr
   end subroutine copy_r4_r4
   
   subroutine copy_r4_r8(cptr_x, cptr_y, n)
      type(c_ptr), intent(in) :: cptr_x, cptr_y
      integer(ESMF_KIND_I8), intent(in) :: n

      real(kind=ESMF_KIND_R4), pointer :: x_ptr(:)
      real(kind=ESMF_KIND_R8), pointer :: y_ptr(:)

      call c_f_pointer(cptr_x, x_ptr, shape = [n])
      call c_f_pointer(cptr_y, y_ptr, shape = [n])

      y_ptr=x_ptr
   end subroutine copy_r4_r8
   
   subroutine copy_r8_r4(cptr_x, cptr_y, n)
      type(c_ptr), intent(in) :: cptr_x, cptr_y
      integer(ESMF_KIND_I8), intent(in) :: n

      real(kind=ESMF_KIND_R8), pointer :: x_ptr(:)
      real(kind=ESMF_KIND_R4), pointer :: y_ptr(:)

      call c_f_pointer(cptr_x, x_ptr, shape = [n])
      call c_f_pointer(cptr_y, y_ptr, shape = [n])

      y_ptr=x_ptr
   end subroutine copy_r8_r4
   
   subroutine copy_r8_r8(cptr_x, cptr_y, n)
      type(c_ptr), intent(in) :: cptr_x, cptr_y
      integer(ESMF_KIND_I8), intent(in) :: n

      real(kind=ESMF_KIND_R8), pointer :: x_ptr(:)
      real(kind=ESMF_KIND_R8), pointer :: y_ptr(:)

      call c_f_pointer(cptr_x, x_ptr, shape = [n])
      call c_f_pointer(cptr_y, y_ptr, shape = [n])

      y_ptr=x_ptr
   end subroutine copy_r8_r8

   logical function is_r4_identity(a)
      real(REAL32), intent(in) :: a
      is_r4_identity = (a == 1)
   end function is_r4_identity
   
   logical function is_r8_identity(a) 
      real(REAL64), intent(in) :: a
      is_r8_identity = (a == 1)
   end function is_r8_identity
   
   subroutine scale_r4(a, x, rc)
      real(kind=ESMF_KIND_R4), intent(in) :: a
      type(ESMF_Field), intent(inout) :: x
      integer, optional, intent(out) :: rc

      real(kind=ESMF_KIND_R4), pointer :: x_ptr(:)
      integer :: status

      if(.not. is_identity(a)) then
         call assign_fptr(x, x_ptr, _RC)
         ! sscal BLAS
         x_ptr = a * x_ptr
      end if

      _RETURN(_SUCCESS)
   end subroutine scale_r4
   
   subroutine scale_r8(a, x, rc)
      real(kind=ESMF_KIND_R8), intent(in) :: a
      type(ESMF_Field), intent(inout) :: x
      integer, optional, intent(out) :: rc 

      real(kind=ESMF_KIND_R8), pointer :: x_ptr(:)
      integer :: status

      if(.not. is_identity(a)) then
         call assign_fptr(x, x_ptr, _RC)
         ! dscal BLAS
         x_ptr = a * x_ptr
      end if

      _RETURN(_SUCCESS)
   end subroutine scale_r8

   subroutine axpy_r4(a, x, y, rc)
      real(kind=ESMF_KIND_R4), intent(in) :: a
      type(ESMF_Field), intent(inout) :: x
      type(ESMF_Field), intent(inout) :: y
      integer, optional, intent(out) :: rc

      type(c_ptr) :: x_cptr, y_cptr
      real(kind=ESMF_KIND_R4), pointer :: x_ptr(:), y_ptr(:)
      integer(kind=ESMF_KIND_R8) :: n
      logical :: same_tk, conformable
      integer :: status

      call verify_typekind(x, ESMF_TYPEKIND_R4, _RC)
      call verify_typekind(y, ESMF_TYPEKIND_R4, _RC)

      conformable = FieldsAreConformable(x, y, _RC)
      _ASSERT(conformable, 'FieldAXPY() - fields not conformable.')
      
      call assign_fptr(x, x_ptr, _RC)
      call assign_fptr(y, y_ptr, _RC)

      ! saxpy 
      if(is_identity(a)) then
         y_ptr = y_ptr + x_ptr
      else
         y_ptr = y_ptr + a * x_ptr
      end if

      _RETURN(_SUCCESS)
   end subroutine axpy_r4

   subroutine axpy_r8(a, x, y, rc)
      real(kind=ESMF_KIND_R8), intent(in) :: a
      type(ESMF_Field), intent(inout) :: x
      type(ESMF_Field), intent(inout) :: y
      integer, optional, intent(out) :: rc

      type(c_ptr) :: x_cptr, y_cptr
      real(kind=ESMF_KIND_R8), pointer :: x_ptr(:), y_ptr(:)
      integer(kind=ESMF_KIND_R8) :: n
      integer :: status
      logical :: conformable

      call verify_typekind(x, ESMF_TYPEKIND_R8, _RC)
      call verify_typekind(y, ESMF_TYPEKIND_R8, _RC)

      conformable = FieldsAreConformable(x, y,_RC)
      _ASSERT(conformable, 'FieldAXPY() - fields not conformable.')
      
      call assign_fptr(x, x_ptr, _RC)
      call assign_fptr(y, y_ptr, _RC)

      ! daxpy 
      if(is_identity(a)) then
         y_ptr = y_ptr + x_ptr
      else
         y_ptr = y_ptr + a * x_ptr
      end if

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
      integer, allocatable :: localElementCount(:)
      integer(kind=ESMF_KIND_I8) :: n_gridded, n_ungridded
      integer(kind=ESMF_KIND_I8) :: fp_shape(2)
      real(kind=ESMF_KIND_R4), pointer :: x_ptr(:,:), y_ptr(:,:)
      integer :: xi, yi, uni
      integer :: status

      _ASSERT(size(A,3) == size(x), 'FieldGEMV() - array A not nonformable with x.')
      _ASSERT(size(A,2) == size(y), 'FieldGEMV() - array A not nonformable with y.')

      call verify_typekind(x, ESMF_TYPEKIND_R4, _RC)
      call verify_typekind(y, ESMF_TYPEKIND_R4, _RC)

      conformable = FieldsAreConformable(x(1), x(2:), _RC)
      _ASSERT(conformable, 'FieldGEMV() - fields not conformable.')
      conformable = FieldsAreConformable(x(1), y, _RC)
      _ASSERT(conformable, 'FieldGEMV() - fields not conformable.')

      ! Reference dimensions
      localElementCount = FieldGetLocalElementCount(x(1), _RC)
      call ESMF_FieldGet(x(1), dimcount=dimcount, _RC)

      n_gridded = product(localElementCount(1:dimcount))
      n_ungridded = product(localElementCount(dimcount+1:))
      _ASSERT(size(A,1) == n_gridded, 'FieldGEMV() - array A not nonformable with gridded dims.')
      fp_shape = [n_gridded, n_ungridded]

!      y = matmul(A, x)
      do yi = 1, size(y)
         call assign_fptr(y(yi), y_ptr, fp_shape, _RC)
         y_ptr(:,yi) = beta * y_ptr(:,yi)

         do xi = 1, size(x)
            call assign_fptr(x(xi), x_ptr, fp_shape, _RC)
            do uni = 1, n_ungridded
               y_ptr(:,yi) = y_ptr(:,yi) + alpha * A(:,xi,yi) * x_ptr(:,uni)
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
      integer, allocatable :: localElementCount(:)
      integer(kind=ESMF_KIND_I8) :: n_gridded, n_ungridded
      integer(kind=ESMF_KIND_I8) :: fp_shape(2)
      real(kind=ESMF_KIND_R8), pointer :: x_ptr(:,:), y_ptr(:,:)
      integer :: xi, yi, uni
      integer :: status

      _ASSERT(size(A,3) == size(x), 'FieldGEMV() - array A not nonformable with x.')
      _ASSERT(size(A,2) == size(y), 'FieldGEMV() - array A not nonformable with y.')

      call verify_typekind(x, ESMF_TYPEKIND_R8, _RC)
      call verify_typekind(y, ESMF_TYPEKIND_R8, _RC)

      conformable = FieldsAreConformable(x(1), x(2:), _RC)
      _ASSERT(conformable, 'FieldGEMV() - fields not conformable.')
      conformable = FieldsAreConformable(x(1), y, _RC)
      _ASSERT(conformable, 'FieldGEMV() - fields not conformable.')

      ! Reference dimensions
      localElementCount = FieldGetLocalElementCount(x(1), _RC)
      call ESMF_FieldGet(x(1), dimcount=dimcount, _RC)

      n_gridded = product(localElementCount(1:dimcount))
      n_ungridded = product(localElementCount(dimcount+1:))
      _ASSERT(size(A,1) == n_gridded, 'FieldGEMV() - array A not nonformable with gridded dims.')
      fp_shape = [n_gridded, n_ungridded]

!      y = matmul(A, x)
      do yi = 1, size(y)
         call assign_fptr(y(yi), y_ptr, fp_shape, _RC)
         y_ptr(:,yi) = beta * y_ptr(:,yi)

         do xi = 1, size(x)
            call assign_fptr(x(xi), x_ptr, fp_shape, _RC)
            do uni = 1, n_ungridded
               y_ptr(:,yi) = y_ptr(:,yi) + alpha * A(:,xi,yi) * x_ptr(:,uni)
            end do
         end do
      end do

      _RETURN(_SUCCESS)
   end subroutine gemv_r8

   subroutine verify_kind_scalar(x, expected_tk, rc)
      type(ESMF_Field), intent(in) :: x(:)
      type(ESMF_TypeKind_Flag), intent(in) :: expected_tk
      integer, optional, intent(out) :: rc
      
      integer :: status
      
      call ESMF_FieldGet(x, typekind=found_tk, _RC)

      _ASSERT(found_tk == expected_tk, 'Found incorrect typekind.')
      _RETURN(_SUCCESS)
   end subroutine verify_kind_scalar


   subroutine verify_kind_vector(x, expected_tk, rc)
      type(ESMF_Field), intent(in) :: x(:)
      type(ESMF_TypeKind_Flag), intent(in) :: expected_tk
      integer, optional, intent(out) :: rc
      
      integer :: status
      integer :: i
      
      do i = 1, size(x)
         call verify_kind(x(i), expected_tk, _RC)
      end do
      
      _RETURN(_SUCCESS)
   end subroutine verify_kind_vector

   subroutine assign_fptr_r4(x, fptr, n, rc)
      type(ESMF_Field), intent(inout) :: x
      real(kind=ESMF_KIND_R4), dimension(:), pointer, intent(out) :: fptr
      integer(ESMF_KIND_I8), dimension(:), optional, intent(in) :: n
      integer, optional, intent(out) :: rc

      ! local declarations
      type(c_ptr) :: cptr
      integer(ESMF_KIND_I8), allocatable :: fp_shape(:)
      call get_fptr_arguments(x, cptr, fp_shape, n, _RC)
      call c_f_ptr(cptr, fptr, shape = fp_shape)

      _RETURN(_SUCCESS)
   end subroutine assign_fptr_r4

   subroutine assign_fptr_r8(x, fptr, n, rc)
      type(ESMF_Field), intent(inout) :: x
      real(kind=ESMF_KIND_R8), dimension(:), pointer, intent(out) :: fptr
      integer(ESMF_KIND_I8), dimension(:), optional, intent(in) :: n
      integer, optional, intent(out) :: rc

      ! local declarations
      type(c_ptr) :: cptr
      integer(ESMF_KIND_I8), allocatable :: fp_shape(:)
      call get_fptr_arguments(x, cptr, fp_shape, n, _RC)
      call c_f_ptr(cptr, fptr, shape = fp_shape)

      _RETURN(_SUCCESS)
   end subroutine assign_fptr_r8

   subroutine get_fptr_arguments(x, cptr, fp_shape, n, rc)
      type(ESMF_Field), intent(inout) :: x
      type(c_ptr), intent(out) :: cptr
      integer(ESMF_KIND_I8), allocatable, intent(out) :: fp_shape(:)
      integer(ESMF_KIND_I8), dimension(:), optional, intent(in) :: n
      integer, optional, intent(out) :: rc 

      integer(ESMF_KIND_I8) :: local_size
      integer :: status

      if(present(n)) then
         fp_shape = n
      else
         local_size = FieldGetLocalSize(x, _RC)
         fp_shape = [ local_size ]
      end if 
      call FieldGetCptr(x, cptr, _RC)

      _RETURN(_SUCCESS)
   end subroutine get_fptr_arguments

   subroutine get_cptr(x, cptr, rc)
      type(ESMF_Field), intent(inout) :: x
      type(c_ptr), intent(out) :: cptr
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_TypeKind_Flag) :: tk_x

      call ESMF_FieldGet(f, typekind=tk_x, _RC)

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
         call ESMF_FieldGet(x, x_1d, _RC)
         cptr = c_loc(x_1d)
      case (2)
         call ESMF_FieldGet(x, x_2d, _RC)
         cptr = c_loc(x_2d)
      case (3)
         call ESMF_FieldGet(x, x_3d, _RC)
         cptr = c_loc(x_3d)
      case (4)
         call ESMF_FieldGet(x, x_4d, _RC)
         cptr = c_loc(x_4d)
      case (5)
         call ESMF_FieldGet(x, x_5d, _RC)
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
         call ESMF_FieldGet(x, x_1d, _RC)
         cptr = c_loc(x_1d)
      case (2)
         call ESMF_FieldGet(x, x_2d, _RC)
         cptr = c_loc(x_2d)
      case (3)
         call ESMF_FieldGet(x, x_3d, _RC)
         cptr = c_loc(x_3d)
      case (4)
         call ESMF_FieldGet(x, x_4d, _RC)
         cptr = c_loc(x_4d)
      case (5)
         call ESMF_FieldGet(x, x_5d, _RC)
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
         call ESMF_FieldGet(x, x_1d, _RC)
         cptr = c_loc(x_1d)
      case (2)
         call ESMF_FieldGet(x, x_2d, _RC)
         cptr = c_loc(x_2d)
      case (3)
         call ESMF_FieldGet(x, x_3d, _RC)
         cptr = c_loc(x_3d)
      case (4)
         call ESMF_FieldGet(x, x_4d, _RC)
         cptr = c_loc(x_4d)
      case (5)
         call ESMF_FieldGet(x, x_5d, _RC)
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
         call ESMF_FieldGet(x, x_1d, _RC)
         cptr = c_loc(x_1d)
      case (2)
         call ESMF_FieldGet(x, x_2d, _RC)
         cptr = c_loc(x_2d)
      case (3)
         call ESMF_FieldGet(x, x_3d, _RC)
         cptr = c_loc(x_3d)
      case (4)
         call ESMF_FieldGet(x, x_4d, _RC)
         cptr = c_loc(x_4d)
      case (5)
         call ESMF_FieldGet(x, x_5d, _RC)
         cptr = c_loc(x_5d)
      case default
         _FAIL('Unsupported rank in FieldGetCptr().')
      end select

      _RETURN(_SUCCESS)
   end subroutine get_cptr_i8


   subroutine convertprec(x, y, rc)
      type(ESMF_Field), intent(in) :: x
      type(ESMF_Field), intent(inout) :: y
      integer, optional, intent(out) :: rc

      integer :: status



      _RETURN(_SUCCESS)
   end subroutine convertprec

   logical function areConformable_scalar_scalar(x, y, rc) result(conformable)
      type(ESMF_Field), intent(in) :: x
      type(ESMF_Field), intent(in) :: y
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: rank_x, rank_y
      integer, allocatable :: localElementCount_x(:)
      integer, allocatable :: localElementCount_y(:)

      conformable = .false. ! default
      
      call ESMF_FieldGet(x, rank=rank_x, _RC)
      call ESMF_FieldGet(y, rank=rank_y, _RC)

      conformable = (rank_x == rank_y)

      if (.not. conformable) then
         _RETURN(_SUCCESS)
      end if

      localElementCount_x = FieldGetLocalElementCount(x, _RC)
      localElementCount_y = FieldGetLocalElementCount(y, _RC)

      conformable = all(localElementCount_x == localElementCount_y)

      _RETURN(_SUCCESS)
   end function areConformable_scalar_scalar

   logical function areConformable_scalar_vector(x, y, rc) result(conformable)
      type(ESMF_Field), intent(in) :: x
      type(ESMF_Field), intent(in) :: y(:)
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: j

      do j = 1, size(y)
         conformable = FieldsAreConformable(x, y(j), _RC)
      end do

      _RETURN(_SUCCESS)
   end function areConformable_vector_vector

   logical function areSameTypeKind(x, y, rc) result(same_tk)
      type(ESMF_Field), intent(in) :: x
      type(ESMF_Field), intent(in) :: y
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: tk_x, tk_y

      same_tk = .false.
      call ESMF_FieldGet(x, typekind=tk_x, _RC)
      call ESMF_FieldGet(y, typekind=tk_y, _RC)

      same_tk = (tk_x == tk_y)

      _RETURN(_SUCCESS)
   end function areSameTypeKind

   function getLocalElementCount(x, element_count, rc) result(element_count)
      type(ESMF_Field), intent(in) :: x
      integer, optional, intent(out) :: rc
      integer, allocatable :: element_count(:)

      integer :: status
      integer :: rank

      element_count = [integer :: ] ! default

      call ESMF_FieldGet(x, rank=rank, _RC)
      allocate(element_count(rank))
      call ESMF_FieldGet(x, localElementCount=element_count, _RC)

      _RETURN(_SUCCESS)
   end function getLocalElementCount

   function getLocalSize(x, rc) result(sz)
      integer(kind=ESMF_KIND_I8) :: sz
      type(ESMF_Field), intent(in) :: x
      integer, optional, intent(out) :: rc

      integer :: status
      integer, allocatable :: element_count(:)

      sz = 0
      element_count = FieldGetLocalElementCount(x, _RC)
      sz = product(element_count, kind=ESMF_KIND_I8)

      _RETURN(_SUCCESS)
   end function getLocalSize


   function sin(f, rc) result(sin_f)
      type(ESMF_Field), intent(inout) ::f
      type(ESMF_Field) :: sin_f
   end function sin

end module mapl3g_FieldBLAS
