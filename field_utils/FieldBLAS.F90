#include "MAPL_Generic.h"

module mapl_FieldBLAS
   use ESMF
   use MAPL_ExceptionHandling
   use MAPL_FieldPointerUtilities
   implicit none
   private

   ! Level 1 BLAS
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

   interface FieldConvertPrec
      module procedure convert_prec
   end interface FieldConvertPrec
   
   interface FieldSpread
      module procedure spread_scalar
   end interface FieldSpread

   interface verify_typekind
      module procedure verify_typekind_scalar
      module procedure verify_typekind_array
   end interface verify_typekind

contains

   subroutine scale_r4(a, x, rc)
      real(kind=ESMF_KIND_R4), intent(in) :: a
      type(ESMF_Field), intent(inout) :: x
      integer, optional, intent(out) :: rc

      real(kind=ESMF_KIND_R4), pointer :: x_ptr(:)
      integer :: status

      call assign_fptr(x, x_ptr, _rc)
      x_ptr = a * x_ptr

      _return(_success)
   end subroutine scale_r4
   
   subroutine scale_r8(a, x, rc)
      real(kind=ESMF_KIND_R8), intent(in) :: a
      type(ESMF_Field), intent(inout) :: x
      integer, optional, intent(out) :: rc 

      real(kind=ESMF_KIND_R8), pointer :: x_ptr(:)
      integer :: status

      call assign_fptr(x, x_ptr, _rc)
      x_ptr = a * x_ptr

      _return(_success)
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
      _assert(conformable, 'FieldAXPY() - fields not conformable.')
      
      call assign_fptr(x, x_ptr, _rc)
      call assign_fptr(y, y_ptr, _rc)

      y_ptr = y_ptr + a * x_ptr

      _return(_success)
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
      _assert(conformable, 'FieldAXPY() - fields not conformable.')
      
      call assign_fptr(x, x_ptr, _rc)
      call assign_fptr(y, y_ptr, _rc)

      y_ptr = y_ptr + a * x_ptr

      _return(_success)
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

      _assert(size(A,3) == size(x), 'FieldGEMV() - array A not nonformable with x.')
      _assert(size(A,2) == size(y), 'FieldGEMV() - array A not nonformable with y.')

      call verify_typekind(x, ESMF_TYPEKIND_R4)
      call verify_typekind(y, ESMF_TYPEKIND_R4)

      conformable = FieldsAreConformable(x(1), x(2:))
      _assert(conformable, 'FieldGEMV() - fields not conformable.')
      conformable = FieldsAreConformable(x(1), y)
      _assert(conformable, 'FieldGEMV() - fields not conformable.')

      ! Reference dimensions
      local_element_count = FieldGetLocalElementCount(x(1), _rc)
      call ESMF_FieldGet(x(1), dimcount=dimcount, _rc)

      n_gridded = product(local_element_count(1:dimcount))
      n_ungridded = product(local_element_count(dimcount+1:))
      _assert(size(A,1) == n_gridded, 'FieldGEMV() - array A not nonformable with gridded dims.')
      fp_shape = [n_gridded, n_ungridded]

!      y = matmul(A, x)
      do jy = 1, size(y)
         call assign_fptr(y(jy), fp_shape, y_ptr, _rc)
         y_ptr(:,jy) = beta * y_ptr(:,jy)
!         call FieldSCAL(beta, y_ptr(:,jy), _rc)

         do ix = 1, size(x)
            call assign_fptr(x(ix), fp_shape, x_ptr, _rc)
            do kv = 1, n_ungridded
               y_ptr(:,jy) = y_ptr(:,jy) + alpha * A(:,ix,jy) * x_ptr(:,kv)
            end do
         end do
      end do

      _return(_success)
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

      _assert(size(A,3) == size(x), 'FieldGEMV() - array A not nonformable with x.')
      _assert(size(A,2) == size(y), 'FieldGEMV() - array A not nonformable with y.')

      call verify_typekind(x, ESMF_TYPEKIND_R8)
      call verify_typekind(y, ESMF_TYPEKIND_R8)

      conformable = FieldsAreConformable(x(1), x(2:))
      _assert(conformable, 'FieldGEMV() - fields not conformable.')
      conformable = FieldsAreConformable(x(1), y)
      _assert(conformable, 'FieldGEMV() - fields not conformable.')

      ! Reference dimensions
      local_element_count = FieldGetLocalElementCount(x(1), _rc)
      call ESMF_FieldGet(x(1), dimcount=dimcount, _rc)

      n_gridded = product(local_element_count(1:dimcount))
      n_ungridded = product(local_element_count(dimcount+1:))
      _assert(size(A,1) == n_gridded, 'FieldGEMV() - array A not nonformable with gridded dims.')
      fp_shape = [n_gridded, n_ungridded]

!      y = matmul(A, x)
      do jy = 1, size(y)
         call assign_fptr(y(jy), fp_shape, y_ptr, _rc)
         y_ptr(:,jy) = beta * y_ptr(:,jy)
!         call FieldSCAL(beta, y_ptr(:,jy), _rc)

         do ix = 1, size(x)
            call assign_fptr(x(ix), fp_shape, x_ptr, _rc)
            do kv = 1, n_ungridded
               y_ptr(:,jy) = y_ptr(:,jy) + alpha * A(:,ix,jy) * x_ptr(:,kv)
            end do
         end do
      end do

      _return(_success)
   end subroutine gemv_r8

   function spread_scalar(source, ncopies, rc) result(vector)
      type(ESMF_Field), intent(inout) :: source
      integer, intent(in) :: ncopies
      integer, optional, intent(out) :: rc
      type(ESMF_Field), allocatable :: vector(:)
      integer :: i
      integer :: status

      _assert(ncopies > 0, 'ncopies must be positive')
      
      allocate(vector(ncopies))

      do i=1, ncopies
         call FieldCOPY(source, vector(i), _rc)
      end do

      _return(_success)
   end function spread_scalar

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
         call ESMF_FieldGet(x, typekind=found_tk, _rc)
         if(actual_tk == found_tk) return
      end do

      _fail('Does not match any expected typekind')

   end subroutine get_typekind

    subroutine verify_typekind_scalar(x, expected_tk, rc)
      type(ESMF_Field), intent(inout) :: x
      type(ESMF_TypeKind_Flag), intent(in) :: expected_tk
      integer, optional, intent(out) :: rc
      
      integer :: status

      type(ESMF_TypeKind_Flag) :: found_tk
      
      call ESMF_FieldGet(x, typekind=found_tk, _rc)

      _assert((found_tk == expected_tk), 'Found incorrect typekind.')
      _return(_success)   
   end subroutine verify_typekind_scalar

   subroutine verify_typekind_array(x, expected_tk, rc)
      type(ESMF_Field), intent(inout) :: x(:)
      type(ESMF_TypeKind_Flag), intent(in) :: expected_tk
      integer, optional, intent(out) :: rc
      
      integer :: status
      integer :: i

      do i = 1, size(x)
         call verify_typekind(x(i), expected_tk, _rc)
      end do
      _return(_success)   
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
!         call verify_typekind(x(i), expected_tk, _rc)
!      end do
!      
!      _return(_success)
!   end subroutine verify_typekind_rank1

   subroutine convert_prec(x, y, rc)
      type(ESMF_Field), intent(inout) :: x
      type(ESMF_Field), intent(inout) :: y
      integer, optional, intent(out) :: rc

      type(ESMF_TypeKind_Flag), parameter :: expected_tks(2) = [ESMF_TYPEKIND_R4, ESMF_TYPEKIND_R8]
      type(ESMF_TypeKind_Flag) :: tk_x, tk_y
      integer :: status

      call ESMF_FieldGet(x, typekind=tk_x, _rc)
      _assert(is_valid_typekind(tk_x, expected_tks), 'Unexpected typekind')
      call ESMF_FieldGet(y, typekind=tk_y, _rc)
      _assert(is_valid_typekind(tk_y, expected_tks), 'Unexpected typekind')

      if(tk_x == tk_y) then
         call FieldCOPY(x, y, _rc)
      else if(tk_x == ESMF_TYPEKIND_R4) then
         call convert_prec_R4_to_R8(x, y, _rc)
      else
         call convert_prec_R8_to_R4(x, y, _rc)
      end if

      _return(_success)
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

      call assign_fptr(original, original_ptr, _rc)
      call assign_fptr(converted, converted_ptr, _rc)

      converted_ptr = original_ptr

      _return(_success)
   end subroutine convert_prec_R4_to_R8

   subroutine convert_prec_R8_to_R4(original, converted, rc)
      type(ESMF_Field), intent(inout) :: original
      type(ESMF_Field), intent(inout) :: converted
      integer, optional, intent(out) :: rc
      integer :: status

      real(kind=ESMF_KIND_R8), pointer :: original_ptr(:)
      real(kind=ESMF_KIND_R4), pointer :: converted_ptr(:)
      
      call assign_fptr(original, original_ptr, _rc)
      call assign_fptr(converted, converted_ptr, _rc)

      converted_ptr = original_ptr

      _return(_success)
   end subroutine convert_prec_R8_to_R4

end module mapl_FieldBLAS
