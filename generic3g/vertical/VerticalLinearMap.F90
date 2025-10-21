#include "MAPL.h"

module mapl3g_VerticalLinearMap

   use mapl_ErrorHandling
   use mapl3g_CSR_SparseMatrix, only: SparseMatrix_sp => CSR_SparseMatrix_sp
   use mapl3g_CSR_SparseMatrix, only: add_row
   use, intrinsic :: iso_fortran_env, only: REAL32

   implicit none
   private

   public :: compute_linear_map

   type IndexValuePair
      integer :: index
      real(REAL32) :: value_
   end type IndexValuePair

   interface operator(==)
      procedure equal_to
   end interface operator(==)

   interface operator(/=)
      procedure not_equal_to
   end interface operator(/=)

contains

   ! Compute linear interpolation transformation matrix,
   ! src*matrix = dst, when regridding (vertical) from src to dst
   ! NOTE: find_bracket_ below ASSUMEs that src array is monotonic and decreasing
   subroutine compute_linear_map(src, dst, matrix, rc)
      real(REAL32), intent(in) :: src(:)
      real(REAL32), intent(in) :: dst(:)
      type(SparseMatrix_sp), intent(out) :: matrix
      ! real(REAL32), allocatable, intent(out) :: matrix(:, :)
      integer, optional, intent(out) :: rc

      real(REAL32) :: val, weight(2)
      integer :: ndx, status
      type(IndexValuePair) :: pair(2)

#ifndef NDEBUG
      _ASSERT(maxval(dst) <= maxval(src), "maxval(dst) > maxval(src)")
      _ASSERT(minval(dst) >= minval(src), "minval(dst) < minval(src)")
      _ASSERT(is_decreasing(src), "src array is not decreasing")
#endif

      ! allocate(matrix(size(dst), size(src)), source=0., _STAT)
      ! Expected 2 non zero entries in each row
      matrix = SparseMatrix_sp(size(dst), size(src), 2*size(dst))
      do ndx = 1, size(dst)
         val = dst(ndx)
         call find_bracket_(val, src, pair)
         call compute_weights_(val, pair%value_, weight)
         if (pair(1) == pair(2)) then
            ! matrix(ndx, pair(1)%index) = weight(1)
            call add_row(matrix, ndx, pair(1)%index, [weight(1)])
         else
            ! matrix(ndx, pair(1)%index) = weight(1)
            ! matrix(ndx, pair(2)%index) = weight(2)
            call add_row(matrix, ndx, pair(1)%index, [weight(1), weight(2)])
         end if
      end do

      _RETURN(_SUCCESS)
   end subroutine compute_linear_map

   ! Find array bracket [pair_1, pair_2] containing val
   ! ASSUME: array is monotonic and decreasing
   subroutine find_bracket_(val, array, pair)
      real(REAL32), intent(in) :: val
      real(REAL32), intent(in) :: array(:)
      Type(IndexValuePair), intent(out) :: pair(2)

      integer :: ndx1, ndx2

      ndx1 = minloc(abs(array - val), 1)
      if (array(ndx1) < val) then
         ndx1 = ndx1 - 1
      end if
      ndx2 = ndx1 ! array(ndx1) == val
      if (array(ndx1) /= val) then
         ndx2 = ndx1 +1
      end if

      pair(1) = IndexValuePair(ndx1, array(ndx1))
      pair(2) = IndexValuePair(ndx2, array(ndx2))
   end subroutine find_bracket_

   ! Compute linear interpolation weights
   subroutine compute_weights_(val, value_, weight)
      real(REAL32), intent(in) :: val
      real(REAL32), intent(in) :: value_(2)
      real(REAL32), intent(out) :: weight(2)

      real(REAL32) :: denominator, epsilon_sp

      denominator = abs(value_(2) - value_(1))
      epsilon_sp = epsilon(1.0)
      if (denominator < epsilon_sp) then
         weight = 1.0
      else
         weight(1) = abs(value_(2) - val)/denominator
         weight(2) = abs(val - value_(1))/denominator
      end if
   end subroutine compute_weights_

   elemental logical function equal_to(a, b)
      type(IndexValuePair), intent(in) :: a, b
      equal_to = .false.
      equal_to = ((a%index == b%index) .and. (a%value_ == b%value_))
   end function equal_to

   elemental logical function not_equal_to(a, b)
      type(IndexValuePair), intent(in) :: a, b
      not_equal_to = .not. (a==b)
   end function not_equal_to

   logical function is_decreasing(array)
      real(REAL32), intent(in) :: array(:)
      integer :: ndx
      is_decreasing = .true.
      do ndx = 1, size(array)-1
         if (array(ndx) < array(ndx+1)) then
            is_decreasing = .false.
            exit
         end if
      end do
   end function is_decreasing

end module mapl3g_VerticalLinearMap
