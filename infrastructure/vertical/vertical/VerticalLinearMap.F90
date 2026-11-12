#include "MAPL.h"

module mapl_VerticalLinearMap_mod

   use mapl_ErrorHandling_mod
   use mapl_CSR_SparseMatrix_mod, only: SparseMatrix_sp => CSR_SparseMatrix_sp
   use mapl_CSR_SparseMatrix_mod, only: SparseMatrix_dp => CSR_SparseMatrix_dp
   use mapl_CSR_SparseMatrix_mod, only: add_row
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64

   implicit none(type,external)
   private

   public :: compute_linear_map
   public :: find_bracket
   public :: compute_weights
   public :: IndexValuePair

   type IndexValuePair
      integer :: index
      real(REAL32) :: value_
   end type IndexValuePair

   ! The REAL64 version builds REAL64 weights, so REAL64 data is not
   ! degraded to REAL32 by the weights.
   interface compute_linear_map
      procedure compute_linear_map_sp
      procedure compute_linear_map_dp
   end interface compute_linear_map

   interface operator(==)
      procedure equal_to
   end interface operator(==)

   interface operator(/=)
      procedure not_equal_to
   end interface operator(/=)

contains

   ! Compute linear interpolation transformation matrix,
   ! src*matrix = dst, when regridding (vertical) from src to dst
   ! NOTE: find_bracket_ below handles monotonic src arrays (increasing or decreasing)
   ! allow_out_of_range: dst values outside src's range take the nearest
   ! end value (what release builds already do) instead of failing debug.
   subroutine compute_linear_map_sp(src, dst, matrix, allow_out_of_range, rc)
      real(REAL32), intent(in) :: src(:)
      real(REAL32), intent(in) :: dst(:)
      type(SparseMatrix_sp), intent(out) :: matrix
      ! real(REAL32), allocatable, intent(out) :: matrix(:, :)
      logical, optional, intent(in) :: allow_out_of_range
      integer, optional, intent(out) :: rc

      real(REAL32) :: val, weight(2)
      integer :: ndx
      type(IndexValuePair) :: pair(2)
#ifndef NDEBUG
      logical :: check_range

      check_range = .true.
      if (present(allow_out_of_range)) check_range = .not. allow_out_of_range
      if (check_range) then
         _ASSERT(maxval(dst) <= maxval(src), "maxval(dst) > maxval(src)")
         _ASSERT(minval(dst) >= minval(src), "minval(dst) < minval(src)")
      end if
      ! _ASSERT(is_decreasing(src), "src array is not decreasing")
#else
      _UNUSED_DUMMY(allow_out_of_range)
#endif

      ! allocate(matrix(size(dst), size(src)), source=0., _STAT)
      ! Expected 2 non zero entries in each row
      matrix = SparseMatrix_sp(size(dst), size(src), 2*size(dst))
       do ndx = 1, size(dst)
          val = dst(ndx)
          call find_bracket(val, src, pair)
          if (pair(1)%index == pair(2)%index) then
             ! matrix(ndx, pair(1)%index) = weight(1)
             call add_row(matrix, ndx, pair(1)%index, [1.0_REAL32])
          else
             call compute_weights(val, pair%value_, weight)
             ! matrix(ndx, pair(1)%index) = weight(1)
             ! matrix(ndx, pair(2)%index) = weight(2)
             call add_row(matrix, ndx, pair(1)%index, [weight(1), weight(2)])
          end if
       end do

      _RETURN(_SUCCESS)
   end subroutine compute_linear_map_sp

   ! Same as the REAL32 version, in REAL64.
   subroutine compute_linear_map_dp(src, dst, matrix, allow_out_of_range, rc)
      real(REAL64), intent(in) :: src(:)
      real(REAL64), intent(in) :: dst(:)
      type(SparseMatrix_dp), intent(out) :: matrix
      logical, optional, intent(in) :: allow_out_of_range
      integer, optional, intent(out) :: rc

      real(REAL64) :: weight(2)
      integer :: ndx, bracket(2)
#ifndef NDEBUG
      logical :: check_range

      check_range = .true.
      if (present(allow_out_of_range)) check_range = .not. allow_out_of_range
      if (check_range) then
         _ASSERT(maxval(dst) <= maxval(src), "maxval(dst) > maxval(src)")
         _ASSERT(minval(dst) >= minval(src), "minval(dst) < minval(src)")
      end if
#else
      _UNUSED_DUMMY(allow_out_of_range)
#endif

      matrix = SparseMatrix_dp(size(dst), size(src), 2*size(dst))
      do ndx = 1, size(dst)
         call find_bracket_dp(dst(ndx), src, bracket)
         if (bracket(1) == bracket(2)) then
            call add_row(matrix, ndx, bracket(1), [1.0_REAL64])
         else
            call compute_weights_dp(dst(ndx), src(bracket), weight)
            call add_row(matrix, ndx, bracket(1), weight)
         end if
      end do

      _RETURN(_SUCCESS)
   end subroutine compute_linear_map_dp


   ! Find array bracket [pair_1, pair_2] containing val.
   ! The array must be monotonic, but may be increasing or decreasing.
   ! An exact match or an out-of-range val returns a degenerate bracket
   ! (pair_1 and pair_2 have the same index).
   subroutine find_bracket(val, array, pair)
      real(REAL32), intent(in) :: val
      real(REAL32), intent(in) :: array(:)
      Type(IndexValuePair), intent(out) :: pair(2)

      integer :: ndx1, ndx2, n, i
      logical :: is_increasing

      n = size(array)
      is_increasing = (array(n) > array(1))
      ndx1 = 1
      ndx2 = 1

      if (is_increasing) then
         if (val <= array(1)) then
            ndx1 = 1; ndx2 = 1
         else if (val >= array(n)) then
            ndx1 = n; ndx2 = n
         else
            do i = 1, n-1
               if (val == array(i)) then
                  ndx1 = i; ndx2 = i
                  exit
               else if (array(i) < val .and. val < array(i+1)) then
                  ndx1 = i; ndx2 = i+1
                  exit
               end if
            end do
         end if
      else  ! decreasing
         if (val >= array(1)) then
            ndx1 = 1; ndx2 = 1
         else if (val <= array(n)) then
            ndx1 = n; ndx2 = n
         else
            do i = 1, n-1
               if (val == array(i)) then
                  ndx1 = i; ndx2 = i
                  exit
               else if (array(i) > val .and. val > array(i+1)) then
                  ndx1 = i; ndx2 = i+1
                  exit
               end if
            end do
         end if
      end if

      pair(1) = IndexValuePair(ndx1, array(ndx1))
      pair(2) = IndexValuePair(ndx2, array(ndx2))
   end subroutine find_bracket

   ! Compute linear interpolation weights
   subroutine compute_weights(val, value_, weight)
      real(REAL32), intent(in) :: val
      real(REAL32), intent(in) :: value_(2)
      real(REAL32), intent(out) :: weight(2)

      real(REAL32) :: denominator, epsilon_sp, t

      denominator = value_(2) - value_(1)
      epsilon_sp = epsilon(1.0_REAL32)
      if (abs(denominator) < epsilon_sp) then
         weight(1) = 1.0_REAL32
         weight(2) = 0.0_REAL32
      else
         t = (val - value_(1))/denominator
          weight(2) = max(0.0_REAL32, min(1.0_REAL32, t))
          weight(1) = 1.0_REAL32 - weight(2)
       end if
    end subroutine compute_weights

   ! REAL64 find_bracket: returns the two bracketing indices only.
   subroutine find_bracket_dp(val, array, bracket)
      real(REAL64), intent(in) :: val
      real(REAL64), intent(in) :: array(:)
      integer, intent(out) :: bracket(2)

      integer :: n, i
      logical :: is_increasing

      n = size(array)
      is_increasing = (array(n) > array(1))
      bracket = 1

      if (is_increasing) then
         if (val <= array(1)) then
            bracket = 1
         else if (val >= array(n)) then
            bracket = n
         else
            do i = 1, n-1
               if (val == array(i)) then
                  bracket = i
                  exit
               else if (array(i) < val .and. val < array(i+1)) then
                  bracket = [i, i+1]
                  exit
               end if
            end do
         end if
      else
         if (val >= array(1)) then
            bracket = 1
         else if (val <= array(n)) then
            bracket = n
         else
            do i = 1, n-1
               if (val == array(i)) then
                  bracket = i
                  exit
               else if (array(i) > val .and. val > array(i+1)) then
                  bracket = [i, i+1]
                  exit
               end if
            end do
         end if
      end if
   end subroutine find_bracket_dp

   subroutine compute_weights_dp(val, value_, weight)
      real(REAL64), intent(in) :: val
      real(REAL64), intent(in) :: value_(2)
      real(REAL64), intent(out) :: weight(2)

      real(REAL64) :: denominator, t

      denominator = value_(2) - value_(1)
      if (abs(denominator) < epsilon(1.0_REAL64)) then
         weight(1) = 1.0_REAL64
         weight(2) = 0.0_REAL64
      else
         t = (val - value_(1))/denominator
         weight(2) = max(0.0_REAL64, min(1.0_REAL64, t))
         weight(1) = 1.0_REAL64 - weight(2)
      end if
   end subroutine compute_weights_dp

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

end module mapl_VerticalLinearMap_mod
