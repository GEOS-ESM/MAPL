#include "macros.h"

   subroutine COMPUTE_LINEAR_MAP_ (src, dst, matrix, rc)
      real(KIND_), intent(in) :: src(:)
      real(KIND_), intent(in) :: dst(:)
      type(SPARSE_MATRIX_), intent(out) :: matrix
      ! real(KIND_), allocatable, intent(out) :: matrix(:, :)
      integer, optional, intent(out) :: rc

      real(KIND_) :: val, weight(2)
      integer :: ndx, status
      type(INDEX_VALUE_PAIR_) :: pair(2)

#ifndef NDEBUG
      _ASSERT(maxval(dst) <= maxval(src), "maxval(dst) > maxval(src)")
      _ASSERT(minval(dst) >= minval(src), "minval(dst) < minval(src)")
      _ASSERT(is_decreasing(src), "src array is not decreasing")
#endif

      ! allocate(matrix(size(dst), size(src)), source=0., _STAT)
      ! Expected 2 non zero entries in each row
      matrix = SPARSE_MATRIX_(size(dst), size(src), 2*size(dst))
      do ndx = 1, size(dst)
         val = dst(ndx)
         call find_bracket(val, src, pair)
         call compute_weights(val, pair%value_, weight)
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

   contains

!===============================================================================
! Find array bracket [pair_1, pair_2] containing val
! ASSUME: array is monotonic and decreasing

      subroutine find_bracket(val, array, pair)
         real(KIND_), intent(in) :: val
         real(KIND_), intent(in) :: array(:)
         type(INDEX_VALUE_PAIR_), intent(out) :: pair(2)

         integer :: ndx1, ndx2

         ndx1 = minloc(abs(array - val), 1)
         if (array(ndx1) < val) then
            ndx1 = ndx1 - 1
         end if
         ndx2 = ndx1 ! array(ndx1) == val
         if (array(ndx1) /= val) then
            ndx2 = ndx1 +1
         end if

         pair(1) = INDEX_VALUE_PAIR_(ndx1, array(ndx1))
         pair(2) = INDEX_VALUE_PAIR_(ndx2, array(ndx2))
      end subroutine find_bracket

!===============================================================================
! Compute linear interpolation weights

      subroutine compute_weights(val, value_, weight)
         real(KIND_), intent(in) :: val
         real(KIND_), intent(in) :: value_(2)
         real(KIND_), intent(out) :: weight(2)

         real(KIND_) :: denominator, epsilon_

         denominator = abs(value_(2) - value_(1))
         epsilon_ = epsilon(1.0)
         if (denominator < epsilon_) then
            weight = 1.0
         else
            weight(1) = abs(value_(2) - val)/denominator
            weight(2) = abs(val - value_(1))/denominator
         end if
      end subroutine compute_weights
      
      logical function is_decreasing(array) result(decreasing)
         real(KIND_), intent(in) :: array(:)
         integer :: ndx
         decreasing = .true.
         do ndx = 1, size(array)-1
            if (array(ndx) < array(ndx+1)) then
               decreasing = .false.
               exit
            end if
         end do
      end function is_decreasing

   end subroutine COMPUTE_LINEAR_MAP_

   elemental logical function EQUAL_TO_(a, b) result(equal_to)
      type(INDEX_VALUE_PAIR_), intent(in) :: a, b
      equal_to = .false.
      equal_to = ((a%index == b%index) .and. (a%value_ == b%value_))
   end function EQUAL_TO_

   elemental logical function NOT_EQUAL_TO_(a, b) result(not_equal_to)
      type(INDEX_VALUE_PAIR_), intent(in) :: a, b
      not_equal_to = .not. (a==b)
   end function NOT_EQUAL_TO_

! vim: ft=fortran
