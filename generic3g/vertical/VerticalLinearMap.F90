#include "MAPL.h"

module mapl3g_VerticalLinearMap

   use mapl_ErrorHandling
   use mapl3g_CSR_SparseMatrix, only: SparseMatrix_sp => CSR_SparseMatrix_sp
   use mapl3g_CSR_SparseMatrix, only: SparseMatrix_dp => CSR_SparseMatrix_dp
   use mapl3g_CSR_SparseMatrix, only: add_row
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64

   implicit none
   private

   public :: compute_linear_map

   type :: IndexValuePairR32
      integer :: index = 0
      real(kind=REAL32) :: value_ = 0
   end type IndexValuePairR32

   type :: IndexValuePairR64
      integer :: index = 0
      real(kind=REAL64) :: value_ = 0
   end type IndexValuePairR64

   interface operator(==)
      procedure equal_to_r32
      procedure equal_to_r64
   end interface operator(==)

   interface operator(/=)
      procedure not_equal_to_r32
      procedure not_equal_to_r64
   end interface operator(/=)

   interface compute_linear_map
      !module procedure :: compute_linear_map_REAL32
      !module procedure :: compute_linear_map_REAL64
      module procedure :: compute_linear_map_R32
      module procedure :: compute_linear_map_R64
   end interface compute_linear_map

!   interface find_bracket_
!      module procedure :: find_bracket_R32
!      module procedure :: find_bracket_R64
!   end interface find_bracket_
!
!   interface compute_weights_
!      module procedure :: compute_weights_R32
!      module procedure :: compute_weights_R64
!   end interface compute_weights_
!
!   interface is_decreasing
!      module procedure :: is_decreasing_R32
!      module procedure :: is_decreasing_R64
!   end interface is_decreasing

contains

#define KIND_ REAL32
#define SUB_ compute_linear_map_R32
#include "VerticalLinearMapProcedure.h"
#undef SUB_
#undef KIND_

#define KIND_ REAL64
#define SUB_ compute_linear_map_R64
#include "VerticalLinearMapProcedure.h"
#undef SUB_
#undef KIND_

   elemental logical function equal_to_r32(a, b) result(equal_to)
      type(IndexValuePairR32), intent(in) :: a, b
      equal_to = .false.
      equal_to = ((a%index == b%index) .and. (a%value_ == b%value_))
   end function equal_to_r32

   elemental logical function not_equal_to_r32(a, b) result(not_equal_to)
      type(IndexValuePairR32), intent(in) :: a, b
      not_equal_to = .not. (a==b)
   end function not_equal_to_r32

   elemental logical function equal_to_r64(a, b) result(equal_to)
      type(IndexValuePairR64), intent(in) :: a, b
      equal_to = .false.
      equal_to = ((a%index == b%index) .and. (a%value_ == b%value_))
   end function equal_to_r64

   elemental logical function not_equal_to_r64(a, b) result(not_equal_to)
      type(IndexValuePairR64), intent(in) :: a, b
      not_equal_to = .not. (a==b)
   end function not_equal_to_r64

!===============================================================================
! Compute linear interpolation transformation matrix,
! src*matrix = dst, when regridding (vertical) from src to dst
! NOTE: find_bracket_ below ASSUMEs that src array is monotonic and decreasing

   subroutine compute_linear_map_REAL32 (src, dst, matrix, rc)
      real(REAL32), intent(in) :: src(:)
      real(REAL32), intent(in) :: dst(:)
      type(SparseMatrix_sp), intent(out) :: matrix
      ! real(REAL32), allocatable, intent(out) :: matrix(:, :)
      integer, optional, intent(out) :: rc

      real(REAL32) :: val, weight(2)
      integer :: ndx, status
      type(IndexValuePairR32) :: pair(2)

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

   end subroutine compute_linear_map_REAL32

   subroutine compute_linear_map_REAL64 (src, dst, matrix, rc)
      real(REAL64), intent(in) :: src(:)
      real(REAL64), intent(in) :: dst(:)
      type(SparseMatrix_dp), intent(out) :: matrix
      ! real(REAL64), allocatable, intent(out) :: matrix(:, :)
      integer, optional, intent(out) :: rc

      real(REAL64) :: val, weight(2)
      integer :: ndx, status
      type(IndexValuePairR64) :: pair(2)

#ifndef NDEBUG
      _ASSERT(maxval(dst) <= maxval(src), "maxval(dst) > maxval(src)")
      _ASSERT(minval(dst) >= minval(src), "minval(dst) < minval(src)")
      _ASSERT(is_decreasing(src), "src array is not decreasing")
#endif

      ! allocate(matrix(size(dst), size(src)), source=0., _STAT)
      ! Expected 2 non zero entries in each row
      matrix = SparseMatrix_dp(size(dst), size(src), 2*size(dst))
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

   end subroutine compute_linear_map_REAL64

!===============================================================================
! Find array bracket [pair_1, pair_2] containing val
! ASSUME: array is monotonic and decreasing

   subroutine find_bracket_R32(val, array, pair)
      real(REAL32), intent(in) :: val
      real(REAL32), intent(in) :: array(:)
      type(IndexValuePairR32), intent(out) :: pair(2)

      integer :: ndx1, ndx2

      ndx1 = minloc(abs(array - val), 1)
      if (array(ndx1) < val) then
         ndx1 = ndx1 - 1
      end if
      ndx2 = ndx1 ! array(ndx1) == val
      if (array(ndx1) /= val) then
         ndx2 = ndx1 +1
      end if

      pair(1) = IndexValuePairR32(ndx1, array(ndx1))
      pair(2) = IndexValuePairR32(ndx2, array(ndx2))
   end subroutine find_bracket_R32

   subroutine find_bracket_R64(val, array, pair)
      real(REAL64), intent(in) :: val
      real(REAL64), intent(in) :: array(:)
      type(IndexValuePairR64), intent(out) :: pair(2)

      integer :: ndx1, ndx2

      ndx1 = minloc(abs(array - val), 1)
      if (array(ndx1) < val) then
         ndx1 = ndx1 - 1
      end if
      ndx2 = ndx1 ! array(ndx1) == val
      if (array(ndx1) /= val) then
         ndx2 = ndx1 +1
      end if

      pair(1) = IndexValuePairR64(ndx1, array(ndx1))
      pair(2) = IndexValuePairR64(ndx2, array(ndx2))
   end subroutine find_bracket_R64

!===============================================================================
! Compute linear interpolation weights

   subroutine compute_weights_R32(val, value_, weight)
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
   end subroutine compute_weights_R32

   subroutine compute_weights_R64(val, value_, weight)
      real(REAL64), intent(in) :: val
      real(REAL64), intent(in) :: value_(2)
      real(REAL64), intent(out) :: weight(2)

      real(REAL64) :: denominator, epsilon_sp

      denominator = abs(value_(2) - value_(1))
      epsilon_sp = epsilon(1.0)
      if (denominator < epsilon_sp) then
         weight = 1.0
      else
         weight(1) = abs(value_(2) - val)/denominator
         weight(2) = abs(val - value_(1))/denominator
      end if
   end subroutine compute_weights_R64

   logical function is_decreasing_R32(array) result(decreasing)
      real(REAL32), intent(in) :: array(:)
      integer :: ndx
      decreasing = .true.
      do ndx = 1, size(array)-1
         if (array(ndx) < array(ndx+1)) then
            decreasing = .false.
            exit
         end if
      end do
   end function is_decreasing_R32

   logical function is_decreasing_R64(array) result(decreasing)
      real(REAL64), intent(in) :: array(:)
      integer :: ndx
      decreasing = .true.
      do ndx = 1, size(array)-1
         if (array(ndx) < array(ndx+1)) then
            decreasing = .false.
            exit
         end if
      end do
   end function is_decreasing_R64

end module mapl3g_VerticalLinearMap
