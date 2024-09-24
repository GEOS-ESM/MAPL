#include "MAPL_Generic.h"

module mapl3g_WeightComputation

   use mapl_ErrorHandling
   use mapl3g_CSR_SparseMatrix, only: SparseMatrix_sp => CSR_SparseMatrix_sp
   use mapl3g_CSR_SparseMatrix, only: add_row
   use mapl3g_CSR_SparseMatrix, only: sparse_matmul_sp => matmul
   use, intrinsic :: iso_fortran_env, only: REAL32

   implicit none
   private

   public :: compute_linear_map_fixedlevels_to_fixedlevels
   public :: apply_linear_map

   type IndexValuePair
      integer :: index
      real(REAL32) :: value_
   end type IndexValuePair

contains

   subroutine apply_linear_map(matrix, fin, fout)
      ! real(REAL32), intent(in) :: matrix(:, :)
      type(SparseMatrix_sp) :: matrix
      real(REAL32), intent(in) :: fin(:)
      real(REAL32), allocatable, intent(out) :: fout(:)

      fout = sparse_matmul_sp(matrix, fin)
   end subroutine apply_linear_map

   ! Compute linear interpolation transformation matrix (src*matrix = dst)
   ! when regridding (vertical) from fixed-levels to fixed-levels
   ! NOTE: find_bracket_ below ASSUMEs that src array is monotonic and decreasing
   subroutine compute_linear_map_fixedlevels_to_fixedlevels(src, dst, matrix, rc)
      real(REAL32), intent(in) :: src(:)
      real(REAL32), intent(in) :: dst(:)
      type(SparseMatrix_sp), intent(out) :: matrix
      ! real(REAL32), allocatable, intent(out) :: matrix(:, :)
      integer, optional, intent(out) :: rc

      real(REAL32) :: val, weight(2)
      integer :: ndx, status
      type(IndexValuePair) :: pair(2) ! [pair(1), pair(2)] is a bracket

      _ASSERT(maxval(dst) <= maxval(src), "maxval(dst) > maxval(src)")
      _ASSERT(minval(dst) >= minval(src), "minval(dst) < minval(src)")

      ! Expected 2 non zero entries in each row
      ! allocate(matrix(size(dst), size(src)), source=0., _STAT)
      matrix = SparseMatrix_sp(size(dst), size(src), 2*size(dst))
      do ndx = 1, size(dst)
         val = dst(ndx)
         call find_bracket_(val, src, pair)
         call compute_linear_interpolation_weights_(val, pair%value_, weight)
         ! matrix(ndx, pair(1)%index) = weight(1)
         ! matrix(ndx, pair(2)%index) = weight(2)
         if (pair(1)%index < pair(2)%index) then
            call add_row(matrix, ndx, pair(1)%index, [weight(1), weight(2)])
         else if (pair(1)%index > pair(2)%index) then
            call add_row(matrix, ndx, pair(2)%index, [weight(2), weight(1)])
         else
            call add_row(matrix, ndx, pair(1)%index, [weight(1)])
         end if
      end do
      ! print *, matrix

      _RETURN(_SUCCESS)
   end subroutine compute_linear_map_fixedlevels_to_fixedlevels

   ! Find array bracket containing val
   ! ASSUME: array is monotonic and decreasing
   subroutine find_bracket_(val, array, pair)
      real(REAL32), intent(in) :: val
      real(REAL32), intent(in) :: array(:)
      Type(IndexValuePair), intent(out) :: pair(2)

      integer :: ndx1, ndx2

      ndx1 = minloc(abs(array - val), 1)
      pair(1) = IndexValuePair(ndx1, array(ndx1))
      if (array(ndx1) < val) then
         ndx2 = ndx1 - 1
      else if (array(ndx1) > val) then
         ndx2 = ndx1 + 1
      else
         ndx2 = ndx1
      end if
      pair(2) = IndexValuePair(ndx2, array(ndx2))
   end subroutine find_bracket_

   subroutine compute_linear_interpolation_weights_(val, value_, weight)
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
   end subroutine compute_linear_interpolation_weights_

end module mapl3g_WeightComputation
