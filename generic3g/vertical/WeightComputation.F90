#include "MAPL_Generic.h"

module mapl3g_WeightComputation

   use mapl_ErrorHandling
   use mapl3g_CSR_SparseMatrix
   ! use esmf
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
      real(REAL32), intent(in) :: matrix(:, :)
      real(REAL32), intent(in) :: fin(:)
      real(REAL32), allocatable, intent(out) :: fout(:)

      fout = matmul(matrix, fin)
   end subroutine apply_linear_map

   ! Compute linear interpolation transformation matrix (src*matrix = dst)
   ! when regridding (vertical) from fixed-levels to fixed-levels
   ! NOTE: find_bracket_ below ASSUMEs that src array is monotonic and decreasing
   subroutine compute_linear_map_fixedlevels_to_fixedlevels(src, dst, matrix, rc)
      real(REAL32), intent(in) :: src(:)
      real(REAL32), intent(in) :: dst(:)
      ! type(CSR_SparseMatrix_sp), intent(out) :: matrix ! size of horz dims
      real(REAL32), allocatable, intent(out) :: matrix(:, :)
      integer, optional, intent(out) :: rc

      real(REAL32) :: val, weight(2)
      integer :: ndx, status
      type(IndexValuePair) :: pair(2) ! [pair(1), pair(2)] is a bracket

      _ASSERT(maxval(dst) <= maxval(src), "maxval(dst) > maxval(src)")
      _ASSERT(minval(dst) >= minval(src), "minval(dst) < minval(src)")

      allocate(matrix(size(dst), size(src)), source=0., _STAT)
      do ndx = 1, size(dst)
         val = dst(ndx)
         call find_bracket_(val, src, pair)
         call compute_linear_interpolation_weights_(val, pair%value_, weight)
         matrix(ndx, pair(1)%index) = weight(1)
         matrix(ndx, pair(2)%index) = weight(2)
      end do

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
