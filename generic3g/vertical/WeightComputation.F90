#include "MAPL_Generic.h"

module mapl3g_WeightComputation

   use mapl_ErrorHandling
   use mapl3g_CSR_SparseMatrix
   ! use esmf
   use, intrinsic :: iso_fortran_env, only: REAL32

   implicit none
   private

   public :: get_weights_fixedlevels_to_fixedlevels_linear

   type Pair
      integer :: index
      real(REAL32) :: value_
   end type Pair

contains

   ! Compute linear interpolation weights when doing vertical regridding from
   ! fixed-levels vertical grid to fixed-levels vertical grid
   subroutine get_weights_fixedlevels_to_fixedlevels_linear(src, dst, weight, rc)
      real(REAL32), intent(in) :: src(:)
      real(REAL32), intent(in) :: dst(:)
      ! type(CSR_SparseMatrix_sp), intent(out) :: weight ! size of horz dims
      real(REAL32), allocatable, intent(out) :: weight(:, :)
      integer, optional, intent(out) :: rc

      real(REAL32) :: val, weight_(2)
      integer :: ndx, status
      type(Pair) :: pair_(2)

      _ASSERT(maxval(dst) <= maxval(src), "maxval(dst) > maxval(src)")
      _ASSERT(minval(dst) >= minval(src), "minval(dst) < minval(src)")

      allocate(weight(size(dst), size(src)), source=0., _STAT)
      do ndx = 1, size(dst)
         val = dst(ndx)
         call find_bracket_(val, src, pair_)
         call compute_linear_interpolation_weights_(val, pair_%value_, weight_)
         weight(ndx, pair_(1)%index) = weight_(1)
         weight(ndx, pair_(2)%index) = weight_(2)
      end do

      _RETURN(_SUCCESS)
   end subroutine get_weights_fixedlevels_to_fixedlevels_linear

   ! Find array bracket containing val
   ! ASSUME: array is monotonic
   subroutine find_bracket_(val, array, pair_)
      real(REAL32), intent(in) :: val
      real(REAL32), intent(in) :: array(:)
      Type(Pair), intent(out) :: pair_(2)

      integer :: ndx1, ndx2

      ndx1 = minloc(abs(array - val), 1)
      pair_(1) = Pair(ndx1, array(ndx1))
      if (array(ndx1) < val) then
         ndx2 = ndx1 - 1
      else if (array(ndx1) > val) then
         ndx2 = ndx1 + 1
      else
         ndx2 = ndx1
      end if
      pair_(2) = Pair(ndx2, array(ndx2))
   end subroutine find_bracket_

   subroutine compute_linear_interpolation_weights_(val, value_, weight_)
      real(REAL32), intent(in) :: val
      real(REAL32), intent(in) :: value_(2)
      real(REAL32), intent(out) :: weight_(2)

      real(REAL32) :: denominator, epsilon_sp

      denominator = abs(value_(2) - value_(1))
      epsilon_sp = epsilon(1.0)
      if (denominator < epsilon_sp) then
         weight_ = 1.0
      else
         weight_(1) = abs(value_(2) - val)/denominator
         weight_(2) = abs(val - value_(1))/denominator
      end if
   end subroutine compute_linear_interpolation_weights_

end module mapl3g_WeightComputation
