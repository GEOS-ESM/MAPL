#include "MAPL_Generic.h"

module mapl3g_WeightComputation

   use mapl_ErrorHandling
   use mapl3g_CSR_SparseMatrix
   ! use esmf
   use, intrinsic :: iso_fortran_env, only: REAL32

   implicit none
   private

   public :: get_weights_fixedlevels_to_fixedlevels_linear

   type Bracket
      integer :: index
      real(REAL32) :: value
   end type Bracket

contains

   ! Compute linear interpolation weights when doing vertical regridding from
   ! fixed-levels vertical grid to fixed-levels vertical grid
   subroutine get_weights_fixedlevels_to_fixedlevels_linear(src, dst, weights, rc)
      real(REAL32), intent(in) :: src(:)
      real(REAL32), intent(in) :: dst(:)
      ! type(CSR_SparseMatrix_sp), intent(out) :: weights ! size of horz dims
      real(REAL32), allocatable, intent(out) :: weights(:, :)
      integer, optional, intent(out) :: rc

      real(REAL32) :: val
      integer :: ndx, status
      type(Bracket) :: bracket_(2)

      _ASSERT(maxval(dst) <= maxval(src), "maxval(dst) > maxval(src)")
      _ASSERT(minval(dst) >= minval(src), "minval(dst) < minval(src)")

      allocate(weights(size(dst), size(src)), source=0., _STAT)
      do ndx = 1, size(dst)
         val = dst(ndx)
         call find_bracket_(val, src, bracket_, rc)
      end do

      _RETURN(_SUCCESS)
   end subroutine get_weights_fixedlevels_to_fixedlevels_linear

   ! Find array bracket containing val
   ! ASSUME: array is monotonic
   subroutine find_bracket_(val, array, bracket_, rc)
      real(REAL32), intent(in) :: val
      real(REAL32), intent(in) :: array(:)
      Type(Bracket), intent(out) :: bracket_(2)
      integer, optional, intent(out) :: rc

      integer :: ndx1, ndx2

      ndx1 = minloc(abs(array - val), 1)
      bracket_(1) = Bracket(ndx1, array(ndx1))
      if (array(ndx1) < val) then
         ndx2 = ndx1 - 1
      else if (array(ndx1) > val) then
         ndx2 = ndx1 + 1
      else
         ndx2 = ndx1
      end if
      bracket_(2) = Bracket(ndx2, array(ndx2))

      _RETURN(_SUCCESS)
   end subroutine find_bracket_

end module mapl3g_WeightComputation
