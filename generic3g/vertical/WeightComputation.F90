#include "MAPL_Generic.h"

module mapl3g_WeightComputation

   use mapl_ErrorHandling
   use mapl3g_VerticalRegridMethod
   use mapl3g_CSR_SparseMatrix
   use esmf
   use, intrinsic :: iso_fortran_env, only: REAL32

   implicit none
   private

   public :: get_weights_fixedlevels_subset

contains

   subroutine get_weights_fixedlevels_subset(src_v_coord, dst_v_coord, regrid_method, weights, rc)
      real(REAL32), intent(in) :: src_v_coord(:)
      real(REAL32), intent(in) :: dst_v_coord(:)
      type(VerticalRegridMethod_Flag) :: regrid_method
      ! type(CSR_SparseMatrix_sp), intent(out) :: weights ! size of horz dims
      real(REAL32), allocatable, intent(out) :: weights(:, :)
      integer, optional, intent(out) :: rc

      integer :: ndx_dst, ndx_src, status

      _ASSERT(regrid_method == VERTICAL_REGRID_SUBSET, "wrong regrid_method passed")
      _ASSERT(size(dst_v_coord) < size(src_v_coord), "not subsetting")

      allocate(weights(size(dst_v_coord), size(src_v_coord)), source=0., _STAT)
      do ndx_dst = 1, size(dst_v_coord)
         ndx_src = findloc(src_v_coord, dst_v_coord(ndx_dst), 1)
         _ASSERT(ndx_src /= 0, "dst coord not in src coord")
         weights(ndx_dst, ndx_src) = 1.
      end do

      _RETURN(_SUCCESS)
   end subroutine get_weights_fixedlevels_subset

end module mapl3g_WeightComputation
