#include "MAPL.h"

module mapl3g_GridGetGlobal

   use esmf
   use mapl_ErrorHandling

   implicit none(type, external)
   private

   public :: GridGetGlobalCellCountPerDim

   interface GridGetGlobalCellCountPerDim
      procedure :: grid_get_global_cell_count_per_dim
   end interface GridGetGlobalCellCountPerDim

contains

   subroutine grid_get_global_cell_count_per_dim(grid, globalCellCountPerDim, rc)
      type(ESMF_Grid), intent(in) :: grid
      integer, allocatable, intent(inout) :: globalCellCountPerDim(:)
      integer, optional, intent(out) :: rc

      integer :: tileCount, dimCount
      integer, allocatable :: mincounts(:), maxcounts(:)
      integer :: status

      call ESMF_GridGet(grid, tileCount=tileCount, dimCount=dimCount, _RC)
      allocate(globalCellCountPerDim(dimCount))
      allocate(mincounts(dimCount), maxcounts(dimCount))

      call ESMF_GridGet(grid, tile=1, staggerLoc=ESMF_STAGGERLOC_CENTER, &
           minIndex=mincounts, &
           maxIndex=maxcounts, _RC)

      globalCellCountPerDim = maxcounts - mincounts + 1

      ! For cubed-sphere, multiply j-dimension by 6 (matching MAPL_GridGet behavior)
      if (tileCount == 6) then
         if (globalCellCountPerDim(1) /= 1) then
            globalCellCountPerDim(2) = globalCellCountPerDim(2) * 6
         end if
      end if

      deallocate(mincounts, maxcounts)
      _RETURN(_SUCCESS)
   end subroutine grid_get_global_cell_count_per_dim

end module mapl3g_GridGetGlobal
