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
      integer, allocatable :: minIndexPTile(:,:), maxIndexPTile(:,:)
      integer, allocatable :: minCounts(:), maxCounts(:)
      integer, allocatable :: globalIndexBounds(:,:)
      character(len=ESMF_MAXSTR) :: error_message
      integer :: status, iTile

      ! Get grid information
      call ESMF_GridGet(grid, tileCount=tileCount, dimCount=dimCount, _RC)
      allocate(globalCellCountPerDim(dimCount))

      ! Check grid type based on tile count
      if (tileCount == 6) then
         ! Likely cubed-sphere
         ! Additional verification: check if all tiles are square and equal
         allocate(minIndexPTile(dimCount, tileCount), maxIndexPTile(dimCount, tileCount))
         do iTile = 1,6
            call ESMF_GridGet(grid, tile=iTile, &
                 staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER, &
                 minIndex=minIndexPTile(:,iTile),  &
                 maxIndex=maxIndexPTile(:,iTile), _RC)
         end do
         ! Verify all tiles have same dimensions and are square
         if (all(maxIndexPTile(1,:) == maxIndexPTile(1,1)) .and. &
             all(maxIndexPTile(2,:) == maxIndexPTile(2,1)) .and. &
             maxIndexPTile(1,1) == maxIndexPTile(2,1)     .and. &
             all(minIndexPTile(1,:) == minIndexPTile(1,1))  .and. &
             all(minIndexPTile(2,:) == minIndexPTile(2,1))  .and. &
             minIndexPTile(1,1) == minIndexPTile(2,1)) then
             globalCellCountPerDim(1) = maxIndexPTile(1,1) - minIndexPtile(1,1)
             globalCellCountPerDim(2) = globalCellCountPerDim(1)*6
             deallocate(minIndexPTile, maxIndexPTile)
             print *, "Confirmed cubed-sphere: C", maxIndexPTile(1,1)
         else
             error stop "6-tile grid but not standard cubed-sphere"
         end if
         deallocate(minIndexPTile)
         deallocate(maxIndexPTile)
      else if (tileCount == 1) then
         ! Regular lat-lon or regional grid
         allocate(minCounts(dimCount), maxCounts(dimCount))
         call ESMF_GridGet(grid, tile=1, staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER, &
                 minIndex=minCounts,  &
                 maxIndex=maxCounts, _RC)
         globalCellCountPerDim =  maxCounts - minCounts
         deallocate(minCounts, maxCounts)
         print *, "Regular (single-tile) grid"
      else
         write(error_message, '(A,i0,A)') "Non-standard grid with ", tileCount, " tiles"
         error stop trim(error_message)
      end if

      _RETURN(_SUCCESS)
   end subroutine grid_get_global_cell_count_per_dim

end module mapl3g_GridGetGlobal
