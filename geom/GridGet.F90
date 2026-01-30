#include "MAPL.h"

module mapl3g_GridGet

   use esmf
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling

   implicit none
   private

   public :: GridGet
   public :: GridGetCoordinates

   interface GridGet
      procedure :: grid_get
   end interface GridGet

   interface GridGetCoordinates
      procedure :: grid_get_coordinates_r4
      procedure :: grid_get_coordinates_r8
      procedure :: grid_get_coordinates_r8ptr
   end interface GridGetCoordinates

contains

   subroutine grid_get(grid, unusable, name, dimCount, coordDimCount, &
                       im, jm, globalCellCountPerDim, rc)
      type(esmf_Grid), intent(in) :: grid
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(:), optional, allocatable, intent(out) :: name
      integer, optional, intent(out) :: dimCount
      integer, optional, allocatable, intent(out) :: coordDimCount(:)
      integer, optional, intent(out) :: im, jm
      integer, optional, allocatable, intent(out) :: globalCellCountPerDim(:)
      integer, optional, intent(out) :: rc

      integer :: dimCount_
      character(ESMF_MAXSTR) :: name_
      integer :: status
      real(kind=ESMF_KIND_R8), pointer :: coords(:,:)
      logical :: has_de
      logical :: isCubedSphere, isRegular
      integer, allocatable :: globalIndexBounds(:,:)
      integer, allocatable :: maxIndexPTile(:,:)
      integer :: tileCount

      call esmf_GridGet(grid, dimCount=dimCount_, _RC)
      if (present(dimCount)) then
         dimCount = dimCount_
      end if

      if (present(coordDimCount)) then
         allocate(coordDimCount(dimCount_))
         call esmf_GridGet(grid, coordDimCount=coordDimCount, _RC)
      end if

      if (present(name)) then
         call esmf_GridGet(grid, name=name_, _RC)
         name = trim(name_)
      end if

      if (present(im) .or. present(jm)) then
         call esmf_GridGetCoord(grid, coordDim=1, farrayPtr=coords, _RC)
         if (present(im)) im = size(coords,1)
         if (present(jm)) jm = size(coords,2)
      end if

      ! Getting global cell counts
      if (present(globalCellCountPerDim)) then
         call get_globalCellCountPerDim(grid, globalCellCountPerDim, _RC)
      end if
      
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine grid_get
   
   !subroutine check_grid_type(grid, unusable, globalCoordDimCount)
   subroutine get_globalCellCountPerDim(grid, globalCellCountPerDim, rc)
      type(ESMF_Grid), intent(in) :: grid
      !class(KeywordEnforcer), optional, intent(in) :: unusable
      !integer, allocatable, optional, intent(inout) :: globalCoordDimCount(:)
      integer, allocatable, intent(inout) :: globalCellCountPerDim(:)
      integer, optional, intent(out) :: rc
      
      integer :: tileCount, dimCount
      integer, allocatable :: minIndexPTile(:,:), maxIndexPTile(:,:)
      integer, allocatable :: minCounts(:), maxCounts(:)
      integer, allocatable :: globalIndexBounds(:,:)
      character(len=ESMF_MAXSTR) :: error_message
      integer :: status, iTile
      
      ! Get grid information
      !if (present(globalCoordDimCount)) then
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
       
      !end if
       
      _RETURN(_SUCCESS)
      !_UNUSED_DUMMY(unusable)
       
   end subroutine get_globalCellCountPerDim

   logical function grid_has_DE(grid,rc) result(has_DE)
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(out), optional :: rc
      
      integer :: status
      type(ESMF_DistGrid) :: distGrid
      type(ESMF_DeLayout) :: layout
      integer :: localDECount
      
      call ESMF_GridGet    (GRID, distGrid=distGrid, _RC)
      call ESMF_DistGridGet(distGRID, delayout=layout, _RC)
      call ESMF_DELayoutGet(layout, localDeCount=localDeCount,_RC)
      has_DE = (localDECount /=0)
      
      _RETURN(_SUCCESS)
   end function grid_has_DE

   subroutine grid_get_coordinates_r4(grid, longitudes, latitudes, rc)
      type(esmf_Grid), intent(in) :: grid
      real(ESMF_KIND_R4), allocatable, intent(out) :: longitudes(:,:)
      real(ESMF_KIND_R4), allocatable, intent(out) :: latitudes(:,:)
      integer, optional, intent(out) :: rc

      integer :: status
      real(ESMF_KIND_R8), pointer :: ptr(:,:)

      call esmf_GridGetCoord(grid, coordDim=1, farrayPtr=ptr, _RC)
      longitudes = ptr

      call esmf_GridGetCoord(grid, coordDim=2, farrayPtr=ptr, _RC)
      latitudes = ptr

      _RETURN(_SUCCESS)
   end subroutine grid_get_coordinates_r4

   subroutine grid_get_coordinates_r8(grid, longitudes, latitudes, rc)
      type(esmf_Grid), intent(in) :: grid
      real(ESMF_KIND_R8), allocatable, intent(out) :: longitudes(:,:)
      real(ESMF_KIND_R8), allocatable, intent(out) :: latitudes(:,:)
      integer, optional, intent(out) :: rc

      integer :: status
      real(ESMF_KIND_R8), pointer :: ptr(:,:)

      call esmf_GridGetCoord(grid, coordDim=1, farrayPtr=ptr, _RC)
      longitudes = ptr

      call esmf_GridGetCoord(grid, coordDim=2, farrayPtr=ptr, _RC)
      latitudes = ptr

      _RETURN(_SUCCESS)
   end subroutine grid_get_coordinates_r8

   subroutine grid_get_coordinates_r8ptr(grid, longitudes, latitudes, rc)
      type(esmf_Grid), intent(in) :: grid
      real(ESMF_KIND_R8), pointer, intent(out) :: longitudes(:,:)
      real(ESMF_KIND_R8), pointer, intent(out) :: latitudes(:,:)
      integer, optional, intent(out) :: rc

      integer :: status

      call esmf_GridGetCoord(grid, coordDim=1, farrayPtr=longitudes, _RC)
      call esmf_GridGetCoord(grid, coordDim=2, farrayPtr=latitudes, _RC)

      _RETURN(_SUCCESS)
   end subroutine grid_get_coordinates_r8ptr

end module mapl3g_GridGet
