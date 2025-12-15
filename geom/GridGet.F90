#include "MAPL.h"

module mapl3g_GridGet
   use esmf
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   implicit none
   private

   public :: GridGet
   public :: GridGetCoords

   interface GridGet
      procedure :: grid_get
   end interface GridGet

contains

   subroutine grid_get(grid, unusable, &
        dimCount, coordDimCount, &
        im, jm, &
        longitudes, latitudes, &
        name, &
        globalCellCountPerDim, localCellCountPerDim, &
        rc)

      type(esmf_Grid), intent(in) :: grid
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: dimCount
      integer, optional, allocatable, intent(out) :: coordDimCount(:)
      integer, optional, intent(out) :: im, jm
      real(ESMF_KIND_R8), optional, pointer :: longitudes(:,:)
      real(ESMF_KIND_R8), optional, pointer :: latitudes(:,:)
      character(:), optional, allocatable :: name
      integer, optional, intent(inout) :: globalCellCountPerDim(:)
      integer, optional, intent(inout) :: localCellCountPerDim(:)
      integer, optional, intent(out) :: rc

      integer :: dimCount_
      character(ESMF_MAXSTR) :: name_
      integer :: status
      real(kind=ESMF_KIND_R8), pointer :: lons(:,:)
      logical :: has_de

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

      if (present(longitudes)) then
         call esmf_GridGetCoord(grid, coordDim=1, farrayPtr=longitudes, _RC)
      end if

      if (present(latitudes)) then
         call esmf_GridGetCoord(grid, coordDim=2, farrayPtr=latitudes, _RC)
      end if

      if (present(im) .or. present(jm)) then
         call esmf_GridGetCoord(grid, coordDim=1, farrayPtr=lons, _RC)
         if (present(im)) im = size(lons,1)
         if (present(jm)) jm = size(lons,2)
      end if

      if (present(localCellCountPerDim)) then
         _HERE, 'deprecated'
         localCellCountPerDim = 1 ! unless
         has_DE = grid_has_de(grid, _RC)
         if (has_DE) then
            call esmf_GridGet(grid, localDE=0, &
                 staggerloc=ESMF_STAGGERLOC_CENTER, &
                 exclusiveCount=localCellCountPerDim, _RC)
         end if
      end if
 
      if (present(globalCellCountPerDim)) then
         _HERE, 'deprecated'
         globalCellCountPerDim = 1
         
      end if

      _RETURN(_SUCCESS)
   end subroutine grid_get
   
   logical function grid_has_DE(grid,rc) result(has_DE)
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(out), optional :: rc
      
      integer :: status
      type(ESMF_DistGrid) :: distGrid
      type(ESMF_DeLayout) :: layout
      integer :: localDECount
      logical :: hasDE
      
      call ESMF_GridGet    (GRID, distGrid=distGrid, _RC)
      call ESMF_DistGridGet(distGRID, delayout=layout, _RC)
      call ESMF_DELayoutGet(layout, localDeCount=localDeCount,_RC)
      has_DE = (localDECount /=0)
      
      _RETURN(_SUCCESS)
   end function grid_has_DE

end module mapl3g_GridGet
