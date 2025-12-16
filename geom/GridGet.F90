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
   end interface GridGetCoordinates

contains

   subroutine grid_get(grid, unusable, &
        name, &
        dimCount, coordDimCount, &
        im, jm, &
        longitudes, latitudes, &
        rc)

      type(esmf_Grid), intent(in) :: grid
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(:), optional, allocatable, intent(out) :: name
      integer, optional, intent(out) :: dimCount
      integer, optional, allocatable, intent(out) :: coordDimCount(:)
      integer, optional, intent(out) :: im, jm
      real(kind=ESMF_KIND_R4), optional, allocatable, intent(out) :: longitudes(:,:)
      real(kind=ESMF_KIND_R4), optional, allocatable, intent(out) :: latitudes(:,:)
      integer, optional, intent(out) :: rc

      integer :: dimCount_
      character(ESMF_MAXSTR) :: name_
      integer :: status
      real(kind=ESMF_KIND_R8), pointer :: coords(:,:)
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

      if (present(im) .or. present(jm)) then
         call esmf_GridGetCoord(grid, coordDim=1, farrayPtr=coords, _RC)
         if (present(im)) im = size(coords,1)
         if (present(jm)) jm = size(coords,2)
      end if

      if (present(longitudes) .or. present(latitudes)) then
         call GridGetCoordinates(grid, longitudes=longitudes, latitudes=latitudes, _RC)
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine grid_get
   
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

end module mapl3g_GridGet
