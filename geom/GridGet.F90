#include "MAPL.h"

module mapl3g_GridGet
   use esmf
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   implicit none
   private

   public :: GridGet

   interface GridGet
      procedure :: grid_get
   end interface GridGet

contains

   subroutine grid_get(grid, unusable, &
        dimCount, coordDimCount, &
        im, jm, &
        name, &
        rc)

      type(esmf_Grid), intent(in) :: grid
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: dimCount
      integer, optional, allocatable, intent(out) :: coordDimCount(:)
      integer, optional, intent(out) :: im, jm
      character(:), optional, allocatable :: name
      integer, optional, intent(out) :: rc

      integer :: dimCount_
      character(ESMF_MAXSTR) :: name_
      integer :: status
      real(kind=ESMF_KIND_R8), pointer :: lons(:,:)

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
         call esmf_GridGetCoord(grid, coordDim=1, farrayPtr=lons, _RC)
         if (present(im)) im = size(lons,1)
         if (present(jm)) jm = size(lons,2)
      end if

      _RETURN(_SUCCESS)
   end subroutine Grid_Get

end module mapl3g_GridGet
