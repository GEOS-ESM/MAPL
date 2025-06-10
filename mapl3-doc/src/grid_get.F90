#include "MAPL_Generic.h"

submodule (mapl3g_Geom_API) grid_get_smod

   use mapl_ErrorHandling
   use mapl3g_VectorBasis, only: GridGetCoords

   implicit none

contains

   module subroutine grid_get(grid, unusable, im, jm, latitudes, longitudes, rc)
      use mapl_KeywordEnforcer
      type(ESMF_Grid), intent(in) :: grid
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: im
      integer, optional, intent(out) :: jm
      real(kind=ESMF_KIND_R4), optional, pointer, intent(out) :: latitudes(:,:)
      real(kind=ESMF_KIND_R4), optional, pointer, intent(out) :: longitudes(:,:)
      integer, optional, intent(out) :: rc

      real(kind=ESMF_KIND_R8), pointer :: lats_(:,:), lons_(:,:)
      real(kind=ESMF_KIND_R4), allocatable, target :: lats_r4_(:,:), lons_r4_(:,:)
      integer, allocatable :: shape_(:)
      integer :: status

      call GridGetCoords(grid, longitudes=lons_, latitudes=lats_, _RC)
      shape_ = shape(lons_)

      if (present(im)) im = shape_(1)
      if (present(jm)) jm = shape_(2)
      if (present(longitudes)) then
         lons_r4_ = real(lons_, kind=ESMF_KIND_R4)
         longitudes => lons_r4_
      end if
      if (present(latitudes)) then
         lats_r4_ = real(lats_, kind=ESMF_KIND_R4)
         latitudes => lats_r4_
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine grid_get

end submodule grid_get_smod
