#include "MAPL_ErrLog.h"

submodule (mapl3g_VectorBasis) MAPL_GeomGetCoords_smod
contains


   module subroutine MAPL_GeomGetCoords(geom, longitudes, latitudes, rc)
      type(ESMF_Geom), intent(in) :: geom
      real(kind=ESMF_KIND_R8), pointer :: longitudes(:)
      real(kind=ESMF_KIND_R8), pointer :: latitudes(:)
      integer, optional, intent(out) :: rc

      type(ESMF_GeomType_Flag) :: geomtype
      type(ESMF_Grid) :: grid
      type(ESMF_LocStream) :: locstream
      integer :: status

      call ESMF_GeomGet(geom, geomtype=geomtype, _RC)
      if (geomtype == ESMF_GEOMTYPE_GRID) then
         call ESMF_GeomGet(geom, grid=grid, _RC)
         call GridGetCoords(grid, longitudes, latitudes, _RC)
      else if (geomtype == ESMF_GEOMTYPE_LOCSTREAM) then
         call ESMF_GeomGet(geom, locstream=locstream, _RC)
         call get_locstream_coords(locstream, longitudes, latitudes, _RC)
      else if (any([geomtype==ESMF_GEOMTYPE_MESH, geomtype==ESMF_GEOMTYPE_XGRID])) then
         _FAIL("Unsupported geom type.")
      else
         _FAIL("Illeggal geom type.")
      end if
      _RETURN(ESMF_SUCCESS)

   contains

      subroutine get_locstream_coords(locstream, longitudes, latitudes, rc)
         type(ESMF_LocStream), intent(in) :: locstream
         real(kind=ESMF_KIND_R8), pointer :: longitudes(:)
         real(kind=ESMF_KIND_R8), pointer :: latitudes(:)
         integer, optional, intent(out) :: rc

         integer :: status

         call ESMF_LocStreamGetKey(locstream, keyName='ESMF:Lon', farray=longitudes, _RC)
         call ESMF_LocStreamGetKey(locstream, keyName='ESMF:Lat', farray=latitudes, _RC)

         _RETURN(ESMF_SUCCESS)
      end subroutine get_locstream_coords

   end subroutine MAPL_GeomGetCoords

end submodule MAPL_GeomGetCoords_smod
