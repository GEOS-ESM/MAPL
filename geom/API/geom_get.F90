#include "MAPL.h"

submodule (mapl3g_Geom_API) geom_get_smod

   use mapl_ErrorHandling
   use mapl3g_CubedSphereGeomSpec, only: CubedSphereGeomSpec

   implicit none(type,external)

contains

   module subroutine geom_get(geom, topology, rc)
      type(ESMF_Geom), intent(in) :: geom
      integer, allocatable, optional, intent(out) :: topology(:)
      integer, optional, intent(out) :: rc

      type(MaplGeom), pointer :: mapl_geom
      class(GeomSpec), allocatable :: geom_spec
      integer :: status

      mapl_geom => get_mapl_geom(geom, _RC)
      geom_spec = mapl_geom%get_spec()
      select type (geom_spec)
      type is (CubedSphereGeomSpec)
         _HERE
         topology = geom_spec%get_topology()
      class default
         _FAIL("geom_spec type not supported yet")
      end select

      _RETURN(_SUCCESS)
   end subroutine geom_get

end submodule geom_get_smod
