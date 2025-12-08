#include "MAPL.h"

module mapl3g_GeomGet

   use ESMF, only: ESMF_Geom
   use mapl_ErrorHandling
   use mapl3g_MaplGeom, only: MaplGeom
   use mapl3g_GeomSpec, only: GeomSpec
   use mapl3g_GeomManager, only: get_mapl_geom
   use mapl3g_CubedSphereGeomSpec, only: CubedSphereGeomSpec

   implicit none (type,external)
   private

   public :: GeomGet

   interface GeomGet
      procedure geom_get
   end interface GeomGet

contains

   subroutine geom_get(geom, topology, rc)
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
         topology = geom_spec%get_topology()
      class default
         _FAIL("geom_spec type not supported yet")
      end select

      _RETURN(_SUCCESS)
   end subroutine geom_get
      
end module mapl3g_GeomGet
