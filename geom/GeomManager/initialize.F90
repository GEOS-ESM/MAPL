#include "MAPL.h"

submodule (mapl3g_GeomManager) initialize_smod

   implicit none

contains
   
   module subroutine initialize(this)
      use mapl3g_LatLonGeomFactory
      use mapl3g_CubedSphereGeomFactory
      use mapl3g_XYGeomFactory
      use mapl3g_EASEGeomFactory
      class(GeomManager), intent(inout) :: this

      ! Load default factories
      type(LatLonGeomFactory) :: latlon_factory
      type(CubedSphereGeomFactory) :: cs_factory
      type(XYGeomFactory) :: xy_factory
      type(EASEGeomFactory) :: ease_factory

      call this%add_factory(latlon_factory)
      call this%add_factory(cs_factory)
      call this%add_factory(xy_factory)
      call this%add_factory(ease_factory)

   end subroutine initialize

end submodule initialize_smod
