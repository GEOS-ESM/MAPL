#include "MAPL.h"

submodule (mapl_GeomManager) initialize_smod

   implicit none

contains
   
   module subroutine initialize(this)
      use mapl_LatLonGeomFactory_mod
      use mapl_CubedSphereGeomFactory_mod
      use mapl_XYGeomFactory_mod
      use mapl_EASEGeomFactory_mod
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
