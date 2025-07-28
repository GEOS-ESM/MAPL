#include "MAPL.h"

submodule (mapl3g_GeomManager) initialize_smod

   implicit none

contains
   
   module subroutine initialize(this)
      use mapl3g_LatLonGeomFactory
      use mapl3g_CubedSphereGeomFactory
      class(GeomManager), intent(inout) :: this

      ! Load default factories
      type(LatLonGeomFactory) :: latlon_factory
      type(CubedSphereGeomFactory) :: cs_factory

      call this%add_factory(latlon_factory)
      call this%add_factory(cs_factory)

   end subroutine initialize

end submodule initialize_smod
