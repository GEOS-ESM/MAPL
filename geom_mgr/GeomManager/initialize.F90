#include "MAPL_Generic.h"

submodule (mapl3g_GeomManager) initialize_smod
   use mapl3g_GeomSpec
   use mapl3g_NullGeomSpec
   use mapl3g_MaplGeom
   use mapl3g_GeomFactory
   use mapl3g_GeomFactoryVector
   use mapl3g_GeomSpecVector
   use mapl3g_IntegerMaplGeomMap
   use mapl_ErrorHandlingMod
   use pfio_FileMetadataMod
   use esmf
   use gftl2_IntegerVector
   implicit none

contains
   
   module subroutine initialize(this)
      use mapl3g_LatLonGeomFactory
      class(GeomManager), intent(inout) :: this

      ! Load default factories
      type(LatLonGeomFactory) :: latlon_factory

      call this%add_factory(latlon_factory)

   end subroutine initialize

end submodule initialize_smod
