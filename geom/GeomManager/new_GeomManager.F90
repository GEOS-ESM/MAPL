#include "MAPL.h"

submodule (mapl3g_GeomManager) new_GeomManager_smod

   implicit none

contains
   
   module function new_GeomManager() result(mgr)
      use mapl3g_LatLonGeomFactory
      use mapl3g_CubedSphereGeomFactory
!#      use mapl_CubedSphereGeomFactory
      type(GeomManager) :: mgr

      ! Load default factories
      type(LatLonGeomFactory) :: latlon_factory
      type(CubedSphereGeomFactory) :: cs_factory
!#      type(FakeCubedSphereGeomFactory) :: fake_cs_factory 
!#      type(TripolarGeomFactory) :: tripolar_factory
!#      type(CustomGeomFactory) :: custom_geom_factory
!#
!#      call mgr%factories%push_back(latlon_factory)
!#      call mgr%factories%push_back(cs_factory)
!#      call mgr%factories%push_back(fake_cs_factory)
!#      call mgr%factories%push_back(tripolar_factory)
!#      call mgr%factories%push_back(custom_geom_factory)
!#
!#      ! Output only samplers.  These cannot be created from metadata.
!#      ! And likely have a time dependence.
!#      call mgr%factories%push_back(StationSampler_factory)
!#      call mgr%factories%push_back(TrajectorySampler_factory)
!#      call mgr%factories%push_back(SwathSampler_factory)

      call mgr%add_factory(latlon_factory)
      call mgr%add_factory(cs_factory)

   end function new_GeomManager

end submodule new_GeomManager_smod
