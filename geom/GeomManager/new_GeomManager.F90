#include "MAPL.h"

submodule (mapl3g_GeomManager) new_GeomManager_smod

   implicit none

contains

   module function new_GeomManager() result(mgr)
      use mapl3g_LatLonGeomFactory
      use mapl3g_CubedSphereGeomFactory
      use mapl3g_LocStreamGeomFactory
      use mapl3g_XYGeomFactory
      type(GeomManager) :: mgr

      type(LatLonGeomFactory)      :: latlon_factory
      type(CubedSphereGeomFactory) :: cs_factory
      type(LocStreamGeomFactory)   :: locstream_factory
      type(XYGeomFactory)          :: xy_factory
!#      type(TripolarGeomFactory) :: tripolar_factory
!#      type(CustomGeomFactory) :: custom_geom_factory

      call mgr%add_factory(latlon_factory)
      call mgr%add_factory(cs_factory)
      call mgr%add_factory(locstream_factory)
      call mgr%add_factory(xy_factory)
!#      call mgr%add_factory(tripolar_factory)
!#      call mgr%add_factory(custom_geom_factory)
!#
!#      ! Output only samplers.  These cannot be created from metadata.
!#      ! And likely have a time dependence.
!#      call mgr%add_factory(StationSampler_factory)
!#      call mgr%add_factory(TrajectorySampler_factory)
!#      call mgr%add_factory(SwathSampler_factory)

   end function new_GeomManager

end submodule new_GeomManager_smod
