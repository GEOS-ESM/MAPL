#include "MAPL.h"

submodule (mapl3g_Geom_API) geom_create_from_hconfig_smod

   use mapl_ErrorHandling
   use mapl3g_GeomManager, only: GeomManager, get_geom_manager
   use mapl3g_GeomSpec, only: GeomSpec
   use esmf, only: ESMF_KIND_R8

   implicit none(type,external)

contains

   module function geom_create_from_hconfig(hconfig, rc) result(geom)
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc
      type(ESMF_Geom) :: geom

      type(GeomManager), pointer :: geom_mgr
      class(GeomSpec), allocatable :: geom_spec
      type(MaplGeom) :: mapl_geom
      integer :: status

      geom_mgr => get_geom_manager()
      geom_spec = geom_mgr%make_geom_spec(hconfig, _RC)
      mapl_geom = geom_mgr%get_mapl_geom(geom_spec, _RC)
      geom = mapl_geom%get_geom()
   end function geom_create_from_hconfig

end submodule geom_create_from_hconfig_smod
