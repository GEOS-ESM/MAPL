#include "MAPL_Generic.h"
module mapl3g_HistoryCollectionGridComp_private

   use generic3g
   use esmf
   use Mapl_ErrorHandling
   use mapl3g_geom_mgr
   implicit none
   private

   public :: make_geom

contains

   function make_geom(hconfig, rc) result(geom)
      type(ESMF_Geom) :: geom
      type(ESMF_HConfig), intent(inout) :: hconfig
      integer, optional, intent(out) :: rc
      integer :: status
      type(GeomManager), pointer :: geom_mgr
      type(ESMF_HConfig) :: geom_hconfig
      type(MaplGeom) :: mapl_geom

      geom_mgr => get_geom_manager()

      geom_hconfig = ESMF_HConfigCreateAt(hconfig, keystring='geom', _RC)
      mapl_geom = geom_mgr%get_mapl_geom(geom_hconfig, _RC)
      geom = mapl_geom%get_geom()

      call ESMF_HConfigDestroy(geom_hconfig, _RC)
      _RETURN(_SUCCESS)
   end function make_geom

end module mapl3g_HistoryCollectionGridComp_private
