#include "MAPL_Generic.h"
module mapl3g_HistoryCollectionGridComp_private

   use generic3g
   use mapl3g_VariableSpec
   use esmf
   use Mapl_ErrorHandling
   use mapl3g_geom_mgr
   implicit none
   private

   public :: make_geom
   !public :: make_import_state

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

   !subroutine make_import_state(gridcomp, hconfig, rc)
      !type(ESMF_GridComp), intent(inout) :: gridcomp
      !type(ESMF_HConfig), intent(in) :: hconfig
      !integer, optional, intent(out) :: rc

      !type(ESMF_HConfigIter) :: iter, iter_begin, iter_end
      !type(ESMF_HConfig) :: var_list
      !character(len=:), allocatable :: var_name
      !type(VariableSpec) :: varspec
      !integer :: status

      !var_list = ESMF_HConfigCreateAt(hconfig, keystring='var_list', _RC)
      !iter_begin = ESMF_HConfigIterBegin(var_list,_RC)
      !iter_end = ESMF_HConfigIterEnd(var_list,_RC)
      !iter = iter_begin

      !do while (ESMF_HConfigIterLoop(iter,iter_begin,iter_end,rc=status))

         !var_name = ESMF_HConfigAsString(iter,_RC)
         !!varspec = VariableSpec(ESMF_STATEINTENT_IMPORT, var_name)
         !call MAPL_AddSpec(gridcomp, varspec, _RC)

      !end do
      !_RETURN(_SUCCESS)

   !end subroutine make_import_state

end module mapl3g_HistoryCollectionGridComp_private
