#include "MAPL_Generic.h"

module mapl3g_ExtDataGridComp
   use generic3g
   use mapl_ErrorHandling
   use pFlogger, only: logger
   use esmf
   use pfio
   use mapl3g_ExtDataGridComp_private
   use mapl3g_Geom_API
   
   implicit none(type,external)
   private

   public :: setServices

contains

   subroutine setServices(gridcomp, rc)
      type(ESMF_GridComp) :: gridcomp
      integer, intent(out) :: rc

      type(ESMF_HConfig) :: hconfig, merged_hconfig
      ! we will make a random grid right to use when adding varspec
      ! now because of MAPL3 limitations
      ! this will be removed when we can
      type(ESMF_HConfig) :: grid_hconfig
      type(GeomManager), pointer :: geom_mgr
      type(MaplGeom) :: mapl_geom
      type(ESMF_Geom) :: fake_geom
      integer :: status

      type(BasicVerticalGrid) :: vertical_grid

      vertical_grid = BasicVerticalGrid(4)
      call MAPL_GRidCompSetVerticalGrid(gridcomp, vertical_grid, _RC)

      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, init, phase_name="GENERIC::INIT_USER", _RC)
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, run, phase_name='run', _RC)

      call MAPL_GridCompGet(gridcomp, hconfig=hconfig, _RC)
      
      grid_hconfig = ESMF_HConfigCreate(content="{class: latlon, im_world: 12, jm_world: 13, pole: PC, dateline: DC, nx: 1, ny: 1}", _RC)
      geom_mgr => get_geom_manager()
      mapl_geom = geom_mgr%get_mapl_geom(grid_hconfig, _RC)
      fake_geom = mapl_geom%get_geom()
      ! ESMF has a bug, for now we will not merge hconfig until fixed
      !merged_configs = ESMF_HConfigCreate(_RC)
      ! instead pass hconfig and this will have to traverse the subconfigs for now
      call add_var_specs(gridcomp, hconfig, fake_geom, _RC)

      _RETURN(_SUCCESS)
   end subroutine setServices

   subroutine init(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp)   :: gridcomp
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, intent(out)  :: rc

      integer :: status
      
      _RETURN(_SUCCESS)
   end subroutine init


   subroutine run(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp)   :: gridcomp
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, intent(out)  :: rc

      integer :: status

      call MAPL_GridcompRunChildren(gridcomp, phase_name='run', _RC)
      ! for now I will hard code weights...
      call set_weights(exportState, _RC) 

      _RETURN(_SUCCESS)
   end subroutine run

end module mapl3g_ExtDataGridComp

subroutine setServices(gridcomp,rc)
   use ESMF
   use MAPL_ErrorHandlingMod
   use mapl3g_ExtDataGridComp, only: ExtData_setServices => SetServices    
   type(ESMF_GridComp)  :: gridcomp
   integer, intent(out) :: rc

   integer :: status

   call ExtData_setServices(gridcomp,_RC)
   _RETURN(_SUCCESS)

end subroutine

