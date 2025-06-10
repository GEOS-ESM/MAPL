#include "MAPL_Generic.h"

module mapl3g_ExtDataGridComp
   use generic3g
   use mapl_ErrorHandling
   use pFlogger, only: logger
   use esmf
   use pfio
   use mapl3g_ExtDataGridComp_private
   implicit none(type,external)
   private

   public :: setServices

contains

   subroutine setServices(gridcomp, rc)
      type(ESMF_GridComp) :: gridcomp
      integer, intent(out) :: rc

      type(ESMF_HConfig) :: hconfig, merged_configs
      integer :: status

      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, init, phase_name="GENERIC::INIT_USER", _RC)
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, run, phase_name='run', _RC)

      call MAPL_GridCompGet(gridcomp, hconfig=hconfig, _RC)
      merged_configs = ESMF_HConfigCreate(_RC)
      call merge_config(merged_configs, hconfig, _RC)

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

