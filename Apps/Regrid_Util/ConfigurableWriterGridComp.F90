#include "MAPL.h"

module mapl3g_ConfigurableWriterGridComp

   use mapl_ErrorHandling
   use mapl3g_Generic, only: MAPL_GridCompSetEntryPoint
   use esmf
   use regrid_util_support_mod

   implicit none
   private

   public :: setServices

contains

   subroutine setServices(gridcomp, rc)
      type(ESMF_GridComp) :: gridcomp
      integer, intent(out) :: rc

      integer :: status
      type(ESMF_HConfig) :: hconfig
      character(len=:), allocatable :: input_file

      _HERE, ' bmaa '
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, init_geom, phase_name='GENERIC::INIT_GEOM_A', _RC)
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, init, _RC)
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, run, phase_name="run", _RC)
      call MAPL_GridCompGet(gridcomp, hconfig=hconfig, _RC)
      input_file = ESMF_HConfigAsString(hconfig,keyString='input_file', _RC)
      _HERE,' bmaa '//trim(input_file)
      call add_varspecs_from_file(gridcomp, input_file, ESMF_STATEINTENT_IMPORT, _RC)

      _RETURN(_SUCCESS)
   end subroutine setServices

   subroutine init_geom(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp)   :: gridcomp
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock
      integer, intent(out)  :: rc

      integer :: status
      type(ESMF_HConfig) :: hconfig
      type(GeomManager), pointer :: geom_mgr
      type(ESMF_HConfig) :: geom_hconfig
      type(MaplGeom) :: mapl_geom
      type(ESMF_Geom) :: geom

      call MAPL_GridCompGet(gridcomp, hconfig=hconfig, _RC)
      geom_mgr => get_geom_manager()
      geom_hconfig = ESMF_HConfigCreateAt(hconfig, keystring='output_geom', _RC)
      mapl_geom = geom_mgr%get_mapl_geom(geom_hconfig, _RC)
      geom = mapl_geom%get_geom()
      call ESMF_HConfigDestroy(geom_hconfig, _RC)
      call MAPL_GridCompSetGeom(gridcomp, geom, _RC)
      
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
   end subroutine init_geom

   subroutine init(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gridcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(gridcomp)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
   end subroutine init

   recursive subroutine run(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gridcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(gridcomp)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
   end subroutine run

end module mapl3g_ConfigurableWriterGridComp

subroutine setServices(gridcomp, rc)
   use ESMF
   use MAPL_ErrorHandlingMod
   use mapl3g_ConfigurableWriterGridComp, only: ConfigurableWriter_setServices => SetServices
   type(ESMF_GridComp)  :: gridcomp
   integer, intent(out) :: rc

   integer :: status

   call ConfigurableWriter_setServices(gridcomp, _RC)

   _RETURN(_SUCCESS)
end subroutine setServices
