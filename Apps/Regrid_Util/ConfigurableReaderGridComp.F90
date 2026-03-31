#include "MAPL.h"

module mapl3g_ConfigurableReaderGridComp

   use mapl_ErrorHandling
   use mapl3
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

      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, init, _RC)
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, run, phase_name="run", _RC)
      call MAPL_GridCompGet(gridcomp, hconfig=hconfig, _RC)
      input_file = ESMF_HConfigAsString(hconfig,keyString='input_file', _RC)
      _HERE,' bmaa '//trim(input_file) 
      call add_varspecs_from_file(gridcomp, input_file, ESMF_STATEINTENT_EXPORT, _RC)
      _RETURN(_SUCCESS)
   end subroutine setServices

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

      integer :: status 
      type(ESMF_Time) :: current_time
      call ESMF_ClockGet(clock, currTime=current_time, _RC)
      call ESMF_TimePrint(current_time, options='string', preString='bmaa reader time: ', _RC)
      _HERE, 'bmaa reader run'
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(gridcomp)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
   end subroutine run

end module mapl3g_ConfigurableReaderGridComp

subroutine setServices(gridcomp, rc)
   use ESMF
   use MAPL_ErrorHandlingMod
   use mapl3g_ConfigurableReaderGridComp, only: ConfigurableReader_setServices => SetServices
   type(ESMF_GridComp)  :: gridcomp
   integer, intent(out) :: rc

   integer :: status

   call ConfigurableReader_setServices(gridcomp, _RC)

   _RETURN(_SUCCESS)
end subroutine setServices
