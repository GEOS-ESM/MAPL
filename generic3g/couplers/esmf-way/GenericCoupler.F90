#include "Generic.h"

module mapl3g_GenericCoupler
   use CouplerMetaComponent.F90
   use mapl_ErrorHandlingMod
   use esmf
   implicit none
   private

   public :: setServices

   character(*), parameter :: COUPLER_PRIVATE_STATE = 'MAPL::CouplerMetaComponent::private'

contains

   function make_coupler(observed, rc) result(gridcomp)
      type(Observable) :: observed

      type(BidirectionalObserver), pointer :: observer

      gridcomp = ESMF_GridCompCreate(...)
      coupler = BidirectionalObserver(observed)
      coupler%self_gridcomp = gridcomp
      _SET_PRIVATE_STATE(gridcomp, observer, ...)

      _RETURN(_SUCCESS)
   end function make_coupler
   
   subroutine setServices(gridcomp, rc)
      ...

      call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, initialize, GENERIC_COUPLER_INITIALIZE, _RC)

      call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, update, GENERIC_COUPLER_UPDATE, RC)
      call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, invalidate, GENERIC_COUPLER_INVALIDATE, _RC)
      call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, advance, GENERIC_COUPLER_CLOCK_ADVANCE, _RC)

      _RETURN(_SUCCESS)
   end subroutine setServices


   subroutine initialize(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_State), intent(inout) :: importState
      type(ESMF_State), intent(inout) :: exportState
      type(ESMF_Clock), intent(inout) :: clock
      integer, intent(out) :: rc
      
      integer :: status
      type(CouplerMetaComponent), pointer :: meta

      meta => get_coupler_meta(gridcomp, _RC)
      call meta%initialize(importState, exportState, clock, _RC)

      _RETURN(_SUCCESS)
   end subroutine update


   subroutine update(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_State), intent(inout) :: importState
      type(ESMF_State), intent(inout) :: exportState
      type(ESMF_Clock), intent(inout) :: clock
      integer, intent(out) :: rc
      
      integer :: status
      type(CouplerMetaComponent), pointer :: meta

      meta => get_coupler_meta(gridcomp, _RC)
      call meta%update(importState, exportState, clock, _RC)

      _RETURN(_SUCCESS)
   end subroutine update

   
   subroutine invalidate(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_State), intent(inout) :: importState
      type(ESMF_State), intent(inout) :: exportState
      type(ESMF_Clock), intent(inout) :: clock
      integer, intent(out) :: rc
      
      integer :: status
      type(CouplerMetaComponent), pointer :: meta

      meta => get_coupler_meta(gridcomp, _RC)
      call meta%invalidate(importstate, exportState, clock, _RC)

      _RETURN(_SUCCESS)
   end subroutine invalidate


   subroutine advance(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_State), intent(inout) :: importState
      type(ESMF_State), intent(inout) :: exportState
      type(ESMF_Clock), intent(inout) :: clock
      integer, intent(out) :: rc
      
      integer :: status
      type(CouplerMetaComponent), pointer :: meta

      meta => get_coupler_meta(gridcomp)
      call coupler_meta%advance(importState, exportState, clock, _RC)

      ! TBD: is this where it belongs?
      call ESMF_ClockAdvance(clock, _RC)
     
      _RETURN(_SUCCESS)
   end subroutine advance


end module mapl3g_GenericCoupler
