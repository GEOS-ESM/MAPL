#include "MAPL.h"
! See external setservices() procedure at end of file

! A user gridcomp with one BRACKET export. YAML cannot declare a bracket, so
! it is declared here with MAPL_GridCompAddSpec.
module ProtoBracketGridComp
   use mapl_Generic_mod
   use mapl_VerticalStaggerLoc_mod
   use mapl_ErrorHandling_mod
   use mapl_Generic_api, only: mapl_GridCompAddSpec
   use mapl_StateItem_mod, only: MAPL_STATEITEM_BRACKET
   use mapl_field_bundle_api, only: MAPL_FieldBundleSet
   use esmf
   implicit none(type,external)
   private

   public :: setservices

contains

   subroutine setservices(gc, rc)
      use mapl_Generic_api, only: MAPL_GridCompSetEntryPoint
      type(ESMF_GridComp) :: gc
      integer, intent(out) :: rc

      integer :: status

      call mapl_GridCompSetEntryPoint(gc, ESMF_METHOD_RUN, run, phase_name="run", _RC)

      call mapl_GridCompAddSpec(gc, short_name='B', &
           state_intent=ESMF_STATEINTENT_EXPORT, &
           itemtype=MAPL_STATEITEM_BRACKET, &
           bracket_size=2, &
           dims='xy', &
           vertical_stagger=VERTICAL_STAGGER_NONE, &
           standard_name='air_temperature', &
           units='K', _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine setservices

   ! The bracket is marked updated and given weights, which the couplers read.
   ! Its members are not filled: the scenario checks the graph structure only.
   subroutine run(gc, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gc
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      integer :: status
      type(ESMF_FieldBundle) :: bundle

      call ESMF_StateGet(exportState, itemName='B', fieldBundle=bundle, _RC)
      call MAPL_FieldBundleSet(bundle, interpolation_weights=[0., 0.5, 0.5], &
           bracket_updated=.true., _RC)

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(gc)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
   end subroutine run

end module ProtoBracketGridComp

subroutine setServices(gc, rc)
   use esmf, only: ESMF_GridComp
   use esmf, only: ESMF_SUCCESS
   use mapl_ErrorHandling_mod
   use ProtoBracketGridComp, only: inner_setservices => setServices
   type(ESMF_GridComp) :: gc
   integer, intent(out) :: rc

   integer :: status

   call inner_setservices(gc, _RC)

   _RETURN(ESMF_SUCCESS)
end subroutine setServices
