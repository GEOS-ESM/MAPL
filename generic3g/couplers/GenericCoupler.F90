#include "MAPL.h"

module mapl3g_GenericCoupler
   use mapl3g_CouplerPhases
   use mapl3g_CouplerMetaComponent
   use mapl3g_ExtensionTransform
   use mapl3g_TransformId
   use mapl3g_VerticalRegridTransform
   use mapl3g_ComponentDriver
   use mapl3g_GriddedComponentDriver
   use mapl_ErrorHandlingMod
   use esmf

   implicit none
   private

   public :: setServices
   public :: make_coupler
   public :: mapl_CouplerAddConsumer

   character(*), parameter :: COUPLER_PRIVATE_STATE = 'MAPL::CouplerMetaComponent::private'

contains

   function make_coupler(transform, source, rc) result(coupler_gridcomp)
      type(ESMF_GridComp) :: coupler_gridcomp
      class(ExtensionTransform), intent(in) :: transform
      class(ComponentDriver), target, optional, intent(in) :: source
      integer, optional, intent(out) :: rc

      integer :: status
      type(CouplerMetaComponent), pointer :: coupler_meta
      type(TransformId) :: id
      character(:), allocatable :: name

      id = transform%get_transformId()
      name = 'coupler['//id%to_string()//']'
      coupler_gridcomp = ESMF_GridCompCreate(name=name, contextFlag=ESMF_CONTEXT_PARENT_VM, _RC)
      call attach_coupler_meta(coupler_gridcomp, _RC)
      coupler_meta => get_coupler_meta(coupler_gridcomp, _RC)
#ifndef __GFORTRAN__
      coupler_meta = CouplerMetaComponent(transform, source)
#else
      call ridiculous(coupler_meta, CouplerMetaComponent(transform,source))
#endif
      call ESMF_GridCompSetServices(coupler_gridComp, setServices, _RC)

      _RETURN(_SUCCESS)

   contains

#ifdef __GFORTRAN__
      subroutine ridiculous(a, b)
         type(CouplerMetaComponent), intent(out) :: a
         type(CouplerMetaComponent), intent(in) :: b
         a = b
      end subroutine ridiculous
#endif

   end function make_coupler
   
   subroutine setServices(gridcomp, rc)
      type(ESMF_GridComp) :: gridcomp
      integer, intent(out) :: rc

      integer :: status

      call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, initialize, phase=GENERIC_COUPLER_INITIALIZE, _RC)

      call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, update, phase=GENERIC_COUPLER_UPDATE, _RC)
      call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, invalidate, phase=GENERIC_COUPLER_INVALIDATE, _RC)
      call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, clock_advance, phase=GENERIC_COUPLER_CLOCK_ADVANCE, _RC)

      _RETURN(_SUCCESS)
   end subroutine setServices


   recursive subroutine initialize(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gridcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc
      
      integer :: status
      type(CouplerMetaComponent), pointer :: meta

      meta => get_coupler_meta(gridcomp, _RC)
      call meta%initialize(importState, exportState, clock, _RC)

      _RETURN(_SUCCESS)
   end subroutine initialize


   recursive subroutine update(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gridcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc
      
      integer :: status
      type(CouplerMetaComponent), pointer :: meta

      meta => get_coupler_meta(gridcomp, _RC)
!#      call meta%update_time_varying(importState, exportState, _RC)
      call meta%update(importState, exportState, clock, _RC)

      _RETURN(_SUCCESS)
   end subroutine update

   
   recursive subroutine invalidate(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gridcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc
      
      integer :: status
      type(CouplerMetaComponent), pointer :: meta

      meta => get_coupler_meta(gridcomp, _RC)
!#      call meta%invalidate_time_varying(importState, exportState, _RC)
      call meta%invalidate(importstate, exportState, clock, _RC)

      _RETURN(_SUCCESS)
   end subroutine invalidate


   recursive subroutine clock_advance(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gridcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc
      
      integer :: status
      type(CouplerMetaComponent), pointer :: coupler_meta
      
      coupler_meta => get_coupler_meta(gridcomp)
      call coupler_meta%clock_advance(importState, exportState, clock, _RC)

      ! TBD: is this where it belongs?
      call ESMF_ClockAdvance(clock, _RC)
     
      _RETURN(_SUCCESS)
   end subroutine clock_advance


   subroutine mapl_CouplerAddConsumer(this, consumer, rc)
      class(ComponentDriver), intent(in) :: this
      class(ComponentDriver), intent(in) :: consumer
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_GridComp) :: gridcomp
      type(CouplerMetaComponent), pointer :: meta

      select type (this)
      type is (GriddedComponentDriver)
         gridcomp = this%get_gridcomp()
         meta => get_coupler_meta(gridcomp, _RC)
         call meta%add_consumer(consumer)
      class default
         _FAIL('Incorrect subclass of ComponentDriver.')
      end select

      _RETURN(_SUCCESS)
   end subroutine mapl_CouplerAddConsumer

end module mapl3g_GenericCoupler
