#include "MAPL_ErrLog.h"
! See external setservices() procedure at end of file

module ProtoStatGridComp
   use mapl3g_State_API
   use mapl3g_Field_API
   use mapl3g_Generic
   use mapl3g_esmf_subset
   use mapl3g_VerticalStaggerLoc
   use mapl_ErrorHandling
   use esmf
   implicit none(type, external)
   private

   public :: setservices
   logical, save :: exports_ready = .false.
   
contains

   subroutine setservices(gc, rc)
      use mapl3g_Generic, only: MAPL_GridCompSetEntryPoint
      type(ESMF_GridComp) :: gc
      integer, intent(out) :: rc

      integer :: status

      call mapl_GridCompSetEntryPoint(gc, ESMF_METHOD_INITIALIZE, init_modify_advertised, phase_name='GENERIC::INIT_MODIFY_ADVERTISED', _RC)
      call mapl_GridCompSetEntryPoint(gc, ESMF_METHOD_INITIALIZE, init, phase_name="GENERIC::INIT_USER", _RC)
      call mapl_GridCompSetEntryPoint(gc, ESMF_METHOD_RUN, run, phase_name="run", _RC)

      call mapl_GridCompAddSpec(gc, short_name='A/T', &
           state_intent=ESMF_STATEINTENT_IMPORT, &
           standard_name='<unknown>', &
           dims='xy', &
           vstagger=VERTICAL_STAGGER_NONE, &
           units='K', _RC)

      call mapl_GridCompAddSpec(gc, short_name='avg_T', &
           state_intent=ESMF_STATEINTENT_EXPORT, &
           standard_name='<unknown>', &
           dims='xy', &
           vstagger=VERTICAL_STAGGER_NONE, &
           has_deferred_aspects=.true., &
           units='K', _RC)

      exports_ready = .false.

      _RETURN(ESMF_SUCCESS)
   end subroutine setservices

   
   subroutine init_modify_advertised(gc, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gc
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      integer :: status
      type(ESMF_Field) :: field

      _RETURN_IF(exports_ready)
      
      call esmf_StateGet(exportState, itemName='avg_T', field=field, _RC)
      call mapl_FieldSet(field, has_deferred_aspects = .false., _RC)

      exports_ready = .true.

      _RETURN(_SUCCESS)
   end subroutine init_modify_advertised



   subroutine init(gc, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gc
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      integer :: status
      real, pointer :: X(:,:)
      real, pointer :: avg_X(:,:)

       _RETURN(ESMF_SUCCESS)
   end subroutine init
   
   subroutine run(gc, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gc
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      integer :: status
      real, pointer :: X(:,:)
      real, pointer :: avg_X(:,:)

!#      call mapl_StateGetPointer(importState, X, 'X', _RC)
!#      call mapl_StateGetPointer(exportState, avg_X, 'avg_X', _RC)
!#
!#      _HERE
!#      X = 1
!#      _HERE
!#      avg_X = 2
!#      _HERE

      _RETURN(ESMF_SUCCESS)
   end subroutine run
   
end module ProtoStatGridComp

subroutine setServices(gc, rc)
   use esmf, only: ESMF_GridComp
   use esmf, only: ESMF_SUCCESS
   use mapl_ErrorHandling
   use ProtoStatGridComp, only: inner_setservices => setServices
   type(ESMF_GridComp) :: gc
   integer, intent(out) :: rc

   integer :: status

   call inner_setservices(gc, _RC)

   _RETURN(ESMF_SUCCESS)
end subroutine setServices
