#include "MAPL.h"

module mapl_GenericNuopcModel_mod
   use :: NuopcMetaModel
   use :: mapl_GriddedComponentDriver_mod
   use esmf
   use nuopc
   use NUOPC_Model, modelSS    => SetServices
   use :: mapl_KeywordEnforcer_mod, only: KeywordEnforcer
   use :: mapl_ErrorHandling_mod
   implicit none(type,external)
   private

   ! Procedures
   public :: NuopcSetServices

   interface mapl_NuopcModelCreate
      procedure create_model
   end interface Mapl_NuopcModelCreate

contains

   recursive subroutine NuopcSetServices(model, rc)
      type(ESMF_GridComp) :: model
      integer, intent(out) :: rc

      integer :: status
      type(NuopcMetaModel), pointer :: meta_model
      type(GriddedComponentDriver) :: user_gc_driver
      type(esmf_GridComp) :: user_gridcomp
      type(DSoSetServices) :: user_SetServices

      type(esmf_HConfig) :: hconfig
      character(:), allocatable :: sharedObj, userRoutine

      call NUOPC_CompDerive(model, modelSS, _RC)
      
      call esmf_GridCompGet(model, hconfig=hconfig, _RC)
      sharedObj = esmf_HConfigAsString(hconfig, keystring='sharedObj', _RC)
      userRoutine = esmf_HConfigAsString(hconfig, keystring='userRoutine', _RC)
      user_setServices = DsoSetServices(sharedObj, userRoutine)
      
      user_gridcomp = ESMF_GridCompCreate(name=name, petlist=petlist, contextFlag=contextFlag, _RC)
      call set_subclass(user_gridcomp,subclass='user_gridcomp', _RC)
      user_gc_driver = GriddedComponentDriver(user_gridcomp)
         
      call attach_meta_model(model, _RC)
      meta_model => get_meta_model(model, _RC)

#ifndef __GFORTRAN__
      meta_model = NuopcMetaModel(model, user_gc_driver, user_SetServices, hconfig)
#else
      ! GFortran 12 & 13 cannot directly assign to meta_model.  But
      ! the assignment works for an object without the POINTER
      ! attribute.  An internal procedure is a workaround, but
      ! ... ridiculous.
      call ridiculous(meta_model, NuopcMetaModel(model, user_gc_driver, set_services, hconfig))
#endif
      call meta_model%init(_RC)

      call meta_model%setServices(_RC)
      call set_entry_points(model, _RC)

      _RETURN(ESMF_SUCCESS)

   contains

      subroutine set_entry_points(model, rc)
         type(ESMF_GridComp), intent(inout) :: model
         integer, intent(out) :: rc
         integer :: status
         integer :: phase_idx


         ! Entry points
         ! Initialize specs
         call NUOPC_CompSpecialize(model, specLabel=label_Advertise, specRoutine=Advertise, _RC)
         call NUOPC_CompSpecialize(model, specLabel=label_ModifyAdvertise, specRoutine=ModifyAdvertise, _RC)
         call NUOPC_CompSpecialize(model, specLabel=label_RealizeAccept, specRoutine=RealizeAccept, _RC)
         call NUOPC_CompSpecialize(model, specLabel=label_RealizeProvided, specRoutine=RealizeProvided, _RC)
         call NUOPC_CompSpecialize(model, specLabel=label_DataInitialize, specRoutine=DataInitialize, _RC)
         
         ! Advance specs
         ! MAPL Framework phases
         call NUOPC_CompSpecialize(model, specLabel=label_AdvanceClock, specRoutine=AdvanceClock, _RC)
         call NUOPC_CompSpecialize(model, specLabel=label_Advance, specRoutine=write_restart, specPhaseLabel='GENERIC::WRITE_RESTART', _RC)
         ! User phases
         associate (phases => meta_model%get_phases(ESMF_METHOD_RUN))
           do phase_idx = 1, phases%size()
              associate(phase_label => phases%of(phase_idx)
                call NUOPC_CompSpecialize(model, specLabel=label_Advance, specRoutine=Advance, specPhaseLabel=phase_label, _RC)
              end associate
           end do
         end associate

         ! Finalize specs
         call NUOPC_CompSpecialize(model, specLabel=label_Finalize, specRoutine=Finalize, _RC)

         _RETURN(ESMF_SUCCESS)
      end subroutine set_entry_points

   contains

      subroutine ridiculous(a, b)
         type(NuopcMetaModel), intent(out) :: a
         type(NuopcMetaModel), intent(in) :: b
         a = b
      end subroutine ridiculous
#endif
   end subroutine GenericSetServices

   recursive subroutine Advertise(model, rc)
      type(ESMF_GridComp) :: model
      integer, intent(out) :: rc

      integer :: status
      type(NuopcMetaModel), pointer :: meta_model

      meta_model => get_meta_model(model, _RC)
      call meta_model%advertise(_RC)

      _RETURN(_SUCCESS)
   end subroutine Advertise

   recursive subroutine ModifyAdvertise(model, rc)
      type(ESMF_GridComp) :: model
      integer, intent(out) :: rc

      integer :: status
      type(NuopcMetaModel), pointer :: meta_model

      meta_model => get_meta_model(model, _RC)
      call meta_model%modifyAdvertise(_RC)

      _RETURN(_SUCCESS)
   end subroutine ModifyAdvertise

   recursive subroutine RealizeAccept(model, rc)
      type(ESMF_GridComp) :: model
      integer, intent(out) :: rc

      integer :: status
      type(NuopcMetaModel), pointer :: meta_model

      meta_model => get_meta_model(model, _RC)
      call meta_model%realizeAccept(_RC)

      _RETURN(_SUCCESS)
   end subroutine RealizeAccept

   recursive subroutine RealizeProvided(model, rc)
      type(ESMF_GridComp) :: model
      integer, intent(out) :: rc

      integer :: status
      type(NuopcMetaModel), pointer :: meta_model

      meta_model => get_meta_model(model, _RC)
      call meta_model%realizeProvided(_RC)

      _RETURN(_SUCCESS)
   end subroutine RealizeProvided


   recursive subroutine DataInitialize(model, rc)
      type(ESMF_GridComp) :: model
      integer, intent(out) :: rc

      integer :: status
      type(NuopcMetaModel), pointer :: meta_model
      integer :: phaseIndex
      character(ESMF_MAXSTR) :: phaseLabel

      meta_model => get_meta_model(model, _RC)

      call meta_model%read_restart(_RC)
      call meta_model%init_user(_RC)

      _RETURN(_SUCCESS)
   end subroutine DataInitialize

   recursive subroutine Advance(model, rc)
      type(ESMF_GridComp) :: model
      integer, intent(out) :: rc

      integer :: status
      type(NuopcMetaModel), pointer :: meta_model
      integer :: phaseIndex
      character(ESMF_MAXSTR) :: phaseLabel

      meta_model => get_meta_model(model, _RC)
      call esmf_GridCompGet(model, currentPhase=phaseIndex, _RC)      
      call nuopc_GridCompSearchRevPhaseMap(model, ESMF_METHOD_INITIALIZE, &
           phaseIndex, phaseLabel, _RC)

      call meta_model%advance(trim(phaseLabel), _RC)

      _RETURN(_SUCCESS)
   end subroutine Advance

   recursive subroutine AdvanceClock(model, rc)
      type(ESMF_GridComp) :: model
      integer, intent(out) :: rc

      integer :: status
      type(NuopcMetaModel), pointer :: meta_model

      meta_model => get_meta_model(model, _RC)
      call meta_model%advance_clock(_RC)

      _RETURN(_SUCCESS)
   end subroutine AdvanceClock

   recursive subroutine Finalize(model, rc)
      type(ESMF_GridComp) :: model
      integer, intent(out) :: rc

      integer :: status
      type(NuopcMetaModel), pointer :: meta_model

      meta_model => get_meta_model(model, _RC)
      call meta_model%finalize(_RC)

      _RETURN(_SUCCESS)
   end subroutine Finalize


   ! Parent components name their children, but such names should
   ! apply to the (inner) user grid comp.  The MAPL wrapper gridcomp,
   ! has a different name derived from that name.
   ! "A" -->   "[A]"
   function outer_name(inner_name)
      character(:), allocatable :: outer_name
      character(*), intent(in) :: inner_name

      outer_name = "[" // inner_name // "]"
   end function outer_name

   subroutine set_subclass_nuopc(model, subclass, rc)
      type(ESMF_GridComp), intent(inout) :: model
      character(*), intent(in) :: subclass
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info

      call ESMF_InfoGetFromHost(model, info, _RC)
      call ESMF_InfoSet(info, key='mapl/subclass', value=subclass, _RC)

      _RETURN(_SUCCESS)
   end subroutine set_subclass_nuopc

end module mapl_GenericGridComp_mod
