#include "MAPL.h"

module mapl_GenericNuopcModel_mod
   use :: mapl_NuopcMetaModel_mod
   use :: mapl_GriddedComponentDriver_mod
   use esmf
   use nuopc
   use NUOPC_Model, modelSS => SetServices
   use :: mapl_KeywordEnforcer_mod, only: KeywordEnforcer
   use :: mapl_ErrorHandling_mod
   implicit none(type,external)
   private

   ! Procedures
   public :: SetServices

   interface set_subclass
      module procedure :: set_subclass_nuopc
   end interface set_subclass

contains

   recursive subroutine SetServices(model, rc)
      type(ESMF_GridComp) :: model
      integer, intent(out) :: rc
      integer :: status
      type(NuopcMetaModel), pointer :: meta_model
      type(GriddedComponentDriver) :: user_gc_driver
      type(esmf_GridComp) :: user_gridcomp
      type(esmf_HConfig) :: hconfig
      character(len=ESMF_MAXSTR) :: name

      call NUOPC_CompDerive(model, modelSS, _RC)
      call NUOPC_CompGet(model, name=name, _RC)
      call esmf_GridCompGet(model, hconfig=hconfig, _RC)
      user_gridcomp = ESMF_GridCompCreate(name=trim(name), _RC)
      call set_is_generic(user_gridcomp, flag=.FALSE., _RC)
      call set_subclass(user_gridcomp,subclass='user_gridcomp', _RC)
      user_gc_driver = GriddedComponentDriver(user_gridcomp)
      call set_is_generic(model, _RC)
      call attach_meta_model(model, _RC)
      meta_model => get_meta_model(model, _RC)

#ifndef __GFORTRAN__
      meta_model = NuopcMetaModel(model, user_gc_driver, hconfig)
#else
      ! GFortran 12 & 13 cannot directly assign to meta_model.  But
      ! the assignment works for an object without the POINTER
      ! attribute.  An internal procedure is a workaround, but
      ! ... ridiculous.
      call ridiculous(meta_model, NuopcMetaModel(model, user_gc_driver, hconfig))
#endif
!      call meta_model%init_user_gc(_RC) !wdb fixme deleteme Should be internal to NuopcMetaModel
      call meta_model%setServices(_RC)
      call set_entry_points(model, _RC)

      _RETURN(ESMF_SUCCESS)

   contains

      subroutine set_entry_points(model, rc)
         type(ESMF_GridComp), intent(inout) :: model
         integer, intent(out) :: rc
         integer :: status
         integer :: phase_idx
         character(len=:), pointer :: phase_label

         ! Entry points
         ! Initialize specs
         call NUOPC_CompSpecialize(model, specLabel=label_Advertise, specRoutine=Advertise, _RC)
         call NUOPC_CompSpecialize(model, specLabel=label_ModifyAdvertised, specRoutine=ModifyAdvertise, _RC)
         call NUOPC_CompSpecialize(model, specLabel=label_RealizeAccepted, specRoutine=RealizeAccept, _RC)
         call NUOPC_CompSpecialize(model, specLabel=label_RealizeProvided, specRoutine=RealizeProvided, _RC)
         call NUOPC_CompSpecialize(model, specLabel=label_DataInitialize, specRoutine=DataInitialize, _RC)
         
         ! Advance specs
         ! MAPL Framework phases
         call NUOPC_CompSpecialize(model, specLabel=label_AdvanceClock, specRoutine=AdvanceClock, _RC)
         call NUOPC_CompSpecialize(model, specLabel=label_Advance, specRoutine=write_restart, specPhaseLabel='GENERIC::WRITE_RESTART', _RC)
         ! User phases
         associate (phases => meta_model%get_phases(ESMF_METHOD_RUN))
           do phase_idx = 1, phases%size()
             phase_label => phases%of(phase_idx)
             call NUOPC_CompSpecialize(model, specLabel=label_Advance, specRoutine=Advance, specPhaseLabel=phase_label, _RC)
           end do
         end associate

         ! Finalize specs
         call NUOPC_CompSpecialize(model, specLabel=label_Finalize, specRoutine=Finalize, _RC)

         _RETURN(ESMF_SUCCESS)
      end subroutine set_entry_points

#ifdef __GFORTRAN__
      subroutine ridiculous(a, b)
         type(NuopcMetaModel), intent(out) :: a
         type(NuopcMetaModel), intent(in) :: b
         a = b
      end subroutine ridiculous
#endif
   end subroutine SetServices

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
      call meta_model%modify_advertise(_RC)

      _RETURN(_SUCCESS)
   end subroutine ModifyAdvertise

   recursive subroutine RealizeAccept(model, rc)
      type(ESMF_GridComp) :: model
      integer, intent(out) :: rc

      integer :: status
      type(NuopcMetaModel), pointer :: meta_model

      meta_model => get_meta_model(model, _RC)
      call meta_model%realize_accept(_RC)

      _RETURN(_SUCCESS)
   end subroutine RealizeAccept

   recursive subroutine RealizeProvided(model, rc)
      type(ESMF_GridComp) :: model
      integer, intent(out) :: rc

      integer :: status
      type(NuopcMetaModel), pointer :: meta_model

      meta_model => get_meta_model(model, _RC)
      call meta_model%realize_provided(_RC)

      _RETURN(_SUCCESS)
   end subroutine RealizeProvided


   recursive subroutine DataInitialize(model, rc)
      type(ESMF_GridComp) :: model
      integer, intent(out) :: rc

      integer :: status
      type(NuopcMetaModel), pointer :: meta_model

      meta_model => get_meta_model(model, _RC)
      call meta_model%data_initialize(_RC)

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
      call NUOPC_CompSearchRevPhaseMap(model, ESMF_METHOD_INITIALIZE, &
           phaseIndex=phaseIndex, phaseLabel=phaseLabel, _RC)
      call meta_model%advance(phaseLabel, _RC)

      _RETURN(_SUCCESS)
   end subroutine Advance

   recursive subroutine AdvanceClock(model, rc)
      type(ESMF_GridComp) :: model
      integer, intent(out) :: rc

      integer :: status
      type(NuopcMetaModel), pointer :: meta_model
      type(ESMF_Clock) :: modelClock

      meta_model => get_meta_model(model, _RC)
      call Nuopc_ModelGet(model, modelClock=modelClock, _RC)
      call meta_model%advance_clock(modelClock, _RC)

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

   recursive subroutine write_restart(model, rc)
      type(ESMF_GridComp) :: model
      integer, intent(out) :: rc
      integer :: status
      _RETURN(_SUCCESS)
   end subroutine write_restart

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

   subroutine set_is_generic(gridcomp, flag, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      logical, optional, intent(in) :: flag
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: flag_
      type(ESMF_Info) :: info

      flag_ = .true.
      if (present(flag)) flag_ = flag

      call ESMF_InfoGetFromHost(gridcomp, info, _RC)
      call ESMF_InfoSet(info, key='MAPL/GRIDCOMP_IS_GENERIC', value=flag_, _RC)

      _RETURN(_SUCCESS)
   end subroutine set_is_generic

end module mapl_GenericNuopcModel_mod
