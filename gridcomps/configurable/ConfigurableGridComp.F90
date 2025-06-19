#include "MAPL_Generic.h"

module mapl3g_ConfigurableGridComp

   use mapl_ErrorHandling
   use mapl3g_Generic, only: MAPL_GridCompSetEntryPoint, MAPL_GridCompRunChildren
   use mapl3g_Generic, only: MAPL_GridCompGet
   use mapl3g_State_API, only: MAPL_StateGetPointer
   use esmf

   implicit none
   private

   public :: setServices

   character(*), parameter :: MAPL_SECTION = "mapl"
   character(*), parameter :: COMPONENT_STATES_SECTION = "states"
   character(*), parameter :: COMPONENT_EXPORT_STATE_SECTION = "export"
   character(*), parameter :: KEY_DEFAULT_VERT_PROFILE = "default_vertical_profile"

contains

   subroutine setServices(gridcomp, rc)
      type(ESMF_GridComp) :: gridcomp
      integer, intent(out) :: rc

      integer :: status

      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, init, _RC)
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, run, phase_name="run", _RC)

      _RETURN(_SUCCESS)
   end subroutine setServices

   subroutine init(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gridcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      character(:), allocatable :: field_name
      type(ESMF_HConfig) :: hconfig, mapl_cfg, states_cfg, export_cfg, field_cfg
      logical :: has_export_section, has_default_vert_profile
      real(kind=ESMF_KIND_R4), allocatable :: default_vert_profile(:)
      real(kind=ESMF_KIND_R4), pointer :: ptr3d(:, :, :)
      integer :: ii, jj, shape_(3), status

      type(ESMF_HConfigIter) :: iter, e, b

      call MAPL_GridCompGet(gridcomp, hconfig=hconfig, _RC)
      ! ASSUME: mapl and states sections always exist
      mapl_cfg = ESMF_HConfigCreateAt(hconfig, keyString=MAPL_SECTION, _RC)
      states_cfg = ESMF_HConfigCreateAt(mapl_cfg, keyString=COMPONENT_STATES_SECTION, _RC)
      has_export_section = ESMF_HConfigIsDefined(states_cfg, keyString=COMPONENT_EXPORT_STATE_SECTION, _RC)
      _RETURN_UNLESS(has_export_section)

      ! For each field getting 'export'ed, check hconfig and use default_vert_profile if specified
      export_cfg = ESMF_HConfigCreateAt(states_cfg, keyString=COMPONENT_EXPORT_STATE_SECTION, _RC)
      b = ESMF_HConfigIterBegin(export_cfg, _RC)
      e = ESMF_HConfigIterEnd(export_cfg, _RC)
      iter = b
      do while (ESMF_HConfigIterLoop(iter, b, e))
         field_name = ESMF_HConfigAsStringMapKey(iter, _RC)
         ! print *, "FIELD: ", field_name
         field_cfg = ESMF_HConfigCreateAtMapVal(iter, _RC)
         has_default_vert_profile = ESMF_HConfigIsDefined(field_cfg, keyString=KEY_DEFAULT_VERT_PROFILE, _RC)
         if (has_default_vert_profile) then
            default_vert_profile = ESMF_HConfigAsR4Seq(field_cfg, keyString=KEY_DEFAULT_VERT_PROFILE, _RC)
            call MAPL_StateGetPointer(exportState, ptr3d, trim(field_name), _RC)
            shape_ = shape(ptr3d)
            _ASSERT(shape_(3) == size(default_vert_profile), "incorrect size of vertical profile")
            do concurrent(ii = 1:shape_(1), jj=1:shape_(2))
               ptr3d(ii, jj, :) = default_vert_profile
            end do
         end if
      end do

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(clock)
   end subroutine init

   recursive subroutine run(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gridcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      integer :: status

      call MAPL_GridcompRunChildren(gridcomp, phase_name="run", _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
   end subroutine run

end module Mapl3g_ConfigurableGridComp

subroutine setServices(gridcomp, rc)
   use ESMF
   use MAPL_ErrorHandlingMod
   use mapl3g_ConfigurableGridComp, only: Configurable_setServices => SetServices
   type(ESMF_GridComp)  :: gridcomp
   integer, intent(out) :: rc

   integer :: status

   call Configurable_setServices(gridcomp, _RC)

   _RETURN(_SUCCESS)
end subroutine setServices
