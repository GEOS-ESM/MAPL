#include "MAPL.h"

module mapl3g_ConfigurableGridComp

   use mapl_ErrorHandling
   use mapl3g_Generic, only: MAPL_GridCompSetEntryPoint, MAPL_GridCompRunChildren
   use mapl3g_Generic, only: MAPL_GridCompGet
   use mapl, only: MAPL_GetPointer
   use MAPL_FieldPointerUtilities
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
            call MAPL_GetPointer(exportState, ptr3d, trim(field_name), _RC)
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
      type(esmf_HConfig) :: hconfig
      logical :: has_run_section
      type(esmf_HConfig) :: run_cfg, field_cfg
      type(ESMF_HConfigIter) :: iter, e, b
      integer(kind=ESMF_KIND_I8) :: advanceCount
      integer, allocatable :: value
      real(ESMF_KIND_R4), pointer :: r4_ptr(:)
      real(ESMF_KIND_R8), pointer :: r8_ptr(:)
      type(esmf_Field) :: field
      character(:), allocatable :: field_name
      type(esmf_TypeKind_Flag) :: typekind
    

      call mapl_GridCompGet(gridcomp, hconfig=hconfig, _RC)

      has_run_section = esmf_HConfigIsDefined(hconfig, keyString='run', _RC)
      if (has_run_section) then
         call esmf_ClockGet(clock, advanceCount=advanceCount, _RC)
         run_cfg = esmf_HConfigCreateAt(hconfig, keyString='run', _RC)
         b = ESMF_HConfigIterBegin(run_cfg, _RC)
         e = ESMF_HConfigIterEnd(run_cfg, _RC)
         iter = b
         do while (ESMF_HConfigIterLoop(iter, b, e))
            field_name = ESMF_HConfigAsStringMapKey(iter, _RC)
            value = esmf_HConfigAsI4MapVal(iter, index=int(advanceCount+1), _RC)

            call esmf_StateGet(exportState, itemName=field_name, field=field, _RC)
            call esmf_FieldGet(field, typekind=typekind, _RC)
            if (typekind == ESMF_TYPEKIND_R4) then
               call assign_fptr(field, r4_ptr, _RC)
               r4_ptr = value
            else if (typekind == ESMF_TYPEKIND_R8) then
               call assign_fptr(field, r8_ptr, _RC)
               r8_ptr = value
               _HERE, 'field_name: ', field_name, value
            end if
         end do
         call esmf_HConfigDestroy(run_cfg, _RC)
      endif
          
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
