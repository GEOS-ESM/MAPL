#include "MAPL_Generic.h"

module mapl3g_ConfigurableGridComp

   use mapl_ErrorHandling
   use mapl3g_Generic, only: MAPL_GridCompSetEntryPoint, MAPL_RunChildren
   use mapl3g_Generic, only: MAPL_GridCompGet
   use mapl3g_StateGet, only: MAPL_StateGet
   use mapl, only: MAPL_GetPointer
   use esmf

   implicit none
   private

   public :: setServices

   character(*), parameter :: MAPL_SECTION = "mapl"
   character(*), parameter :: COMPONENT_STATES_SECTION = "states"
   character(*), parameter :: COMPONENT_EXPORT_STATE_SECTION = "export"
   character(*), parameter :: KEY_K_VALUES = "k_values"

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

      type(ESMF_FieldBundle) :: export_bundle
      type(ESMF_Field), allocatable :: field_list(:)
      character(len=ESMF_MAXSTR) :: field_name
      type(ESMF_HConfig) :: hconfig, mapl_cfg, states_cfg, export_cfg, var_cfg
      logical :: has_k_values
      real(kind=ESMF_KIND_R4), allocatable :: k_values(:)
      real(kind=ESMF_KIND_R4), pointer :: ptr3d(:, :, :)
      integer :: field_count, idx, i, j, shape_(3), status

      call MAPL_GridCompGet(gridcomp, hconfig=hconfig, _RC)
      mapl_cfg = ESMF_HConfigCreateAt(hconfig, keyString=MAPL_SECTION, _RC)
      states_cfg = ESMF_HConfigCreateAt(mapl_cfg, keyString=COMPONENT_STATES_SECTION, _RC)
      export_cfg = ESMF_HConfigCreateAt(states_cfg, keyString=COMPONENT_EXPORT_STATE_SECTION, _RC)

      ! For each field getting 'export'ed, check hconfig and use k_values if specified
      export_bundle = MAPL_StateGet(exportState, _RC)
      call ESMF_FieldBundleGet(export_bundle, fieldCount=field_count, _RC)
      allocate(field_list(field_count), _STAT)
      call ESMF_FieldBundleGet(export_bundle, fieldList=field_list, _RC)
      do idx = 1, field_count
         call ESMF_FieldGet(field_list(idx), name=field_name, _RC)
         var_cfg = ESMF_HConfigCreateAt(export_cfg, keyString=trim(field_name), _RC)
         has_k_values = ESMF_HConfigIsDefined(var_cfg, keyString=KEY_K_VALUES, _RC)
         if (has_k_values) then
            k_values = ESMF_HConfigAsR4Seq(var_cfg, keyString=KEY_K_VALUES, _RC)
            call MAPL_GetPointer(exportState, ptr3d, trim(field_name), _RC)
            shape_ = shape(ptr3d)
            _ASSERT(shape_(3) == size(k_values), "incorrect number of k_values")
            do concurrent(i = 1:shape_(1), j=1:shape_(2))
               ptr3d(i, j, :) = k_values
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

      call MAPL_RunChildren(gridcomp, phase_name="run", _RC)

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
