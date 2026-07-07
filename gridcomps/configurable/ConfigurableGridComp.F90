#include "MAPL.h"

module mapl_ConfigurableGridComp_mod

   use MAPL
   use esmf

   implicit none
   private

   public :: setServices

   character(*), parameter :: MAPL_SECTION = "mapl"
   character(*), parameter :: COMPONENT_STATES_SECTION = "states"
   character(*), parameter :: COMPONENT_EXPORT_STATE_SECTION = "export"
   character(*), parameter :: KEY_DEFAULT_VERT_PROFILE = "default_vertical_profile"
   character(*), parameter :: KEY_CHILDREN_SECTION = "children"
   character(*), parameter :: KEY_RUN_PHASES = "run_phases"

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
      logical :: has_states_section, has_export_section, has_default_vert_profile
      real(kind=ESMF_KIND_R4), allocatable :: default_vert_profile(:)
      real(kind=ESMF_KIND_R4), pointer :: ptr3d(:, :, :)
      integer :: ii, jj, shape_(3), status

      type(ESMF_HConfigIter) :: iter, e, b

      call MAPL_GridCompGet(gridcomp, hconfig=hconfig, _RC)
      ! ASSUME: mapl section always exists
      mapl_cfg = ESMF_HConfigCreateAt(hconfig, keyString=MAPL_SECTION, _RC)
      has_states_section = ESMF_HConfigIsDefined(mapl_cfg, keyString=COMPONENT_STATES_SECTION, _RC)
      _RETURN_UNLESS(has_states_section)
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
      type(esmf_HConfig) :: hconfig
      logical :: has_run_section
      type(esmf_HConfig) :: run_cfg
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
               call mapl_AssignFptr(field, r4_ptr, _RC)
               r4_ptr = value
            else if (typekind == ESMF_TYPEKIND_R8) then
               call mapl_AssignFptr(field, r8_ptr, _RC)
               r8_ptr = value
            end if
         end do
         call esmf_HConfigDestroy(run_cfg, _RC)
      endif

      call run_children_phases_(gridcomp, hconfig, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
   end subroutine run

   recursive subroutine run_children_phases_(gridcomp, hconfig, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, intent(out) :: rc

      integer :: status, i
      type(ESMF_HConfig) :: mapl_cfg, children_cfg, child_cfg
      logical :: has_mapl, has_children, has_run_phases
      character(ESMF_MAXSTR) :: child_name
      character(ESMF_MAXSTR), allocatable :: phase_names(:)
      type(ESMF_HConfigIter) :: iter, b, e

      has_mapl = ESMF_HConfigIsDefined(hconfig, keyString=MAPL_SECTION, _RC)
      _RETURN_UNLESS(has_mapl)

      mapl_cfg = ESMF_HConfigCreateAt(hconfig, keyString=MAPL_SECTION, _RC)
      has_children = ESMF_HConfigIsDefined(mapl_cfg, keyString=KEY_CHILDREN_SECTION, _RC)
      if (has_children) then
         children_cfg = ESMF_HConfigCreateAt(mapl_cfg, keyString=KEY_CHILDREN_SECTION, _RC)
         b = ESMF_HConfigIterBegin(children_cfg, _RC)
         e = ESMF_HConfigIterEnd(children_cfg, _RC)
         iter = b
         do while (ESMF_HConfigIterLoop(iter, b, e))
            child_name = ESMF_HConfigAsStringMapKey(iter, _RC)
            child_cfg = ESMF_HConfigCreateAtMapVal(iter, _RC)
            has_run_phases = ESMF_HConfigIsDefined(child_cfg, keyString=KEY_RUN_PHASES, _RC)
            phase_names = ['run']
            if (has_run_phases) then
               phase_names = ESMF_HConfigAsStringSeq(child_cfg, keyString=KEY_RUN_PHASES, stringLen=ESMF_MAXSTR, _RC)
            end if
            do i = 1, size(phase_names)
               call MAPL_GridCompRunChild(gridcomp, child_name=trim(child_name), phase_name=trim(phase_names(i)), _RC)
            end do
         end do
      end if
      call ESMF_HConfigDestroy(mapl_cfg, _RC)

      _RETURN(_SUCCESS)
   end subroutine run_children_phases_

end module mapl_ConfigurableGridComp_mod

subroutine setServices(gridcomp, rc)
   use ESMF
   use MAPL
   use mapl_ConfigurableGridComp_mod, only: Configurable_setServices => SetServices
   type(ESMF_GridComp)  :: gridcomp
   integer, intent(out) :: rc

   integer :: status

   call Configurable_setServices(gridcomp, _RC)

   _RETURN(_SUCCESS)
end subroutine setServices
