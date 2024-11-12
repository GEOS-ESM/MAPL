#include "MAPL_Generic.h"

module mapl3g_FakeDynGridComp

   use mapl_ErrorHandling
   use generic3g
   use mapl3g_FieldCondensedArray, only: assign_fptr_condensed_array
   use mapl3g_Field_API, only: MAPL_FieldEmptyComplete, VerticalStaggerLoc, VERTICAL_STAGGER_CENTER
   use mapl3g_UngriddedDims
   use esmf

   implicit none
   private

   public :: SetServices

contains

   subroutine SetServices(gridcomp, rc)
      type(ESMF_GridComp) :: gridcomp
      integer, intent(out) :: rc

      integer :: status

      ! Set entry points
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, init, phase_name="GENERIC::INIT_REALIZE", _RC)
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, run, phase_name="run", _RC)

      _RETURN(_SUCCESS)
   end subroutine setServices

   subroutine init(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gridcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out)  :: rc

      type(ESMF_Geom) :: geom
      type(ESMF_Field) :: field1, field2
      class(VerticalGrid), allocatable :: vertical_grid
      integer :: num_levels, status

      call MAPL_GridCompGet(gridcomp, geom=geom, vertical_grid=vertical_grid, _RC)
      num_levels = vertical_grid%get_num_levels()

      call ESMF_StateGet(exportState, "PL", field1, _RC)
      call field_complete_(field1, geom, num_levels, VERTICAL_STAGGER_CENTER, "hPa", "air_pressure", rc)
      call set_pressure_(field1, _RC)

      call ESMF_StateGet(exportState, "T_DYN", field2, _RC)
      call field_complete_(field2, geom, num_levels, VERTICAL_STAGGER_CENTER, "K", "temeperature", rc)
      call set_temperature_(field2, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(clock)
   end subroutine init

   subroutine run(gridcomp, importState, exportState, clock, rc)
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

   subroutine field_complete_(field, geom, num_levels, vertical_stagger, units, standard_name, rc)
      type(ESMF_Field), intent(inout) :: field
      type(ESMF_Geom), intent(in) :: geom
      integer, intent(in) :: num_levels
      type(VerticalStaggerLoc), intent(in) :: vertical_stagger
      character(*), intent(in) :: units
      character(*), intent(in) :: standard_name
      integer, optional, intent(out) :: rc

      type(ESMF_FieldStatus_Flag) :: field_status
      integer :: status

      call ESMF_FieldGet(field, status=field_status, _RC)
      _ASSERT(field_status == ESMF_FIELDSTATUS_EMPTY, "field is not empty")
      call ESMF_FieldEmptySet(field, geom, _RC)
      call MAPL_FieldEmptyComplete( &
           field, &
           typekind=ESMF_TYPEKIND_R4, &
           ungridded_dims=UngriddedDims(), &
           num_levels=num_levels, &
           vert_staggerLoc=vertical_stagger, &
           units=units, &
           standard_name=standard_name, &
           _RC)

      _RETURN(_SUCCESS)
   end subroutine field_complete_

   subroutine set_pressure_(field, rc)
      type(ESMF_Field), intent(inout) :: field
      integer, optional, intent(out) :: rc

      real(kind=ESMF_KIND_R4), pointer :: farr(:, :, :)
      integer :: shape_(3), num_levels, horz, vert, ungrd, status

      call assign_fptr_condensed_array(field, farr, _RC)
      shape_ = shape(farr); num_levels = shape_(2)
      do concurrent(horz = 1:shape_(1), ungrd = 1:shape_(3))
         do vert = 1, num_levels
            farr(horz, vert, ungrd) = real((num_levels - vert + 1) * 10)
         end do
      end do

      _RETURN(_SUCCESS)
   end subroutine set_pressure_

   subroutine set_temperature_(field, rc)
      type(ESMF_Field), intent(inout) :: field
      integer, optional, intent(out) :: rc

      real(kind=ESMF_KIND_R4), pointer :: farr(:, :, :)
      integer :: shape_(3), num_levels, horz, vert, ungrd, status

      call assign_fptr_condensed_array(field, farr, _RC)
      shape_ = shape(farr); num_levels = shape_(2)
      do concurrent(horz = 1:shape_(1), ungrd = 1:shape_(3))
         do vert = 1, num_levels
            farr(horz, vert, ungrd) = real(5 * (2 ** (num_levels - vert)))
         end do
      end do

      _RETURN(_SUCCESS)
   end subroutine set_temperature_

end module mapl3g_FakeDynGridComp

subroutine SetServices(gridcomp,rc)
   use MAPL_ErrorHandlingMod
   use mapl3g_FakeDynGridComp, only: FakeDyn_SetServices => SetServices
   use esmf

   type(ESMF_GridComp)  :: gridcomp
   integer, intent(out) :: rc

   integer :: status

   call FakeDyn_SetServices(gridcomp,_RC)
   _RETURN(_SUCCESS)
end subroutine SetServices
