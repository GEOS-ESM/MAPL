#include "MAPL_Generic.h"

module mapl3g_FakeDynGridComp

   use mapl_ErrorHandling
   use generic3g
   use mapl3g_FieldCondensedArray, only: assign_fptr_condensed_array
   use mapl3g_Field_API, only: MAPL_FieldEmptyComplete, VERTICAL_STAGGER_CENTER
   use mapl3g_UngriddedDims
   use esmf

   implicit none
   private

   public :: SetServices

contains

   subroutine SetServices(gridcomp, rc)
      type(ESMF_GridComp) :: gridcomp
      integer, intent(out) :: rc

      type(ESMF_HConfig) :: hconfig, collections_config, child_hconfig
      character(len=:), allocatable :: child_name, collection_name
      type(ESMF_HConfigIter) :: iter, iter_begin, iter_end
      logical :: has_active_collections
      ! class(logger), pointer :: lgr
      integer :: num_collections, status

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

      type(OuterMetaComponent), pointer :: outer_meta
      type(ESMF_Geom) :: geom
      class(VerticalGrid), allocatable :: vertical_grid
      type(ESMF_Field) :: field
      type(ESMF_FieldStatus_Flag) :: field_status
      real(kind=ESMF_KIND_R4), pointer :: pl(:, :, :)
      integer :: shape_(3), num_levels, horz, vert, ungrd, status

      outer_meta => get_outer_meta_from_inner_gc(gridcomp,_RC)
      geom = outer_meta%get_geom()
      vertical_grid = outer_meta%get_vertical_grid()

      call ESMF_StateGet(exportState, "PL", field, _RC)
      call ESMF_FieldGet(field, status=field_status, _RC)
      _ASSERT(field_status == ESMF_FIELDSTATUS_EMPTY, "field is not empty")
      call ESMF_FieldEmptySet(field, geom, _RC)
      call MAPL_FieldEmptyComplete( &
           field, &
           typekind=ESMF_TYPEKIND_R4, &
           ungridded_dims=UngriddedDims(), & !this%ungridded_dims, &
           num_levels=vertical_grid%get_num_levels(), &
           vert_staggerLoc=VERTICAL_STAGGER_CENTER, &
           units="hPa", &
           standard_name="air_pressure", &
           _RC)
      call assign_fptr_condensed_array(field, pl, _RC)
      shape_ = shape(pl); num_levels = shape_(2)
      do concurrent(horz = 1:shape_(1), ungrd = 1:shape_(3))
         do vert = 1, num_levels
            pl(horz, vert, ungrd) = real((num_levels - vert + 1) * 10)
         end do
      end do

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
