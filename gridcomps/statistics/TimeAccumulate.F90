#include "MAPL.h"

module mapl_TimeAccumulate_mod

   use mapl_AbstractTimeStatistic_mod
   use MAPL
   use ESMF

   implicit none(type,external)
   private

   public :: TimeAccumulate
   public :: advertise_time_accumulate_internal_fields

   type, extends(AbstractTimeStatistic) :: TimeAccumulate
      private
      type(MAPL_SimpleAlarm) :: alarm
      type(esmf_Field) :: f       ! input
      type(esmf_Field) :: accum_f ! output
   contains
      procedure :: destroy
      procedure :: reset
      procedure :: update
      procedure :: compute_result
      procedure :: add_to_state
      procedure :: get_alarm
   end type TimeAccumulate

   interface TimeAccumulate
      module procedure new_TimeAccumulate
   end interface TimeAccumulate

contains

   function new_TimeAccumulate(unusable, gridcomp, f, accum_f, alarm, rc) result(stat)
      type(TimeAccumulate) :: stat
      class(mapl_KeywordEnforcer), optional, intent(in) :: unusable
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_Field), intent(in) :: f
      type(esmf_Field), intent(inout) :: accum_f
      type(MAPL_SimpleAlarm), intent(in) :: alarm
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Geom), allocatable :: geom
      type(MAPL_UngriddedDims) :: ungridded_dims
      character(:), allocatable :: units, name
      type(esmf_TypeKind_Flag) :: typekind
      type(MAPL_VerticalStaggerLoc) :: vstagger
      class(mapl_VerticalGrid), pointer :: vertical_grid
      type(esmf_Field) :: sum_f

      stat%f = f
      stat%accum_f = accum_f
      stat%alarm = alarm

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(f, short_name=name, _RC)
      call mapl_FieldGet(f, &
           geom=geom, &
           ungridded_dims=ungridded_dims, &
           units=units, &
           typekind=typekind, &
           vgrid=vertical_grid, &
           vert_staggerloc=vstagger, &
           _RC)

      call mapl_FieldSet(accum_f, &
           geom=geom, &
           ungridded_dims=ungridded_dims, &
           units=units, &
           typekind=typekind, &
           vgrid=vertical_grid, &
           vert_staggerloc=vstagger, &
           standard_name='foo', &
           has_deferred_aspects=.false., &
           _RC)

      call esmf_StateGet(internal_state, 'sum_'//name, field=sum_f, _RC)
      call mapl_FieldSet(sum_f, &
           geom=geom, &
           ungridded_dims=ungridded_dims, &
           units=units, &
           typekind=typekind, &
           vgrid=vertical_grid, &
           vert_staggerloc=vstagger, &
           has_deferred_aspects=.false., &
           _RC)

      _UNUSED_DUMMY(unusable)
      _RETURN(_SUCCESS)
   end function new_TimeAccumulate

   subroutine destroy(this, rc)
      class(TimeAccumulate), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call esmf_FieldDestroy(this%accum_f, _RC)

      _RETURN(_SUCCESS)
   end subroutine destroy

   subroutine reset(this, gridcomp, rc)
      class(TimeAccumulate), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Field) :: sum_f
      character(:), allocatable :: name

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(this%f, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'sum_'//name, field=sum_f, _RC)

      call esmf_FieldFill(sum_f, dataFillScheme='const', const1=0.d0, _RC)

      _RETURN(_SUCCESS)
   end subroutine reset

   subroutine update(this, gridcomp, clock, rc)
      class(TimeAccumulate), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_Clock), intent(in) :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_TypeKind_Flag) :: typekind
      logical :: is_ringing
      type(esmf_Time) :: nextTime

      call mapl_FieldGet(this%f, typekind=typekind, _RC)

      if (typekind == ESMF_TYPEKIND_R4) then
         call update_r4(this, gridcomp, _RC)
      else if (typekind == ESMF_TYPEKIND_R8) then
         call update_r8(this, gridcomp, _RC)
      end if

      call ESMF_ClockGetNextTime(clock, nextTime=nextTime, _RC)
      is_ringing = this%alarm%is_ringing(nextTime, _RC)
      _RETURN_UNLESS(is_ringing)

      call this%compute_result(gridcomp, _RC)
      call this%reset(gridcomp, _RC)

      _RETURN(_SUCCESS)
   end subroutine update

   subroutine update_r4(this, gridcomp, rc)
      class(TimeAccumulate), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Field) :: sum_f
      character(:), allocatable :: name
      real(kind=ESMF_KIND_R4), pointer :: f(:), sum_f_ptr(:)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(this%f, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'sum_'//name, field=sum_f, _RC)

      call MAPL_AssignFptr(this%f, f, _RC)
      call MAPL_AssignFptr(sum_f, sum_f_ptr, _RC)

      where (sum_f_ptr /= MAPL_UNDEF)
         where (f == MAPL_UNDEF)
            sum_f_ptr = MAPL_UNDEF
         elsewhere
            sum_f_ptr = sum_f_ptr + f
         end where
      end where

      _RETURN(_SUCCESS)
   end subroutine update_r4

   subroutine update_r8(this, gridcomp, rc)
      class(TimeAccumulate), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Field) :: sum_f
      character(:), allocatable :: name
      real(kind=ESMF_KIND_R8), pointer :: f(:), sum_f_ptr(:)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(this%f, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'sum_'//name, field=sum_f, _RC)

      call MAPL_AssignFptr(this%f, f, _RC)
      call MAPL_AssignFptr(sum_f, sum_f_ptr, _RC)

      where (sum_f_ptr /= MAPL_UNDEF)
         where (f == MAPL_UNDEF)
            sum_f_ptr = MAPL_UNDEF
         elsewhere
            sum_f_ptr = sum_f_ptr + f
         end where
      end where

      _RETURN(_SUCCESS)
   end subroutine update_r8

   subroutine compute_result(this, gridcomp, rc)
      class(TimeAccumulate), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_TypeKind_Flag) :: typekind

      call mapl_FieldGet(this%f, typekind=typekind, _RC)

      if (typekind == ESMF_TYPEKIND_R4) then
         call compute_result_r4(this, gridcomp, _RC)
      else if (typekind == ESMF_TYPEKIND_R8) then
         call compute_result_r8(this, gridcomp, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine compute_result

   subroutine compute_result_r4(this, gridcomp, rc)
      class(TimeAccumulate), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Field) :: sum_f
      character(:), allocatable :: name
      real(kind=ESMF_KIND_R4), pointer :: sum_f_ptr(:), accum_f(:)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(this%f, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'sum_'//name, field=sum_f, _RC)

      call MAPL_AssignFptr(sum_f, sum_f_ptr, _RC)
      call MAPL_AssignFptr(this%accum_f, accum_f, _RC)

      accum_f = sum_f_ptr

      _RETURN(_SUCCESS)
   end subroutine compute_result_r4

   subroutine compute_result_r8(this, gridcomp, rc)
      class(TimeAccumulate), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Field) :: sum_f
      character(:), allocatable :: name
      real(kind=ESMF_KIND_R8), pointer :: sum_f_ptr(:), accum_f(:)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(this%f, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'sum_'//name, field=sum_f, _RC)

      call MAPL_AssignFptr(sum_f, sum_f_ptr, _RC)
      call MAPL_AssignFptr(this%accum_f, accum_f, _RC)

      accum_f = sum_f_ptr

      _RETURN(_SUCCESS)
   end subroutine compute_result_r8

   subroutine add_to_state(this, state, rc)
      class(TimeAccumulate), intent(inout) :: this
      type(esmf_State), intent(inout) :: state
      integer, optional, intent(out) :: rc

      integer :: status

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(state)

      _RETURN(_SUCCESS)
   end subroutine add_to_state

   function get_alarm(this) result(alarm)
      class(TimeAccumulate), intent(in) :: this
      type(MAPL_SimpleAlarm) :: alarm

      alarm = this%alarm
   end function get_alarm

   subroutine advertise_time_accumulate_internal_fields(gridcomp, name, rc)
      type(esmf_GridComp), intent(inout) :: gridcomp
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc

      integer :: status, slash_pos
      character(len=:), allocatable :: just_name

      slash_pos = index(name, "/")
      just_name = name
      if (slash_pos > 0) then
         just_name = name(slash_pos+1:)
      end if

      call MAPL_GridCompAddSpec(gridcomp, ESMF_STATEINTENT_INTERNAL, 'sum_'//just_name, fill_value=0.0, _RC)

      _RETURN(_SUCCESS)
   end subroutine advertise_time_accumulate_internal_fields

end module mapl_TimeAccumulate_mod
