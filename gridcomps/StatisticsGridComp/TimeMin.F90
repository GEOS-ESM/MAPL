#include "MAPL.h"

module mapl3g_TimeMin

   use mapl3g_AbstractTimeStatistic
   use MAPL
   use ESMF
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer

   implicit none(type,external)
   private

   public :: TimeMin
   public :: advertise_time_min_internal_fields

   type, extends(AbstractTimeStatistic) :: TimeMin
      private
      type(esmf_Alarm) :: alarm
      type(esmf_Field) :: f      ! input
      type(esmf_Field) :: min_f  ! output
   contains
      procedure :: destroy
      procedure :: reset
      procedure :: update
      procedure :: compute_result
      procedure :: add_to_state
      procedure :: get_alarm
   end type TimeMin

   interface TimeMin
      module procedure new_TimeMin
   end interface TimeMin

contains

   function new_TimeMin(unusable, gridcomp, f, min_f, alarm, rc) result(stat)
      type(TimeMin) :: stat
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_Field), intent(in) :: f
      type(esmf_Field), intent(inout) :: min_f
      type(esmf_Alarm), intent(in) :: alarm
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Geom), allocatable :: geom
      type(UngriddedDims) :: ungridded_dims
      character(:), allocatable :: units, name
      type(esmf_TypeKind_Flag) :: typekind
      class(VerticalGrid), pointer :: vertical_grid
      type(VerticalStaggerLoc) :: vstagger
      type(esmf_Field) :: temp_min_f

      stat%f = f
      stat%min_f = min_f
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

      call mapl_FieldSet(min_f, &
           geom=geom, &
           ungridded_dims=ungridded_dims, &
           units=units, &
           typekind=typekind, &
           vgrid=vertical_grid, &
           vert_staggerloc=vstagger, &
           standard_name='foo', &
           has_deferred_aspects=.false., &
           _RC)

      call esmf_StateGet(internal_state, 'temp_min'//name, field=temp_min_f, _RC)
      call mapl_FieldSet(temp_min_f, &
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
   end function new_TimeMin

   subroutine destroy(this, rc)
      class(TimeMin), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call esmf_FieldDestroy(this%min_f, _RC)

      _RETURN(_SUCCESS)
   end subroutine destroy

   subroutine reset(this, gridcomp, rc)
      class(TimeMin), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Field) :: temp_min_f
      character(:), allocatable :: name
      type(esmf_TypeKind_Flag) :: typekind
      real(kind=ESMF_KIND_R4), pointer :: f4(:)
      real(kind=ESMF_KIND_R8), pointer :: f8(:)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(this%f, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'temp_min'//name, field=temp_min_f, _RC)

      call mapl_FieldGet(temp_min_f, typekind=typekind, _RC)
      if (typekind == ESMF_TYPEKIND_R4) then
         call MAPL_AssignFptr(temp_min_f, f4, _RC)
         f4 = MAPL_UNDEF
      else if (typekind == ESMF_TYPEKIND_R8) then
         call MAPL_AssignFptr(temp_min_f, f8, _RC)
         f8 = MAPL_UNDEF
      end if

      _RETURN(_SUCCESS)
   end subroutine reset

   subroutine update(this, gridcomp, rc)
      class(TimeMin), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_TypeKind_Flag) :: typekind
      logical :: is_ringing

      call mapl_FieldGet(this%f, typekind=typekind, _RC)

      if (typekind == ESMF_TYPEKIND_R4) then
         call update_r4(this, gridcomp, _RC)
      else if (typekind == ESMF_TYPEKIND_R8) then
         call update_r8(this, gridcomp, _RC)
      end if

      is_ringing = esmf_AlarmWillRingNext(this%alarm, _RC)
      _RETURN_UNLESS(is_ringing)

      call this%compute_result(gridcomp, _RC)
      call this%reset(gridcomp, _RC)

      _RETURN(_SUCCESS)
   end subroutine update

   subroutine update_r4(this, gridcomp, rc)
      class(TimeMin), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Field) :: temp_min_f
      character(:), allocatable :: name
      real(kind=ESMF_KIND_R4), pointer :: f(:), temp_min_f_ptr(:)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(this%f, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'temp_min'//name, field=temp_min_f, _RC)

      call MAPL_AssignFptr(this%f, f, _RC)
      call MAPL_AssignFptr(temp_min_f, temp_min_f_ptr, _RC)

      where (f /= MAPL_UNDEF)
         where (temp_min_f_ptr == MAPL_UNDEF)
            temp_min_f_ptr = f
         elsewhere
            temp_min_f_ptr = min(temp_min_f_ptr, f)
         end where
      end where

      _RETURN(_SUCCESS)
   end subroutine update_r4

   subroutine update_r8(this, gridcomp, rc)
      class(TimeMin), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Field) :: temp_min_f
      character(:), allocatable :: name
      real(kind=ESMF_KIND_R8), pointer :: f(:), temp_min_f_ptr(:)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(this%f, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'temp_min'//name, field=temp_min_f, _RC)

      call MAPL_AssignFptr(this%f, f, _RC)
      call MAPL_AssignFptr(temp_min_f, temp_min_f_ptr, _RC)

      where (f /= MAPL_UNDEF)
         where (temp_min_f_ptr == MAPL_UNDEF)
            temp_min_f_ptr = f
         elsewhere
            temp_min_f_ptr = min(temp_min_f_ptr, f)
         end where
      end where

      _RETURN(_SUCCESS)
   end subroutine update_r8

   subroutine compute_result(this, gridcomp, rc)
      class(TimeMin), intent(inout) :: this
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
      class(TimeMin), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Field) :: temp_min_f
      character(:), allocatable :: name
      real(kind=ESMF_KIND_R4), pointer :: temp_min_f_ptr(:), min_f(:)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(this%f, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'temp_min'//name, field=temp_min_f, _RC)

      call MAPL_AssignFptr(temp_min_f, temp_min_f_ptr, _RC)
      call MAPL_AssignFptr(this%min_f, min_f, _RC)

      min_f = temp_min_f_ptr

      _RETURN(_SUCCESS)
   end subroutine compute_result_r4

   subroutine compute_result_r8(this, gridcomp, rc)
      class(TimeMin), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Field) :: temp_min_f
      character(:), allocatable :: name
      real(kind=ESMF_KIND_R8), pointer :: temp_min_f_ptr(:), min_f(:)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(this%f, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'temp_min'//name, field=temp_min_f, _RC)

      call MAPL_AssignFptr(temp_min_f, temp_min_f_ptr, _RC)
      call MAPL_AssignFptr(this%min_f, min_f, _RC)

      min_f = temp_min_f_ptr

      _RETURN(_SUCCESS)
   end subroutine compute_result_r8

   subroutine add_to_state(this, state, rc)
      class(TimeMin), intent(inout) :: this
      type(esmf_State), intent(inout) :: state
      integer, optional, intent(out) :: rc

      integer :: status

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(state)

      _RETURN(_SUCCESS)
   end subroutine add_to_state

   function get_alarm(this) result(alarm)
      class(TimeMin), intent(in) :: this
      type(esmf_Alarm) :: alarm

      alarm = this%alarm
   end function get_alarm

   subroutine advertise_time_min_internal_fields(gridcomp, name, rc)
      type(esmf_GridComp), intent(inout) :: gridcomp
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc

      integer :: status, slash_pos
      type(VariableSpec) :: varspec
      character(len=:), allocatable :: just_name

      slash_pos = index(name, "/")
      just_name = name
      if (slash_pos > 0) then
         just_name = name(slash_pos+1:)
      end if

      varspec = make_VariableSpec(ESMF_STATEINTENT_INTERNAL, 'temp_min'//just_name, _RC)
      call MAPL_GridCompAddVarSpec(gridcomp, varspec, _RC)

      _RETURN(_SUCCESS)
   end subroutine advertise_time_min_internal_fields

end module mapl3g_TimeMin
