#include "MAPL.h"

module mapl3g_TimeMax

   use mapl3g_AbstractTimeStatistic
   use MAPL
   use ESMF
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer

   implicit none(type,external)
   private

   public :: TimeMax
   public :: advertise_time_max_internal_fields

   type, extends(AbstractTimeStatistic) :: TimeMax
      private
      type(esmf_Alarm) :: alarm
      type(esmf_Field) :: f      ! input
      type(esmf_Field) :: max_f  ! output
   contains
      procedure :: destroy
      procedure :: reset
      procedure :: update
      procedure :: compute_result
      procedure :: add_to_state
   end type TimeMax

   interface TimeMax
      module procedure new_TimeMax
   end interface TimeMax

contains

   function new_TimeMax(unusable, gridcomp, f, max_f, alarm, rc) result(stat)
      type(TimeMax) :: stat
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_Field), intent(in) :: f
      type(esmf_Field), intent(inout) :: max_f
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
      type(esmf_Field) :: temp_max_f

      stat%f = f
      stat%max_f = max_f
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

      call mapl_FieldSet(max_f, &
           geom=geom, &
           ungridded_dims=ungridded_dims, &
           units=units, &
           typekind=typekind, &
           vgrid=vertical_grid, &
           vert_staggerloc=vstagger, &
           standard_name='foo', &
           has_deferred_aspects=.false., &
           _RC)

      call esmf_StateGet(internal_state, 'temp_max'//name, field=temp_max_f, _RC)
      call mapl_FieldSet(temp_max_f, &
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
   end function new_TimeMax

   subroutine destroy(this, rc)
      class(TimeMax), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call esmf_FieldDestroy(this%max_f, _RC)

      _RETURN(_SUCCESS)
   end subroutine destroy

   subroutine reset(this, gridcomp, rc)
      class(TimeMax), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Field) :: temp_max_f
      character(:), allocatable :: name
      type(esmf_TypeKind_Flag) :: typekind
      real(kind=ESMF_KIND_R4), pointer :: f4(:)
      real(kind=ESMF_KIND_R8), pointer :: f8(:)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(this%f, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'temp_max'//name, field=temp_max_f, _RC)

      call mapl_FieldGet(temp_max_f, typekind=typekind, _RC)
      if (typekind == ESMF_TYPEKIND_R4) then
         call MAPL_AssignFptr(temp_max_f, f4, _RC)
         f4 = MAPL_UNDEF
      else if (typekind == ESMF_TYPEKIND_R8) then
         call MAPL_AssignFptr(temp_max_f, f8, _RC)
         f8 = MAPL_UNDEF
      end if

      _RETURN(_SUCCESS)
   end subroutine reset

   subroutine update(this, gridcomp, rc)
      class(TimeMax), intent(inout) :: this
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
      class(TimeMax), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Field) :: temp_max_f
      character(:), allocatable :: name
      real(kind=ESMF_KIND_R4), pointer :: f(:), temp_max_f_ptr(:)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(this%f, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'temp_max'//name, field=temp_max_f, _RC)

      call MAPL_AssignFptr(this%f, f, _RC)
      call MAPL_AssignFptr(temp_max_f, temp_max_f_ptr, _RC)

      where (f /= MAPL_UNDEF)
         where (temp_max_f_ptr == MAPL_UNDEF)
            temp_max_f_ptr = f
         elsewhere
            temp_max_f_ptr = max(temp_max_f_ptr, f)
         end where
      end where

      _RETURN(_SUCCESS)
   end subroutine update_r4

   subroutine update_r8(this, gridcomp, rc)
      class(TimeMax), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Field) :: temp_max_f
      character(:), allocatable :: name
      real(kind=ESMF_KIND_R8), pointer :: f(:), temp_max_f_ptr(:)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(this%f, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'temp_max'//name, field=temp_max_f, _RC)

      call MAPL_AssignFptr(this%f, f, _RC)
      call MAPL_AssignFptr(temp_max_f, temp_max_f_ptr, _RC)

      where (f /= MAPL_UNDEF)
         where (temp_max_f_ptr == MAPL_UNDEF)
            temp_max_f_ptr = f
         elsewhere
            temp_max_f_ptr = max(temp_max_f_ptr, f)
         end where
      end where

      _RETURN(_SUCCESS)
   end subroutine update_r8

   subroutine compute_result(this, gridcomp, rc)
      class(TimeMax), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_TypeKind_Flag) :: typekind

      call mapl_FieldGet(this%f, typekind=typekind, _RC)

      if (typekind == ESMF_TYPEKIND_R4) then
         call compute_result_r4(this, gridcomp, _RC)
      else if (typekind == ESMF_TYPEKIND_R8) then
         call compute_result_r8(this, gridcomp, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine compute_result

   subroutine compute_result_r4(this, gridcomp, rc)
      class(TimeMax), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Field) :: temp_max_f
      character(:), allocatable :: name
      real(kind=ESMF_KIND_R4), pointer :: temp_max_f_ptr(:), max_f(:)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(this%f, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'temp_max'//name, field=temp_max_f, _RC)

      call MAPL_AssignFptr(temp_max_f, temp_max_f_ptr, _RC)
      call MAPL_AssignFptr(this%max_f, max_f, _RC)

      max_f = temp_max_f_ptr

      _RETURN(_SUCCESS)
   end subroutine compute_result_r4

   subroutine compute_result_r8(this, gridcomp, rc)
      class(TimeMax), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Field) :: temp_max_f
      character(:), allocatable :: name
      real(kind=ESMF_KIND_R8), pointer :: temp_max_f_ptr(:), max_f(:)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(this%f, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'temp_max'//name, field=temp_max_f, _RC)

      call MAPL_AssignFptr(temp_max_f, temp_max_f_ptr, _RC)
      call MAPL_AssignFptr(this%max_f, max_f, _RC)

      max_f = temp_max_f_ptr

      _RETURN(_SUCCESS)
   end subroutine compute_result_r8

   subroutine add_to_state(this, state, rc)
      class(TimeMax), intent(inout) :: this
      type(esmf_State), intent(inout) :: state
      integer, optional, intent(out) :: rc

      integer :: status

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(state)

      _RETURN(_SUCCESS)
   end subroutine add_to_state

   subroutine advertise_time_max_internal_fields(gridcomp, name, rc)
      type(esmf_GridComp), intent(inout) :: gridcomp
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc

      integer :: status
      type(VariableSpec) :: varspec

      varspec = make_VariableSpec(ESMF_STATEINTENT_INTERNAL, 'temp_max'//name, _RC)
      call MAPL_GridCompAddVarSpec(gridcomp, varspec, _RC)

      _RETURN(_SUCCESS)
   end subroutine advertise_time_max_internal_fields

end module mapl3g_TimeMax
