#include "MAPL.h"

module mapl3g_TimeAverage

   use mapl3g_AbstractTimeStatistic
   use MAPL
   use ESMF
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer

   implicit none(type,external)
   private

   public :: TimeAverage
   public :: advertise_time_average_internal_fields

   type, extends(AbstractTimeStatistic) :: TimeAverage
      private
      type(esmf_Alarm) :: alarm
      type(esmf_Field) :: f      ! input
      type(esmf_Field) :: avg_f  ! output
   contains
      procedure :: destroy
      procedure :: reset
      procedure :: update
      procedure :: compute_result
      procedure :: add_to_state
   end type TimeAverage

   interface TimeAverage
      module procedure new_TimeAverage
   end interface TimeAverage

contains

   function new_TimeAverage(unusable, gridcomp, f, avg_f, alarm, rc) result(stat)
      type(TimeAverage) :: stat
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_Field), intent(in) :: f
      type(esmf_Field), intent(in) :: avg_f
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
      type(esmf_Field) :: sum_f, counts_f

      stat%f = f
      stat%avg_f = avg_f
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

      call esmf_StateGet(internal_state, 'counts_'//name, field=counts_f, _RC)
      call mapl_FieldSet(counts_f, &
           geom=geom, &
           ungridded_dims=ungridded_dims, &
           units='1', &
           typekind=ESMF_TYPEKIND_I4, &
           vgrid=vertical_grid, &
           vert_staggerloc=vstagger, &
           has_deferred_aspects=.false., &
           _RC)

      _UNUSED_DUMMY(unusable)
      _RETURN(_SUCCESS)
   end function new_TimeAverage

   subroutine destroy(this, rc)
      class(TimeAverage), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call esmf_FieldDestroy(this%avg_f, _RC)

      _RETURN(_SUCCESS)
   end subroutine destroy

   subroutine reset(this, gridcomp, rc)
      class(TimeAverage), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Field) :: sum_f, counts_f
      character(:), allocatable :: name
      integer(kind=ESMF_KIND_I4), pointer :: counts(:)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(this%f, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'sum_'//name, field=sum_f, _RC)
      call esmf_StateGet(internal_state, 'counts_'//name, field=counts_f, _RC)

      call esmf_FieldFill(sum_f, dataFillScheme='const', const1=0.d0, _RC)
      call MAPL_AssignFptr(counts_f, counts, _RC)
      counts = 0

      _RETURN(_SUCCESS)
   end subroutine reset

   subroutine update(this, gridcomp, rc)
      class(TimeAverage), intent(inout) :: this
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
      class(TimeAverage), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Field) :: sum_f, counts_f
      character(:), allocatable :: name
      real(kind=ESMF_KIND_R4), pointer :: f(:), sum_f_ptr(:)
      integer(kind=ESMF_KIND_I4), pointer :: counts(:)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(this%f, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'sum_'//name, field=sum_f, _RC)
      call esmf_StateGet(internal_state, 'counts_'//name, field=counts_f, _RC)

      call MAPL_AssignFptr(this%f, f, _RC)
      call MAPL_AssignFptr(sum_f, sum_f_ptr, _RC)
      call MAPL_AssignFptr(counts_f, counts, _RC)

      where (f /= MAPL_UNDEF)
         sum_f_ptr = sum_f_ptr + f
         counts = counts + 1
      end where

      _RETURN(_SUCCESS)
   end subroutine update_r4

   subroutine update_r8(this, gridcomp, rc)
      class(TimeAverage), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Field) :: sum_f, counts_f
      character(:), allocatable :: name
      real(kind=ESMF_KIND_R8), pointer :: f(:), sum_f_ptr(:)
      integer(kind=ESMF_KIND_I4), pointer :: counts(:)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(this%f, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'sum_'//name, field=sum_f, _RC)
      call esmf_StateGet(internal_state, 'counts_'//name, field=counts_f, _RC)

      call MAPL_AssignFptr(this%f, f, _RC)
      call MAPL_AssignFptr(sum_f, sum_f_ptr, _RC)
      call MAPL_AssignFptr(counts_f, counts, _RC)

      where (f /= MAPL_UNDEF)
         sum_f_ptr = sum_f_ptr + f
         counts = counts + 1
      end where

      _RETURN(_SUCCESS)
   end subroutine update_r8

   subroutine compute_result(this, gridcomp, rc)
      class(TimeAverage), intent(inout) :: this
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
      class(TimeAverage), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Field) :: sum_f, counts_f
      character(:), allocatable :: name
      real(kind=ESMF_KIND_R4), pointer :: f(:), sum_f_ptr(:), avg_f(:)
      integer(kind=ESMF_KIND_I4), pointer :: counts(:)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(this%f, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'sum_'//name, field=sum_f, _RC)
      call esmf_StateGet(internal_state, 'counts_'//name, field=counts_f, _RC)

      call MAPL_AssignFptr(this%f, f, _RC)
      call MAPL_AssignFptr(sum_f, sum_f_ptr, _RC)
      call MAPL_AssignFptr(this%avg_f, avg_f, _RC)
      call MAPL_AssignFptr(counts_f, counts, _RC)

      where (counts > 0)
         avg_f = sum_f_ptr / counts
      elsewhere
         avg_f = MAPL_UNDEF
      end where

      _RETURN(_SUCCESS)
   end subroutine compute_result_r4

   subroutine compute_result_r8(this, gridcomp, rc)
      class(TimeAverage), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Field) :: sum_f, counts_f
      character(:), allocatable :: name
      real(kind=ESMF_KIND_R8), pointer :: f(:), sum_f_ptr(:), avg_f(:)
      integer(kind=ESMF_KIND_I4), pointer :: counts(:)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(this%f, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'sum_'//name, field=sum_f, _RC)
      call esmf_StateGet(internal_state, 'counts_'//name, field=counts_f, _RC)

      call MAPL_AssignFptr(this%f, f, _RC)
      call MAPL_AssignFptr(sum_f, sum_f_ptr, _RC)
      call MAPL_AssignFptr(this%avg_f, avg_f, _RC)
      call MAPL_AssignFptr(counts_f, counts, _RC)

      where (counts > 0)
         avg_f = sum_f_ptr / counts
      elsewhere
         avg_f = MAPL_UNDEF
      end where

      _RETURN(_SUCCESS)
   end subroutine compute_result_r8

   subroutine add_to_state(this, state, rc)
      class(TimeAverage), intent(inout) :: this
      type(esmf_State), intent(inout) :: state
      integer, optional, intent(out) :: rc

      integer :: status

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(state)

      _RETURN(_SUCCESS)
   end subroutine add_to_state

   subroutine advertise_time_average_internal_fields(gridcomp, name, rc)
      type(esmf_GridComp), intent(inout) :: gridcomp
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc

      integer :: status
      type(VariableSpec) :: varspec

      varspec = make_VariableSpec(ESMF_STATEINTENT_INTERNAL, 'sum_'//name, _RC)
      call MAPL_GridCompAddVarSpec(gridcomp, varspec, _RC)

      varspec = make_VariableSpec(ESMF_STATEINTENT_INTERNAL, 'counts_'//name, _RC)
      call MAPL_GridCompAddVarSpec(gridcomp, varspec, _RC)

      _RETURN(_SUCCESS)
   end subroutine advertise_time_average_internal_fields

end module mapl3g_TimeAverage
