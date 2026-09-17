#include "MAPL.h"

module mapl_TimeAverage_mod

   use mapl_AbstractTimeStatistic_mod
   use MAPL
   use ESMF

   implicit none(type,external)
   private

   public :: TimeAverage
   public :: advertise_time_average_internal_fields

   type, extends(AbstractTimeStatistic) :: TimeAverage
      private
      logical :: is_bundle = .false.
      type(MAPL_SimpleAlarm) :: alarm
      type(esmf_Field) :: f      ! input
      type(esmf_Field) :: avg_f  ! output
      type(ESMF_FieldBundle) :: b ! input
      type(ESMF_FieldBundle) :: avg_b ! output
   contains
      procedure :: destroy
      procedure :: reset
      procedure :: update
      procedure :: compute_result
      procedure :: add_to_state
      procedure :: get_alarm
   end type TimeAverage

   interface TimeAverage
      module procedure new_TimeAverage_field
      module procedure new_TimeAverage_fieldbundle
   end interface TimeAverage

contains

   function new_TimeAverage_field(unusable, gridcomp, f, avg_f, alarm, rc) result(stat)
      type(TimeAverage) :: stat
      class(mapl_KeywordEnforcer), optional, intent(in) :: unusable
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_Field), intent(in) :: f
      type(esmf_Field), intent(inout) :: avg_f
       type(MAPL_SimpleAlarm), intent(in) :: alarm
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Geom), allocatable :: geom
      type(mapl_UngriddedDims) :: ungridded_dims
      character(:), allocatable :: units, name
      type(esmf_TypeKind_Flag) :: typekind
      class(mapl_VerticalGrid), pointer :: vertical_grid
      type(mapl_VerticalStaggerLoc) :: vstagger
      type(esmf_Field) :: sum_f, counts_f

      stat%is_bundle = .false.
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

      call mapl_FieldSet(avg_f, &
           geom=geom, &
           ungridded_dims=ungridded_dims, &
           units=units, &
           typekind=typekind, &
           vgrid=vertical_grid, &
           vert_staggerloc=vstagger, &
           standard_name='foo', &
           _RC)

      call esmf_StateGet(internal_state, 'sum_'//name, field=sum_f, _RC)
      call mapl_FieldSet(sum_f, &
           geom=geom, &
           ungridded_dims=ungridded_dims, &
           units=units, &
           typekind=typekind, &
           vgrid=vertical_grid, &
           vert_staggerloc=vstagger, &
           _RC)

      call esmf_StateGet(internal_state, 'counts_'//name, field=counts_f, _RC)
      call mapl_FieldSet(counts_f, &
           geom=geom, &
           ungridded_dims=ungridded_dims, &
           units='1', &
           typekind=ESMF_TYPEKIND_I4, &
           vgrid=vertical_grid, &
           vert_staggerloc=vstagger, &
           _RC)

      _UNUSED_DUMMY(unusable)
      _RETURN(_SUCCESS)
   end function new_TimeAverage_field

   function new_TimeAverage_fieldbundle(unusable, gridcomp, b, avg_b, alarm, rc) result(stat)
      type(TimeAverage) :: stat
      class(mapl_KeywordEnforcer), optional, intent(in) :: unusable
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_FieldBundle), intent(in) :: b
      type(esmf_FieldBundle), intent(inout) :: avg_b
       type(MAPL_SimpleAlarm), intent(in) :: alarm
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Geom), allocatable :: geom
      type(mapl_UngriddedDims) :: ungridded_dims
      character(:), allocatable :: units, name
      type(esmf_TypeKind_Flag) :: typekind
      class(mapl_VerticalGrid), pointer :: vertical_grid
      type(mapl_VerticalStaggerLoc) :: vstagger
      type(esmf_FieldBundle) :: sum_b, counts_b

      stat%is_bundle = .true.
      stat%b = b
      stat%avg_b = avg_b
      stat%alarm = alarm

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldBundleGet(b, short_name=name, _RC)
      call mapl_FieldBundleGet(b, &
           geom=geom, &
           ungridded_dims=ungridded_dims, &
           units=units, &
           typekind=typekind, &
           vgrid=vertical_grid, &
           vert_staggerloc=vstagger, &
           _RC)

      call mapl_fieldbundleset(avg_b, &
           geom=geom, &
           ungridded_dims=ungridded_dims, &
           units=units, &
           typekind=typekind, &
           vgrid=vertical_grid, &
           vert_staggerloc=vstagger, &
           standard_name='foo', &
           _RC)

      call esmf_StateGet(internal_state, 'sum_'//name, fieldbundle=sum_b, _RC)
      call mapl_fieldbundleset(sum_b, &
           geom=geom, &
           ungridded_dims=ungridded_dims, &
           units=units, &
           typekind=typekind, &
           vgrid=vertical_grid, &
           vert_staggerloc=vstagger, &
           _RC)

      call esmf_StateGet(internal_state, 'counts_'//name, fieldbundle=counts_b, _RC)
      call mapl_fieldbundleset(counts_b, &
           geom=geom, &
           ungridded_dims=ungridded_dims, &
           units='1', &
           typekind=ESMF_TYPEKIND_I4, &
           vgrid=vertical_grid, &
           vert_staggerloc=vstagger, &
           _RC)

      _UNUSED_DUMMY(unusable)
      _RETURN(_SUCCESS)
   end function new_TimeAverage_fieldbundle


   subroutine destroy(this, rc)
      class(TimeAverage), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      if (this%is_bundle) then
         call MAPL_FieldBundleDestroy(this%avg_b, _RC)
      else
         call esmf_FieldDestroy(this%avg_f, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine destroy

   subroutine reset(this, gridcomp, rc)
      class(TimeAverage), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status

      if (this%is_bundle) then
         call reset_bundle(this, gridcomp, _RC)
      else
         call reset_field(this, gridcomp, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine reset

   subroutine reset_field(this, gridcomp, rc)
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
   end subroutine reset_field

   subroutine reset_bundle(this, gridcomp, rc)
      class(TimeAverage), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status, i
      type(esmf_State) :: internal_state
      type(esmf_FieldBundle) :: sum_b, counts_b
      character(:), allocatable :: name
      type(esmf_Field), allocatable :: sum_fieldlist(:), counts_fieldlist(:)
      integer(kind=ESMF_KIND_I4), pointer :: counts(:)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldBundleGet(this%b, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'sum_'//name, fieldbundle=sum_b, _RC)
      call esmf_StateGet(internal_state, 'counts_'//name, fieldbundle=counts_b, _RC)

      call mapl_FieldBundleGet(sum_b, fieldList=sum_fieldlist, _RC)
      call mapl_FieldBundleGet(counts_b, fieldList=counts_fieldlist, _RC)

      do i = 1, size(sum_fieldlist)
         call esmf_FieldFill(sum_fieldlist(i), dataFillScheme='const', const1=0.d0, _RC)
         call MAPL_AssignFptr(counts_fieldlist(i), counts, _RC)
         counts = 0
      end do

      _RETURN(_SUCCESS)
   end subroutine reset_bundle

   subroutine update(this, gridcomp, clock, rc)
      class(TimeAverage), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_Clock), intent(in) :: clock
      integer, optional, intent(out) :: rc

       integer :: status
       type(esmf_TypeKind_Flag) :: typekind
       logical :: is_ringing
       type(esmf_Time) :: nextTime

       if (this%is_bundle) then
          call mapl_FieldBundleGet(this%b, typekind=typekind, _RC)
       else
          call mapl_FieldGet(this%f, typekind=typekind, _RC)
       end if

       if (this%is_bundle) then
          if (typekind == ESMF_TYPEKIND_R4) then
             call update_bundle_r4(this, gridcomp, _RC)
          else if (typekind == ESMF_TYPEKIND_R8) then
             call update_bundle_r8(this, gridcomp, _RC)
          end if
       else
          if (typekind == ESMF_TYPEKIND_R4) then
             call update_r4(this, gridcomp, _RC)
          else if (typekind == ESMF_TYPEKIND_R8) then
             call update_r8(this, gridcomp, _RC)
          end if
       end if

       call ESMF_ClockGetNextTime(clock, nextTime=nextTime, _RC)
       is_ringing = this%alarm%is_ringing(nextTime, _RC)
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

   subroutine update_bundle_r4(this, gridcomp, rc)
      class(TimeAverage), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, intent(out) :: rc

      integer :: status, i
      type(esmf_State) :: internal_state
      type(esmf_FieldBundle) :: sum_b, counts_b
      character(:), allocatable :: name
      type(esmf_Field), allocatable :: fieldlist(:), sum_fieldlist(:), counts_fieldlist(:)
      real(kind=ESMF_KIND_R4), pointer :: f(:), sum_f_ptr(:)
      integer(kind=ESMF_KIND_I4), pointer :: counts(:)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldBundleGet(this%b, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'sum_'//name, fieldbundle=sum_b, _RC)
      call esmf_StateGet(internal_state, 'counts_'//name, fieldbundle=counts_b, _RC)

      call mapl_FieldBundleGet(this%b, fieldList=fieldlist, _RC)
      call mapl_FieldBundleGet(sum_b, fieldList=sum_fieldlist, _RC)
      call mapl_FieldBundleGet(counts_b, fieldList=counts_fieldlist, _RC)

      do i = 1, size(fieldlist)
         call MAPL_AssignFptr(fieldlist(i), f, _RC)
         call MAPL_AssignFptr(sum_fieldlist(i), sum_f_ptr, _RC)
         call MAPL_AssignFptr(counts_fieldlist(i), counts, _RC)

         where (f /= MAPL_UNDEF)
            sum_f_ptr = sum_f_ptr + f
            counts = counts + 1
         end where
      end do

      _RETURN(_SUCCESS)
   end subroutine update_bundle_r4

   subroutine update_bundle_r8(this, gridcomp, rc)
      class(TimeAverage), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, intent(out) :: rc

      integer :: status, i
      type(esmf_State) :: internal_state
      type(esmf_FieldBundle) :: sum_b, counts_b
      character(:), allocatable :: name
      type(esmf_Field), allocatable :: fieldlist(:), sum_fieldlist(:), counts_fieldlist(:)
      real(kind=ESMF_KIND_R8), pointer :: f(:), sum_f_ptr(:)
      integer(kind=ESMF_KIND_I4), pointer :: counts(:)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldBundleGet(this%b, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'sum_'//name, fieldbundle=sum_b, _RC)
      call esmf_StateGet(internal_state, 'counts_'//name, fieldbundle=counts_b, _RC)

      call mapl_FieldBundleGet(this%b, fieldList=fieldlist, _RC)
      call mapl_FieldBundleGet(sum_b, fieldList=sum_fieldlist, _RC)
      call mapl_FieldBundleGet(counts_b, fieldList=counts_fieldlist, _RC)

      do i = 1, size(fieldlist)
         call MAPL_AssignFptr(fieldlist(i), f, _RC)
         call MAPL_AssignFptr(sum_fieldlist(i), sum_f_ptr, _RC)
         call MAPL_AssignFptr(counts_fieldlist(i), counts, _RC)

         where (f /= MAPL_UNDEF)
            sum_f_ptr = sum_f_ptr + f
            counts = counts + 1
         end where
      end do

      _RETURN(_SUCCESS)
   end subroutine update_bundle_r8

   subroutine compute_result(this, gridcomp, rc)
      class(TimeAverage), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_TypeKind_Flag) :: typekind

      if (this%is_bundle) then
         call mapl_FieldBundleGet(this%b, typekind=typekind, _RC)
      else
         call mapl_FieldGet(this%f, typekind=typekind, _RC)
      end if

      if (this%is_bundle) then
         if (typekind == ESMF_TYPEKIND_R4) then
            call compute_result_bundle_r4(this, gridcomp, _RC)
         else if (typekind == ESMF_TYPEKIND_R8) then
            call compute_result_bundle_r8(this, gridcomp, _RC)
         end if
      else
         if (typekind == ESMF_TYPEKIND_R4) then
            call compute_result_r4(this, gridcomp, _RC)
         else if (typekind == ESMF_TYPEKIND_R8) then
            call compute_result_r8(this, gridcomp, _RC)
         end if
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

   subroutine compute_result_bundle_r4(this, gridcomp, rc)
      class(TimeAverage), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status, i
      type(esmf_State) :: internal_state
      type(esmf_FieldBundle) :: sum_b, counts_b
      character(:), allocatable :: name
      type(esmf_Field), allocatable :: avg_fieldlist(:), sum_fieldlist(:), counts_fieldlist(:)
      real(kind=ESMF_KIND_R4), pointer :: sum_f_ptr(:), avg_f(:)
      integer(kind=ESMF_KIND_I4), pointer :: counts(:)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldBundleGet(this%b, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'sum_'//name, fieldbundle=sum_b, _RC)
      call esmf_StateGet(internal_state, 'counts_'//name, fieldbundle=counts_b, _RC)

      call mapl_FieldBundleGet(this%avg_b, fieldList=avg_fieldlist, _RC)
      call mapl_FieldBundleGet(sum_b, fieldList=sum_fieldlist, _RC)
      call mapl_FieldBundleGet(counts_b, fieldList=counts_fieldlist, _RC)

      do i = 1, size(avg_fieldlist)
         call MAPL_AssignFptr(sum_fieldlist(i), sum_f_ptr, _RC)
         call MAPL_AssignFptr(avg_fieldlist(i), avg_f, _RC)
         call MAPL_AssignFptr(counts_fieldlist(i), counts, _RC)

         where (counts > 0)
            avg_f = sum_f_ptr / counts
         elsewhere
            avg_f = MAPL_UNDEF
         end where
      end do

      _RETURN(_SUCCESS)
   end subroutine compute_result_bundle_r4

   subroutine compute_result_bundle_r8(this, gridcomp, rc)
      class(TimeAverage), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status, i
      type(esmf_State) :: internal_state
      type(esmf_FieldBundle) :: sum_b, counts_b
      character(:), allocatable :: name
      type(esmf_Field), allocatable :: avg_fieldlist(:), sum_fieldlist(:), counts_fieldlist(:)
      real(kind=ESMF_KIND_R8), pointer :: sum_f_ptr(:), avg_f(:)
      integer(kind=ESMF_KIND_I4), pointer :: counts(:)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldBundleGet(this%b, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'sum_'//name, fieldbundle=sum_b, _RC)
      call esmf_StateGet(internal_state, 'counts_'//name, fieldbundle=counts_b, _RC)

      call mapl_FieldBundleGet(this%avg_b, fieldList=avg_fieldlist, _RC)
      call mapl_FieldBundleGet(sum_b, fieldList=sum_fieldlist, _RC)
      call mapl_FieldBundleGet(counts_b, fieldList=counts_fieldlist, _RC)

      do i = 1, size(avg_fieldlist)
         call MAPL_AssignFptr(sum_fieldlist(i), sum_f_ptr, _RC)
         call MAPL_AssignFptr(avg_fieldlist(i), avg_f, _RC)
         call MAPL_AssignFptr(counts_fieldlist(i), counts, _RC)

         where (counts > 0)
            avg_f = sum_f_ptr / counts
         elsewhere
            avg_f = MAPL_UNDEF
         end where
      end do

      _RETURN(_SUCCESS)
   end subroutine compute_result_bundle_r8

   subroutine add_to_state(this, state, rc)
      class(TimeAverage), intent(inout) :: this
      type(esmf_State), intent(inout) :: state
      integer, optional, intent(out) :: rc

      integer :: status

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(state)

      _RETURN(_SUCCESS)
   end subroutine add_to_state

    function get_alarm(this) result(alarm)
       class(TimeAverage), intent(in) :: this
       type(MAPL_SimpleAlarm) :: alarm

      alarm = this%alarm
   end function get_alarm

   subroutine advertise_time_average_internal_fields(gridcomp, name, item_type, rc)
      type(esmf_GridComp), intent(inout) :: gridcomp
      character(*), intent(in) :: name
      type(ESMF_StateItem_Flag), intent(in) :: item_type
      integer, optional, intent(out) :: rc

      integer :: status, slash_pos
       character(len=:), allocatable :: just_name

       slash_pos = index(name, "/")
       just_name = name
       if (slash_pos > 0) then
          just_name = name(slash_pos+1:)
       end if
       call MAPL_GridCompAddSpec(gridcomp, ESMF_STATEINTENT_INTERNAL, 'sum_'//just_name, fill_value=0.0, itemtype=item_type, _RC)

       call MAPL_GridCompAddSpec(gridcomp, ESMF_STATEINTENT_INTERNAL, 'counts_'//just_name, fill_value=0.0, itemtype=item_type, _RC)

      _RETURN(_SUCCESS)
   end subroutine advertise_time_average_internal_fields

end module mapl_TimeAverage_mod
