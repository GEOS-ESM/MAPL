#include "MAPL.h"

module mapl_TimeVariance_mod

   use mapl_AbstractTimeStatistic_mod
   use mapl_AbstractCovarianceKernel_mod
   use mapl_ShiftedCovarianceKernel_mod
   use mapl_WelfordCovarianceKernel_mod
   use MAPL
   use ESMF

   implicit none(type, external)
   private

   public :: TimeVariance
   public :: advertise_time_variance_internal_fields
   public :: WELFORD, SHIFTED

   enum, bind(c)
      enumerator :: WELFORD
      enumerator :: SHIFTED
   end enum

   integer(kind=kind(WELFORD)), parameter :: DEFAULT_ALGORITHM = WELFORD

   type, extends(AbstractTimeStatistic) :: TimeVariance
      private
      logical           :: is_bundle = .false.
      type(MAPL_SimpleAlarm) :: alarm
      type(esmf_Field)  :: f       ! input field
      type(esmf_Field)  :: var_f   ! output field
      type(ESMF_FieldBundle) :: b     ! input bundle
      type(ESMF_FieldBundle) :: var_b ! output bundle
      logical           :: biased_ = .false.
      class(AbstractCovarianceKernel), allocatable :: kernel
      integer(kind=kind(DEFAULT_ALGORITHM)) :: algorithm = DEFAULT_ALGORITHM
   contains
      procedure :: destroy
      procedure :: reset
      procedure :: update
      procedure :: compute_result
      procedure :: add_to_state
      procedure :: get_alarm
   end type TimeVariance

   interface TimeVariance
      module procedure new_TimeVariance_field
      module procedure new_TimeVariance_fieldbundle
   end interface TimeVariance

contains

   function new_TimeVariance_field(unusable, gridcomp, f, var_f, alarm, algorithm, biased, rc) result(stat)
      type(TimeVariance) :: stat
      class(mapl_KeywordEnforcer), optional, intent(in) :: unusable
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_Field), intent(in) :: f
      type(esmf_Field), intent(inout) :: var_f
      type(MAPL_SimpleAlarm), intent(in) :: alarm
      integer(kind=kind(DEFAULT_ALGORITHM)), optional, intent(in) :: algorithm
      logical, optional, intent(in) :: biased
      integer, optional, intent(out) :: rc

      integer :: status

      stat%is_bundle = .false.
      stat%f     = f
      stat%var_f = var_f
      stat%alarm = alarm
      if (present(algorithm)) stat%algorithm = algorithm
      if (present(biased)) stat%biased_ = biased

      call initialize(stat, gridcomp, _RC)

      _UNUSED_DUMMY(unusable)
      _RETURN(_SUCCESS)

   end function new_TimeVariance_field

   function new_TimeVariance_fieldbundle(unusable, gridcomp, b, var_b, alarm, algorithm, biased, rc) result(stat)
      type(TimeVariance) :: stat
      class(mapl_KeywordEnforcer), optional, intent(in) :: unusable
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_FieldBundle), intent(in) :: b
      type(esmf_FieldBundle), intent(inout) :: var_b
      type(MAPL_SimpleAlarm), intent(in) :: alarm
      integer(kind=kind(DEFAULT_ALGORITHM)), optional, intent(in) :: algorithm
      logical, optional, intent(in) :: biased
      integer, optional, intent(out) :: rc

      integer :: status

      stat%is_bundle = .true.
      stat%b     = b
      stat%var_b = var_b
      stat%alarm = alarm
      if (present(algorithm)) stat%algorithm = algorithm
      if (present(biased)) stat%biased_ = biased

      call initialize(stat, gridcomp, _RC)

      _UNUSED_DUMMY(unusable)
      _RETURN(_SUCCESS)

   end function new_TimeVariance_fieldbundle

   subroutine destroy(this, rc)
      class(TimeVariance), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      if (this%is_bundle) then
         call MAPL_FieldBundleDestroy(this%var_b, _RC)
      else
         call esmf_FieldDestroy(this%var_f, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine destroy

   ! Resolves the kernel's internal fields (in the fixed order returned by
   ! kernel%get_internal_field_prefixes()) for either the scalar field case
   ! (member_index is ignored) or, in bundle mode, for the member_index-th
   ! member of each internal-state bundle.
   function resolve_internal_fields(this, gridcomp, member_index, rc) result(fields)
      class(TimeVariance), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, intent(in) :: member_index
      integer, optional, intent(out) :: rc
      type(esmf_Field), allocatable :: fields(:)

      integer :: status, k
      type(esmf_State) :: internal_state
      character(len=16), allocatable :: prefixes(:)
      character(:), allocatable :: name
      type(esmf_FieldBundle) :: tmp_b
      type(esmf_Field), allocatable :: tmp_list(:)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      prefixes = this%kernel%get_internal_field_prefixes()

      if (this%is_bundle) then
         call mapl_FieldBundleGet(this%b, short_name=name, _RC)
      else
         call mapl_FieldGet(this%f, short_name=name, _RC)
      end if

      allocate(fields(size(prefixes)))
      do k = 1, size(prefixes)
         if (this%is_bundle) then
            call esmf_StateGet(internal_state, trim(prefixes(k))//name, fieldbundle=tmp_b, _RC)
            call mapl_FieldBundleGet(tmp_b, fieldList=tmp_list, _RC)
            fields(k) = tmp_list(member_index)
         else
            call esmf_StateGet(internal_state, trim(prefixes(k))//name, field=fields(k), _RC)
         end if
      end do

      _RETURN(_SUCCESS)
   end function resolve_internal_fields

   subroutine reset(this, gridcomp, rc)
      class(TimeVariance), intent(inout) :: this
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
      class(TimeVariance), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Field) :: counts_f
      character(:), allocatable :: name
      integer(kind=ESMF_KIND_I4), pointer :: counts(:)
      type(esmf_Field), allocatable :: internal_fields(:)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(this%f, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'counts_'//name, field=counts_f, _RC)

      call MAPL_AssignFptr(counts_f, counts, _RC)
      counts = 0

      internal_fields = resolve_internal_fields(this, gridcomp, 1, _RC)
      call this%kernel%reset(gridcomp, internal_fields, _RC)

      _RETURN(_SUCCESS)
   end subroutine reset_field

   subroutine reset_bundle(this, gridcomp, rc)
      class(TimeVariance), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status, i
      type(esmf_State) :: internal_state
      type(esmf_FieldBundle) :: counts_b
      character(:), allocatable :: name
      integer(kind=ESMF_KIND_I4), pointer :: counts(:)
      type(esmf_Field), allocatable :: counts_fieldlist(:)
      type(esmf_Field), allocatable :: internal_fields(:)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldBundleGet(this%b, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'counts_'//name, fieldbundle=counts_b, _RC)
      call mapl_FieldBundleGet(counts_b, fieldList=counts_fieldlist, _RC)

      do i = 1, size(counts_fieldlist)
         call MAPL_AssignFptr(counts_fieldlist(i), counts, _RC)
         counts = 0

         internal_fields = resolve_internal_fields(this, gridcomp, i, _RC)
         call this%kernel%reset(gridcomp, internal_fields, _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine reset_bundle

   subroutine update(this, gridcomp, clock, rc)
      class(TimeVariance), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_Clock), intent(in) :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_TypeKind_Flag) :: typekind
      logical :: is_ringing
      type(esmf_Time) :: nextTime

      if (needs_initialization(this)) then
         call initialize(this, gridcomp, _RC)
      end if

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
            call update_field_r4(this, gridcomp, _RC)
         else if (typekind == ESMF_TYPEKIND_R8) then
            call update_field_r8(this, gridcomp, _RC)
         end if
      end if

      call ESMF_ClockGetNextTime(clock, nextTime=nextTime, _RC)
      is_ringing = this%alarm%is_ringing(nextTime, _RC)
      _RETURN_UNLESS(is_ringing)

      call this%compute_result(gridcomp, _RC)
      call this%reset(gridcomp, _RC)

      _RETURN(_SUCCESS)
   end subroutine update

   subroutine update_field_r4(this, gridcomp, rc)
      class(TimeVariance), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Field) :: counts_f
      character(:), allocatable :: name
      type(esmf_Field), allocatable :: internal_fields(:)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(this%f, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'counts_'//name, field=counts_f, _RC)

      internal_fields = resolve_internal_fields(this, gridcomp, 1, _RC)
      call this%kernel%update_r4(gridcomp, this%f, this%f, counts_f, internal_fields, _RC)

      _RETURN(_SUCCESS)
   end subroutine update_field_r4

   subroutine update_field_r8(this, gridcomp, rc)
      class(TimeVariance), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Field) :: counts_f
      character(:), allocatable :: name
      type(esmf_Field), allocatable :: internal_fields(:)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(this%f, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'counts_'//name, field=counts_f, _RC)

      internal_fields = resolve_internal_fields(this, gridcomp, 1, _RC)
      call this%kernel%update_r8(gridcomp, this%f, this%f, counts_f, internal_fields, _RC)

      _RETURN(_SUCCESS)
   end subroutine update_field_r8

   subroutine update_bundle_r4(this, gridcomp, rc)
      class(TimeVariance), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, intent(out) :: rc

      integer :: status, i
      type(esmf_State) :: internal_state
      type(esmf_FieldBundle) :: counts_b
      character(:), allocatable :: name
      type(esmf_Field), allocatable :: fieldlist(:), counts_fieldlist(:)
      type(esmf_Field), allocatable :: internal_fields(:)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldBundleGet(this%b, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'counts_'//name, fieldbundle=counts_b, _RC)

      call mapl_FieldBundleGet(this%b, fieldList=fieldlist, _RC)
      call mapl_FieldBundleGet(counts_b, fieldList=counts_fieldlist, _RC)

      do i = 1, size(fieldlist)
         internal_fields = resolve_internal_fields(this, gridcomp, i, _RC)
         call this%kernel%update_r4(gridcomp, fieldlist(i), fieldlist(i), counts_fieldlist(i), internal_fields, _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine update_bundle_r4

   subroutine update_bundle_r8(this, gridcomp, rc)
      class(TimeVariance), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, intent(out) :: rc

      integer :: status, i
      type(esmf_State) :: internal_state
      type(esmf_FieldBundle) :: counts_b
      character(:), allocatable :: name
      type(esmf_Field), allocatable :: fieldlist(:), counts_fieldlist(:)
      type(esmf_Field), allocatable :: internal_fields(:)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldBundleGet(this%b, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'counts_'//name, fieldbundle=counts_b, _RC)

      call mapl_FieldBundleGet(this%b, fieldList=fieldlist, _RC)
      call mapl_FieldBundleGet(counts_b, fieldList=counts_fieldlist, _RC)

      do i = 1, size(fieldlist)
         internal_fields = resolve_internal_fields(this, gridcomp, i, _RC)
         call this%kernel%update_r8(gridcomp, fieldlist(i), fieldlist(i), counts_fieldlist(i), internal_fields, _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine update_bundle_r8

   subroutine compute_result(this, gridcomp, rc)
      class(TimeVariance), intent(inout) :: this
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
            call compute_result_field_r4(this, gridcomp, _RC)
         else if (typekind == ESMF_TYPEKIND_R8) then
            call compute_result_field_r8(this, gridcomp, _RC)
         end if
      end if

      _RETURN(_SUCCESS)
   end subroutine compute_result

   subroutine compute_result_field_r4(this, gridcomp, rc)
      class(TimeVariance), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Field) :: counts_f
      character(:), allocatable :: name
      type(esmf_Field), allocatable :: internal_fields(:)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(this%f, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'counts_'//name, field=counts_f, _RC)

      internal_fields = resolve_internal_fields(this, gridcomp, 1, _RC)
      call this%kernel%compute_r4(gridcomp, this%f, this%f, counts_f, this%var_f, internal_fields, this%biased_, _RC)

      _RETURN(_SUCCESS)
   end subroutine compute_result_field_r4

   subroutine compute_result_field_r8(this, gridcomp, rc)
      class(TimeVariance), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Field) :: counts_f
      character(:), allocatable :: name
      type(esmf_Field), allocatable :: internal_fields(:)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(this%f, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'counts_'//name, field=counts_f, _RC)

      internal_fields = resolve_internal_fields(this, gridcomp, 1, _RC)
      call this%kernel%compute_r8(gridcomp, this%f, this%f, counts_f, this%var_f, internal_fields, this%biased_, _RC)

      _RETURN(_SUCCESS)
   end subroutine compute_result_field_r8

   subroutine compute_result_bundle_r4(this, gridcomp, rc)
      class(TimeVariance), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status, i
      type(esmf_State) :: internal_state
      type(esmf_FieldBundle) :: counts_b
      character(:), allocatable :: name
      type(esmf_Field), allocatable :: fieldlist(:), var_fieldlist(:), counts_fieldlist(:)
      type(esmf_Field), allocatable :: internal_fields(:)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldBundleGet(this%b, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'counts_'//name, fieldbundle=counts_b, _RC)

      call mapl_FieldBundleGet(this%b, fieldList=fieldlist, _RC)
      call mapl_FieldBundleGet(this%var_b, fieldList=var_fieldlist, _RC)
      call mapl_FieldBundleGet(counts_b, fieldList=counts_fieldlist, _RC)

      do i = 1, size(fieldlist)
         internal_fields = resolve_internal_fields(this, gridcomp, i, _RC)
         call this%kernel%compute_r4(gridcomp, fieldlist(i), fieldlist(i), counts_fieldlist(i), &
              var_fieldlist(i), internal_fields, this%biased_, _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine compute_result_bundle_r4

   subroutine compute_result_bundle_r8(this, gridcomp, rc)
      class(TimeVariance), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status, i
      type(esmf_State) :: internal_state
      type(esmf_FieldBundle) :: counts_b
      character(:), allocatable :: name
      type(esmf_Field), allocatable :: fieldlist(:), var_fieldlist(:), counts_fieldlist(:)
      type(esmf_Field), allocatable :: internal_fields(:)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldBundleGet(this%b, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'counts_'//name, fieldbundle=counts_b, _RC)

      call mapl_FieldBundleGet(this%b, fieldList=fieldlist, _RC)
      call mapl_FieldBundleGet(this%var_b, fieldList=var_fieldlist, _RC)
      call mapl_FieldBundleGet(counts_b, fieldList=counts_fieldlist, _RC)

      do i = 1, size(fieldlist)
         internal_fields = resolve_internal_fields(this, gridcomp, i, _RC)
         call this%kernel%compute_r8(gridcomp, fieldlist(i), fieldlist(i), counts_fieldlist(i), &
              var_fieldlist(i), internal_fields, this%biased_, _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine compute_result_bundle_r8

   subroutine add_to_state(this, state, rc)
      class(TimeVariance), intent(inout) :: this
      type(esmf_State), intent(inout) :: state
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(state)
      _RETURN(_SUCCESS)
   end subroutine add_to_state

   function get_alarm(this) result(alarm)
      class(TimeVariance), intent(in) :: this
      type(MAPL_SimpleAlarm) :: alarm

      alarm = this%alarm
   end function get_alarm

   ! Advertise union of all internal fields for both algorithms (Option B).
   subroutine advertise_time_variance_internal_fields(gridcomp, name, item_type, rc)
      type(esmf_GridComp), intent(inout) :: gridcomp
      character(*), intent(in) :: name
      type(ESMF_StateItem_Flag), intent(in) :: item_type
      integer, optional, intent(out) :: rc

      integer :: status, slash_pos
      character(len=:), allocatable :: just_name
      type(WelfordCovarianceKernel) :: wk
      type(ShiftedCovarianceKernel) :: sk

      slash_pos = index(name, "/")
      just_name = name
      if (slash_pos > 0) just_name = name(slash_pos+1:)

      ! counts_ owned by Variance, common to both kernels
      call MAPL_GridCompAddSpec(gridcomp, ESMF_STATEINTENT_INTERNAL, 'counts_'//just_name, fill_value=0.0, itemtype=item_type, _RC)

      ! Welford fields: mux_, muy_, c_
      call wk%advertise(gridcomp, just_name, item_type, _RC)

      ! Shifted fields: kx_, ky_, ex_, ey_, exy_
      call sk%advertise(gridcomp, just_name, item_type, _RC)

      _RETURN(_SUCCESS)
   end subroutine advertise_time_variance_internal_fields

   ! Propagates geom/ungridded_dims/units/typekind/vgrid/vert_staggerloc metadata
   ! from an input field onto the variance-output field and the counts_ field,
   ! shared between field-mode initialize and each member of bundle-mode initialize.
   subroutine propagate_metadata(f_i, var_f_i, counts_f_i, rc)
      type(esmf_Field), intent(inout) :: f_i
      type(esmf_Field), intent(inout) :: var_f_i
      type(esmf_Field), intent(inout) :: counts_f_i
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_Geom), allocatable :: geom
      type(MAPL_UngriddedDims) :: ungridded_dims
      character(:), allocatable :: units
      type(esmf_TypeKind_Flag) :: typekind
      type(MAPL_VerticalStaggerLoc) :: vstagger
      class(mapl_VerticalGrid), pointer :: vertical_grid

      call mapl_FieldGet(f_i, &
           geom=geom, &
           ungridded_dims=ungridded_dims, &
           units=units, &
           typekind=typekind, &
           vgrid=vertical_grid, &
           vert_staggerloc=vstagger, &
           _RC)

      call mapl_FieldSet(var_f_i, &
           geom=geom, &
           ungridded_dims=ungridded_dims, &
           units=units, &
           typekind=typekind, &
           vgrid=vertical_grid, &
           vert_staggerloc=vstagger, &
           standard_name='foo', &
           _RC)

      call mapl_FieldSet(counts_f_i, &
           geom=geom, &
           ungridded_dims=ungridded_dims, &
           units='1', &
           typekind=ESMF_TYPEKIND_I4, &
           vgrid=vertical_grid, &
           vert_staggerloc=vstagger, &
           _RC)

      _RETURN(_SUCCESS)
   end subroutine propagate_metadata

   subroutine initialize(stat, gridcomp, rc)
      class(TimeVariance), intent(inout) :: stat
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc
      integer :: status

      select case (stat%algorithm)
      case (WELFORD)
         allocate(WelfordCovarianceKernel :: stat%kernel)
      case (SHIFTED)
         allocate(ShiftedCovarianceKernel :: stat%kernel)
      case default
         _FAIL("Unrecognized Variance algorithm")
      end select

      if (stat%is_bundle) then
         call initialize_bundle(stat, gridcomp, _RC)
      else
         call initialize_field(stat, gridcomp, _RC)
      end if

      _RETURN(_SUCCESS)

   end subroutine initialize

   subroutine initialize_field(stat, gridcomp, rc)
      class(TimeVariance), intent(inout) :: stat
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc
      integer :: status
      type(esmf_Field) :: counts_f
      type(esmf_State) :: internal_state
      character(:), allocatable :: name
      type(esmf_Field), allocatable :: internal_fields(:)

      ! Realize internal state fields
      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(stat%f, short_name=name, _RC)

      ! Realize counts_ internal field
      call esmf_StateGet(internal_state, 'counts_'//name, field=counts_f, _RC)

      call propagate_metadata(stat%f, stat%var_f, counts_f, _RC)

      ! Variance passes f as both x and y: Cov(f, f) = Var(f)
      internal_fields = resolve_internal_fields(stat, gridcomp, 1, _RC)
      call stat%kernel%initialize(gridcomp, stat%f, stat%f, counts_f, internal_fields, _RC)

      _RETURN(_SUCCESS)
   end subroutine initialize_field

   subroutine initialize_bundle(stat, gridcomp, rc)
      class(TimeVariance), intent(inout) :: stat
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc
      integer :: status, k
      type(esmf_FieldBundle) :: counts_b, tmp_b
      type(esmf_State) :: internal_state
      character(:), allocatable :: name
      character(len=16), allocatable :: prefixes(:)
      type(esmf_Geom), allocatable :: geom
      type(MAPL_UngriddedDims) :: ungridded_dims
      character(:), allocatable :: units
      type(esmf_TypeKind_Flag) :: typekind
      type(MAPL_VerticalStaggerLoc) :: vstagger
      class(mapl_VerticalGrid), pointer :: vertical_grid

      ! Bundle mode: member fields of stat%b/stat%var_b/the internal-state
      ! bundles do not exist yet during realize_provided (they appear later
      ! via the framework's structure-mirroring mechanism), so -- unlike
      ! field mode -- we cannot loop over per-member fieldLists here. Instead,
      ! mirror TimeAverage's bundle-constructor pattern: propagate metadata at
      ! the whole-bundle level via mapl_fieldbundleset. Per-member math is
      ! deferred to update_bundle_r4/r8 and compute_result_bundle_r4/r8 (via
      ! resolve_internal_fields), which run later during RUN once bundle
      ! members exist.
      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldBundleGet(stat%b, short_name=name, _RC)
      call mapl_FieldBundleGet(stat%b, &
           geom=geom, &
           ungridded_dims=ungridded_dims, &
           units=units, &
           typekind=typekind, &
           vgrid=vertical_grid, &
           vert_staggerloc=vstagger, &
           _RC)

      call mapl_fieldbundleset(stat%var_b, &
           geom=geom, &
           ungridded_dims=ungridded_dims, &
           units=units, &
           typekind=typekind, &
           vgrid=vertical_grid, &
           vert_staggerloc=vstagger, &
           standard_name='foo', &
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

      prefixes = stat%kernel%get_internal_field_prefixes()
      do k = 1, size(prefixes)
         call esmf_StateGet(internal_state, trim(prefixes(k))//name, fieldbundle=tmp_b, _RC)
         call mapl_fieldbundleset(tmp_b, &
              geom=geom, &
              ungridded_dims=ungridded_dims, &
              units=units, &
              typekind=typekind, &
              vgrid=vertical_grid, &
              vert_staggerloc=vstagger, &
              _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine initialize_bundle

   logical function needs_initialization(stat)
      class(TimeVariance), intent(in) :: stat

      needs_initialization = .not. allocated(stat%kernel)

   end function needs_initialization

end module mapl_TimeVariance_mod
