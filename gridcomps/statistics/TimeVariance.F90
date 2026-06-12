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
      type(MAPL_SimpleAlarm) :: alarm
      type(esmf_Field)  :: f       ! input field
      type(esmf_Field)  :: var_f   ! output field
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
      module procedure new_TimeVariance
   end interface TimeVariance

contains

   function new_TimeVariance(unusable, f, var_f, alarm, algorithm, biased) result(stat)
      type(TimeVariance) :: stat
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(esmf_Field), intent(in) :: f
      type(esmf_Field), intent(inout) :: var_f
      type(MAPL_SimpleAlarm), intent(in) :: alarm
      integer(kind=kind(DEFAULT_ALGORITHM)), optional, intent(in) :: algorithm
      logical, optional, intent(in) :: biased

      stat%f     = f
      stat%var_f = var_f
      stat%alarm = alarm
      if (present(algorithm)) stat%algorithm = algorithm
      if (present(biased)) stat%biased_ = biased
      _UNUSED_DUMMY(unusable)

   end function new_TimeVariance

   subroutine destroy(this, rc)
      class(TimeVariance), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call esmf_FieldDestroy(this%var_f, _RC)

      _RETURN(_SUCCESS)
   end subroutine destroy

   subroutine reset(this, gridcomp, rc)
      class(TimeVariance), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Field) :: counts_f
      character(:), allocatable :: name
      integer(kind=ESMF_KIND_I4), pointer :: counts(:)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(this%f, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'counts_'//name, field=counts_f, _RC)

      call MAPL_AssignFptr(counts_f, counts, _RC)
      counts = 0

      call this%kernel%reset(gridcomp, this%f, _RC)

      _RETURN(_SUCCESS)
   end subroutine reset

   subroutine update(this, gridcomp, clock, rc)
      class(TimeVariance), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_Clock), intent(in) :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_TypeKind_Flag) :: typekind
      logical :: is_ringing
      type(esmf_Time) :: nextTime
      type(esmf_State) :: internal_state
      type(esmf_Field) :: counts_f
      character(:), allocatable :: name

      if(needs_initialization(this)) then
         call initialize(this, gridcomp, _RC)
      end if

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(this%f, short_name=name, typekind=typekind, _RC)
      call esmf_StateGet(internal_state, 'counts_'//name, field=counts_f, _RC)

      ! Variance: pass this%f as both x and y
      if (typekind == ESMF_TYPEKIND_R4) then
         call this%kernel%update_r4(gridcomp, this%f, this%f, counts_f, _RC)
      else if (typekind == ESMF_TYPEKIND_R8) then
         call this%kernel%update_r8(gridcomp, this%f, this%f, counts_f, _RC)
      end if

      call ESMF_ClockGetNextTime(clock, nextTime=nextTime, _RC)
      is_ringing = this%alarm%is_ringing(nextTime, _RC)
      _RETURN_UNLESS(is_ringing)

      call this%compute_result(gridcomp, _RC)
      call this%reset(gridcomp, _RC)

      _RETURN(_SUCCESS)
   end subroutine update

   subroutine compute_result(this, gridcomp, rc)
      class(TimeVariance), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_TypeKind_Flag) :: typekind
      type(esmf_State) :: internal_state
      type(esmf_Field) :: counts_f
      character(:), allocatable :: name

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(this%f, short_name=name, typekind=typekind, _RC)
      call esmf_StateGet(internal_state, 'counts_'//name, field=counts_f, _RC)

      ! Variance: pass this%f as both x and y
      if (typekind == ESMF_TYPEKIND_R4) then
         call this%kernel%compute_r4(gridcomp, this%f, this%f, counts_f, this%var_f, this%biased_, _RC)
      else if (typekind == ESMF_TYPEKIND_R8) then
         call this%kernel%compute_r8(gridcomp, this%f, this%f, counts_f, this%var_f, this%biased_, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine compute_result

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
   subroutine advertise_time_variance_internal_fields(gridcomp, name, rc)
      type(esmf_GridComp), intent(inout) :: gridcomp
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc

      integer :: status, slash_pos
      character(len=:), allocatable :: just_name
      type(WelfordCovarianceKernel) :: wk
      type(ShiftedCovarianceKernel) :: sk

      slash_pos = index(name, "/")
      just_name = name
      if (slash_pos > 0) just_name = name(slash_pos+1:)

      ! counts_ owned by Variance, common to both kernels
      call MAPL_GridCompAddSpec(gridcomp, ESMF_STATEINTENT_INTERNAL, 'counts_'//just_name, fill_value=0.0, _RC)

      ! Welford fields: mux_, muy_, c_
      call wk%advertise(gridcomp, just_name, _RC)

      ! Shifted fields: kx_, ky_, ex_, ey_, exy_
      call sk%advertise(gridcomp, just_name, _RC)

      _RETURN(_SUCCESS)
   end subroutine advertise_time_variance_internal_fields

   subroutine initialize(stat, gridcomp, rc)
      class(TimeVariance), intent(inout) :: stat
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc
      integer :: status
      type(esmf_Field) :: f, var_f, counts_f
      type(esmf_State) :: internal_state
      type(esmf_Geom), allocatable :: geom
      type(MAPL_UngriddedDims) :: ungridded_dims
      character(:), allocatable :: units, name
      type(esmf_TypeKind_Flag) :: typekind
      class(VerticalGrid), pointer :: vertical_grid
      type(MAPL_VerticalStaggerLoc) :: vstagger

      f = stat%f
      var_f = stat%var_f

      select case (stat%algorithm)
      case (WELFORD)
         allocate(WelfordCovarianceKernel :: stat%kernel)
      case (SHIFTED)
         allocate(ShiftedCovarianceKernel :: stat%kernel)
      case default
         _FAIL("Unrecognized Variance algorithm")
      end select

      ! Realize internal state fields
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

      ! Realize var_f output field
      call mapl_FieldSet(var_f, &
           geom=geom, &
           ungridded_dims=ungridded_dims, &
           units=units, &
           typekind=typekind, &
           vgrid=vertical_grid, &
           vert_staggerloc=vstagger, &
           standard_name='foo', &
           has_deferred_aspects=.false., &
           _RC)

      ! Realize counts_ internal field
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

      ! Variance passes f as both x and y: Cov(f, f) = Var(f)
      call stat%kernel%initialize(gridcomp, f, f, counts_f, _RC)

      _RETURN(_SUCCESS)

   end subroutine initialize

   logical function needs_initialization(stat)
      class(TimeVariance), intent(in) :: stat

      needs_initialization = .not. allocated(stat%kernel)

   end function needs_initialization

end module mapl_TimeVariance_mod
