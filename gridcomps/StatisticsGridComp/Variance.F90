#include "MAPL.h"

module mapl3g_Variance

   use mapl3g_AbstractTimeStatistic
   use mapl3g_AbstractCovarianceKernel
   use mapl3g_ShiftedCovarianceKernel
   use mapl3g_WelfordCovarianceKernel
   use MAPL
   use ESMF
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer
   use mapl3g_SimpleAlarm, only: SimpleAlarm

   implicit none(type, external)
   private

   public :: Variance
   public :: advertise_variance_internal_fields
   public :: WELFORD, SHIFTED

   enum, bind(c)
      enumerator :: VARIANCE_ALGORITHM = 0
      enumerator :: WELFORD
      enumerator :: SHIFTED
   end enum

   integer(kind=kind(VARIANCE_ALGORITHM)), parameter :: DEFAULT_ALGORITHM = WELFORD

   type, extends(AbstractTimeStatistic) :: Variance
      private
      type(SimpleAlarm) :: alarm
      type(esmf_Field)  :: f       ! input field
      type(esmf_Field)  :: var_f   ! output field
      logical           :: biased_ = .false.
      class(AbstractCovarianceKernel), allocatable :: kernel
   contains
      procedure :: destroy
      procedure :: reset
      procedure :: update
      procedure :: compute_result
      procedure :: add_to_state
      procedure :: get_alarm
   end type Variance

   interface Variance
      module procedure new_Variance
   end interface Variance

contains

   function new_Variance(unusable, gridcomp, f, var_f, alarm, algorithm, biased, rc) result(stat)
      type(Variance) :: stat
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_Field), intent(in) :: f
      type(esmf_Field), intent(inout) :: var_f
      type(SimpleAlarm), intent(in) :: alarm
      integer(kind=kind(VARIANCE_ALGORITHM)), optional, intent(in) :: algorithm
      logical, optional, intent(in) :: biased
      integer, optional, intent(out) :: rc

      integer :: status
      integer(kind=kind(VARIANCE_ALGORITHM)) :: algorithm_
      type(esmf_State) :: internal_state
      type(esmf_Geom), allocatable :: geom
      type(UngriddedDims) :: ungridded_dims
      character(:), allocatable :: units, name
      type(esmf_TypeKind_Flag) :: typekind
      class(VerticalGrid), pointer :: vertical_grid
      type(VerticalStaggerLoc) :: vstagger
      type(esmf_Field) :: counts_f

      stat%f     = f
      stat%var_f = var_f
      stat%alarm = alarm
      if (present(biased)) stat%biased_ = biased

      algorithm_ = DEFAULT_ALGORITHM
      if (present(algorithm)) algorithm_ = algorithm

      select case (algorithm_)
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

      _UNUSED_DUMMY(unusable)
      _RETURN(_SUCCESS)
   end function new_Variance

   subroutine destroy(this, rc)
      class(Variance), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call esmf_FieldDestroy(this%var_f, _RC)

      _RETURN(_SUCCESS)
   end subroutine destroy

   subroutine reset(this, gridcomp, rc)
      class(Variance), intent(inout) :: this
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
      class(Variance), intent(inout) :: this
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
      class(Variance), intent(inout) :: this
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
      class(Variance), intent(inout) :: this
      type(esmf_State), intent(inout) :: state
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(state)
      _RETURN(_SUCCESS)
   end subroutine add_to_state

   function get_alarm(this) result(alarm)
      class(Variance), intent(in) :: this
      type(SimpleAlarm) :: alarm

      alarm = this%alarm
   end function get_alarm

   ! Advertise union of all internal fields for both algorithms (Option B).
   subroutine advertise_variance_internal_fields(gridcomp, name, rc)
      type(esmf_GridComp), intent(inout) :: gridcomp
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc

      integer :: status, slash_pos
      type(VariableSpec) :: varspec
      character(len=:), allocatable :: just_name
      type(WelfordCovarianceKernel) :: wk
      type(ShiftedCovarianceKernel) :: sk

      slash_pos = index(name, "/")
      just_name = name
      if (slash_pos > 0) just_name = name(slash_pos+1:)

      ! counts_ owned by Variance, common to both kernels
      varspec = make_VariableSpec(ESMF_STATEINTENT_INTERNAL, 'counts_'//just_name, fill_value=0.0, _RC)
      call MAPL_GridCompAddVarSpec(gridcomp, varspec, _RC)

      ! Welford fields: mux_, muy_, c_
      call wk%advertise(gridcomp, just_name, _RC)

      ! Shifted fields: kx_, ky_, ex_, ey_, exy_
      call sk%advertise(gridcomp, just_name, _RC)

      _RETURN(_SUCCESS)
   end subroutine advertise_variance_internal_fields

end module mapl3g_Variance
