#include "MAPL.h"

!------------------------------------------------------------------------------
! mapl_TurbulentMixingStep_mod: the per-time-step dispatch logic a
! turbulent_mixing Invoker runs on one realized callback ESMF_State - no
! graph, no ESMF_GridComp, no collection iterator. Given cb_state (built
! the way a real Handler would, e.g. mapl_ConfigurableCallbackHandler_mod
! in tests), run_turbulent_mixing_step reads mix_like and
! bottom_boundary, checks the MF9 unused-argument case itself (the
! framework does not), then advances mixed_quantity one implicit step
! with mapl_ImplicitColumnDiffusion_mod, column by column.
!
! One ImplicitColumnDiffusion instance is built per distinct mix_like
! value and reused across calls (its own doc comment: "one instance
! serves every column of the class"); a cache entry also records the
! number of layers it was built for, since a State whose vertical grid
! differs from an already-cached class's forces a rebuild.
!
! The kappa profile per class, the neumann flux, the dirichlet pin value
! and the air density used to turn dry_deposition_velocity into a Robin
! coefficient are all test-tool constants, documented where declared -
! not a production contract, no interface argument or attribute carries
! them.
!------------------------------------------------------------------------------
module mapl_TurbulentMixingStep_mod
   use mapl_ImplicitColumnDiffusion_mod, only: ImplicitColumnDiffusion, bottom_boundary_from_name
   use mapl_ImplicitColumnDiffusion_mod, only: BOTTOM_NEUMANN_HOMOGENEOUS, BOTTOM_NEUMANN
   use mapl_ImplicitColumnDiffusion_mod, only: BOTTOM_ROBIN, BOTTOM_DIRICHLET
   use mapl_TurbulentMixingConstants_mod, only: TURBULENT_MIXING_ARGUMENT_MIXED_QUANTITY
   use mapl_TurbulentMixingConstants_mod, only: TURBULENT_MIXING_ARGUMENT_AIR_MASS
   use mapl_TurbulentMixingConstants_mod, only: TURBULENT_MIXING_ARGUMENT_DRY_DEPOSITION_VELOCITY
   use mapl_TurbulentMixingConstants_mod, only: TURBULENT_MIXING_ATTRIBUTE_MIX_LIKE
   use mapl_TurbulentMixingConstants_mod, only: TURBULENT_MIXING_ATTRIBUTE_BOTTOM_BOUNDARY
   use mapl_TurbulentMixingConstants_mod, only: TURBULENT_MIXING_MIX_LIKE_HEAT
   use mapl_TurbulentMixingConstants_mod, only: TURBULENT_MIXING_MIX_LIKE_MOISTURE
   use mapl_TurbulentMixingConstants_mod, only: TURBULENT_MIXING_MIX_LIKE_MOMENTUM
   use mapl_TurbulentMixingConstants_mod, only: TURBULENT_MIXING_BOTTOM_BOUNDARY_ROBIN
   use mapl_CallbackAttributeAccess_mod, only: MAPL_CallbackGetAttribute
   use mapl_KeywordEnforcer_mod, only: KeywordEnforcer
   use esmf, only: ESMF_State, ESMF_StateGet, ESMF_StateItem_Flag, ESMF_STATEITEM_NOTFOUND
   use esmf, only: ESMF_Field, ESMF_FieldGet, ESMF_KIND_R8
   use esmf, only: operator(==), operator(/=)
   use, intrinsic :: iso_fortran_env, only: REAL64
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: run_turbulent_mixing_step

   integer, parameter :: ATTRIBUTE_VALUE_LEN = 64

   ! Per-mix_like-class diffusion coefficients (kg m-2 s-1, constant down
   ! the column): test-tool numbers only, ordered so momentum mixes
   ! fastest and moisture slowest, a plausible boundary-layer ranking,
   ! not a measured value. Kept small and exact in binary so
   ! Test_TurbulentMixingStep.pf can hand-check the resulting column
   ! solves, the same style Test_ImplicitColumnDiffusion.pf itself uses.
   real(REAL64), parameter :: KAPPA_HEAT = 1._REAL64
   real(REAL64), parameter :: KAPPA_MOISTURE = 0.5_REAL64
   real(REAL64), parameter :: KAPPA_MOMENTUM = 2._REAL64

   ! Fixed test-tool surface forcings, one neumann flux and one dirichlet
   ! pin value per mix_like class (neither is a physical input; both are
   ! TestInvoker-internal numbers). Test_TurbulentMixingStep.pf exercises
   ! mix_like heat only, so the heat pair is the one its hand-checked
   ! arithmetic depends on; the moisture and momentum pairs are
   ! unexercised but documented here for completeness.
   real(REAL64), parameter :: NEUMANN_SURFACE_FLUX_HEAT = 3._REAL64
   real(REAL64), parameter :: NEUMANN_SURFACE_FLUX_MOISTURE = 1._REAL64
   real(REAL64), parameter :: NEUMANN_SURFACE_FLUX_MOMENTUM = 0.5_REAL64
   real(REAL64), parameter :: DIRICHLET_SURFACE_VALUE_HEAT = 2._REAL64
   real(REAL64), parameter :: DIRICHLET_SURFACE_VALUE_MOISTURE = 1._REAL64
   real(REAL64), parameter :: DIRICHLET_SURFACE_VALUE_MOMENTUM = 0._REAL64

   ! Air density used to turn dry_deposition_velocity into robin's linear
   ! coefficient (rho * v_d); no argument supplies a physical air density
   ! here, so this is a single fixed test constant, the same value
   ! Test_ImplicitColumnDiffusion.pf's own M9 scenario uses.
   real(REAL64), parameter :: TEST_SURFACE_AIR_DENSITY = 1.2_REAL64

   ! One cached operator per (mix_like, num_layers) pair actually seen.
   type :: OperatorCacheEntry
      character(:), allocatable :: mix_like
      integer :: num_layers = -1
      type(ImplicitColumnDiffusion) :: op
   end type OperatorCacheEntry

   type(OperatorCacheEntry), allocatable, save :: operator_cache(:)

contains

   ! Advances cb_state's mixed_quantity one implicit step of length dt.
   ! realized_flux(n), when present, is the flux realized_surface_upward_flux
   ! of mapl_ImplicitColumnDiffusion_mod%step reports for column n, with
   ! n = (j-1)*ni + i over mixed_quantity's horizontal extent (ni, nj) -
   ! an Invoker-owned diagnostic with no interface-level argument or
   ! attribute behind it (nothing to "put" back through the callback
   ! mechanism itself).
   subroutine run_turbulent_mixing_step(cb_state, dt, unusable, realized_flux, rc)
      type(ESMF_State), intent(in) :: cb_state
      real(REAL64), intent(in) :: dt
      class(KeywordEnforcer), optional, intent(in) :: unusable
      real(REAL64), allocatable, optional, intent(out) :: realized_flux(:)
      integer, optional, intent(out) :: rc

      character(ATTRIBUTE_VALUE_LEN) :: mix_like, bottom_boundary_name
      type(ESMF_StateItem_Flag) :: item_type
      logical :: dry_deposition_velocity_present
      integer :: bottom_boundary, num_layers, cache_index
      type(ESMF_Field) :: mixed_quantity_field, air_mass_field, dry_deposition_velocity_field
      real(kind=ESMF_KIND_R8), pointer :: phi3d(:,:,:), mass3d(:,:,:), dry_deposition_velocity2d(:,:)
      real(REAL64) :: neumann_flux_value, dirichlet_pin_value
      character(:), allocatable :: msg
      integer :: i, j, ni, nj, status

      nullify(dry_deposition_velocity2d)
      mix_like = ''
      call MAPL_CallbackGetAttribute(cb_state, TURBULENT_MIXING_ATTRIBUTE_MIX_LIKE, mix_like, &
           member=TURBULENT_MIXING_ARGUMENT_MIXED_QUANTITY, _RC)
      bottom_boundary_name = ''
      call MAPL_CallbackGetAttribute(cb_state, TURBULENT_MIXING_ATTRIBUTE_BOTTOM_BOUNDARY, &
           bottom_boundary_name, member=TURBULENT_MIXING_ARGUMENT_MIXED_QUANTITY, _RC)

      ! MF9: the framework does not check argument use; an Invoker that binds
      ! dry_deposition_velocity without a bottom_boundary that uses it must
      ! reject the State itself, before the solver ever runs.
      call ESMF_StateGet(cb_state, itemName=TURBULENT_MIXING_ARGUMENT_DRY_DEPOSITION_VELOCITY, &
           itemType=item_type, _RC)
      dry_deposition_velocity_present = item_type /= ESMF_STATEITEM_NOTFOUND
      if (dry_deposition_velocity_present .and. &
           trim(bottom_boundary_name) /= TURBULENT_MIXING_BOTTOM_BOUNDARY_ROBIN) then
         msg = "run_turbulent_mixing_step: dry_deposition_velocity is bound but bottom_boundary is '" // &
              trim(bottom_boundary_name) // "', not robin; it would go unused"
         _FAIL(msg)
      end if

      call bottom_boundary_from_name(trim(bottom_boundary_name), bottom_boundary, _RC)

      call ESMF_StateGet(cb_state, itemName=TURBULENT_MIXING_ARGUMENT_MIXED_QUANTITY, &
           field=mixed_quantity_field, _RC)
      call ESMF_StateGet(cb_state, itemName=TURBULENT_MIXING_ARGUMENT_AIR_MASS, &
           field=air_mass_field, _RC)
      call ESMF_FieldGet(mixed_quantity_field, farrayPtr=phi3d, _RC)
      call ESMF_FieldGet(air_mass_field, farrayPtr=mass3d, _RC)
      if (dry_deposition_velocity_present) then
         call ESMF_StateGet(cb_state, itemName=TURBULENT_MIXING_ARGUMENT_DRY_DEPOSITION_VELOCITY, &
              field=dry_deposition_velocity_field, _RC)
         call ESMF_FieldGet(dry_deposition_velocity_field, farrayPtr=dry_deposition_velocity2d, _RC)
      end if

      ni = size(phi3d, 1)
      nj = size(phi3d, 2)
      num_layers = size(phi3d, 3)
      cache_index = get_or_build_operator(trim(mix_like), num_layers, _RC)
      call neumann_flux_for_mix_like(trim(mix_like), neumann_flux_value, _RC)
      call dirichlet_value_for_mix_like(trim(mix_like), dirichlet_pin_value, _RC)

      if (present(realized_flux)) allocate(realized_flux(ni * nj))
      do j = 1, nj
         do i = 1, ni
            call step_one_column(operator_cache(cache_index)%op, phi3d(i,j,:), mass3d(i,j,:), dt, &
                 bottom_boundary, dry_deposition_velocity_present, dry_deposition_velocity2d, &
                 neumann_flux_value, dirichlet_pin_value, i, j, realized_flux, (j-1)*ni + i, _RC)
         end do
      end do

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine run_turbulent_mixing_step

   ! Dispatches one column by bottom_boundary, supplying exactly the
   ! arguments that condition requires, and (if realized_flux is present)
   ! records the step's realized flux at column_index.
   subroutine step_one_column(op, phi, mass, dt, bottom_boundary, dry_deposition_velocity_present, &
        dry_deposition_velocity2d, neumann_flux_value, dirichlet_pin_value, i, j, realized_flux, &
        column_index, rc)
      type(ImplicitColumnDiffusion), intent(in) :: op
      real(kind=ESMF_KIND_R8), intent(inout) :: phi(:)
      real(kind=ESMF_KIND_R8), intent(in) :: mass(:)
      real(REAL64), intent(in) :: dt
      integer, intent(in) :: bottom_boundary
      logical, intent(in) :: dry_deposition_velocity_present
      real(kind=ESMF_KIND_R8), pointer, intent(in) :: dry_deposition_velocity2d(:,:)
      real(REAL64), intent(in) :: neumann_flux_value, dirichlet_pin_value
      integer, intent(in) :: i, j
      real(REAL64), allocatable, optional, intent(inout) :: realized_flux(:)
      integer, intent(in) :: column_index
      integer, optional, intent(out) :: rc

      real(REAL64) :: phi_column(size(phi)), mass_column(size(mass)), realized, robin_coefficient
      integer :: status

      phi_column = phi
      mass_column = mass

      select case (bottom_boundary)
      case (BOTTOM_NEUMANN_HOMOGENEOUS)
         call op%step(phi_column, mass_column, dt, bottom_boundary, &
              realized_surface_upward_flux=realized, _RC)
      case (BOTTOM_NEUMANN)
         call op%step(phi_column, mass_column, dt, bottom_boundary, &
              surface_upward_flux=neumann_flux_value, realized_surface_upward_flux=realized, _RC)
      case (BOTTOM_ROBIN)
         _ASSERT(dry_deposition_velocity_present, 'robin requires dry_deposition_velocity to be bound')
         robin_coefficient = real(dry_deposition_velocity2d(i,j), REAL64) * TEST_SURFACE_AIR_DENSITY
         call op%step(phi_column, mass_column, dt, bottom_boundary, &
              robin_coefficient=robin_coefficient, realized_surface_upward_flux=realized, _RC)
      case (BOTTOM_DIRICHLET)
         call op%step(phi_column, mass_column, dt, bottom_boundary, &
              surface_value=dirichlet_pin_value, realized_surface_upward_flux=realized, _RC)
      case default
         _FAIL('run_turbulent_mixing_step: unknown bottom boundary condition')
      end select

      phi = phi_column
      if (present(realized_flux)) realized_flux(column_index) = realized

      _RETURN(_SUCCESS)
   end subroutine step_one_column

   ! The constant kappa value for one mix_like class (not the whole
   ! profile; build_mix_like_operator broadcasts it across num_layers-1
   ! interior interfaces).
   subroutine kappa_for_mix_like(mix_like, kappa_value, rc)
      character(*), intent(in) :: mix_like
      real(REAL64), intent(out) :: kappa_value
      integer, optional, intent(out) :: rc

      character(:), allocatable :: msg

      select case (mix_like)
      case (TURBULENT_MIXING_MIX_LIKE_HEAT)
         kappa_value = KAPPA_HEAT
      case (TURBULENT_MIXING_MIX_LIKE_MOISTURE)
         kappa_value = KAPPA_MOISTURE
      case (TURBULENT_MIXING_MIX_LIKE_MOMENTUM)
         kappa_value = KAPPA_MOMENTUM
      case default
         msg = "run_turbulent_mixing_step: unknown mix_like '" // mix_like // "'"
         _FAIL(msg)
      end select

      _RETURN(_SUCCESS)
   end subroutine kappa_for_mix_like

   ! The neumann surface_upward_flux test constant for one mix_like class.
   subroutine neumann_flux_for_mix_like(mix_like, flux_value, rc)
      character(*), intent(in) :: mix_like
      real(REAL64), intent(out) :: flux_value
      integer, optional, intent(out) :: rc

      character(:), allocatable :: msg

      select case (mix_like)
      case (TURBULENT_MIXING_MIX_LIKE_HEAT)
         flux_value = NEUMANN_SURFACE_FLUX_HEAT
      case (TURBULENT_MIXING_MIX_LIKE_MOISTURE)
         flux_value = NEUMANN_SURFACE_FLUX_MOISTURE
      case (TURBULENT_MIXING_MIX_LIKE_MOMENTUM)
         flux_value = NEUMANN_SURFACE_FLUX_MOMENTUM
      case default
         msg = "run_turbulent_mixing_step: unknown mix_like '" // mix_like // "'"
         _FAIL(msg)
      end select

      _RETURN(_SUCCESS)
   end subroutine neumann_flux_for_mix_like

   ! The dirichlet surface_value test constant for one mix_like class.
   subroutine dirichlet_value_for_mix_like(mix_like, pin_value, rc)
      character(*), intent(in) :: mix_like
      real(REAL64), intent(out) :: pin_value
      integer, optional, intent(out) :: rc

      character(:), allocatable :: msg

      select case (mix_like)
      case (TURBULENT_MIXING_MIX_LIKE_HEAT)
         pin_value = DIRICHLET_SURFACE_VALUE_HEAT
      case (TURBULENT_MIXING_MIX_LIKE_MOISTURE)
         pin_value = DIRICHLET_SURFACE_VALUE_MOISTURE
      case (TURBULENT_MIXING_MIX_LIKE_MOMENTUM)
         pin_value = DIRICHLET_SURFACE_VALUE_MOMENTUM
      case default
         msg = "run_turbulent_mixing_step: unknown mix_like '" // mix_like // "'"
         _FAIL(msg)
      end select

      _RETURN(_SUCCESS)
   end subroutine dirichlet_value_for_mix_like

   ! Returns operator_cache's index for mix_like at num_layers, building
   ! and appending a new entry on first use (or on a layer-count change
   ! for an already-seen mix_like).
   function get_or_build_operator(mix_like, num_layers, rc) result(cache_index)
      character(*), intent(in) :: mix_like
      integer, intent(in) :: num_layers
      integer, optional, intent(out) :: rc
      integer :: cache_index

      type(OperatorCacheEntry), allocatable :: grown(:)
      real(REAL64) :: kappa_value
      integer :: n, status

      if (.not. allocated(operator_cache)) allocate(operator_cache(0))

      do cache_index = 1, size(operator_cache)
         if (operator_cache(cache_index)%mix_like == mix_like .and. &
              operator_cache(cache_index)%num_layers == num_layers) then
            _RETURN(_SUCCESS)
         end if
      end do

      call kappa_for_mix_like(mix_like, kappa_value, _RC)

      n = size(operator_cache)
      allocate(grown(n + 1))
      grown(1:n) = operator_cache
      grown(n+1)%mix_like = mix_like
      grown(n+1)%num_layers = num_layers
      grown(n+1)%op = ImplicitColumnDiffusion(spread(kappa_value, 1, num_layers - 1))
      call move_alloc(grown, operator_cache)
      cache_index = n + 1

      _RETURN(_SUCCESS)
   end function get_or_build_operator

end module mapl_TurbulentMixingStep_mod
