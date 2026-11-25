#include "MAPL.h"

!> Implicit vertical diffusion of one column, mass-weighted.
!!
!! Layers are numbered k = 1 (top) .. L (bottom). One backward-Euler step solves
!!
!!    A phi_new = phi_old + dt b,    A = I + dt M^-1 (D^T K D + B)
!!
!! with M = diag(mass), K = diag(kappa) at the L-1 interior interfaces (zero
!! flux at the top), and B, b set by the bottom boundary condition:
!!
!!    neumann_homogeneous : no flux                     B = 0,           b = 0
!!    neumann             : upward flux F               B = 0,           b = F/m_L e_L
!!    robin               : flux F - r phi_L, r >= 0    B = r e_L e_L^T, b = F/m_L e_L
!!    dirichlet           : phi_L = phi_hat             row L replaced by e_L^T, rhs phi_hat
!!
!! dirichlet pins the bottom layer itself: phi_L is reset to phi_hat at the start
!! of the step, so its realized flux sum(m (phi_new - phi_old))/dt includes that
!! reset, m_L (phi_hat - phi_old_L)/dt, plus the flux kappa_{L-1} carries upward.
!!
!! For kappa >= 0 and r >= 0 every row has a positive diagonal, non-positive
!! off-diagonals and strict diagonal dominance, so A is an M-matrix: A^-1 >= 0
!! entrywise. The system is factored directly (no rank-one update), so for
!! non-negative right-hand sides the elimination adds only non-negative terms
!! and the result cannot be negative, not even by round-off.
module mapl_ImplicitColumnDiffusion_mod

   use mapl_ErrorHandling_mod
   use mapl_KeywordEnforcer_mod, only: KeywordEnforcer
   use, intrinsic :: iso_fortran_env, only: REAL64

   implicit none(type,external)
   private

   public :: ImplicitColumnDiffusion
   public :: BOTTOM_NEUMANN_HOMOGENEOUS
   public :: BOTTOM_NEUMANN
   public :: BOTTOM_ROBIN
   public :: BOTTOM_DIRICHLET
   public :: bottom_boundary_from_name

   ! Values match the bottom_boundary attribute of the turbulent_mixing interface.
   integer, parameter :: BOTTOM_NEUMANN_HOMOGENEOUS = 1
   integer, parameter :: BOTTOM_NEUMANN = 2
   integer, parameter :: BOTTOM_ROBIN = 3
   integer, parameter :: BOTTOM_DIRICHLET = 4

   !> One operator per mixing class: holds that class's interface coefficients
   !! kappa(k) (between layers k and k+1, kg m-2 s-1). Mass and time step are
   !! supplied per call, so one instance serves every column of the class.
   type :: ImplicitColumnDiffusion
      private
      real(REAL64), allocatable :: kappa(:)
   contains
      procedure :: get_num_layers
      procedure :: assemble
      procedure :: step
   end type ImplicitColumnDiffusion

   interface ImplicitColumnDiffusion
      procedure :: new_ImplicitColumnDiffusion
   end interface ImplicitColumnDiffusion

contains

   function new_ImplicitColumnDiffusion(kappa) result(operator)
      type(ImplicitColumnDiffusion) :: operator
      real(REAL64), intent(in) :: kappa(:)

      operator%kappa = kappa
   end function new_ImplicitColumnDiffusion

   integer function get_num_layers(this) result(num_layers)
      class(ImplicitColumnDiffusion), intent(in) :: this

      num_layers = size(this%kappa) + 1
   end function get_num_layers

   !> Map a turbulent_mixing bottom_boundary attribute value to its constant.
   subroutine bottom_boundary_from_name(name, bottom_boundary, rc)
      character(*), intent(in) :: name
      integer, intent(out) :: bottom_boundary
      integer, optional, intent(out) :: rc

      select case (name)
      case ('neumann_homogeneous')
         bottom_boundary = BOTTOM_NEUMANN_HOMOGENEOUS
      case ('neumann')
         bottom_boundary = BOTTOM_NEUMANN
      case ('robin')
         bottom_boundary = BOTTOM_ROBIN
      case ('dirichlet')
         bottom_boundary = BOTTOM_DIRICHLET
      case default
         bottom_boundary = -1
         _FAIL("unknown bottom_boundary '" // name // "'; allowed: neumann_homogeneous, neumann, robin, dirichlet")
      end select

      _RETURN(_SUCCESS)
   end subroutine bottom_boundary_from_name

   !> Build the tridiagonal system A phi_new = rhs for one column.
   !!
   !! lower(k) couples row k to k-1 (lower(1) = 0), upper(k) couples row k to
   !! k+1 (upper(L) = 0). surface_upward_flux is F (kg m-2 s-1 times units of
   !! phi), robin_coefficient is r = rho v_d (kg m-2 s-1), surface_value is
   !! phi_hat. Each condition requires exactly the inputs it uses; passing an
   !! unused one is an error so a caller cannot silently drop a forcing.
   subroutine assemble(this, phi, mass, dt, bottom_boundary, lower, diag, upper, rhs, unusable, &
        surface_upward_flux, robin_coefficient, surface_value, rc)
      class(ImplicitColumnDiffusion), intent(in) :: this
      real(REAL64), intent(in) :: phi(:)
      real(REAL64), intent(in) :: mass(:)
      real(REAL64), intent(in) :: dt
      integer, intent(in) :: bottom_boundary
      real(REAL64), intent(out) :: lower(:)
      real(REAL64), intent(out) :: diag(:)
      real(REAL64), intent(out) :: upper(:)
      real(REAL64), intent(out) :: rhs(:)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      real(REAL64), optional, intent(in) :: surface_upward_flux
      real(REAL64), optional, intent(in) :: robin_coefficient
      real(REAL64), optional, intent(in) :: surface_value
      integer, optional, intent(out) :: rc

      integer :: k, num_layers
      real(REAL64) :: alpha

      num_layers = this%get_num_layers()
      _ASSERT(size(phi) == num_layers, 'phi size must equal the number of layers')
      _ASSERT(size(mass) == num_layers, 'mass size must equal the number of layers')
      _ASSERT(size(lower) == num_layers .and. size(diag) == num_layers, 'system arrays must have one entry per layer')
      _ASSERT(size(upper) == num_layers .and. size(rhs) == num_layers, 'system arrays must have one entry per layer')
      _ASSERT(all(mass > 0), 'layer mass must be positive')
      _ASSERT(all(this%kappa >= 0), 'diffusion coefficients must be non-negative')
      _ASSERT(dt > 0, 'time step must be positive')

      do k = 1, num_layers
         alpha = dt / mass(k)
         lower(k) = 0
         upper(k) = 0
         if (k > 1) lower(k) = -alpha * this%kappa(k-1)
         if (k < num_layers) upper(k) = -alpha * this%kappa(k)
         ! Written as 1 + (non-negative terms) so the diagonal never cancels.
         diag(k) = 1 - lower(k) - upper(k)
         rhs(k) = phi(k)
      end do

      alpha = dt / mass(num_layers)
      select case (bottom_boundary)
      case (BOTTOM_NEUMANN_HOMOGENEOUS)
         _ASSERT(.not. present(surface_upward_flux), 'neumann_homogeneous takes no surface_upward_flux')
         _ASSERT(.not. present(robin_coefficient), 'neumann_homogeneous takes no robin_coefficient')
         _ASSERT(.not. present(surface_value), 'neumann_homogeneous takes no surface_value')
      case (BOTTOM_NEUMANN)
         _ASSERT(present(surface_upward_flux), 'neumann requires surface_upward_flux')
         _ASSERT(.not. present(robin_coefficient), 'neumann takes no robin_coefficient')
         _ASSERT(.not. present(surface_value), 'neumann takes no surface_value')
         rhs(num_layers) = rhs(num_layers) + alpha * surface_upward_flux
      case (BOTTOM_ROBIN)
         _ASSERT(present(robin_coefficient), 'robin requires robin_coefficient')
         _ASSERT(.not. present(surface_value), 'robin takes no surface_value')
         _ASSERT(robin_coefficient >= 0, 'robin_coefficient must be non-negative')
         diag(num_layers) = diag(num_layers) + alpha * robin_coefficient
         if (present(surface_upward_flux)) then
            rhs(num_layers) = rhs(num_layers) + alpha * surface_upward_flux
         end if
      case (BOTTOM_DIRICHLET)
         _ASSERT(present(surface_value), 'dirichlet requires surface_value')
         _ASSERT(.not. present(surface_upward_flux), 'dirichlet takes no surface_upward_flux')
         _ASSERT(.not. present(robin_coefficient), 'dirichlet takes no robin_coefficient')
         lower(num_layers) = 0
         diag(num_layers) = 1
         rhs(num_layers) = surface_value
      case default
         _FAIL('unknown bottom boundary condition')
      end select

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine assemble

   !> Advance phi by one implicit step of length dt, in place.
   !!
   !! realized_surface_upward_flux, when present, is the flux that entered the
   !! column through the bottom during the step: 0, F, F - r phi_new(L), or for
   !! dirichlet sum(mass * (phi_new - phi_old)) / dt, including the bottom-layer reset.
   subroutine step(this, phi, mass, dt, bottom_boundary, unusable, &
        surface_upward_flux, robin_coefficient, surface_value, realized_surface_upward_flux, rc)
      class(ImplicitColumnDiffusion), intent(in) :: this
      real(REAL64), intent(inout) :: phi(:)
      real(REAL64), intent(in) :: mass(:)
      real(REAL64), intent(in) :: dt
      integer, intent(in) :: bottom_boundary
      class(KeywordEnforcer), optional, intent(in) :: unusable
      real(REAL64), optional, intent(in) :: surface_upward_flux
      real(REAL64), optional, intent(in) :: robin_coefficient
      real(REAL64), optional, intent(in) :: surface_value
      real(REAL64), optional, intent(out) :: realized_surface_upward_flux
      integer, optional, intent(out) :: rc

      integer :: status, num_layers
      real(REAL64), allocatable :: lower(:), diag(:), upper(:), rhs(:), phi_old(:)
      real(REAL64) :: flux

      num_layers = this%get_num_layers()
      allocate(lower(num_layers), diag(num_layers), upper(num_layers), rhs(num_layers))
      call this%assemble(phi, mass, dt, bottom_boundary, lower, diag, upper, rhs, &
           surface_upward_flux=surface_upward_flux, robin_coefficient=robin_coefficient, &
           surface_value=surface_value, _RC)
      phi_old = phi
      call solve_tridiagonal(lower, diag, upper, rhs, phi, _RC)

      if (present(realized_surface_upward_flux)) then
         select case (bottom_boundary)
         case (BOTTOM_NEUMANN_HOMOGENEOUS)
            flux = 0
         case (BOTTOM_NEUMANN)
            flux = surface_upward_flux
         case (BOTTOM_ROBIN)
            flux = -robin_coefficient * phi(num_layers)
            if (present(surface_upward_flux)) flux = flux + surface_upward_flux
         case (BOTTOM_DIRICHLET)
            flux = sum(mass * (phi - phi_old)) / dt
         case default
            _FAIL('unknown bottom boundary condition')
         end select
         realized_surface_upward_flux = flux
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine step

   !> Thomas algorithm for a tridiagonal system (lower(1), upper(n) ignored).
   !!
   !! For an M-matrix (positive diagonal, non-positive off-diagonals, strict
   !! diagonal dominance) every pivot is positive and, for rhs >= 0, every
   !! update adds a non-negative term, so x >= 0 holds exactly.
   subroutine solve_tridiagonal(lower, diag, upper, rhs, x, rc)
      real(REAL64), intent(in) :: lower(:)
      real(REAL64), intent(in) :: diag(:)
      real(REAL64), intent(in) :: upper(:)
      real(REAL64), intent(in) :: rhs(:)
      real(REAL64), intent(out) :: x(:)
      integer, optional, intent(out) :: rc

      integer :: k, n
      real(REAL64), allocatable :: c(:), d(:)
      real(REAL64) :: pivot

      n = size(diag)
      _ASSERT(n > 0, 'empty system')
      _ASSERT(size(lower) == n .and. size(upper) == n, 'inconsistent system sizes')
      _ASSERT(size(rhs) == n .and. size(x) == n, 'inconsistent system sizes')
      allocate(c(n), d(n))

      pivot = diag(1)
      _ASSERT(pivot > 0, 'non-positive pivot: system is not an M-matrix')
      c(1) = upper(1) / pivot
      d(1) = rhs(1) / pivot
      do k = 2, n
         pivot = diag(k) - lower(k) * c(k-1)
         _ASSERT(pivot > 0, 'non-positive pivot: system is not an M-matrix')
         c(k) = 0
         if (k < n) c(k) = upper(k) / pivot
         d(k) = (rhs(k) - lower(k) * d(k-1)) / pivot
      end do

      x(n) = d(n)
      do k = n - 1, 1, -1
         x(k) = d(k) - c(k) * x(k+1)
      end do

      _RETURN(_SUCCESS)
   end subroutine solve_tridiagonal

end module mapl_ImplicitColumnDiffusion_mod
