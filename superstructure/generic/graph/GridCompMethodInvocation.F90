#include "MAPL.h"

!------------------------------------------------------------------------------
! GridCompMethodInvocation: MethodInvocationAdapter concrete subtype modeling
! a GridComp initialize/run phase invocation (REQ-MTH-001). Carries a stable
! driver identifier (character key, REQ-MTH-009's "stable local identifier,
! never a raw pointer to the driver" shape) and a phase_idx; delegates to an
! injected GridCompPhaseInvoker rather than calling anything ESMF-specific
! itself, satisfying REQ-MTH-003 by construction.
!
! phase_idx (integer), not a phase name: it matches
! GriddedComponentDriver%run/initialize/finalize
! (superstructure/component/GriddedComponentDriver.F90), which take
! phase_idx. Translating a phase name to phase_idx (via
! MethodPhasesMap%get_phase_index, as OuterMetaComponent/run_child_by_name.F90
! does) is the job of whoever constructs a GridCompMethodInvocation, not this
! module.
!
! The real invoker is GridCompDriverPhaseInvoker.F90, backed by
! GriddedComponentDriver, with driver_key -> GriddedComponentDriver
! resolution by DriverResolver (MethodInvocationAdapter.F90). Tests use a
! synthetic double (superstructure/generic/graph/tests/
! Test_GridCompMethodInvocation.pf).
!------------------------------------------------------------------------------
module mapl_GridCompMethodInvocation_mod
   use mapl_MethodInvocationAdapter_mod, only: MethodInvocationAdapter, GridCompPhaseInvoker
   use mapl_ArgumentSpecMap_mod, only: ArgumentSpecMap
   use mapl_StateItemMemberMap_mod, only: StateItemMemberMap
   use ESMF, only: ESMF_Clock
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: GridCompMethodInvocation

   type, extends(MethodInvocationAdapter) :: GridCompMethodInvocation
      private
      character(:), allocatable :: driver_key
      integer :: phase_idx
      class(GridCompPhaseInvoker), allocatable :: invoker
   contains
      procedure :: invoke => gridcomp_invoke
      procedure :: get_driver_key => gridcomp_get_driver_key
      procedure :: get_phase_idx => gridcomp_get_phase_idx
   end type GridCompMethodInvocation

   interface GridCompMethodInvocation
      module procedure new_GridCompMethodInvocation
   end interface GridCompMethodInvocation

contains

   ! invoker is optional so a caller (or a test of the not-yet-attached case)
   ! MAY construct an adapter with none attached; invoke() then fails loudly
   ! (below) rather than silently doing nothing.
   function new_GridCompMethodInvocation(driver_key, phase_idx, invoker) result(adapter)
      character(*), intent(in) :: driver_key
      integer, intent(in) :: phase_idx
      class(GridCompPhaseInvoker), optional, intent(in) :: invoker
      type(GridCompMethodInvocation) :: adapter

      adapter%driver_key = driver_key
      adapter%phase_idx = phase_idx
      if (present(invoker)) allocate(adapter%invoker, source=invoker)
   end function new_GridCompMethodInvocation

   function gridcomp_get_driver_key(this) result(driver_key)
      class(GridCompMethodInvocation), intent(in) :: this
      character(:), allocatable :: driver_key

      driver_key = this%driver_key
   end function gridcomp_get_driver_key

   function gridcomp_get_phase_idx(this) result(phase_idx)
      class(GridCompMethodInvocation), intent(in) :: this
      integer :: phase_idx

      phase_idx = this%phase_idx
   end function gridcomp_get_phase_idx

   ! REQ-MTH-003: gathers bound arguments, calls the one injected
   ! GridCompPhaseInvoker entry point, returns. Nothing else.
   subroutine gridcomp_invoke(this, arguments, bindings, clock, rc)
      class(GridCompMethodInvocation), intent(inout) :: this
      type(ArgumentSpecMap), intent(in) :: arguments
      type(StateItemMemberMap), intent(in) :: bindings
      type(ESMF_Clock), optional, intent(in) :: clock
      integer, optional, intent(out) :: rc

      integer :: status

      _ASSERT(allocated(this%invoker), 'GridCompMethodInvocation: invoke called with no GridCompPhaseInvoker attached')

      call this%invoker%invoke_phase(this%driver_key, this%phase_idx, arguments, bindings, clock, _RC)

      _RETURN(_SUCCESS)
   end subroutine gridcomp_invoke

end module mapl_GridCompMethodInvocation_mod
