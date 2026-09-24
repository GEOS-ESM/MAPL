#include "MAPL.h"

!------------------------------------------------------------------------------
! GridCompDriverPhaseInvoker: the real GridCompPhaseInvoker implementation
! (MethodInvocationAdapter.F90), backed by the existing
! GriddedComponentDriver (superstructure/component/
! GriddedComponentDriver.F90)'s own initialize/run/finalize entry
! points - griddedcomponentdriver-integration-lifecycle design.md
! Decisions ("A new DriverResolver abstraction decouples the real
! GridCompPhaseInvoker from OuterMetaComponent").
!
! Holds which of initialize/run/finalize this instance calls (one
! ESMF_Method_Flag, fixed at construction) and a DriverResolver used to
! turn a driver_key into the concrete driver to call it on. invoke_phase()
! does exactly: resolve, dispatch to the one driver call matching the
! held method flag, propagate rc - nothing else touches arguments/
! bindings/clock, satisfying REQ-MTH-003 ("must not duplicate invocation
! logic already in GriddedComponentDriver") by construction. clock is
! unused here: GriddedComponentDriver's own clock member (set at
! construction/via set_clock) is what run/initialize/finalize actually
! use internally - a second clock threaded through this call would be
! redundant, not authoritative.
!------------------------------------------------------------------------------
module mapl_GridCompDriverPhaseInvoker_mod
   use mapl_MethodInvocationAdapter_mod, only: GridCompPhaseInvoker, DriverResolver
   use mapl_ArgumentSpecMap_mod, only: ArgumentSpecMap
   use mapl_StateItemMemberMap_mod, only: StateItemMemberMap
   use mapl_GriddedComponentDriver_mod, only: GriddedComponentDriver
   use ESMF, only: ESMF_Clock, ESMF_Method_Flag
   use ESMF, only: ESMF_METHOD_INITIALIZE, ESMF_METHOD_RUN, ESMF_METHOD_FINALIZE
   use ESMF, only: operator(==)
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: GridCompDriverPhaseInvoker

   type, extends(GridCompPhaseInvoker) :: GridCompDriverPhaseInvoker
      private
      class(DriverResolver), allocatable :: resolver
      type(ESMF_Method_Flag) :: method
   contains
      procedure :: invoke_phase => driver_invoke_phase
   end type GridCompDriverPhaseInvoker

   interface GridCompDriverPhaseInvoker
      module procedure new_GridCompDriverPhaseInvoker
   end interface GridCompDriverPhaseInvoker

contains

   function new_GridCompDriverPhaseInvoker(resolver, method) result(invoker)
      class(DriverResolver), intent(in) :: resolver
      type(ESMF_Method_Flag), intent(in) :: method
      type(GridCompDriverPhaseInvoker) :: invoker

      allocate(invoker%resolver, source=resolver)
      invoker%method = method
   end function new_GridCompDriverPhaseInvoker

   subroutine driver_invoke_phase(this, driver_key, phase_idx, arguments, bindings, clock, rc)
      class(GridCompDriverPhaseInvoker), intent(inout) :: this
      character(*), intent(in) :: driver_key
      integer, intent(in) :: phase_idx
      type(ArgumentSpecMap), intent(in) :: arguments
      type(StateItemMemberMap), intent(in) :: bindings
      type(ESMF_Clock), optional, intent(in) :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      class(GriddedComponentDriver), pointer :: driver

      driver => this%resolver%resolve(driver_key, _RC)

      if (this%method == ESMF_METHOD_INITIALIZE) then
         call driver%initialize(phase_idx=phase_idx, _RC)
      else if (this%method == ESMF_METHOD_RUN) then
         call driver%run(phase_idx=phase_idx, _RC)
      else if (this%method == ESMF_METHOD_FINALIZE) then
         call driver%finalize(phase_idx=phase_idx, _RC)
      else
         _FAIL('GridCompDriverPhaseInvoker: unsupported ESMF_Method_Flag')
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(arguments)
      _UNUSED_DUMMY(bindings)
      _UNUSED_DUMMY(clock)
   end subroutine driver_invoke_phase

end module mapl_GridCompDriverPhaseInvoker_mod
