#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) initialize_advertise_smod
   use mapl_enums_api, only: MAPL_GENERIC_INIT_ADVERTISE
   use mapl_VirtualConnectionPt_mod
   use mapl_StateItem_mod
   use mapl_VariableSpec_mod
   use mapl_VariableSpecVector_mod, only: VariableSpecVectorIterator
   use esmf, only: operator(==)
   use mapl_Connection_mod
   use mapl_ConnectionVector_mod, only: ConnectionVectorIterator
   use mapl_ConnectionVector_mod, only: operator(/=)
   use mapl_VariableSpecVector_mod, only: operator(/=)
   use mapl_StateItemSpec_mod
   use mapl_MultiState_mod
   use mapl_MpiTimerGauge_mod, only: MpiTimerGauge
   use mapl_GraphBuilder_mod, only: GraphBuilder
   use mapl_GraphOwnsUserStates_mod, only: graph_populates_user_states
   use mapl_ErrorHandling_mod
   implicit none (type, external)


contains

   module recursive subroutine initialize_advertise(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(esmf_State) :: importState
      type(esmf_State) :: exportState
      type(esmf_Clock) :: clock
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(MultiState) :: user_states
      type(ESMF_VM) :: vm
      integer :: comm
      integer :: status
      type(GraphBuilder) :: gb
      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_ADVERTISE'

      ! Initialize profiler
      call ESMF_VMGetCurrent(vm, _RC)
      call ESMF_VMGet(vm, mpiCommunicator=comm, _RC)
      this%profiler = DistributedProfiler(this%user_gc_driver%get_name(), MpiTimerGauge(), comm=comm)
      call this%profiler%start(_RC)

      call recurse(this, phase_idx=MAPL_GENERIC_INIT_ADVERTISE, _RC)
      call self_advertise(this, _RC)
      ! Populate this component's graph representation alongside the
      ! legacy advertise path (see mapl_GraphBuilder_mod's module header).
      ! run_advertise_hook() never propagates failure into this routine's
      ! own error path.
      call gb%run_advertise_hook(this)
      ! Re-export ports must exist before the parent propagates this
      ! component's export ports.
      call gb%run_reexport_hook(this, _RC)
      ! No geometry hook here: GEOM_A/GEOM_B run strictly after ADVERTISE
      ! (GENERIC_INIT_PHASE_SEQUENCE in GenericPhases.F90), so this
      ! component's has_geom() is not yet true and the hook would be a
      ! silent no-op. It is called from initialize_geom_b.F90 instead.
      call this%run_custom(ESMF_METHOD_INITIALIZE, PHASE_NAME, _RC)

      call process_connections(this, _RC)
      ! process_connections() above only calls Connection%activate() at
      ! this phase - it does NOT form real wiring (that is
      ! initialize_accept_transfer.F90's job, mirroring connect()). The
      ! activate()-time analog here is likewise read-only: it determines
      ! which ordinary-match imports would be unresolved, matching the
      ! same decision propagate_unsatisfied_imports() below is about to
      ! make, without creating any graph structure (see
      ! mapl_GraphBuilder_mod module header, "Two-phase timing").
      call gb%run_activate_hook(this)
      ! Graph analog of the registry%propagate_unsatisfied_imports() call
      ! immediately below, placed right after run_activate_hook for the
      ! same reason legacy's call sits after process_connections()/
      ! activate(): a child's activate-time resolution must already be
      ! reflected before this component's mirroring step reads it.
      ! Mirroring before activate would needlessly bubble every import
      ! the child resolved at its own level. Never propagates failure
      ! into this routine's own error path (same as run_advertise_hook/
      ! run_activate_hook above).
      call gb%run_propagate_hook(this)
      call this%registry%propagate_unsatisfied_imports(_RC)
      ! Graph analog of legacy registry%propagate_exports() immediately
      ! below - same "parent reaches into child" shape as
      ! run_propagate_hook above, but unconditional for exports (no
      ! "already resolved" gate - see
      ! graphbuilder_run_export_propagate_hook in
      ! GraphBuilder_ConnectionResolution_smod.F90). No ordering
      ! dependency on run_activate_hook/run_propagate_hook (export
      ! propagation only reads child export-port tables, populated
      ! unconditionally at advertise_one time - no activate()-style
      ! "was this resolved yet" state to wait for); placed here, next
      ! to legacy's own analogous call, purely for readability.
      call gb%run_export_propagate_hook(this)
      call this%registry%propagate_exports(_RC)

      ! Graph-owned user states: the graph inserts each item's created Field
      ! now, legacy otherwise.
      user_states = this%user_gc_driver%get_states()
      if (graph_populates_user_states()) then
         ! A user component reads its own states in INIT_MODIFY_ADVERTISED.
         ! A failure is reported and not fatal, like the other graph hooks.
         call gb%run_populate_state_hook(this, user_states, own_only=.true.)
      else
         call this%registry%add_to_states(user_states, mode='user', _RC)
      end if

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine initialize_advertise

   subroutine self_advertise(this, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(VariableSpecVectorIterator) :: iter
      type(VariableSpec), pointer :: var_spec

      associate (e => this%component_spec%var_specs%end())
        iter = this%component_spec%var_specs%begin()
        do while (iter /= e)
           var_spec => iter%of()
           call this%advertise_variable(var_spec, _RC)
           call iter%next()
        end do
      end associate

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine self_advertise


   subroutine process_connections(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ConnectionVectorIterator) :: iter
      class(Connection), pointer :: c

      associate (e => this%component_spec%connections%end())
        iter = this%component_spec%connections%begin()
        do while (iter /= e)
           c => iter%of()
           call c%activate(this%registry, _RC)
           call iter%next()
        end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine process_connections

end submodule initialize_advertise_smod
