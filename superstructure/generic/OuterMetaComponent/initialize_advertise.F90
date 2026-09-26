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
      ! Phase 3b (docs/graph/spec/20-implementation-roadmap.md sec 20.4.1):
      ! populate this component's graph representation alongside the
      ! legacy advertise path, never replacing it - see
      ! mapl_GraphBuilder_mod's module header and this change's design.md
      ! "Invocation point" decision. run_advertise_hook() never
      ! propagates failure into this routine's own error path.
      call gb%run_advertise_hook(this)
      ! openspec/changes/horizontal-geometry-graph-state-item (Phase
      ! 4e): gated behind mapl_GraphMode_mod%graph_native_enabled(), a
      ! no-op today. Must run after run_advertise_hook above (needs
      ! nothing from it directly, but keeps geometry's own advertise
      ! step alongside ordinary items') and relies on recurse() above
      ! having already completed every child's own INIT_ADVERTISE
      ! (including their own run_geometry_hook) bottom-up.
      call gb%run_geometry_hook(this)
      call this%run_custom(ESMF_METHOD_INITIALIZE, PHASE_NAME, _RC)

      call process_connections(this, _RC)
      ! Phase 3b: process_connections() above only calls Connection%activate()
      ! at this phase - it does NOT form real wiring (that is
      ! initialize_accept_transfer.F90's job, mirroring connect()). The
      ! activate()-time analog here is likewise read-only: it determines
      ! which ordinary-match imports would be unresolved, matching the
      ! same decision propagate_unsatisfied_imports() below is about to
      ! make, without creating any graph structure (see
      ! mapl_GraphBuilder_mod module header, "Two-phase timing").
      call gb%run_activate_hook(this)
      call this%registry%propagate_unsatisfied_imports(_RC)
      call this%registry%propagate_exports(_RC)

      user_states = this%user_gc_driver%get_states()
      call this%registry%add_to_states(user_states, mode='user', _RC)

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
