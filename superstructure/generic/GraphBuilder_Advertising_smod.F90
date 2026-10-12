#include "MAPL.h"

! Task 2: Advertising
! (openspec/changes/graphbuilder-submodule-split: procedure bodies moved
! here verbatim from GraphBuilder.F90, mechanical subroutine->module
! subroutine / function->module function change only - see that
! change's design.md D1 for the full section-to-file mapping.)

submodule (mapl_GraphBuilder_mod) GraphBuilder_Advertising_smod
   use esmf, only: operator(==)
   use mapl_CompositeStateMaterialization_mod, only: materialize_composite
   use mapl_VariableSpecVector_mod, only: VariableSpecVectorIterator, operator(/=)
   use mapl_ErrorHandling_mod
   implicit none(type, external)

contains

   ! REQ-GB-003 "Advertising state items" / "Creating StateItemNodes":
   ! one StateItemNode per advertised import/export/internal item, plus
   ! (task 4.1) import/export items are recorded as public ports - MAPL's
   ! existing coupling semantics already treat every import/export as
   ! externally addressable (any sibling's connect_all can reach it by
   ! name) while internal items are component-private, so that existing
   ! distinction is reused here as "declared a public port" rather than
   ! inventing a new marker on VariableSpec/ComponentSpec.
   module subroutine graphbuilder_advertise(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentSpec), pointer :: comp_spec
      type(ComponentGraph), pointer :: graph
      type(VariableSpecVectorIterator) :: iter
      type(VariableSpec), pointer :: var_spec

      comp_spec => this%get_component_spec()
      graph => this%get_component_graph()

      associate (e => comp_spec%var_specs%ftn_end())
         iter = comp_spec%var_specs%ftn_begin()
         do while (iter /= e)
            call iter%next()
            var_spec => iter%of()
            call advertise_one(graph, var_spec, _RC)
         end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine graphbuilder_advertise

   module subroutine advertise_one(graph, var_spec, rc)
      type(ComponentGraph), target, intent(inout) :: graph
      type(VariableSpec), intent(in) :: var_spec
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: key
      type(NodeId), pointer :: existing
      type(NodeId) :: id
      type(StateItemNode) :: node
      type(GraphStateItem) :: payload
      type(NodeRevision) :: revision
      type(PortId) :: port_id
      type(StringVector) :: member_names

      key = item_key(var_spec%state_intent, var_spec%short_name)

      ! Idempotent re-advertisement (spec scenario "Re-advertising the
      ! same item does not duplicate its node"): if this identity is
      ! already indexed, this item already has a node - nothing to do.
      existing => graph%get_resource_index(key)
      if (associated(existing)) then
         _RETURN(_SUCCESS)
      end if

      ! openspec/changes/composite-state-spec: a VariableSpec with
      ! declared members is a composite - build its real, individually
      ! addressable node tree (mapl_CompositeStateMaterialization_mod)
      ! instead of the flat, unallocated-payload node below. No other
      ! branch of this procedure changes: the same identity key,
      ! resource-index registration, and port registration apply
      ! uniformly to both cases (design.md Decisions).
      member_names = var_spec%get_member_names()
      if (member_names%size() > 0) then
         id = materialize_composite(graph, var_spec, _RC)
      else
         id = graph%next_node_id(_RC)

         ! payload/revision are left default-initialized (no ESMF handle
         ! allocated, NodeRevision invalid): this item is advertised, not
         ! yet realized - REALIZE happens in a later init phase this
         ! slice does not touch (design.md Non-Goals).
         node = StateItemNode(id, payload, revision)
         call graph%register_node(node, _RC)
      end if
      call graph%add_resource_index(key, id, _RC)

      if (var_spec%state_intent == ESMF_STATEINTENT_IMPORT) then
         port_id = graph%next_port_id(_RC)
         call graph%add_import_port(port_id, id, _RC)
      else if (var_spec%state_intent == ESMF_STATEINTENT_EXPORT) then
         port_id = graph%next_port_id(_RC)
         call graph%add_export_port(port_id, id, _RC)
      end if
      ! INTERNAL items get neither: they are not visible outside the
      ! component (task 4.1's "declared a public port" does not apply).

      _RETURN(_SUCCESS)
   end subroutine advertise_one

end submodule GraphBuilder_Advertising_smod
