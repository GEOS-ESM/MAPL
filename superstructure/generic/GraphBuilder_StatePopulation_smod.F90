#include "MAPL.h"

! Graph-native state population: inserts resolved fields into the real
! ESMF import/export States.

submodule (mapl_GraphBuilder_mod) GraphBuilder_StatePopulation_smod
   use esmf, only: ESMF_State, ESMF_StateAddReplace, ESMF_STATEITEM_FIELD, &
        operator(==), ESMF_STATEINTENT_INTERNAL
   use mapl_FieldNamedAlias_mod, only: MAPL_NamedAlias
   use mapl_StateItemCharacteristic_mod, only: SYNC_PHASE_DESCRIPTIVE
   use mapl_GraphMode_mod, only: graph_mode, GRAPH_MODE_GRAPH
   use mapl_ExtensionMaterialization_mod, only: complete_created_field, &
        describe_created_field, field_extension_is_created
   use mapl_GraphOwnsUserStates_mod, only: graph_populates_user_states
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_GriddedComponentDriver_mod, only: GriddedComponentDriver
   use mapl_NodeIdSet_mod, only: NodeIdSet, NodeIdSetIterator
   use mapl_VariableSpecVector_mod, only: VariableSpecVectorIterator, operator(/=)
   use mapl_ErrorHandling_mod
   implicit none(type, external)

contains

   ! Populates the real ESMF States from the graph. A real producer only
   ! in GRAPH_MODE_GRAPH; a no-op in REGISTRY and BOTH mode, so `both`
   ! can never reach the ESMF_StateAdd below.
   !
   ! Two walks. (1) Items `this` advertises and connects to itself. (2)
   ! Each declared child's var_specs, looked up in THAT CHILD's graph.
   ! For a connection between two children, the resolved item is each
   ! child's own StateItemNode, never a node in the parent's graph, so
   ! walking `this`'s var_specs alone would find nothing. The dependency
   ! edge still lives in `this`'s default network (`this` declared the
   ! connection); node lookup works from any ComponentGraph in the
   ! hierarchy because node storage is shared.
   !
   ! An export's field is on its own node. An import's field is found by
   ! walking predecessor edges to the first node with a real Field
   ! (find_materialized_predecessor), which handles a direct match and
   ! an extension chain alike. Non-Field items are skipped. An unresolved
   ! import gets its own created Field when graph_populates_user_states()
   ! is on, and nothing otherwise.
   !
   ! With graph_populates_user_states() off, only Fields already completed
   ! are inserted and INTERNAL items are skipped: legacy filled the user
   ! states and must keep them. With it on, an INTERNAL item is completed
   ! and allocated, and a created export goes in as it is (GRIDSET).
   module subroutine graphbuilder_populate_state(this, multi_state, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(MultiState), intent(inout) :: multi_state
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentSpec), pointer :: this_spec, child_spec
      type(ComponentGraph), pointer :: graph
      type(ComponentGraph), pointer :: child_graph
      type(DependencyNetwork), pointer :: net
      type(VariableSpecVectorIterator) :: iter
      type(VariableSpec), pointer :: var_spec
      type(NodeId), pointer :: id_ptr
      character(:), allocatable :: child_name
      integer :: n, i
      type(OuterMetaComponent), pointer :: child_meta
      type(GriddedComponentDriver), pointer :: child_driver
      type(MultiState) :: child_states

      _RETURN_IF(graph_mode() /= GRAPH_MODE_GRAPH)

      graph => this%get_component_graph()
      net => graph%get_network(graph%get_default_network_id())

      ! Same-component case: item connected entirely to itself.
      this_spec => this%get_component_spec()
      associate (e => this_spec%var_specs%ftn_end())
         iter = this_spec%var_specs%ftn_begin()
         do while (iter /= e)
            call iter%next()
            var_spec => iter%of()
            if (.not. (var_spec%state_intent == ESMF_STATEINTENT_IMPORT .or. &
                 var_spec%state_intent == ESMF_STATEINTENT_EXPORT .or. &
                 var_spec%state_intent == ESMF_STATEINTENT_INTERNAL)) cycle
            id_ptr => graph%get_resource_index(item_key(var_spec%state_intent, var_spec%short_name))
            call populate_one(graph, net, multi_state, id_ptr, var_spec%state_intent, var_spec%short_name, _RC)
         end do
      end associate

      ! Cross-component case: populate INTO THE CHILD's own real
      ! MultiState, never `this`'s `multi_state`; the connected item
      ! belongs to the child. Only the parent reaches down.
      n = this%get_num_children()
      do i = 1, n
         child_name = this%get_child_name(i, _RC)
         child_spec => this%get_child_component_spec(child_name, _RC)
         child_graph => this%get_child_component_graph(child_name, _RC)
         child_meta => this%get_child_outer_meta(child_name, _RC)
         child_driver => child_meta%get_user_gc_driver()
         child_states = child_driver%get_states()
         associate (e => child_spec%var_specs%ftn_end())
            iter = child_spec%var_specs%ftn_begin()
            do while (iter /= e)
               call iter%next()
               var_spec => iter%of()
               if (.not. (var_spec%state_intent == ESMF_STATEINTENT_IMPORT .or. &
                    var_spec%state_intent == ESMF_STATEINTENT_EXPORT)) cycle
               id_ptr => child_graph%get_resource_index(item_key(var_spec%state_intent, var_spec%short_name))
               call populate_one(graph, net, child_states, id_ptr, var_spec%state_intent, var_spec%short_name, _RC)
            end do
         end associate
      end do

      _RETURN(_SUCCESS)
   end subroutine graphbuilder_populate_state

   ! Shared by both walks in graphbuilder_populate_state: resolves
   ! id_ptr's real field (export: direct; import: predecessor walk) and,
   ! if found, inserts a NAMED ALIAS of it (not the raw field) into the
   ! State for state_intent, under short_name.
   !
   ! The alias is required because ESMF_StateAddReplace matches by name:
   ! a chain's final field has no explicit name, so inserting it raw
   ! would duplicate rather than replace on the second
   ! GENERIC_INIT_ACCEPT_TRANSFER pass. Mirrors legacy
   ! FieldClassAspect%add_to_state.
   module subroutine populate_one(graph, net, multi_state, id_ptr, state_intent, short_name, rc)
      type(ComponentGraph), target, intent(in) :: graph
      type(DependencyNetwork), intent(in) :: net
      type(MultiState), intent(inout) :: multi_state
      type(NodeId), pointer, intent(in) :: id_ptr
      type(ESMF_StateIntent_Flag), intent(in) :: state_intent
      character(*), intent(in) :: short_name
      integer, optional, intent(out) :: rc

      integer :: status
      type(NodeId) :: item_id, field_node_id
      logical :: found
      type(ESMF_Field) :: field
      type(ESMF_Field) :: alias
      type(ESMF_State) :: state
      type(GraphStateItem) :: alias_handle
      logical :: owns, created_only

      if (.not. associated(id_ptr)) then
         _RETURN(_SUCCESS)
      end if
      item_id = id_ptr

      owns = graph_populates_user_states()
      if (state_intent == ESMF_STATEINTENT_INTERNAL .and. .not. owns) then
         _RETURN(_SUCCESS)
      end if
      if (owns .and. .not. (state_intent == ESMF_STATEINTENT_IMPORT)) then
         call prepare_created_field(graph, item_id, &
              state_intent == ESMF_STATEINTENT_INTERNAL, _RC)
      end if

      if (state_intent == ESMF_STATEINTENT_EXPORT .or. &
           state_intent == ESMF_STATEINTENT_INTERNAL) then
         call get_field_if_materialized(graph, item_id, field, found, _RC)
      else
         field_node_id = find_materialized_predecessor(graph, net, item_id, found)
         ! Keep the if-then-end-if block: a one-line `if` guarding a call
         ! that ends in the error macro leaves the macro's status check
         ! unguarded and reads an uninitialized status.
         if (found) then
            call get_field_if_materialized(graph, field_node_id, field, found, _RC)
         else if (owns) then
            ! No producer: the import's own created Field, as legacy leaves
            ! an unconnected import.
            call prepare_created_field(graph, item_id, .false., _RC)
            call get_field_if_materialized(graph, item_id, field, found, _RC)
         end if
      end if
      if (.not. found) then
         _RETURN(_SUCCESS)
      end if
      if (.not. owns) then
         if (state_intent == ESMF_STATEINTENT_IMPORT) then
            created_only = field_extension_is_created(graph, field_node_id)
         else
            created_only = field_extension_is_created(graph, item_id)
         end if
         if (created_only) then
            _RETURN(_SUCCESS)
         end if
      end if

      call multi_state%get_state(state, state_intent, _RC)

      ! Add-or-replace by the alias's stable name: idempotent across the
      ! two ACCEPT_TRANSFER passes.
      alias = MAPL_NamedAlias(field, name=short_name, _RC)
      call ESMF_StateAddReplace(state, [alias], _RC)

      ! Per-alias keys (restart_mode, long_name) come from item_id's own
      ! descriptor, not field_node_id's: each declared item keeps its own
      ! values regardless of which field backs it. Field-wide keys belong to
      ! the exporting item and are not touched here.
      call alias_handle%set(alias, _RC)
      call sync_alias_info(graph, item_id, short_name, alias_handle, _RC)

      _RETURN(_SUCCESS)
   end subroutine populate_one

   ! Completes and allocates every INTERNAL Field of this component that is
   ! still only created and whose descriptor is complete by now. One whose
   ! descriptor is still incomplete stays created.
   module subroutine graphbuilder_allocate_internals(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentSpec), pointer :: spec
      type(ComponentGraph), pointer :: graph
      type(VariableSpecVectorIterator) :: iter
      type(VariableSpec), pointer :: var_spec
      type(NodeId), pointer :: id_ptr

      _RETURN_UNLESS(graph_populates_user_states())

      graph => this%get_component_graph()
      spec => this%get_component_spec()
      associate (e => spec%var_specs%ftn_end())
         iter = spec%var_specs%ftn_begin()
         do while (iter /= e)
            call iter%next()
            var_spec => iter%of()
            if (.not. (var_spec%state_intent == &
                 ESMF_STATEINTENT_INTERNAL)) cycle
            id_ptr => graph%get_resource_index( &
                 item_key(var_spec%state_intent, var_spec%short_name))
            if (.not. associated(id_ptr)) cycle
            call prepare_created_field(graph, id_ptr, .true., _RC)
         end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine graphbuilder_allocate_internals

   ! A created Field whose descriptor is complete gets its description
   ! (geom first), so a user sees a GRIDSET Field. An INTERNAL item is also
   ! completed and allocated (legacy activates it always). An incomplete
   ! descriptor leaves the Field as created.
   subroutine prepare_created_field(graph, node_id, allocate_it, rc)
      type(ComponentGraph), target, intent(in) :: graph
      type(NodeId), intent(in) :: node_id
      logical, intent(in) :: allocate_it
      integer, optional, intent(out) :: rc

      integer :: status
      class(GraphNode), pointer :: node
      type(StateItemDescriptor), pointer :: descriptor
      type(StateItemCharacteristicKind), allocatable :: incomplete(:)

      node => graph%get_node(node_id)
      _RETURN_UNLESS(associated(node))
      select type (node)
      class is (StateItemNode)
         descriptor => node%get_descriptor()
         incomplete = descriptor%get_incomplete_kinds()
         if (size(incomplete) == 0) then
            if (allocate_it) then
               call complete_created_field(node, _RC)
            else
               call describe_created_field(node, _RC)
            end if
         end if
      end select

      _RETURN(_SUCCESS)
   end subroutine prepare_created_field

   ! Field-only, no-op-if-not-materialized accessor shared by
   ! graphbuilder_populate_state's export and (resolved) import cases.
   module subroutine get_field_if_materialized(graph, node_id, field, found, rc)
      type(ComponentGraph), target, intent(in) :: graph
      type(NodeId), intent(in) :: node_id
      type(ESMF_Field), intent(out) :: field
      logical, intent(out) :: found
      integer, optional, intent(out) :: rc

      integer :: status
      class(GraphNode), pointer :: node
      type(GraphStateItem) :: payload
      type(ESMF_StateItem_Flag) :: kind_flag

      found = .false.
      node => graph%get_node(node_id)
      if (.not. associated(node)) then
         _RETURN(_SUCCESS)
      end if

      select type (node)
      class is (StateItemNode)
         payload = node%get_payload()
         kind_flag = payload%itemType(_RC)
         if (kind_flag == ESMF_STATEITEM_FIELD) then
            field = payload%get_field(_RC)
            found = .true.
         end if
      end select

      _RETURN(_SUCCESS)
   end subroutine get_field_if_materialized

   ! Writes node_id's per-alias characteristics onto the alias handle.
   ! Private to this submodule.
   subroutine sync_alias_info(graph, node_id, short_name, alias_handle, rc)
      type(ComponentGraph), target, intent(in) :: graph
      type(NodeId), intent(in) :: node_id
      character(*), intent(in) :: short_name
      type(GraphStateItem), intent(inout) :: alias_handle
      integer, optional, intent(out) :: rc

      integer :: status
      class(GraphNode), pointer :: node
      type(StateItemDescriptor), pointer :: descriptor

      node => graph%get_node(node_id)
      _ASSERT(associated(node), 'GraphBuilder: sync_alias_info - NodeId not found in graph: ' // short_name)

      select type (node)
      class is (StateItemNode)
         descriptor => node%get_descriptor()
         call descriptor%sync_per_alias_to_info(alias_handle, SYNC_PHASE_DESCRIPTIVE, _RC)
      class default
         _FAIL('GraphBuilder: sync_alias_info - NodeId does not refer to a StateItemNode: ' // short_name)
      end select

      _RETURN(_SUCCESS)
   end subroutine sync_alias_info

   ! Walks predecessor edges from start_id, one hop at a time, until a
   ! node whose payload is a real Field is reached. Takes the FIRST
   ! predecessor at each hop: validation already guarantees a single
   ! producer per import. found=.false. (not an error) if there is no
   ! predecessor (unresolved import) or the defensive hop limit is hit.
   module function find_materialized_predecessor(graph, net, start_id, found) result(field_node_id)
      type(ComponentGraph), target, intent(in) :: graph
      type(DependencyNetwork), intent(in) :: net
      type(NodeId), intent(in) :: start_id
      logical, intent(out) :: found
      type(NodeId) :: field_node_id

      integer, parameter :: MAX_CHAIN_HOPS = 64
      type(NodeId) :: current
      class(GraphNode), pointer :: node
      type(GraphStateItem) :: payload
      type(ESMF_StateItem_Flag) :: kind_flag
      ! target required: the ftn_begin() iterator points into preds.
      type(NodeIdSet), target :: preds
      type(NodeIdSetIterator) :: pred_iter
      integer :: hop, status

      found = .false.
      current = start_id

      do hop = 1, MAX_CHAIN_HOPS
         node => graph%get_node(current)
         if (.not. associated(node)) exit

         select type (node)
         class is (StateItemNode)
            payload = node%get_payload()
            kind_flag = payload%itemType(rc=status)
            if (status == 0 .and. kind_flag == ESMF_STATEITEM_FIELD) then
               if (.not. field_extension_is_created(graph, current)) then
                  field_node_id = current
                  found = .true.
                  return
               end if
               ! A Field that is only created is no producer. The start
               ! node is the import itself, whose own created Field is
               ! walked past; further along, a created-only Field means the
               ! connection did not complete it, so there is nothing to find.
               if (hop > 1) return
            end if
         end select

         if (.not. net%has_predecessors(current)) exit
         preds = net%get_predecessors(current)
         pred_iter = preds%ftn_begin()
         call pred_iter%next()
         current = pred_iter%of()
      end do
   end function find_materialized_predecessor

end submodule GraphBuilder_StatePopulation_smod
