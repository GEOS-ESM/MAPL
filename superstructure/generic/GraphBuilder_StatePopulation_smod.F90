#include "MAPL.h"

! Graph-native state population: inserts resolved fields into the real
! ESMF import/export States.

submodule (mapl_GraphBuilder_mod) GraphBuilder_StatePopulation_smod
   use esmf, only: ESMF_State, ESMF_StateAddReplace, ESMF_STATEITEM_FIELD, &
        ESMF_STATEITEM_FIELDBUNDLE, operator(==), ESMF_STATEINTENT_INTERNAL
   use mapl_FieldNamedAlias_mod, only: MAPL_NamedAlias
   use mapl_StateItemCharacteristic_mod, only: SYNC_PHASE_DESCRIPTIVE
   use mapl_GraphMode_mod, only: graph_mode, GRAPH_MODE_GRAPH
   use mapl_ExtensionMaterialization_mod, only: complete_created_field, &
        describe_created_field, field_extension_is_created, &
        write_advertised_keys
   use mapl_GraphOwnsUserStates_mod, only: graph_populates_user_states
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_GriddedComponentDriver_mod, only: GriddedComponentDriver
   use mapl_NodeIdSet_mod, only: NodeIdSet, NodeIdSetIterator
   use mapl_VariableSpecVector_mod, only: VariableSpecVector, &
        VariableSpecVectorIterator, operator(/=)
   use mapl_PortIdNodeIdMap_mod, only: PortIdNodeIdMap, PortIdNodeIdMapIterator, operator(/=)
   use mapl_ESMF_Utilities_mod, only: get_substate
   use mapl_ErrorHandling_mod
   implicit none(type, external)

   ! One var_spec that passed a walk's intent filter, with the node its
   ! item_key resolves to. id_ptr is null for a declared item that has no
   ! node; what that means is the call site's business.
   type :: SpecNodePair
      type(VariableSpec), pointer :: var_spec => null()
      type(NodeId), pointer :: id_ptr => null()
   end type SpecNodePair

contains

   ! Populates the real ESMF States from the graph. A real producer only
   ! in GRAPH_MODE_GRAPH; a no-op in REGISTRY and BOTH mode, so `both`
   ! can never reach the ESMF_StateAdd below.
   !
   ! Two walks. (1) Items `this` advertises and connects to itself. (2)
   ! Each declared descendant's var_specs, looked up in THAT COMPONENT's
   ! graph (populate_descendants).
   ! For a connection between two children, the resolved item is each
   ! child's own StateItemNode, never a node in the parent's graph, so
   ! walking `this`'s var_specs alone would find nothing. The dependency
   ! edge lives in the hierarchy's forward network; node lookup works from
   ! any ComponentGraph in the hierarchy because node storage is shared.
   !
   ! An export's field is on its own node. An import's field is found by
   ! walking predecessor edges to the first node with a real Field
   ! (find_materialized_predecessor), which handles a direct match and
   ! an extension chain alike. With the switch on, a VECTOR or BRACKET
   ! export goes in as its bundle. Other items are skipped. An unresolved
   ! import gets its own created Field when graph_populates_user_states()
   ! is on, and nothing otherwise.
   !
   ! With graph_populates_user_states() on, the export-port table is walked
   ! too (populate_export_ports): a re-export has no declared item of that
   ! name. A propagated child export goes only to the outer state.
   !
   ! own_only (advertise time, switch on): only the user states of `this`
   ! itself, and only each item's own created Field, unconnected and
   ! undescribed except for the non-structural keys (units, names). No
   ! child walk, no completion, no allocation. The later full population
   ! replaces the same entries.
   !
   ! With graph_populates_user_states() off, only Fields already completed
   ! are inserted, and INTERNAL and EXPORT items are skipped: legacy filled
   ! those user states and must keep them. With it on, an INTERNAL item is
   ! completed and allocated, and a created export goes in as it is (GRIDSET).
   module subroutine graphbuilder_populate_state(this, multi_state, rc, &
        own_only)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(MultiState), intent(inout) :: multi_state
      integer, optional, intent(out) :: rc
      logical, optional, intent(in) :: own_only

      integer :: status
      logical :: only_own
      type(ComponentSpec), pointer :: this_spec, child_spec
      type(ComponentGraph), pointer :: graph
      type(ComponentGraph), pointer :: child_graph
      type(DependencyNetwork), pointer :: net
      type(VariableSpec), pointer :: var_spec
      type(SpecNodePair), allocatable :: spec_nodes(:)
      character(:), allocatable :: child_name
      integer :: n, i
      type(OuterMetaComponent), pointer :: child_meta
      type(GriddedComponentDriver), pointer :: child_driver
      type(MultiState) :: child_states

      _RETURN_IF(graph_mode() /= GRAPH_MODE_GRAPH)
      only_own = .false.
      if (present(own_only)) only_own = own_only
      _RETURN_IF(only_own .and. .not. graph_populates_user_states())

      graph => this%get_component_graph()
      net => graph%get_network(graph%get_forward_network_id())

      ! Same-component case: item connected entirely to itself.
      this_spec => this%get_component_spec()
      spec_nodes = collect_spec_nodes(this_spec%var_specs, graph, &
           [ESMF_STATEINTENT_IMPORT, ESMF_STATEINTENT_EXPORT, &
           ESMF_STATEINTENT_INTERNAL])
      do i = 1, size(spec_nodes)
         var_spec => spec_nodes(i)%var_spec
         call populate_one(graph, net, multi_state, spec_nodes(i)%id_ptr, &
              var_spec%state_intent, var_spec%short_name, only_own, &
              .true., _RC)
      end do

      ! Export ports: a re-exported or propagated item is an export under the
      ! port's name that no declared item names.
      if (graph_populates_user_states()) then
         call populate_export_ports(graph, multi_state, only_own, .false., _RC)
      end if

      _RETURN_IF(only_own)

      ! Cross-component case: populate INTO THE CHILD's own real
      ! MultiState, never `this`'s `multi_state`; the connected item
      ! belongs to the child. Only the parent reaches down.
      call populate_descendants(this, graph, net, .true., _RC)

      _RETURN(_SUCCESS)
   end subroutine graphbuilder_populate_state

   ! Fills the import/export States the parent sees, as legacy
   ! add_to_states(mode='outer') does, with the same Fields the user states
   ! hold. A no-op unless graph_populates_user_states() is on. Declared
   ! IMPORT and EXPORT items, then every export port including a propagated
   ! child export (a slash in the port name), which only this state gets.
   ! INTERNAL items are not outer items. No descendants: each component
   ! fills its own outer states at its own realize_accepted.
   module subroutine graphbuilder_populate_outer_state(this, multi_state, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(MultiState), intent(inout) :: multi_state
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentSpec), pointer :: this_spec
      type(ComponentGraph), pointer :: graph
      type(DependencyNetwork), pointer :: net
      type(VariableSpec), pointer :: var_spec
      type(SpecNodePair), allocatable :: spec_nodes(:)
      integer :: i

      _RETURN_UNLESS(graph_populates_user_states())

      graph => this%get_component_graph()
      net => graph%get_network(graph%get_forward_network_id())

      this_spec => this%get_component_spec()
      spec_nodes = collect_spec_nodes(this_spec%var_specs, graph, &
           [ESMF_STATEINTENT_IMPORT, ESMF_STATEINTENT_EXPORT])
      do i = 1, size(spec_nodes)
         var_spec => spec_nodes(i)%var_spec
         call populate_one(graph, net, multi_state, spec_nodes(i)%id_ptr, &
              var_spec%state_intent, var_spec%short_name, .false., &
              .true., _RC)
      end do

      call populate_export_ports(graph, multi_state, .false., .true., _RC)

      _RETURN(_SUCCESS)
   end subroutine graphbuilder_populate_outer_state

   ! Populates the user states of every descendant of `owner`, using the
   ! hierarchy's forward network. A grandchild is reached from every
   ! ancestor. Below the direct children, an import with no producer yet is
   ! left to the ancestor whose connection will provide one: no own-created
   ! fallback, which would replace the right Field.
   recursive subroutine populate_descendants(owner, graph, net, direct, rc)
      class(OuterMetaComponent), target, intent(inout) :: owner
      type(ComponentGraph), target, intent(in) :: graph
      type(DependencyNetwork), intent(in) :: net
      logical, intent(in) :: direct
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentSpec), pointer :: child_spec
      type(ComponentGraph), pointer :: child_graph
      type(VariableSpec), pointer :: var_spec
      type(SpecNodePair), allocatable :: spec_nodes(:)
      character(:), allocatable :: child_name
      integer :: n, i, j
      type(OuterMetaComponent), pointer :: child_meta
      type(GriddedComponentDriver), pointer :: child_driver
      type(MultiState) :: child_states

      n = owner%get_num_children()
      do i = 1, n
         child_name = owner%get_child_name(i, _RC)
         child_spec => owner%get_child_component_spec(child_name, _RC)
         child_graph => owner%get_child_component_graph(child_name, _RC)
         child_meta => owner%get_child_outer_meta(child_name, _RC)
         child_driver => child_meta%get_user_gc_driver()
         child_states = child_driver%get_states()
         ! The lookup is on the CHILD's graph; populate_one still gets the
         ! owner's graph/net, because the walk is driven from above.
         spec_nodes = collect_spec_nodes(child_spec%var_specs, child_graph, &
              [ESMF_STATEINTENT_IMPORT, ESMF_STATEINTENT_EXPORT])
         do j = 1, size(spec_nodes)
            var_spec => spec_nodes(j)%var_spec
            call populate_one(graph, net, child_states, &
                 spec_nodes(j)%id_ptr, var_spec%state_intent, &
                 var_spec%short_name, .false., direct, _RC)
         end do
         if (graph_populates_user_states()) then
            call populate_descendants(child_meta, graph, net, .false., _RC)
         end if
      end do

      _RETURN(_SUCCESS)
   end subroutine populate_descendants

   ! Shared by both walks in graphbuilder_populate_state: resolves
   ! id_ptr's real field (export: direct; import: predecessor walk) and,
   ! if found, inserts it into the State for state_intent, under
   ! short_name (see insert_alias).
   module subroutine populate_one(graph, net, multi_state, id_ptr, &
        state_intent, short_name, own_only, may_fallback, rc)
      type(ComponentGraph), target, intent(in) :: graph
      type(DependencyNetwork), intent(in) :: net
      type(MultiState), intent(inout) :: multi_state
      type(NodeId), pointer, intent(in) :: id_ptr
      type(ESMF_StateIntent_Flag), intent(in) :: state_intent
      character(*), intent(in) :: short_name
      logical, intent(in) :: own_only
      logical, intent(in) :: may_fallback
      integer, optional, intent(out) :: rc

      integer :: status
      type(NodeId) :: item_id, field_node_id
      logical :: found
      type(ESMF_Field) :: field
      type(ESMF_FieldBundle) :: bundle
      type(ESMF_StateItem_Flag) :: item_kind
      type(ESMF_State) :: substate
      logical :: owns, created_only
      character(:), allocatable :: inner_name

      if (.not. associated(id_ptr)) then
         _RETURN(_SUCCESS)
      end if
      item_id = id_ptr

      owns = graph_populates_user_states()
      if (state_intent == ESMF_STATEINTENT_INTERNAL .and. .not. owns) then
         _RETURN(_SUCCESS)
      end if
      ! Legacy created the export Field its gridcomp holds; replacing it
      ! leaves the gridcomp writing to a Field no one reads.
      if (state_intent == ESMF_STATEINTENT_EXPORT .and. .not. owns) then
         _RETURN(_SUCCESS)
      end if
      if (own_only) then
         call write_keys_on_created_field(graph, item_id, _RC)
      else if (owns .and. .not. (state_intent == ESMF_STATEINTENT_IMPORT)) then
         call prepare_created_field(graph, item_id, &
              state_intent == ESMF_STATEINTENT_INTERNAL, _RC)
      end if

      if (own_only .or. state_intent == ESMF_STATEINTENT_EXPORT .or. &
           state_intent == ESMF_STATEINTENT_INTERNAL) then
         call get_item_if_materialized(graph, item_id, field, bundle, &
              item_kind, found, _RC)
      else
         field_node_id = find_materialized_predecessor(graph, net, item_id, found)
         ! Keep the if-then-end-if block: a one-line `if` guarding a call
         ! that ends in the error macro leaves the macro's status check
         ! unguarded and reads an uninitialized status.
         if (found) then
            call get_item_if_materialized(graph, field_node_id, field, &
                 bundle, item_kind, found, _RC)
         else if (owns .and. may_fallback) then
            ! No producer: the import's own created Field, as legacy leaves
            ! an unconnected import.
            call prepare_created_field(graph, item_id, .false., _RC)
            call get_item_if_materialized(graph, item_id, field, bundle, &
                 item_kind, found, _RC)
         end if
      end if
      if (.not. found) then
         _RETURN(_SUCCESS)
      end if
      ! Legacy fills the state with its own bundles (it adds them under the
      ! same name and rejects a different one), so the graph adds only Fields
      ! unless it owns the states.
      if (item_kind == ESMF_STATEITEM_FIELDBUNDLE .and. .not. owns) then
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

      call get_target_substate(multi_state, state_intent, short_name, &
           substate, inner_name, _RC)

      ! Per-alias keys come from item_id's own descriptor, not
      ! field_node_id's: each declared item keeps its own values regardless
      ! of which field backs it.
      call insert_alias(graph, item_id, substate, inner_name, item_kind, &
           field, bundle, _RC)

      _RETURN(_SUCCESS)
   end subroutine populate_one

   ! Adds the Field of every export port's bound node to the user export
   ! State under the port's name. A port whose name resolves to its own
   ! node is the declared-item walk's entry and is skipped; a same-named
   ! port on another node (a re-export) is not. A propagated child export
   ! (slash in the name) is for the outer state only, as in legacy, so
   ! include_propagated is false for a user pass.
   subroutine populate_export_ports(graph, multi_state, own_only, &
        include_propagated, rc)
      type(ComponentGraph), target, intent(in) :: graph
      type(MultiState), intent(inout) :: multi_state
      logical, intent(in) :: own_only
      logical, intent(in) :: include_propagated
      integer, optional, intent(out) :: rc

      integer :: status
      type(PortIdNodeIdMap), target :: export_ports
      type(PortIdNodeIdMapIterator) :: iter
      type(NodeId) :: node_id
      type(NodeId), pointer :: declared_id
      character(:), allocatable :: port_name

      export_ports = graph%get_export_ports()
      associate (e => export_ports%ftn_end())
         iter = export_ports%ftn_begin()
         do while (iter /= e)
            call iter%next()
            node_id = iter%second()
            port_name = graph%get_export_port_name(iter%first())
            if (len(port_name) == 0) cycle
            declared_id => graph%get_resource_index(item_key(ESMF_STATEINTENT_EXPORT, port_name))
            if (associated(declared_id)) then
               if (declared_id == node_id) cycle
            end if
            if (.not. include_propagated .and. index(port_name, '/') > 0) cycle
            call populate_export_port(graph, multi_state, node_id, &
                 port_name, own_only, _RC)
         end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine populate_export_ports

   ! The substate path mirrors FieldClassAspect%add_to_state: the part of
   ! the name before the last slash is a substate, created if missing.
   subroutine populate_export_port(graph, multi_state, node_id, port_name, &
        own_only, rc)
      type(ComponentGraph), target, intent(in) :: graph
      type(MultiState), intent(inout) :: multi_state
      type(NodeId), intent(in) :: node_id
      character(*), intent(in) :: port_name
      logical, intent(in) :: own_only
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: found
      type(ESMF_Field) :: field
      type(ESMF_FieldBundle) :: bundle
      type(ESMF_StateItem_Flag) :: item_kind
      type(ESMF_State) :: substate
      character(:), allocatable :: inner_name

      call get_item_if_materialized(graph, node_id, field, bundle, &
           item_kind, found, _RC)
      if (.not. found) then
         _RETURN(_SUCCESS)
      end if
      if (own_only) then
         call write_keys_on_created_field(graph, node_id, _RC)
      else
         call prepare_created_field(graph, node_id, &
              node_is_internal(graph, node_id), _RC)
      end if

      call get_target_substate(multi_state, ESMF_STATEINTENT_EXPORT, &
           port_name, substate, inner_name, _RC)

      call insert_alias(graph, node_id, substate, inner_name, item_kind, &
           field, bundle, _RC)

      _RETURN(_SUCCESS)
   end subroutine populate_export_port

   ! Inserts a NAMED ALIAS of the item (not the raw field) under
   ! inner_name, then writes node_id's per-alias characteristics onto it.
   !
   ! The alias is required because ESMF_StateAddReplace matches by name:
   ! a chain's final field has no explicit name, so inserting it raw
   ! would duplicate rather than replace on the second
   ! GENERIC_INIT_ACCEPT_TRANSFER pass. Mirrors legacy
   ! FieldClassAspect%add_to_state.
   !
   ! Field-wide keys belong to the exporting item and are not touched
   ! here. For a bundle alias sync_alias_info runs and does nothing.
   subroutine insert_alias(graph, node_id, substate, inner_name, &
        item_kind, field, bundle, rc)
      type(ComponentGraph), target, intent(in) :: graph
      type(NodeId), intent(in) :: node_id
      type(ESMF_State), intent(inout) :: substate
      character(*), intent(in) :: inner_name
      type(ESMF_StateItem_Flag), intent(in) :: item_kind
      type(ESMF_Field), intent(in) :: field
      type(ESMF_FieldBundle), intent(in) :: bundle
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Field) :: alias
      type(ESMF_FieldBundle) :: bundle_alias
      type(GraphStateItem) :: alias_handle

      ! Add-or-replace by the alias's stable name: idempotent across the
      ! two ACCEPT_TRANSFER passes.
      if (item_kind == ESMF_STATEITEM_FIELDBUNDLE) then
         bundle_alias = MAPL_NamedAlias(bundle, name=inner_name, _RC)
         call ESMF_StateAddReplace(substate, [bundle_alias], _RC)
         call alias_handle%set(bundle_alias, _RC)
      else
         alias = MAPL_NamedAlias(field, name=inner_name, _RC)
         call ESMF_StateAddReplace(substate, [alias], _RC)
         call alias_handle%set(alias, _RC)
      end if

      call sync_alias_info(graph, node_id, inner_name, alias_handle, _RC)

      _RETURN(_SUCCESS)
   end subroutine insert_alias

   ! The shared head of every var_spec walk: the specs whose state_intent
   ! is in `intents`, each paired with the node `graph` resolves its
   ! item_key to.
   !
   ! `graph` is only the graph the lookup is done on; the caller may act on
   ! a different one (populate_descendants resolves on the child's graph
   ! but populates through the owner's).
   function collect_spec_nodes(var_specs, graph, intents) result(spec_nodes)
      type(VariableSpecVector), target, intent(in) :: var_specs
      type(ComponentGraph), target, intent(in) :: graph
      type(ESMF_StateIntent_Flag), intent(in) :: intents(:)
      type(SpecNodePair), allocatable :: spec_nodes(:)

      integer :: n
      type(VariableSpecVectorIterator) :: iter
      type(VariableSpec), pointer :: var_spec
      type(SpecNodePair), allocatable :: buffer(:)

      allocate(buffer(var_specs%size()))
      n = 0
      associate (e => var_specs%ftn_end())
         iter = var_specs%ftn_begin()
         do while (iter /= e)
            call iter%next()
            var_spec => iter%of()
            if (.not. intent_in_set(var_spec%state_intent, intents)) cycle
            n = n + 1
            buffer(n)%var_spec => var_spec
            buffer(n)%id_ptr => graph%get_resource_index( &
                 item_key(var_spec%state_intent, var_spec%short_name))
         end do
      end associate
      spec_nodes = buffer(:n)
   end function collect_spec_nodes

   ! ESMF_StateIntent_Flag has no elemental comparison, so the set test is
   ! an explicit loop rather than any(state_intent == intents).
   function intent_in_set(state_intent, intents) result(in_set)
      type(ESMF_StateIntent_Flag), intent(in) :: state_intent
      type(ESMF_StateIntent_Flag), intent(in) :: intents(:)
      logical :: in_set

      integer :: i

      in_set = .false.
      do i = 1, size(intents)
         if (state_intent == intents(i)) then
            in_set = .true.
            return
         end if
      end do
   end function intent_in_set

   ! The State a name is inserted into: the part of the name before the last
   ! slash is a substate (created if missing), the rest the inner name.
   subroutine get_target_substate(multi_state, state_intent, full_name, &
        substate, inner_name, rc)
      type(MultiState), intent(inout) :: multi_state
      type(ESMF_StateIntent_Flag), intent(in) :: state_intent
      character(*), intent(in) :: full_name
      type(ESMF_State), intent(out) :: substate
      character(:), allocatable, intent(out) :: inner_name
      integer, optional, intent(out) :: rc

      integer :: status, idx
      type(ESMF_State) :: state

      call multi_state%get_state(state, state_intent, _RC)
      idx = index(full_name, '/', back=.true.)
      if (idx > 0) then
         call get_substate(state, full_name(:idx-1), substate=substate, _RC)
         inner_name = full_name(idx+1:)
      else
         substate = state
         inner_name = full_name
      end if

      _RETURN(_SUCCESS)
   end subroutine get_target_substate

   ! A re-exported internal is allocated like any internal; a created export
   ! stays created.
   function node_is_internal(graph, node_id) result(is_internal)
      type(ComponentGraph), target, intent(in) :: graph
      type(NodeId), intent(in) :: node_id
      logical :: is_internal

      class(GraphNode), pointer :: node

      is_internal = .false.
      node => graph%get_node(node_id)
      if (.not. associated(node)) return
      select type (node)
      class is (StateItemNode)
         is_internal = (node%get_state_intent() == ESMF_STATEINTENT_INTERNAL)
      end select
   end function node_is_internal

   ! Completes and allocates every INTERNAL Field of this component that is
   ! still only created and whose descriptor is complete by now. One whose
   ! descriptor is still incomplete stays created.
   module subroutine graphbuilder_allocate_internals(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentSpec), pointer :: spec
      type(ComponentGraph), pointer :: graph
      type(SpecNodePair), allocatable :: spec_nodes(:)
      integer :: i

      _RETURN_UNLESS(graph_populates_user_states())

      graph => this%get_component_graph()
      spec => this%get_component_spec()
      spec_nodes = collect_spec_nodes(spec%var_specs, graph, &
           [ESMF_STATEINTENT_INTERNAL])
      do i = 1, size(spec_nodes)
         ! prepare_created_field takes a plain NodeId, so an item without a
         ! node must be dropped here rather than passed along.
         if (.not. associated(spec_nodes(i)%id_ptr)) cycle
         call prepare_created_field(graph, spec_nodes(i)%id_ptr, .true., _RC)
      end do

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

   ! Writes the non-structural keys of node_id onto its Field when that is
   ! only created (see write_advertised_keys).
   subroutine write_keys_on_created_field(graph, node_id, rc)
      type(ComponentGraph), target, intent(in) :: graph
      type(NodeId), intent(in) :: node_id
      integer, optional, intent(out) :: rc

      integer :: status
      class(GraphNode), pointer :: node

      node => graph%get_node(node_id)
      _RETURN_UNLESS(associated(node))
      select type (node)
      class is (StateItemNode)
         call write_advertised_keys(node, _RC)
      end select

      _RETURN(_SUCCESS)
   end subroutine write_keys_on_created_field

   ! No-op-if-not-materialized accessor shared by graphbuilder_populate_state's
   ! export and (resolved) import cases. The payload is a Field or a
   ! FieldBundle (item_kind says which; the unused output is reset to its default).
   module subroutine get_item_if_materialized(graph, node_id, field, bundle, &
        item_kind, found, rc)
      type(ComponentGraph), target, intent(in) :: graph
      type(NodeId), intent(in) :: node_id
      type(ESMF_Field), intent(out) :: field
      type(ESMF_FieldBundle), intent(out) :: bundle
      type(ESMF_StateItem_Flag), intent(out) :: item_kind
      logical, intent(out) :: found
      integer, optional, intent(out) :: rc

      integer :: status
      class(GraphNode), pointer :: node
      type(GraphStateItem), pointer :: payload

      found = .false.
      node => graph%get_node(node_id)
      if (.not. associated(node)) then
         _RETURN(_SUCCESS)
      end if

      select type (node)
      class is (StateItemNode)
         payload => node%get_payload_ptr()
         item_kind = payload%itemType(_RC)
         if (item_kind == ESMF_STATEITEM_FIELD) then
            field = payload%get_field(_RC)
            found = .true.
         else if (item_kind == ESMF_STATEITEM_FIELDBUNDLE) then
            bundle = payload%get_field_bundle(_RC)
            found = .true.
         end if
      end select

      _RETURN(_SUCCESS)
   end subroutine get_item_if_materialized

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
   ! node whose payload is a real Field or FieldBundle is reached. Takes the FIRST
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
            if (status == 0 .and. (kind_flag == ESMF_STATEITEM_FIELD .or. &
                 kind_flag == ESMF_STATEITEM_FIELDBUNDLE)) then
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
