#include "MAPL.h"

! Horizontal geometry as a graph node, and its parent/child wiring.

submodule (mapl_GraphBuilder_mod) GraphBuilder_Geometry_smod
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind
   use mapl_ExtensionResolution_mod, only: find_mismatched_characteristics, find_or_build_extension_chain
   use mapl_CharacteristicDefaults_mod, only: CharacteristicDefaults
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_NodeIdSet_mod, only: NodeIdSet, NodeIdSetIterator, operator(/=)
   use mapl_GeometrySpec_mod, only: GEOMETRY_FROM_CHILD, GEOMETRY_FROM_PARENT
   use mapl_GeomId_mod, only: GeomId
   use mapl_GeomProxyField_mod, only: new_geom_proxy_item, &
        add_geom_proxy_identity
   use mapl_GraphMode_mod, only: graph_mode, GRAPH_MODE_REGISTRY
   use mapl_GriddedComponentDriver_mod, only: GriddedComponentDriver
   use mapl_MultiState_mod, only: MultiState
   use mapl_geom_api, only: MAPL_GeomGetId
   use mapl_field_api, only: MAPL_FieldGet
   use esmf, only: ESMF_State, ESMF_StateGet, ESMF_STATEITEM_FIELD, ESMF_STATEITEM_FIELDBUNDLE, &
        ESMF_FieldStatus_Flag, ESMF_FieldGet, ESMF_FieldIsCreated, ESMF_FieldBundleIsCreated, ESMF_FieldBundleGet, &
        operator(==), ESMF_FIELDSTATUS_GRIDSET, ESMF_FIELDSTATUS_COMPLETE
   use mapl_ErrorHandling_mod
   implicit none(type, external)

   ! Which way wire_geometry adds the edge: PULL from a child into this
   ! component, PUSH from this component into a child.
   integer, parameter :: GEOMETRY_PULL = 1
   integer, parameter :: GEOMETRY_PUSH = 2

contains

   module subroutine graphbuilder_run_geometry_hook(this)
      class(OuterMetaComponent), target, intent(inout) :: this

      integer :: status

      ! Not gated on graph mode: connection resolution runs in every mode
      ! and a pair with a DEFERRED characteristic waits.
      call graphbuilder_resolve_deferred(this, status)
      call report_if_failed(this, 'resolve_deferred', status)

      if (graph_mode() == GRAPH_MODE_REGISTRY) return

      call graphbuilder_advertise_geometry(this, status)
      call report_if_failed(this, 'advertise_geometry', status)

      call graphbuilder_resolve_geometry(this, status)
      call report_if_failed(this, 'resolve_geometry', status)
   end subroutine graphbuilder_run_geometry_hook

   ! Wraps this%get_geom() (already resolved by GeometrySpec before this
   ! runs) as an ordinary StateItemNode in this component's graph.
   ! Idempotent.
   module subroutine graphbuilder_advertise_geometry(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentGraph), pointer :: graph
      character(:), allocatable :: key
      type(NodeId), pointer :: existing
      type(NodeId) :: id
      type(ESMF_Geom) :: geom
      type(GraphStateItem) :: payload
      type(NodeRevision) :: revision
      type(StateItemNode) :: node

      _RETURN_UNLESS(this%has_geom())

      graph => this%get_component_graph()
      key = item_key(ESMF_STATEINTENT_EXPORT, GEOMETRY_ITEM_NAME)

      existing => graph%get_resource_index(key)
      if (associated(existing)) then
         _RETURN(_SUCCESS)
      end if

      geom = this%get_geom(_RC)
      payload = new_geom_proxy_item(geom, _RC)
      id = graph%next_node_id(_RC)
      node = StateItemNode(id, payload, revision)
      call graph%register_node(node, _RC)
      call graph%add_resource_index(key, id, _RC)

      _RETURN(_SUCCESS)
   end subroutine graphbuilder_advertise_geometry

   ! Offers this component's geometry and vertical grid to every item
   ! characteristic that is still DEFERRED, then drops those that do not
   ! apply without a value. GEOM_B is the first point at which the
   ! component's geometry exists, so this must run before connection
   ! resolution.
   module subroutine graphbuilder_resolve_deferred(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(CharacteristicDefaults) :: defaults
      type(ESMF_Geom) :: geom
      class(VerticalGrid), pointer :: vgrid
      type(ComponentGraph), pointer :: graph
      type(NodeIdSet), target :: node_ids
      type(NodeIdSetIterator) :: iter
      class(GraphNode), pointer :: node
      type(StateItemDescriptor), pointer :: descriptor

      if (this%has_geom()) then
         geom = this%get_geom(_RC)
         defaults = CharacteristicDefaults(geom=geom, geom_id=this%get_geom_id())
      end if
      vgrid => this%get_vertical_grid()
      if (associated(vgrid)) call defaults%set_vgrid(vgrid)
      call defaults%set_component_scope()

      graph => this%get_component_graph()
      node_ids = graph%get_node_ids()
      associate (e => node_ids%ftn_end())
         iter = node_ids%ftn_begin()
         do while (iter /= e)
            call iter%next()
            node => graph%get_node(iter%of())
            if (.not. associated(node)) cycle
            select type (node)
            class is (StateItemNode)
               descriptor => node%get_descriptor()
               call descriptor%resolve_deferred(defaults, _RC)
               call descriptor%drop_unsupplied(defaults, _RC)
            end select
         end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine graphbuilder_resolve_deferred

   ! Fills still-DEFERRED characteristics, and settled ones still missing a
   ! creation value, from what the component's own phase put on the item's
   ! field or bundle (a component with no default geom supplies it there).
   ! Walks the subtree.
   module recursive subroutine graphbuilder_harvest_deferred(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status, i
      type(ComponentGraph), pointer :: graph
      type(NodeIdSet), target :: node_ids
      type(NodeIdSetIterator) :: iter
      class(GraphNode), pointer :: node
      type(StateItemDescriptor), pointer :: descriptor
      type(OuterMetaComponent), pointer :: child_meta
      type(GriddedComponentDriver), pointer :: driver
      type(MultiState) :: states
      character(:), allocatable :: child_name

      driver => this%get_user_gc_driver()
      states = driver%get_states()

      graph => this%get_component_graph()
      node_ids = graph%get_node_ids()
      associate (e => node_ids%ftn_end())
         iter = node_ids%ftn_begin()
         do while (iter /= e)
            call iter%next()
            node => graph%get_node(iter%of())
            if (.not. associated(node)) cycle
            select type (node)
            class is (StateItemNode)
               descriptor => node%get_descriptor()
               if (size(descriptor%get_unresolved_kinds()) == 0 .and. &
                    size(descriptor%get_incomplete_kinds()) == 0) cycle
               call fill_deferred_from_handle(states, node, descriptor, _RC)
            end select
         end do
      end associate

      do i = 1, this%get_num_children()
         child_name = this%get_child_name(i, _RC)
         child_meta => this%get_child_outer_meta(child_name, _RC)
         call graphbuilder_harvest_deferred(child_meta, _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine graphbuilder_harvest_deferred

   logical function status_has_geom(field_status) result(has_geom)
      type(ESMF_FieldStatus_Flag), intent(in) :: field_status

      has_geom = field_status == ESMF_FIELDSTATUS_GRIDSET .or. &
           field_status == ESMF_FIELDSTATUS_COMPLETE
   end function status_has_geom

   ! Reads the geom off the component's field or bundle of that name (the
   ! handle's, if already bound), lets each characteristic harvest its own
   ! recorded value, and offers both to the node's characteristics.
   ! Read-only: the handle is not bound here. An item the component has not
   ! realized yet is left alone: a later pass finds it.
   subroutine fill_deferred_from_handle(states, node, descriptor, rc)
      type(MultiState), intent(in) :: states
      class(StateItemNode), target, intent(in) :: node
      type(StateItemDescriptor), intent(inout) :: descriptor
      integer, optional, intent(out) :: rc

      integer :: status
      type(GraphStateItem), pointer :: handle
      type(ESMF_State) :: state
      type(ESMF_StateItem_Flag) :: item_kind
      type(ESMF_Field) :: field
      type(ESMF_FieldBundle) :: bundle
      type(ESMF_FieldStatus_Flag) :: field_status
      type(ESMF_Geom), allocatable :: geom
      type(CharacteristicDefaults) :: defaults
      type(GeomId) :: geom_id
      logical :: has_id
      integer :: field_count
      character(:), allocatable :: name
      type(GraphStateItem) :: realized
      logical :: payload_has_geom

      _RETURN_UNLESS(node%has_short_name())
      name = node%get_short_name()

      ! An import gets its geometry from the connected export.
      _RETURN_IF(node%get_state_intent() == ESMF_STATEINTENT_IMPORT)

      handle => node%get_payload_ptr()
      item_kind = handle%itemType(_RC)

      ! A Field only created at advertise has no geom: read the component's
      ! own field of that name instead.
      payload_has_geom = .false.
      if (item_kind == ESMF_STATEITEM_FIELD) then
         field = handle%get_field(_RC)
         call ESMF_FieldGet(field, status=field_status, _RC)
         payload_has_geom = status_has_geom(field_status)
      end if

      ! Likewise a bundle only created at advertise. Members are the reliable
      ! source of a bundle's geom (ESMF has no status query for it); a
      ! memberless bundle's geom, if any, came from the descriptor, which
      ! therefore already has it.
      if (item_kind == ESMF_STATEITEM_FIELDBUNDLE) then
         bundle = handle%get_field_bundle(_RC)
         call ESMF_FieldBundleGet(bundle, fieldCount=field_count, _RC)
         payload_has_geom = field_count > 0
      end if

      if (.not. payload_has_geom) then
         call states%get_state(state, node%get_state_intent(), _RC)
         call ESMF_StateGet(state, itemName=name, itemType=item_kind, _RC)
         if (item_kind == ESMF_STATEITEM_FIELD) then
            call ESMF_StateGet(state, itemName=name, field=field, _RC)
            _RETURN_UNLESS(ESMF_FieldIsCreated(field))
            call ESMF_FieldGet(field, status=field_status, _RC)
            _RETURN_UNLESS(status_has_geom(field_status))
         else if (item_kind == ESMF_STATEITEM_FIELDBUNDLE) then
            call ESMF_StateGet(state, itemName=name, fieldbundle=bundle, _RC)
            _RETURN_UNLESS(ESMF_FieldBundleIsCreated(bundle))
            call ESMF_FieldBundleGet(bundle, fieldCount=field_count, _RC)
            _RETURN_UNLESS(field_count > 0)
         else
            _RETURN(_SUCCESS)
         end if
      end if

      ! A bundle's geom is read from ESMF: MAPL_FieldBundleGet returns it
      ! only when the bundle Info records one, and members are the source
      ! here (see above).
      if (item_kind == ESMF_STATEITEM_FIELD) then
         call MAPL_FieldGet(field, geom=geom, _RC)
      else
         allocate(geom)
         call ESMF_FieldBundleGet(bundle, geom=geom, _RC)
      end if

      geom_id = MAPL_GeomGetId(geom, isPresent=has_id)
      if (has_id) then
         defaults = CharacteristicDefaults(geom=geom, geom_id=geom_id)
      else
         defaults = CharacteristicDefaults(geom=geom)
      end if
      if (item_kind == ESMF_STATEITEM_FIELD) then
         call realized%set(field, _RC)
      else
         call realized%set(bundle, _RC)
      end if
      call descriptor%harvest(realized, defaults, _RC)
      call descriptor%resolve_deferred(defaults, _RC)

      _RETURN(_SUCCESS)
   end subroutine fill_deferred_from_handle

   ! Gives the already-resolved parent/child geometry relationship (per
   ! geometry_spec%kind) graph structure: a dependency edge from the
   ! providing side to the receiving side. Does not re-decide
   ! own/from_parent/from_child; that is GeometrySpec's decision.
   module subroutine graphbuilder_resolve_geometry(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i, n
      character(:), allocatable :: child_name
      type(ComponentSpec), pointer :: this_spec
      type(ComponentSpec), pointer :: child_spec

      this_spec => this%get_component_spec()

      ! -- pull: this component receives geometry from a named child --
      if (this_spec%geometry_spec%kind == GEOMETRY_FROM_CHILD) then
         call resolve_geometry_from_child(this, this_spec%geometry_spec%provider, _RC)
      end if

      ! -- push: for each of this component's own children that wants
      ! geometry from its parent (this). Not back to this component's own
      ! provider: its geometry is the source of this one, and the push
      ! edge would close a cycle in the forward network. The skip is
      ! temporary, pending a geometry redesign. --
      n = this%get_num_children()
      do i = 1, n
         child_name = this%get_child_name(i, _RC)
         if (this_spec%geometry_spec%kind == GEOMETRY_FROM_CHILD) then
            if (child_name == this_spec%geometry_spec%provider) cycle
         end if
         child_spec => this%get_child_component_spec(child_name, _RC)
         if (child_spec%geometry_spec%kind == GEOMETRY_FROM_PARENT .and. this%has_geom()) then
            call push_geometry_to_child(this, child_name, _RC)
         end if
      end do

      _RETURN(_SUCCESS)
   end subroutine graphbuilder_resolve_geometry

   ! Pull direction: wires the named child's real geometry NodeId
   ! directly to this component's, like an ordinary cross-boundary
   ! MatchConnection (no parent-local proxy).
   module subroutine resolve_geometry_from_child(this, provider_name, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: provider_name
      integer, optional, intent(out) :: rc

      ! Pass rc through unwrapped so this wrapper adds no exception layer.
      call wire_geometry(this, provider_name, GEOMETRY_PULL, rc=rc)
   end subroutine resolve_geometry_from_child

   ! Push direction: adds a dependency edge, in the forward network, from
   ! this (parent)'s geometry NodeId to the named child's, through the
   ! CHILD's graph.
   module subroutine push_geometry_to_child(this, child_name, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: child_name
      integer, optional, intent(out) :: rc

      ! Pass rc through unwrapped so this wrapper adds no exception layer.
      call wire_geometry(this, child_name, GEOMETRY_PUSH, rc=rc)
   end subroutine push_geometry_to_child

   ! Adds the geometry edge between this component and the named child.
   ! The edge, and any extension chain, goes on the graph that owns the
   ! destination node: this_graph for PULL, the child's graph for PUSH
   ! (add_dependency admits the other side as a cross-component endpoint).
   subroutine wire_geometry(this, child_name, direction, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: child_name
      integer, intent(in) :: direction
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentGraph), pointer :: this_graph
      type(ComponentGraph), pointer :: child_graph
      type(ComponentGraph), pointer :: owner_graph
      type(DependencyNetworkId) :: net_id
      type(NodeId), pointer :: existing
      type(NodeId) :: this_own_id
      type(NodeId) :: child_own_id
      type(NodeId) :: source_id, dest_id
      type(NodeId) :: final_id
      type(NodeId), allocatable :: intermediate_ids(:)
      type(OuterMetaComponent), pointer :: child_meta
      type(StateItemCharacteristicMap), target :: this_map, child_map
      type(StateItemCharacteristicMap), pointer :: source_map, dest_map
      type(StateItemCharacteristicKind), allocatable :: mismatched(:)
      character(:), allocatable :: unsupported
      character(:), allocatable :: assertion_message
      type(GeomId) :: this_geom_id
      type(GeomId) :: child_geom_id

      this_graph => this%get_component_graph()
      existing => this_graph%get_resource_index(item_key(ESMF_STATEINTENT_EXPORT, GEOMETRY_ITEM_NAME))
      if (direction == GEOMETRY_PULL) then
         assertion_message = 'GraphBuilder: resolve_geometry_from_child called before this component advertised its own geometry'
      else
         assertion_message = 'GraphBuilder: push_geometry_to_child called before this component advertised its own geometry'
      end if
      _ASSERT(associated(existing), assertion_message)
      this_own_id = existing

      ! Same ComponentGraph that get_child_component_graph() returns.
      child_meta => this%get_child_outer_meta(child_name, _RC)
      child_graph => child_meta%get_component_graph()
      existing => child_graph%get_resource_index(item_key(ESMF_STATEINTENT_EXPORT, GEOMETRY_ITEM_NAME))
      if (direction == GEOMETRY_PULL) then
         assertion_message = 'GraphBuilder: resolve_geometry_from_child - child "' // child_name // &
              '" has not advertised its own geometry yet'
      else
         assertion_message = 'GraphBuilder: child "' // child_name // &
              '" has no geometry to receive into - has it run its own advertise_geometry yet?'
      end if
      _ASSERT(associated(existing), assertion_message)
      child_own_id = existing

      ! `this%get_geom_id()%get_value()` is not a valid designator
      ! (NAG rejects it); bind the intermediate result first.
      this_geom_id = this%get_geom_id()
      child_geom_id = child_meta%get_geom_id()
      call add_geom_proxy_identity(this_map, this_geom_id)
      call add_geom_proxy_identity(child_map, child_geom_id)

      if (direction == GEOMETRY_PULL) then
         owner_graph => this_graph
         source_id = child_own_id
         source_map => child_map
         dest_id = this_own_id
         dest_map => this_map
      else
         owner_graph => child_graph
         source_id = this_own_id
         source_map => this_map
         dest_id = child_own_id
         dest_map => child_map
      end if
      net_id = owner_graph%get_forward_network_id(_RC)

      ! Same dispatch shape as resolve_one: direct edge on
      ! exact match, else extension chain. A mismatch should be
      ! unreachable given GeometrySpec's exclusive resolution, but goes
      ! through the real path rather than being assumed away.
      mismatched = find_mismatched_characteristics(source_map, dest_map, _RC)
      if (size(mismatched) == 0) then
         call owner_graph%add_dependency(net_id, source_id, dest_id, _RC)
         _RETURN(_SUCCESS)
      end if

      call find_or_build_extension_chain(owner_graph, net_id, source_id, &
           source_map, dest_map, mismatched, final_id, intermediate_ids, unsupported, _RC)
      if (direction == GEOMETRY_PULL) then
         assertion_message = 'GraphBuilder: geometry from child ' // child_name // &
               ' does not match the receiving component own resolved geometry (unsupported: ' // &
               unsupported // ') - should be unreachable given GeometrySpec own exclusive resolution'
      else
         assertion_message = 'GraphBuilder: parent geometry does not match child ' // child_name // &
               ' own resolved geometry (unsupported: ' // unsupported // &
               ') - should be unreachable given GeometrySpec own exclusive resolution'
      end if
      _ASSERT(unsupported == '', assertion_message)
      ! No materialize_extension_chain call: component-level entries hold
      ! only a geom id, so the geometry build_transform reports unsupported
      ! and `unsupported` is never '' in practice. The assertion above flags
      ! this site if that changes.

      call owner_graph%add_dependency(net_id, final_id, dest_id, _RC)

      _RETURN(_SUCCESS)
   end subroutine wire_geometry

end submodule GraphBuilder_Geometry_smod
