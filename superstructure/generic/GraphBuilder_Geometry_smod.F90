#include "MAPL.h"

! Horizontal geometry as a graph node, and its parent/child wiring.

submodule (mapl_GraphBuilder_mod) GraphBuilder_Geometry_smod
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind, GEOMETRY_CHARACTERISTIC_KIND
   use mapl_ExtensionResolution_mod, only: find_mismatched_characteristics, find_or_build_extension_chain
   use mapl_GeometryCharacteristic_mod, only: GeometryCharacteristic
   use mapl_GeometrySpec_mod, only: GEOMETRY_FROM_CHILD, GEOMETRY_FROM_PARENT
   use mapl_GeomId_mod, only: GeomId
   use mapl_GeomProxyField_mod, only: new_geom_proxy_item
   use mapl_GraphMode_mod, only: graph_mode, GRAPH_MODE_REGISTRY
   use mapl_ErrorHandling_mod
   implicit none(type, external)

contains

   module subroutine graphbuilder_run_geometry_hook(this)
      class(OuterMetaComponent), target, intent(inout) :: this

      integer :: status

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
      ! geometry from its parent (this) --
      n = this%get_num_children()
      do i = 1, n
         child_name = this%get_child_name(i, _RC)
         child_spec => this%get_child_component_spec(child_name, _RC)
         if (child_spec%geometry_spec%kind == GEOMETRY_FROM_PARENT) then
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

      integer :: status
      type(ComponentGraph), pointer :: this_graph
      type(ComponentGraph), pointer :: child_graph
      type(DependencyNetworkId) :: net_id
      type(NodeId), pointer :: existing
      type(NodeId) :: this_own_id
      type(NodeId) :: child_own_id
      type(NodeId) :: final_id
      type(NodeId), allocatable :: intermediate_ids(:)
      type(OuterMetaComponent), pointer :: child_meta
      type(StateItemCharacteristicMap), target :: this_map, child_map
      type(StateItemCharacteristicKind), allocatable :: mismatched(:)
      character(20) :: id_buffer
      character(:), allocatable :: unsupported
      character(:), allocatable :: assertion_message
      type(GeomId) :: this_own_geom_id
      type(GeomId) :: provider_geom_id

      this_graph => this%get_component_graph()
      net_id = this_graph%get_default_network_id()
      existing => this_graph%get_resource_index(item_key(ESMF_STATEINTENT_EXPORT, GEOMETRY_ITEM_NAME))
      _ASSERT(associated(existing), 'GraphBuilder: resolve_geometry_from_child called before this component advertised its own geometry')
      this_own_id = existing

      child_meta => this%get_child_outer_meta(provider_name, _RC)
      child_graph => child_meta%get_component_graph()
      existing => child_graph%get_resource_index(item_key(ESMF_STATEINTENT_EXPORT, GEOMETRY_ITEM_NAME))
      assertion_message = 'GraphBuilder: resolve_geometry_from_child - child "' // provider_name // &
           '" has not advertised its own geometry yet'
      _ASSERT(associated(existing), assertion_message)
      child_own_id = existing

      ! `this%get_geom_id()%get_value()` is not a valid designator
      ! (NAG rejects it); bind the intermediate result first.
      this_own_geom_id = this%get_geom_id()
      provider_geom_id = child_meta%get_geom_id()
      write(id_buffer, '(I0)') provider_geom_id%get_value()
      call child_map%insert(GEOMETRY_CHARACTERISTIC_KIND, GeometryCharacteristic(geom_id=trim(id_buffer)))
      write(id_buffer, '(I0)') this_own_geom_id%get_value()
      call this_map%insert(GEOMETRY_CHARACTERISTIC_KIND, GeometryCharacteristic(geom_id=trim(id_buffer)))

      ! Same dispatch shape as resolve_one (REQ-EXT-001): direct edge on
      ! exact match, else extension chain. A mismatch should be
      ! unreachable given GeometrySpec's exclusive resolution, but goes
      ! through the real path rather than being assumed away.
      mismatched = find_mismatched_characteristics(child_map, this_map)
      if (size(mismatched) == 0) then
         call this_graph%add_dependency(net_id, child_own_id, this_own_id, _RC)
         _RETURN(_SUCCESS)
      end if

      call find_or_build_extension_chain(this_graph, net_id, child_own_id, &
           child_map, this_map, mismatched, final_id, intermediate_ids, unsupported, _RC)
      assertion_message = 'GraphBuilder: geometry from child ' // provider_name // &
            ' does not match the receiving component own resolved geometry (unsupported: ' // &
            unsupported // ') - should be unreachable given GeometrySpec own exclusive resolution'
      _ASSERT(unsupported == '', assertion_message)
      ! No materialize_extension_chain call: GeometryCharacteristic's
      ! build_transform always fails (no horizontal-regrid Transform
      ! exists), so `unsupported` is never '' in practice. The assertion
      ! above flags this site if that changes.

      call this_graph%add_dependency(net_id, final_id, this_own_id, _RC)

      _RETURN(_SUCCESS)
   end subroutine resolve_geometry_from_child

   ! Push direction: adds a dependency edge from this (parent)'s
   ! geometry NodeId to the named child's, in the CHILD's graph/network,
   ! via the framework-internal get_child_component_graph() carve-out
   ! (REQ-GB-002).
   module subroutine push_geometry_to_child(this, child_name, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: child_name
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentGraph), pointer :: this_graph
      type(ComponentGraph), pointer :: child_graph
      type(DependencyNetworkId) :: child_net_id
      type(NodeId), pointer :: existing
      type(NodeId) :: this_own_id
      type(NodeId) :: child_own_id
      type(NodeId) :: final_id
      type(NodeId), allocatable :: intermediate_ids(:)
      type(OuterMetaComponent), pointer :: child_meta
      type(StateItemCharacteristicMap), target :: this_map, child_map
      type(StateItemCharacteristicKind), allocatable :: mismatched(:)
      character(20) :: id_buffer
      character(:), allocatable :: unsupported
      character(:), allocatable :: assertion_message
      type(GeomId) :: this_geom_id
      type(GeomId) :: child_geom_id

      this_graph => this%get_component_graph()
      existing => this_graph%get_resource_index(item_key(ESMF_STATEINTENT_EXPORT, GEOMETRY_ITEM_NAME))
      _ASSERT(associated(existing), 'GraphBuilder: push_geometry_to_child called before this component advertised its own geometry')
      this_own_id = existing

      child_graph => this%get_child_component_graph(child_name, _RC)
      child_meta => this%get_child_outer_meta(child_name, _RC)

      existing => child_graph%get_resource_index(item_key(ESMF_STATEINTENT_EXPORT, GEOMETRY_ITEM_NAME))
      assertion_message = 'GraphBuilder: child "' // child_name // &
            '" has no geometry to receive into - has it run its own advertise_geometry yet?'
      _ASSERT(associated(existing), assertion_message)
      child_own_id = existing

      ! Bind intermediates first; see resolve_geometry_from_child.
      this_geom_id = this%get_geom_id()
      child_geom_id = child_meta%get_geom_id()
      write(id_buffer, '(I0)') this_geom_id%get_value()
      call this_map%insert(GEOMETRY_CHARACTERISTIC_KIND, GeometryCharacteristic(geom_id=trim(id_buffer)))
      write(id_buffer, '(I0)') child_geom_id%get_value()
      call child_map%insert(GEOMETRY_CHARACTERISTIC_KIND, GeometryCharacteristic(geom_id=trim(id_buffer)))

      ! Same dispatch shape as resolve_geometry_from_child. The edge (and
      ! any chain) lives in the CHILD's graph/network because the
      ! receiving node is a child_graph node; add_dependency admits
      ! this_own_id as a cross-component endpoint.
      child_net_id = child_graph%get_default_network_id()
      mismatched = find_mismatched_characteristics(this_map, child_map)
      if (size(mismatched) == 0) then
         call child_graph%add_dependency(child_net_id, this_own_id, child_own_id, _RC)
         _RETURN(_SUCCESS)
      end if

      call find_or_build_extension_chain(child_graph, child_net_id, this_own_id, &
           this_map, child_map, mismatched, final_id, intermediate_ids, unsupported, _RC)
      assertion_message = 'GraphBuilder: parent geometry does not match child ' // child_name // &
            ' own resolved geometry (unsupported: ' // unsupported // &
            ') - should be unreachable given GeometrySpec own exclusive resolution'
      _ASSERT(unsupported == '', assertion_message)
      ! No materialization; see resolve_geometry_from_child.

      call child_graph%add_dependency(child_net_id, final_id, child_own_id, _RC)

      _RETURN(_SUCCESS)
   end subroutine push_geometry_to_child

end submodule GraphBuilder_Geometry_smod
