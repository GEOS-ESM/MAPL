#include "MAPL.h"

! Horizontal geometry (openspec/changes/horizontal-geometry-graph-state-item, Phase 4e)
! (openspec/changes/graphbuilder-submodule-split: procedure bodies moved
! here verbatim from GraphBuilder.F90, mechanical subroutine->module
! subroutine / function->module function change only - see that
! change's design.md D1 for the full section-to-file mapping.)

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

   ! Wraps this%get_geom() (already resolved by GeometrySpec/
   ! initialize_geom_a.F90/initialize_geom_b.F90, long before this ever
   ! runs) as an ordinary StateItemNode in this component's own local
   ! graph. Idempotent, mirroring advertise_one's own re-advertisement
   ! guard.
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

   ! Reserved key for the proxy, in a child's own local graph, standing
   ! in for that child's parent's geometry item - the push-direction
   ! mirror of proxy_key()'s own pull-direction convention.
   module function parent_geometry_proxy_key() result(key)
      character(:), allocatable :: key

      key = 'PROXY:' // PARENT_PROXY_NAME // ':' // item_key(ESMF_STATEINTENT_EXPORT, GEOMETRY_ITEM_NAME)
   end function parent_geometry_proxy_key

   ! Gives the already-resolved parent/child geometry relationship (per
   ! this%get_component_spec()%geometry_spec%kind) real graph structure:
   ! a dependency edge from whichever side provides to whichever side
   ! receives. Does not re-decide own/from_parent/from_child - that
   ! decision is GeometrySpec's, already made in phases 3-4.
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

   ! Pull direction: reuses get_or_make_local_node_id() unmodified -
   ! structurally identical to an ordinary cross-boundary MatchConnection
   ! pulling a named child's item into this component's own graph.
   module subroutine resolve_geometry_from_child(this, provider_name, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: provider_name
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentGraph), pointer :: this_graph
      type(DependencyNetworkId) :: net_id
      type(NodeId), pointer :: existing
      type(NodeId) :: this_own_id
      type(NodeId) :: child_proxy_id
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

      child_proxy_id = get_or_make_local_node_id(this, provider_name, ESMF_STATEINTENT_EXPORT, GEOMETRY_ITEM_NAME, _RC)

      child_meta => this%get_child_outer_meta(provider_name, _RC)

      ! Fortran does not permit invoking a type-bound procedure directly
      ! on the (non-pointer, non-variable) result of another type-bound
      ! function call - `this%get_geom_id()%get_value()` is not a valid
      ! designator. Bind each intermediate result to a named variable
      ! first instead (confirmed by bisection against NAG's rejection).
      this_own_geom_id = this%get_geom_id()
      provider_geom_id = child_meta%get_geom_id()
      write(id_buffer, '(I0)') provider_geom_id%get_value()
      call child_map%insert(GEOMETRY_CHARACTERISTIC_KIND, GeometryCharacteristic(geom_id=trim(id_buffer)))
      write(id_buffer, '(I0)') this_own_geom_id%get_value()
      call this_map%insert(GEOMETRY_CHARACTERISTIC_KIND, GeometryCharacteristic(geom_id=trim(id_buffer)))

      ! Same dispatch shape as resolve_one's own REQ-EXT-001/003/005
      ! handling (mirrored, not duplicated as a bespoke check): no-op on
      ! exact match, else delegate to extension-reuse rather than wiring
      ! the mismatched pair directly - here expected to be unreachable
      ! given GeometrySpec's own exclusive resolution (design.md D4), but
      ! handled through the real path rather than assumed away.
      mismatched = find_mismatched_characteristics(child_map, this_map)
      if (size(mismatched) == 0) then
         call this_graph%add_dependency(net_id, child_proxy_id, this_own_id, _RC)
         _RETURN(_SUCCESS)
      end if

      call find_or_build_extension_chain(this_graph, net_id, child_proxy_id, &
           child_map, this_map, mismatched, final_id, intermediate_ids, unsupported, _RC)
      assertion_message = 'GraphBuilder: geometry from child ' // provider_name // &
            ' does not match the receiving component own resolved geometry (unsupported: ' // &
            unsupported // ') - should be unreachable given GeometrySpec own exclusive resolution'
      _ASSERT(unsupported == '', assertion_message)
      ! openspec/changes/phase-11b-chain-intermediate-materialization:
      ! intentionally no materialize_extension_chain call here - this
      ! branch is asserted unreachable immediately above
      ! (GeomCharacteristic%build_transform always fails, so unsupported
      ! is never '' in practice; the assertion exists for the case where
      ! that stops being true without this call site being revisited).
      ! No real horizontal-regrid Transform exists yet (Phase 13,
      ! REQ-GEO-002a) - adding real materialization here is deferred to
      ! that future capability, not attempted by this change.

      call this_graph%add_dependency(net_id, final_id, this_own_id, _RC)

      _RETURN(_SUCCESS)
   end subroutine resolve_geometry_from_child

   ! Push direction: get_or_make_local_node_id() cannot express this (it
   ! only pulls a NAMED CHILD's item into the CALLER's own graph) - this
   ! is the symmetric case, injecting a proxy for the PARENT (this) into
   ! the CHILD's own graph, using the same framework-internal
   ! get_child_component_graph() carve-out (REQ-GB-002).
   module subroutine push_geometry_to_child(this, child_name, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: child_name
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentGraph), pointer :: this_graph
      type(ComponentGraph), pointer :: child_graph
      type(DependencyNetworkId) :: child_net_id
      type(NodeId), pointer :: existing
      type(NodeId) :: child_own_id
      type(NodeId) :: parent_proxy_id
      type(NodeId) :: final_id
      type(NodeId), allocatable :: intermediate_ids(:)
      character(:), allocatable :: pkey
      type(OuterMetaComponent), pointer :: child_meta
      type(StateItemCharacteristicMap), target :: this_map, child_map
      type(StateItemCharacteristicKind), allocatable :: mismatched(:)
      character(20) :: id_buffer
      type(StateItemNode) :: proxy_node
      type(GraphStateItem) :: payload
      type(NodeRevision) :: revision
      character(:), allocatable :: unsupported
      character(:), allocatable :: assertion_message
      type(GeomId) :: this_geom_id
      type(GeomId) :: child_geom_id

      this_graph => this%get_component_graph()
      existing => this_graph%get_resource_index(item_key(ESMF_STATEINTENT_EXPORT, GEOMETRY_ITEM_NAME))
      _ASSERT(associated(existing), 'GraphBuilder: push_geometry_to_child called before this component advertised its own geometry')
      ! Existence is all that matters here (this component must have
      ! already advertised its own geometry before it can push it into a
      ! child) - unlike resolve_geometry_from_child, this component's own
      ! NodeId is not itself an endpoint of the dependency edge added
      ! below, so it is not retained past this check.

      child_graph => this%get_child_component_graph(child_name, _RC)
      child_meta => this%get_child_outer_meta(child_name, _RC)

      existing => child_graph%get_resource_index(item_key(ESMF_STATEINTENT_EXPORT, GEOMETRY_ITEM_NAME))
      assertion_message = 'GraphBuilder: child "' // child_name // &
            '" has no geometry to receive into - has it run its own advertise_geometry yet?'
      _ASSERT(associated(existing), assertion_message)
      child_own_id = existing

      pkey = parent_geometry_proxy_key()
      existing => child_graph%get_resource_index(pkey)
      if (associated(existing)) then
         parent_proxy_id = existing
      else
         parent_proxy_id = child_graph%next_node_id(_RC)
         proxy_node = StateItemNode(parent_proxy_id, payload, revision)
         call child_graph%register_node(proxy_node, _RC)
         call child_graph%add_resource_index(pkey, parent_proxy_id, _RC)
      end if

      ! See resolve_geometry_from_child's own comment above - same
      ! not-a-valid-designator reason for binding each intermediate
      ! result to a named variable first.
      this_geom_id = this%get_geom_id()
      child_geom_id = child_meta%get_geom_id()
      write(id_buffer, '(I0)') this_geom_id%get_value()
      call this_map%insert(GEOMETRY_CHARACTERISTIC_KIND, GeometryCharacteristic(geom_id=trim(id_buffer)))
      write(id_buffer, '(I0)') child_geom_id%get_value()
      call child_map%insert(GEOMETRY_CHARACTERISTIC_KIND, GeometryCharacteristic(geom_id=trim(id_buffer)))

      ! Same dispatch shape as resolve_one's own REQ-EXT-001/003/005
      ! handling, and as resolve_geometry_from_child's own pull-direction
      ! mirror above - the edge (and, if mismatched, any extension chain)
      ! lives in the CHILD's own graph/network, since both its endpoints
      ! (parent_proxy_id, child_own_id) are child_graph nodes.
      child_net_id = child_graph%get_default_network_id()
      mismatched = find_mismatched_characteristics(this_map, child_map)
      if (size(mismatched) == 0) then
         call child_graph%add_dependency(child_net_id, parent_proxy_id, child_own_id, _RC)
         _RETURN(_SUCCESS)
      end if

      call find_or_build_extension_chain(child_graph, child_net_id, parent_proxy_id, &
           this_map, child_map, mismatched, final_id, intermediate_ids, unsupported, _RC)
      assertion_message = 'GraphBuilder: parent geometry does not match child ' // child_name // &
            ' own resolved geometry (unsupported: ' // unsupported // &
            ') - should be unreachable given GeometrySpec own exclusive resolution'
      _ASSERT(unsupported == '', assertion_message)
      ! openspec/changes/phase-11b-chain-intermediate-materialization:
      ! same as resolve_geometry_from_child's own note above - this
      ! branch is asserted unreachable; no real materialization added
      ! here, deferred to a future horizontal-regrid capability.

      call child_graph%add_dependency(child_net_id, final_id, child_own_id, _RC)

      _RETURN(_SUCCESS)
   end subroutine push_geometry_to_child

end submodule GraphBuilder_Geometry_smod
