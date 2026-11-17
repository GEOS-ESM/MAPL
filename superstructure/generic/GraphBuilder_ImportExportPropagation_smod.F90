#include "MAPL.h"

! Hierarchy propagation: mirrors each direct child's still-unresolved
! imports and exports up into this component's own import/export port
! tables, and drives the ReexportConnection passthrough hook.
!
! A direct sibling of GraphBuilder_ConnectionResolution_smod.F90 (both
! descend from mapl_GraphBuilder_mod directly): shared helpers it calls
! (push_warnings, find_export_node, mutable_descriptor, ...) are module
! procedures declared in mapl_GraphBuilder_mod's own interface block, not
! reached through submodule nesting.
submodule (mapl_GraphBuilder_mod) GraphBuilder_ImportExportPropagation_smod
   use esmf, only: ESMF_State, ESMF_StateCreate, ESMF_StateGet, &
        ESMF_FieldDestroy, &
        ESMF_STATEITEM_NOTFOUND, ESMF_STATEITEM_FIELD, &
        ESMF_STATEITEM_FIELDBUNDLE, ESMF_STATEITEM_STATE, &
        ESMF_STATEINTENT_INTERNAL, operator(==), operator(/=)
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind
   use mapl_ConnectionVector_mod, only: ConnectionVectorIterator, operator(/=)
   use mapl_ExpressionTransform_mod, only: ExpressionTransform
   use mapl_ExtensionResolution_mod, only: find_or_build_extension_chain, materialize_extensions_enabled
   use mapl_StateItemDescriptor_mod, only: resolve_descriptors, &
        resolve_characteristics
   use mapl_CharacteristicStatus_mod, only: CHARACTERISTIC_STATUS_MIRRORED, &
        CHARACTERISTIC_STATUS_DEFERRED, CHARACTERISTIC_STATUS_SPECIFIED, &
        operator(==)
   use mapl_StateItemCharacteristic_mod, only: &
        StateItemCharacteristicMapIterator, operator(/=)
   use mapl_FieldNamedAlias_mod, only: MAPL_NamedAlias
   use mapl_GraphMode_mod, only: graph_mode, GRAPH_MODE_REGISTRY
   use mapl_GraphOwnsUserStates_mod, only: graph_populates_user_states
   use mapl_ExtensionMaterialization_mod, only: field_extension_is_created
   use mapl_FieldCreate_mod, only: FieldsAreAliased
   use mapl_field_bundle_api, only: MAPL_FieldBundleDestroy
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_GraphResolutionEntry_mod, only: GraphResolutionEntry
   use mapl_GriddedComponentDriver_mod, only: GriddedComponentDriver
   use mapl_NodeIdSet_mod, only: NodeIdSet, NodeIdSetIterator, operator(/=)
   use mapl_PortIdNodeIdMap_mod, only: PortIdNodeIdMap, PortIdNodeIdMapIterator, operator(/=)
   use mapl_PortId_mod, only: PortId
   use mapl_StateArithmeticParser_mod, only: parser_variables_in_expression
   use mapl_TransformGraphNode_mod, only: TransformGraphNode
   use gFTL2_StringVector, only: StringVectorIterator, operator(/=)
   use pflogger, only: Logger
   use mapl_ReexportConnection_mod, only: ReexportConnection
   use mapl_ErrorHandling_mod
   implicit none(type, external)

contains

   ! For each direct child C of `this`, obtains C's still-unresolved imports
   ! (graphbuilder_unresolved_import_node_ids, via the framework-internal
   ! get_child_component_graph carve-out) and mirrors each into `this`'s
   ! import-port table under C's own NodeId: no new node or proxy, so
   ! for_each_matching_import finds it like any import `this` advertised.
   !
   ! Idempotent: ESMF Provide/Accept/Realize negotiation can invoke
   ! GENERIC_INIT_ADVERTISE (and so this hook) more than once per component;
   ! unguarded, it hangs under gfortran and re-logs warnings. A NodeId already
   ! present as a VALUE in this import-port table is skipped
   ! (already_mirrored); a repeat call re-derives the same set, so that check
   ! is sufficient.
   module subroutine graphbuilder_run_propagate_hook(this)
      class(OuterMetaComponent), target, intent(inout) :: this

      integer :: status
      integer :: i, num_children
      character(:), allocatable :: child_name
      type(OuterMetaComponent), pointer :: child_meta
      type(ComponentGraph), pointer :: this_graph
      type(UnresolvedImportEntry), allocatable :: unresolved(:)
      type(PortId) :: port_id
      integer :: j

      this_graph => this%get_component_graph()
      num_children = this%get_num_children()

      do i = 1, num_children
         child_name = this%get_child_name(i, status)
         call report_if_failed(this, 'run_propagate_hook:get_child_name', status)
         if (status /= 0) cycle

         child_meta => this%get_child_outer_meta(child_name, status)
         call report_if_failed(this, 'run_propagate_hook:get_child_outer_meta', status)
         if (status /= 0) cycle

         unresolved = graphbuilder_unresolved_import_node_ids(this, child_meta, status)
         call report_if_failed(this, 'run_propagate_hook:unresolved_import_node_ids', status)
         if (status /= 0) cycle

         do j = 1, size(unresolved)
            if (already_mirrored(this_graph, unresolved(j)%node_id)) cycle

            port_id = this_graph%next_port_id(status)
            call report_if_failed(this, 'run_propagate_hook:next_port_id', status)
            if (status /= 0) cycle
            call this_graph%add_import_port(port_id, unresolved(j)%node_id, status)
            call report_if_failed(this, 'run_propagate_hook:add_import_port', status)
         end do
      end do
   end subroutine graphbuilder_run_propagate_hook

   ! Idempotency helper for graphbuilder_run_propagate_hook above: does
   ! graph's own import-port table already contain target_id as a
   ! VALUE (not a key - PortId keys are always freshly minted, so a key
   ! check would never match)? A plain linear scan - this table is
   ! bounded by one component's own import count, not hierarchy-wide.
   function already_mirrored(graph, target_id) result(found)
      type(ComponentGraph), target, intent(in) :: graph
      type(NodeId), intent(in) :: target_id
      logical :: found

      type(PortIdNodeIdMap), target :: import_ports
      type(PortIdNodeIdMapIterator) :: iter
      type(NodeId) :: candidate

      found = .false.
      import_ports = graph%get_import_ports()
      associate (e => import_ports%ftn_end())
         iter = import_ports%ftn_begin()
         do while (iter /= e)
            call iter%next()
            candidate = iter%second()
            if (candidate == target_id) then
               found = .true.
               return
            end if
         end do
      end associate
   end function already_mirrored

   ! For each direct child C of `this`, propagates every export-port entry of
   ! C into `this`'s export-port table under C's own NodeId, with the
   ! qualified name from qualify_exported_name. Unconditional (no "already
   ! resolved" gate): an export's usefulness depends on whether some
   ! descendant's import matches it, which is not knowable here.
   module subroutine graphbuilder_run_export_propagate_hook(this)
      class(OuterMetaComponent), target, intent(inout) :: this

      integer :: status
      integer :: i, num_children
      character(:), allocatable :: child_name
      type(OuterMetaComponent), pointer :: child_meta
      type(ComponentGraph), pointer :: this_graph
      type(ComponentGraph), pointer :: child_graph
      type(PortIdNodeIdMap), target :: child_export_ports
      type(PortIdNodeIdMapIterator) :: iter
      type(PortId) :: child_port_id
      type(NodeId) :: export_node_id
      character(:), allocatable :: qualified_name
      type(PortId) :: new_port_id

      this_graph => this%get_component_graph()
      num_children = this%get_num_children()

      do i = 1, num_children
         child_name = this%get_child_name(i, status)
         call report_if_failed(this, 'run_export_propagate_hook:get_child_name', status)
         if (status /= 0) cycle

         child_meta => this%get_child_outer_meta(child_name, status)
         call report_if_failed(this, 'run_export_propagate_hook:get_child_outer_meta', status)
         if (status /= 0) cycle

         child_graph => child_meta%get_component_graph()
         child_export_ports = child_graph%get_export_ports()

         associate (e => child_export_ports%ftn_end())
            iter = child_export_ports%ftn_begin()
            do while (iter /= e)
               call iter%next()
               child_port_id = iter%first()
               export_node_id = iter%second()

               qualified_name = qualify_exported_name(child_graph, child_port_id, child_name)
               if (len(qualified_name) == 0) cycle

               if (already_exported(this_graph, export_node_id, qualified_name)) cycle

               new_port_id = this_graph%next_port_id(status)
               call report_if_failed(this, 'run_export_propagate_hook:next_port_id', status)
               if (status /= 0) cycle
               call this_graph%add_export_port(new_port_id, export_node_id, status, &
                    qualified_name=qualified_name)
               call report_if_failed(this, 'run_export_propagate_hook:add_export_port', status)
            end do
         end associate
      end do
   end subroutine graphbuilder_run_export_propagate_hook

   ! See the interface in GraphBuilder.F90. A repeat call (same name, same
   ! node) is a no-op.
   module subroutine graphbuilder_run_reexport_hook(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentSpec), pointer :: comp_spec
      type(ConnectionVectorIterator) :: iter
      class(Connection), pointer :: c

      comp_spec => this%get_component_spec()
      associate (e => comp_spec%connections%ftn_end())
         iter = comp_spec%connections%ftn_begin()
         do while (iter /= e)
            call iter%next()
            c => iter%of()
            select type (c)
            type is (ReexportConnection)
               call reexport_one(this, c, status)
               _VERIFY(status)
            class default
               ! Not a re-export.
            end select
         end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine graphbuilder_run_reexport_hook

   subroutine reexport_one(this, conn, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      class(Connection), intent(in) :: conn
      integer, intent(out) :: rc

      integer :: status
      type(ConnectionPt) :: src_pt, dst_pt
      type(OuterMetaComponent), pointer :: src_meta
      type(ComponentGraph), pointer :: src_graph, this_graph
      type(NodeId), pointer :: internal_id
      class(StateItemNode), pointer :: export_node
      type(NodeId) :: src_node_id, existing_id
      type(PortId) :: new_port_id
      character(:), allocatable :: dst_name, src_name, msg
      logical :: taken

      src_pt = conn%get_source()
      dst_pt = conn%get_destination()
      src_name = src_pt%v_pt%get_esmf_name()
      dst_name = dst_pt%v_pt%get_esmf_name()
      this_graph => this%get_component_graph()

      src_meta => resolve_qualified_component(this, src_pt%component_name, _RC)
      src_graph => src_meta%get_component_graph()

      if (src_pt%v_pt%is_internal()) then
         internal_id => src_graph%get_resource_index( &
              item_key(ESMF_STATEINTENT_INTERNAL, src_name))
         msg = 'Re-export source does not exist: ' // src_name
         _ASSERT(associated(internal_id), msg)
         src_node_id = internal_id
      else
         export_node => find_export_node(src_meta, src_name)
         msg = 'Re-export source does not exist: ' // src_name
         _ASSERT(associated(export_node), msg)
         src_node_id = export_node%get_node_id()
      end if

      call find_export_port_by_name(this_graph, dst_name, taken, existing_id)
      if (taken) then
         msg = 'Re-export destination already exists: ' // dst_name
         _ASSERT(existing_id == src_node_id, msg)
         _RETURN(_SUCCESS)
      end if

      new_port_id = this_graph%next_port_id(_RC)
      call this_graph%add_export_port(new_port_id, src_node_id, status, &
           qualified_name=dst_name)
      _VERIFY(status)

      _RETURN(_SUCCESS)
   end subroutine reexport_one

   subroutine find_export_port_by_name(graph, name, found, node_id)
      type(ComponentGraph), target, intent(in) :: graph
      character(*), intent(in) :: name
      logical, intent(out) :: found
      type(NodeId), intent(out) :: node_id

      type(PortIdNodeIdMap), target :: export_ports
      type(PortIdNodeIdMapIterator) :: iter

      found = .false.
      export_ports = graph%get_export_ports()
      associate (e => export_ports%ftn_end())
         iter = export_ports%ftn_begin()
         do while (iter /= e)
            call iter%next()
            if (graph%get_export_port_name(iter%first()) == name) then
               found = .true.
               node_id = iter%second()
               return
            end if
         end do
      end associate
   end subroutine find_export_port_by_name

   ! Name a child's export-port entry carries once propagated into the
   ! parent's export-port table. Mirrors legacy propagate_exports_virtual_pt:
   ! a name already qualified (propagated up from a grandchild) passes
   ! through unchanged; a child's own direct export is qualified as
   ! child_name // '/' // short_name.
   function qualify_exported_name(child_graph, child_port_id, child_name) result(qualified_name)
      type(ComponentGraph), target, intent(in) :: child_graph
      type(PortId), intent(in) :: child_port_id
      character(*), intent(in) :: child_name
      character(:), allocatable :: qualified_name

      character(:), allocatable :: own_name

      allocate(qualified_name, source='')
      own_name = child_graph%get_export_port_name(child_port_id)
      if (len(own_name) == 0) return

      ! An override-carrying name was set by an earlier propagation level and
      ! always has the form "descendant/short_name". get_export_port_name does
      ! not expose whether an override exists, so '/' in the name is the
      ! signal; child names are plain identifiers with no '/'.
      if (index(own_name, '/') > 0) then
         qualified_name = own_name
      else
         qualified_name = child_name // '/' // own_name
      end if
   end function qualify_exported_name

   ! Idempotency helper for graphbuilder_run_export_propagate_hook, symmetric
   ! to already_mirrored: is (target_id, target_name) already present in
   ! graph's export-port table? Both must be checked: in a diamond-shaped
   ! hierarchy one export NodeId can legitimately appear under several
   ! qualified names; only an identical pair indicates a repeat ADVERTISE
   ! call.
   function already_exported(graph, target_id, target_name) result(found)
      type(ComponentGraph), target, intent(in) :: graph
      type(NodeId), intent(in) :: target_id
      character(*), intent(in) :: target_name
      logical :: found

      type(PortIdNodeIdMap), target :: export_ports
      type(PortIdNodeIdMapIterator) :: iter
      type(PortId) :: candidate_port_id
      type(NodeId) :: candidate_id
      character(:), allocatable :: candidate_name

      found = .false.
      export_ports = graph%get_export_ports()
      associate (e => export_ports%ftn_end())
         iter = export_ports%ftn_begin()
         do while (iter /= e)
            call iter%next()
            candidate_port_id = iter%first()
            candidate_id = iter%second()
            if (.not. (candidate_id == target_id)) cycle
            candidate_name = graph%get_export_port_name(candidate_port_id)
            if (len(candidate_name) == 0) cycle
            if (candidate_name == target_name) then
               found = .true.
               return
            end if
         end do
      end associate
   end function already_exported

end submodule GraphBuilder_ImportExportPropagation_smod
