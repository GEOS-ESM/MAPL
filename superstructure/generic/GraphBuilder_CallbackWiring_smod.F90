#include "MAPL.h"

! Callback wiring (openspec/changes/callback-wiring, Phase 4d)
! (openspec/changes/graphbuilder-submodule-split: procedure bodies moved
! here verbatim from GraphBuilder.F90, mechanical subroutine->module
! subroutine / function->module function change only - see that
! change's design.md D1 for the full section-to-file mapping.)

submodule (mapl_GraphBuilder_mod) GraphBuilder_CallbackWiring_smod
   use esmf, only: ESMF_State, ESMF_StateCreate
   use mapl_AccessSpec_mod, only: AccessSpec, MAPL_ACCESS_IN, MAPL_ACCESS_INOUT, MAPL_ACCESS_OUT, operator(==)
   use mapl_AccessSpecMap_mod, only: AccessSpecMap, AccessSpecMapIterator, operator(/=)
   use mapl_CallbackArgumentSpec_mod, only: CallbackArgumentSpec
   use mapl_CallbackArgumentSpecMap_mod, only: CallbackArgumentSpecMap, CallbackArgumentSpecMapIterator, operator(/=)
   use mapl_CallbackMethodSpec_mod, only: CallbackMethodSpec
   use mapl_CharacteristicId_mod, only: CharacteristicId
   use mapl_ExtensionResolution_mod, only: find_mismatched_characteristics, find_or_build_extension_chain
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_MethodGraphNode_mod, only: MethodGraphNode
   use mapl_StateItemFlag_mod, only: MAPL_StateItem_Flag
   use mapl_StateItemMemberMap_mod, only: StateItemMemberMapIterator, operator(/=)
   use mapl_StateMethodInvocation_mod, only: StateMethodInvocation
   use mapl_VariableSpecVector_mod, only: VariableSpecVectorIterator, operator(/=)
   use mapl_ErrorHandling_mod
   implicit none(type, external)

contains

   ! REQ-CB-013/014: builds root's own flattened qualified-export
   ! namespace by recursing across every descendant level (design.md
   ! Decision 1 - a fresh, uncached snapshot at resolution time; REQ-CB-016's
   ! own "currently-known" phrasing). root's own exports are keyed by
   ! their own short name; a descendant's export is additionally keyed
   ! by comp_path/short_name, composing one segment per hierarchy level.
   module function build_qualified_export_namespace(root, rc) result(entries)
      class(OuterMetaComponent), target, intent(inout) :: root
      integer, optional, intent(out) :: rc
      type(QualifiedExportEntry), allocatable :: entries(:)

      integer :: status

      allocate(entries(0))
      call collect_qualified_exports(root, '', entries, _RC)

      _RETURN(_SUCCESS)
   end function build_qualified_export_namespace

   recursive module subroutine collect_qualified_exports(owner, path_prefix, entries, rc)
      class(OuterMetaComponent), target, intent(inout) :: owner
      character(*), intent(in) :: path_prefix
      type(QualifiedExportEntry), allocatable, intent(inout) :: entries(:)
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i, num_children
      type(ComponentSpec), pointer :: comp_spec
      type(VariableSpecVectorIterator) :: iter
      type(VariableSpec), pointer :: var_spec
      character(:), allocatable :: child_name, child_path
      type(OuterMetaComponent), pointer :: child_meta
      type(QualifiedExportEntry) :: entry
      type(QualifiedExportEntry), allocatable :: grown_entries(:)
      integer :: n

      comp_spec => owner%get_component_spec()

      associate (e => comp_spec%var_specs%ftn_end())
         iter = comp_spec%var_specs%ftn_begin()
         do while (iter /= e)
            call iter%next()
            var_spec => iter%of()
            if (.not. (var_spec%state_intent == ESMF_STATEINTENT_EXPORT)) cycle
            entry%comp_path = path_prefix
            if (len(path_prefix) == 0) then
               entry%qualified_name = var_spec%short_name
            else
               entry%qualified_name = path_prefix // '/' // var_spec%short_name
            end if
            entry%var_spec = var_spec
            ! Classic ifort (2021.13, -O0) segfaults compiling the more
            ! natural self-referencing array-constructor growth
            ! `entries = [entries, entry]` here, where entries is an
            ! allocatable array of a derived type (QualifiedExportEntry)
            ! with a nested allocatable-component type (VariableSpec) -
            ! bisected against this file's own ICE (gfortran, NAG, and
            ! ifx all accept the array-constructor form without issue).
            ! Grow via an explicit fresh temporary and move_alloc
            ! instead - functionally identical, but never assigns the
            ! array constructor result back onto one of its own operands.
            n = size(entries)
            allocate(grown_entries(n + 1))
            ! NAG (observed 7.2.41, -O0 Debug): a zero-extent array-section
            ! assignment (grown_entries(1:0) = entries, n==0) of this
            ! derived type - QualifiedExportEntry nests a VariableSpec,
            ! which nests a VariableSpecMemberMap (polymorphic, deep_copy
            ! defined assignment, recursively self-referential) - triggers
            ! a real bus error (invalid address alignment) in the
            ! compiler-generated whole-array defined-assignment dispatch,
            ! even though the assigned section has no elements. Guarding
            ! the n==0 case (nothing to copy) avoids the bad codegen path
            ! entirely; functionally a no-op either way.
            if (n > 0) grown_entries(1:n) = entries
            grown_entries(n + 1) = entry
            call move_alloc(grown_entries, entries)
         end do
      end associate

      num_children = owner%get_num_children()
      do i = 1, num_children
         child_name = owner%get_child_name(i, _RC)
         child_meta => owner%get_child_outer_meta(child_name, _RC)
         if (len(path_prefix) == 0) then
            child_path = child_name
         else
            child_path = path_prefix // '/' // child_name
         end if
         call collect_qualified_exports(child_meta, child_path, entries, _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine collect_qualified_exports

   ! openspec/changes/graph-builder-resolution-index, design.md Decisions
   ! "resolve_qualified_component is resolve_descendant, relocated and
   ! renamed, with no algorithm change": a one-line forward to the
   ! shared primitive now living in GraphBuilder.F90's own
   ! shared-helpers section - this entry point itself is kept so every
   ! existing call site in this submodule
   ! (materialize_callback_collection/build_callback_state_binding) needs
   ! no further change. comp_path MUST be non-empty - callers guard
   ! this; an empty comp_path means "root itself," never routed here.
   module function resolve_descendant(root, comp_path, rc) result(meta)
      class(OuterMetaComponent), target, intent(inout) :: root
      character(*), intent(in) :: comp_path
      integer, optional, intent(out) :: rc
      type(OuterMetaComponent), pointer :: meta

      integer :: status

      meta => resolve_qualified_component(root, comp_path, _RC)

      _RETURN(_SUCCESS)
   end function resolve_descendant

   ! Resolves entry (found under a namespace rooted at src_component_name,
   ! relative to `this`) to a NodeId usable directly in `this`'s own
   ! graph: `this`'s own export with no further descendant path resolves
   ! to its already-advertised NodeId directly; every other case resolves
   ! through a cached-or-newly-created parent-local proxy StateItemNode
   ! (REQ-HIER-006), generalizing get_or_make_local_node_id's own
   ! single-level proxy shape to the full path from `this` down to the
   ! real owner (src_component_name, then entry%comp_path's own further
   ! segments).
   module function resolve_qualified_export_node_id(this, src_component_name, entry, rc) result(node_id)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: src_component_name
      type(QualifiedExportEntry), intent(in) :: entry
      integer, optional, intent(out) :: rc
      type(NodeId) :: node_id

      integer :: status
      character(:), allocatable :: full_path
      type(ComponentGraph), pointer :: this_graph, owner_graph
      character(:), allocatable :: pkey
      type(NodeId), pointer :: existing
      type(NodeId), pointer :: owner_item_id
      type(OuterMetaComponent), pointer :: owner_meta
      type(StateItemNode) :: proxy_node
      type(GraphStateItem) :: payload
      type(NodeRevision) :: revision
      type(PortId) :: port_id

      this_graph => this%get_component_graph()

      if (is_self(this, src_component_name) .and. len(entry%comp_path) == 0) then
         existing => this_graph%get_resource_index(item_key(ESMF_STATEINTENT_EXPORT, entry%var_spec%short_name))
         _ASSERT(associated(existing), 'GraphBuilder: callback wiring references an export this component never advertised: ' // entry%var_spec%short_name)
         node_id = existing
         _RETURN(_SUCCESS)
      end if

      if (is_self(this, src_component_name)) then
         full_path = entry%comp_path
      else if (len(entry%comp_path) == 0) then
         full_path = src_component_name
      else
         full_path = src_component_name // '/' // entry%comp_path
      end if

      pkey = 'CALLBACK_PROXY:' // full_path // ':' // item_key(ESMF_STATEINTENT_EXPORT, entry%var_spec%short_name)
      existing => this_graph%get_resource_index(pkey)
      if (associated(existing)) then
         node_id = existing
         _RETURN(_SUCCESS)
      end if

      owner_meta => resolve_descendant(this, full_path, _RC)
      owner_graph => owner_meta%get_component_graph()
      owner_item_id => owner_graph%get_resource_index(item_key(ESMF_STATEINTENT_EXPORT, entry%var_spec%short_name))
      _ASSERT(associated(owner_item_id), 'GraphBuilder: callback wiring references an export never advertised at path ' // full_path)

      node_id = this_graph%next_node_id(_RC)
      proxy_node = StateItemNode(node_id, payload, revision)
      call this_graph%register_node(proxy_node, _RC)
      call this_graph%add_resource_index(pkey, node_id, _RC)

      port_id = this_graph%next_port_id(_RC)
      call this_graph%add_child_port_binding(port_id, node_id, _RC)

      _RETURN(_SUCCESS)
   end function resolve_qualified_export_node_id

   ! design.md Decision 4: an export "implements" expected_iface when its
   ! own declared composite members (composite-state-spec's
   ! declare_member tree) cover every argument name expected_iface
   ! declares, with each member's own itemType matching that argument's
   ! expected_kind (compared by name - MAPL_StateItem_Flag's variant-flag
   ! vocabulary and itemtype_name()'s existing ESMF-domain vocabulary
   ! already agree on FIELD/FIELDBUNDLE/STATE for the base kinds this
   ! check needs). Method presence (get/put, ...) is not checked here -
   ! CallbackStateBinding%bind_method already reports a missing method
   ! attachment explicitly at binding time (design.md Risks).
   module function export_conforms_to_interface(var_spec, expected_iface, reason) result(conforms)
      type(VariableSpec), target, intent(in) :: var_spec
      type(CallbackInterface), target, intent(in) :: expected_iface
      character(:), allocatable, intent(out) :: reason
      logical :: conforms

      type(CallbackArgumentSpecMap), target :: arguments
      type(CallbackArgumentSpecMapIterator) :: iter
      character(:), allocatable :: argument_name
      type(CallbackArgumentSpec) :: arg_spec
      type(MAPL_StateItem_Flag) :: expected_kind
      type(VariableSpec) :: member
      integer :: status

      conforms = .true.
      reason = ''

      arguments = expected_iface%get_arguments()
      iter = arguments%ftn_begin()
      do while (iter /= arguments%ftn_end())
         call iter%next()
         argument_name = iter%first()
         arg_spec = iter%second()

         if (.not. var_spec%has_member(argument_name)) then
            conforms = .false.
            reason = 'missing_member:' // argument_name
            return
         end if

         member = var_spec%get_member(argument_name, rc=status)
         if (status /= 0) then
            conforms = .false.
            reason = 'unreadable_member:' // argument_name
            return
         end if

         expected_kind = arg_spec%get_expected_kind()
         if (itemtype_name(member%itemType) /= expected_kind%to_string()) then
            conforms = .false.
            reason = 'kind_mismatch:' // argument_name
            return
         end if
      end do
   end function export_conforms_to_interface

   ! REQ-CB-016 "materialize a flat callback collection": builds one new
   ! StateItemNode (a real, structurally-empty ESMF_State, mirroring
   ! composite-state-spec's own nested-level materialization, design.md
   ! Decision 5) in this's own graph whose state_members map has one
   ! entry per resolved match, keyed by that match's own qualified name
   ! (unique within one resolution by construction) mapping to that
   ! match's own destination-local NodeId
   ! (resolve_qualified_export_node_id above). Built directly, not
   ! routed through CompositeStateMaterialization's VariableSpec-tree
   ! walk (design.md Decision 5's own rationale).
   module function materialize_callback_collection(this, src_pt, matches, rc) result(collection_id)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(ConnectionPt), intent(in) :: src_pt
      type(QualifiedExportEntry), intent(in) :: matches(:)
      integer, optional, intent(out) :: rc
      type(NodeId) :: collection_id

      integer :: status
      type(ComponentGraph), pointer :: graph
      type(ESMF_State) :: state
      type(GraphStateItem) :: payload
      type(NodeRevision) :: revision
      type(StateItemNode) :: node
      type(NodeId) :: member_id
      integer :: i

      graph => this%get_component_graph()

      state = ESMF_StateCreate(_RC)
      call payload%set(state, _RC)

      do i = 1, size(matches)
         member_id = resolve_qualified_export_node_id(this, src_pt%component_name, matches(i), _RC)
         call payload%add_state_member(matches(i)%qualified_name, member_id, _RC)
      end do

      collection_id = graph%next_node_id(_RC)
      node = StateItemNode(collection_id, payload, revision)
      call graph%register_node(node, _RC)

      _RETURN(_SUCCESS)
   end function materialize_callback_collection

   ! Task 6 / design.md Context: the first real, non-test caller of
   ! CallbackStateBinding%bind_argument, using the matched callback
   ! state's own already-materialized composite member NodeIds - read
   ! directly from its owning component's own graph via
   ! GraphStateItem%state_members(), not re-derived from VariableSpec
   ! and not caller-supplied stand-ins.
   module function build_callback_state_binding(this, src_component_name, entry, expected_interface_id, &
        expected_iface, rc) result(binding)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: src_component_name
      type(QualifiedExportEntry), intent(in) :: entry
      type(CallbackInterfaceId), intent(in) :: expected_interface_id
      type(CallbackInterface), intent(in) :: expected_iface
      integer, optional, intent(out) :: rc
      type(CallbackStateBinding) :: binding

      integer :: status
      character(:), allocatable :: full_path
      class(OuterMetaComponent), pointer :: owner_meta
      type(ComponentGraph), pointer :: owner_graph
      type(NodeId), pointer :: state_node_id_ptr
      type(NodeId) :: state_node_id
      class(GraphNode), pointer :: state_node
      type(GraphStateItem) :: state_payload
      type(StateItemMemberMap) :: members
      type(CallbackArgumentSpecMap), target :: arguments
      type(CallbackArgumentSpecMapIterator) :: iter
      character(:), allocatable :: argument_name
      type(NodeId), pointer :: member_node_id

      if (is_self(this, src_component_name)) then
         full_path = entry%comp_path
      else if (len(entry%comp_path) == 0) then
         full_path = src_component_name
      else
         full_path = src_component_name // '/' // entry%comp_path
      end if

      if (len(full_path) == 0) then
         owner_meta => this
      else
         owner_meta => resolve_descendant(this, full_path, _RC)
      end if
      owner_graph => owner_meta%get_component_graph()

      state_node_id_ptr => owner_graph%get_resource_index(item_key(ESMF_STATEINTENT_EXPORT, entry%var_spec%short_name))
      _ASSERT(associated(state_node_id_ptr), 'GraphBuilder: callback state binding - export was never advertised: ' // entry%qualified_name)
      state_node_id = state_node_id_ptr

      state_node => owner_graph%get_node(state_node_id)
      _ASSERT(associated(state_node), 'GraphBuilder: callback state binding - state node not found')
      select type (state_node)
      class is (StateItemNode)
         state_payload = state_node%get_payload()
      class default
         _ASSERT(.false., 'GraphBuilder: callback state binding - state node is not a StateItemNode')
      end select
      members = state_payload%state_members()

      binding = CallbackStateBinding(expected_interface_id, expected_iface, state_node_id)

      arguments = expected_iface%get_arguments()
      iter = arguments%ftn_begin()
      do while (iter /= arguments%ftn_end())
         call iter%next()
         argument_name = iter%first()

         member_node_id => members%at(argument_name)
         _ASSERT(associated(member_node_id), 'GraphBuilder: callback state binding - member not found for argument: ' // argument_name)
         call binding%bind_argument(argument_name, member_node_id, _RC)
      end do

      _RETURN(_SUCCESS)
   end function build_callback_state_binding

   ! REQ-CB-018/019: builds one CallbackMethodBinding for method_name on
   ! state_binding (a CallbackStateBinding already bound to a real
   ! callback state via build_callback_state_binding, task 6) - registers
   ! a MethodGraphNode (holding the StateMethodInvocation attachment
   ! CallbackStateBinding%bind_method already produced) in this's own
   ! graph, creates a get network and a put network via
   ! ComponentGraph%create_network(), and wires each declared argument's
   ! dependency edge into the appropriate network by access mode,
   ! reusing find_mismatched_characteristics/find_or_build_extension_chain
   ! exactly like ordinary connection resolution (design.md Decisions) -
   ! both sides' CharacteristicMaps are always empty at this data-model
   ! tier (CallbackArgumentSpec has no units/vgrid declaration yet), so
   ! this always takes the exact-match direct-edge branch today; the
   ! transform-chain branch remains available for a future
   ! characteristics-aware CallbackArgumentSpec.
   module function build_callback_method_binding(this, iface, method_name, state_binding, &
        provider_get_ids, provider_put_ids, rc) result(binding)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(CallbackInterface), intent(in) :: iface
      character(*), intent(in) :: method_name
      type(CallbackStateBinding), target, intent(in) :: state_binding
      type(StateItemMemberMap), intent(in) :: provider_get_ids
      type(StateItemMemberMap), intent(in) :: provider_put_ids
      integer, optional, intent(out) :: rc
      type(CallbackMethodBinding) :: binding

      integer :: status
      type(ComponentGraph), pointer :: graph
      type(DependencyNetworkId) :: get_net_id, put_net_id
      type(StateMethodInvocation), pointer :: attachment
      type(NodeId) :: method_node_id
      type(MethodGraphNode) :: method_node
      type(CallbackMethodSpec) :: method_spec
      type(AccessSpecMap), target :: accesses
      type(AccessSpecMapIterator) :: iter
      character(:), allocatable :: argument_name
      type(AccessSpec) :: access
      type(CallbackArgumentSpec) :: arg_spec
      type(MAPL_StateItem_Flag) :: expected_kind
      type(NodeId), pointer :: member_id
      type(NodeId), pointer :: provider_id

      graph => this%get_component_graph()

      attachment => state_binding%get_method_attachment(method_name)
      _ASSERT(associated(attachment), 'GraphBuilder: build_callback_method_binding - method not bound on state binding')

      method_node_id = graph%next_node_id(_RC)
      ! Not registered yet - node_declare_argument/node_bind_argument
      ! (below) must run on this local value before it is handed to
      ! register_node(), which copies it into the graph's own node map;
      ! mutating a fetched-back pointer would work too (the pattern
      ! finish_transform/extension materialization already use) but
      ! populating before the one-shot registration is simpler here
      ! since nothing else needs this node's identity in between.
      method_node = MethodGraphNode(method_node_id, attachment)

      get_net_id = graph%create_network(_RC)
      put_net_id = graph%create_network(_RC)

      binding = CallbackMethodBinding(method_node_id, get_net_id, put_net_id)

      method_spec = iface%get_method(method_name, _RC)
      accesses = method_spec%get_argument_accesses()

      iter = accesses%ftn_begin()
      do while (iter /= accesses%ftn_end())
         call iter%next()
         argument_name = iter%first()
         access = iter%second()

         member_id => state_binding%get_argument_binding(argument_name)
         _ASSERT(associated(member_id), 'GraphBuilder: build_callback_method_binding - argument not bound on state binding: ' // argument_name)

         ! Populates the MethodGraphNode's own declared-argument/binding
         ! storage (REQ-MTH-002) with exactly the same argument name,
         ! access mode, and bound member NodeId CallbackStateBinding's
         ! own invoke_method() would otherwise materialize fresh per
         ! call (CallbackStateBinding.F90's build_arguments_for_method) -
         ! so this method_node's own invoke() (called from
         ! invoke_callback_method, task 8) is self-sufficient and needs
         ! no separate call back into state_binding.
         arg_spec = iface%get_argument(argument_name, _RC)
         expected_kind = arg_spec%get_expected_kind()
         call method_node%declare_argument(argument_name, access, expected_kind=expected_kind, rc=status)
         _VERIFY(status)
         call method_node%bind_argument(argument_name, member_id, actual_kind=expected_kind, rc=status)
         _VERIFY(status)

         if (access == MAPL_ACCESS_IN .or. access == MAPL_ACCESS_INOUT) then
            provider_id => provider_get_ids%at(argument_name)
            _ASSERT(associated(provider_id), 'GraphBuilder: build_callback_method_binding - no get-side provider for argument: ' // argument_name)
            call wire_callback_argument(graph, get_net_id, provider_id, member_id, _RC)
            call binding%bind_get_argument(argument_name, provider_id, _RC)
         end if

         if (access == MAPL_ACCESS_OUT .or. access == MAPL_ACCESS_INOUT) then
            provider_id => provider_put_ids%at(argument_name)
            _ASSERT(associated(provider_id), 'GraphBuilder: build_callback_method_binding - no put-side provider for argument: ' // argument_name)
            call wire_callback_argument(graph, put_net_id, member_id, provider_id, _RC)
            call binding%bind_put_argument(argument_name, provider_id, _RC)
         end if
      end do

      call graph%register_node(method_node, _RC)

      _RETURN(_SUCCESS)
   end function build_callback_method_binding

   ! Wires one argument's data flow edge into network_id, reusing
   ! find_mismatched_characteristics/find_or_build_extension_chain
   ! exactly like ordinary connection resolution (design.md Decisions) -
   ! both source_characteristics/target_characteristics are empty
   ! CharacteristicMaps at this data-model tier, so this always takes
   ! the exact-match direct-edge branch today; the transform-chain
   ! branch remains available for a future characteristics-aware
   ! CallbackArgumentSpec.
   module subroutine wire_callback_argument(graph, network_id, source_id, target_id, rc)
      type(ComponentGraph), target, intent(inout) :: graph
      type(DependencyNetworkId), intent(in) :: network_id
      type(NodeId), intent(in) :: source_id
      type(NodeId), intent(in) :: target_id
      integer, optional, intent(out) :: rc

      integer :: status
      type(CharacteristicMap), target :: empty_characteristics
      type(CharacteristicId), allocatable :: mismatched(:)
      type(NodeId) :: final_id
      type(NodeId), allocatable :: intermediate_ids(:)
      character(:), allocatable :: unsupported

      mismatched = find_mismatched_characteristics(empty_characteristics, empty_characteristics)

      if (size(mismatched) == 0) then
         call graph%add_dependency(network_id, source_id, target_id, _RC)
         _RETURN(_SUCCESS)
      end if

      call find_or_build_extension_chain(graph, network_id, source_id, &
           empty_characteristics, empty_characteristics, mismatched, final_id, intermediate_ids, unsupported, _RC)
      ! openspec/changes/phase-11b-chain-intermediate-materialization:
      ! intentionally no materialize_extension_chain call here -
      ! empty_characteristics means mismatched is always zero-length
      ! above, so this branch is never entered by any real caller today
      ! (no CallbackArgumentSpec declares units/typekind/vertical_grid
      ! yet). No VariableSpec exists at this call site either, so
      ! materialize_extension_chain's own signature could not be called
      ! here even if this branch were reachable - deferred to whenever
      ! CallbackArgumentSpec gains real characteristics.
      _ASSERT(unsupported == '', 'GraphBuilder: wire_callback_argument - unsupported characteristic: ' // unsupported)

      call graph%add_dependency(network_id, final_id, target_id, _RC)

      _RETURN(_SUCCESS)
   end subroutine wire_callback_argument

   ! REQ-CB-020: invoke-once-after-all-args-ready discipline, mirroring
   ! mapl_MethodInvocation_mod%invoke_on_default_network's exact
   ! three-phase shape (design.md Decision 7) but parameterized by
   ! binding's own get/put networks instead of the default network.
   ! Never registers the callback MethodGraphNode into demand-driven
   ! update's own dispatch - only its get-network predecessor(s) are
   ! pulled current via the ordinary graph%update() entry point, and
   ! only its put-network target(s) are advanced, both by this
   ! procedure's own explicit request.
   module subroutine invoke_callback_method(graph, binding, rc)
      class(ComponentGraph), target, intent(inout) :: graph
      type(CallbackMethodBinding), target, intent(in) :: binding
      integer, optional, intent(out) :: rc

      integer :: status
      type(StateItemMemberMap) :: get_bindings, put_bindings
      class(GraphNode), pointer :: generic_node
      class(MethodGraphNode), pointer :: method_node
      type(DependencyNetworkId) :: get_net_id

      ! put_bindings' own revisions are advanced directly by NodeId
      ! (advance_callback_put_arguments below, mirroring
      ! mapl_MethodInvocation_mod%advance_bound_outputs' own precedent) -
      ! no per-network traversal is needed for "advance," so the put
      ! network's own id is not read here.
      get_net_id = binding%get_get_network_id()
      get_bindings = binding%get_get_bindings()
      put_bindings = binding%get_put_bindings()

      generic_node => graph%get_node(binding%get_method_node_id())
      _ASSERT(associated(generic_node), 'GraphBuilder: invoke_callback_method - method node not found')
      select type (generic_node)
      class is (MethodGraphNode)
         method_node => generic_node
      class default
         _ASSERT(.false., 'GraphBuilder: invoke_callback_method - node_id does not identify a MethodGraphNode')
      end select

      call pull_callback_get_arguments(graph, get_net_id, get_bindings, _RC)

      call method_node%invoke(_RC)

      call advance_callback_put_arguments(graph, put_bindings, _RC)

      _RETURN(_SUCCESS)
   end subroutine invoke_callback_method

   ! REQ-REV-011's "pull-all-before", applied to binding's own get
   ! network (mapl_MethodInvocation_mod%pull_bound_inputs' own
   ! collect-then-process shape, design.md Decision 7 - no map iterator
   ! live across the graph%update() call).
   module subroutine pull_callback_get_arguments(graph, network_id, get_bindings, rc)
      class(ComponentGraph), target, intent(in) :: graph
      type(DependencyNetworkId), intent(in) :: network_id
      type(StateItemMemberMap), target, intent(in) :: get_bindings
      integer, optional, intent(out) :: rc

      integer :: status
      type(StateItemMemberMapIterator) :: iter
      character(:), allocatable :: names(:)
      integer :: n, i
      type(NodeId), pointer :: provider_id

      allocate(character(MAX_CALLBACK_ARGUMENT_NAME_LEN) :: names(get_bindings%size()))
      n = 0
      iter = get_bindings%ftn_begin()
      do while (iter /= get_bindings%ftn_end())
         call iter%next()
         n = n + 1
         names(n) = iter%first()
      end do

      do i = 1, n
         provider_id => get_bindings%at(trim(names(i)))
         _ASSERT(associated(provider_id), 'GraphBuilder: pull_callback_get_arguments - bound name not found')
         call graph%update(network_id, provider_id, _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine pull_callback_get_arguments

   ! REQ-REV-011's "advance-all-after", applied to binding's own put
   ! network - only reached once invoke() has already succeeded
   ! (invoke_callback_method's own "_RC"-checked call above returns early
   ! otherwise, spec scenario "Failed invocation does not advance bound
   ! outputs").
   module subroutine advance_callback_put_arguments(graph, put_bindings, rc)
      class(ComponentGraph), target, intent(in) :: graph
      type(StateItemMemberMap), target, intent(in) :: put_bindings
      integer, optional, intent(out) :: rc

      integer :: status
      type(StateItemMemberMapIterator) :: iter
      character(:), allocatable :: names(:)
      integer :: n, i
      type(NodeId), pointer :: target_id
      class(GraphNode), pointer :: generic_node

      allocate(character(MAX_CALLBACK_ARGUMENT_NAME_LEN) :: names(put_bindings%size()))
      n = 0
      iter = put_bindings%ftn_begin()
      do while (iter /= put_bindings%ftn_end())
         call iter%next()
         n = n + 1
         names(n) = iter%first()
      end do

      do i = 1, n
         target_id => put_bindings%at(trim(names(i)))
         _ASSERT(associated(target_id), 'GraphBuilder: advance_callback_put_arguments - bound name not found')
         generic_node => graph%get_node(target_id)
         _ASSERT(associated(generic_node), 'GraphBuilder: advance_callback_put_arguments - bound NodeId not found in graph')
         select type (generic_node)
         class is (StateItemNode)
            call generic_node%advance_revision(_RC)
         class default
            _ASSERT(.false., 'GraphBuilder: advance_callback_put_arguments - bound NodeId is not a StateItemNode')
         end select
      end do

      _RETURN(_SUCCESS)
   end subroutine advance_callback_put_arguments

end submodule GraphBuilder_CallbackWiring_smod
