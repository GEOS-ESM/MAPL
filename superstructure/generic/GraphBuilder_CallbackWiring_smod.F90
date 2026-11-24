#include "MAPL.h"

! Callback wiring: callback states, method nodes and their get/put networks.

submodule (mapl_GraphBuilder_mod) GraphBuilder_CallbackWiring_smod
   use esmf, only: ESMF_StateCreate, ESMF_STATEINTENT_INTERNAL
   use mapl_AccessSpec_mod, only: AccessSpec, MAPL_ACCESS_IN, MAPL_ACCESS_INOUT, MAPL_ACCESS_OUT, operator(==)
   use mapl_AccessSpecMap_mod, only: AccessSpecMap, AccessSpecMapIterator, operator(/=)
   use mapl_CallbackArgumentSpec_mod, only: CallbackArgumentSpec
   use mapl_CallbackArgumentSpecMap_mod, only: CallbackArgumentSpecMap, CallbackArgumentSpecMapIterator, operator(/=)
   use mapl_CallbackMethodSpec_mod, only: CallbackMethodSpec
   use mapl_CallbackMethodSpecMap_mod, only: CallbackMethodSpecMap, CallbackMethodSpecMapIterator, operator(/=)
   use mapl_EsmfStateMethodInvoker_mod, only: EsmfStateMethodInvoker
   use gFTL2_StringStringMap, only: StringStringMap, StringStringMapIterator, operator(/=)
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristicMap
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind
   use mapl_StateItemDescriptor_mod, only: resolve_characteristics
   use mapl_ExtensionResolution_mod, only: find_or_build_extension_chain, &
        materialize_extensions_enabled
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_MethodGraphNode_mod, only: MethodGraphNode
   use mapl_StateItemFlag_mod, only: MAPL_StateItem_Flag
   use mapl_StateItemMemberMap_mod, only: StateItemMemberMapIterator, operator(/=)
   use mapl_StateMethodInvocation_mod, only: StateMethodInvocation
   use mapl_VariableSpecVector_mod, only: VariableSpecVectorIterator, operator(/=)
   use mapl_CallbackContractViolation_mod, only: CallbackContractViolation, &
        missing_argument_violation, kind_mismatch_violation, dims_mismatch_violation, &
        unreadable_argument_violation
   use mapl_VerticalStaggerLoc_mod, only: VerticalStaggerLoc, &
        VERTICAL_STAGGER_CENTER, VERTICAL_STAGGER_EDGE, operator(==)
   use mapl_ErrorHandling_mod
   implicit none(type, external)

contains

   ! Builds root's flattened qualified-export namespace
   ! by recursing over every descendant. A fresh, uncached snapshot
   ! ("currently-known" exports only). root's exports are keyed by short
   ! name; a descendant's by comp_path/short_name.
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
            ! Classic ifort 2021.13 -O0 segfaults on `entries = [entries, entry]`
            ! for this nested-allocatable type; grow via move_alloc.
            n = size(entries)
            allocate(grown_entries(n + 1))
            ! NAG 7.2.41 -O0: the zero-extent section assignment
            ! grown_entries(1:0) = entries (n==0) hits a bus error in the
            ! generated defined-assignment dispatch; hence the guard.
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

   ! Forwards to resolve_qualified_component in GraphBuilder.F90's
   ! shared helpers, so callers in this submodule need no change.
   ! comp_path MUST be non-empty; an empty path means root itself and
   ! callers guard it.
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
   ! relative to `this`) to the matched export's real NodeId, found via
   ! its owning component's resource index. Node storage is shared
   ! hierarchy-wide, so the owner's own NodeId is used directly and no
   ! parent-local proxy is created.
   module function resolve_qualified_export_node_id(this, src_component_name, entry, rc) result(node_id)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: src_component_name
      type(QualifiedExportEntry), intent(in) :: entry
      integer, optional, intent(out) :: rc
      type(NodeId) :: node_id

      integer :: status
      character(:), allocatable :: full_path
      type(ComponentGraph), pointer :: owner_graph
      type(NodeId), pointer :: owner_item_id
      type(OuterMetaComponent), pointer :: owner_meta

      ! allocate(source=) rather than `=` into a deferred-length
      ! character: gfortran realloc-on-assignment memory corruption (see
      ! StateItemNode.F90's constructor).
      if (is_self(this, src_component_name)) then
         allocate(full_path, source=entry%comp_path)
      else if (len(entry%comp_path) == 0) then
         allocate(full_path, source=src_component_name)
      else
         allocate(full_path, source=src_component_name // '/' // entry%comp_path)
      end if

      if (len(full_path) == 0) then
         owner_meta => this
      else
         owner_meta => resolve_descendant(this, full_path, _RC)
      end if
      owner_graph => owner_meta%get_component_graph()
      owner_item_id => owner_graph%get_resource_index(item_key(ESMF_STATEINTENT_EXPORT, entry%var_spec%short_name))
      _ASSERT(associated(owner_item_id), 'GraphBuilder: callback wiring references an export never advertised at path ' // full_path)
      node_id = owner_item_id

      _RETURN(_SUCCESS)
   end function resolve_qualified_export_node_id

   ! An export "implements" expected_iface when its declared composite
   ! members cover every required argument name expected_iface declares
   ! (an optional argument, CallbackArgumentSpec%is_optional, may be
   ! absent with no violation), each present member matching the
   ! argument's expected itemType (compared by name) and, for a FIELD
   ! argument, its declared dims ('xy'/'xyz', read from the member's own
   ! vertical_stagger - a real YAML-declared member's normal source of
   ! truth for 2D/3D - rather than from resolved graph characteristics;
   ! this runs before any graph node exists for the member). A kind
   ! mismatch skips the dims comparison for that argument: dims are only
   ! meaningful once the kind itself matches. A mirrored or not-yet-given
   ! stagger (member_dims's `determinable` output false) also skips the
   ! dims comparison, rather than guessing - see member_dims. Every
   ! violation found is returned, not just the first. Method presence is
   ! not checked here; CallbackStateBinding%bind_method reports a missing
   ! method attachment at binding time.
   module function export_conforms_to_interface(var_spec, expected_iface, violations) result(conforms)
      type(VariableSpec), target, intent(in) :: var_spec
      type(CallbackInterface), target, intent(in) :: expected_iface
      type(CallbackContractViolation), allocatable, intent(out) :: violations(:)
      logical :: conforms

      type(CallbackArgumentSpecMap), target :: arguments
      type(CallbackArgumentSpecMapIterator) :: iter
      character(:), allocatable :: argument_name
      type(CallbackArgumentSpec) :: arg_spec
      type(MAPL_StateItem_Flag) :: expected_kind
      type(VariableSpec) :: member
      character(:), allocatable :: found_dims, expected_dims
      logical :: dims_determinable
      integer :: status

      allocate(violations(0))

      arguments = expected_iface%get_arguments()
      iter = arguments%ftn_begin()
      do while (iter /= arguments%ftn_end())
         call iter%next()
         ! deallocate+allocate(source=) rather than `=` in a loop: gfortran
         ! realloc-on-assignment memory corruption (see StateItemNode.F90's
         ! constructor).
         if (allocated(argument_name)) deallocate(argument_name)
         allocate(argument_name, source=iter%first())
         arg_spec = iter%second()

         if (.not. var_spec%has_member(argument_name)) then
            if (arg_spec%is_optional()) cycle
            if (arg_spec%has_weight_for()) then
               call append_violation(violations, &
                    missing_argument_violation(argument_name, weight_of=arg_spec%get_weight_for()))
            else
               call append_violation(violations, missing_argument_violation(argument_name))
            end if
            cycle
         end if

         member = var_spec%get_member(argument_name, rc=status)
         if (status /= 0) then
            ! The member is declared but could not be read back as a
            ! VariableSpec (StateItemSpecMap holding an unexpected
            ! concrete type) - a real structural error, not "this
            ! argument wasn't declared".
            call append_violation(violations, unreadable_argument_violation(argument_name))
            cycle
         end if

         expected_kind = arg_spec%get_expected_kind()
         if (itemtype_name(member%itemType) /= expected_kind%to_string()) then
            call append_violation(violations, kind_mismatch_violation(argument_name, &
                 itemtype_name(member%itemType), expected_kind%to_string()))
            cycle
         end if

         if (expected_kind%to_string() == 'FIELD' .and. arg_spec%has_dims()) then
            expected_dims = arg_spec%get_dims()
            dims_determinable = member_dims(member, found_dims)
            if (dims_determinable) then
               if (found_dims /= expected_dims) then
                  call append_violation(violations, dims_mismatch_violation(argument_name, found_dims, expected_dims))
               end if
            end if
         end if
      end do

      conforms = size(violations) == 0
   end function export_conforms_to_interface

   ! Duplicated from CallbackStateConformance.F90's own append: not
   ! shared across module boundaries, same grow-allocate-copy pattern.
   subroutine append_violation(violations, violation)
      type(CallbackContractViolation), allocatable, intent(inout) :: violations(:)
      type(CallbackContractViolation), intent(in) :: violation

      type(CallbackContractViolation), allocatable :: grown(:)
      integer :: n

      n = size(violations)
      allocate(grown(n + 1))
      grown(1:n) = violations
      grown(n + 1) = violation
      call move_alloc(grown, violations)
   end subroutine append_violation

   ! Declaration-level dims source of truth: the member VariableSpec's own
   ! vertical_stagger - parse_var_specs sets this from the YAML
   ! 'vertical_dim_spec' key (CENTER/EDGE/NONE/MIRROR), not a resolved
   ! StateItemDescriptor characteristic (no graph node exists for the
   ! member yet at this point). A directly-set vertical_grid takes
   ! precedence when present: parse_callback_members sets one from a
   ! member's own 'vertical_grid:' key, and a top-level item can carry one
   ! the same way. MIRROR defers the real vertical grid to another item
   ! and an unallocated stagger means the declaration never said - either
   ! way dims cannot be determined yet, so dims returns unset and
   ! determinable is false rather than guessing 'xy'. Neither case is
   ! reachable for a member (parse_callback_members always sets an
   ! explicit NONE/CENTER/given value, never MIRROR or unallocated); only
   ! a top-level item whose YAML omitted vertical_dim_spec reaches them.
   function member_dims(member, dims) result(determinable)
      type(VariableSpec), intent(in) :: member
      character(:), allocatable, intent(out) :: dims
      logical :: determinable

      determinable = .true.
      if (allocated(member%vertical_grid)) then
         dims = 'xyz'
      else if (.not. allocated(member%vertical_stagger)) then
         determinable = .false.
      else if (member%vertical_stagger%is_mirror()) then
         determinable = .false.
      else if (member%vertical_stagger%is_none()) then
         dims = 'xy'
      else if (member%vertical_stagger == VERTICAL_STAGGER_CENTER .or. &
           member%vertical_stagger == VERTICAL_STAGGER_EDGE) then
         dims = 'xyz'
      else
         ! Explicit VERTICAL_STAGGER_INVALID: a top-level item's default
         ! when its YAML omits vertical_dim_spec, deferred to a later
         ! harvest pass. Not reachable for a member (parse_callback_members
         ! never leaves a member's stagger INVALID).
         determinable = .false.
      end if
   end function member_dims

   ! Materializes a flat callback collection: builds one
   ! StateItemNode (a real, structurally empty ESMF_State) in this's
   ! graph whose state_members map has one entry per resolved match,
   ! keyed by its qualified name (unique within one resolution) and
   ! mapping to the match's NodeId
   ! (resolve_qualified_export_node_id). Built directly rather than via
   ! CompositeStateMaterialization's VariableSpec-tree walk.
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

   ! Builds a CallbackStateBinding using the matched callback state's
   ! already-materialized composite member NodeIds, read from its owning
   ! component's graph via GraphStateItem%state_members(), not
   ! re-derived from VariableSpec.
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
      type(GraphStateItem), pointer :: state_payload
      type(StateItemMemberMap), pointer :: members
      type(CallbackArgumentSpecMap), target :: arguments
      type(CallbackArgumentSpecMapIterator) :: iter
      character(:), allocatable :: argument_name
      type(NodeId), pointer :: member_node_id
      type(CallbackMethodSpecMap), target :: methods
      type(CallbackMethodSpecMapIterator) :: method_iter
      character(:), allocatable :: method_name

      ! allocate(source=): see resolve_qualified_export_node_id.
      if (is_self(this, src_component_name)) then
         allocate(full_path, source=entry%comp_path)
      else if (len(entry%comp_path) == 0) then
         allocate(full_path, source=src_component_name)
      else
         allocate(full_path, source=src_component_name // '/' // entry%comp_path)
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
         state_payload => state_node%get_payload_ptr()
      class default
         _ASSERT(.false., 'GraphBuilder: callback state binding - state node is not a StateItemNode')
      end select
      members => state_payload%state_members_ptr()

      binding = CallbackStateBinding(expected_interface_id, expected_iface, state_node_id)

      arguments = expected_iface%get_arguments()
      iter = arguments%ftn_begin()
      do while (iter /= arguments%ftn_end())
         call iter%next()
         ! deallocate+allocate(source=) rather than `=` in a loop: gfortran
         ! realloc-on-assignment memory corruption (see StateItemNode.F90's
         ! constructor).
         if (allocated(argument_name)) deallocate(argument_name)
         allocate(argument_name, source=iter%first())

         member_node_id => members%at(argument_name)
         _ASSERT(associated(member_node_id), 'GraphBuilder: callback state binding - member not found for argument: ' // argument_name)
         call binding%bind_argument(argument_name, member_node_id, _RC)
      end do

      ! Production binding: attach the ESMF-backed invoker to every method
      ! the interface declares, so build_callback_method_binding (called
      ! per method below) finds a method_attachment via
      ! get_method_attachment. Tests that build a CallbackStateBinding
      ! directly (not through this function) attach a mock invoker instead.
      methods = expected_iface%get_methods()
      method_iter = methods%ftn_begin()
      do while (method_iter /= methods%ftn_end())
         call method_iter%next()
         if (allocated(method_name)) deallocate(method_name)
         allocate(method_name, source=method_iter%first())
         call binding%bind_method(method_name, status, invoker=EsmfStateMethodInvoker(owner_graph))
         _VERIFY(status)
      end do

      _RETURN(_SUCCESS)
   end function build_callback_state_binding

   ! Builds one CallbackMethodBinding for method_name on
   ! state_binding. Registers a MethodGraphNode (holding the attachment
   ! from CallbackStateBinding%bind_method) in this's graph, creates a
   ! put network, and wires each argument's dependency edge into the
   ! forward network (get) or the put network, by access mode, reusing
   ! resolve_characteristics/find_or_build_extension_chain as ordinary
   ! connection resolution does (wire_callback_argument).
   module function build_callback_method_binding(this, import_short_name, iface, method_name, state_binding, &
        provider_get_ids, provider_put_ids, rc) result(binding)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: import_short_name
      type(CallbackInterface), intent(in) :: iface
      character(*), intent(in) :: method_name
      type(CallbackStateBinding), target, intent(in) :: state_binding
      type(StateItemMemberMap), intent(in) :: provider_get_ids
      type(StateItemMemberMap), intent(in) :: provider_put_ids
      integer, optional, intent(out) :: rc
      type(CallbackMethodBinding) :: binding

      integer :: status
      type(ComponentGraph), pointer :: graph
      type(DependencyNetworkId) :: forward_net_id, put_net_id
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
      character(:), allocatable :: msg

      graph => this%get_component_graph()

      attachment => state_binding%get_method_attachment(method_name)
      _ASSERT(associated(attachment), 'GraphBuilder: build_callback_method_binding - method not bound on state binding')

      method_node_id = graph%next_node_id(_RC)
      ! Declare/bind on the local value before register_node(), which
      ! copies it into the graph's node map.
      method_node = MethodGraphNode(method_node_id, attachment)

      ! Get edges carry data toward the method before it runs, so they join
      ! the forward network; only the put (write-back) network is the
      ! method's own.
      forward_net_id = graph%get_forward_network_id(_RC)
      put_net_id = graph%create_network(_RC)

      binding = CallbackMethodBinding(method_node_id, put_net_id)

      method_spec = iface%get_method(method_name, _RC)
      accesses = method_spec%get_argument_accesses()

      iter = accesses%ftn_begin()
      do while (iter /= accesses%ftn_end())
         call iter%next()
         ! deallocate+allocate(source=) rather than `=` in a loop: gfortran
         ! realloc-on-assignment memory corruption (see StateItemNode.F90's
         ! constructor).
         if (allocated(argument_name)) deallocate(argument_name)
         allocate(argument_name, source=iter%first())
         access = iter%second()

         member_id => state_binding%get_argument_binding(argument_name)
         _ASSERT(associated(member_id), 'GraphBuilder: build_callback_method_binding - argument not bound on state binding: ' // argument_name)

         ! Populate the node's argument/binding storage with
         ! what CallbackStateBinding's invoke_method() would build per
         ! call, so method_node%invoke() is self-sufficient.
         arg_spec = iface%get_argument(argument_name, _RC)
         expected_kind = arg_spec%get_expected_kind()
         call method_node%declare_argument(argument_name, access, expected_kind=expected_kind, rc=status)
         _VERIFY(status)
         call method_node%bind_argument(argument_name, member_id, actual_kind=expected_kind, rc=status)
         _VERIFY(status)

         if (access == MAPL_ACCESS_IN .or. access == MAPL_ACCESS_INOUT) then
            provider_id => provider_get_ids%at(argument_name)
            msg = 'GraphBuilder: build_callback_method_binding - IMPORT "' // import_short_name // &
                 '" method "' // method_name // '" has no "provides:" entry for argument: ' // argument_name
            _ASSERT(associated(provider_id), msg)
            call wire_callback_argument(graph, forward_net_id, provider_id, member_id, _RC)
            ! Pulling the callback-side member reaches the provider and
            ! whatever produces it, plus any conversion wired between them.
            call binding%bind_get_argument(argument_name, member_id, _RC)
         end if

         if (access == MAPL_ACCESS_OUT .or. access == MAPL_ACCESS_INOUT) then
            provider_id => provider_put_ids%at(argument_name)
            msg = 'GraphBuilder: build_callback_method_binding - IMPORT "' // import_short_name // &
                 '" method "' // method_name // '" has no "receives:" entry for argument: ' // argument_name
            _ASSERT(associated(provider_id), msg)
            call wire_callback_argument(graph, put_net_id, member_id, provider_id, _RC)
            call binding%bind_put_argument(argument_name, provider_id, _RC)
         end if
      end do

      call graph%register_node(method_node, _RC)

      _RETURN(_SUCCESS)
   end function build_callback_method_binding

   ! Resolves one of the Invoker's own items (the item an argument's
   ! `provides:`/`receives:` entry names) to a NodeId, by short name, among
   ! owner_graph's own IMPORT/EXPORT/INTERNAL items (item_key is
   ! state-intent-qualified, so all three candidate keys are tried). Not
   ! found, or found under more than one state, is an error naming the
   ! import, the argument and the item, since the YAML gives no state -
   ! only a short name - and ambiguity across states is not resolvable
   ! here.
   function resolve_invoker_item_id(owner_graph, import_short_name, argument_name, item_short_name, rc) result(node_id)
      type(ComponentGraph), pointer, intent(in) :: owner_graph
      character(*), intent(in) :: import_short_name
      character(*), intent(in) :: argument_name
      character(*), intent(in) :: item_short_name
      integer, optional, intent(out) :: rc
      type(NodeId) :: node_id

      type(NodeId), pointer :: import_hit, export_hit, internal_hit
      integer :: num_hits
      character(:), allocatable :: context

      import_hit => owner_graph%get_resource_index(item_key(ESMF_STATEINTENT_IMPORT, item_short_name))
      export_hit => owner_graph%get_resource_index(item_key(ESMF_STATEINTENT_EXPORT, item_short_name))
      internal_hit => owner_graph%get_resource_index(item_key(ESMF_STATEINTENT_INTERNAL, item_short_name))

      num_hits = count([associated(import_hit), associated(export_hit), associated(internal_hit)])

      context = 'GraphBuilder: callback provider map - IMPORT "' // import_short_name // &
           '" argument "' // argument_name // '" names item "' // item_short_name // '"'
      _ASSERT(num_hits > 0, context // ': no such item on this component (any state)')
      _ASSERT(num_hits == 1, context // ': ambiguous - item exists under more than one state')

      if (associated(import_hit)) then
         node_id = import_hit
      else if (associated(export_hit)) then
         node_id = export_hit
      else
         node_id = internal_hit
      end if

      _RETURN(_SUCCESS)
   end function resolve_invoker_item_id

   ! Builds the Invoker IMPORT's provider maps:
   ! argument -> NodeId of the Invoker's own item that supplies a get (IN/
   ! INOUT, from the YAML `provides:` map, VariableSpec%callback_provides)
   ! or receives a put (OUT/INOUT, from `receives:`,
   ! VariableSpec%callback_receives). Built once per Invoker IMPORT, not
   ! per matched Handler member - build_callback_method_binding's
   ! provider_get_ids/provider_put_ids arguments are shared across every
   ! match. An argument absent from both YAML maps simply has no entry
   ! here; build_callback_method_binding's own asserts catch a
   ! required-but-unsupplied argument at bind time.
   module subroutine build_callback_provider_maps(this, comp_name, import_short_name, &
        provider_get_ids, provider_put_ids, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: comp_name
      character(*), intent(in) :: import_short_name
      type(StateItemMemberMap), intent(out) :: provider_get_ids
      type(StateItemMemberMap), intent(out) :: provider_put_ids
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: owner_meta
      type(ComponentGraph), pointer :: owner_graph
      type(ComponentSpec), pointer :: comp_spec
      type(VariableSpecVectorIterator) :: iter
      type(VariableSpec), pointer :: var_spec
      type(VariableSpec), pointer :: import_spec
      type(StringStringMapIterator) :: map_iter
      character(:), allocatable :: argument_name
      character(:), allocatable :: item_short_name
      type(NodeId) :: provider_id

      owner_meta => resolve_qualified_component(this, comp_name, _RC)
      owner_graph => owner_meta%get_component_graph()
      comp_spec => owner_meta%get_component_spec()

      nullify(import_spec)
      associate (e => comp_spec%var_specs%ftn_end())
         iter = comp_spec%var_specs%ftn_begin()
         do while (iter /= e)
            call iter%next()
            var_spec => iter%of()
            if (.not. (var_spec%state_intent == ESMF_STATEINTENT_IMPORT)) cycle
            if (var_spec%short_name /= import_short_name) cycle
            import_spec => var_spec
            exit
         end do
      end associate
      _ASSERT(associated(import_spec), 'GraphBuilder: build_callback_provider_maps - IMPORT not found: ' // import_short_name)

      associate (e => import_spec%callback_provides%ftn_end())
         map_iter = import_spec%callback_provides%ftn_begin()
         do while (map_iter /= e)
            call map_iter%next()
            if (allocated(argument_name)) deallocate(argument_name)
            allocate(argument_name, source=map_iter%first())
            if (allocated(item_short_name)) deallocate(item_short_name)
            allocate(item_short_name, source=map_iter%second())
            provider_id = resolve_invoker_item_id(owner_graph, import_short_name, argument_name, item_short_name, _RC)
            call provider_get_ids%insert(argument_name, provider_id)
         end do
      end associate

      associate (e => import_spec%callback_receives%ftn_end())
         map_iter = import_spec%callback_receives%ftn_begin()
         do while (map_iter /= e)
            call map_iter%next()
            if (allocated(argument_name)) deallocate(argument_name)
            allocate(argument_name, source=map_iter%first())
            if (allocated(item_short_name)) deallocate(item_short_name)
            allocate(item_short_name, source=map_iter%second())
            provider_id = resolve_invoker_item_id(owner_graph, import_short_name, argument_name, item_short_name, _RC)
            call provider_put_ids%insert(argument_name, provider_id)
         end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine build_callback_provider_maps

   ! Builds and stores every (matched member, declared method) binding for
   ! one callback collection: a CallbackStateBinding (shared by every
   ! method on the same member, since arguments/method attachments do not
   ! vary by method), then one CallbackMethodBinding per method the
   ! interface declares, stored on collection_node under
   ! callback_method_binding_key(entry%qualified_name, method_name), for an
   ! Invoker to look up a matched member's bound methods later.
   module subroutine build_callback_bindings_for_match(this, src_component_name, entry, &
        expected_interface_id, iface, import_short_name, provider_get_ids, provider_put_ids, collection_node, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: src_component_name
      type(QualifiedExportEntry), intent(in) :: entry
      type(CallbackInterfaceId), intent(in) :: expected_interface_id
      type(CallbackInterface), intent(in) :: iface
      character(*), intent(in) :: import_short_name
      type(StateItemMemberMap), intent(in) :: provider_get_ids
      type(StateItemMemberMap), intent(in) :: provider_put_ids
      class(StateItemNode), intent(inout) :: collection_node
      integer, optional, intent(out) :: rc

      integer :: status
      type(CallbackStateBinding) :: state_binding
      type(CallbackMethodSpecMap), target :: methods
      type(CallbackMethodSpecMapIterator) :: method_iter
      character(:), allocatable :: method_name
      type(CallbackMethodBinding) :: method_binding

      state_binding = build_callback_state_binding(this, src_component_name, entry, &
           expected_interface_id, iface, _RC)

      methods = iface%get_methods()
      method_iter = methods%ftn_begin()
      do while (method_iter /= methods%ftn_end())
         call method_iter%next()
         if (allocated(method_name)) deallocate(method_name)
         allocate(method_name, source=method_iter%first())

         method_binding = build_callback_method_binding(this, import_short_name, iface, method_name, state_binding, &
              provider_get_ids, provider_put_ids, _RC)
         call collection_node%add_callback_method_binding( &
              callback_method_binding_key(entry%qualified_name, method_name), method_binding, _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine build_callback_bindings_for_match

   ! Wires one argument's data-flow edge into network_id, comparing the real
   ! descriptors of source_id/target_id (both ordinary StateItemNodes - the
   ! Handler's member and the Invoker-side copy) the same way ordinary
   ! connection resolution compares an export/import pair:
   ! resolve_characteristics decides whether an extension chain is needed,
   ! find_or_build_extension_chain builds (or reuses) it, and
   ! materialize_extension_chain fills it with real Fields when
   ! materialize_extensions_enabled() (chain structure is unconditional,
   ! same gating as ordinary connection resolution). A geometry mismatch is
   ! resolved like any other: by whichever side's VariableSpec declared a
   ! regridder_param (the Invoker-side copy is expected to declare the
   ! default first-order-conservative one; this function has no
   ! callback-specific regrid logic of its own).
   !
   ! Unlike ordinary connection resolution, this has no later-connect-pass
   ! retry for a DEFERRED characteristic: callback binding has no production
   ! caller yet, and its intended call site runs only after ordinary
   ! connections have already resolved their own DEFERRED characteristics.
   ! A DEFERRED side reaching here means that future caller ran this too
   ! early - a caller-ordering bug to fix at the call site, not something to
   ! retry or silently skip here - so resolve_characteristics is called
   ! directly (passing waiting) instead of through the
   ! find_mismatched_characteristics wrapper, which has no `waiting` output
   ! and always fails hard.
   module subroutine wire_callback_argument(graph, network_id, source_id, target_id, rc)
      type(ComponentGraph), target, intent(inout) :: graph
      type(DependencyNetworkId), intent(in) :: network_id
      type(NodeId), intent(in) :: source_id
      type(NodeId), intent(in) :: target_id
      integer, optional, intent(out) :: rc

      integer :: status
      type(StateItemDescriptor), pointer :: source_item, target_item
      type(StateItemCharacteristicMap) :: source_map, target_map
      type(StateItemCharacteristicKind), allocatable :: mismatched(:)
      type(StateItemCharacteristicKind), allocatable :: adopted(:)
      character(:), allocatable :: conflict
      character(:), allocatable :: incompatible_pair
      character(:), allocatable :: assert_message
      logical :: waiting
      type(NodeId) :: final_id
      type(NodeId), allocatable :: intermediate_ids(:)
      character(:), allocatable :: unsupported
      character(:), allocatable :: materialization_failure

      source_item => callback_argument_descriptor(graph, source_id, _RC)
      target_item => callback_argument_descriptor(graph, target_id, _RC)
      source_map = source_item%get_characteristics()
      target_map = target_item%get_characteristics()

      waiting = .false.
      call resolve_characteristics(source_map, target_map, source_item%get_declared_item_type(), &
           mismatched, adopted, conflict, rc=status, waiting=waiting)
      _VERIFY(status)
      assert_message = 'GraphBuilder: wire_callback_argument - a characteristic is still unresolved ' // &
           '(DEFERRED); callback binding must run after ordinary connections resolve'
      _ASSERT(.not. waiting, assert_message)
      _ASSERT(conflict == '', 'GraphBuilder: wire_callback_argument - ' // conflict)
      if (size(adopted) > 0) then
         assert_message = 'GraphBuilder: wire_callback_argument - MIRRORED import needs a descriptor to adopt ' // &
              adopted(1)%to_string()
         _FAIL(assert_message)
      end if

      if (size(mismatched) == 0) then
         call graph%add_dependency(network_id, source_id, target_id, _RC)
         _RETURN(_SUCCESS)
      end if

      call find_or_build_extension_chain(graph, network_id, source_id, &
           source_map, target_map, mismatched, final_id, intermediate_ids, unsupported, &
           conflict=incompatible_pair, _RC)
      _ASSERT(incompatible_pair == '', 'GraphBuilder: wire_callback_argument - ' // incompatible_pair)
      _ASSERT(unsupported == '', 'GraphBuilder: wire_callback_argument - unsupported characteristic: ' // unsupported)

      if (materialize_extensions_enabled()) then
         call materialize_extension_chain(source_item, target_item, intermediate_ids, graph, &
              materialization_failure, _RC)
         _ASSERT(materialization_failure == '', 'GraphBuilder: wire_callback_argument - ' // materialization_failure)
      end if

      call graph%add_dependency(network_id, final_id, target_id, _RC)

      _RETURN(_SUCCESS)
   end subroutine wire_callback_argument

   ! Mutable view of a node's descriptor, reached through the graph -
   ! same pattern as GraphBuilder_ConnectionResolution_smod.F90's own
   ! mutable_descriptor (not shared across submodule files; see
   ! materialize_extension_chain's own cross-submodule precedent above for
   ! why a plain, non-`module` procedure must be duplicated instead).
   function callback_argument_descriptor(graph, node_id, rc) result(descriptor)
      type(ComponentGraph), target, intent(in) :: graph
      type(NodeId), intent(in) :: node_id
      integer, optional, intent(out) :: rc
      type(StateItemDescriptor), pointer :: descriptor

      class(GraphNode), pointer :: node

      descriptor => null()
      node => graph%get_node(node_id)
      _ASSERT(associated(node), 'GraphBuilder: wire_callback_argument - NodeId not found')
      select type (node)
      class is (StateItemNode)
         descriptor => node%get_descriptor()
      class default
         _FAIL('GraphBuilder: wire_callback_argument - NodeId is not a StateItemNode')
      end select

      _RETURN(_SUCCESS)
   end function callback_argument_descriptor

   ! Invoke once after all arguments are ready, in the same
   ! three-phase shape as mapl_MethodInvocation_mod%
   ! invoke_on_forward_network: get arguments are pulled through the
   ! forward network, put targets are advanced directly. The callback
   ! MethodGraphNode is never scheduled by demand-driven update.
   module subroutine invoke_callback_method(graph, binding, rc)
      class(ComponentGraph), target, intent(inout) :: graph
      type(CallbackMethodBinding), target, intent(in) :: binding
      integer, optional, intent(out) :: rc

      integer :: status
      type(StateItemMemberMap) :: get_bindings, put_bindings
      class(GraphNode), pointer :: generic_node
      class(MethodGraphNode), pointer :: method_node

      ! put revisions are advanced directly by NodeId (as
      ! advance_bound_outputs does), so the put network id is not needed.
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

      call pull_callback_get_arguments(graph, get_bindings, _RC)

      call method_node%invoke(_RC)

      call advance_callback_put_arguments(graph, put_bindings, _RC)

      _RETURN(_SUCCESS)
   end subroutine invoke_callback_method

   ! Pull-all-before, through the forward network: each bound
   ! callback-side member is pulled, reaching its provider's producer
   ! wherever that edge was declared.
   ! Collect names first so no map iterator is live across graph%update().
   module subroutine pull_callback_get_arguments(graph, get_bindings, rc)
      class(ComponentGraph), target, intent(in) :: graph
      type(StateItemMemberMap), target, intent(in) :: get_bindings
      integer, optional, intent(out) :: rc

      integer :: status
      type(StateItemMemberMapIterator) :: iter
      character(:), allocatable :: names(:)
      integer :: n, i
      type(NodeId), pointer :: member_id

      allocate(character(MAX_CALLBACK_ARGUMENT_NAME_LEN) :: names(get_bindings%size()))
      n = 0
      iter = get_bindings%ftn_begin()
      do while (iter /= get_bindings%ftn_end())
         call iter%next()
         n = n + 1
         names(n) = iter%first()
      end do

      do i = 1, n
         member_id => get_bindings%at(trim(names(i)))
         _ASSERT(associated(member_id), 'GraphBuilder: pull_callback_get_arguments - bound name not found')
         call graph%update(member_id, _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine pull_callback_get_arguments

   ! Advance-all-after, on binding's put network. Only
   ! reached once invoke() succeeded, so a failed invocation does not
   ! advance bound outputs.
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
