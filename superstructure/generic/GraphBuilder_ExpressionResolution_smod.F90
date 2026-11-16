#include "MAPL.h"

! Expression-item resolution: the EXPRESSION-item advertise hook, its
! operand-naming check, and building the per-consumer StateArithmeticParser
! evaluation (ExpressionTransform) at connect time.
!
! A direct sibling of GraphBuilder_ConnectionResolution_smod.F90 (both
! descend from mapl_GraphBuilder_mod directly): shared helpers it calls
! (push_warnings, find_export_node, mutable_descriptor, ...) are module
! procedures declared in mapl_GraphBuilder_mod's own interface block, not
! reached through submodule nesting.
submodule (mapl_GraphBuilder_mod) GraphBuilder_ExpressionResolution_smod
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
   ! Names check for each of `this`'s EXPRESSION items. The item itself is a
   ! placeholder: evaluation is built per consumer at connect time
   ! (build_expression_evaluation), so this hook adds no nodes or edges.
   ! A missing name is reported in unresolved; a nested, self-referencing
   ! or empty expression in unsupported.
   module subroutine graphbuilder_run_expression_hook(this, unusable, &
        unresolved, unsupported, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      class(KE), optional, intent(in) :: unusable
      type(GraphResolutionEntryVector), optional, intent(out) :: unresolved
      type(GraphResolutionEntryVector), optional, intent(out) :: unsupported
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentGraph), pointer :: graph
      type(NodeIdSet), target :: node_ids
      type(NodeIdSetIterator) :: node_iter
      type(NodeId) :: node_id
      class(GraphNode), pointer :: generic_node
      type(GraphResolutionEntryVector) :: local_unresolved
      type(GraphResolutionEntryVector) :: local_unsupported
      type(StateItemDescriptor), pointer :: descriptor

      graph => this%get_component_graph()
      node_ids = graph%get_node_ids()

      node_iter = node_ids%ftn_begin()
      do while (node_iter /= node_ids%ftn_end())
         call node_iter%next()
         node_id = node_iter%of()
         generic_node => graph%get_node(node_id)
         if (.not. associated(generic_node)) cycle
         select type (generic_node)
         class is (StateItemNode)
            descriptor => generic_node%get_descriptor()
            if (.not. (descriptor%get_declared_item_type() == MAPL_STATEITEM_EXPRESSION)) cycle
            call check_expression_item(this, generic_node, local_unresolved, &
                 local_unsupported, _RC)
         end select
      end do

      if (present(unresolved)) unresolved = local_unresolved
      if (present(unsupported)) unsupported = local_unsupported

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine graphbuilder_run_expression_hook

   ! Per-item worker for graphbuilder_run_expression_hook: one entry per
   ! problem found in node's expression.
   subroutine check_expression_item(this, node, unresolved, unsupported, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      class(StateItemNode), target, intent(in) :: node
      type(GraphResolutionEntryVector), intent(inout) :: unresolved
      type(GraphResolutionEntryVector), intent(inout) :: unsupported
      integer, optional, intent(out) :: rc

      integer :: status
      type(StateItemDescriptor), pointer :: descriptor, ref_descriptor
      character(:), allocatable :: expression, this_name, item_name
      type(StringVector) :: referenced_names
      type(StringVectorIterator) :: iter
      character(:), pointer :: name
      type(NodeId) :: ref_id
      class(StateItemNode), pointer :: ref_node
      logical :: found

      this_name = this%get_name(_RC)
      item_name = node%get_short_name()
      descriptor => node%get_descriptor()
      call descriptor%get_expression(expression, _RC)
      if (len_trim(expression) == 0) then
         call unsupported%push_back(GraphResolutionEntry(this_name, item_name, &
              'expression is empty'))
         _RETURN(_SUCCESS)
      end if

      referenced_names = parser_variables_in_expression(expression, _RC)
      iter = referenced_names%begin()
      do while (iter /= referenced_names%end())
         name => iter%of()
         call iter%next()
         if (name == item_name) then
            call unsupported%push_back(GraphResolutionEntry(this_name, &
                 item_name, 'expression references itself'))
            cycle
         end if
         found = find_export_node_id(this, SELF_COMPONENT_NAME, name, &
              ref_id, ref_node, _RC)
         if (.not. found) then
            call unresolved%push_back(GraphResolutionEntry(this_name, name, &
                 'expression ' // item_name // ' references missing item ' // name))
            cycle
         end if
         ref_descriptor => ref_node%get_descriptor()
         if (ref_descriptor%get_declared_item_type() == MAPL_STATEITEM_EXPRESSION) then
            call unsupported%push_back(GraphResolutionEntry(this_name, &
                 item_name, 'expression references expression item'))
         end if
      end do

      _RETURN(_SUCCESS)
   end subroutine check_expression_item

   ! One evaluation of export_node's expression for a consumer described
   ! by import_item, built in `graph` (the connecting graph): a result node
   ! fed by an ExpressionTransform whose inputs are the referenced sibling
   ! exports, each through its own extension chain to the result's
   ! consumer-adopted kinds. Consumers whose results agree on those kinds
   ! share one evaluation (EXPREVAL: key). All input chains are built before
   ! the transform, so a failing input leaves no partial evaluation.
   ! `evaluated` is false when the names are invalid (the expression hook
   ! reports those), when a side is DEFERRED (`waiting`), or when `problem`
   ! or `conflict` is set. `strict` marks a `problem` that is an
   ! unsupported kind (kept for the freeze check).
   module subroutine build_expression_evaluation(this, graph, net_id, src_comp, &
        export_node, import_item, result_id, evaluated, problem, conflict, &
        waiting, strict, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(ComponentGraph), target, intent(inout) :: graph
      type(DependencyNetworkId), intent(in) :: net_id
      character(*), intent(in) :: src_comp
      class(StateItemNode), target, intent(in) :: export_node
      type(StateItemDescriptor), intent(in) :: import_item
      type(NodeId), intent(out) :: result_id
      logical, intent(out) :: evaluated
      character(:), allocatable, intent(out) :: problem
      character(:), allocatable, intent(out) :: conflict
      logical, intent(inout) :: waiting
      logical, intent(out) :: strict
      integer, optional, intent(out) :: rc

      integer :: status, i, k
      type(StateItemDescriptor), pointer :: export_desc, input_desc, stored
      type(StateItemCharacteristicKind), allocatable :: kinds(:)
      type(StateItemCharacteristicKind), allocatable :: mismatched(:)
      type(StateItemCharacteristicKind), allocatable :: adopted(:)
      logical, allocatable :: declared_wins(:)
      class(StateItemCharacteristic), allocatable :: c
      character(:), allocatable :: key, expression, item_name
      character(:), allocatable :: status_conflict, unsupported_kind
      type(NodeId), pointer :: existing
      type(StringVector) :: names
      character(:), pointer :: name
      type(NodeId), allocatable :: input_ids(:), bound_ids(:)
      type(NodeId), allocatable :: intermediates(:)
      type(NodeId) :: input_id, transform_id, export_id
      class(StateItemNode), pointer :: input_node
      type(ExpressionTransform) :: transformer
      type(GraphStateItem) :: empty_payload
      type(NodeRevision) :: empty_revision
      type(StateItemCharacteristicMap), target :: export_map
      type(StateItemCharacteristicMapIterator) :: iter
      logical :: found, has, input_waiting
      character(:), allocatable :: failure
      type(ESMF_StateItem_Flag) :: input_type

      evaluated = .false.
      problem = ''
      conflict = ''
      strict = .false.
      item_name = export_node%get_short_name()
      export_desc => export_node%get_descriptor()
      call export_desc%get_consumer_adopted_kinds(kinds, &
           declared_wins=declared_wins, _RC)

      export_id = export_node%get_node_id()
      key = 'EXPREVAL:' // export_id%to_string()
      do k = 1, size(kinds)
         call result_characteristic(k, c, has, _RC)
         if (.not. has) cycle
         if (c%get_status() == CHARACTERISTIC_STATUS_DEFERRED) then
            waiting = .true.
            _RETURN(_SUCCESS)
         end if
         key = key // ':' // c%get_signature()
      end do
      existing => graph%get_reuse_entry(key, _RC)
      if (associated(existing)) then
         result_id = existing
         evaluated = .true.
         _RETURN(_SUCCESS)
      end if

      call export_desc%get_expression(expression, _RC)
      _RETURN_IF(len_trim(expression) == 0)
      names = parser_variables_in_expression(expression, _RC)
      allocate(input_ids(names%size()), bound_ids(names%size()))
      do i = 1, names%size()
         name => names%of(i)
         _RETURN_IF(name == item_name)
         found = find_export_node_id(this, src_comp, name, input_id, &
              input_node, _RC)
         _RETURN_UNLESS(found)
         input_desc => input_node%get_descriptor()
         input_type = input_desc%get_declared_item_type()
         _RETURN_IF(input_type == MAPL_STATEITEM_EXPRESSION)
         input_ids(i) = input_id
      end do

      ! Every input settled and comparable before anything is built.
      do i = 1, names%size()
         input_desc => mutable_descriptor(graph, input_ids(i), _RC)
         block
            type(StateItemDescriptor) :: goal
            call input_goal(input_desc, goal, _RC)
            call resolve_characteristics(input_desc%get_characteristics(), &
                 goal%get_characteristics(), &
                 input_desc%get_declared_item_type(), mismatched, adopted, &
                 status_conflict, rc=status, waiting=input_waiting)
            _VERIFY(status)
         end block
         if (input_waiting) then
            waiting = .true.
            _RETURN(_SUCCESS)
         end if
         if (status_conflict /= '') then
            problem = status_conflict
            _RETURN(_SUCCESS)
         end if
      end do

      ! Inputs need real fields for the transform to compute.
      if (graph_mode() /= GRAPH_MODE_REGISTRY) then
         do i = 1, names%size()
            name => names%of(i)
            ! ExpressionTransform reads each input as a Field, so a bundle
            ! input (VECTOR, SERVICE, BRACKET, ...) is refused here rather
            ! than failing when the transform computes.
            input_desc => mutable_descriptor(graph, input_ids(i), _RC)
            input_type = input_desc%get_declared_item_type()
            if (input_type /= MAPL_STATEITEM_FIELD) then
               problem = 'unsupported_item_class:' // itemtype_name(input_type)
               _RETURN(_SUCCESS)
            end if
            call ensure_export_payload(this, src_comp, name, graph, input_ids(i), input_desc, input_desc, &
                 name, .true., failure, _RC)
            if (failure /= '') then
               problem = failure
               _RETURN(_SUCCESS)
            end if
         end do
      end if

      do i = 1, names%size()
         input_desc => mutable_descriptor(graph, input_ids(i), _RC)
         bound_ids(i) = input_ids(i)
         block
            type(StateItemDescriptor) :: goal, full_goal
            call input_goal(input_desc, goal, _RC)
            call resolve_characteristics(input_desc%get_characteristics(), &
                 goal%get_characteristics(), &
                 input_desc%get_declared_item_type(), mismatched, adopted, &
                 status_conflict, _RC)
            if (size(mismatched) == 0) cycle
            call find_or_build_extension_chain(graph, net_id, input_ids(i), &
                 input_desc%get_characteristics(), goal%get_characteristics(), &
                 mismatched, bound_ids(i), intermediates, unsupported_kind, &
                 conflict=conflict, _RC)
            _RETURN_IF(conflict /= '')
            if (unsupported_kind /= '') then
               problem = unsupported_kind
               strict = .true.
               _RETURN(_SUCCESS)
            end if
            if (size(intermediates) == 0) cycle
            if (materialize_extensions_enabled()) then
               call input_goal(input_desc, full_goal, full=.true., _RC)
               ! StateEval finds each input by its field name.
               call materialize_extension_chain(input_desc, &
                    full_goal, intermediates, graph, failure, &
                    name=names%of(i), _RC)
               if (failure /= '') then
                  problem = failure
                  _RETURN(_SUCCESS)
               end if
            end if
         end block
      end do

      transformer = ExpressionTransform(expression, _RC)
      transform_id = graph%next_node_id(_RC)
      call graph%register_node(TransformGraphNode(transform_id, transformer), _RC)
      result_id = graph%next_node_id(_RC)
      call graph%register_node(StateItemNode(result_id, empty_payload, &
           empty_revision), _RC)

      ! Filled entry by entry: whole-descriptor assignment copies a map of
      ! polymorphic values, which gfortran does not do reliably.
      stored => mutable_descriptor(graph, result_id, _RC)
      call stored%set_declared_item_type(MAPL_STATEITEM_FIELD)
      export_map = export_desc%get_characteristics()
      iter = export_map%ftn_begin()
      do while (iter /= export_map%ftn_end())
         call iter%next()
         call stored%set_characteristic(iter%first(), iter%second(), _RC)
      end do
      do k = 1, size(kinds)
         call result_characteristic(k, c, has, _RC)
         if (has) call stored%set_characteristic(kinds(k), c, _RC)
      end do
      call stored%add_expression_result_defaults(_RC)

      do i = 1, names%size()
         name => names%of(i)
         call graph%bind_port(net_id, transform_id, name, bound_ids(i), _RC)
         call graph%add_dependency(net_id, bound_ids(i), transform_id, _RC)
      end do
      call graph%bind_port(net_id, transform_id, 'destination_field', &
           result_id, _RC)
      call graph%add_dependency(net_id, transform_id, result_id, _RC)
      call graph%add_reuse_entry(key, result_id, _RC)
      evaluated = .true.

      _RETURN(_SUCCESS)
   contains

      ! The result's value for adopted kind kinds(k): a SPECIFIED value
      ! declared on the expression where the kind lets it win; else the
      ! consumer's unless it mirrors; else the expression's own.
      subroutine result_characteristic(k, value, has, rc)
         integer, intent(in) :: k
         class(StateItemCharacteristic), allocatable, intent(out) :: value
         logical, intent(out) :: has
         integer, optional, intent(out) :: rc

         integer :: status

         has = .false.
         if (declared_wins(k) .and. &
              export_desc%has_characteristic(kinds(k))) then
            call export_desc%get_characteristic(kinds(k), value, _RC)
            has = value%get_status() == CHARACTERISTIC_STATUS_SPECIFIED
            if (has) then
               _RETURN(_SUCCESS)
            end if
            deallocate(value)
         end if
         if (import_item%has_characteristic(kinds(k))) then
            call import_item%get_characteristic(kinds(k), value, _RC)
            has = .not. (value%get_status() == CHARACTERISTIC_STATUS_MIRRORED)
            if (has) then
               _RETURN(_SUCCESS)
            end if
            deallocate(value)
         end if
         if (export_desc%has_characteristic(kinds(k))) then
            call export_desc%get_characteristic(kinds(k), value, _RC)
            has = .true.
         end if
         _RETURN(_SUCCESS)
      end subroutine result_characteristic

      ! What an input must become: the result's value for each adopted kind
      ! the input has. With `full`, also the input's other entries (for
      ! materialization), so units and dimensions stay the input's own.
      subroutine input_goal(input, goal, rc, full)
         type(StateItemDescriptor), target, intent(in) :: input
         type(StateItemDescriptor), intent(inout) :: goal
         integer, optional, intent(out) :: rc
         logical, optional, intent(in) :: full

         integer :: status, j
         type(StateItemCharacteristicKind), allocatable :: present_kinds(:)
         class(StateItemCharacteristic), allocatable :: value
         logical :: has_value

         call goal%set_declared_item_type(input%get_declared_item_type())
         if (present(full)) then
            if (full) then
               ! Entry by entry: no copy of the polymorphic map.
               call input%get_kinds(present_kinds)
               do j = 1, size(present_kinds)
                  call input%get_characteristic(present_kinds(j), value, _RC)
                  call goal%set_characteristic(present_kinds(j), value, _RC)
                  deallocate(value)
               end do
            end if
         end if
         do j = 1, size(kinds)
            if (.not. input%has_characteristic(kinds(j))) cycle
            call result_characteristic(j, value, has_value, _RC)
            if (has_value) call goal%set_characteristic(kinds(j), value, _RC)
         end do
         _RETURN(_SUCCESS)
      end subroutine input_goal

   end subroutine build_expression_evaluation

end submodule GraphBuilder_ExpressionResolution_smod
