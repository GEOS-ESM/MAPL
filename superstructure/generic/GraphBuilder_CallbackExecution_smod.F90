#include "MAPL.h"

! Invoker-side callback execution: CallbackCollectionIterator walks the
! Handler members the CallbackMethodBindingMap already bound at CONNECT
! (resolve_callback_destination), and CallbackMemberHandle%execute()
! runs invoke_callback_method's existing three phases for one (member,
! method) pair unchanged.
submodule (mapl_GraphBuilder_mod) GraphBuilder_CallbackExecution_smod
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_StateItemMemberMap_mod, only: StateItemMemberMapIterator, operator(/=)
   use mapl_ErrorHandling_mod
   implicit none(type, external)

contains

   ! Resolves the Invoker's own IMPORT (by short name) to the collection
   ! node resolve_callback_destination already materialized for it - the
   ! same resource-index/reuse-entry lookup graphbuilder_fill_callback_collections
   ! uses. An import with no resource index, or no reuse entry (never
   ! matched/connected), yields a zero-member iterator rather than an
   ! error: an unresolved callback import is reported elsewhere.
   module function begin_callback_collection(this, import_short_name, rc) result(iter)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: import_short_name
      integer, optional, intent(out) :: rc
      type(CallbackCollectionIterator) :: iter

      integer :: status
      type(ComponentGraph), pointer :: graph
      type(NodeId), pointer :: id_ptr, collection_id
      character(:), allocatable :: collection_key, msg
      class(GraphNode), pointer :: generic_node
      type(GraphStateItem), pointer :: payload
      type(StateItemMemberMap), pointer :: members
      type(StateItemMemberMapIterator) :: member_iter

      graph => this%get_component_graph()
      iter%graph => graph
      iter%member_names = StringVector()
      iter%position = 0

      id_ptr => graph%get_resource_index(item_key(ESMF_STATEINTENT_IMPORT, import_short_name))
      if (.not. associated(id_ptr)) then
         _RETURN(_SUCCESS)
      end if

      collection_key = 'CALLBACK_COLLECTION:' // id_ptr%to_string()
      collection_id => graph%get_reuse_entry(collection_key, _RC)
      if (.not. associated(collection_id)) then
         _RETURN(_SUCCESS)
      end if

      generic_node => graph%get_node(collection_id)
      msg = 'GraphBuilder: begin_callback_collection - collection NodeId not found: ' // import_short_name
      _ASSERT(associated(generic_node), msg)
      select type (generic_node)
      class is (StateItemNode)
         iter%collection_node => generic_node
      class default
         _FAIL('GraphBuilder: begin_callback_collection - collection node is not a StateItemNode: ' // import_short_name)
      end select

      payload => iter%collection_node%get_payload_ptr()
      members => payload%state_members_ptr()

      ! Collect names first, same idiom fill_callback_collection uses:
      ! the returned iterator must stay valid across the caller's whole
      ! loop, including intervening %execute() calls, so it cannot hold
      ! a live StateItemMemberMap iterator into a map later code could
      ! in principle touch again.
      member_iter = members%ftn_begin()
      do while (member_iter /= members%ftn_end())
         call member_iter%next()
         call iter%member_names%push_back(member_iter%first())
      end do

      _RETURN(_SUCCESS)
   end function begin_callback_collection

   module subroutine callback_collection_iterator_advance(this)
      class(CallbackCollectionIterator), intent(inout) :: this

      this%position = this%position + 1
   end subroutine callback_collection_iterator_advance

   module function callback_collection_iterator_done(this) result(is_done)
      class(CallbackCollectionIterator), intent(in) :: this
      logical :: is_done

      is_done = this%position > this%member_names%size()
   end function callback_collection_iterator_done

   ! Only meaningful after at least one %advance() and before %done() is
   ! true - a caller-ordering bug here fails loudly rather than indexing
   ! the collected name list out of range.
   module function callback_collection_iterator_current(this, rc) result(handle)
      class(CallbackCollectionIterator), intent(in) :: this
      integer, optional, intent(out) :: rc
      type(CallbackMemberHandle) :: handle

      integer :: status
      character(:), allocatable :: name, msg
      type(StateItemMemberMap), pointer :: members
      type(NodeId), pointer :: member_id
      class(GraphNode), pointer :: member_node
      type(GraphStateItem), pointer :: member_payload
      type(GraphStateItem), pointer :: collection_payload

      msg = 'CallbackCollectionIterator: current() called before advance() or after done()'
      _ASSERT(this%position >= 1 .and. this%position <= this%member_names%size(), msg)

      name = this%member_names%of(this%position)

      collection_payload => this%collection_node%get_payload_ptr()
      members => collection_payload%state_members_ptr()
      member_id => members%at(name)
      msg = 'CallbackCollectionIterator: current() - matched member not found: ' // name
      _ASSERT(associated(member_id), msg)

      member_node => this%graph%get_node(member_id)
      msg = 'CallbackCollectionIterator: current() - matched member NodeId not found: ' // name
      _ASSERT(associated(member_node), msg)
      select type (member_node)
      class is (StateItemNode)
         member_payload => member_node%get_payload_ptr()
      class default
         _FAIL('CallbackCollectionIterator: current() - matched member is not a StateItemNode: ' // name)
      end select

      handle%graph => this%graph
      handle%collection_node => this%collection_node
      handle%member_qualified_name = name
      handle%member_state = member_payload%get_state(_RC)

      _RETURN(_SUCCESS)
   end function callback_collection_iterator_current

   ! Unknown method name fails before any pull/invoke/advance happens -
   ! the map lookup itself is the "check" phase; invoke_callback_method's
   ! three existing phases (pull_callback_get_arguments, method_node%invoke,
   ! advance_callback_put_arguments) are the rest, unchanged.
   module subroutine callback_member_handle_execute(this, method, rc)
      class(CallbackMemberHandle), intent(in) :: this
      character(*), intent(in) :: method
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: key, msg
      type(CallbackMethodBinding), pointer :: binding

      _ASSERT(associated(this%collection_node), 'CallbackMemberHandle: execute() called on an invalid handle')

      key = callback_method_binding_key(this%member_qualified_name, method)
      binding => this%collection_node%get_callback_method_binding(key)
      msg = 'CallbackMemberHandle: execute() - unknown method "' // method // &
           '" for member: ' // this%member_qualified_name
      _ASSERT(associated(binding), msg)

      call invoke_callback_method(this%graph, binding, _RC)

      _RETURN(_SUCCESS)
   end subroutine callback_member_handle_execute

   module function callback_member_handle_get_state(this, rc) result(state)
      class(CallbackMemberHandle), intent(in) :: this
      integer, optional, intent(out) :: rc
      type(ESMF_State) :: state

      _ASSERT(associated(this%collection_node), 'CallbackMemberHandle: get_state() called on an invalid handle')

      state = this%member_state

      _RETURN(_SUCCESS)
   end function callback_member_handle_get_state

   ! The matched export's own qualified name (materialize_callback_collection),
   ! e.g. "cb" for a single top-level Handler or "HANDLER2/cb" when more
   ! than one Handler matches - distinguishes which matched member a
   ! handle came from without comparing Field identity (members of
   ! different matched Handlers for the same argument may share storage;
   ! see docs/graph/callbacks.md).
   module function callback_member_handle_get_member_qualified_name(this) result(name)
      class(CallbackMemberHandle), intent(in) :: this
      character(:), allocatable :: name

      if (allocated(this%member_qualified_name)) then
         name = this%member_qualified_name
      else
         name = ''
      end if
   end function callback_member_handle_get_member_qualified_name

end submodule GraphBuilder_CallbackExecution_smod
