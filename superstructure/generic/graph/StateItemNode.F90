#include "MAPL.h"

!------------------------------------------------------------------------------
! StateItemNode: concrete GraphNode holding exactly one GraphStateItem payload
! (the ESMF handle), one StateItemDescriptor (the item's description), and one
! NodeRevision, in addition to the NodeId and
! lifecycle status inherited from BaseGraphNode. No adjacency.
!------------------------------------------------------------------------------
module mapl_StateItemNode_mod
   use esmf, only: ESMF_StateIntent_Flag, ESMF_STATEINTENT_INVALID
   use mapl_BaseGraphNode_mod, only: BaseGraphNode
   use mapl_NodeId_mod, only: NodeId
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_StateItemDescriptor_mod, only: StateItemDescriptor
   use mapl_NodeRevision_mod, only: NodeRevision
   use mapl_CallbackInterfaceId_mod, only: CallbackInterfaceId
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: StateItemNode

   ! Identity (short_name/state_intent, mirroring what item_key() encodes as a
   ! string) and resolution-mode metadata (is_inout_borrower/
   ! callback_interface_id, copies of the VariableSpec fields of the same
   ! names), set once by advertise_one (GraphBuilder_Advertising_smod.F90) and
   ! read directly off the node by later resolution steps instead of being
   ! re-derived from a VariableSpec looked up by component name.
   !
   ! They are set via the constructor (flat-item path) or the setters below
   ! (composite path: materialize_composite constructs a composite's top-level
   ! node with no identity args, so advertise_one sets identity afterward via
   ! graph%get_node()). The setters are public because advertise_one is in a
   ! different module; they are framework-internal, like set_payload/
   ! set_revision.
   !
   ! short_name is unset (get_short_name() returns '') and state_intent is
   ! ESMF_STATEINTENT_INVALID for a node never given an identity (e.g. a
   ! cross-component proxy or callback-collection node, with no originating
   ! VariableSpec); callers check has_short_name() / get_state_intent().
   type, extends(BaseGraphNode) :: StateItemNode
      private
      type(GraphStateItem) :: payload
      type(StateItemDescriptor) :: descriptor
      type(NodeRevision) :: revision
      character(:), allocatable :: short_name
      type(ESMF_StateIntent_Flag) :: state_intent = ESMF_STATEINTENT_INVALID
      logical :: is_inout_borrower_ = .false.
      type(CallbackInterfaceId) :: callback_interface_id
   contains
      procedure :: get_payload => stateitemnode_get_payload
      procedure :: get_payload_ptr => stateitemnode_get_payload_ptr
      procedure :: set_payload => stateitemnode_set_payload
      procedure :: get_descriptor => stateitemnode_get_descriptor
      procedure :: get_revision => stateitemnode_get_revision
      procedure :: set_revision => stateitemnode_set_revision
      procedure :: advance_revision => stateitemnode_advance_revision
      procedure :: get_short_name => stateitemnode_get_short_name
      procedure :: has_short_name => stateitemnode_has_short_name
      procedure :: set_short_name => stateitemnode_set_short_name
      procedure :: get_state_intent => stateitemnode_get_state_intent
      procedure :: set_state_intent => stateitemnode_set_state_intent
      procedure :: is_inout_borrower => stateitemnode_is_inout_borrower
      procedure :: set_is_inout_borrower => stateitemnode_set_is_inout_borrower
      procedure :: get_callback_interface_id => stateitemnode_get_callback_interface_id
      procedure :: set_callback_interface_id => stateitemnode_set_callback_interface_id
   end type StateItemNode

   interface StateItemNode
      module procedure new_StateItemNode
   end interface StateItemNode

contains

   ! short_name/state_intent/is_inout_borrower/callback_interface_id are
   ! optional and defaulted, so nodes without a VariableSpec identity (synthetic
   ! test nodes, cross-component proxies, callback-collection nodes) compile and
   ! behave unchanged.
   function new_StateItemNode(id, payload, revision, short_name, state_intent, &
        is_inout_borrower, callback_interface_id) result(node)
      type(NodeId), intent(in) :: id
      type(GraphStateItem), intent(in) :: payload
      type(NodeRevision), intent(in) :: revision
      character(*), optional, intent(in) :: short_name
      type(ESMF_StateIntent_Flag), optional, intent(in) :: state_intent
      logical, optional, intent(in) :: is_inout_borrower
      type(CallbackInterfaceId), optional, intent(in) :: callback_interface_id
      type(StateItemNode) :: node

      node%BaseGraphNode = BaseGraphNode(id)
      node%payload = payload
      node%revision = revision
      ! Explicit allocate(..., source=...) instead of `=` into a deferred-length
      ! character component: gfortran's realloc-on-assignment path for allocatable
      ! character components corrupts the heap (not reproducible run-to-run).
      if (present(short_name)) then
         if (allocated(node%short_name)) deallocate(node%short_name)
         allocate(node%short_name, source=short_name)
      end if
      if (present(state_intent)) node%state_intent = state_intent
      if (present(is_inout_borrower)) node%is_inout_borrower_ = is_inout_borrower
      if (present(callback_interface_id)) node%callback_interface_id = callback_interface_id
   end function new_StateItemNode

   function stateitemnode_get_short_name(this) result(short_name)
      class(StateItemNode), intent(in) :: this
      character(:), allocatable :: short_name

      if (allocated(this%short_name)) then
         allocate(short_name, source=this%short_name)
      else
         allocate(short_name, source='')
      end if
   end function stateitemnode_get_short_name

   logical function stateitemnode_has_short_name(this) result(has)
      class(StateItemNode), intent(in) :: this
      has = allocated(this%short_name)
   end function stateitemnode_has_short_name

   subroutine stateitemnode_set_short_name(this, short_name)
      class(StateItemNode), intent(inout) :: this
      character(*), intent(in) :: short_name

      if (allocated(this%short_name)) deallocate(this%short_name)
      allocate(this%short_name, source=short_name)
   end subroutine stateitemnode_set_short_name

   function stateitemnode_get_state_intent(this) result(state_intent)
      class(StateItemNode), intent(in) :: this
      type(ESMF_StateIntent_Flag) :: state_intent

      state_intent = this%state_intent
   end function stateitemnode_get_state_intent

   subroutine stateitemnode_set_state_intent(this, state_intent)
      class(StateItemNode), intent(inout) :: this
      type(ESMF_StateIntent_Flag), intent(in) :: state_intent

      this%state_intent = state_intent
   end subroutine stateitemnode_set_state_intent

   logical function stateitemnode_is_inout_borrower(this) result(is_borrower)
      class(StateItemNode), intent(in) :: this

      is_borrower = this%is_inout_borrower_
   end function stateitemnode_is_inout_borrower

   subroutine stateitemnode_set_is_inout_borrower(this, is_inout_borrower)
      class(StateItemNode), intent(inout) :: this
      logical, intent(in) :: is_inout_borrower

      this%is_inout_borrower_ = is_inout_borrower
   end subroutine stateitemnode_set_is_inout_borrower

   function stateitemnode_get_callback_interface_id(this) result(callback_interface_id)
      class(StateItemNode), intent(in) :: this
      type(CallbackInterfaceId) :: callback_interface_id

      callback_interface_id = this%callback_interface_id
   end function stateitemnode_get_callback_interface_id

   subroutine stateitemnode_set_callback_interface_id(this, callback_interface_id)
      class(StateItemNode), intent(inout) :: this
      type(CallbackInterfaceId), intent(in) :: callback_interface_id

      this%callback_interface_id = callback_interface_id
   end subroutine stateitemnode_set_callback_interface_id

   function stateitemnode_get_payload(this) result(payload)
      class(StateItemNode), intent(in) :: this
      type(GraphStateItem) :: payload

      payload = this%payload
   end function stateitemnode_get_payload

   ! Pointer accessor into the stored payload, deliberately not the
   ! intrinsic assignment get_payload() uses (`payload = this%payload`): copying
   ! a populated StateItemMemberMap (gFTL2 defined-assignment deep_copy, in a
   ! composite item) segfaults nondeterministically under gfortran, not under
   ! NAG. Callers that only read or mutate the payload in place should prefer
   ! this over get_payload()/set_payload() (e.g. try_alias_export_field in
   ! GraphBuilder_ConnectionResolution_smod.F90). `this` must be a target; callers
   ! already hold the node via a class(StateItemNode), pointer.
   function stateitemnode_get_payload_ptr(this) result(payload)
      class(StateItemNode), target, intent(in) :: this
      type(GraphStateItem), pointer :: payload

      payload => this%payload
   end function stateitemnode_get_payload_ptr

   ! Pointer accessor: the descriptor holds a populated characteristics map,
   ! which is not safe to copy by assignment under gfortran. `this` must be a
   ! target.
   function stateitemnode_get_descriptor(this) result(descriptor)
      class(StateItemNode), target, intent(in) :: this
      type(StateItemDescriptor), pointer :: descriptor

      descriptor => this%descriptor
   end function stateitemnode_get_descriptor

   subroutine stateitemnode_set_payload(this, payload)
      class(StateItemNode), intent(inout) :: this
      type(GraphStateItem), intent(in) :: payload

      this%payload = payload
   end subroutine stateitemnode_set_payload

   function stateitemnode_get_revision(this) result(revision)
      class(StateItemNode), intent(in) :: this
      type(NodeRevision) :: revision

      revision = this%revision
   end function stateitemnode_get_revision

   ! Raw revision assignment, for construction-time setup. The usual
   ! path (only advance, never assign an arbitrary value to mean changed)
   ! is advance_revision() below, which the demand-driven update uses.
   subroutine stateitemnode_set_revision(this, revision)
      class(StateItemNode), intent(inout) :: this
      type(NodeRevision), intent(in) :: revision

      this%revision = revision
   end subroutine stateitemnode_set_revision

   ! The only mutation path that means "this node's
   ! logical value just changed" - delegates to NodeRevision%advance(),
   ! never assigns a caller-fabricated value.
   subroutine stateitemnode_advance_revision(this, rc)
      class(StateItemNode), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call this%revision%advance(_RC)

      _RETURN(_SUCCESS)
   end subroutine stateitemnode_advance_revision

end module mapl_StateItemNode_mod
