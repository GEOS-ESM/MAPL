#include "MAPL.h"

!------------------------------------------------------------------------------
! StateItemNode: concrete GraphNode holding exactly one GraphStateItem payload
! and one NodeRevision (spec/03-graph-node-hierarchy.md REQ-NODE-003a,
! spec/04-graph-value-hierarchy.md REQ-SI-005), in addition to the NodeId
! and lifecycle-status properties inherited from BaseGraphNode. No
! adjacency.
!------------------------------------------------------------------------------
module mapl_StateItemNode_mod
   use esmf, only: ESMF_StateIntent_Flag, ESMF_STATEINTENT_INVALID
   use mapl_BaseGraphNode_mod, only: BaseGraphNode
   use mapl_NodeId_mod, only: NodeId
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_NodeRevision_mod, only: NodeRevision
   use mapl_CallbackInterfaceId_mod, only: CallbackInterfaceId
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: StateItemNode

   ! openspec/changes/graph-builder-stateitem-resolution, design.md
   ! Decisions: four new fields, set exactly once by advertise_one
   ! (GraphBuilder_Advertising_smod.F90), via either the constructor
   ! (the flat-item path) or the setters below (the composite-item path,
   ! design.md correction surfaced during implementation -
   ! materialize_composite/CompositeStateMaterialization.F90 constructs
   ! a composite's own top-level node with no identity args at all, so
   ! advertise_one sets identity on it afterward, via graph%get_node(),
   ! the same post-construction pattern it already uses to populate
   ! characteristics). Setters are deliberately NOT private - callable
   ! from advertise_one (a different module) for exactly this reason -
   ! but are documented framework-internal, same precedent as
   ! set_payload/set_revision above. Identity
   ! (short_name/state_intent, mirroring what item_key() already encodes
   ! as a string) and resolution-mode metadata
   ! (is_inout_borrower/callback_interface_id, plain copies of the
   ! identically-named VariableSpec fields), read directly off the node
   ! by every later resolution step instead of re-derived from a
   ! VariableSpec reached by component name. short_name is unallocated
   ! and state_intent is ESMF_STATEINTENT_INVALID for a node never given
   ! an identity (e.g. a cross-component proxy or callback-collection
   ! node, which have no originating VariableSpec at all) - callers that
   ! care check get_short_name()'s allocation status / get_state_intent()
   ! against ESMF_STATEINTENT_INVALID, same as any other "not set" case
   ! in this codebase.
   type, extends(BaseGraphNode) :: StateItemNode
      private
      type(GraphStateItem) :: payload
      type(NodeRevision) :: revision
      character(:), allocatable :: short_name
      type(ESMF_StateIntent_Flag) :: state_intent = ESMF_STATEINTENT_INVALID
      logical :: is_inout_borrower_ = .false.
      type(CallbackInterfaceId) :: callback_interface_id
   contains
      procedure :: get_payload => stateitemnode_get_payload
      procedure :: get_payload_ptr => stateitemnode_get_payload_ptr
      procedure :: set_payload => stateitemnode_set_payload
      procedure :: get_revision => stateitemnode_get_revision
      procedure :: set_revision => stateitemnode_set_revision
      procedure :: advance_revision => stateitemnode_advance_revision
      procedure :: get_short_name => stateitemnode_get_short_name
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
   ! all optional, defaulted so every existing call site (synthetic test
   ! nodes, cross-component proxies, callback-collection nodes - none of
   ! which have an originating VariableSpec identity) compiles and
   ! behaves unchanged (tasks.md 1.1).
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
      ! openspec/changes/graph-builder-stateitem-resolution: explicit
      ! allocate(..., source=...) instead of plain `=` assignment into
      ! a deferred-length character component - gfortran has known
      ! memory-corruption bugs on the realloc-on-assignment path for
      ! allocatable character components (user-identified during
      ! gfortran portability testing of this change), not reliably
      ! reproducible run-to-run, consistent with heap corruption rather
      ! than a deterministic logic error.
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
      end if
   end function stateitemnode_get_short_name

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

   ! openspec/changes/graph-builder-stateitem-resolution: pointer
   ! accessor into this node's own stored payload, deliberately NOT
   ! going through an intrinsic assignment the way get_payload()'s own
   ! `payload = this%payload` does - a defined-assignment-bearing
   ! derived type (GraphStateItem's own StateItemMemberMap component,
   ! for a composite item, invokes gFTL2's own deep_copy) is not safe to
   ! copy this way under every compiler for every payload shape
   ! (observed: gfortran -fcheck=all -finit-real=snan nondeterministically
   ! segfaults inside StateItemMemberMap's own deep_copy when the member
   ! map is actually populated - a composite item's own case - NAG does
   ! not reproduce it, consistent with a known-hard class of Fortran
   ! compiler bug around nested defined-assignment, not a logic error in
   ! this codebase's own gFTL2 usage). Callers that only need to read or
   ! mutate the payload IN PLACE (never replace it wholesale) should
   ! prefer this over get_payload()/set_payload() - advertise_one's own
   ! populate_characteristics is the first such caller
   ! (GraphBuilder_Advertising_smod.F90) and the direct motivation for
   ! adding this accessor. `this` must be a target - callers hold node
   ! via a class(StateItemNode), pointer already (graph%get_node()'s own
   ! contract), so this is never a new requirement in practice.
   function stateitemnode_get_payload_ptr(this) result(payload)
      class(StateItemNode), target, intent(in) :: this
      type(GraphStateItem), pointer :: payload

      payload => this%payload
   end function stateitemnode_get_payload_ptr

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

   ! Raw revision assignment - retained from Phase 1 for construction-time
   ! setup (existing tests/callers depend on it). REQ-REV-002/003's
   ! "only advance, never assign an arbitrary value to mean 'changed'"
   ! path is advance_revision() below; that is what the demand-driven
   ! update engine (spec/11-revision-and-update.md) uses.
   subroutine stateitemnode_set_revision(this, revision)
      class(StateItemNode), intent(inout) :: this
      type(NodeRevision), intent(in) :: revision

      this%revision = revision
   end subroutine stateitemnode_set_revision

   ! REQ-REV-002/003: the only mutation path that means "this node's
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
