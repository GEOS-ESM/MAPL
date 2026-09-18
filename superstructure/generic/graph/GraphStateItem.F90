#include "MAPL.h"

!------------------------------------------------------------------------------
! GraphStateItem: concrete, non-polymorphic node payload type
! (spec/04-graph-value-hierarchy.md REQ-SI-001/002). Three allocatable
! ESMF handle components - a field, a field bundle, and a state - of
! which at most one may be allocated at any time (REQ-SI-004). No
! separate route-handle component: a route handle is represented by
! allocating the state component in a persistent wrapper role, holding
! exactly one ESMF_RouteHandle member (REQ-SI-002a).
!
! Exposes a two-tier classification: itemType() (native ESMF
! classification - FIELD/FIELDBUNDLE/STATE/NOTFOUND, matching ESMF's
! own naming convention for this kind of query - purely "which
! component is allocated," for further retrieval via get_field()/
! get_field_bundle()/get_state()) and variant() (new, extensible
! MAPL_StateItem_Flag, Info-backed; REQ-SI-002b/002c - "which
! subclass/role this instance plays," including the route-handle-
! wrapper role as a refinement of STATE, parallel to how
! MAPL_STATEITEM_VERTICALGRID also refines STATE). All allocation goes
! through the single generic `set` binding below, which enforces the
! at-most-one-allocated invariant and the membership-map gating
! (REQ-SI-006) at a single choke point; raw component access is
! private.
!------------------------------------------------------------------------------
module mapl_GraphStateItem_mod
   use ESMF, only: ESMF_Field, ESMF_FieldBundle, ESMF_State, ESMF_RouteHandle
   use ESMF, only: ESMF_StateItem_Flag
   use ESMF, only: ESMF_STATEITEM_FIELD, ESMF_STATEITEM_FIELDBUNDLE, &
                   ESMF_STATEITEM_STATE, ESMF_STATEITEM_ROUTEHANDLE, &
                   ESMF_STATEITEM_NOTFOUND
   use ESMF, only: ESMF_StateCreate, ESMF_StateAdd, ESMF_StateGet
   use ESMF, only: ESMF_MAXSTR
   use ESMF, only: operator(==)
   use mapl_StateItemFlag_mod, only: MAPL_StateItem_Flag, &
                                     MAPL_STATEITEM_FIELD, MAPL_STATEITEM_FIELDBUNDLE, &
                                     MAPL_STATEITEM_STATE, MAPL_STATEITEM_ROUTEHANDLE, &
                                     MAPL_STATEITEM_NOTFOUND, operator(==), operator(/=)
   use mapl_StateItemVariantInfo_mod, only: get_variant
   use mapl_NodeId_mod, only: NodeId
   use mapl_StateItemMemberMap_mod, only: StateItemMemberMap, StateItemMemberMapIterator
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: GraphStateItem
   ! operator(==)/operator(/=) are the one legitimate exception to "don't
   ! re-export a use-associated name": they are Fortran's own idiom for
   ! merging a generic across module boundaries, and every consumer that
   ! needs to compare an MAPL_StateItem_Flag/ESMF_StateItem_Flag reached
   ! through this module needs the merged generic, not just this
   ! module's own (nonexistent) specific procedure. Every other name
   ! below was previously re-exported too (ESMF_StateItem_Flag/
   ! ESMF_STATEITEM_*, MAPL_StateItem_Flag, StateItemMemberMap*) - real
   ! plain-name re-exports of another module's own entity, not an
   ! operator-merge case, and Intel's compiler correctly rejects that
   ! when a client also reaches the same entity through its true
   ! defining module (icc/ifx error #6405). Removed; callers now import
   ! those directly from ESMF/mapl_StateItemFlag_mod/
   ! mapl_StateItemMemberMap_mod instead.
   public :: operator(==)
   public :: operator(/=)

   type :: GraphStateItem
      private
      type(ESMF_Field), allocatable :: esmf_field
      type(ESMF_FieldBundle), allocatable :: esmf_field_bundle
      type(ESMF_State), allocatable :: esmf_state
      type(StateItemMemberMap) :: field_bundle_members_map
      type(StateItemMemberMap) :: state_members_map
   contains
      procedure :: itemType => stateitem_itemType
      procedure :: variant => stateitem_variant
      procedure :: check_invariant => stateitem_check_invariant
      procedure, private :: set_field => stateitem_set_field
      procedure, private :: set_field_bundle => stateitem_set_field_bundle
      procedure, private :: set_state => stateitem_set_state
      procedure, private :: set_route_handle => stateitem_set_route_handle
      generic :: set => set_field, set_field_bundle, set_state, set_route_handle
      procedure :: get_field => stateitem_get_field
      procedure :: get_field_bundle => stateitem_get_field_bundle
      procedure :: get_state => stateitem_get_state
      procedure :: get_route_handle => stateitem_get_route_handle
      procedure :: field_bundle_members => stateitem_field_bundle_members
      procedure :: state_members => stateitem_state_members
      procedure :: add_field_bundle_member => stateitem_add_field_bundle_member
      procedure :: add_state_member => stateitem_add_state_member
      ! Test-only backdoor: deliberately violates the at-most-one-
      ! allocated invariant, bypassing the `set` choke point, so the
      ! checked-defect path (REQ-SI-004) can be exercised. Not part of
      ! the type's intended usage; production code MUST NOT call this.
      procedure :: debug_force_double_allocate_for_test => stateitem_debug_force_double_allocate
   end type GraphStateItem

contains

   ! -- classification -------------------------------------------------

   ! Purely "which of the three components is allocated" - no
   ! route-handle-wrapper special-casing here (that distinction is
   ! variant()'s job, since it's a role/subclass question, not a "which
   ! ESMF type do I retrieve" question).
   function stateitem_itemType(this, rc) result(kind_flag)
      class(GraphStateItem), intent(in) :: this
      integer, optional, intent(out) :: rc
      type(ESMF_StateItem_Flag) :: kind_flag

      integer :: status

      call this%check_invariant(_RC)

      kind_flag = ESMF_STATEITEM_NOTFOUND

      if (allocated(this%esmf_field)) then
         kind_flag = ESMF_STATEITEM_FIELD
         _RETURN(_SUCCESS)
      end if
      if (allocated(this%esmf_field_bundle)) then
         kind_flag = ESMF_STATEITEM_FIELDBUNDLE
         _RETURN(_SUCCESS)
      end if
      if (allocated(this%esmf_state)) then
         kind_flag = ESMF_STATEITEM_STATE
         _RETURN(_SUCCESS)
      end if

      _RETURN(_SUCCESS)
   end function stateitem_itemType

   ! Vanilla default per native kind (FIELD/FIELDBUNDLE/STATE/NOTFOUND,
   ! mirroring itemType()'s own vocabulary) when no finer-grained
   ! variant tag has been attached; otherwise the tagged refinement
   ! (e.g. GEOM, VECTOR, BRACKET, VECTORBRACKET, VERTICALGRID) read
   ! back via mapl_StateItemVariantInfo_mod. The route-handle-wrapper role
   ! is reported here (MAPL_STATEITEM_ROUTEHANDLE, a refinement of
   ! STATE, never queried from Info) rather than at the itemType()
   ! tier, since "is this state actually a route-handle wrapper" is a
   ! role question, exactly like "is this state actually a vertical
   ! grid."
   function stateitem_variant(this, rc) result(variant_flag)
      class(GraphStateItem), intent(in) :: this
      integer, optional, intent(out) :: rc
      type(MAPL_StateItem_Flag) :: variant_flag

      integer :: status
      logical :: found
      logical :: is_wrapper
      type(ESMF_StateItem_Flag) :: kind_flag

      kind_flag = this%itemType(_RC)

      if (kind_flag == ESMF_STATEITEM_FIELD) then
         variant_flag = get_variant(this%esmf_field, found=found, _RC)
         if (.not. found) variant_flag = MAPL_STATEITEM_FIELD
         _RETURN(_SUCCESS)
      end if
      if (kind_flag == ESMF_STATEITEM_FIELDBUNDLE) then
         variant_flag = get_variant(this%esmf_field_bundle, found=found, _RC)
         if (.not. found) variant_flag = MAPL_STATEITEM_FIELDBUNDLE
         _RETURN(_SUCCESS)
      end if
      if (kind_flag == ESMF_STATEITEM_STATE) then
         is_wrapper = state_is_route_handle_wrapper(this, _RC)
         if (is_wrapper) then
            variant_flag = MAPL_STATEITEM_ROUTEHANDLE
            _RETURN(_SUCCESS)
         end if
         variant_flag = get_variant(this%esmf_state, found=found, _RC)
         if (.not. found) variant_flag = MAPL_STATEITEM_STATE
         _RETURN(_SUCCESS)
      end if
      variant_flag = MAPL_STATEITEM_NOTFOUND

      _RETURN(_SUCCESS)
   end function stateitem_variant

   ! Inspects the sole member of an allocated esmf_state via
   ! ESMF_StateGet and reports whether it is itself a route handle -
   ! the one place this module still consults ESMF's native
   ! classification below the GraphStateItem/itemType() tier. When it is,
   ! also returns the member's actual item name (assigned internally by
   ! ESMF_StateAdd - there is no way to choose it, so callers that need
   ! to retrieve the member back out must use whatever name ESMF gave
   ! it, not an assumed fixed constant).
   logical function state_is_route_handle_wrapper(this, rc, member_name) result(is_wrapper)
      class(GraphStateItem), intent(in) :: this
      integer, optional, intent(out) :: rc
      character(:), allocatable, optional, intent(out) :: member_name

      integer :: status
      integer :: item_count
      type(ESMF_StateItem_Flag), allocatable :: item_types(:)
      character(len=ESMF_MAXSTR), allocatable :: item_names(:)

      is_wrapper = .false.

      call ESMF_StateGet(this%esmf_state, itemCount=item_count, _RC)
      if (item_count > 0) then
         allocate(item_types(item_count))
         allocate(item_names(item_count))
         call ESMF_StateGet(this%esmf_state, itemNameList=item_names, itemTypeList=item_types, _RC)
         is_wrapper = (item_types(1) == ESMF_STATEITEM_ROUTEHANDLE)
         if (is_wrapper .and. present(member_name)) member_name = trim(item_names(1))
      end if

      _RETURN(_SUCCESS)
   end function state_is_route_handle_wrapper

   subroutine stateitem_check_invariant(this, rc)
      class(GraphStateItem), intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: allocated_count

      allocated_count = 0
      if (allocated(this%esmf_field)) allocated_count = allocated_count + 1
      if (allocated(this%esmf_field_bundle)) allocated_count = allocated_count + 1
      if (allocated(this%esmf_state)) allocated_count = allocated_count + 1

      _ASSERT(allocated_count <= 1, 'GraphStateItem: more than one of esmf_field/esmf_field_bundle/esmf_state is allocated')

      _RETURN(_SUCCESS)
   end subroutine stateitem_check_invariant

   ! -- mutators (single choke point for the allocation invariant) -----

   subroutine stateitem_set_field(this, field, rc)
      class(GraphStateItem), intent(inout) :: this
      type(ESMF_Field), intent(in) :: field
      integer, optional, intent(out) :: rc

      call clear_components(this)
      this%esmf_field = field

      _RETURN(_SUCCESS)
   end subroutine stateitem_set_field

   subroutine stateitem_set_field_bundle(this, field_bundle, rc)
      class(GraphStateItem), intent(inout) :: this
      type(ESMF_FieldBundle), intent(in) :: field_bundle
      integer, optional, intent(out) :: rc

      call clear_components(this)
      this%esmf_field_bundle = field_bundle

      _RETURN(_SUCCESS)
   end subroutine stateitem_set_field_bundle

   subroutine stateitem_set_state(this, state, rc)
      class(GraphStateItem), intent(inout) :: this
      type(ESMF_State), intent(in) :: state
      integer, optional, intent(out) :: rc

      call clear_components(this)
      this%esmf_state = state

      _RETURN(_SUCCESS)
   end subroutine stateitem_set_state

   ! Convenience overload of `set` for the route-handle-wrapper role:
   ! builds a small ESMF_State containing exactly one ESMF_RouteHandle
   ! member and installs it via set_state, so the wrapper's creation
   ! goes through the same choke point as any other state
   ! (REQ-SI-002a). Renewal (destroying/recreating the underlying route
   ! handle) should replace the member on the existing wrapper state
   ! directly via ESMF_State calls on get_state()'s result, rather than
   ! calling this a second time - the wrapper state object itself is
   ! not destroyed/recreated.
   subroutine stateitem_set_route_handle(this, route_handle, rc)
      class(GraphStateItem), intent(inout) :: this
      type(ESMF_RouteHandle), intent(in) :: route_handle
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_State) :: wrapper

      wrapper = ESMF_StateCreate(_RC)
      call ESMF_StateAdd(wrapper, [route_handle], _RC)
      call this%set_state(wrapper, _RC)

      _RETURN(_SUCCESS)
   end subroutine stateitem_set_route_handle

   subroutine clear_components(this)
      class(GraphStateItem), intent(inout) :: this

      if (allocated(this%esmf_field)) deallocate(this%esmf_field)
      if (allocated(this%esmf_field_bundle)) deallocate(this%esmf_field_bundle)
      if (allocated(this%esmf_state)) deallocate(this%esmf_state)
      this%field_bundle_members_map = StateItemMemberMap()
      this%state_members_map = StateItemMemberMap()
   end subroutine clear_components

   ! -- accessors --------------------------------------------------------

   function stateitem_get_field(this, rc) result(field)
      class(GraphStateItem), intent(in) :: this
      integer, optional, intent(out) :: rc
      type(ESMF_Field) :: field

      _ASSERT(allocated(this%esmf_field), 'GraphStateItem: esmf_field is not allocated')
      field = this%esmf_field

      _RETURN(_SUCCESS)
   end function stateitem_get_field

   function stateitem_get_field_bundle(this, rc) result(field_bundle)
      class(GraphStateItem), intent(in) :: this
      integer, optional, intent(out) :: rc
      type(ESMF_FieldBundle) :: field_bundle

      _ASSERT(allocated(this%esmf_field_bundle), 'GraphStateItem: esmf_field_bundle is not allocated')
      field_bundle = this%esmf_field_bundle

      _RETURN(_SUCCESS)
   end function stateitem_get_field_bundle

   function stateitem_get_state(this, rc) result(state)
      class(GraphStateItem), intent(in) :: this
      integer, optional, intent(out) :: rc
      type(ESMF_State) :: state

      _ASSERT(allocated(this%esmf_state), 'GraphStateItem: esmf_state is not allocated')
      state = this%esmf_state

      _RETURN(_SUCCESS)
   end function stateitem_get_state

   ! Resolves the route handle out of the wrapper's sole member, rather
   ! than exposing a bare stored route-handle field (REQ-SI-002a).
   ! Gated on variant() (the role/subclass question), not itemType()
   ! (which reports plain STATE for a route-handle-wrapper esmf_state,
   ! same as any other state - see the module header). Looks up the
   ! member by whatever name ESMF_StateAdd actually assigned it (there
   ! is no way to choose that name at add time).
   function stateitem_get_route_handle(this, rc) result(route_handle)
      class(GraphStateItem), intent(in) :: this
      integer, optional, intent(out) :: rc
      type(ESMF_RouteHandle) :: route_handle

      integer :: status
      logical :: is_wrapper
      character(:), allocatable :: member_name

      is_wrapper = state_is_route_handle_wrapper(this, rc=status, member_name=member_name)
      _VERIFY(status)
      _ASSERT(is_wrapper, 'GraphStateItem: not playing the route-handle-wrapper role')

      call ESMF_StateGet(this%esmf_state, itemName=member_name, routehandle=route_handle, _RC)

      _RETURN(_SUCCESS)
   end function stateitem_get_route_handle

   function stateitem_field_bundle_members(this) result(members)
      class(GraphStateItem), intent(in) :: this
      type(StateItemMemberMap) :: members

      members = this%field_bundle_members_map
   end function stateitem_field_bundle_members

   function stateitem_state_members(this) result(members)
      class(GraphStateItem), intent(in) :: this
      type(StateItemMemberMap) :: members

      members = this%state_members_map
   end function stateitem_state_members

   subroutine stateitem_add_field_bundle_member(this, name, id, rc)
      class(GraphStateItem), intent(inout) :: this
      character(*), intent(in) :: name
      type(NodeId), intent(in) :: id
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_StateItem_Flag) :: kind_flag

      kind_flag = this%itemType(_RC)
      _ASSERT(kind_flag == ESMF_STATEITEM_FIELDBUNDLE, 'GraphStateItem: field_bundle_members only valid for field-bundle kind')

      call this%field_bundle_members_map%insert(name, id)

      _RETURN(_SUCCESS)
   end subroutine stateitem_add_field_bundle_member

   ! Gated on itemType()==STATE (which of the three components) *and*
   ! variant()/=ROUTEHANDLE (excludes the route-handle-wrapper role,
   ! whose sole member is not a graph-visible, node-identity-
   ! addressable item - REQ-SI-006).
   subroutine stateitem_add_state_member(this, name, id, rc)
      class(GraphStateItem), intent(inout) :: this
      character(*), intent(in) :: name
      type(NodeId), intent(in) :: id
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_StateItem_Flag) :: kind_flag
      type(MAPL_StateItem_Flag) :: variant_flag

      kind_flag = this%itemType(_RC)
      _ASSERT(kind_flag == ESMF_STATEITEM_STATE, 'GraphStateItem: state_members only valid for plain-state kind')
      variant_flag = this%variant(_RC)
      _ASSERT(variant_flag /= MAPL_STATEITEM_ROUTEHANDLE, 'GraphStateItem: state_members not applicable to the route-handle-wrapper role')

      call this%state_members_map%insert(name, id)

      _RETURN(_SUCCESS)
   end subroutine stateitem_add_state_member

   ! Test-only: forces esmf_field and esmf_field_bundle both allocated at
   ! once, bypassing the `set` choke point, so the checked-defect path
   ! in check_invariant()/itemType() can be exercised.
   subroutine stateitem_debug_force_double_allocate(this, field, field_bundle, rc)
      class(GraphStateItem), intent(inout) :: this
      type(ESMF_Field), intent(in) :: field
      type(ESMF_FieldBundle), intent(in) :: field_bundle
      integer, optional, intent(out) :: rc

      if (allocated(this%esmf_field)) deallocate(this%esmf_field)
      if (allocated(this%esmf_field_bundle)) deallocate(this%esmf_field_bundle)
      if (allocated(this%esmf_state)) deallocate(this%esmf_state)
      this%esmf_field = field
      this%esmf_field_bundle = field_bundle

      _RETURN(_SUCCESS)
   end subroutine stateitem_debug_force_double_allocate

end module mapl_GraphStateItem_mod
