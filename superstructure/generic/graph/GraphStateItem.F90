#include "MAPL.h"

!------------------------------------------------------------------------------
! GraphStateItem: concrete, non-polymorphic node payload type
! (REQ-SI-001/002). Three allocatable ESMF handle components - a field, a
! field bundle, and a state - of which at most one may be allocated at any
! time (REQ-SI-004). A route handle is represented by allocating the state
! component in a persistent wrapper role holding exactly one ESMF_RouteHandle
! member (REQ-SI-002a).
!
! Two-tier classification: itemType() is the native ESMF classification
! (FIELD/FIELDBUNDLE/STATE/NOTFOUND: purely which component is allocated, for
! retrieval via get_field()/get_field_bundle()/get_state()); variant() is the
! extensible, Info-backed MAPL_StateItem_Flag (REQ-SI-002b/002c) saying which
! role the instance plays, including the route-handle wrapper as a refinement
! of STATE, like MAPL_STATEITEM_VERTICALGRID. All allocation goes through the
! single generic `set`, which enforces the at-most-one-allocated invariant and
! the membership-map gating (REQ-SI-006); raw component access is private.
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
                                      MAPL_STATEITEM_NOTFOUND, MAPL_STATEITEM_GEOM, &
                                      MAPL_STATEITEM_INVALID, &
                                      operator(==), operator(/=)
   use mapl_field_api, only: MAPL_FieldGet
   use mapl_field_bundle_api, only: MAPL_FieldBundleGet
   use mapl_state_api, only: MAPL_StateGet
   use mapl_NodeId_mod, only: NodeId
   use mapl_StateItemMemberMap_mod, only: StateItemMemberMap, StateItemMemberMapIterator
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, StateItemCharacteristicMap
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind, operator(==), &
                                                    PHYSICAL_UNITS_CHARACTERISTIC_KIND, &
                                                    TYPE_KIND_CHARACTERISTIC_KIND, &
                                                    GEOMETRY_CHARACTERISTIC_KIND
   use mapl_UngriddedDims_mod, only: UngriddedDims
   use mapl_VerticalStaggerLoc_mod, only: VerticalStaggerLoc
   use mapl_VerticalGrid_mod, only: VerticalGrid
   use mapl_RestartModes_mod, only: RestartMode
   use ESMF, only: ESMF_Geom
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: GraphStateItem
   ! operator(==)/operator(/=) are re-exported because that is how Fortran merges
   ! a generic across module boundaries, and consumers comparing a
   ! MAPL_StateItem_Flag/ESMF_StateItem_Flag reached through this module need
   ! the merged generic. Do not re-export other use-associated names: Intel
   ! rejects it when a client also reaches the entity through its defining
   ! module (icc/ifx error #6405).
   public :: operator(==)
   public :: operator(/=)

   type :: GraphStateItem
      private
      type(ESMF_Field), allocatable :: esmf_field
      type(ESMF_FieldBundle), allocatable :: esmf_field_bundle
      type(ESMF_State), allocatable :: esmf_state
      type(StateItemMemberMap) :: field_bundle_members_map
      type(StateItemMemberMap) :: state_members_map
      ! REQ-CHAR-007/008: sparse map, keyed by StateItemCharacteristicKind, of
      ! this item's established/deferred mismatch-relevant characteristics.
      ! Independent of which handle component is allocated: clear_components()
      ! does NOT reset it, since e.g. declared units are not tied to the ESMF
      ! component currently backing the item.
      type(StateItemCharacteristicMap) :: characteristics
      ! Materialization inputs, populated by advertise_one alongside
      ! characteristics and read directly off the export's node by
      ! try_materialize_field_extension/materialize_extension_chain. Same
      ! "independent of the handle component" contract: not reset by
      ! clear_components(). ungridded_dims is always present (like
      ! VariableSpec%ungridded_dims); vertical_stagger is allocatable (a 2D field
      ! has none).
      type(UngriddedDims) :: ungridded_dims
      type(VerticalStaggerLoc), allocatable :: vertical_stagger
      ! The originating VariableSpec's per-item EXPLICIT overrides for
      ! geom/vertical_grid, and its declared classification (var_spec%itemType),
      ! set once by advertise_one so that try_materialize_field_extension/
      ! materialize_extension_chain and resolve_export_geom/resolve_export_vgrid
      ! need no VariableSpec reference. The owning component's component-wide
      ! defaults are not stored here: they are read live via
      ! OuterMetaComponent%has_geom()/get_geom()/get_vertical_grid()
      ! (GraphBuilder_PortsAndProxies_smod.F90) because GEOM_B runs after
      ! ADVERTISE.
      type(ESMF_Geom), allocatable :: declared_geom
      class(VerticalGrid), allocatable :: declared_vertical_grid
      type(ESMF_StateItem_Flag) :: declared_item_type = ESMF_STATEITEM_FIELD
      ! The VariableSpec's declared restart_mode, captured at advertise time like
      ! declared_geom above. populate_one (GraphBuilder_StatePopulation_smod.F90)
      ! reads it back when creating the MAPL_NamedAlias so RestartHandler sees
      ! correct metadata for a graph-materialized field. Allocatable, no default:
      ! absent means "not explicitly declared", and RestartHandler's
      ! RESTART_REQUIRED default applies.
      type(RestartMode), allocatable :: declared_restart_mode
      ! The VariableSpec's expression string, captured at advertise time like
      ! declared_restart_mode. Allocatable, no default: absent for every
      ! non-EXPRESSION item. Read by graphbuilder_run_expression_hook; never
      ! compared for mismatch (there is no ExpressionCharacteristic).
      character(:), allocatable :: declared_expression
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
      procedure :: field_bundle_members_ptr => stateitem_field_bundle_members_ptr
      procedure :: state_members => stateitem_state_members
      procedure :: state_members_ptr => stateitem_state_members_ptr
      procedure :: add_field_bundle_member => stateitem_add_field_bundle_member
      procedure :: add_state_member => stateitem_add_state_member
      ! -- characteristics (REQ-CHAR-007/008/009/010) -----------------------
      procedure :: has_characteristic => stateitem_has_characteristic
      procedure :: get_characteristic => stateitem_get_characteristic
      procedure :: set_characteristic => stateitem_set_characteristic
      ! Copy of the full characteristics map (REQ-CHAR-009: enumerable so
      ! detection can iterate every entry through the common
      ! StateItemCharacteristic interface); returns a copy like
      ! field_bundle_members()/state_members().
      procedure :: get_characteristics => stateitem_get_characteristics
      procedure :: ordering => stateitem_ordering
      ! -- materialization inputs --
      procedure :: get_ungridded_dims => stateitem_get_ungridded_dims
      procedure :: set_ungridded_dims => stateitem_set_ungridded_dims
      procedure :: get_vertical_stagger => stateitem_get_vertical_stagger
      procedure :: set_vertical_stagger => stateitem_set_vertical_stagger
      ! -- declared geom/vgrid/item-class --
      procedure :: get_declared_geom => stateitem_get_declared_geom
      procedure :: set_declared_geom => stateitem_set_declared_geom
      procedure :: get_declared_vertical_grid => stateitem_get_declared_vertical_grid
      procedure :: set_declared_vertical_grid => stateitem_set_declared_vertical_grid
      procedure :: get_declared_item_type => stateitem_get_declared_item_type
      procedure :: set_declared_item_type => stateitem_set_declared_item_type
      procedure :: get_declared_restart_mode => stateitem_get_declared_restart_mode
      procedure :: set_declared_restart_mode => stateitem_set_declared_restart_mode
      procedure :: get_declared_expression => stateitem_get_declared_expression
      procedure :: set_declared_expression => stateitem_set_declared_expression
      ! Test-only backdoor: deliberately violates the at-most-one-allocated
      ! invariant, bypassing the `set` choke point, so the checked-defect path
      ! (REQ-SI-004) can be exercised. Production code MUST NOT call this.
      procedure :: debug_force_double_allocate_for_test => stateitem_debug_force_double_allocate
   end type GraphStateItem

contains

   ! -- classification -------------------------------------------------

   ! Purely "which of the three components is allocated"; the route-handle
   ! wrapper distinction is variant()'s job (a role question, not which ESMF
   ! type to retrieve).
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

   ! The vanilla default per native kind when no finer-grained variant tag has
   ! been attached; otherwise the tagged refinement (GEOM, VECTOR, BRACKET,
   ! VECTORBRACKET, VERTICALGRID) read back through the MAPL_*Get accessors.
   ! The route-handle wrapper (MAPL_STATEITEM_ROUTEHANDLE, a refinement of
   ! STATE) is reported here, never queried from Info, since it is a role
   ! question like "is this state a vertical grid".
   function stateitem_variant(this, rc) result(variant_flag)
      class(GraphStateItem), intent(in) :: this
      integer, optional, intent(out) :: rc
      type(MAPL_StateItem_Flag) :: variant_flag

      integer :: status
      logical :: is_wrapper
      type(ESMF_StateItem_Flag) :: kind_flag

      kind_flag = this%itemType(_RC)

      if (kind_flag == ESMF_STATEITEM_FIELD) then
         call MAPL_FieldGet(this%esmf_field, state_item_variant=variant_flag, _RC)
         if (variant_flag == MAPL_STATEITEM_INVALID) variant_flag = MAPL_STATEITEM_FIELD
         _RETURN(_SUCCESS)
      end if
      if (kind_flag == ESMF_STATEITEM_FIELDBUNDLE) then
         call MAPL_FieldBundleGet(this%esmf_field_bundle, state_item_variant=variant_flag, _RC)
         if (variant_flag == MAPL_STATEITEM_INVALID) variant_flag = MAPL_STATEITEM_FIELDBUNDLE
         _RETURN(_SUCCESS)
      end if
      if (kind_flag == ESMF_STATEITEM_STATE) then
         is_wrapper = state_is_route_handle_wrapper(this, _RC)
         if (is_wrapper) then
            variant_flag = MAPL_STATEITEM_ROUTEHANDLE
            _RETURN(_SUCCESS)
         end if
         call MAPL_StateGet(this%esmf_state, state_item_variant=variant_flag, _RC)
         if (variant_flag == MAPL_STATEITEM_INVALID) variant_flag = MAPL_STATEITEM_STATE
         _RETURN(_SUCCESS)
      end if
      variant_flag = MAPL_STATEITEM_NOTFOUND

      _RETURN(_SUCCESS)
   end function stateitem_variant

   ! Inspects the sole member of an allocated esmf_state via ESMF_StateGet and
   ! reports whether it is a route handle - the one place this module consults
   ! ESMF's native classification below the itemType() tier. If so, also
   ! returns the member's item name; ESMF_StateAdd assigns it and it cannot be
   ! chosen, so callers must use the returned name.
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

   ! Convenience overload of `set` for the route-handle-wrapper role: builds a
   ! small ESMF_State holding exactly one ESMF_RouteHandle and installs it via
   ! set_state, keeping creation behind the same choke point (REQ-SI-002a).
   ! Renewal should replace the member on the existing wrapper state via
   ! ESMF_State calls on get_state()'s result, not call this again.
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

   ! Resolves the route handle out of the wrapper's sole member rather than
   ! exposing a bare stored component (REQ-SI-002a). Gated on variant(), not
   ! itemType() (which reports plain STATE for a wrapper; see the module
   ! header). Looks the member up by the name ESMF_StateAdd assigned.
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

   ! Pointer accessor into the stored member map, deliberately not an
   ! intrinsic assignment like field_bundle_members(): copying a populated
   ! StateItemMemberMap (gFTL2 defined-assignment deep_copy) segfaults
   ! nondeterministically under gfortran, not under NAG. Callers that only read
   ! the map should prefer this. `this` must be a target.
   function stateitem_field_bundle_members_ptr(this) result(members)
      class(GraphStateItem), target, intent(in) :: this
      type(StateItemMemberMap), pointer :: members

      members => this%field_bundle_members_map
   end function stateitem_field_bundle_members_ptr

   function stateitem_state_members(this) result(members)
      class(GraphStateItem), intent(in) :: this
      type(StateItemMemberMap) :: members

      members = this%state_members_map
   end function stateitem_state_members

   ! Same pointer-accessor fix as field_bundle_members_ptr; this is the one
   ! implicated in the crash (state_members() on a composite/multi-member item).
   function stateitem_state_members_ptr(this) result(members)
      class(GraphStateItem), target, intent(in) :: this
      type(StateItemMemberMap), pointer :: members

      members => this%state_members_map
   end function stateitem_state_members_ptr

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

   ! Gated on itemType()==STATE and variant()/=ROUTEHANDLE (the wrapper's sole
   ! member is not a graph-visible, node-identity-addressable item - REQ-SI-006).
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

   ! -- characteristics (REQ-CHAR-007/008/010) --

   logical function stateitem_has_characteristic(this, kind) result(has)
      class(GraphStateItem), intent(in) :: this
      type(StateItemCharacteristicKind), intent(in) :: kind

      has = this%characteristics%count(kind) > 0
   end function stateitem_has_characteristic

   ! REQ-CHAR-008: an absent key means "never established" and fails
   ! explicitly rather than returning an INVALID-status placeholder. A
   ! subroutine with an intent(out) allocatable dummy, not a function returning
   ! a polymorphic allocatable (as Characteristic%build_transform does): an
   ! unallocated polymorphic function result on the failure path is not
   ! reliably safe for the caller to assign.
   subroutine stateitem_get_characteristic(this, kind, characteristic, rc)
      class(GraphStateItem), target, intent(in) :: this
      type(StateItemCharacteristicKind), intent(in) :: kind
      class(StateItemCharacteristic), allocatable, intent(out) :: characteristic
      integer, optional, intent(out) :: rc

      class(StateItemCharacteristic), pointer :: found
      integer :: status

      found => this%characteristics%at(kind, rc=status)
      _ASSERT(status == _SUCCESS, 'GraphStateItem: get_characteristic - no characteristic established for this kind')
      allocate(characteristic, source=found)

      _RETURN(_SUCCESS)
   end subroutine stateitem_get_characteristic

   ! Inserts or replaces the entry under `kind` (REQ-CHAR-007); the one
   ! explicit-replace operation, no other operation removes or replaces an entry
   ! as a side effect.
   !
   ! REQ-CHAR-006/009 invariant: characteristic%get_kind() MUST equal `kind`.
   ! find_mismatched_state_item_characteristics pairs two items' entries by the
   ! same key and assumes both dynamic types match, so each needs_extension_for
   ! `goal` is the same concrete type as `this` by construction. A mismatch
   ! (e.g. a GeometryCharacteristic under PHYSICAL_UNITS_CHARACTERISTIC_KIND) is
   ! caught here, where it would be introduced.
   subroutine stateitem_set_characteristic(this, kind, characteristic, rc)
      class(GraphStateItem), intent(inout) :: this
      type(StateItemCharacteristicKind), intent(in) :: kind
      class(StateItemCharacteristic), intent(in) :: characteristic
      integer, optional, intent(out) :: rc

      integer :: n_erased

      _ASSERT(characteristic%get_kind() == kind, 'GraphStateItem: set_characteristic - characteristic%get_kind() does not match kind')

      n_erased = int(this%characteristics%erase(kind))
      call this%characteristics%insert(kind, characteristic)

      _RETURN(_SUCCESS)
   end subroutine stateitem_set_characteristic

   function stateitem_get_characteristics(this) result(characteristics)
      class(GraphStateItem), intent(in) :: this
      type(StateItemCharacteristicMap) :: characteristics

      characteristics = this%characteristics
   end function stateitem_get_characteristics

   ! -- materialization inputs --

   function stateitem_get_ungridded_dims(this) result(ungridded_dims)
      class(GraphStateItem), intent(in) :: this
      type(UngriddedDims) :: ungridded_dims

      ungridded_dims = this%ungridded_dims
   end function stateitem_get_ungridded_dims

   subroutine stateitem_set_ungridded_dims(this, ungridded_dims)
      class(GraphStateItem), intent(inout) :: this
      type(UngriddedDims), intent(in) :: ungridded_dims

      this%ungridded_dims = ungridded_dims
   end subroutine stateitem_set_ungridded_dims

   ! A subroutine with an intent(out) allocatable dummy, as for
   ! get_characteristic: a direct `x = item%get_vertical_stagger()` from an
   ! unallocated allocatable function result crashed with a NULL dereference
   ! under gfortran (NAG tolerated it); the standard does not guarantee it is
   ! safe.
   subroutine stateitem_get_vertical_stagger(this, vertical_stagger)
      class(GraphStateItem), intent(in) :: this
      type(VerticalStaggerLoc), allocatable, intent(out) :: vertical_stagger

      if (allocated(this%vertical_stagger)) vertical_stagger = this%vertical_stagger
   end subroutine stateitem_get_vertical_stagger

   ! Explicit deallocate+allocate(..., source=...) instead of `=` into an
   ! allocatable component: avoids the gfortran realloc-on-assignment memory
   ! corruption also seen in StateItemNode%short_name.
   subroutine stateitem_set_vertical_stagger(this, vertical_stagger)
      class(GraphStateItem), intent(inout) :: this
      type(VerticalStaggerLoc), intent(in) :: vertical_stagger

      if (allocated(this%vertical_stagger)) deallocate(this%vertical_stagger)
      allocate(this%vertical_stagger, source=vertical_stagger)
   end subroutine stateitem_set_vertical_stagger

   ! -- declared geom/vgrid/item-class --

   ! Subroutine with an intent(out) allocatable dummy, as for
   ! get_vertical_stagger.
   subroutine stateitem_get_declared_geom(this, geom)
      class(GraphStateItem), intent(in) :: this
      type(ESMF_Geom), allocatable, intent(out) :: geom

      if (allocated(this%declared_geom)) geom = this%declared_geom
   end subroutine stateitem_get_declared_geom

   ! Explicit deallocate+allocate(..., source=...), as for
   ! set_vertical_stagger.
   subroutine stateitem_set_declared_geom(this, geom)
      class(GraphStateItem), intent(inout) :: this
      type(ESMF_Geom), intent(in) :: geom

      if (allocated(this%declared_geom)) deallocate(this%declared_geom)
      allocate(this%declared_geom, source=geom)
   end subroutine stateitem_set_declared_geom

   ! Subroutine with an intent(out) allocatable dummy, as for
   ! get_vertical_stagger.
   subroutine stateitem_get_declared_vertical_grid(this, vertical_grid)
      class(GraphStateItem), intent(in) :: this
      class(VerticalGrid), allocatable, intent(out) :: vertical_grid

      if (allocated(this%declared_vertical_grid)) vertical_grid = this%declared_vertical_grid
   end subroutine stateitem_get_declared_vertical_grid

   ! Explicit deallocate+allocate(..., source=...), as for
   ! set_vertical_stagger. For a polymorphic component this also avoids relying
   ! on the compiler's intrinsic-assignment dynamic-type dispatch for a CLASS
   ! allocatable.
   subroutine stateitem_set_declared_vertical_grid(this, vertical_grid)
      class(GraphStateItem), intent(inout) :: this
      class(VerticalGrid), intent(in) :: vertical_grid

      if (allocated(this%declared_vertical_grid)) deallocate(this%declared_vertical_grid)
      allocate(this%declared_vertical_grid, source=vertical_grid)
   end subroutine stateitem_set_declared_vertical_grid

   function stateitem_get_declared_item_type(this) result(item_type)
      class(GraphStateItem), intent(in) :: this
      type(ESMF_StateItem_Flag) :: item_type

      item_type = this%declared_item_type
   end function stateitem_get_declared_item_type

   subroutine stateitem_set_declared_item_type(this, item_type)
      class(GraphStateItem), intent(inout) :: this
      type(ESMF_StateItem_Flag), intent(in) :: item_type

      this%declared_item_type = item_type
   end subroutine stateitem_set_declared_item_type

   ! Subroutine with an intent(out) allocatable dummy, as for
   ! get_vertical_stagger.
   subroutine stateitem_get_declared_restart_mode(this, restart_mode)
      class(GraphStateItem), intent(in) :: this
      type(RestartMode), allocatable, intent(out) :: restart_mode

      if (allocated(this%declared_restart_mode)) restart_mode = this%declared_restart_mode
   end subroutine stateitem_get_declared_restart_mode

   ! Explicit deallocate+allocate(..., source=...), as for set_declared_geom.
   subroutine stateitem_set_declared_restart_mode(this, restart_mode)
      class(GraphStateItem), intent(inout) :: this
      type(RestartMode), intent(in) :: restart_mode

      if (allocated(this%declared_restart_mode)) deallocate(this%declared_restart_mode)
      allocate(this%declared_restart_mode, source=restart_mode)
   end subroutine stateitem_set_declared_restart_mode

   ! Subroutine with an intent(out) allocatable dummy, as for
   ! get_declared_restart_mode.
   subroutine stateitem_get_declared_expression(this, expression)
      class(GraphStateItem), intent(in) :: this
      character(:), allocatable, intent(out) :: expression

      if (allocated(this%declared_expression)) expression = this%declared_expression
   end subroutine stateitem_get_declared_expression

   ! Explicit deallocate+allocate(..., source=...), as for
   ! set_declared_restart_mode.
   subroutine stateitem_set_declared_expression(this, expression)
      class(GraphStateItem), intent(inout) :: this
      character(*), intent(in) :: expression

      if (allocated(this%declared_expression)) deallocate(this%declared_expression)
      allocate(this%declared_expression, source=expression)
   end subroutine stateitem_set_declared_expression

   ! REQ-CHAR-010/011: delegates to the static per-variant table
   ! characteristic_ordering_table (below). `mismatched_kinds` is the set of
   ! kinds a detection algorithm found mismatched; this reorders it (never adds
   ! or removes) according to this item's variant. A variant with no table
   ! entry keeps the input order.
   function stateitem_ordering(this, mismatched_kinds, rc) result(order)
      class(GraphStateItem), intent(in) :: this
      type(StateItemCharacteristicKind), intent(in) :: mismatched_kinds(:)
      integer, optional, intent(out) :: rc
      type(StateItemCharacteristicKind), allocatable :: order(:)

      integer :: status
      type(MAPL_StateItem_Flag) :: variant_flag
      type(StateItemCharacteristicKind), allocatable :: canonical(:)
      integer :: i, j, n

      variant_flag = this%variant(_RC)
      canonical = characteristic_ordering_table(variant_flag)

      allocate(order(0))
      if (size(canonical) == 0) then
         ! No table entry for this variant: keep the input order.
         order = mismatched_kinds
         _RETURN(_SUCCESS)
      end if

      ! Canonical-first: every mismatched kind the table orders, in table order.
      do i = 1, size(canonical)
         do j = 1, size(mismatched_kinds)
            if (mismatched_kinds(j) == canonical(i)) then
               order = [order, mismatched_kinds(j)]
               exit
            end if
         end do
      end do

      ! Then any mismatched kind the table has no opinion on, in its original
      ! relative order, so tables need not be exhaustive (REQ-CHAR-001).
      n = size(mismatched_kinds)
      do j = 1, n
         if (.not. kind_in_list(mismatched_kinds(j), canonical)) then
            order = [order, mismatched_kinds(j)]
         end if
      end do

      _RETURN(_SUCCESS)
   end function stateitem_ordering

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

   ! -- characteristic-ordering strategy table --

   logical function kind_in_list(k, list) result(found)
      type(StateItemCharacteristicKind), intent(in) :: k
      type(StateItemCharacteristicKind), intent(in) :: list(:)

      integer :: i

      found = .false.
      do i = 1, size(list)
         if (list(i) == k) then
            found = .true.
            return
         end if
      end do
   end function kind_in_list

   ! Static per-variant ordering table. Illustrative, not validated against an
   ! end-to-end comparison use case; kept simple and easy to extend. A variant
   ! with no entry (`case default`) returns a zero-size array, which
   ! stateitem_ordering() treats as "no opinion, use input order".
   function characteristic_ordering_table(variant_flag) result(order)
      type(MAPL_StateItem_Flag), intent(in) :: variant_flag
      type(StateItemCharacteristicKind), allocatable :: order(:)

      if (variant_flag == MAPL_STATEITEM_FIELD) then
         ! Ordinary field: reconcile units first (cheap, element-wise), then
         ! precision, then geometry last (regrid is the most expensive step and is
         ! best done on already-corrected data).
         order = [PHYSICAL_UNITS_CHARACTERISTIC_KIND, TYPE_KIND_CHARACTERISTIC_KIND, &
                  GEOMETRY_CHARACTERISTIC_KIND]
      else if (variant_flag == MAPL_STATEITEM_GEOM) then
         ! A geometry-tagged field: geometry identity is its defining
         ! characteristic, so reconcile it before unit/precision adjustments that
         ! would otherwise operate on wrong-shape storage.
         order = [GEOMETRY_CHARACTERISTIC_KIND, PHYSICAL_UNITS_CHARACTERISTIC_KIND, &
                  TYPE_KIND_CHARACTERISTIC_KIND]
      else
         allocate(order(0))
      end if
   end function characteristic_ordering_table

end module mapl_GraphStateItem_mod
