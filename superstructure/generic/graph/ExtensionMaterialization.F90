#include "MAPL.h"

!------------------------------------------------------------------------------
! ExtensionMaterialization: creates the real ESMF_Field of a StateItemNode
! the graph owns (an export or framework-created extension node) from that
! node's own descriptor. Graph-layer only: never depends on StateRegistry,
! VariableSpec or OuterMetaComponent, and names no characteristic.
!
! A node's Field goes through the allocation statuses CREATED (empty Field,
! no allocation: create_field_extension) and ALLOCATED, as one Field object.
! materialize_field_extension() completes a CREATED Field in place; it is a
! no-op for any other existing Field (e.g. a reused extension chain).
!
! A node whose descriptor declares a VECTOR or BRACKET gets a FieldBundle of
! that type instead, through the same statuses: create_bundle_extension makes
! a memberless CREATED bundle, and materialization completes it in place and
! marks it ALLOCATED. A bundle for an export the graph owns gets its members
! (with_members); a chain link stays memberless, because the Transform that
! fills it creates the members at compute.
! rebuild_field_extension() instead replaces an existing completed Field;
! an ESMF_State or importer holding the old Field is not updated.
!------------------------------------------------------------------------------
module mapl_ExtensionMaterialization_mod
   use mapl_ComponentGraph_mod, only: ComponentGraph
   use mapl_NodeId_mod, only: NodeId
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_StateItemNode_mod, only: StateItemNode
   use mapl_StateItemDescriptor_mod, only: StateItemDescriptor
   use mapl_StateItemCharacteristic_mod, only: SYNC_PHASE_STRUCTURAL, &
        SYNC_PHASE_ADVERTISED
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_field_api, only: MAPL_FieldEmptyComplete, MAPL_FieldSet
   use mapl_field_api, only: MAPL_FieldGet
   use mapl_field_bundle_api, only: MAPL_FieldBundleCreate, MAPL_FieldBundleSet
   use mapl_field_bundle_api, only: MAPL_FieldBundleGet
   use mapl_StateItem_mod, only: MAPL_STATEITEM_VECTOR, MAPL_STATEITEM_BRACKET
   use mapl_enums_api, only: MAPL_FieldBundleType_Flag
   use mapl_enums_api, only: MAPL_FIELDBUNDLETYPE_VECTOR
   use mapl_enums_api, only: MAPL_FIELDBUNDLETYPE_BRACKET
   use mapl_enums_api, only: MAPL_VectorBasisKind, MAPL_VECTOR_BASIS_KIND_NS
   use mapl_enums_api, only: MAPL_STATEITEM_ALLOCATION_ALLOCATED
   use mapl_enums_api, only: MAPL_STATEITEM_ALLOCATION_CREATED
   use mapl_enums_api, only: MAPL_StateItemAllocation, operator(==)
   use ESMF, only: ESMF_Field, ESMF_FieldEmptyCreate, ESMF_FieldGet
   use ESMF, only: ESMF_FieldBundleAdd, ESMF_Info, ESMF_InfoGetFromHost, &
        ESMF_InfoIsPresent
   use mapl_esmf_info_keys_mod, only: INFO_INTERNAL_NAMESPACE, KEY_ALLOCATION_STATUS
   use ESMF, only: ESMF_FieldBundle, ESMF_STATEITEM_FIELDBUNDLE
   use ESMF, only: ESMF_NamedAliasGet
   use ESMF, only: ESMF_StateItem_Flag, ESMF_STATEITEM_FIELD, operator(==)
   use mapl_FieldUtilities_mod, only: FieldSet
   use ESMF, only: ESMF_KIND_R4, ESMF_MAXSTR
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: materialize_field_extension
   public :: rebuild_field_extension
   public :: create_field_extension
   public :: create_bundle_extension
   public :: field_extension_is_created
   public :: complete_created_field
   public :: describe_created_field
   public :: write_advertised_keys

contains

   ! Gives node_id's StateItemNode an allocated ESMF_Field built from its
   ! descriptor: empty-create, write every keyword (sync_to_info STRUCTURAL;
   ! the geom goes on the empty field), then complete from Info. A node that
   ! declares a VECTOR or BRACKET gets a memberless FieldBundle instead. A
   ! no-op if the payload already is a completed Field or FieldBundle. A
   ! bundle that was only created is completed in place. Callers check the
   ! materialization gate, the item class and descriptor completeness; this
   ! module does not.
   !
   ! Optional `name` makes the field findable by short name once inserted
   ! into an ESMF_State (ESMF_StateGet is by name). `with_members` (default
   ! false) gives a VECTOR or BRACKET bundle its members: an export the graph
   ! owns needs them, a chain link does not.
   subroutine materialize_field_extension(graph, node_id, name, with_members, rc)
      class(ComponentGraph), target, intent(inout) :: graph
      type(NodeId), intent(in) :: node_id
      character(*), optional, intent(in) :: name
      logical, optional, intent(in) :: with_members
      integer, optional, intent(out) :: rc

      integer :: status
      class(GraphNode), pointer :: node
      type(GraphStateItem), pointer :: payload
      type(ESMF_StateItem_Flag) :: kind_flag
      type(ESMF_Field) :: field
      logical :: members

      members = .false.
      if (present(with_members)) members = with_members

      node => graph%get_node(node_id)
      _ASSERT(associated(node), 'ExtensionMaterialization: NodeId not found in graph')

      select type (node)
      class is (StateItemNode)
         payload => node%get_payload_ptr()
         kind_flag = payload%itemType(_RC)
         if (kind_flag == ESMF_STATEITEM_FIELDBUNDLE) then
            ! A completed bundle is left alone (a reused extension chain).
            if (payload_is_created(payload)) then
               call complete_bundle(node, members, _RC)
            end if
            _RETURN(_SUCCESS)
         end if
         if (kind_flag == ESMF_STATEITEM_FIELD) then
            ! Already materialized (a reused extension chain), unless it
            ! was only created: then complete that same Field object.
            if (payload_is_created(payload)) then
               field = payload%get_field(_RC)
               call complete_field(node, field, _RC)
            end if
            _RETURN(_SUCCESS)
         end if

         if (is_bundle_class(node)) then
            call build_bundle_payload(node, name, members, _RC)
         else
            call build_field_payload(node, name, _RC)
         end if
      class default
         _FAIL('ExtensionMaterialization: NodeId does not refer to a StateItemNode')
      end select

      _RETURN(_SUCCESS)
   end subroutine materialize_field_extension

   ! Replaces node_id's existing ESMF_Field payload with a new complete
   ! field built from the (since changed) descriptor, keeping the old
   ! field's name. Unlike materialize_field_extension this does not stop
   ! at an existing Field. A Field already inserted into an ESMF_State, or
   ! aliased by importers, is not replaced there.
   subroutine rebuild_field_extension(graph, node_id, rc)
      class(ComponentGraph), target, intent(in) :: graph
      type(NodeId), intent(in) :: node_id
      integer, optional, intent(out) :: rc

      integer :: status
      class(GraphNode), pointer :: node
      type(GraphStateItem) :: payload
      type(ESMF_Field) :: old_field
      type(ESMF_StateItem_Flag) :: kind_flag
      character(len=ESMF_MAXSTR) :: old_name

      node => graph%get_node(node_id)
      _ASSERT(associated(node), 'rebuild: NodeId not found in graph')

      select type (node)
      class is (StateItemNode)
         payload = node%get_payload()
         kind_flag = payload%itemType(_RC)
         _ASSERT(kind_flag == ESMF_STATEITEM_FIELD, 'rebuild needs a Field')
         ! A merely created Field has no shape yet: completion reads the
         ! (new) descriptor, so there is nothing to rebuild.
         _RETURN_IF(payload_is_created(payload))
         old_field = payload%get_field(_RC)
         call ESMF_FieldGet(old_field, name=old_name, _RC)
         call build_field_payload(node, trim(old_name), _RC)
      class default
         _FAIL('ExtensionMaterialization: NodeId is not a StateItemNode')
      end select

      _RETURN(_SUCCESS)
   end subroutine rebuild_field_extension

   ! Gives node_id's StateItemNode an empty ESMF_Field with allocation
   ! status CREATED: no geometry, no allocation. A no-op if the payload
   ! already is a Field.
   subroutine create_field_extension(graph, node_id, name, rc)
      class(ComponentGraph), target, intent(inout) :: graph
      type(NodeId), intent(in) :: node_id
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc

      integer :: status
      class(GraphNode), pointer :: node
      type(GraphStateItem), pointer :: payload
      type(ESMF_StateItem_Flag) :: kind_flag
      type(ESMF_Field) :: field
      character(:), allocatable :: msg

      node => graph%get_node(node_id)
      msg = 'ExtensionMaterialization: NodeId not found in graph'
      _ASSERT(associated(node), msg)

      select type (node)
      class is (StateItemNode)
         ! Mutate the stored handle in place (gfortran copy hazard).
         payload => node%get_payload_ptr()
         kind_flag = payload%itemType(_RC)
         _RETURN_IF(kind_flag == ESMF_STATEITEM_FIELD)

         field = ESMF_FieldEmptyCreate(name=name, _RC)
         call payload%set(field, _RC)
         call MAPL_FieldSet(field, &
              allocation_status=MAPL_STATEITEM_ALLOCATION_CREATED, _RC)
      class default
         msg = 'ExtensionMaterialization: NodeId is not a StateItemNode'
         _FAIL(msg)
      end select

      _RETURN(_SUCCESS)
   end subroutine create_field_extension

   ! Gives node_id's StateItemNode an empty FieldBundle of its declared class
   ! (VECTOR or BRACKET) with allocation status CREATED and no members, as
   ! legacy creates it. A no-op for any other class or if the payload is
   ! already a Field or a FieldBundle.
   subroutine create_bundle_extension(graph, node_id, name, rc)
      class(ComponentGraph), target, intent(inout) :: graph
      type(NodeId), intent(in) :: node_id
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc

      integer :: status
      class(GraphNode), pointer :: node
      type(GraphStateItem), pointer :: payload
      type(ESMF_StateItem_Flag) :: kind_flag
      character(:), allocatable :: msg

      node => graph%get_node(node_id)
      msg = 'ExtensionMaterialization: NodeId not found in graph'
      _ASSERT(associated(node), msg)

      select type (node)
      class is (StateItemNode)
         payload => node%get_payload_ptr()
         kind_flag = payload%itemType(_RC)
         _RETURN_IF(kind_flag == ESMF_STATEITEM_FIELD)
         _RETURN_IF(kind_flag == ESMF_STATEITEM_FIELDBUNDLE)
         _RETURN_UNLESS(is_bundle_class(node))
         call new_bundle(node, name, .true., _RC)
      class default
         msg = 'ExtensionMaterialization: NodeId is not a StateItemNode'
         _FAIL(msg)
      end select

      _RETURN(_SUCCESS)
   end subroutine create_bundle_extension

   ! True when node_id's payload is a Field or FieldBundle that has only been
   ! created, not yet completed and allocated.
   function field_extension_is_created(graph, node_id) result(is_created)
      class(ComponentGraph), target, intent(in) :: graph
      type(NodeId), intent(in) :: node_id
      logical :: is_created

      class(GraphNode), pointer :: node
      type(GraphStateItem), pointer :: payload

      is_created = .false.
      node => graph%get_node(node_id)
      if (.not. associated(node)) return
      select type (node)
      class is (StateItemNode)
         payload => node%get_payload_ptr()
         is_created = payload_is_created(payload)
      end select
   end function field_extension_is_created

   ! Completes and allocates the node's Field (or bundle, with its members)
   ! if it was only created; any other payload is left alone. Does not check
   ! descriptor completeness.
   subroutine complete_created_field(node, rc)
      class(StateItemNode), target, intent(inout) :: node
      integer, optional, intent(out) :: rc

      integer :: status
      type(GraphStateItem), pointer :: payload
      type(ESMF_Field) :: field
      type(ESMF_StateItem_Flag) :: kind_flag

      payload => node%get_payload_ptr()
      _RETURN_UNLESS(payload_is_created(payload))
      kind_flag = payload%itemType(_RC)
      if (kind_flag == ESMF_STATEITEM_FIELDBUNDLE) then
         call complete_bundle(node, .true., _RC)
         _RETURN(_SUCCESS)
      end if
      field = payload%get_field(_RC)
      call complete_field(node, field, _RC)

      _RETURN(_SUCCESS)
   end subroutine complete_created_field

   ! Writes the node's descriptor onto a merely created Field (the geom goes
   ! on the empty Field, so it becomes GRIDSET) without completing or
   ! allocating it. Any other payload is left alone. Does not check
   ! descriptor completeness.
   subroutine describe_created_field(node, rc)
      class(StateItemNode), target, intent(inout) :: node
      integer, optional, intent(out) :: rc

      integer :: status
      type(GraphStateItem), pointer :: payload
      type(StateItemDescriptor), pointer :: descriptor

      payload => node%get_payload_ptr()
      _RETURN_UNLESS(payload_is_created(payload))
      descriptor => node%get_descriptor()
      call descriptor%sync_to_info(payload, SYNC_PHASE_STRUCTURAL, _RC)

      _RETURN(_SUCCESS)
   end subroutine describe_created_field

   ! Writes only the non-structural keys (units, standard name, long name,
   ! ...) of the node's descriptor onto a merely created Field: what a user
   ! component may read at advertise. No geom, no typekind, no allocation.
   ! Any other payload is left alone.
   subroutine write_advertised_keys(node, rc)
      class(StateItemNode), target, intent(inout) :: node
      integer, optional, intent(out) :: rc

      integer :: status
      type(GraphStateItem), pointer :: payload
      type(StateItemDescriptor), pointer :: descriptor

      payload => node%get_payload_ptr()
      _RETURN_UNLESS(payload_is_created(payload))
      descriptor => node%get_descriptor()
      call descriptor%sync_to_info(payload, SYNC_PHASE_ADVERTISED, _RC)

      _RETURN(_SUCCESS)
   end subroutine write_advertised_keys

   ! Errors are swallowed: a Field or FieldBundle with no allocation status is
   ! not CREATED. The item must also be an original, never a named alias
   ! (alias id 0): legacy writes the same CREATED status on its own items,
   ! and the only legacy item a node ever holds is a MAPL_NamedAlias of it.
   logical function payload_is_created(payload)
      type(GraphStateItem), intent(in) :: payload

      integer :: status
      type(ESMF_StateItem_Flag) :: kind_flag
      type(ESMF_Field) :: field
      type(ESMF_FieldBundle) :: bundle
      type(ESMF_Info) :: info
      type(MAPL_StateItemAllocation) :: alloc_status
      integer :: alias_id

      payload_is_created = .false.
      kind_flag = payload%itemType(rc=status)
      if (status /= 0) return
      if (kind_flag == ESMF_STATEITEM_FIELD) then
         field = payload%get_field(rc=status)
         if (status /= 0) return
         call ESMF_NamedAliasGet(field, id=alias_id, rc=status)
         if (status /= 0 .or. alias_id /= 0) return
         call MAPL_FieldGet(field, allocation_status=alloc_status, rc=status)
         if (status /= 0) return
      else if (kind_flag == ESMF_STATEITEM_FIELDBUNDLE) then
         bundle = payload%get_field_bundle(rc=status)
         if (status /= 0) return
         call ESMF_NamedAliasGet(bundle, id=alias_id, rc=status)
         if (status /= 0 .or. alias_id /= 0) return
         ! A bundle built by hand may carry no status; reading it would
         ! record an error.
         call ESMF_InfoGetFromHost(bundle, info, rc=status)
         if (status /= 0) return
         if (.not. ESMF_InfoIsPresent(info, &
              key=INFO_INTERNAL_NAMESPACE // KEY_ALLOCATION_STATUS)) return
         call MAPL_FieldBundleGet(bundle, allocation_status=alloc_status, rc=status)
         if (status /= 0) return
      else
         return
      end if
      payload_is_created = (alloc_status == MAPL_STATEITEM_ALLOCATION_CREATED)
   end function payload_is_created

   ! Builds a complete field from the node's descriptor and installs it
   ! as the node's payload, replacing whatever payload was there.
   subroutine build_field_payload(node, name, rc)
      class(StateItemNode), target, intent(inout) :: node
      character(*), optional, intent(in) :: name
      integer, optional, intent(out) :: rc

      integer :: status
      type(GraphStateItem), pointer :: payload
      type(ESMF_Field) :: field

      payload => node%get_payload_ptr()
      field = ESMF_FieldEmptyCreate(name=name, _RC)
      call payload%set(field, _RC)
      call complete_field(node, field, _RC)
   end subroutine build_field_payload

   ! The bundle-backed item classes this module can create. The class comes
   ! from the descriptor: a fresh node has no payload to ask.
   logical function is_bundle_class(node)
      class(StateItemNode), target, intent(in) :: node

      type(StateItemDescriptor), pointer :: descriptor
      type(ESMF_StateItem_Flag) :: item_class

      descriptor => node%get_descriptor()
      item_class = descriptor%get_declared_item_type()
      is_bundle_class = (item_class == MAPL_STATEITEM_VECTOR) .or. &
           (item_class == MAPL_STATEITEM_BRACKET)
   end function is_bundle_class

   ! Installs a new FieldBundle of the node's declared class as its payload,
   ! writes the descriptor on it and marks it ALLOCATED. Members only if asked.
   subroutine build_bundle_payload(node, name, with_members, rc)
      class(StateItemNode), target, intent(inout) :: node
      character(*), optional, intent(in) :: name
      logical, intent(in) :: with_members
      integer, optional, intent(out) :: rc

      integer :: status

      call new_bundle(node, name, with_members, _RC)
      call complete_bundle(node, with_members, _RC)

      _RETURN(_SUCCESS)
   end subroutine build_bundle_payload

   ! Installs a new empty FieldBundle of the node's declared class as its
   ! payload. `legacy_attributes` also gives it what legacy create does: the
   ! CREATED status, a vector's basis and a bracket's updated flag. A chain
   ! link skips them: its Transform carries them over from its input.
   subroutine new_bundle(node, name, legacy_attributes, rc)
      class(StateItemNode), target, intent(inout) :: node
      character(*), optional, intent(in) :: name
      logical, intent(in) :: legacy_attributes
      integer, optional, intent(out) :: rc

      integer :: status
      type(StateItemDescriptor), pointer :: descriptor
      type(GraphStateItem), pointer :: payload
      type(ESMF_FieldBundle) :: bundle
      type(MAPL_FieldBundleType_Flag) :: bundle_type
      type(ESMF_StateItem_Flag) :: item_class
      type(MAPL_VectorBasisKind) :: basis_kind
      logical :: has_basis

      descriptor => node%get_descriptor()
      item_class = descriptor%get_declared_item_type()
      if (item_class == MAPL_STATEITEM_VECTOR) then
         bundle_type = MAPL_FIELDBUNDLETYPE_VECTOR
      else
         bundle_type = MAPL_FIELDBUNDLETYPE_BRACKET
      end if

      bundle = MAPL_FieldBundleCreate(name=name, fieldBundleType=bundle_type, _RC)
      payload => node%get_payload_ptr()
      call payload%set(bundle, _RC)
      _RETURN_UNLESS(legacy_attributes)

      call MAPL_FieldBundleSet(bundle, &
           allocation_status=MAPL_STATEITEM_ALLOCATION_CREATED, _RC)
      if (item_class == MAPL_STATEITEM_VECTOR) then
         call descriptor%get_vector_basis_kind(basis_kind, has_basis)
         if (.not. has_basis) basis_kind = MAPL_VECTOR_BASIS_KIND_NS
         call MAPL_FieldBundleSet(bundle, vector_basis_kind=basis_kind, _RC)
      else
         call MAPL_FieldBundleSet(bundle, bracket_updated=.true., _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine new_bundle

   ! Writes the descriptor on the node's bundle, gives it its members if
   ! asked, and marks it ALLOCATED. The descriptor goes first: setting the
   ! geom on a bundle resets it.
   subroutine complete_bundle(node, with_members, rc)
      class(StateItemNode), target, intent(inout) :: node
      logical, intent(in) :: with_members
      integer, optional, intent(out) :: rc

      integer :: status
      type(StateItemDescriptor), pointer :: descriptor
      type(GraphStateItem), pointer :: payload
      type(ESMF_FieldBundle) :: bundle

      descriptor => node%get_descriptor()
      payload => node%get_payload_ptr()
      bundle = payload%get_field_bundle(_RC)
      call descriptor%sync_to_info(payload, SYNC_PHASE_STRUCTURAL, _RC)
      if (with_members) then
         call create_members(descriptor, bundle, _RC)
      end if
      call MAPL_FieldBundleSet(bundle, &
           allocation_status=MAPL_STATEITEM_ALLOCATION_ALLOCATED, _RC)

      _RETURN(_SUCCESS)
   end subroutine complete_bundle

   ! Adds the members of a VECTOR (two, named for the components) or BRACKET
   ! (bracket_size, at most two) to `bundle`, each a complete Field built
   ! from the descriptor as an ordinary export Field is.
   subroutine create_members(descriptor, bundle, rc)
      type(StateItemDescriptor), intent(in) :: descriptor
      type(ESMF_FieldBundle), intent(inout) :: bundle
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i, n_members
      logical :: has_size, has_names
      character(:), allocatable :: name_1, name_2
      type(ESMF_StateItem_Flag) :: item_class
      type(ESMF_Field) :: member
      type(GraphStateItem) :: handle
      real(ESMF_KIND_R4) :: fill_value
      logical :: has_fill_value

      item_class = descriptor%get_declared_item_type()
      has_names = .false.
      if (item_class == MAPL_STATEITEM_VECTOR) then
         n_members = 2
         call descriptor%get_vector_component_names(name_1, name_2, has_names)
      else
         call descriptor%get_bracket_size(n_members, has_size)
         _ASSERT(has_size, 'ExtensionMaterialization: a BRACKET export needs a bracket_size')
         _ASSERT(n_members <= 2, 'ExtensionMaterialization: a BRACKET can only have 2 fields in the bundle')
      end if
      call descriptor%get_fill_value(fill_value, has_fill_value)

      do i = 1, n_members
         member = ESMF_FieldEmptyCreate(_RC)
         call MAPL_FieldSet(member, &
              allocation_status=MAPL_STATEITEM_ALLOCATION_CREATED, _RC)
         call handle%set(member, _RC)
         call descriptor%sync_to_info(handle, SYNC_PHASE_STRUCTURAL, _RC)
         ! After the descriptor: a vector has no standard name entry of its
         ! own that could overwrite the component name.
         if (has_names) then
            if (i == 1) call MAPL_FieldSet(member, standard_name=name_1, _RC)
            if (i == 2) call MAPL_FieldSet(member, standard_name=name_2, _RC)
         end if
         call MAPL_FieldEmptyComplete(member, _RC)
         if (has_fill_value) call FieldSet(member, fill_value, _RC)
         call MAPL_FieldSet(member, &
              allocation_status=MAPL_STATEITEM_ALLOCATION_ALLOCATED, _RC)
         call ESMF_FieldBundleAdd(bundle, [member], multiflag=.true., _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine create_members

   ! Completes and allocates `field`, the node's payload Field, from the
   ! node's descriptor: write every keyword (sync_to_info STRUCTURAL; the
   ! geom goes on the empty field), then complete from Info.
   subroutine complete_field(node, field, rc)
      class(StateItemNode), target, intent(inout) :: node
      type(ESMF_Field), intent(inout) :: field
      integer, optional, intent(out) :: rc

      integer :: status
      type(StateItemDescriptor), pointer :: descriptor
      type(GraphStateItem) :: handle
      real(ESMF_KIND_R4) :: fill_value
      logical :: has_fill_value

      descriptor => node%get_descriptor()
      call handle%set(field, _RC)
      call descriptor%sync_to_info(handle, SYNC_PHASE_STRUCTURAL, _RC)
      call MAPL_FieldEmptyComplete(field, _RC)
      call descriptor%get_fill_value(fill_value, has_fill_value)
      if (has_fill_value) call FieldSet(field, fill_value, _RC)
      call MAPL_FieldSet(field, &
           allocation_status=MAPL_STATEITEM_ALLOCATION_ALLOCATED, _RC)

      _RETURN(_SUCCESS)
   end subroutine complete_field

end module mapl_ExtensionMaterialization_mod
