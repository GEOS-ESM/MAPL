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
! rebuild_field_extension() instead replaces an existing completed Field;
! an ESMF_State or importer holding the old Field is not updated.
!------------------------------------------------------------------------------
module mapl_ExtensionMaterialization_mod
   use mapl_ComponentGraph_mod, only: ComponentGraph
   use mapl_NodeId_mod, only: NodeId
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_StateItemNode_mod, only: StateItemNode
   use mapl_StateItemDescriptor_mod, only: StateItemDescriptor
   use mapl_StateItemCharacteristic_mod, only: SYNC_PHASE_STRUCTURAL
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_field_api, only: MAPL_FieldEmptyComplete, MAPL_FieldSet
   use mapl_field_api, only: MAPL_FieldGet
   use mapl_enums_api, only: MAPL_STATEITEM_ALLOCATION_ALLOCATED
   use mapl_enums_api, only: MAPL_STATEITEM_ALLOCATION_CREATED
   use mapl_enums_api, only: MAPL_StateItemAllocation, operator(==)
   use ESMF, only: ESMF_Field, ESMF_FieldEmptyCreate, ESMF_FieldGet
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
   public :: field_extension_is_created
   public :: complete_created_field
   public :: describe_created_field

contains

   ! Gives node_id's StateItemNode an allocated ESMF_Field built from its
   ! descriptor: empty-create, write every keyword (sync_to_info STRUCTURAL;
   ! the geom goes on the empty field), then complete from Info. A no-op if
   ! the payload already is a Field. Callers check the materialization gate,
   ! the item class and descriptor completeness; this module does not.
   !
   ! Optional `name` makes the field findable by short name once inserted
   ! into an ESMF_State (ESMF_StateGet is by name).
   subroutine materialize_field_extension(graph, node_id, name, rc)
      class(ComponentGraph), target, intent(inout) :: graph
      type(NodeId), intent(in) :: node_id
      character(*), optional, intent(in) :: name
      integer, optional, intent(out) :: rc

      integer :: status
      class(GraphNode), pointer :: node
      type(GraphStateItem), pointer :: payload
      type(ESMF_StateItem_Flag) :: kind_flag
      type(ESMF_Field) :: field

      node => graph%get_node(node_id)
      _ASSERT(associated(node), 'ExtensionMaterialization: NodeId not found in graph')

      select type (node)
      class is (StateItemNode)
         payload => node%get_payload_ptr()
         kind_flag = payload%itemType(_RC)
         if (kind_flag == ESMF_STATEITEM_FIELD) then
            ! Already materialized (a reused extension chain), unless it
            ! was only created: then complete that same Field object.
            if (payload_is_created(payload)) then
               field = payload%get_field(_RC)
               call complete_field(node, field, _RC)
            end if
            _RETURN(_SUCCESS)
         end if

         call build_field_payload(node, name, _RC)
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

   ! True when node_id's payload is a Field that has only been created, not
   ! yet completed and allocated.
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

   ! Completes and allocates the node's Field if it was only created; any
   ! other payload is left alone. Does not check descriptor completeness.
   subroutine complete_created_field(node, rc)
      class(StateItemNode), target, intent(inout) :: node
      integer, optional, intent(out) :: rc

      integer :: status
      type(GraphStateItem), pointer :: payload
      type(ESMF_Field) :: field

      payload => node%get_payload_ptr()
      _RETURN_UNLESS(payload_is_created(payload))
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

   ! Errors are swallowed: a Field with no allocation status is not CREATED.
   ! The Field must also be an original, never a named alias (alias id 0):
   ! legacy writes the same CREATED status on its own Fields, and the only
   ! legacy Field a node ever holds is a MAPL_NamedAlias of it.
   logical function payload_is_created(payload)
      type(GraphStateItem), intent(in) :: payload

      integer :: status
      type(ESMF_StateItem_Flag) :: kind_flag
      type(ESMF_Field) :: field
      type(MAPL_StateItemAllocation) :: alloc_status
      integer :: alias_id

      payload_is_created = .false.
      kind_flag = payload%itemType(rc=status)
      if (status /= 0) return
      if (.not. (kind_flag == ESMF_STATEITEM_FIELD)) return
      field = payload%get_field(rc=status)
      if (status /= 0) return
      call ESMF_NamedAliasGet(field, id=alias_id, rc=status)
      if (status /= 0 .or. alias_id /= 0) return
      call MAPL_FieldGet(field, allocation_status=alloc_status, rc=status)
      if (status /= 0) return
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
