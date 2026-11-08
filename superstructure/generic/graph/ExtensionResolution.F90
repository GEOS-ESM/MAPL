#include "MAPL.h"

!------------------------------------------------------------------------------
! ExtensionResolution: mismatch detection, extension-chain creation, and
! reuse lookup (REQ-EXT-001/003). Graph-neutral: depends only on
! StateItemCharacteristic/StateItemCharacteristicMap/
! StateItemCharacteristicKind, ComponentGraph/DependencyNetworkId/NodeId, and
! TransformGraphNode/Transform - not on OuterMetaComponent/StateRegistry/
! VariableSpec. GraphBuilder.F90 builds each side's StateItemCharacteristicMap
! from its own VariableSpec data and calls in with the resulting maps.
!
! find_mismatched_characteristics compares only kinds present on BOTH maps; a
! kind declared by only one side is not a mismatch (absence means "not
! applicable to this shape"), which also keeps import_map%at() in
! build_chain/chain_key on keys known to exist.
!
! Chain-building calls each mismatched characteristic's own
! StateItemCharacteristic%build_transform, an ordinary deferred method with
! no provider registry. A characteristic with no real conversion yet reports
! BUILD_TRANSFORM_UNSUPPORTED (e.g. VerticalGridCharacteristic); one whose pair
! can never connect reports BUILD_TRANSFORM_INCOMPATIBLE. Neither raises.
!
! Reuse search (find_or_build_extension_chain) is keyed through
! ComponentGraph%add_resource_index/get_resource_index (REQ-CG-001), not a
! DependencyNetwork%get_successors walk: TransformGraphNode holds its
! Transform in a private component with no accessor, so a structural walk
! cannot recover which characteristic value an existing chain's output
! represents.
!
! Each mismatched characteristic is resolved independently against the
! ORIGINAL export's declared value; a chain link for one characteristic does
! not change what a later link of a different kind is compared against. This
! is narrower than legacy's incremental ExtensionFamily%find_closest_spec
! search and gives the same outcome for any single-characteristic mismatch.
module mapl_ExtensionResolution_mod
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, StateItemCharacteristicMap, &
        StateItemCharacteristicMapIterator, BUILD_TRANSFORM_INCOMPATIBLE, operator(==), operator(/=)
   use mapl_StateItemCharacteristicKind_mod, only: &
        StateItemCharacteristicKind, operator(==)
   use mapl_StateItemDescriptor_mod, only: resolve_characteristics, &
        StateItemDescriptor
   use ESMF, only: ESMF_STATEITEM_FIELD, ESMF_KIND_R4
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_ComponentGraph_mod, only: ComponentGraph
   use mapl_DependencyNetworkId_mod, only: DependencyNetworkId
   use mapl_NodeId_mod, only: NodeId
   use mapl_StateItemNode_mod, only: StateItemNode
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_NodeRevision_mod, only: NodeRevision
   use mapl_TransformGraphNode_mod, only: TransformGraphNode
   use mapl_Transform_mod, only: Transform
   use mapl_TransformBase_mod, only: TransformBase
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: find_mismatched_characteristics
   public :: find_or_build_extension_chain
   public :: set_materialize_extensions
   public :: materialize_extensions_enabled

   character(*), parameter :: INPUT_PORT_NAME = 'source_field'
   character(*), parameter :: OUTPUT_PORT_NAME = 'destination_field'

   ! Gates only whether GraphBuilder.F90 performs the real FieldCreate-based
   ! materialization (mapl_ExtensionMaterialization_mod) for a resolved chain;
   ! chain structure built here (NodeIds, edges, resource-index entries) is
   ! unaffected. Off by default; only tests call set_materialize_extensions.
   logical, save :: materialize_extensions = .false.

contains

   ! Test-only setter for the flag above; no production code calls it.
   subroutine set_materialize_extensions(enabled)
      logical, intent(in) :: enabled

      materialize_extensions = enabled
   end subroutine set_materialize_extensions

   logical function materialize_extensions_enabled() result(enabled)
      enabled = materialize_extensions
   end function materialize_extensions_enabled

   ! REQ-EXT-003/REQ-EXT-001: which characteristic kinds does the export
   ! declare that the import also declares, needing a Transform? Status is
   ! resolved generically (resolve_characteristics). With bare maps there is
   ! no item type and no import to record an adoption on, so a status
   ! conflict or a needed MIRRORED adoption fails; connection paths use
   ! resolve_descriptors instead.
   function find_mismatched_characteristics(export_map, import_map, rc) result(mismatched)
      type(StateItemCharacteristicMap), target, intent(in) :: export_map
      type(StateItemCharacteristicMap), target, intent(in) :: import_map
      integer, optional, intent(out) :: rc
      type(StateItemCharacteristicKind), allocatable :: mismatched(:)

      type(StateItemCharacteristicKind), allocatable :: adopted(:)
      character(:), allocatable :: conflict
      integer :: status

      call resolve_characteristics(export_map, import_map, ESMF_STATEITEM_FIELD, mismatched, adopted, conflict, _RC)
      _ASSERT(conflict == '', 'ExtensionResolution: ' // conflict)
      if (size(adopted) > 0) then
         _FAIL('ExtensionResolution: MIRRORED import needs a descriptor to adopt ' // adopted(1)%to_string())
      end if

      _RETURN(_SUCCESS)
   end function find_mismatched_characteristics

   ! REQ-EXT-001: resolve export_node_id -> (chain of transform steps and
   ! extension items, one per build step) -> final_node_id,
   ! reusing an existing chain for the same export and combined mismatch
   ! signature when one exists. Reports "unsupported" via
   ! unsupported_characteristic (non-empty name, rc still success - an expected
   ! outcome, not an error) the first time a mismatched kind's build_transform
   ! fails; nothing is wired to the import in that case, and any chain nodes
   ! already created for earlier kinds stay in the graph unreferenced. `rc`
   ! reports genuine errors (e.g. graph mutation failure) separately.
   !
   ! A kind whose build_transform reports BUILD_TRANSFORM_INCOMPATIBLE is
   ! reported through `conflict` instead (text naming the kind and both
   ! signatures), leaving unsupported_characteristic empty. A caller that
   ! does not pass `conflict` gets the kind in unsupported_characteristic.
   !
   ! intermediate_node_ids(:) has one entry per build step: entry i is the
   ! NodeId holding the tip after step i, and the last entry equals
   ! final_node_id. A step may cover several kinds (covered_kinds), so there
   ! can be fewer entries than mismatched_kinds. It is zero-size on the reuse
   ! path (the first call already materialized the links) and truncated to the
   ! links actually built on an early unsupported exit (the failed step's
   ! orphaned extension node is excluded).
   subroutine find_or_build_extension_chain(graph, network_id, export_node_id, &
        export_map, import_map, mismatched_kinds, &
        final_node_id, intermediate_node_ids, unsupported_characteristic, rc, conflict)
      class(ComponentGraph), target, intent(inout) :: graph
      type(DependencyNetworkId), intent(in) :: network_id
      type(NodeId), intent(in) :: export_node_id
      type(StateItemCharacteristicMap), target, intent(in) :: export_map
      type(StateItemCharacteristicMap), target, intent(in) :: import_map
      type(StateItemCharacteristicKind), intent(in) :: mismatched_kinds(:)
      type(NodeId), intent(out) :: final_node_id
      type(NodeId), allocatable, intent(out) :: intermediate_node_ids(:)
      character(:), allocatable, intent(out) :: unsupported_characteristic
      integer, optional, intent(out) :: rc
      character(:), allocatable, optional, intent(out) :: conflict

      integer :: status
      character(:), allocatable :: reuse_key
      type(NodeId), pointer :: existing

      unsupported_characteristic = ''
      if (present(conflict)) conflict = ''

      reuse_key = chain_key(export_node_id, mismatched_kinds, import_map)
      existing => graph%get_resource_index(reuse_key)
      if (associated(existing)) then
         final_node_id = existing
         allocate(intermediate_node_ids(0))
         _RETURN(_SUCCESS)
      end if

      call build_chain(graph, network_id, export_node_id, export_map, import_map, &
           mismatched_kinds, final_node_id, intermediate_node_ids, unsupported_characteristic, &
           conflict=conflict, _RC)

      if (unsupported_characteristic /= '') then
         _RETURN(_SUCCESS)
      end if
      if (present(conflict)) then
         if (conflict /= '') then
            _RETURN(_SUCCESS)
         end if
      end if

      call graph%add_resource_index(reuse_key, final_node_id, _RC)

      _RETURN(_SUCCESS)
   end subroutine find_or_build_extension_chain

   function chain_key(export_node_id, mismatched_kinds, import_map) result(key)
      type(NodeId), intent(in) :: export_node_id
      type(StateItemCharacteristicKind), intent(in) :: mismatched_kinds(:)
      type(StateItemCharacteristicMap), target, intent(in) :: import_map
      character(:), allocatable :: key

      integer :: i, status
      class(StateItemCharacteristic), pointer :: goal

      key = 'EXTCHAIN:' // export_node_id%to_string()
      do i = 1, size(mismatched_kinds)
         goal => import_map%at(mismatched_kinds(i), rc=status)
         key = key // ':' // goal%get_signature()
      end do
   end function chain_key

   subroutine build_chain(graph, network_id, export_node_id, export_map, import_map, &
        mismatched_kinds, final_node_id, intermediate_node_ids, unsupported_characteristic, rc, conflict)
      class(ComponentGraph), target, intent(inout) :: graph
      type(DependencyNetworkId), intent(in) :: network_id
      type(NodeId), intent(in) :: export_node_id
      type(StateItemCharacteristicMap), target, intent(in) :: export_map
      type(StateItemCharacteristicMap), target, intent(in) :: import_map
      type(StateItemCharacteristicKind), intent(in) :: mismatched_kinds(:)
      type(NodeId), intent(out) :: final_node_id
      type(NodeId), allocatable, intent(out) :: intermediate_node_ids(:)
      character(:), allocatable, intent(out) :: unsupported_characteristic
      integer, optional, intent(out) :: rc
      character(:), allocatable, optional, intent(out) :: conflict

      integer :: status
      integer :: i
      integer :: num_built
      class(StateItemCharacteristic), pointer :: export_characteristic, import_characteristic
      ! TransformBase-declared, not Transform: build_transform's `transformer`
      ! dummy is TransformBase to avoid a circular module dependency through
      ! PortNameGraphStateItemMap -> GraphStateItem. Narrowed to Transform by the
      ! `select type` below; there is no implicit downcast of a polymorphic
      ! allocatable.
      class(TransformBase), allocatable :: transformer
      type(NodeId) :: current_node_id
      type(NodeId) :: extension_node_id
      type(NodeId) :: transform_node_id
      type(StateItemNode), target :: extension_node
      type(StateItemDescriptor), pointer :: link
      type(StateItemCharacteristicMapIterator) :: iter
      type(StateItemCharacteristicMap), target :: tip, step_tip
      type(StateItemCharacteristicKind), allocatable :: covered(:), done(:)
      logical :: covers_own_kind
      character(:), allocatable :: assertion_message
      type(GraphStateItem) :: empty_payload
      type(NodeRevision) :: empty_revision
      type(TransformGraphNode) :: transform_node
      class(GraphNode), pointer :: export_node
      type(StateItemDescriptor), pointer :: export_descriptor
      real(ESMF_KIND_R4) :: export_fill_value
      logical :: has_fill_value

      unsupported_characteristic = ''
      ! Extension items carry the export's fill_value (never the goal's) so
      ! materialization can fill them.
      has_fill_value = .false.
      export_fill_value = 0.0
      export_node => graph%get_node(export_node_id)
      if (associated(export_node)) then
         select type (export_node)
         class is (StateItemNode)
            export_descriptor => export_node%get_descriptor()
            call export_descriptor%get_fill_value(export_fill_value, &
                 has_fill_value)
         end select
      end if
      if (present(conflict)) conflict = ''
      allocate(intermediate_node_ids(size(mismatched_kinds)))
      allocate(done(0))
      num_built = 0
      current_node_id = export_node_id

      do i = 1, size(mismatched_kinds)
         ! A step that already covered this kind leaves nothing to resolve.
         if (has_kind(done, mismatched_kinds(i))) cycle
         export_characteristic => export_map%at(mismatched_kinds(i), rc=status)
         import_characteristic => import_map%at(mismatched_kinds(i), rc=status)

         ! Tip at this step: the export with goal values for kinds covered
         ! so far.
         call make_tip(export_map, import_map, done, step_tip, _RC)
         covered = export_characteristic%covered_kinds(step_tip, import_map)
         covers_own_kind = has_kind(covered, mismatched_kinds(i))
         assertion_message = 'ExtensionResolution: covered_kinds omits ' // &
              'own kind ' // mismatched_kinds(i)%to_string()
         _ASSERT(covers_own_kind, assertion_message)
         done = [done, covered]

         ! Extension StateItemNode: payload left unallocated ("not yet realized"),
         ! as advertise_one does.
         extension_node_id = graph%next_node_id(_RC)
         extension_node = StateItemNode(extension_node_id, empty_payload, empty_revision)
         ! The link describes the item after this step: the next tip.
         call make_tip(export_map, import_map, done, tip, _RC)
         link => extension_node%get_descriptor()
         iter = tip%ftn_begin()
         do while (iter /= tip%ftn_end())
            call iter%next()
            call link%set_characteristic(iter%first(), iter%second(), _RC)
         end do
         if (has_fill_value) call link%set_fill_value(export_fill_value)
         call graph%register_node(extension_node, _RC)

         call export_characteristic%build_transform_for_item( &
              import_characteristic, step_tip, transformer, status)
         if (status /= 0) then
            if (status == BUILD_TRANSFORM_INCOMPATIBLE .and. present(conflict)) then
               conflict = 'incompatible ' // mismatched_kinds(i)%to_string() // ': export ' // &
                    export_characteristic%get_signature() // ', import ' // import_characteristic%get_signature()
            else
               unsupported_characteristic = mismatched_kinds(i)%to_string()
            end if
            ! Truncate to the links built before this failure; this step's orphaned
            ! extension_node_id (created above, never wired) is excluded.
            intermediate_node_ids = intermediate_node_ids(1:num_built)
            _RETURN(_SUCCESS)
         end if

         transform_node_id = graph%next_node_id(_RC)
         ! Every build_transform success path allocates a genuine Transform
         ! subclass, but TransformGraphNode needs the full Transform interface, so
         ! narrow via select type.
         select type (transformer)
         class is (Transform)
            transform_node = TransformGraphNode(transform_node_id, transformer)
         class default
            _FAIL('ExtensionResolution: build_transform allocated a non-Transform TransformBase - internal inconsistency')
         end select
         call graph%register_node(transform_node, _RC)

         call graph%bind_port(network_id, transform_node_id, INPUT_PORT_NAME, current_node_id, _RC)
         call graph%bind_port(network_id, transform_node_id, OUTPUT_PORT_NAME, extension_node_id, _RC)

         call graph%add_dependency(network_id, current_node_id, transform_node_id, _RC)
         call graph%add_dependency(network_id, transform_node_id, extension_node_id, _RC)

         num_built = num_built + 1
         intermediate_node_ids(num_built) = extension_node_id

         current_node_id = extension_node_id
      end do

      intermediate_node_ids = intermediate_node_ids(1:num_built)
      final_node_id = current_node_id

      _RETURN(_SUCCESS)
   end subroutine build_chain

   logical function has_kind(kinds, kind)
      type(StateItemCharacteristicKind), intent(in) :: kinds(:)
      type(StateItemCharacteristicKind), intent(in) :: kind

      integer :: i

      has_kind = .false.
      do i = 1, size(kinds)
         if (kinds(i) == kind) has_kind = .true.
      end do
   end function has_kind

   ! tip := export_map with the goal's value for every kind in `done`.
   ! Entries are inserted one at a time (no whole-map assignment).
   subroutine make_tip(export_map, goal_map, done, tip, rc)
      type(StateItemCharacteristicMap), target, intent(in) :: export_map
      type(StateItemCharacteristicMap), target, intent(in) :: goal_map
      type(StateItemCharacteristicKind), intent(in) :: done(:)
      type(StateItemCharacteristicMap), target, intent(inout) :: tip
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i
      logical :: in_tip, in_goal
      character(:), allocatable :: assertion_message
      type(StateItemCharacteristicMapIterator) :: iter
      class(StateItemCharacteristic), pointer :: value

      do i = 1, size(done)
         in_tip = export_map%count(done(i)) > 0
         in_goal = goal_map%count(done(i)) > 0
         assertion_message = 'ExtensionResolution: covered kind ' // &
              done(i)%to_string() // ' is missing from the tip or the goal'
         _ASSERT(in_tip .and. in_goal, assertion_message)
      end do

      call tip%clear()
      iter = export_map%ftn_begin()
      do while (iter /= export_map%ftn_end())
         call iter%next()
         if (has_kind(done, iter%first())) then
            value => goal_map%at(iter%first(), _RC)
            call tip%insert(iter%first(), value)
         else
            call tip%insert(iter%first(), iter%second())
         end if
      end do

      _RETURN(_SUCCESS)
   end subroutine make_tip

end module mapl_ExtensionResolution_mod
