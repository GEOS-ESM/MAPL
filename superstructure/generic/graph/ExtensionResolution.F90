#include "MAPL.h"

!------------------------------------------------------------------------------
! ExtensionResolution: mismatch detection, extension-chain creation, and
! reuse lookup for graph/extension-reuse (docs/graph/spec/09-extension-
! reuse.md REQ-EXT-001/002/003/005; extension-reuse change design.md
! Decisions). Graph-neutral: depends only on Characteristic/
! CharacteristicMap/CharacteristicId, ComponentGraph/DependencyNetworkId/
! NodeId, and TransformGraphNode/Transform - not on OuterMetaComponent/
! StateRegistry/VariableSpec. GraphBuilder.F90 builds each side's
! CharacteristicMap from its own VariableSpec data and calls into this
! module with the resulting maps.
!
! Chain-building calls each mismatched characteristic's own
! Characteristic%build_transform - an ordinary deferred method (mirrors
! StateItemAspect%make_transform), not a separate provider registry.
! "Unregistered characteristic fails loudly" (spec) falls directly out
! of a subclass's own build_transform failing explicitly (rc /= 0) when
! it has no real conversion implemented (e.g.
! VerticalGridCharacteristic) - there is no registry lookup to "miss."
!
! Reuse search (find_or_build_extension_chain): per design.md Decisions
! ("Reuse search uses ComponentGraph's resource index... corrected
! mid-implementation"), keyed via ComponentGraph%add_resource_index/
! get_resource_index (REQ-CG-001) - the same mechanism 3b already
! established for item_key/proxy_key - rather than a live
! DependencyNetwork%get_successors walk: TransformGraphNode holds its
! bound Transform in a private component with no accessor, so a
! structural walk cannot recover which characteristic value an existing
! chain's output represents.
!
! Scope simplification (this change only): each mismatched characteristic
! is resolved independently against the ORIGINAL export's own declared
! value for that characteristic - a chain link for one characteristic
! does not evolve what a later, different-kind link is compared against.
! This differs from legacy's fully general incremental
! ExtensionFamily%find_closest_spec search, but is sufficient for this
! change's scope (units the only real provider) and does not change the
! required equivalence check's outcome for any single-characteristic
! mismatch.
!------------------------------------------------------------------------------
module mapl_ExtensionResolution_mod
   use mapl_Characteristic_mod, only: Characteristic, CharacteristicMap, CharacteristicMapIterator, &
        operator(==), operator(/=)
   use mapl_CharacteristicId_mod, only: CharacteristicId
   use mapl_ComponentGraph_mod, only: ComponentGraph
   use mapl_DependencyNetworkId_mod, only: DependencyNetworkId
   use mapl_NodeId_mod, only: NodeId
   use mapl_StateItemNode_mod, only: StateItemNode
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_NodeRevision_mod, only: NodeRevision
   use mapl_TransformGraphNode_mod, only: TransformGraphNode, Transform
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: find_mismatched_characteristics
   public :: find_or_build_extension_chain
   public :: set_materialize_extensions
   public :: materialize_extensions_enabled

   character(*), parameter :: INPUT_PORT_NAME = 'source_field'
   character(*), parameter :: OUTPUT_PORT_NAME = 'destination_field'

   ! extension-registry-visibility change (design.md Decisions, "Gate
   ! real materialization behind a simple, global, default-off, internal
   ! switch"): guards only whether GraphBuilder.F90 performs the real
   ! FieldCreate-based materialization step (mapl_ExtensionMaterialization_mod)
   ! for a resolved extension chain - chain *structure* built by this
   ! module (NodeIds, edges, resource-index entries, above) is unaffected
   ! and runs unconditionally regardless of this flag, exactly as 3c left
   ! it. Off (.false.) is the state of every real production run unless a
   ! test explicitly calls set_materialize_extensions(.true.) - no
   ! production initialization code does.
   logical, save :: materialize_extensions = .false.

contains

   ! Test-only (and future equivalence-fixture-only) setter - see the
   ! module-level flag comment above. No production code calls this.
   subroutine set_materialize_extensions(enabled)
      logical, intent(in) :: enabled

      materialize_extensions = enabled
   end subroutine set_materialize_extensions

   logical function materialize_extensions_enabled() result(enabled)
      enabled = materialize_extensions
   end function materialize_extensions_enabled

   ! REQ-EXT-003/REQ-EXT-001: which characteristic kinds does the export
   ! declare that the import also declares, with different values? A
   ! kind declared by only one side is not a "mismatch" by this entry
   ! point's contract - GraphBuilder's own advertised-item checks (item
   ! existence, etc.) are a separate concern.
   function find_mismatched_characteristics(export_map, import_map) result(mismatched)
      type(CharacteristicMap), target, intent(in) :: export_map
      type(CharacteristicMap), target, intent(in) :: import_map
      type(CharacteristicId), allocatable :: mismatched(:)

      type(CharacteristicMapIterator) :: iter
      type(CharacteristicId) :: kind_id
      class(Characteristic), pointer :: export_characteristic, import_characteristic
      integer :: status

      allocate(mismatched(0))

      associate (e => export_map%ftn_end())
         iter = export_map%ftn_begin()
         do while (iter /= e)
            call iter%next()
            kind_id = iter%first()
            if (import_map%count(kind_id) == 0) cycle

            export_characteristic => export_map%at(kind_id, rc=status)
            import_characteristic => import_map%at(kind_id, rc=status)
            if (export_characteristic%needs_extension_for(import_characteristic)) then
               mismatched = [mismatched, kind_id]
            end if
         end do
      end associate
   end function find_mismatched_characteristics

   ! REQ-EXT-001/002/005: resolve export_node_id -> (chain of transform
   ! steps and extension items, one per entry in mismatched_kinds) ->
   ! final_node_id, reusing an existing chain for the same export + same
   ! combined mismatch signature when one already exists. Reports
   ! "unsupported" explicitly (unsupported_characteristic set to a
   ! non-empty name, rc still a success code - this is a structured, expected
   ! outcome for the caller to check, not an exceptional error) the
   ! first time a mismatched kind's own Characteristic%build_transform
   ! fails - no wiring to the import is done by this routine in that
   ! case (any chain nodes already created for earlier, supported kinds
   ! remain in the graph, simply unreferenced by any import - acceptable,
   ! not incorrect). `rc` reports a genuine error (e.g. graph mutation
   ! failure) separately from this "unsupported" report.
   subroutine find_or_build_extension_chain(graph, network_id, export_node_id, &
        export_map, import_map, mismatched_kinds, &
        final_node_id, unsupported_characteristic, rc)
      class(ComponentGraph), target, intent(inout) :: graph
      type(DependencyNetworkId), intent(in) :: network_id
      type(NodeId), intent(in) :: export_node_id
      type(CharacteristicMap), target, intent(in) :: export_map
      type(CharacteristicMap), target, intent(in) :: import_map
      type(CharacteristicId), intent(in) :: mismatched_kinds(:)
      type(NodeId), intent(out) :: final_node_id
      character(:), allocatable, intent(out) :: unsupported_characteristic
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: reuse_key
      type(NodeId), pointer :: existing

      unsupported_characteristic = ''

      reuse_key = chain_key(export_node_id, mismatched_kinds, import_map)
      existing => graph%get_resource_index(reuse_key)
      if (associated(existing)) then
         final_node_id = existing
         _RETURN(_SUCCESS)
      end if

      call build_chain(graph, network_id, export_node_id, export_map, import_map, &
           mismatched_kinds, final_node_id, unsupported_characteristic, _RC)

      if (unsupported_characteristic /= '') then
         _RETURN(_SUCCESS)
      end if

      call graph%add_resource_index(reuse_key, final_node_id, _RC)

      _RETURN(_SUCCESS)
   end subroutine find_or_build_extension_chain

   function chain_key(export_node_id, mismatched_kinds, import_map) result(key)
      type(NodeId), intent(in) :: export_node_id
      type(CharacteristicId), intent(in) :: mismatched_kinds(:)
      type(CharacteristicMap), target, intent(in) :: import_map
      character(:), allocatable :: key

      integer :: i, status
      class(Characteristic), pointer :: goal

      key = 'EXTCHAIN:' // export_node_id%to_string()
      do i = 1, size(mismatched_kinds)
         goal => import_map%at(mismatched_kinds(i), rc=status)
         key = key // ':' // goal%get_signature()
      end do
   end function chain_key

   subroutine build_chain(graph, network_id, export_node_id, export_map, import_map, &
        mismatched_kinds, final_node_id, unsupported_characteristic, rc)
      class(ComponentGraph), target, intent(inout) :: graph
      type(DependencyNetworkId), intent(in) :: network_id
      type(NodeId), intent(in) :: export_node_id
      type(CharacteristicMap), target, intent(in) :: export_map
      type(CharacteristicMap), target, intent(in) :: import_map
      type(CharacteristicId), intent(in) :: mismatched_kinds(:)
      type(NodeId), intent(out) :: final_node_id
      character(:), allocatable, intent(out) :: unsupported_characteristic
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i
      class(Characteristic), pointer :: export_characteristic, import_characteristic
      class(Transform), allocatable :: transformer
      type(NodeId) :: current_node_id
      type(NodeId) :: extension_node_id
      type(NodeId) :: transform_node_id
      type(StateItemNode) :: extension_node
      type(GraphStateItem) :: empty_payload
      type(NodeRevision) :: empty_revision
      type(TransformGraphNode) :: transform_node

      current_node_id = export_node_id

      do i = 1, size(mismatched_kinds)
         export_characteristic => export_map%at(mismatched_kinds(i), rc=status)
         import_characteristic => import_map%at(mismatched_kinds(i), rc=status)

         ! Extension StateItemNode: payload left unallocated - not yet
         ! realized, matching advertise_one's own precedent
         ! (GraphBuilder.F90) for advertised items at this point in the
         ! lifecycle.
         extension_node_id = graph%next_node_id(_RC)
         extension_node = StateItemNode(extension_node_id, empty_payload, empty_revision)
         call graph%register_node(extension_node, _RC)

         call export_characteristic%build_transform(graph, current_node_id, extension_node_id, &
              import_characteristic, transformer, status)
         if (status /= 0) then
            unsupported_characteristic = mismatched_kinds(i)%to_string()
            _RETURN(_SUCCESS)
         end if

         transform_node_id = graph%next_node_id(_RC)
         transform_node = TransformGraphNode(transform_node_id, transformer)
         call graph%register_node(transform_node, _RC)

         call graph%bind_port(network_id, transform_node_id, INPUT_PORT_NAME, current_node_id, _RC)
         call graph%bind_port(network_id, transform_node_id, OUTPUT_PORT_NAME, extension_node_id, _RC)

         call graph%add_dependency(network_id, current_node_id, transform_node_id, _RC)
         call graph%add_dependency(network_id, transform_node_id, extension_node_id, _RC)

         current_node_id = extension_node_id
      end do

      final_node_id = current_node_id
      unsupported_characteristic = ''

      _RETURN(_SUCCESS)
   end subroutine build_chain

end module mapl_ExtensionResolution_mod
