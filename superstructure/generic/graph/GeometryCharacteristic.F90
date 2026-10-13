#include "MAPL.h"

!------------------------------------------------------------------------------
! GeometryCharacteristic: the "geometry" StateItemCharacteristic
! (docs/graph/spec/18-state-item-characteristics.md REQ-CHAR-001 table) -
! a ReferenceCharacteristic (REQ-CHAR-002a/002b: holds a NodeId
! referencing a shared graph node, not a value of its own).
!
! Named GeometryCharacteristic, NOT GeomCharacteristic (the spec table's
! own suggested name) - design.md D3: an exact, unavoidable collision
! with the existing, unrelated mapl_GeomCharacteristic_mod/
! GeomCharacteristic type already shipped for graph/extension-reuse
! (Fortran module/type names share one global namespace). Same
! collision-avoidance discipline already applied once in this codebase
! for UnitsConverterTransform vs. legacy ConvertUnitsTransform.
!
! openspec/changes/merge-characteristic-families: absorbs the retired
! mapl_GeomCharacteristic_mod's own opaque geom_id string-token identity
! and its (always-failing) build_transform/get_signature, in addition to
! this type's own pre-existing NodeId-sharing role - same HYBRID
! needs_extension_for pattern as VerticalCoordinateCharacteristic.F90
! (see that module's own header comment for the full rationale): a real
! referenced_node_id set on both sides (the GraphStateItem-sharing use
! case, REQ-CHAR-012) is authoritative and unchanged from this type's
! own pre-merge behavior; otherwise (the GraphBuilder-production-
! call-site case, which supplies only an opaque geom_id string token via
! GeomId%get_value() rendered to text, never a NodeId) falls back to the
! retired mapl_GeomCharacteristic_mod's own identity-token comparison.
!------------------------------------------------------------------------------
module mapl_GeometryCharacteristic_mod
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, ReferenceCharacteristic
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind, GEOMETRY_CHARACTERISTIC_KIND
   use mapl_NodeId_mod, only: NodeId, operator(==), operator(/=)
   use mapl_TransformBase_mod, only: TransformBase
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: GeometryCharacteristic

   type, extends(ReferenceCharacteristic) :: GeometryCharacteristic
      private
      ! Moved from the retired mapl_GeomCharacteristic_mod - the opaque
      ! identity token, carried alongside the inherited
      ! referenced_node_id (same deliberate mix as
      ! VerticalCoordinateCharacteristic.F90, design.md Risks).
      character(:), allocatable :: geom_id
   contains
      procedure, nopass :: get_kind => geometry_get_kind
      procedure :: needs_extension_for => geometry_needs_extension_for
      procedure :: build_transform => geometry_build_transform
      procedure :: get_signature => geometry_get_signature
      procedure :: get_geom_id
      procedure :: has_geom_id
   end type GeometryCharacteristic

   interface GeometryCharacteristic
      module procedure new_GeometryCharacteristic
   end interface GeometryCharacteristic

contains

   ! referenced_node_id: the NodeId of the ordinary graph node (an
   ! esmf_field-kind StateItemNode in the GRIDSET-only geometry-proxy
   ! role, per 04-graph-value-hierarchy.md §4.6.4/REQ-CHAR-002b) this
   ! characteristic points at - set via the inherited
   ! ReferenceCharacteristic%set_referenced_node_id accessor, not a bare
   ! field assignment (that component is private to
   ! mapl_StateItemCharacteristic_mod). OPTIONAL, mirroring
   ! VerticalCoordinateCharacteristic.F90's own constructor (task 2.4): a
   ! GraphBuilder-production call site (GraphBuilder_Geometry_smod.F90's
   ! GeomCharacteristic(trim(id_buffer)) call sites) has no NodeId to
   ! supply, only geom_id; a GraphStateItem-sharing call site supplies
   ! only referenced_node_id.
   function new_GeometryCharacteristic(referenced_node_id, geom_id) result(characteristic)
      type(NodeId), optional, intent(in) :: referenced_node_id
      character(*), optional, intent(in) :: geom_id
      type(GeometryCharacteristic) :: characteristic

      if (present(referenced_node_id)) call characteristic%set_referenced_node_id(referenced_node_id)
      if (present(geom_id)) characteristic%geom_id = geom_id
   end function new_GeometryCharacteristic

   function geometry_get_kind() result(kind)
      type(StateItemCharacteristicKind) :: kind

      kind = GEOMETRY_CHARACTERISTIC_KIND
   end function geometry_get_kind

   ! `goal` is guaranteed a GeometryCharacteristic for the real call
   ! chain this method is used in: the one production caller,
   ! find_mismatched_state_item_characteristics, looks both sides up by
   ! the same StateItemCharacteristicKind key, and
   ! GraphStateItem%set_characteristic (the only insertion path into that
   ! map) asserts a characteristic's own kind matches its map key before
   ! inserting (REQ-CHAR-006/009, design.md D9). The class default
   ! branch below should be checked by the calling procedure, matching
   ! this codebase's own established `error stop` precedent for exactly
   ! that situation (DependencyNetwork.F90, ActualConnectionPt.F90).
   !
   ! Hybrid comparison (module header): real NodeId on both sides is
   ! authoritative when present; otherwise falls back to the retired
   ! mapl_GeomCharacteristic_mod's own identity-token comparison.
   logical function geometry_needs_extension_for(this, goal) result(needs_extension)
      class(GeometryCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal

      type(NodeId) :: this_node_id, goal_node_id

      select type (goal)
      class is (GeometryCharacteristic)
         this_node_id = this%get_referenced_node_id()
         goal_node_id = goal%get_referenced_node_id()
         if (this_node_id%is_valid() .and. goal_node_id%is_valid()) then
            needs_extension = (this_node_id /= goal_node_id)
         else
            needs_extension = (this%geom_id /= goal%geom_id)
         end if
      class default
         error stop 'GeometryCharacteristic: needs_extension_for called with a non-GeometryCharacteristic goal - should be checked by calling procedure'
      end select
   end function geometry_needs_extension_for

   ! Moved from the retired mapl_GeomCharacteristic_mod's own
   ! geom_build_transform - no real horizontal-regrid Transform exists
   ! yet, fails explicitly rather than allocating a no-op/stub Transform.
   subroutine geometry_build_transform(this, goal, transformer, rc)
      class(GeometryCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal
      class(TransformBase), allocatable, intent(out) :: transformer
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(goal)

      _FAIL('GeometryCharacteristic: no horizontal-regrid Transform is implemented yet')
   end subroutine geometry_build_transform

   ! Moved from the retired mapl_GeomCharacteristic_mod's own
   ! geom_get_signature. The NodeId-sharing case (no geom_id set) falls
   ! back to the referenced node's own string form - mirrors
   ! VerticalCoordinateCharacteristic%vcoord_get_signature's own
   ! equivalent fallback.
   function geometry_get_signature(this) result(signature)
      class(GeometryCharacteristic), intent(in) :: this
      character(:), allocatable :: signature

      type(NodeId) :: this_node_id

      if (allocated(this%geom_id)) then
         signature = 'geom=' // this%geom_id
      else
         this_node_id = this%get_referenced_node_id()
         signature = 'geom_node=' // this_node_id%to_string()
      end if
   end function geometry_get_signature

   function get_geom_id(this) result(geom_id)
      class(GeometryCharacteristic), intent(in) :: this
      character(:), allocatable :: geom_id

      geom_id = this%geom_id
   end function get_geom_id

   logical function has_geom_id(this) result(has)
      class(GeometryCharacteristic), intent(in) :: this

      has = allocated(this%geom_id)
   end function has_geom_id

end module mapl_GeometryCharacteristic_mod
