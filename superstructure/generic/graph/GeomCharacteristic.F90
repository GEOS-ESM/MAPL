#include "MAPL.h"

!------------------------------------------------------------------------------
! GeomCharacteristic: the "geom" Characteristic (Characteristic.F90),
! following VerticalGridCharacteristic.F90's own precedent exactly:
! needs_extension_for is a presence/identity check only, sufficient to
! *detect* a mismatch, and build_transform always fails explicitly - no
! real horizontal-regrid provider exists yet
! (openspec/changes/horizontal-geometry-graph-state-item, design.md
! Non-Goals). A follow-up sub-change is expected to give build_transform
! a real implementation here, mirroring VerticalGridCharacteristic's own
! stated follow-up.
!
! The opaque identity token is `GeomId%get_value()` rendered to text -
! `GeomId` (mapl_GeomId_mod, infrastructure/geom/GeomId.F90) is the same
! identity type legacy GeomAspect already uses (via MAPL_GeomGetId/
! MAPL_SameGeom, mapl_geom_api) to compare two ESMF_Geom handles; this
! module reuses that identity concept rather than inventing a parallel
! one (design.md Decision D3).
!------------------------------------------------------------------------------
module mapl_GeomCharacteristic_mod
   use mapl_Characteristic_mod, only: Characteristic
   use mapl_CharacteristicId_mod, only: CharacteristicId, GEOM_CHARACTERISTIC_ID
   use mapl_ComponentGraph_mod, only: ComponentGraph
   use mapl_NodeId_mod, only: NodeId
   use mapl_Transform_mod, only: Transform
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: GeomCharacteristic

   type, extends(Characteristic) :: GeomCharacteristic
      private
      character(:), allocatable :: geom_id
   contains
      procedure, nopass :: get_id => geom_get_id
      procedure :: get_signature => geom_get_signature
      procedure :: needs_extension_for => geom_needs_extension_for
      procedure :: build_transform => geom_build_transform
      procedure :: get_geom_id
   end type GeomCharacteristic

   interface GeomCharacteristic
      module procedure new_GeomCharacteristic
   end interface GeomCharacteristic

contains

   ! `geom_id` is an opaque identity token for the declared horizontal
   ! geometry - callers (mapl_GeomProxyField_mod/GraphBuilder.F90) are
   ! responsible for deriving one that distinguishes different geometries
   ! (in practice, MAPL_GeomGetId's own GeomId value rendered to text);
   ! this type only compares the token, it does not interpret it.
   function new_GeomCharacteristic(geom_id) result(characteristic)
      character(*), intent(in) :: geom_id
      type(GeomCharacteristic) :: characteristic

      characteristic%geom_id = geom_id
   end function new_GeomCharacteristic

   function geom_get_id() result(id)
      type(CharacteristicId) :: id

      id = GEOM_CHARACTERISTIC_ID
   end function geom_get_id

   function geom_get_signature(this) result(signature)
      class(GeomCharacteristic), intent(in) :: this
      character(:), allocatable :: signature

      signature = 'geom=' // this%geom_id
   end function geom_get_signature

   ! Defensive class-default branch (no `rc` argument on this interface,
   ! so _ASSERT/_RETURN are not used here - mirrors
   ! VerticalGridCharacteristic%needs_extension_for's own rationale): a
   ! genuine kind mismatch reports "needs extension" rather than silently
   ! matching.
   logical function geom_needs_extension_for(this, goal) result(needs_extension)
      class(GeomCharacteristic), intent(in) :: this
      class(Characteristic), intent(in) :: goal

      select type (goal)
      class is (GeomCharacteristic)
         needs_extension = (this%geom_id /= goal%geom_id)
      class default
         needs_extension = .true.
      end select
   end function geom_needs_extension_for

   ! No real horizontal-regrid Transform exists yet - fails explicitly
   ! (spec "A geometry mismatch is reported through extension-chain
   ! delegation, not silently mis-wired") rather than allocating a
   ! no-op/stub Transform.
   subroutine geom_build_transform(this, graph, input_node_id, output_node_id, goal, transformer, rc)
      class(GeomCharacteristic), intent(in) :: this
      class(ComponentGraph), target, intent(in) :: graph
      type(NodeId), intent(in) :: input_node_id
      type(NodeId), intent(in) :: output_node_id
      class(Characteristic), intent(in) :: goal
      class(Transform), allocatable, intent(out) :: transformer
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(graph)
      _UNUSED_DUMMY(input_node_id)
      _UNUSED_DUMMY(output_node_id)
      _UNUSED_DUMMY(goal)

      _FAIL('GeomCharacteristic: no horizontal-regrid Transform is implemented yet')
   end subroutine geom_build_transform

   function get_geom_id(this) result(geom_id)
      class(GeomCharacteristic), intent(in) :: this
      character(:), allocatable :: geom_id

      geom_id = this%geom_id
   end function get_geom_id

end module mapl_GeomCharacteristic_mod
