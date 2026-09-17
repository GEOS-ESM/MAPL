#include "MAPL.h"

!------------------------------------------------------------------------------
! VerticalGridCharacteristic: the "vertical_grid" Characteristic
! (Characteristic.F90). Deliberately minimal for this change:
! needs_extension_for is a presence/identity check only, sufficient to
! *detect* a mismatch, and build_transform always fails explicitly - no
! real vertical-regrid provider exists yet (extension-reuse change
! design.md Non-Goals). Exists so this change's Characteristic hierarchy
! has a genuine "mismatch detected, no real transform implemented"
! example to exercise (spec "Unregistered characteristic fails loudly"),
! without requiring real ESMF regridding to be implemented first. A
! follow-up sub-change is expected to give build_transform a real
! implementation here.
!------------------------------------------------------------------------------
module mapl_VerticalGridCharacteristic_mod
   use mapl_Characteristic_mod, only: Characteristic
   use mapl_CharacteristicId_mod, only: CharacteristicId, VERTICAL_GRID_CHARACTERISTIC_ID
   use mapl_ComponentGraph_mod, only: ComponentGraph
   use mapl_NodeId_mod, only: NodeId
   use mapl_Transform_mod, only: Transform
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: VerticalGridCharacteristic

   type, extends(Characteristic) :: VerticalGridCharacteristic
      private
      character(:), allocatable :: grid_id
   contains
      procedure, nopass :: get_id => vgrid_get_id
      procedure :: get_signature => vgrid_get_signature
      procedure :: needs_extension_for => vgrid_needs_extension_for
      procedure :: build_transform => vgrid_build_transform
      procedure :: get_grid_id
   end type VerticalGridCharacteristic

   interface VerticalGridCharacteristic
      module procedure new_VerticalGridCharacteristic
   end interface VerticalGridCharacteristic

contains

   ! `grid_id` is an opaque identity token for the declared vertical
   ! grid - callers (mapl_ExtensionResolution_mod) are responsible for
   ! deriving one that distinguishes different vertical grids; this type
   ! only compares the token, it does not interpret it.
   function new_VerticalGridCharacteristic(grid_id) result(characteristic)
      character(*), intent(in) :: grid_id
      type(VerticalGridCharacteristic) :: characteristic

      characteristic%grid_id = grid_id
   end function new_VerticalGridCharacteristic

   function vgrid_get_id() result(id)
      type(CharacteristicId) :: id

      id = VERTICAL_GRID_CHARACTERISTIC_ID
   end function vgrid_get_id

   function vgrid_get_signature(this) result(signature)
      class(VerticalGridCharacteristic), intent(in) :: this
      character(:), allocatable :: signature

      signature = 'vertical_grid=' // this%grid_id
   end function vgrid_get_signature

   ! Defensive class-default branch (no `rc` argument on this interface,
   ! so _ASSERT/_RETURN are not used here - mirrors
   ! UnitsCharacteristic%needs_extension_for's own rationale): a genuine
   ! kind mismatch reports "needs extension" rather than silently
   ! matching.
   logical function vgrid_needs_extension_for(this, goal) result(needs_extension)
      class(VerticalGridCharacteristic), intent(in) :: this
      class(Characteristic), intent(in) :: goal

      select type (goal)
      class is (VerticalGridCharacteristic)
         needs_extension = (this%grid_id /= goal%grid_id)
      class default
         needs_extension = .true.
      end select
   end function vgrid_needs_extension_for

   ! No real vertical-regrid Transform exists yet - fails explicitly
   ! (spec "Unregistered characteristic fails loudly") rather than
   ! allocating a no-op/stub Transform.
   subroutine vgrid_build_transform(this, graph, input_node_id, output_node_id, goal, transformer, rc)
      class(VerticalGridCharacteristic), intent(in) :: this
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

      _FAIL('VerticalGridCharacteristic: no vertical-regrid Transform is implemented yet')
   end subroutine vgrid_build_transform

   function get_grid_id(this) result(grid_id)
      class(VerticalGridCharacteristic), intent(in) :: this
      character(:), allocatable :: grid_id

      grid_id = this%grid_id
   end function get_grid_id

end module mapl_VerticalGridCharacteristic_mod
