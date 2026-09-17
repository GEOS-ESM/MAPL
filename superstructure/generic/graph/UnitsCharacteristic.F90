#include "MAPL.h"

!------------------------------------------------------------------------------
! UnitsCharacteristic: the "units" Characteristic (Characteristic.F90).
! needs_extension_for is a plain string-inequality check - mirrors
! (never calls) UnitsAspect%matches's own comparison
! (superstructure/generic/specs/UnitsAspect.F90): units strings differ ->
! needs an extension; identical -> no-op.
!
! build_transform is the one real Characteristic provider in this change
! (extension-reuse change design.md Decisions): constructs
! UnitsConverterTransform, a fresh mapl_Transform_mod subclass operating
! directly on ESMF_Field data via mapl_FieldPointerUtilities_mod and
! udunits2f - no legacy ExtensionTransform interface involvement.
!------------------------------------------------------------------------------
module mapl_UnitsCharacteristic_mod
   use mapl_Characteristic_mod, only: Characteristic
   use mapl_CharacteristicId_mod, only: CharacteristicId, UNITS_CHARACTERISTIC_ID
   use mapl_ComponentGraph_mod, only: ComponentGraph
   use mapl_NodeId_mod, only: NodeId
   use mapl_Transform_mod, only: Transform
   use mapl_UnitsConverterTransform_mod, only: UnitsConverterTransform
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: UnitsCharacteristic

   type, extends(Characteristic) :: UnitsCharacteristic
      private
      character(:), allocatable :: units
   contains
      procedure, nopass :: get_id => units_get_id
      procedure :: get_signature => units_get_signature
      procedure :: needs_extension_for => units_needs_extension_for
      procedure :: build_transform => units_build_transform
      procedure :: get_units
   end type UnitsCharacteristic

   interface UnitsCharacteristic
      module procedure new_UnitsCharacteristic
   end interface UnitsCharacteristic

contains

   function new_UnitsCharacteristic(units) result(characteristic)
      character(*), intent(in) :: units
      type(UnitsCharacteristic) :: characteristic

      characteristic%units = units
   end function new_UnitsCharacteristic

   function units_get_id() result(id)
      type(CharacteristicId) :: id

      id = UNITS_CHARACTERISTIC_ID
   end function units_get_id

   function units_get_signature(this) result(signature)
      class(UnitsCharacteristic), intent(in) :: this
      character(:), allocatable :: signature

      signature = 'units=' // this%units
   end function units_get_signature

   ! `goal` is guaranteed a UnitsCharacteristic by the caller (both come
   ! from a CharacteristicMap-comparison entry point that pairs entries
   ! by matching kind - mapl_ExtensionResolution_mod). The class default
   ! branch is defensive only; it deliberately does not use
   ! _ASSERT/_RETURN (no `rc` argument on this interface) and instead
   ! reports "needs extension" so a genuine kind mismatch is never
   ! silently treated as a match.
   logical function units_needs_extension_for(this, goal) result(needs_extension)
      class(UnitsCharacteristic), intent(in) :: this
      class(Characteristic), intent(in) :: goal

      select type (goal)
      class is (UnitsCharacteristic)
         needs_extension = (this%units /= goal%units)
      class default
         needs_extension = .true.
      end select
   end function units_needs_extension_for

   subroutine units_build_transform(this, graph, input_node_id, output_node_id, goal, transformer, rc)
      class(UnitsCharacteristic), intent(in) :: this
      class(ComponentGraph), target, intent(in) :: graph
      type(NodeId), intent(in) :: input_node_id
      type(NodeId), intent(in) :: output_node_id
      class(Characteristic), intent(in) :: goal
      class(Transform), allocatable, intent(out) :: transformer
      integer, optional, intent(out) :: rc

      select type (goal)
      class is (UnitsCharacteristic)
         allocate(transformer, source=UnitsConverterTransform(graph, input_node_id, output_node_id, &
              this%units, goal%units))
      class default
         _FAIL('UnitsCharacteristic: build_transform called with a non-units goal')
      end select

      _RETURN(_SUCCESS)
   end subroutine units_build_transform

   function get_units(this) result(units)
      class(UnitsCharacteristic), intent(in) :: this
      character(:), allocatable :: units

      units = this%units
   end function get_units

end module mapl_UnitsCharacteristic_mod
