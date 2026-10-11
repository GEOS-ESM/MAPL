#include "MAPL.h"

!------------------------------------------------------------------------------
! TypeKindCharacteristicAdapter: the Characteristic-side bridge
! (mapl_Characteristic_mod) wrapping the already-shipped, StateItemCharacteristic
! -side TypeKindCharacteristic (TypeKindCharacteristic.F90) by composition -
! openspec/changes/phase-10c-typekind-characteristic design.md D1 (the
! archived phase-10c-typekind-unification amendment's own "Decision 2").
!
! Exists solely because Fortran has no retroactive multiple inheritance:
! TypeKindCharacteristic already extends ValueCharacteristic (a different
! abstract type, a different interface, no build_transform anywhere in
! that hierarchy by design - REQ-CHAR-002, "describes, does not adapt").
! This type extends Characteristic instead, holds the existing,
! unmodified TypeKindCharacteristic by composition, and forwards every
! query to it directly - no comparison logic is duplicated. The one real
! new capability this type adds is build_transform, which the wrapped
! type's own hierarchy deliberately does not have.
!------------------------------------------------------------------------------
module mapl_TypeKindCharacteristicAdapter_mod
   use mapl_Characteristic_mod, only: Characteristic
   use mapl_CharacteristicId_mod, only: CharacteristicId, TYPE_KIND_CHARACTERISTIC_ID
   use mapl_ComponentGraph_mod, only: ComponentGraph
   use mapl_NodeId_mod, only: NodeId
   use mapl_Transform_mod, only: Transform
   use mapl_TypeKindCharacteristic_mod, only: TypeKindCharacteristic
   use mapl_TypeKindConverterTransform_mod, only: TypeKindConverterTransform
   use ESMF, only: ESMF_TypeKind_Flag, ESMF_TYPEKIND_R4, ESMF_TYPEKIND_R8
   use ESMF, only: operator(==)
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: TypeKindCharacteristicAdapter

   type, extends(Characteristic) :: TypeKindCharacteristicAdapter
      private
      type(TypeKindCharacteristic) :: inner
   contains
      procedure, nopass :: get_id => adapter_get_id
      procedure :: get_signature => adapter_get_signature
      procedure :: needs_extension_for => adapter_needs_extension_for
      procedure :: build_transform => adapter_build_transform
      procedure :: get_type_kind => adapter_get_type_kind
   end type TypeKindCharacteristicAdapter

   interface TypeKindCharacteristicAdapter
      module procedure new_TypeKindCharacteristicAdapter_label
      module procedure new_TypeKindCharacteristicAdapter_flag
   end interface TypeKindCharacteristicAdapter

contains

   ! Direct form - plain 'R4'/'R8' label, matching
   ! TypeKindCharacteristic's own constructor exactly.
   function new_TypeKindCharacteristicAdapter_label(type_kind) result(characteristic)
      character(*), intent(in) :: type_kind
      type(TypeKindCharacteristicAdapter) :: characteristic

      characteristic%inner = TypeKindCharacteristic(type_kind)
   end function new_TypeKindCharacteristicAdapter_label

   ! Convenience form - real ESMF_TypeKind_Flag, converted to the plain
   ! label internally (design.md Risks - a small, local helper, not a
   ! new general-purpose typekind-to-string utility). GraphBuilder.F90's
   ! build_characteristics() uses this form directly, since
   ! VariableSpec%typekind is a real ESMF_TypeKind_Flag.
   function new_TypeKindCharacteristicAdapter_flag(type_kind) result(characteristic)
      type(ESMF_TypeKind_Flag), intent(in) :: type_kind
      type(TypeKindCharacteristicAdapter) :: characteristic

      characteristic%inner = TypeKindCharacteristic(typekind_to_label(type_kind))
   end function new_TypeKindCharacteristicAdapter_flag

   function typekind_to_label(type_kind) result(label)
      type(ESMF_TypeKind_Flag), intent(in) :: type_kind
      character(:), allocatable :: label

      if (type_kind == ESMF_TYPEKIND_R4) then
         label = 'R4'
      else if (type_kind == ESMF_TYPEKIND_R8) then
         label = 'R8'
      else
         error stop 'TypeKindCharacteristicAdapter: unsupported ESMF_TypeKind_Flag'
      end if
   end function typekind_to_label

   function label_to_typekind(label) result(type_kind)
      character(*), intent(in) :: label
      type(ESMF_TypeKind_Flag) :: type_kind

      if (label == 'R4') then
         type_kind = ESMF_TYPEKIND_R4
      else if (label == 'R8') then
         type_kind = ESMF_TYPEKIND_R8
      else
         error stop 'TypeKindCharacteristicAdapter: unsupported type-kind label'
      end if
   end function label_to_typekind

   function adapter_get_id() result(id)
      type(CharacteristicId) :: id

      id = TYPE_KIND_CHARACTERISTIC_ID
   end function adapter_get_id

   function adapter_get_signature(this) result(signature)
      class(TypeKindCharacteristicAdapter), intent(in) :: this
      character(:), allocatable :: signature

      signature = 'typekind=' // this%inner%get_type_kind()
   end function adapter_get_signature

   ! `goal` is guaranteed a TypeKindCharacteristicAdapter by the caller
   ! (both come from a CharacteristicMap-comparison entry point that
   ! pairs entries by matching kind - mapl_ExtensionResolution_mod).
   ! Mirrors UnitsCharacteristic's own defensive-but-not-_FAIL'd default
   ! (Characteristic's interface has no rc to fail through) - not
   ! TypeKindCharacteristic's own `error stop`, since the calling
   ! convention here is Characteristic's, not StateItemCharacteristic's.
   logical function adapter_needs_extension_for(this, goal) result(needs_extension)
      class(TypeKindCharacteristicAdapter), intent(in) :: this
      class(Characteristic), intent(in) :: goal

      select type (goal)
      class is (TypeKindCharacteristicAdapter)
         needs_extension = this%inner%needs_extension_for(goal%inner)
      class default
         needs_extension = .true.
      end select
   end function adapter_needs_extension_for

   subroutine adapter_build_transform(this, graph, input_node_id, output_node_id, goal, transformer, rc)
      class(TypeKindCharacteristicAdapter), intent(in) :: this
      class(ComponentGraph), target, intent(in) :: graph
      type(NodeId), intent(in) :: input_node_id
      type(NodeId), intent(in) :: output_node_id
      class(Characteristic), intent(in) :: goal
      class(Transform), allocatable, intent(out) :: transformer
      integer, optional, intent(out) :: rc

      select type (goal)
      class is (TypeKindCharacteristicAdapter)
         allocate(transformer, source=TypeKindConverterTransform(graph, input_node_id, output_node_id, &
              label_to_typekind(this%inner%get_type_kind()), label_to_typekind(goal%inner%get_type_kind())))
      class default
         _FAIL('TypeKindCharacteristicAdapter: build_transform called with a non-typekind goal')
      end select

      _RETURN(_SUCCESS)
   end subroutine adapter_build_transform

   function adapter_get_type_kind(this) result(type_kind)
      class(TypeKindCharacteristicAdapter), intent(in) :: this
      character(:), allocatable :: type_kind

      type_kind = this%inner%get_type_kind()
   end function adapter_get_type_kind

end module mapl_TypeKindCharacteristicAdapter_mod
