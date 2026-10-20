#include "MAPL.h"

!------------------------------------------------------------------------------
! ItemShapeCharacteristic: the "item-class/shape" StateItemCharacteristic
! (openspec/changes/phase-7a-shape-dispatch-wiring, docs/graph/spec/
! 20-implementation-roadmap.md Sec 20.4.5) - a ValueCharacteristic
! (design.md D1): holds its own copy of the declared item class
! (type(ESMF_StateItem_Flag): FIELD/VECTOR/BRACKET/VECTORBRACKET/...)
! inline, rather than teaching find_mismatched_characteristics/build_chain
! (ExtensionResolution.F90) a declared_item_type-specific comparison path
! outside the characteristics map.
!
! Deliberately duplicates the same value GraphStateItem%declared_item_type
! already carries (set from the same var_spec%itemType expression,
! GraphBuilder_Advertising_smod.F90's populate_characteristics,
! design.md D1) - a narrow, accepted cost of walking this axis through
! the SAME find_mismatched_characteristics/build_chain loop already used
! for Units and TypeKind, uniformly, rather than a bespoke non-uniform
! mismatch check for this one axis (design.md D1, roadmap Sec 20.4.5).
!
! build_transform (design.md D3) succeeds only for the BRACKET (this) ->
! FIELD (goal) pairing, returning a BracketTimeInterpolationTransform
! (Section 3) - every other pairing, including VECTORBRACKET->VECTOR,
! fails explicitly, mirroring PhysicalUnitsCharacteristic%units_build_
! transform's own class default -> _FAIL precedent.
!------------------------------------------------------------------------------
module mapl_ItemShapeCharacteristic_mod
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, ValueCharacteristic
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind, ITEM_SHAPE_CHARACTERISTIC_KIND
   use mapl_TransformBase_mod, only: TransformBase
   use mapl_BracketTimeInterpolationTransform_mod, only: BracketTimeInterpolationTransform
   use mapl_StateItem_mod, only: MAPL_STATEITEM_FIELD, MAPL_STATEITEM_FIELDBUNDLE, MAPL_STATEITEM_STATE, &
        MAPL_STATEITEM_SERVICE, MAPL_STATEITEM_WILDCARD, MAPL_STATEITEM_BRACKET, MAPL_STATEITEM_VECTOR, &
        MAPL_STATEITEM_VECTORBRACKET, MAPL_STATEITEM_EXPRESSION
   use ESMF, only: ESMF_StateItem_Flag
   use ESMF, only: operator(==)
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: ItemShapeCharacteristic

   type, extends(ValueCharacteristic) :: ItemShapeCharacteristic
      private
      type(ESMF_StateItem_Flag) :: item_class
   contains
      procedure, nopass :: get_kind => item_shape_get_kind
      procedure :: needs_extension_for => item_shape_needs_extension_for
      procedure :: build_transform => item_shape_build_transform
      procedure :: get_signature => item_shape_get_signature
      procedure :: get_item_class
   end type ItemShapeCharacteristic

   interface ItemShapeCharacteristic
      module procedure new_ItemShapeCharacteristic
   end interface ItemShapeCharacteristic

contains

   function new_ItemShapeCharacteristic(item_class) result(characteristic)
      type(ESMF_StateItem_Flag), intent(in) :: item_class
      type(ItemShapeCharacteristic) :: characteristic

      characteristic%item_class = item_class
   end function new_ItemShapeCharacteristic

   function item_shape_get_kind() result(kind)
      type(StateItemCharacteristicKind) :: kind

      kind = ITEM_SHAPE_CHARACTERISTIC_KIND
   end function item_shape_get_kind

   function get_item_class(this) result(item_class)
      class(ItemShapeCharacteristic), intent(in) :: this
      type(ESMF_StateItem_Flag) :: item_class

      item_class = this%item_class
   end function get_item_class

   ! `goal` is guaranteed an ItemShapeCharacteristic for the real call
   ! chain this method is used in: the one production caller,
   ! find_mismatched_state_item_characteristics, looks both sides up by
   ! the same StateItemCharacteristicKind key, and
   ! GraphStateItem%set_characteristic (the only insertion path into that
   ! map) asserts a characteristic's own kind matches its map key before
   ! inserting (REQ-CHAR-006/009, design.md D9) - so the class default
   ! branch below should be checked by the calling procedure, matching
   ! this codebase's own established `error stop` precedent for exactly
   ! that situation (PhysicalUnitsCharacteristic.F90/TypeKindCharacteristic
   ! .F90's own identical precedent, design.md D2).
   logical function item_shape_needs_extension_for(this, goal) result(needs_extension)
      class(ItemShapeCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal

      select type (goal)
      class is (ItemShapeCharacteristic)
         needs_extension = .not. (this%item_class == goal%item_class)
      class default
         error stop 'ItemShapeCharacteristic: needs_extension_for called with a non-ItemShapeCharacteristic goal - should be checked by calling procedure'
      end select
   end function item_shape_needs_extension_for

   ! design.md D3: succeeds only for BRACKET (this) -> FIELD (goal) -
   ! every other pairing (including VECTORBRACKET->VECTOR, design.md
   ! Non-Goals) _FAILs explicitly, naming both sides' item classes.
   subroutine item_shape_build_transform(this, goal, transformer, rc)
      class(ItemShapeCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal
      class(TransformBase), allocatable, intent(out) :: transformer
      integer, optional, intent(out) :: rc

      character(:), allocatable :: message

      select type (goal)
      class is (ItemShapeCharacteristic)
         if (this%item_class == MAPL_STATEITEM_BRACKET .and. goal%item_class == MAPL_STATEITEM_FIELD) then
            allocate(transformer, source=BracketTimeInterpolationTransform())
         else
            message = 'ItemShapeCharacteristic: build_transform - unsupported item-class pairing: ' // &
                 item_class_name(this%item_class) // ' -> ' // item_class_name(goal%item_class)
            _FAIL(message)
         end if
      class default
         _FAIL('ItemShapeCharacteristic: build_transform called with a non-item-shape goal')
      end select

      _RETURN(_SUCCESS)
   end subroutine item_shape_build_transform

   function item_shape_get_signature(this) result(signature)
      class(ItemShapeCharacteristic), intent(in) :: this
      character(:), allocatable :: signature

      signature = 'item_class=' // item_class_name(this%item_class)
   end function item_shape_get_signature

   ! Human-readable item-class name, confirmed during implementation
   ! (task 2.5/2.6): ESMF_StateItem_Flag has no to_string-equivalent
   ! method, only a plain integer %ot component - mirrors GraphBuilder
   ! .F90's own itemtype_name %ot select-case precedent exactly, kept as
   ! its own local copy here rather than a cross-module reference, since
   ! this module (graph/characteristics) sits below GraphBuilder.F90 in
   ! the dependency layering.
   function item_class_name(item_class) result(name)
      type(ESMF_StateItem_Flag), intent(in) :: item_class
      character(:), allocatable :: name

      select case (item_class%ot)
      case (MAPL_STATEITEM_FIELD%ot)
         name = 'FIELD'
      case (MAPL_STATEITEM_FIELDBUNDLE%ot)
         name = 'FIELDBUNDLE'
      case (MAPL_STATEITEM_VECTOR%ot)
         name = 'VECTOR'
      case (MAPL_STATEITEM_VECTORBRACKET%ot)
         name = 'VECTORBRACKET'
      case (MAPL_STATEITEM_BRACKET%ot)
         name = 'BRACKET'
      case (MAPL_STATEITEM_STATE%ot)
         name = 'STATE'
      case (MAPL_STATEITEM_SERVICE%ot)
         name = 'SERVICE'
      case (MAPL_STATEITEM_EXPRESSION%ot)
         name = 'EXPRESSION'
      case (MAPL_STATEITEM_WILDCARD%ot)
         name = 'WILDCARD'
      case default
         name = 'UNKNOWN'
      end select
   end function item_class_name

end module mapl_ItemShapeCharacteristic_mod
