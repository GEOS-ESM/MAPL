#include "MAPL.h"

!------------------------------------------------------------------------------
! ItemShapeCharacteristic: the item class (FIELD, FIELDBUNDLE, STATE, SERVICE,
! WILDCARD, BRACKET, VECTOR, VECTORBRACKET, EXPRESSION) of a state item, plus
! the optional MAPL_StateItem_Flag variant (e.g. a VERTICALGRID composite).
! Every shape carries one.
!
! needs_extension_for and build_transform follow legacy ClassAspect matches /
! supports_conversion_specific. A pair the matrix accepts needs no step; a
! BRACKET export reaching a FIELD import is time-interpolated; an EXPRESSION
! export matches a FIELD import (the expression hook evaluates into the export
! node); VECTORBRACKET -> VECTOR, which legacy converts, reports unsupported;
! every other pair can never connect and reports incompatible.
!
! get_extension_order gives the order in which this shape reconciles
! mismatched characteristics (legacy get_aspect_order). Kinds the list does
! not name are ordered by the caller.
!------------------------------------------------------------------------------
module mapl_ItemShapeCharacteristic_mod
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, &
        ValueCharacteristic, StateItemCharacteristicMap, &
        report_build_transform_unsupported, &
        report_build_transform_incompatible, SYNC_PHASE_STRUCTURAL
   use mapl_CharacteristicStatus_mod, only: CHARACTERISTIC_STATUS_SPECIFIED
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind
   use mapl_StateItemCharacteristicKind_mod, only: &
        ITEM_SHAPE => ITEM_SHAPE_CHARACTERISTIC_KIND, &
        ATTRIBUTES => ATTRIBUTES_CHARACTERISTIC_KIND, &
        UNGRIDDED_DIMS => UNGRIDDED_DIMS_CHARACTERISTIC_KIND, &
        QUANTITY_TYPE => QUANTITY_TYPE_CHARACTERISTIC_KIND, &
        CONSERVATION => CONSERVATION_CHARACTERISTIC_KIND, &
        HORIZONTAL_DIMS_SPEC => HORIZONTAL_DIMS_SPEC_CHARACTERISTIC_KIND, &
        GEOMETRY => GEOMETRY_CHARACTERISTIC_KIND, &
        VERTICAL_GRID => VERTICAL_GRID_CHARACTERISTIC_KIND, &
        VERTICAL_STAGGER => VERTICAL_STAGGER_CHARACTERISTIC_KIND, &
        VERTICAL_ORIENTATION => VERTICAL_ORIENTATION_CHARACTERISTIC_KIND, &
        NORMALIZATION => NORMALIZATION_CHARACTERISTIC_KIND, &
        PHYSICAL_UNITS => PHYSICAL_UNITS_CHARACTERISTIC_KIND, &
        STANDARD_NAME => STANDARD_NAME_CHARACTERISTIC_KIND, &
        TYPE_KIND => TYPE_KIND_CHARACTERISTIC_KIND
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_TransformBase_mod, only: TransformBase
   use mapl_BracketTimeInterpolationTransform_mod, only: &
        BracketTimeInterpolationTransform
   use mapl_StateItem_mod, only: MAPL_STATEITEM_FIELD, &
        MAPL_STATEITEM_FIELDBUNDLE, MAPL_STATEITEM_STATE, &
        MAPL_STATEITEM_SERVICE, MAPL_STATEITEM_WILDCARD, &
        MAPL_STATEITEM_BRACKET, MAPL_STATEITEM_VECTOR, &
        MAPL_STATEITEM_VECTORBRACKET, MAPL_STATEITEM_EXPRESSION
   use mapl_StateItemFlag_mod, only: MAPL_StateItem_Flag, operator(==), &
        VARIANT_GEOM => MAPL_STATEITEM_GEOM, &
        VARIANT_VERTICALGRID => MAPL_STATEITEM_VERTICALGRID
   use mapl_enums_api, only: MAPL_FieldBundleType_Flag, &
        MAPL_FIELDBUNDLETYPE_BRACKET, MAPL_FIELDBUNDLETYPE_VECTOR, &
        MAPL_FIELDBUNDLETYPE_VECTORBRACKET, MAPL_FIELDBUNDLETYPE_SERVICE
   use mapl_field_api, only: MAPL_FieldSet
   use mapl_field_bundle_api, only: MAPL_FieldBundleSet
   use mapl_state_api, only: MAPL_StateSet
   use ESMF, only: ESMF_StateItem_Flag, ESMF_Field, ESMF_FieldBundle, ESMF_State
   use ESMF, only: ESMF_STATEITEM_FIELD, ESMF_STATEITEM_FIELDBUNDLE, &
        ESMF_STATEITEM_STATE
   use ESMF, only: operator(==)
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: ItemShapeCharacteristic

   type, extends(ValueCharacteristic) :: ItemShapeCharacteristic
      private
      type(ESMF_StateItem_Flag) :: item_class
      type(MAPL_StateItem_Flag), allocatable :: variant
   contains
      procedure, nopass :: get_kind => item_shape_get_kind
      procedure :: needs_extension_for => item_shape_needs_extension_for
      procedure :: build_transform => item_shape_build_transform
      procedure :: get_signature => item_shape_get_signature
      procedure :: write_info => item_shape_write_info
      procedure :: get_extension_order
      procedure :: get_item_class
   end type ItemShapeCharacteristic

   interface ItemShapeCharacteristic
      module procedure new_ItemShapeCharacteristic
   end interface ItemShapeCharacteristic

   ! Outcomes of the shape matrix for one (export, import) pair.
   integer, parameter :: SHAPE_MATCH = 0
   integer, parameter :: SHAPE_CONVERT = 1
   integer, parameter :: SHAPE_UNSUPPORTED = 2
   integer, parameter :: SHAPE_INCOMPATIBLE = 3

contains

   function new_ItemShapeCharacteristic(item_class, variant) &
        result(characteristic)
      type(ESMF_StateItem_Flag), intent(in) :: item_class
      type(MAPL_StateItem_Flag), optional, intent(in) :: variant
      type(ItemShapeCharacteristic) :: characteristic

      characteristic%item_class = item_class
      if (present(variant)) characteristic%variant = variant
      call characteristic%set_status(CHARACTERISTIC_STATUS_SPECIFIED)
   end function new_ItemShapeCharacteristic

   function item_shape_get_kind() result(kind)
      type(StateItemCharacteristicKind) :: kind

      kind = ITEM_SHAPE
   end function item_shape_get_kind

   function get_item_class(this) result(item_class)
      class(ItemShapeCharacteristic), intent(in) :: this
      type(ESMF_StateItem_Flag) :: item_class

      item_class = this%item_class
   end function get_item_class

   ! The goal is always an ItemShapeCharacteristic in production: both sides
   ! are looked up under the same kind key, and set_characteristic asserts the
   ! key matches the entry's kind.
   logical function item_shape_needs_extension_for(this, goal) &
        result(needs_extension)
      class(ItemShapeCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal

      select type (goal)
      class is (ItemShapeCharacteristic)
         needs_extension = &
              shape_outcome(this%item_class, goal%item_class) /= SHAPE_MATCH
      class default
         error stop 'ItemShapeCharacteristic: needs_extension_for ' // &
              'called with a non-ItemShapeCharacteristic goal'
      end select
   end function item_shape_needs_extension_for

   subroutine item_shape_build_transform(this, goal, transformer, rc)
      class(ItemShapeCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal
      class(TransformBase), allocatable, intent(out) :: transformer
      integer, optional, intent(out) :: rc

      select type (goal)
      class is (ItemShapeCharacteristic)
         select case (shape_outcome(this%item_class, goal%item_class))
         case (SHAPE_CONVERT)
            allocate(transformer, source=BracketTimeInterpolationTransform())
         case (SHAPE_INCOMPATIBLE)
            call report_build_transform_incompatible(rc)
            return
         case default
            call report_build_transform_unsupported(rc)
            return
         end select
      class default
         _FAIL('ItemShapeCharacteristic: build_transform: goal is not a shape')
      end select

      _RETURN(_SUCCESS)
   end subroutine item_shape_build_transform

   ! Shape matrix (export class -> import class). A WILDCARD import is matched
   ! by FIELD, FIELDBUNDLE and STATE exports, as in legacy; its members are
   ! resolved elsewhere.
   integer function shape_outcome(export_class, import_class) result(outcome)
      type(ESMF_StateItem_Flag), intent(in) :: export_class
      type(ESMF_StateItem_Flag), intent(in) :: import_class

      outcome = SHAPE_INCOMPATIBLE

      select case (export_class%ot)
      case (MAPL_STATEITEM_FIELD%ot, MAPL_STATEITEM_FIELDBUNDLE%ot, &
           MAPL_STATEITEM_STATE%ot)
         if (import_class == export_class) outcome = SHAPE_MATCH
         if (import_class == MAPL_STATEITEM_WILDCARD) outcome = SHAPE_MATCH
      case (MAPL_STATEITEM_VECTOR%ot, MAPL_STATEITEM_SERVICE%ot)
         if (import_class == export_class) outcome = SHAPE_MATCH
      case (MAPL_STATEITEM_BRACKET%ot)
         if (import_class == MAPL_STATEITEM_FIELD) outcome = SHAPE_CONVERT
      case (MAPL_STATEITEM_EXPRESSION%ot)
         ! The expression hook evaluates into the export node itself, so the
         ! consumer reads it like a field.
         if (import_class == MAPL_STATEITEM_FIELD) outcome = SHAPE_MATCH
      case (MAPL_STATEITEM_VECTORBRACKET%ot)
         ! No vector time-interpolation step yet.
         if (import_class == MAPL_STATEITEM_VECTOR) outcome = SHAPE_UNSUPPORTED
      end select
   end function shape_outcome

   ! Kinds in the order this shape reconciles them. `goal` is the import's
   ! characteristics; no shape needs it yet.
   function get_extension_order(this, goal, rc) result(order)
      class(ItemShapeCharacteristic), intent(in) :: this
      type(StateItemCharacteristicMap), intent(in) :: goal
      integer, optional, intent(out) :: rc
      type(StateItemCharacteristicKind), allocatable :: order(:)

      type(StateItemCharacteristicKind), parameter :: GEOM(*) = &
           [HORIZONTAL_DIMS_SPEC, GEOMETRY]
      type(StateItemCharacteristicKind), parameter :: VGRID(*) = &
           [VERTICAL_GRID, VERTICAL_STAGGER, VERTICAL_ORIENTATION]
      type(StateItemCharacteristicKind), parameter :: FIELD_HEAD(*) = &
           [ITEM_SHAPE, ATTRIBUTES, UNGRIDDED_DIMS, QUANTITY_TYPE, &
           CONSERVATION, GEOM, VGRID, NORMALIZATION, PHYSICAL_UNITS]
      type(StateItemCharacteristicKind), parameter :: BRACKET_ORDER(*) = &
           [ATTRIBUTES, UNGRIDDED_DIMS, QUANTITY_TYPE, GEOM, ITEM_SHAPE]

      allocate(order(0))

      if (allocated(this%variant)) then
         if (this%variant == VARIANT_GEOM) then
            order = [GEOMETRY]
            _RETURN(_SUCCESS)
         end if
         if (this%variant == VARIANT_VERTICALGRID) then
            order = [VERTICAL_GRID]
            _RETURN(_SUCCESS)
         end if
      end if

      select case (this%item_class%ot)
      case (MAPL_STATEITEM_FIELD%ot)
         order = [FIELD_HEAD, STANDARD_NAME, TYPE_KIND]
      case (MAPL_STATEITEM_FIELDBUNDLE%ot, MAPL_STATEITEM_VECTOR%ot)
         order = [FIELD_HEAD, TYPE_KIND]
      case (MAPL_STATEITEM_BRACKET%ot, MAPL_STATEITEM_VECTORBRACKET%ot)
         order = BRACKET_ORDER
      case (MAPL_STATEITEM_EXPRESSION%ot)
         order = [GEOMETRY, VGRID, TYPE_KIND, ITEM_SHAPE]
      case (MAPL_STATEITEM_SERVICE%ot)
         order = [ITEM_SHAPE, GEOMETRY]
      end select

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(goal)
   end function get_extension_order

   ! Writes the declared variant only: an EXPRESSION or a BRACKET->FIELD node
   ! is materialized as a plain Field and must not carry its declared class.
   ! A bundle's fieldBundleType is a creation input (STRUCTURAL only).
   subroutine item_shape_write_info(this, handle, phase, rc)
      class(ItemShapeCharacteristic), intent(in) :: this
      type(GraphStateItem), intent(inout) :: handle
      integer, intent(in) :: phase
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_StateItem_Flag) :: handle_type
      type(ESMF_Field) :: field
      type(ESMF_FieldBundle) :: bundle
      type(ESMF_State) :: state
      type(MAPL_FieldBundleType_Flag), allocatable :: bundle_type

      handle_type = handle%itemType(_RC)

      if (handle_type == ESMF_STATEITEM_FIELD) then
         _RETURN_UNLESS(allocated(this%variant))
         field = handle%get_field(_RC)
         call MAPL_FieldSet(field, state_item_variant=this%variant, _RC)
      else if (handle_type == ESMF_STATEITEM_FIELDBUNDLE) then
         bundle = handle%get_field_bundle(_RC)
         if (phase == SYNC_PHASE_STRUCTURAL) then
            call bundle_type_of(this, bundle_type)
         end if
         if (allocated(bundle_type)) then
            call MAPL_FieldBundleSet(bundle, fieldBundleType=bundle_type, _RC)
         end if
         if (allocated(this%variant)) then
            call MAPL_FieldBundleSet(bundle, state_item_variant=this%variant, &
                 _RC)
         end if
      else if (handle_type == ESMF_STATEITEM_STATE) then
         _RETURN_UNLESS(allocated(this%variant))
         state = handle%get_state(_RC)
         call MAPL_StateSet(state, state_item_variant=this%variant, _RC)
      else
         _FAIL('ItemShapeCharacteristic: write_info needs a payload handle')
      end if

      _RETURN(_SUCCESS)
   end subroutine item_shape_write_info

   ! The fieldBundleType of a bundle-backed shape; left unallocated for the
   ! rest.
   subroutine bundle_type_of(this, bundle_type)
      class(ItemShapeCharacteristic), intent(in) :: this
      type(MAPL_FieldBundleType_Flag), allocatable, intent(out) :: bundle_type

      select case (this%item_class%ot)
      case (MAPL_STATEITEM_BRACKET%ot)
         bundle_type = MAPL_FIELDBUNDLETYPE_BRACKET
      case (MAPL_STATEITEM_VECTOR%ot)
         bundle_type = MAPL_FIELDBUNDLETYPE_VECTOR
      case (MAPL_STATEITEM_VECTORBRACKET%ot)
         bundle_type = MAPL_FIELDBUNDLETYPE_VECTORBRACKET
      case (MAPL_STATEITEM_SERVICE%ot)
         bundle_type = MAPL_FIELDBUNDLETYPE_SERVICE
      end select
   end subroutine bundle_type_of

   function item_shape_get_signature(this) result(signature)
      class(ItemShapeCharacteristic), intent(in) :: this
      character(:), allocatable :: signature

      signature = 'item_class=' // item_class_name(this%item_class)
      if (allocated(this%variant)) then
         signature = signature // ',variant=' // this%variant%to_string()
      end if
   end function item_shape_get_signature

   ! ESMF_StateItem_Flag has no to_string; mirrors GraphBuilder's
   ! itemtype_name, which this module cannot reach (it sits below GraphBuilder).
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
