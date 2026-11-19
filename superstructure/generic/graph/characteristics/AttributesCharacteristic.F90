#include "MAPL.h"

!------------------------------------------------------------------------------
! AttributesCharacteristic: the "attributes" StateItemCharacteristic
! ValueCharacteristic storing a StringVector of attribute names, ported from
! legacy AttributesAspect (superstructure/generic/specs/AttributesAspect.F90).
!
! ASYMMETRIC COMPARISON - the one asymmetric characteristic in this
! hierarchy. Every other characteristic's needs_extension_for(this, goal) is
! symmetric (needs_extension_for(a,b) == needs_extension_for(b,a)). This one
! is NOT: it ports AttributesAspect%matches()'s own documented rule
! verbatim - "we require that an export provides all attributes that an
! import specifies as a shared attribute" - so `this` is read as the
! provides-side (export) and `goal` as the requires-side (import):
! needs_extension_for(this, goal) is .false. iff every name in goal's set is
! present in this's set. `this` having extra names goal does not require is
! fine; `this` missing a name goal requires is not. Swapping this/goal for
! the same two non-equal sets can therefore produce the opposite result -
! callers must pass provides-side as `this`, requires-side as `goal`, same
! direction convention as legacy's own matches(src, dst).
!
! A `build_transform` is required by the common interface but reports
! incompatible here: legacy AttributesAspect never converts a mismatch
! (supports_conversion_* are false), so no Transform can ever exist.
! `write_info` writes nothing: attributes have no Info key (legacy
! AttributesAspect%update_payload is a no-op), so no setter argument exists.
! `get_signature` derives a stable, value-distinguishing string from
! `attribute_names` (sorted, comma-joined) for
! mapl_ExtensionResolution_mod's reuse-search cache key.
!------------------------------------------------------------------------------
module mapl_AttributesCharacteristic_mod
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, ValueCharacteristic
   use mapl_StateItemCharacteristic_mod, only: report_build_transform_incompatible
   use mapl_CharacteristicStatus_mod, only: CHARACTERISTIC_STATUS_SPECIFIED
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind, ATTRIBUTES_CHARACTERISTIC_KIND
   use gFTL2_StringVector, only: StringVector
   use mapl_TransformBase_mod, only: TransformBase
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_StringSortUtilities_mod, only: sorted_names
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: AttributesCharacteristic

   type, extends(ValueCharacteristic) :: AttributesCharacteristic
      private
      type(StringVector) :: attribute_names
   contains
      procedure, nopass :: get_kind => attributes_get_kind
      procedure :: needs_extension_for => attributes_needs_extension_for
      procedure :: build_transform => attributes_build_transform
      procedure :: get_signature => attributes_get_signature
      procedure :: write_info => attributes_write_info
      procedure :: get_attribute_names
   end type AttributesCharacteristic

   interface AttributesCharacteristic
      module procedure new_AttributesCharacteristic
   end interface AttributesCharacteristic

contains

   function new_AttributesCharacteristic(attribute_names) result(characteristic)
      type(StringVector), optional, intent(in) :: attribute_names
      type(AttributesCharacteristic) :: characteristic

      if (present(attribute_names)) characteristic%attribute_names = attribute_names
      call characteristic%set_status(CHARACTERISTIC_STATUS_SPECIFIED)
   end function new_AttributesCharacteristic

   function attributes_get_kind() result(kind)
      type(StateItemCharacteristicKind) :: kind

      kind = ATTRIBUTES_CHARACTERISTIC_KIND
   end function attributes_get_kind

   ! ASYMMETRIC (see module header comment): `this` = provides
   ! (export) side, `goal` = requires (import) side. `goal` is guaranteed
   ! an AttributesCharacteristic for the real call chain this method is
   ! used in (same guarantee documented on GeometryCharacteristic/
   ! PhysicalUnitsCharacteristic).
   logical function attributes_needs_extension_for(this, goal) result(needs_extension)
      class(AttributesCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal

      select type (goal)
      class is (AttributesCharacteristic)
         needs_extension = .not. includes(this%attribute_names, goal%attribute_names)
      class default
         error stop 'AttributesCharacteristic: needs_extension_for called with a non-AttributesCharacteristic goal - should be checked by calling procedure'
      end select
   end function attributes_needs_extension_for

   ! Mirrors AttributesAspect.F90's own `includes` helper exactly: every
   ! name in `mandatory_names` must be present in `provided_names`.
   logical function includes(provided_names, mandatory_names)
      type(StringVector), target, intent(in) :: provided_names
      type(StringVector), target, intent(in) :: mandatory_names

      integer :: i, j
      character(:), pointer :: mandatory_name

      includes = .true.
      m: do i = 1, mandatory_names%size()
         mandatory_name => mandatory_names%of(i)
         do j = 1, provided_names%size()
            if (mandatory_name == provided_names%of(j)) cycle m
         end do
         includes = .false.
         return
      end do m
   end function includes

   function get_attribute_names(this) result(attribute_names)
      class(AttributesCharacteristic), intent(in) :: this
      type(StringVector) :: attribute_names

      attribute_names = this%attribute_names
   end function get_attribute_names

   ! A mismatch can never be adapted (module header): reports incompatible.
   subroutine attributes_build_transform(this, goal, transformer, rc)
      class(AttributesCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal
      class(TransformBase), allocatable, intent(out) :: transformer
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(goal)

      call report_build_transform_incompatible(rc)
   end subroutine attributes_build_transform

   ! Attributes have no Info representation, so there is nothing to write.
   subroutine attributes_write_info(this, handle, phase, rc)
      class(AttributesCharacteristic), intent(in) :: this
      type(GraphStateItem), intent(inout) :: handle
      integer, intent(in) :: phase
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(handle)
      _UNUSED_DUMMY(phase)

      _RETURN(_SUCCESS)
   end subroutine attributes_write_info

   ! A stable, value-distinguishing signature derived from this subclass's
   ! own stored value: a sorted, comma-joined list of attribute_names.
   function attributes_get_signature(this) result(signature)
      class(AttributesCharacteristic), intent(in) :: this
      character(:), allocatable :: signature

      signature = 'attributes=' // join(sorted_names(this%attribute_names))
   end function attributes_get_signature

   function join(names) result(joined)
      character(*), intent(in) :: names(:)
      character(:), allocatable :: joined

      integer :: i, n

      n = size(names)
      joined = ''
      do i = 1, n
         if (i > 1) joined = joined // ','
         joined = joined // trim(names(i))
      end do
   end function join

end module mapl_AttributesCharacteristic_mod
