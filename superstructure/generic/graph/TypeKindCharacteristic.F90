#include "MAPL.h"

!------------------------------------------------------------------------------
! TypeKindCharacteristic: the "type/kind (precision)" StateItemCharacteristic
! (docs/graph/spec/18-state-item-characteristics.md REQ-CHAR-001 table) -
! a ValueCharacteristic (REQ-CHAR-002a: holds its value inline, never
! shared). Holds a plain label (e.g. 'R4'/'R8') rather than an ESMF
! typekind handle directly - sufficient to *describe* and *detect* a
! mismatch (REQ-CHAR-002: description, not adaptation); the associated
! Transform (REQ-CHAR-001's table calls out a placeholder name,
! "CopyTransform", explicitly flagged poor and not final) has no real
! implementation anywhere yet, in this capability or graph/extension-reuse
! - a mismatch on this characteristic fails explicitly rather than being
! silently coerced (openspec/changes/state-item-characteristics design.md
! Non-Goals).
!------------------------------------------------------------------------------
module mapl_TypeKindCharacteristic_mod
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, ValueCharacteristic
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind, TYPE_KIND_CHARACTERISTIC_KIND
   implicit none(type, external)
   private

   public :: TypeKindCharacteristic

   type, extends(ValueCharacteristic) :: TypeKindCharacteristic
      private
      character(:), allocatable :: type_kind
   contains
      procedure, nopass :: get_kind => typekind_get_kind
      procedure :: needs_extension_for => typekind_needs_extension_for
      procedure :: get_type_kind
   end type TypeKindCharacteristic

   interface TypeKindCharacteristic
      module procedure new_TypeKindCharacteristic
   end interface TypeKindCharacteristic

contains

   function new_TypeKindCharacteristic(type_kind) result(characteristic)
      character(*), intent(in) :: type_kind
      type(TypeKindCharacteristic) :: characteristic

      characteristic%type_kind = type_kind
   end function new_TypeKindCharacteristic

   function typekind_get_kind() result(kind)
      type(StateItemCharacteristicKind) :: kind

      kind = TYPE_KIND_CHARACTERISTIC_KIND
   end function typekind_get_kind

   ! `goal` is guaranteed a TypeKindCharacteristic for the real call
   ! chain this method is used in: the one production caller,
   ! find_mismatched_state_item_characteristics, looks both sides up by
   ! the same StateItemCharacteristicKind key, and
   ! GraphStateItem%set_characteristic (the only insertion path into that
   ! map) asserts a characteristic's own kind matches its map key before
   ! inserting (REQ-CHAR-006/009, design.md D9) - so the class default
   ! branch below should be checked by the calling procedure, matching
   ! this codebase's own established `error stop` precedent for exactly
   ! that situation (DependencyNetwork.F90, ActualConnectionPt.F90).
   logical function typekind_needs_extension_for(this, goal) result(needs_extension)
      class(TypeKindCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal

      select type (goal)
      class is (TypeKindCharacteristic)
         needs_extension = (this%type_kind /= goal%type_kind)
      class default
         error stop 'TypeKindCharacteristic: needs_extension_for called with a non-TypeKindCharacteristic goal - should be checked by calling procedure'
      end select
   end function typekind_needs_extension_for

   function get_type_kind(this) result(type_kind)
      class(TypeKindCharacteristic), intent(in) :: this
      character(:), allocatable :: type_kind

      type_kind = this%type_kind
   end function get_type_kind

end module mapl_TypeKindCharacteristic_mod
