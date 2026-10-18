#include "MAPL.h"

!------------------------------------------------------------------------------
! TypeKindCharacteristic: the "type/kind (precision)" StateItemCharacteristic
! (docs/graph/spec/18-state-item-characteristics.md REQ-CHAR-001 table) -
! a ValueCharacteristic (REQ-CHAR-002a: holds its value inline, never
! shared). Holds a plain label (e.g. 'R4'/'R8') rather than an ESMF
! typekind handle directly.
!
! openspec/changes/merge-characteristic-families: build_transform/
! get_signature moved from the now-retired
! mapl_TypeKindCharacteristicAdapter_mod's own adapter_build_transform/
! adapter_get_signature (that module's own real adaptation provider,
! previously bridged onto this type by composition purely to reach
! Characteristic's own build_transform interface) - the
! label_to_typekind/typekind_to_label helper pair moves here with it,
! since no other module needs them once the adapter is removed.
!------------------------------------------------------------------------------
module mapl_TypeKindCharacteristic_mod
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, ValueCharacteristic
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind, TYPE_KIND_CHARACTERISTIC_KIND
   use mapl_TransformBase_mod, only: TransformBase
   use mapl_TypeKindConverterTransform_mod, only: TypeKindConverterTransform
   use ESMF, only: ESMF_TypeKind_Flag, ESMF_TYPEKIND_R4, ESMF_TYPEKIND_R8, &
        ESMF_TYPEKIND_I4, ESMF_TYPEKIND_I8, ESMF_TYPEKIND_LOGICAL
   use ESMF, only: operator(==)
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: TypeKindCharacteristic
   ! openspec/changes/graph-builder-stateitem-resolution: exposed so a
   ! materialization call site (GraphBuilder.F90's
   ! try_materialize_field_extension/materialize_extension_chain) can
   ! recover the real ESMF_TypeKind_Flag this characteristic's own label
   ! was converted from, once that call site no longer has a
   ! VariableSpec reference to read %typekind off directly (proposal.md
   ! "nothing downstream should need to walk back to the originating
   ! VariableSpec") - the inverse of new_TypeKindCharacteristic_flag's
   ! own conversion, previously private to this module.
   public :: label_to_typekind

   type, extends(ValueCharacteristic) :: TypeKindCharacteristic
      private
      character(:), allocatable :: type_kind
   contains
      procedure, nopass :: get_kind => typekind_get_kind
      procedure :: needs_extension_for => typekind_needs_extension_for
      procedure :: build_transform => typekind_build_transform
      procedure :: get_signature => typekind_get_signature
      procedure :: get_type_kind
   end type TypeKindCharacteristic

   interface TypeKindCharacteristic
      module procedure new_TypeKindCharacteristic
      module procedure new_TypeKindCharacteristic_flag
   end interface TypeKindCharacteristic

contains

   function new_TypeKindCharacteristic(type_kind) result(characteristic)
      character(*), intent(in) :: type_kind
      type(TypeKindCharacteristic) :: characteristic

      ! openspec/changes/graph-builder-stateitem-resolution: explicit
      ! allocate(..., source=...) instead of plain `=` assignment into
      ! a deferred-length character component - same gfortran
      ! realloc-on-assignment memory-corruption class documented in
      ! StateItemNode.F90's constructor.
      allocate(characteristic%type_kind, source=type_kind)
   end function new_TypeKindCharacteristic

   ! Convenience form - real ESMF_TypeKind_Flag, converted to the plain
   ! label internally (moved from the retired
   ! mapl_TypeKindCharacteristicAdapter_mod's own
   ! new_TypeKindCharacteristicAdapter_flag) - GraphBuilder.F90's
   ! build_characteristics() uses this form directly, since
   ! VariableSpec%typekind is a real ESMF_TypeKind_Flag.
   function new_TypeKindCharacteristic_flag(type_kind) result(characteristic)
      type(ESMF_TypeKind_Flag), intent(in) :: type_kind
      type(TypeKindCharacteristic) :: characteristic

      ! Same allocate(..., source=...) fix as new_TypeKindCharacteristic
      ! above, same reason - typekind_to_label() returns a
      ! character(:), allocatable function result.
      allocate(characteristic%type_kind, source=typekind_to_label(type_kind))
   end function new_TypeKindCharacteristic_flag

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

      ! Same allocate(..., source=...) fix as new_TypeKindCharacteristic
      ! above, same reason - this%type_kind is itself a character(:),
      ! allocatable component.
      allocate(type_kind, source=this%type_kind)
   end function get_type_kind

   ! Moved from the retired mapl_TypeKindCharacteristicAdapter_mod's own
   ! adapter_build_transform, reading through get_type_kind() (already
   ! existed) in place of the original's inner-component access.
   subroutine typekind_build_transform(this, goal, transformer, rc)
      class(TypeKindCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal
      class(TransformBase), allocatable, intent(out) :: transformer
      integer, optional, intent(out) :: rc

      select type (goal)
      class is (TypeKindCharacteristic)
         allocate(transformer, source=TypeKindConverterTransform( &
              label_to_typekind(this%get_type_kind()), label_to_typekind(goal%get_type_kind())))
      class default
         _FAIL('TypeKindCharacteristic: build_transform called with a non-typekind goal')
      end select

      _RETURN(_SUCCESS)
   end subroutine typekind_build_transform

   ! Moved from the retired mapl_TypeKindCharacteristicAdapter_mod's own
   ! adapter_get_signature.
   function typekind_get_signature(this) result(signature)
      class(TypeKindCharacteristic), intent(in) :: this
      character(:), allocatable :: signature

      signature = 'typekind=' // this%get_type_kind()
   end function typekind_get_signature

   ! Moved from the retired mapl_TypeKindCharacteristicAdapter_mod - no
   ! other module needs these once the adapter is removed (confirmed by
   ! grep, task 6.1).
   !
   ! openspec/changes/graph-builder-stateitem-resolution: extended
   ! beyond R4/R8 (I4/I8/LOGICAL, mirroring mapl_FieldInfo_mod's own
   ! to_string/to_typekind precedent exactly - infrastructure/field/
   ! FieldInfo.F90) - defensive broadening to the full set of
   ! ESMF_TypeKind_Flag values MAPL fields actually use, now that
   ! advertise_one calls this unconditionally for every advertised
   ! item (task 2.2), not only ones actually reached through a
   ! MatchConnection. MAPL_TYPEKIND_MIRROR (infrastructure/esmf/
   ! TypeKind.F90, a declared "inherit the matched export's kind"
   ! placeholder - not a real ESMF_TypeKind_Flag at all) is handled
   ! separately, by not calling this function in the first place
   ! (GraphBuilder_Advertising_smod.F90's populate_characteristics,
   ! design.md correction) - that is the actual case this change's own
   ! implementation surfaced (StatisticsGridComp.F90's mirrored
   ! imports), not a missing I4/I8/LOGICAL label as initially
   ! suspected.
   function typekind_to_label(type_kind) result(label)
      type(ESMF_TypeKind_Flag), intent(in) :: type_kind
      character(:), allocatable :: label

      if (type_kind == ESMF_TYPEKIND_R4) then
         label = 'R4'
      else if (type_kind == ESMF_TYPEKIND_R8) then
         label = 'R8'
      else if (type_kind == ESMF_TYPEKIND_I4) then
         label = 'I4'
      else if (type_kind == ESMF_TYPEKIND_I8) then
         label = 'I8'
      else if (type_kind == ESMF_TYPEKIND_LOGICAL) then
         label = 'LOGICAL'
      else
         error stop 'TypeKindCharacteristic: unsupported ESMF_TypeKind_Flag'
      end if
   end function typekind_to_label

   function label_to_typekind(label) result(type_kind)
      character(*), intent(in) :: label
      type(ESMF_TypeKind_Flag) :: type_kind

      if (label == 'R4') then
         type_kind = ESMF_TYPEKIND_R4
      else if (label == 'R8') then
         type_kind = ESMF_TYPEKIND_R8
      else if (label == 'I4') then
         type_kind = ESMF_TYPEKIND_I4
      else if (label == 'I8') then
         type_kind = ESMF_TYPEKIND_I8
      else if (label == 'LOGICAL') then
         type_kind = ESMF_TYPEKIND_LOGICAL
      else
         error stop 'TypeKindCharacteristic: unsupported type-kind label'
      end if
   end function label_to_typekind

end module mapl_TypeKindCharacteristic_mod
