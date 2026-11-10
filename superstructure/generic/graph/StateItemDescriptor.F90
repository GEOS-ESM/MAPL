#include "MAPL.h"

!------------------------------------------------------------------------------
! StateItemDescriptor: the description of a state item, kept apart from the
! ESMF handle (GraphStateItem). Holds the sparse StateItemCharacteristicMap
! plus the description values that are not yet characteristics (declared
! item type).
!
! Independent of any ESMF object: a StateItemNode's descriptor is populated at
! ADVERTISE, before any handle exists. Layering: graph/ only; must not depend
! on OuterMetaComponent, StateRegistry, or VariableSpec.
!------------------------------------------------------------------------------
module mapl_StateItemDescriptor_mod
   use ESMF, only: ESMF_StateItem_Flag, ESMF_STATEITEM_FIELD, operator(==)
   use ESMF, only: ESMF_KIND_R4
   use mapl_enums_api, only: MAPL_VectorBasisKind
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, StateItemCharacteristicMap, &
        StateItemCharacteristicMapIterator, operator(/=), &
        SYNC_PHASE_STRUCTURAL, SYNC_PHASE_DESCRIPTIVE, SYNC_PHASE_ADVERTISED
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_CharacteristicDefaults_mod, only: CharacteristicDefaults
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind, operator(==), &
        GEOMETRY_CHARACTERISTIC_KIND, VERTICAL_GRID_CHARACTERISTIC_KIND, TYPE_KIND_CHARACTERISTIC_KIND, &
        VERTICAL_STAGGER_CHARACTERISTIC_KIND, VERTICAL_ORIENTATION_CHARACTERISTIC_KIND
   use mapl_CharacteristicStatus_mod, only: CharacteristicStatus, operator(==), &
        CHARACTERISTIC_STATUS_SPECIFIED, CHARACTERISTIC_STATUS_MIRRORED, &
        CHARACTERISTIC_STATUS_UNCHECKED, CHARACTERISTIC_STATUS_DEFERRED
   use mapl_StateItemCharacteristicKind_mod, only: &
        ITEM_SHAPE_CHARACTERISTIC_KIND, EXPRESSION_CHARACTERISTIC_KIND, &
        HORIZONTAL_DIMS_SPEC_CHARACTERISTIC_KIND
   use mapl_ItemShapeCharacteristic_mod, only: ItemShapeCharacteristic
   use mapl_StateItem_mod, only: MAPL_STATEITEM_EXPRESSION
   use mapl_ExpressionCharacteristic_mod, only: ExpressionCharacteristic
   use mapl_HorizontalDimsSpecCharacteristic_mod, only: HorizontalDimsSpecCharacteristic
   use mapl_HorizontalDimsSpec_mod, only: HORIZONTAL_DIMS_GEOM
   use gFTL2_StringVector, only: StringVector
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: StateItemDescriptor
   public :: resolve_pair
   public :: resolve_characteristics
   public :: resolve_descriptors
   public :: is_mirrored_export_allowed
   public :: PAIR_MATCH, PAIR_EXTEND, PAIR_ADOPT, PAIR_CONFLICT, PAIR_INVALID, PAIR_WAIT

   ! Outcomes of resolve_pair for one kind present on both sides.
   ! PAIR_ADOPT: the import takes a copy of the export's value.
   ! PAIR_INVALID: an INVALID status reached resolution (programming error).
   ! PAIR_WAIT: a side is still DEFERRED; the component fills it in a later
   ! phase, so the pair is neither wired nor an error yet.
   integer, parameter :: PAIR_MATCH = 1
   integer, parameter :: PAIR_EXTEND = 2
   integer, parameter :: PAIR_ADOPT = 3
   integer, parameter :: PAIR_CONFLICT = 4
   integer, parameter :: PAIR_INVALID = 5
   integer, parameter :: PAIR_WAIT = 6

   ! (export item type, kind) pairs whose export may be MIRRORED. An
   ! expression has no geometry or precision of its own; each consumer's
   ! chain tip takes the consumer's value. Add entries only with a
   ! demonstrated use case.
   ! declared_wins: a SPECIFIED value declared on the export beats the
   ! consumer's. Precision is always the consumer's.
   type :: MirroredExportAllowance
      type(ESMF_StateItem_Flag) :: item_type
      type(StateItemCharacteristicKind) :: kind
      logical :: declared_wins
   end type MirroredExportAllowance

   type(MirroredExportAllowance), parameter :: ALLOWLIST(*) = [ &
           MirroredExportAllowance(MAPL_STATEITEM_EXPRESSION, &
                GEOMETRY_CHARACTERISTIC_KIND, .true.), &
           MirroredExportAllowance(MAPL_STATEITEM_EXPRESSION, &
                VERTICAL_GRID_CHARACTERISTIC_KIND, .true.), &
           MirroredExportAllowance(MAPL_STATEITEM_EXPRESSION, &
                VERTICAL_STAGGER_CHARACTERISTIC_KIND, .true.), &
           MirroredExportAllowance(MAPL_STATEITEM_EXPRESSION, &
                VERTICAL_ORIENTATION_CHARACTERISTIC_KIND, .true.), &
           MirroredExportAllowance(MAPL_STATEITEM_EXPRESSION, &
                TYPE_KIND_CHARACTERISTIC_KIND, .false.)]

   type :: StateItemDescriptor
      private
      type(StateItemCharacteristicMap) :: characteristics
      ! Optional or defaulted values: "not declared" is meaningful.
      type(ESMF_StateItem_Flag) :: declared_item_type = ESMF_STATEITEM_FIELD
      ! Not a characteristic: never compared or mirrored. Materialization
      ! fills the created field with it (the export's or internal's own).
      real(ESMF_KIND_R4), allocatable :: fill_value
      ! Not characteristics: never compared or mirrored. They only shape the
      ! members of a VECTOR or BRACKET export the graph creates.
      integer, allocatable :: bracket_size
      type(MAPL_VectorBasisKind), allocatable :: vector_basis_kind
      character(:), allocatable :: vector_component_name_1
      character(:), allocatable :: vector_component_name_2
   contains
      procedure :: has_characteristic => descriptor_has_characteristic
      procedure :: get_characteristic => descriptor_get_characteristic
      procedure :: set_characteristic => descriptor_set_characteristic
      procedure :: get_characteristics => descriptor_get_characteristics
      procedure :: get_kinds => descriptor_get_kinds
      procedure :: get_unresolved_kinds => descriptor_get_unresolved_kinds
      procedure :: get_incomplete_kinds => descriptor_get_incomplete_kinds
      procedure :: harvest => descriptor_harvest
      procedure :: check_resolved => descriptor_check_resolved
      procedure :: resolve_deferred => descriptor_resolve_deferred
      procedure :: drop_unsupplied => descriptor_drop_unsupplied
      procedure :: fallback_unsupplied_deferred => &
           descriptor_fallback_unsupplied_deferred
      procedure :: sync_to_info => descriptor_sync_to_info
      procedure :: sync_per_alias_to_info => descriptor_sync_per_alias_to_info
      procedure :: get_declared_item_type => descriptor_get_declared_item_type
      procedure :: set_declared_item_type => descriptor_set_declared_item_type
      procedure :: take_declared_item_type_from_shape => &
           descriptor_take_declared_item_type_from_shape
      procedure :: set_fill_value => descriptor_set_fill_value
      procedure :: get_fill_value => descriptor_get_fill_value
      procedure :: set_bracket_size => descriptor_set_bracket_size
      procedure :: get_bracket_size => descriptor_get_bracket_size
      procedure :: set_vector_basis_kind => descriptor_set_vector_basis_kind
      procedure :: get_vector_basis_kind => descriptor_get_vector_basis_kind
      procedure :: set_vector_component_names => &
           descriptor_set_vector_component_names
      procedure :: get_vector_component_names => &
           descriptor_get_vector_component_names
      procedure :: get_expression => descriptor_get_expression
      procedure :: add_expression_result_defaults => &
           descriptor_add_expression_result_defaults
      procedure :: get_consumer_adopted_kinds => &
           descriptor_get_consumer_adopted_kinds
   end type StateItemDescriptor

contains

   ! The kinds an export of this item type takes from each consumer
   ! (the MIRRORED-export allowlist entries for the declared item type).
   ! declared_wins(i): kinds(i) keeps a SPECIFIED value declared on this
   ! item over the consumer's.
   subroutine descriptor_get_consumer_adopted_kinds(this, kinds, rc, &
        declared_wins)
      class(StateItemDescriptor), intent(in) :: this
      type(StateItemCharacteristicKind), allocatable, intent(out) :: kinds(:)
      integer, optional, intent(out) :: rc
      logical, allocatable, optional, intent(out) :: declared_wins(:)

      integer :: i
      logical, allocatable :: wins(:)

      allocate(kinds(0), wins(0))
      do i = 1, size(ALLOWLIST)
         if (ALLOWLIST(i)%item_type == this%get_declared_item_type()) then
            kinds = [kinds, ALLOWLIST(i)%kind]
            wins = [wins, ALLOWLIST(i)%declared_wins]
         end if
      end do
      if (present(declared_wins)) call move_alloc(wins, declared_wins)
      _RETURN(_SUCCESS)
   end subroutine descriptor_get_consumer_adopted_kinds

   ! -- characteristics --

   logical function descriptor_has_characteristic(this, kind) result(has)
      class(StateItemDescriptor), intent(in) :: this
      type(StateItemCharacteristicKind), intent(in) :: kind

      has = this%characteristics%count(kind) > 0
   end function descriptor_has_characteristic

   ! An absent key means "never established" and fails
   ! explicitly rather than returning an INVALID-status placeholder. A
   ! subroutine with an intent(out) allocatable dummy, not a function returning
   ! a polymorphic allocatable: an unallocated polymorphic function result on
   ! the failure path is not reliably safe for the caller to assign.
   subroutine descriptor_get_characteristic(this, kind, characteristic, rc)
      class(StateItemDescriptor), target, intent(in) :: this
      type(StateItemCharacteristicKind), intent(in) :: kind
      class(StateItemCharacteristic), allocatable, intent(out) :: characteristic
      integer, optional, intent(out) :: rc

      class(StateItemCharacteristic), pointer :: found
      integer :: status

      found => this%characteristics%at(kind, rc=status)
      _ASSERT(status == _SUCCESS, 'StateItemDescriptor: get_characteristic - no characteristic established for this kind')
      allocate(characteristic, source=found)

      _RETURN(_SUCCESS)
   end subroutine descriptor_get_characteristic

   ! Inserts or replaces the entry under `kind`; the one explicit-replace
   ! operation. characteristic%get_kind() MUST equal `kind`,
   ! because detection pairs two items' entries by key and assumes both dynamic
   ! types match.
   subroutine descriptor_set_characteristic(this, kind, characteristic, rc)
      class(StateItemDescriptor), intent(inout) :: this
      type(StateItemCharacteristicKind), intent(in) :: kind
      class(StateItemCharacteristic), intent(in) :: characteristic
      integer, optional, intent(out) :: rc

      integer :: n_erased

      _ASSERT(characteristic%get_kind() == kind, 'StateItemDescriptor: set_characteristic - characteristic%get_kind() does not match kind')

      n_erased = int(this%characteristics%erase(kind))
      call this%characteristics%insert(kind, characteristic)

      _RETURN(_SUCCESS)
   end subroutine descriptor_set_characteristic

   ! Enumerable so detection can iterate every entry. Returns a
   ! copy.
   function descriptor_get_characteristics(this) result(characteristics)
      class(StateItemDescriptor), intent(in) :: this
      type(StateItemCharacteristicMap) :: characteristics

      characteristics = this%characteristics
   end function descriptor_get_characteristics

   ! The kinds established on this item, read in place (no map copy).
   subroutine descriptor_get_kinds(this, kinds)
      class(StateItemDescriptor), target, intent(in) :: this
      type(StateItemCharacteristicKind), allocatable, intent(out) :: kinds(:)

      type(StateItemCharacteristicMapIterator) :: iter

      allocate(kinds(0))
      associate (e => this%characteristics%ftn_end())
         iter = this%characteristics%ftn_begin()
         do while (iter /= e)
            call iter%next()
            kinds = [kinds, iter%first()]
         end do
      end associate
   end subroutine descriptor_get_kinds

   ! -- generic status resolution --

   logical function is_mirrored_export_allowed(export_item_type, kind) result(allowed)
      type(ESMF_StateItem_Flag), intent(in) :: export_item_type
      type(StateItemCharacteristicKind), intent(in) :: kind

      integer :: i

      allowed = .false.
      do i = 1, size(ALLOWLIST)
         if (ALLOWLIST(i)%item_type == export_item_type .and. ALLOWLIST(i)%kind == kind) then
            allowed = .true.
            return
         end if
      end do
   end function is_mirrored_export_allowed

   ! Resolves one kind present on both sides from the two statuses alone.
   ! The subclass comparison runs only when both sides are SPECIFIED.
   integer function resolve_pair(export_c, import_c, export_item_type) result(outcome)
      class(StateItemCharacteristic), intent(in) :: export_c
      class(StateItemCharacteristic), intent(in) :: import_c
      type(ESMF_StateItem_Flag), intent(in) :: export_item_type

      type(CharacteristicStatus) :: e, i

      e = export_c%get_status()
      i = import_c%get_status()

      if (.not. (is_valid(e) .and. is_valid(i))) then
         outcome = PAIR_INVALID
      else if (e == CHARACTERISTIC_STATUS_DEFERRED .or. i == CHARACTERISTIC_STATUS_DEFERRED) then
         outcome = PAIR_WAIT
      else if (e == CHARACTERISTIC_STATUS_UNCHECKED .and. i == CHARACTERISTIC_STATUS_MIRRORED) then
         outcome = PAIR_ADOPT
      else if (e == CHARACTERISTIC_STATUS_UNCHECKED .or. i == CHARACTERISTIC_STATUS_UNCHECKED) then
         outcome = PAIR_MATCH
      else if (e == CHARACTERISTIC_STATUS_MIRRORED) then
         outcome = PAIR_CONFLICT
         if (i == CHARACTERISTIC_STATUS_SPECIFIED .and. &
              is_mirrored_export_allowed(export_item_type, export_c%get_kind())) outcome = PAIR_MATCH
      else if (i == CHARACTERISTIC_STATUS_MIRRORED) then
         outcome = PAIR_ADOPT
      else if (export_c%needs_extension_for(import_c)) then
         outcome = PAIR_EXTEND
      else
         outcome = PAIR_MATCH
      end if

   contains

      logical function is_valid(s)
         type(CharacteristicStatus), intent(in) :: s
         is_valid = s == CHARACTERISTIC_STATUS_SPECIFIED .or. s == CHARACTERISTIC_STATUS_MIRRORED .or. &
              s == CHARACTERISTIC_STATUS_UNCHECKED .or. s == CHARACTERISTIC_STATUS_DEFERRED
      end function is_valid

   end function resolve_pair

   ! Applies resolve_pair to every kind present on both maps; a kind on only
   ! one side is not applicable to that shape and is skipped. Stops at the
   ! first status conflict and describes it in `conflict` ('' if none).
   ! `adopted` lists kinds the import must copy from the export.
   ! A kind that must wait (a DEFERRED side) sets `waiting`; a caller that
   ! does not pass `waiting` gets a conflict instead, since it cannot retry.
   ! `mismatched` is in extension order (order_mismatched).
   ! `warnings` collects each matched pair's match_warning text.
   subroutine resolve_characteristics(export_map, import_map, export_item_type, mismatched, adopted, conflict, rc, &
        waiting, warnings)
      type(StateItemCharacteristicMap), target, intent(in) :: export_map
      type(StateItemCharacteristicMap), target, intent(in) :: import_map
      type(ESMF_StateItem_Flag), intent(in) :: export_item_type
      type(StateItemCharacteristicKind), allocatable, intent(out) :: mismatched(:)
      type(StateItemCharacteristicKind), allocatable, intent(out) :: adopted(:)
      character(:), allocatable, intent(out) :: conflict
      integer, optional, intent(out) :: rc
      logical, optional, intent(out) :: waiting
      type(StringVector), optional, intent(out) :: warnings

      type(StateItemCharacteristicMapIterator) :: iter
      type(StateItemCharacteristicKind) :: kind
      class(StateItemCharacteristic), pointer :: export_c, import_c
      type(CharacteristicStatus) :: export_status, import_status
      integer :: status
      character(:), allocatable :: warning

      allocate(mismatched(0), adopted(0))
      conflict = ''
      if (present(waiting)) waiting = .false.

      associate (e => export_map%ftn_end())
         iter = export_map%ftn_begin()
         do while (iter /= e)
            call iter%next()
            kind = iter%first()
            if (import_map%count(kind) == 0) cycle

            export_c => export_map%at(kind, rc=status)
            import_c => import_map%at(kind, rc=status)
            select case (resolve_pair(export_c, import_c, export_item_type))
            case (PAIR_MATCH)
               if (present(warnings)) then
                  warning = export_c%match_warning(import_c)
                  if (warning /= '') call warnings%push_back(warning)
               end if
            case (PAIR_EXTEND)
               mismatched = [mismatched, kind]
            case (PAIR_ADOPT)
               adopted = [adopted, kind]
            case (PAIR_WAIT)
               if (present(waiting)) then
                  waiting = .true.
               else
                  export_status = export_c%get_status()
                  import_status = import_c%get_status()
                  conflict = 'status conflict on ' // kind%to_string() // ': export ' // &
                       export_status%to_string() // ', import ' // import_status%to_string()
                  exit
               end if
            case (PAIR_CONFLICT)
               export_status = export_c%get_status()
               import_status = import_c%get_status()
               conflict = 'status conflict on ' // kind%to_string() // ': export ' // &
                    export_status%to_string() // ', import ' // import_status%to_string()
               exit
            case default
               _FAIL('StateItemDescriptor: INVALID characteristic status reached connection resolution for ' // kind%to_string())
            end select
         end do
      end associate

      if (conflict == '') then
         mismatched = order_mismatched(export_map, import_map, mismatched, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine resolve_characteristics

   ! Orders mismatched kinds as the chain reconciles them. Each step takes the
   ! first mismatched kind in the current tip's shape order, then the kinds
   ! that order does not list, by kind name. After the ITEM_SHAPE step the tip
   ! has the import's shape, so the remaining kinds follow that shape's order.
   ! With no ITEM_SHAPE entry, all kinds go by name.
   function order_mismatched(export_map, import_map, mismatched, rc) &
        result(ordered)
      type(StateItemCharacteristicMap), target, intent(in) :: export_map
      type(StateItemCharacteristicMap), target, intent(in) :: import_map
      type(StateItemCharacteristicKind), intent(in) :: mismatched(:)
      integer, optional, intent(out) :: rc
      type(StateItemCharacteristicKind), allocatable :: ordered(:)

      integer :: status, i, n
      type(StateItemCharacteristicKind), allocatable :: remaining(:)
      type(StateItemCharacteristicKind), allocatable :: shape_order(:)
      class(StateItemCharacteristic), pointer :: tip_shape
      logical, allocatable :: done(:)

      remaining = by_name(mismatched)
      n = size(remaining)
      allocate(ordered(0))
      allocate(done(n), source=.false.)

      tip_shape => null()
      if (export_map%count(ITEM_SHAPE_CHARACTERISTIC_KIND) > 0) then
         tip_shape => export_map%at(ITEM_SHAPE_CHARACTERISTIC_KIND, _RC)
      end if

      do while (size(ordered) < n)
         allocate(shape_order(0))
         if (associated(tip_shape)) then
            select type (tip_shape)
            type is (ItemShapeCharacteristic)
               shape_order = tip_shape%get_extension_order(import_map, _RC)
            class default
               _FAIL('StateItemDescriptor: ITEM_SHAPE entry has the wrong type')
            end select
         end if

         i = first_pending(shape_order)
         if (i == 0) i = findloc(done, .false., dim=1)
         done(i) = .true.
         ordered = [ordered, remaining(i)]
         deallocate(shape_order)

         if (remaining(i) == ITEM_SHAPE_CHARACTERISTIC_KIND) then
            tip_shape => import_map%at(ITEM_SHAPE_CHARACTERISTIC_KIND, _RC)
         end if
      end do

      _RETURN(_SUCCESS)

   contains

      ! Index in `remaining` of the first not-yet-ordered kind listed in
      ! `list`, or 0.
      integer function first_pending(list) result(idx)
         type(StateItemCharacteristicKind), intent(in) :: list(:)

         integer :: j, k

         idx = 0
         do j = 1, size(list)
            do k = 1, n
               if (.not. done(k) .and. remaining(k) == list(j)) then
                  idx = k
                  return
               end if
            end do
         end do
      end function first_pending

   end function order_mismatched

   ! Insertion sort by to_string(), so the fallback order does not depend on
   ! the numeric kind values.
   function by_name(kinds) result(sorted)
      type(StateItemCharacteristicKind), intent(in) :: kinds(:)
      type(StateItemCharacteristicKind), allocatable :: sorted(:)

      integer :: i, j
      type(StateItemCharacteristicKind) :: key

      sorted = kinds
      do i = 2, size(sorted)
         key = sorted(i)
         j = i - 1
         do while (j >= 1)
            if (.not. (key%to_string() < sorted(j)%to_string())) exit
            sorted(j + 1) = sorted(j)
            j = j - 1
         end do
         sorted(j + 1) = key
      end do
   end function by_name

   ! Descriptor form of resolve_characteristics: uses the export's declared
   ! item type for the allowlist and records each adoption on the import.
   ! The export is never modified.
   subroutine resolve_descriptors(export, import, mismatched, conflict, rc, waiting, warnings)
      type(StateItemDescriptor), intent(in) :: export
      type(StateItemDescriptor), intent(inout) :: import
      type(StateItemCharacteristicKind), allocatable, intent(out) :: mismatched(:)
      character(:), allocatable, intent(out) :: conflict
      integer, optional, intent(out) :: rc
      logical, optional, intent(out) :: waiting
      type(StringVector), optional, intent(out) :: warnings

      integer :: status, i
      type(StateItemCharacteristicKind), allocatable :: adopted(:)
      class(StateItemCharacteristic), allocatable :: source

      call resolve_characteristics(export%characteristics, import%characteristics, export%declared_item_type, &
           mismatched, adopted, conflict, rc=status, waiting=waiting, warnings=warnings)
      _VERIFY(status)
      _RETURN_UNLESS(conflict == '')
      if (present(waiting)) then
         _RETURN_IF(waiting)
      end if

      do i = 1, size(adopted)
         call export%get_characteristic(adopted(i), source, _RC)
         call import%set_characteristic(adopted(i), source, _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine resolve_descriptors

   ! Kinds whose status is DEFERRED or INVALID. MIRRORED is not listed: an
   ! unreached mirrored import is reported as unresolved elsewhere, and an
   ! unconsumed mirrored export is never extended.
   function descriptor_get_unresolved_kinds(this) result(kinds)
      class(StateItemDescriptor), target, intent(in) :: this
      type(StateItemCharacteristicKind), allocatable :: kinds(:)

      type(StateItemCharacteristicMapIterator) :: iter
      class(StateItemCharacteristic), pointer :: c
      type(CharacteristicStatus) :: s

      allocate(kinds(0))
      associate (e => this%characteristics%ftn_end())
         iter = this%characteristics%ftn_begin()
         do while (iter /= e)
            call iter%next()
            c => iter%second()
            s = c%get_status()
            if (s == CHARACTERISTIC_STATUS_SPECIFIED .or. s == CHARACTERISTIC_STATUS_UNCHECKED .or. &
                 s == CHARACTERISTIC_STATUS_MIRRORED) cycle
            kinds = [kinds, iter%first()]
         end do
      end associate
   end function descriptor_get_unresolved_kinds

   ! Kinds whose entry still lacks a value the item's field is created from
   ! (is_complete), whatever their status.
   function descriptor_get_incomplete_kinds(this) result(kinds)
      class(StateItemDescriptor), target, intent(in) :: this
      type(StateItemCharacteristicKind), allocatable :: kinds(:)

      type(StateItemCharacteristicMapIterator) :: iter
      class(StateItemCharacteristic), pointer :: c

      allocate(kinds(0))
      associate (e => this%characteristics%ftn_end())
         iter = this%characteristics%ftn_begin()
         do while (iter /= e)
            call iter%next()
            c => iter%second()
            if (c%is_complete()) cycle
            kinds = [kinds, iter%first()]
         end do
      end associate
   end function descriptor_get_incomplete_kinds

   ! Lets each characteristic add to defaults what the component recorded on
   ! its realized item (handle).
   subroutine descriptor_harvest(this, handle, defaults, rc)
      class(StateItemDescriptor), target, intent(in) :: this
      type(GraphStateItem), intent(in) :: handle
      type(CharacteristicDefaults), intent(inout) :: defaults
      integer, optional, intent(out) :: rc

      integer :: status
      type(StateItemCharacteristicMapIterator) :: iter
      class(StateItemCharacteristic), pointer :: c

      associate (e => this%characteristics%ftn_end())
         iter = this%characteristics%ftn_begin()
         do while (iter /= e)
            call iter%next()
            c => iter%second()
            call c%harvest(handle, defaults, _RC)
         end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine descriptor_harvest

   ! Freeze-time check: fails naming the item and the first unresolved kind.
   subroutine descriptor_check_resolved(this, item_name, rc)
      class(StateItemDescriptor), target, intent(in) :: this
      character(*), intent(in) :: item_name
      integer, optional, intent(out) :: rc

      type(StateItemCharacteristicKind), allocatable :: kinds(:)
      class(StateItemCharacteristic), allocatable :: c
      type(CharacteristicStatus) :: s
      integer :: status

      kinds = this%get_unresolved_kinds()
      if (size(kinds) > 0) then
         call this%get_characteristic(kinds(1), c, _RC)
         s = c%get_status()
         _FAIL('StateItemDescriptor: item ' // item_name // ' has characteristic ' // kinds(1)%to_string() // ' still ' // s%to_string() // ' at freeze')
      end if

      _RETURN(_SUCCESS)
   end subroutine descriptor_check_resolved

   ! Offers the component-supplied values to every DEFERRED or UNCHECKED
   ! characteristic (an UNCHECKED one may still be waiting for a value it
   ! carries but does not compare). Each characteristic takes what it needs;
   ! none is named here. A DEFERRED one that finds nothing to take stays
   ! DEFERRED and fails freeze.
   subroutine descriptor_resolve_deferred(this, defaults, rc)
      class(StateItemDescriptor), target, intent(inout) :: this
      type(CharacteristicDefaults), intent(in) :: defaults
      integer, optional, intent(out) :: rc

      integer :: status
      type(StateItemCharacteristicMapIterator) :: iter
      class(StateItemCharacteristic), pointer :: c
      type(CharacteristicStatus) :: s

      associate (e => this%characteristics%ftn_end())
         iter = this%characteristics%ftn_begin()
         do while (iter /= e)
            call iter%next()
            c => iter%second()
            s = c%get_status()
            if (s == CHARACTERISTIC_STATUS_DEFERRED .or. s == CHARACTERISTIC_STATUS_UNCHECKED) then
               call c%resolve_deferred(defaults, _RC)
            end if
         end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine descriptor_resolve_deferred

   ! Removes each characteristic that is still DEFERRED and that says it does
   ! not apply without a component-supplied value. Run after resolve_deferred.
   subroutine descriptor_drop_unsupplied(this, defaults, rc)
      class(StateItemDescriptor), target, intent(inout) :: this
      type(CharacteristicDefaults), intent(in) :: defaults
      integer, optional, intent(out) :: rc

      type(StateItemCharacteristicMapIterator) :: iter
      class(StateItemCharacteristic), pointer :: c
      type(StateItemCharacteristicKind), allocatable :: dropped(:)
      integer :: i, n_erased

      allocate(dropped(0))
      associate (e => this%characteristics%ftn_end())
         iter = this%characteristics%ftn_begin()
         do while (iter /= e)
            call iter%next()
            c => iter%second()
            if (c%drop_if_unsupplied(defaults)) dropped = [dropped, iter%first()]
         end do
      end associate

      do i = 1, size(dropped)
         n_erased = int(this%characteristics%erase(dropped(i)))
      end do

      _RETURN(_SUCCESS)
   end subroutine descriptor_drop_unsupplied

   ! Converts each characteristic that is still DEFERRED and whose
   ! is_deferred_fatal() says no to UNCHECKED, so it stops waiting instead of
   ! failing freeze. Returns the kinds converted, so the caller can report
   ! what happened without this method naming any particular kind.
   subroutine descriptor_fallback_unsupplied_deferred(this, fallen_back, rc)
      class(StateItemDescriptor), target, intent(inout) :: this
      type(StateItemCharacteristicKind), allocatable, intent(out) :: &
           fallen_back(:)
      integer, optional, intent(out) :: rc

      type(StateItemCharacteristicMapIterator) :: iter
      class(StateItemCharacteristic), pointer :: c

      allocate(fallen_back(0))
      associate (e => this%characteristics%ftn_end())
         iter = this%characteristics%ftn_begin()
         do while (iter /= e)
            call iter%next()
            c => iter%second()
            if (c%get_status() == CHARACTERISTIC_STATUS_DEFERRED .and. &
                 .not. c%is_deferred_fatal()) then
               call c%set_status(CHARACTERISTIC_STATUS_UNCHECKED)
               fallen_back = [fallen_back, iter%first()]
            end if
         end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine descriptor_fallback_unsupplied_deferred

   ! Writes each SPECIFIED or UNCHECKED characteristic onto the handle's Info.
   ! The DESCRIPTIVE and ADVERTISED phases skip structural characteristics, so
   ! a completed field is never re-created through FieldDelta.
   subroutine descriptor_sync_to_info(this, handle, phase, rc)
      class(StateItemDescriptor), target, intent(in) :: this
      type(GraphStateItem), intent(inout) :: handle
      integer, intent(in) :: phase
      integer, optional, intent(out) :: rc

      integer :: status

      call sync_entries(this, handle, phase, per_alias_only=.false., _RC)

      _RETURN(_SUCCESS)
   end subroutine descriptor_sync_to_info

   ! Writes only the per-alias entries (is_per_alias()) onto an alias handle.
   ! A field-wide key written through an alias would overwrite the field's
   ! owner (an import's UNCHECKED standard_name over the export's). Only a
   ! Field alias carries per-alias metadata, so any other handle is left alone.
   subroutine descriptor_sync_per_alias_to_info(this, handle, phase, rc)
      class(StateItemDescriptor), target, intent(in) :: this
      type(GraphStateItem), intent(inout) :: handle
      integer, intent(in) :: phase
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_StateItem_Flag) :: item_type

      item_type = handle%itemType(_RC)
      _RETURN_UNLESS(item_type == ESMF_STATEITEM_FIELD)
      call sync_entries(this, handle, phase, per_alias_only=.true., _RC)

      _RETURN(_SUCCESS)
   end subroutine descriptor_sync_per_alias_to_info

   subroutine sync_entries(this, handle, phase, per_alias_only, rc)
      class(StateItemDescriptor), target, intent(in) :: this
      type(GraphStateItem), intent(inout) :: handle
      integer, intent(in) :: phase
      logical, intent(in) :: per_alias_only
      integer, optional, intent(out) :: rc

      integer :: status
      type(StateItemCharacteristicMapIterator) :: iter
      class(StateItemCharacteristic), pointer :: c
      type(CharacteristicStatus) :: s
      logical :: known_phase

      known_phase = (phase == SYNC_PHASE_STRUCTURAL .or. &
           phase == SYNC_PHASE_DESCRIPTIVE .or. &
           phase == SYNC_PHASE_ADVERTISED)
      _ASSERT(known_phase, 'StateItemDescriptor: sync_to_info - unknown phase')

      associate (e => this%characteristics%ftn_end())
         iter = this%characteristics%ftn_begin()
         do while (iter /= e)
            call iter%next()
            c => iter%second()
            s = c%get_status()
            if (.not. (s == CHARACTERISTIC_STATUS_SPECIFIED .or. s == CHARACTERISTIC_STATUS_UNCHECKED)) cycle
            if (phase /= SYNC_PHASE_STRUCTURAL .and. c%is_structural()) cycle
            if (per_alias_only .and. .not. c%is_per_alias()) cycle
            call c%write_info(handle, phase, _RC)
         end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine sync_entries

   ! -- declared item type --

   function descriptor_get_declared_item_type(this) result(item_type)
      class(StateItemDescriptor), intent(in) :: this
      type(ESMF_StateItem_Flag) :: item_type

      item_type = this%declared_item_type
   end function descriptor_get_declared_item_type

   subroutine descriptor_set_declared_item_type(this, item_type)
      class(StateItemDescriptor), intent(inout) :: this
      type(ESMF_StateItem_Flag), intent(in) :: item_type

      this%declared_item_type = item_type
   end subroutine descriptor_set_declared_item_type

   ! Makes the declared item type that of the ITEM_SHAPE entry, so a
   ! descriptor built entry by entry (an extension link) describes the same
   ! item class as its shape. An EXPRESSION shape gives FIELD: the
   ! expression is evaluated into a Field, and a link must not look like
   ! another expression item. Without a shape entry it is left as it is.
   subroutine descriptor_take_declared_item_type_from_shape(this, rc)
      class(StateItemDescriptor), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      class(StateItemCharacteristic), pointer :: item_shape

      _RETURN_UNLESS(this%characteristics%count(ITEM_SHAPE_CHARACTERISTIC_KIND) > 0)
      item_shape => this%characteristics%at(ITEM_SHAPE_CHARACTERISTIC_KIND, _RC)
      select type (item_shape)
      type is (ItemShapeCharacteristic)
         this%declared_item_type = item_shape%get_item_class()
         if (this%declared_item_type == MAPL_STATEITEM_EXPRESSION) then
            this%declared_item_type = ESMF_STATEITEM_FIELD
         end if
      class default
         _FAIL('StateItemDescriptor: ITEM_SHAPE entry has the wrong type')
      end select

      _RETURN(_SUCCESS)
   end subroutine descriptor_take_declared_item_type_from_shape

   subroutine descriptor_set_fill_value(this, fill_value)
      class(StateItemDescriptor), intent(inout) :: this
      real(ESMF_KIND_R4), intent(in) :: fill_value

      this%fill_value = fill_value
   end subroutine descriptor_set_fill_value

   ! found is .false. (and fill_value 0) when none was declared.
   subroutine descriptor_get_fill_value(this, fill_value, found)
      class(StateItemDescriptor), intent(in) :: this
      real(ESMF_KIND_R4), intent(out) :: fill_value
      logical, intent(out) :: found

      found = allocated(this%fill_value)
      fill_value = 0.0
      if (found) fill_value = this%fill_value
   end subroutine descriptor_get_fill_value

   subroutine descriptor_set_bracket_size(this, bracket_size)
      class(StateItemDescriptor), intent(inout) :: this
      integer, intent(in) :: bracket_size

      this%bracket_size = bracket_size
   end subroutine descriptor_set_bracket_size

   ! found is .false. (and bracket_size 0) when none was declared.
   subroutine descriptor_get_bracket_size(this, bracket_size, found)
      class(StateItemDescriptor), intent(in) :: this
      integer, intent(out) :: bracket_size
      logical, intent(out) :: found

      found = allocated(this%bracket_size)
      bracket_size = 0
      if (found) bracket_size = this%bracket_size
   end subroutine descriptor_get_bracket_size

   subroutine descriptor_set_vector_basis_kind(this, basis_kind)
      class(StateItemDescriptor), intent(inout) :: this
      type(MAPL_VectorBasisKind), intent(in) :: basis_kind

      this%vector_basis_kind = basis_kind
   end subroutine descriptor_set_vector_basis_kind

   ! found is .false. when none was declared; basis_kind is then undefined.
   subroutine descriptor_get_vector_basis_kind(this, basis_kind, found)
      class(StateItemDescriptor), intent(in) :: this
      type(MAPL_VectorBasisKind), intent(out) :: basis_kind
      logical, intent(out) :: found

      found = allocated(this%vector_basis_kind)
      if (found) basis_kind = this%vector_basis_kind
   end subroutine descriptor_get_vector_basis_kind

   subroutine descriptor_set_vector_component_names(this, name_1, name_2)
      class(StateItemDescriptor), intent(inout) :: this
      character(*), intent(in) :: name_1
      character(*), intent(in) :: name_2

      this%vector_component_name_1 = name_1
      this%vector_component_name_2 = name_2
   end subroutine descriptor_set_vector_component_names

   ! found is .false. (and both names empty) when none were declared.
   subroutine descriptor_get_vector_component_names(this, name_1, name_2, found)
      class(StateItemDescriptor), intent(in) :: this
      character(:), allocatable, intent(out) :: name_1
      character(:), allocatable, intent(out) :: name_2
      logical, intent(out) :: found

      found = allocated(this%vector_component_name_1) .and. &
           allocated(this%vector_component_name_2)
      name_1 = ''
      name_2 = ''
      if (found) then
         name_1 = this%vector_component_name_1
         name_2 = this%vector_component_name_2
      end if
   end subroutine descriptor_get_vector_component_names

   ! The expression string of an EXPRESSION item (possibly ''). Fails if
   ! the descriptor has no Expression entry.
   subroutine descriptor_get_expression(this, expression, rc)
      class(StateItemDescriptor), intent(in) :: this
      character(:), allocatable, intent(out) :: expression
      integer, optional, intent(out) :: rc

      integer :: status
      class(StateItemCharacteristic), allocatable :: characteristic

      _ASSERT(this%has_characteristic(EXPRESSION_CHARACTERISTIC_KIND), 'StateItemDescriptor: no Expression entry')
      call this%get_characteristic(EXPRESSION_CHARACTERISTIC_KIND, &
           characteristic, _RC)
      select type (characteristic)
      class is (ExpressionCharacteristic)
         expression = characteristic%get_expression()
      class default
         _FAIL('StateItemDescriptor: Expression entry has the wrong type')
      end select

      _RETURN(_SUCCESS)
   end subroutine descriptor_get_expression

   ! Gives an expression result descriptor a SPECIFIED GEOM
   ! HorizontalDimsSpec, as FieldCreate assumes, unless it has an entry.
   subroutine descriptor_add_expression_result_defaults(this, rc)
      class(StateItemDescriptor), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      if (.not. this%has_characteristic( &
           HORIZONTAL_DIMS_SPEC_CHARACTERISTIC_KIND)) then
         call this%set_characteristic( &
              HORIZONTAL_DIMS_SPEC_CHARACTERISTIC_KIND, &
              HorizontalDimsSpecCharacteristic(HORIZONTAL_DIMS_GEOM), _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine descriptor_add_expression_result_defaults

end module mapl_StateItemDescriptor_mod
