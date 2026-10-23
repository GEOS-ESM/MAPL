#include "MAPL.h"

!------------------------------------------------------------------------------
! StateItemDescriptor: the description of a state item, kept apart from the
! ESMF handle (GraphStateItem). Holds the sparse StateItemCharacteristicMap
! (REQ-CHAR-007/008/009/010) plus the description values that are not yet
! characteristics (vertical stagger, declared geom / vertical
! grid / item type / restart mode / expression).
!
! Independent of any ESMF object: a StateItemNode's descriptor is populated at
! ADVERTISE, before any handle exists. Layering: graph/ only; must not depend
! on OuterMetaComponent, StateRegistry, or VariableSpec.
!------------------------------------------------------------------------------
module mapl_StateItemDescriptor_mod
   use ESMF, only: ESMF_StateItem_Flag, ESMF_STATEITEM_FIELD, operator(==)
   use ESMF, only: ESMF_Geom
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, StateItemCharacteristicMap, &
        StateItemCharacteristicMapIterator, operator(/=), SYNC_PHASE_STRUCTURAL, SYNC_PHASE_DESCRIPTIVE
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind, operator(==), &
        GEOMETRY_CHARACTERISTIC_KIND, VERTICAL_COORDINATE_CHARACTERISTIC_KIND, TYPE_KIND_CHARACTERISTIC_KIND
   use mapl_CharacteristicStatus_mod, only: CharacteristicStatus, operator(==), &
        CHARACTERISTIC_STATUS_SPECIFIED, CHARACTERISTIC_STATUS_MIRRORED, &
        CHARACTERISTIC_STATUS_UNCHECKED, CHARACTERISTIC_STATUS_DEFERRED
   use mapl_StateItem_mod, only: MAPL_STATEITEM_EXPRESSION
   use mapl_VerticalStaggerLoc_mod, only: VerticalStaggerLoc
   use mapl_VerticalGrid_mod, only: VerticalGrid
   use mapl_RestartModes_mod, only: RestartMode
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: StateItemDescriptor
   public :: resolve_pair
   public :: resolve_characteristics
   public :: resolve_descriptors
   public :: is_mirrored_export_allowed
   public :: PAIR_MATCH, PAIR_EXTEND, PAIR_ADOPT, PAIR_CONFLICT, PAIR_INVALID

   ! Outcomes of resolve_pair for one kind present on both sides.
   ! PAIR_ADOPT: the import takes a copy of the export's value.
   ! PAIR_INVALID: an INVALID status reached resolution (programming error).
   integer, parameter :: PAIR_MATCH = 1
   integer, parameter :: PAIR_EXTEND = 2
   integer, parameter :: PAIR_ADOPT = 3
   integer, parameter :: PAIR_CONFLICT = 4
   integer, parameter :: PAIR_INVALID = 5

   ! (export item type, kind) pairs whose export may be MIRRORED. An
   ! expression has no geometry or precision of its own; each consumer's
   ! chain tip takes the consumer's value. Add entries only with a
   ! demonstrated use case.
   type :: MirroredExportAllowance
      type(ESMF_StateItem_Flag) :: item_type
      type(StateItemCharacteristicKind) :: kind
   end type MirroredExportAllowance

   type :: StateItemDescriptor
      private
      type(StateItemCharacteristicMap) :: characteristics
      ! Optional or defaulted values: "not declared" is meaningful.
      type(VerticalStaggerLoc), allocatable :: vertical_stagger
      type(ESMF_Geom), allocatable :: declared_geom
      class(VerticalGrid), allocatable :: declared_vertical_grid
      type(ESMF_StateItem_Flag) :: declared_item_type = ESMF_STATEITEM_FIELD
      type(RestartMode), allocatable :: declared_restart_mode
      character(:), allocatable :: declared_expression
   contains
      procedure :: has_characteristic => descriptor_has_characteristic
      procedure :: get_characteristic => descriptor_get_characteristic
      procedure :: set_characteristic => descriptor_set_characteristic
      procedure :: get_characteristics => descriptor_get_characteristics
      procedure :: get_unresolved_kinds => descriptor_get_unresolved_kinds
      procedure :: check_resolved => descriptor_check_resolved
      procedure :: sync_to_info => descriptor_sync_to_info
      procedure :: get_vertical_stagger => descriptor_get_vertical_stagger
      procedure :: set_vertical_stagger => descriptor_set_vertical_stagger
      procedure :: get_declared_geom => descriptor_get_declared_geom
      procedure :: set_declared_geom => descriptor_set_declared_geom
      procedure :: get_declared_vertical_grid => descriptor_get_declared_vertical_grid
      procedure :: set_declared_vertical_grid => descriptor_set_declared_vertical_grid
      procedure :: get_declared_item_type => descriptor_get_declared_item_type
      procedure :: set_declared_item_type => descriptor_set_declared_item_type
      procedure :: get_declared_restart_mode => descriptor_get_declared_restart_mode
      procedure :: set_declared_restart_mode => descriptor_set_declared_restart_mode
      procedure :: get_declared_expression => descriptor_get_declared_expression
      procedure :: set_declared_expression => descriptor_set_declared_expression
   end type StateItemDescriptor

contains

   ! -- characteristics (REQ-CHAR-007/008/010) --

   logical function descriptor_has_characteristic(this, kind) result(has)
      class(StateItemDescriptor), intent(in) :: this
      type(StateItemCharacteristicKind), intent(in) :: kind

      has = this%characteristics%count(kind) > 0
   end function descriptor_has_characteristic

   ! REQ-CHAR-008: an absent key means "never established" and fails
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
   ! operation. REQ-CHAR-006/009: characteristic%get_kind() MUST equal `kind`,
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

   ! REQ-CHAR-009: enumerable so detection can iterate every entry. Returns a
   ! copy.
   function descriptor_get_characteristics(this) result(characteristics)
      class(StateItemDescriptor), intent(in) :: this
      type(StateItemCharacteristicMap) :: characteristics

      characteristics = this%characteristics
   end function descriptor_get_characteristics

   ! -- generic status resolution --

   logical function is_mirrored_export_allowed(export_item_type, kind) result(allowed)
      type(ESMF_StateItem_Flag), intent(in) :: export_item_type
      type(StateItemCharacteristicKind), intent(in) :: kind

      type(MirroredExportAllowance), parameter :: ALLOWLIST(*) = [ &
           MirroredExportAllowance(MAPL_STATEITEM_EXPRESSION, GEOMETRY_CHARACTERISTIC_KIND), &
           MirroredExportAllowance(MAPL_STATEITEM_EXPRESSION, VERTICAL_COORDINATE_CHARACTERISTIC_KIND), &
           MirroredExportAllowance(MAPL_STATEITEM_EXPRESSION, TYPE_KIND_CHARACTERISTIC_KIND)]
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
         outcome = PAIR_CONFLICT
      else if (e == CHARACTERISTIC_STATUS_UNCHECKED .and. i == CHARACTERISTIC_STATUS_MIRRORED) then
         outcome = PAIR_CONFLICT
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
   subroutine resolve_characteristics(export_map, import_map, export_item_type, mismatched, adopted, conflict, rc)
      type(StateItemCharacteristicMap), target, intent(in) :: export_map
      type(StateItemCharacteristicMap), target, intent(in) :: import_map
      type(ESMF_StateItem_Flag), intent(in) :: export_item_type
      type(StateItemCharacteristicKind), allocatable, intent(out) :: mismatched(:)
      type(StateItemCharacteristicKind), allocatable, intent(out) :: adopted(:)
      character(:), allocatable, intent(out) :: conflict
      integer, optional, intent(out) :: rc

      type(StateItemCharacteristicMapIterator) :: iter
      type(StateItemCharacteristicKind) :: kind
      class(StateItemCharacteristic), pointer :: export_c, import_c
      type(CharacteristicStatus) :: export_status, import_status
      integer :: status

      allocate(mismatched(0), adopted(0))
      conflict = ''

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
            case (PAIR_EXTEND)
               mismatched = [mismatched, kind]
            case (PAIR_ADOPT)
               adopted = [adopted, kind]
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

      _RETURN(_SUCCESS)
   end subroutine resolve_characteristics

   ! Descriptor form of resolve_characteristics: uses the export's declared
   ! item type for the allowlist and records each adoption on the import.
   ! The export is never modified.
   subroutine resolve_descriptors(export, import, mismatched, conflict, rc)
      type(StateItemDescriptor), intent(in) :: export
      type(StateItemDescriptor), intent(inout) :: import
      type(StateItemCharacteristicKind), allocatable, intent(out) :: mismatched(:)
      character(:), allocatable, intent(out) :: conflict
      integer, optional, intent(out) :: rc

      integer :: status, i
      type(StateItemCharacteristicKind), allocatable :: adopted(:)
      class(StateItemCharacteristic), allocatable :: source

      call resolve_characteristics(export%characteristics, import%characteristics, export%declared_item_type, &
           mismatched, adopted, conflict, _RC)
      _RETURN_UNLESS(conflict == '')

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

   ! Writes each SPECIFIED or UNCHECKED characteristic onto the handle's Info.
   ! The DESCRIPTIVE phase skips structural characteristics, so a completed
   ! field is never re-created through FieldDelta.
   subroutine descriptor_sync_to_info(this, handle, phase, rc)
      class(StateItemDescriptor), target, intent(in) :: this
      type(GraphStateItem), intent(inout) :: handle
      integer, intent(in) :: phase
      integer, optional, intent(out) :: rc

      integer :: status
      type(StateItemCharacteristicMapIterator) :: iter
      class(StateItemCharacteristic), pointer :: c
      type(CharacteristicStatus) :: s

      _ASSERT(phase == SYNC_PHASE_STRUCTURAL .or. phase == SYNC_PHASE_DESCRIPTIVE, 'StateItemDescriptor: sync_to_info - unknown phase')

      associate (e => this%characteristics%ftn_end())
         iter = this%characteristics%ftn_begin()
         do while (iter /= e)
            call iter%next()
            c => iter%second()
            s = c%get_status()
            if (.not. (s == CHARACTERISTIC_STATUS_SPECIFIED .or. s == CHARACTERISTIC_STATUS_UNCHECKED)) cycle
            if (phase == SYNC_PHASE_DESCRIPTIVE .and. c%is_structural()) cycle
            call c%write_info(handle, phase, _RC)
         end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine descriptor_sync_to_info

   ! -- materialization inputs --

   ! A subroutine with an intent(out) allocatable dummy: a direct
   ! `x = d%get_vertical_stagger()` from an unallocated allocatable function
   ! result crashed with a NULL dereference under gfortran.
   subroutine descriptor_get_vertical_stagger(this, vertical_stagger)
      class(StateItemDescriptor), intent(in) :: this
      type(VerticalStaggerLoc), allocatable, intent(out) :: vertical_stagger

      if (allocated(this%vertical_stagger)) vertical_stagger = this%vertical_stagger
   end subroutine descriptor_get_vertical_stagger

   ! Explicit deallocate + allocate(source=) instead of `=` into an allocatable
   ! component: avoids a gfortran realloc-on-assignment memory corruption.
   subroutine descriptor_set_vertical_stagger(this, vertical_stagger)
      class(StateItemDescriptor), intent(inout) :: this
      type(VerticalStaggerLoc), intent(in) :: vertical_stagger

      if (allocated(this%vertical_stagger)) deallocate(this%vertical_stagger)
      allocate(this%vertical_stagger, source=vertical_stagger)
   end subroutine descriptor_set_vertical_stagger

   ! -- declared geom / vgrid / item type / restart mode / expression --

   subroutine descriptor_get_declared_geom(this, geom)
      class(StateItemDescriptor), intent(in) :: this
      type(ESMF_Geom), allocatable, intent(out) :: geom

      if (allocated(this%declared_geom)) geom = this%declared_geom
   end subroutine descriptor_get_declared_geom

   subroutine descriptor_set_declared_geom(this, geom)
      class(StateItemDescriptor), intent(inout) :: this
      type(ESMF_Geom), intent(in) :: geom

      if (allocated(this%declared_geom)) deallocate(this%declared_geom)
      allocate(this%declared_geom, source=geom)
   end subroutine descriptor_set_declared_geom

   subroutine descriptor_get_declared_vertical_grid(this, vertical_grid)
      class(StateItemDescriptor), intent(in) :: this
      class(VerticalGrid), allocatable, intent(out) :: vertical_grid

      if (allocated(this%declared_vertical_grid)) vertical_grid = this%declared_vertical_grid
   end subroutine descriptor_get_declared_vertical_grid

   ! allocate(source=) also avoids relying on intrinsic-assignment dynamic-type
   ! dispatch for a CLASS allocatable component.
   subroutine descriptor_set_declared_vertical_grid(this, vertical_grid)
      class(StateItemDescriptor), intent(inout) :: this
      class(VerticalGrid), intent(in) :: vertical_grid

      if (allocated(this%declared_vertical_grid)) deallocate(this%declared_vertical_grid)
      allocate(this%declared_vertical_grid, source=vertical_grid)
   end subroutine descriptor_set_declared_vertical_grid

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

   subroutine descriptor_get_declared_restart_mode(this, restart_mode)
      class(StateItemDescriptor), intent(in) :: this
      type(RestartMode), allocatable, intent(out) :: restart_mode

      if (allocated(this%declared_restart_mode)) restart_mode = this%declared_restart_mode
   end subroutine descriptor_get_declared_restart_mode

   subroutine descriptor_set_declared_restart_mode(this, restart_mode)
      class(StateItemDescriptor), intent(inout) :: this
      type(RestartMode), intent(in) :: restart_mode

      if (allocated(this%declared_restart_mode)) deallocate(this%declared_restart_mode)
      allocate(this%declared_restart_mode, source=restart_mode)
   end subroutine descriptor_set_declared_restart_mode

   subroutine descriptor_get_declared_expression(this, expression)
      class(StateItemDescriptor), intent(in) :: this
      character(:), allocatable, intent(out) :: expression

      if (allocated(this%declared_expression)) expression = this%declared_expression
   end subroutine descriptor_get_declared_expression

   subroutine descriptor_set_declared_expression(this, expression)
      class(StateItemDescriptor), intent(inout) :: this
      character(*), intent(in) :: expression

      if (allocated(this%declared_expression)) deallocate(this%declared_expression)
      allocate(this%declared_expression, source=expression)
   end subroutine descriptor_set_declared_expression

end module mapl_StateItemDescriptor_mod
