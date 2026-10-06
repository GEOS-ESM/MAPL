#include "MAPL.h"

!------------------------------------------------------------------------------
! VerticalGridCharacteristic: the "vertical_grid" Characteristic
! (Characteristic.F90). Carries two independent pieces of information
! about a declared vertical grid:
!
!   - an OPTIONAL opaque identity token (`grid_id`) - present for an
!     ordinary Field's declared `vertical_grid` aspect
!     (GraphBuilder.F90's build_characteristics, via
!     `VerticalGrid%get_id()`), absent for a
!     MAPL_STATEITEM_VERTICALGRID-tagged composite (openspec/changes/
!     vertical-grid-graph-state-item), which has no legacy VerticalGrid
!     object to pull an id from - only its own declared member names.
!   - the declared set of physical-dimension names (`dimensions`) -
!     REQ-GEO-004a's "member name = physical dimension". Populated from
!     `VerticalGrid%get_supported_physical_dimensions()` (a pure
!     accessor, no StateRegistry involvement) for the ordinary-Field
!     case, and from a composite VariableSpec's own
!     `get_member_names()` for the tagged-composite case.
!
! Comparison (needs_extension_for): when BOTH sides carry an identity
! token, that is the authoritative exact-match check (unchanged
! behavior from before this capability's own revision - two ordinary
! Fields declaring the identical VerticalGrid id always match). When
! either side lacks one (always true for a composite), the dimension
! SETS are compared instead - set equality is the match criterion.
!
! On mismatch, build_transform implements
! docs/graph/spec/13-geometry-and-vertical-grids.md REQ-GEO-007a's
! three-way dimension-overlap classification (computed from `dimensions`
! on both sides, regardless of which comparison path detected the
! mismatch): exactly one overlapping physical dimension identifies the
! (not-yet-implemented) adaptation candidate; zero overlap is
! "incompatible"; more than one overlap is "ambiguous" (cannot silently
! guess which dimension to adapt through). In all three cases
! build_transform still fails explicitly (_FAIL) - no real
! vertical-regrid Transform is implemented by this capability; a
! follow-up change is expected to give it a real implementation,
! reusing legacy's own low-level regrid numerics
! (`VerticalGridAspect`/`VerticalRegridTransform`) where separable from
! `StateRegistry`-coupled orchestration, which this module itself must
! never call into.
!------------------------------------------------------------------------------
module mapl_VerticalGridCharacteristic_mod
   use mapl_Characteristic_mod, only: Characteristic
   use mapl_CharacteristicId_mod, only: CharacteristicId, VERTICAL_GRID_CHARACTERISTIC_ID
   use mapl_ComponentGraph_mod, only: ComponentGraph
   use mapl_NodeId_mod, only: NodeId
   use mapl_Transform_mod, only: Transform
   use gFTL2_StringVector, only: StringVector
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: VerticalGridCharacteristic
   public :: classify_dimension_overlap
   public :: DIMENSION_OVERLAP_SINGLE
   public :: DIMENSION_OVERLAP_INCOMPATIBLE
   public :: DIMENSION_OVERLAP_AMBIGUOUS

   integer, parameter :: DIMENSION_OVERLAP_SINGLE = 1
   integer, parameter :: DIMENSION_OVERLAP_INCOMPATIBLE = 2
   integer, parameter :: DIMENSION_OVERLAP_AMBIGUOUS = 3

   type, extends(Characteristic) :: VerticalGridCharacteristic
      private
      character(:), allocatable :: grid_id
      type(StringVector) :: dimensions
   contains
      procedure, nopass :: get_id => vgrid_get_id
      procedure :: get_signature => vgrid_get_signature
      procedure :: needs_extension_for => vgrid_needs_extension_for
      procedure :: build_transform => vgrid_build_transform
      procedure :: get_grid_id
      procedure :: get_dimensions
      procedure :: has_grid_id
   end type VerticalGridCharacteristic

   interface VerticalGridCharacteristic
      module procedure new_VerticalGridCharacteristic
   end interface VerticalGridCharacteristic

contains

   ! `grid_id` is an opaque identity token for the declared vertical
   ! grid - callers (GraphBuilder.F90's build_characteristics) are
   ! responsible for deriving one that distinguishes different vertical
   ! grids; this type only compares the token, it does not interpret
   ! it. OPTIONAL: a MAPL_STATEITEM_VERTICALGRID-tagged composite has no
   ! legacy VerticalGrid object to pull an id from, so it is constructed
   ! with `dimensions` only (grid_id left unallocated - `has_grid_id()`
   ! reports false). `dimensions` is likewise optional, defaulting to an
   ! empty set, for callers (existing call sites predating this
   ! capability) that have no physical-dimension information to supply.
   function new_VerticalGridCharacteristic(grid_id, dimensions) result(characteristic)
      character(*), optional, intent(in) :: grid_id
      type(StringVector), optional, intent(in) :: dimensions
      type(VerticalGridCharacteristic) :: characteristic

      if (present(grid_id)) characteristic%grid_id = grid_id
      if (present(dimensions)) characteristic%dimensions = dimensions
   end function new_VerticalGridCharacteristic

   function vgrid_get_id() result(id)
      type(CharacteristicId) :: id

      id = VERTICAL_GRID_CHARACTERISTIC_ID
   end function vgrid_get_id

   ! Stable, unique-per-value signature - used by
   ! mapl_ExtensionResolution_mod's existing chain-reuse cache key.
   ! Includes both the identity token (when present) and the sorted
   ! dimension set (when non-empty), so two otherwise-identical-looking
   ! characteristics that differ in either respect get distinct cache
   ! entries.
   function vgrid_get_signature(this) result(signature)
      class(VerticalGridCharacteristic), intent(in) :: this
      character(:), allocatable :: signature

      if (allocated(this%grid_id)) then
         signature = 'vertical_grid=' // this%grid_id
      else
         signature = 'vertical_grid=<none>'
      end if
      if (this%dimensions%size() > 0) then
         signature = signature // ';dims=' // join(sorted_names(this%dimensions))
      end if
   end function vgrid_get_signature

   ! Defensive class-default branch (no `rc` argument on this interface,
   ! so _ASSERT/_RETURN are not used here - mirrors
   ! UnitsCharacteristic%needs_extension_for's own rationale): a genuine
   ! kind mismatch reports "needs extension" rather than silently
   ! matching. When both sides carry an identity token, that comparison
   ! is authoritative (exact grid-identity match/mismatch, unchanged
   ! behavior for ordinary Fields); otherwise (always true for a
   ! composite, which has none) falls back to dimension-set equality.
   logical function vgrid_needs_extension_for(this, goal) result(needs_extension)
      class(VerticalGridCharacteristic), intent(in) :: this
      class(Characteristic), intent(in) :: goal

      select type (goal)
      class is (VerticalGridCharacteristic)
         if (allocated(this%grid_id) .and. allocated(goal%grid_id)) then
            needs_extension = (this%grid_id /= goal%grid_id)
         else
            needs_extension = .not. same_set(this%dimensions, goal%dimensions)
         end if
      class default
         needs_extension = .true.
      end select
   end function vgrid_needs_extension_for

   ! REQ-GEO-007a's three-way classification, computed from `dimensions`
   ! regardless of which comparison path (identity-token or
   ! dimension-set) detected the mismatch. No real vertical-regrid
   ! Transform is implemented by this capability in any of the three
   ! cases - each _FAIL message is distinguishable so the outcome is
   ! diagnosable rather than one undifferentiated mismatch.
   subroutine vgrid_build_transform(this, graph, input_node_id, output_node_id, goal, transformer, rc)
      class(VerticalGridCharacteristic), intent(in) :: this
      class(ComponentGraph), target, intent(in) :: graph
      type(NodeId), intent(in) :: input_node_id
      type(NodeId), intent(in) :: output_node_id
      class(Characteristic), intent(in) :: goal
      class(Transform), allocatable, intent(out) :: transformer
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: outcome
      character(:), allocatable :: overlap_name
      character(:), allocatable :: overlap_list
      character(:), allocatable :: failure_message

      _UNUSED_DUMMY(graph)
      _UNUSED_DUMMY(input_node_id)
      _UNUSED_DUMMY(output_node_id)

      select type (goal)
      class is (VerticalGridCharacteristic)
         outcome = classify_dimension_overlap(this%dimensions, goal%dimensions, overlap_name, overlap_list, _RC)
      class default
         _FAIL('VerticalGridCharacteristic: goal is not a VerticalGridCharacteristic')
      end select

      select case (outcome)
      case (DIMENSION_OVERLAP_SINGLE)
         ! Built as an ordinary local variable, not inline as the _FAIL
         ! macro's own argument - gfortran's preprocessor does not
         ! support Fortran's `&` continuation spanning a macro
         ! invocation's own argument list (unlike a plain assignment
         ! statement, where `&` continuation is genuine Fortran, not
         ! macro text). Mirrors GraphBuilder.F90's own
         ! assertion_message/_ASSERT precedent.
         failure_message = 'VerticalGridCharacteristic: single overlapping physical dimension ' // &
              '"' // overlap_name // '" identified as the adaptation candidate, ' // &
              'but no real vertical-regrid Transform is implemented yet'
         _FAIL(failure_message)
      case (DIMENSION_OVERLAP_INCOMPATIBLE)
         failure_message = 'VerticalGridCharacteristic: incompatible - no common physical dimension ' // &
              'between export and import vertical grids'
         _FAIL(failure_message)
      case (DIMENSION_OVERLAP_AMBIGUOUS)
         failure_message = 'VerticalGridCharacteristic: ambiguous - more than one common physical ' // &
              'dimension between export and import vertical grids (' // overlap_list // ')'
         _FAIL(failure_message)
      case default
         _FAIL('VerticalGridCharacteristic: unrecognized dimension-overlap outcome')
      end select
   end subroutine vgrid_build_transform

   function get_grid_id(this) result(grid_id)
      class(VerticalGridCharacteristic), intent(in) :: this
      character(:), allocatable :: grid_id

      grid_id = this%grid_id
   end function get_grid_id

   logical function has_grid_id(this) result(has)
      class(VerticalGridCharacteristic), intent(in) :: this

      has = allocated(this%grid_id)
   end function has_grid_id

   function get_dimensions(this) result(dimensions)
      class(VerticalGridCharacteristic), intent(in) :: this
      type(StringVector) :: dimensions

      dimensions = this%dimensions
   end function get_dimensions

   ! REQ-GEO-007a, as a small, pure, independently-testable helper (no
   ! Characteristic/ComponentGraph dependency): computes the set overlap
   ! between two physical-dimension-name sets and classifies the result.
   ! `overlap_name` is set only for DIMENSION_OVERLAP_SINGLE (the one
   ! overlapping dimension); `overlap_list` is set only for
   ! DIMENSION_OVERLAP_AMBIGUOUS (comma-joined list of the overlapping
   ! dimensions, sorted for stable diagnostics).
   function classify_dimension_overlap(export_dims, import_dims, overlap_name, overlap_list, rc) result(outcome)
      type(StringVector), intent(in) :: export_dims
      type(StringVector), intent(in) :: import_dims
      character(:), allocatable, intent(out) :: overlap_name
      character(:), allocatable, intent(out) :: overlap_list
      integer, optional, intent(out) :: rc
      integer :: outcome

      type(StringVector) :: overlap
      integer :: i, n
      character(:), pointer :: name_ptr

      overlap_name = ''
      overlap_list = ''

      n = export_dims%size()
      do i = 1, n
         name_ptr => export_dims%of(i)
         if (contains_name(import_dims, name_ptr)) then
            call overlap%push_back(name_ptr)
         end if
      end do

      select case (overlap%size())
      case (0)
         outcome = DIMENSION_OVERLAP_INCOMPATIBLE
      case (1)
         outcome = DIMENSION_OVERLAP_SINGLE
         overlap_name = overlap%of(1)
      case default
         outcome = DIMENSION_OVERLAP_AMBIGUOUS
         overlap_list = join(sorted_names(overlap))
      end select

      _RETURN(_SUCCESS)
   end function classify_dimension_overlap

   logical function contains_name(names, name) result(found)
      type(StringVector), intent(in) :: names
      character(*), intent(in) :: name

      integer :: i, n

      found = .false.
      n = names%size()
      do i = 1, n
         if (names%of(i) == name) then
            found = .true.
            return
         end if
      end do
   end function contains_name

   ! Set equality: same size, and every name in `a` is present in `b`
   ! (sufficient given neither set is expected to contain duplicates -
   ! REQ "Member names are unique within one composite declaration
   ! level", composite-state-spec).
   logical function same_set(a, b) result(equal)
      type(StringVector), intent(in) :: a
      type(StringVector), intent(in) :: b

      integer :: i, n

      equal = (a%size() == b%size())
      if (.not. equal) return

      n = a%size()
      do i = 1, n
         if (.not. contains_name(b, a%of(i))) then
            equal = .false.
            return
         end if
      end do
   end function same_set

   ! Simple insertion sort - dimension counts are expected to be small
   ! (a handful of physical dimensions per vertical grid), so this is
   ! not performance-sensitive; chosen over pulling in a generic sort
   ! algorithm for one small, local use.
   function sorted_names(names) result(sorted)
      type(StringVector), intent(in) :: names
      character(:), allocatable :: sorted(:)

      integer :: i, j, n
      character(:), allocatable :: key
      integer :: maxlen

      n = names%size()
      maxlen = 0
      do i = 1, n
         maxlen = max(maxlen, len(names%of(i)))
      end do
      allocate(character(len=maxlen) :: sorted(n))
      do i = 1, n
         sorted(i) = names%of(i)
      end do

      do i = 2, n
         key = trim(sorted(i))
         j = i - 1
         do while (j >= 1)
            if (trim(sorted(j)) <= key) exit
            sorted(j + 1) = sorted(j)
            j = j - 1
         end do
         sorted(j + 1) = key
      end do
   end function sorted_names

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

end module mapl_VerticalGridCharacteristic_mod
