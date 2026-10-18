#include "MAPL.h"

!------------------------------------------------------------------------------
! VerticalCoordinateCharacteristic: the "vertical grid" StateItemCharacteristic
! (openspec/changes/state-item-characteristic-subclasses, design.md D1) - a
! ReferenceCharacteristic, same shape as GeometryCharacteristic.F90: holds a
! NodeId referencing a shared graph node, not a value of its own.
!
! Named VerticalCoordinateCharacteristic, NOT VerticalGridCharacteristic -
! design.md D1: an exact, unavoidable collision with the existing, unrelated
! graph/extension-reuse VerticalGridCharacteristic type (Fortran module/type
! names share one global namespace). Same collision-avoidance discipline
! already applied for GeometryCharacteristic vs. legacy GeomCharacteristic.
!
! openspec/changes/merge-characteristic-families: absorbs the retired
! mapl_VerticalGridCharacteristic_mod's own grid_id/dimensions value data
! and its real build_transform (dimension-overlap classification)/
! get_signature, in addition to this type's own pre-existing NodeId-
! sharing role. needs_extension_for is therefore now a HYBRID comparison
! (design.md Risks - "a deliberate, narrow exception specific to this one
! subclass"):
!   - when a real referenced_node_id has been set on BOTH sides (the
!     GraphStateItem-sharing use case, REQ-CHAR-012: sharing is nothing
!     more than two map entries holding the same NodeId), that identity
!     comparison is authoritative, UNCHANGED from this type's own
!     pre-merge behavior.
!   - otherwise (at least one side has no real NodeId - the
!     GraphBuilder-production-call-site case, which supplies only an
!     opaque grid_id string token/dimension set, never a NodeId, since
!     GraphBuilder's build_characteristics()/GraphBuilder_Geometry_smod's
!     mismatch-detection call sites have no NodeId to give it), falls
!     back to the same identity-token-else-dimension-set comparison
!     mapl_VerticalGridCharacteristic_mod's own vgrid_needs_extension_for
!     used, UNCHANGED in substance from that retired module's own logic.
! Both branches answer the same question ("do these two values need an
! extension to reconcile") - which comparison path applies is determined
! entirely by what data the two sides actually carry, not by a
! separate flag.
!------------------------------------------------------------------------------
module mapl_VerticalCoordinateCharacteristic_mod
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, ReferenceCharacteristic
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind, VERTICAL_COORDINATE_CHARACTERISTIC_KIND
   use mapl_NodeId_mod, only: NodeId, operator(==), operator(/=)
   use mapl_TransformBase_mod, only: TransformBase
   use gFTL2_StringVector, only: StringVector
   use mapl_StringSortUtilities_mod, only: sorted_names
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: VerticalCoordinateCharacteristic
   public :: classify_dimension_overlap
   public :: DIMENSION_OVERLAP_SINGLE
   public :: DIMENSION_OVERLAP_INCOMPATIBLE
   public :: DIMENSION_OVERLAP_AMBIGUOUS

   integer, parameter :: DIMENSION_OVERLAP_SINGLE = 1
   integer, parameter :: DIMENSION_OVERLAP_INCOMPATIBLE = 2
   integer, parameter :: DIMENSION_OVERLAP_AMBIGUOUS = 3

   type, extends(ReferenceCharacteristic) :: VerticalCoordinateCharacteristic
      private
      ! Moved from the retired mapl_VerticalGridCharacteristic_mod -
      ! value data for the dimension-overlap build_transform/get_signature
      ! below, carried alongside the inherited referenced_node_id (design.md
      ! Risks: a deliberate mix of reference and value data, unique to
      ! this one subclass).
      character(:), allocatable :: grid_id
      type(StringVector) :: dimensions
   contains
      procedure, nopass :: get_kind => vcoord_get_kind
      procedure :: needs_extension_for => vcoord_needs_extension_for
      procedure :: build_transform => vcoord_build_transform
      procedure :: get_signature => vcoord_get_signature
      procedure :: get_grid_id
      procedure :: has_grid_id
      procedure :: get_dimensions
   end type VerticalCoordinateCharacteristic

   interface VerticalCoordinateCharacteristic
      module procedure new_VerticalCoordinateCharacteristic
   end interface VerticalCoordinateCharacteristic

contains

   ! referenced_node_id: the NodeId of the ordinary graph node this
   ! characteristic points at - set via the inherited
   ! ReferenceCharacteristic%set_referenced_node_id accessor, not a bare
   ! field assignment (that component is private to
   ! mapl_StateItemCharacteristic_mod). OPTIONAL, mirroring the retired
   ! mapl_VerticalGridCharacteristic_mod's own new_VerticalGridCharacteristic
   ! constructor (design.md Decisions, task 2.3): a GraphBuilder-production
   ! call site has no NodeId to supply, only grid_id/dimensions; a
   ! GraphStateItem-sharing call site supplies only referenced_node_id.
   function new_VerticalCoordinateCharacteristic(referenced_node_id, grid_id, dimensions) result(characteristic)
      type(NodeId), optional, intent(in) :: referenced_node_id
      character(*), optional, intent(in) :: grid_id
      type(StringVector), optional, intent(in) :: dimensions
      type(VerticalCoordinateCharacteristic) :: characteristic

      if (present(referenced_node_id)) call characteristic%set_referenced_node_id(referenced_node_id)
      if (present(grid_id)) characteristic%grid_id = grid_id
      if (present(dimensions)) characteristic%dimensions = dimensions
   end function new_VerticalCoordinateCharacteristic

   function vcoord_get_kind() result(kind)
      type(StateItemCharacteristicKind) :: kind

      kind = VERTICAL_COORDINATE_CHARACTERISTIC_KIND
   end function vcoord_get_kind

   ! `goal` is guaranteed a VerticalCoordinateCharacteristic for the real
   ! call chain this method is used in (same guarantee documented on
   ! GeometryCharacteristic/PhysicalUnitsCharacteristic: GraphStateItem%
   ! set_characteristic asserts kind match before inserting). The class
   ! default branch below should be checked by the calling procedure.
   !
   ! Hybrid comparison (module header): real NodeId on both sides is
   ! authoritative when present; otherwise falls back to the retired
   ! mapl_VerticalGridCharacteristic_mod's own identity-token-else-
   ! dimension-set comparison.
   logical function vcoord_needs_extension_for(this, goal) result(needs_extension)
      class(VerticalCoordinateCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal

      type(NodeId) :: this_node_id, goal_node_id

      select type (goal)
      class is (VerticalCoordinateCharacteristic)
         this_node_id = this%get_referenced_node_id()
         goal_node_id = goal%get_referenced_node_id()
         if (this_node_id%is_valid() .and. goal_node_id%is_valid()) then
            needs_extension = (this_node_id /= goal_node_id)
         else if (allocated(this%grid_id) .and. allocated(goal%grid_id)) then
            needs_extension = (this%grid_id /= goal%grid_id)
         else
            needs_extension = .not. same_set(this%dimensions, goal%dimensions)
         end if
      class default
         error stop 'VerticalCoordinateCharacteristic: needs_extension_for called with a non-VerticalCoordinateCharacteristic goal - should be checked by calling procedure'
      end select
   end function vcoord_needs_extension_for

   function get_grid_id(this) result(grid_id)
      class(VerticalCoordinateCharacteristic), intent(in) :: this
      character(:), allocatable :: grid_id

      grid_id = this%grid_id
   end function get_grid_id

   logical function has_grid_id(this) result(has)
      class(VerticalCoordinateCharacteristic), intent(in) :: this

      has = allocated(this%grid_id)
   end function has_grid_id

   function get_dimensions(this) result(dimensions)
      class(VerticalCoordinateCharacteristic), intent(in) :: this
      type(StringVector) :: dimensions

      dimensions = this%dimensions
   end function get_dimensions

   ! Moved from the retired mapl_VerticalGridCharacteristic_mod's own
   ! vgrid_build_transform - REQ-GEO-007a's three-way dimension-overlap
   ! classification, computed from `dimensions` regardless of which
   ! comparison path (node-identity, identity-token, or dimension-set)
   ! detected the mismatch. No real vertical-regrid Transform is
   ! implemented yet in any of the three cases - each _FAIL message is
   ! distinguishable so the outcome is diagnosable rather than one
   ! undifferentiated mismatch. needs_extension_for's own node-identity
   ! comparison above is unaffected - a different operation answering a
   ! different question on the same instance.
   subroutine vcoord_build_transform(this, goal, transformer, rc)
      class(VerticalCoordinateCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal
      class(TransformBase), allocatable, intent(out) :: transformer
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: outcome
      character(:), allocatable :: overlap_name
      character(:), allocatable :: overlap_list
      character(:), allocatable :: failure_message

      select type (goal)
      class is (VerticalCoordinateCharacteristic)
         outcome = classify_dimension_overlap(this%dimensions, goal%dimensions, overlap_name, overlap_list, _RC)
      class default
         _FAIL('VerticalCoordinateCharacteristic: goal is not a VerticalCoordinateCharacteristic')
      end select

      select case (outcome)
      case (DIMENSION_OVERLAP_SINGLE)
         ! Built as an ordinary local variable, not inline as the _FAIL
         ! macro's own argument - gfortran's preprocessor does not
         ! support Fortran's `&` continuation spanning a macro
         ! invocation's own argument list.
         failure_message = 'VerticalCoordinateCharacteristic: single overlapping physical dimension ' // &
              '"' // overlap_name // '" identified as the adaptation candidate, ' // &
              'but no real vertical-regrid Transform is implemented yet'
         _FAIL(failure_message)
      case (DIMENSION_OVERLAP_INCOMPATIBLE)
         failure_message = 'VerticalCoordinateCharacteristic: incompatible - no common physical dimension ' // &
              'between export and import vertical grids'
         _FAIL(failure_message)
      case (DIMENSION_OVERLAP_AMBIGUOUS)
         failure_message = 'VerticalCoordinateCharacteristic: ambiguous - more than one common physical ' // &
              'dimension between export and import vertical grids (' // overlap_list // ')'
         _FAIL(failure_message)
      case default
         _FAIL('VerticalCoordinateCharacteristic: unrecognized dimension-overlap outcome')
      end select
   end subroutine vcoord_build_transform

   ! Moved from the retired mapl_VerticalGridCharacteristic_mod's own
   ! vgrid_get_signature - includes both the identity token (when
   ! present) and the sorted dimension set (when non-empty), so two
   ! otherwise-identical-looking characteristics that differ in either
   ! respect get distinct reuse-cache keys. The NodeId-sharing case (no
   ! grid_id/dimensions set) falls back to the referenced node's own
   ! string form.
   function vcoord_get_signature(this) result(signature)
      class(VerticalCoordinateCharacteristic), intent(in) :: this
      character(:), allocatable :: signature

      type(NodeId) :: this_node_id

      if (allocated(this%grid_id)) then
         signature = 'vertical_grid=' // this%grid_id
      else
         this_node_id = this%get_referenced_node_id()
         if (this_node_id%is_valid()) then
            signature = 'vertical_grid_node=' // this_node_id%to_string()
         else
            signature = 'vertical_grid=<none>'
         end if
      end if
      if (this%dimensions%size() > 0) then
         signature = signature // ';dims=' // join(sorted_names(this%dimensions))
      end if
   end function vcoord_get_signature

   ! REQ-GEO-007a, as a small, pure, independently-testable helper (no
   ! ComponentGraph dependency): computes the set overlap between two
   ! physical-dimension-name sets and classifies the result.
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

end module mapl_VerticalCoordinateCharacteristic_mod
