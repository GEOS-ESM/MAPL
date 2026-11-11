#include "MAPL.h"

!------------------------------------------------------------------------------
! VerticalGridCharacteristic: the "vertical grid" StateItemCharacteristic
! ReferenceCharacteristic, same shape as GeometryCharacteristic.F90: holds a
! NodeId referencing a shared graph node, not a value of its own.
!
! It also carries grid_id/dimensions value data for the dimension-overlap
! build_transform and get_signature. needs_extension_for is therefore a
! hybrid comparison, chosen by the data the two sides carry:
!   - when a real referenced_node_id is set on BOTH sides (sharing is
!     two map entries holding the same NodeId), that identity
!     comparison is authoritative.
!   - when both sides hold a VerticalGrid, the legacy rule: same grid id,
!     or the export grid matches the import grid (VerticalGrid%matches: a
!     basic grid matches any grid with the same level count).
!   - otherwise (a GraphBuilder call site that supplies only an opaque
!     grid_id or dimension set), the identity-token-else-dimension-set
!     comparison is used.
!
! A Field or VECTOR mismatch between two held grids that share one
! dimension, both with level values for it, is a VerticalColumnRegridTransform
! step that also covers the orientation; the export must be at level
! centers. Two basic grids with different level counts are incompatible. Any
! other mismatch (a grid without level values, e.g. a model grid; a
! non-center stagger; an R8 tip; any other item class, such as a plain
! FIELDBUNDLE or a BRACKET) reports unsupported. A BRACKET becomes a Field
! before this step.
!
! An item-level instance may also hold the VerticalGrid itself (written by
! write_info). Status: SPECIFIED when constructed with a value. DEFERRED
! means the owning component supplies its vertical grid (resolve_deferred,
! GEOM_B); a DEFERRED entry whose component has none is dropped
! (drop_if_unsupplied), because the item has no vertical dimension.
!------------------------------------------------------------------------------
module mapl_VerticalGridCharacteristic_mod
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, ReferenceCharacteristic
   use mapl_StateItemCharacteristic_mod, only: SYNC_PHASE_STRUCTURAL, report_build_transform_unsupported
   use mapl_StateItemCharacteristic_mod, only: &
        report_build_transform_incompatible
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristicMap
   use mapl_CharacteristicStatus_mod, only: operator(==), CHARACTERISTIC_STATUS_SPECIFIED, CHARACTERISTIC_STATUS_DEFERRED
   use mapl_CharacteristicDefaults_mod, only: CharacteristicDefaults
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_VerticalGrid_mod, only: VerticalGrid
   use mapl_BasicVerticalGrid_mod, only: BasicVerticalGrid
   use mapl_ItemShapeCharacteristic_mod, only: ItemShapeCharacteristic
   use mapl_VerticalStaggerCharacteristic_mod, only: &
        VerticalStaggerCharacteristic
   use mapl_TypeKindCharacteristic_mod, only: TypeKindCharacteristic
   use mapl_VerticalStaggerLoc_mod, only: VerticalStaggerLoc, &
        VERTICAL_STAGGER_CENTER
   use mapl_StateItem_mod, only: MAPL_STATEITEM_FIELD, MAPL_STATEITEM_VECTOR
   use mapl_VerticalColumnRegridTransform_mod, only: &
        VerticalColumnRegridTransform
   use mapl_field_api, only: MAPL_FieldSet
   use mapl_field_bundle_api, only: MAPL_FieldBundleSet
   use ESMF, only: ESMF_Field, ESMF_FieldBundle, ESMF_StateItem_Flag, ESMF_STATEITEM_FIELD, ESMF_STATEITEM_FIELDBUNDLE
   use ESMF, only: operator(==)
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind, VERTICAL_GRID_CHARACTERISTIC_KIND
   use mapl_StateItemCharacteristicKind_mod, only: &
        VERTICAL_ORIENTATION_CHARACTERISTIC_KIND, &
        VERTICAL_STAGGER_CHARACTERISTIC_KIND, &
        ITEM_SHAPE_CHARACTERISTIC_KIND, TYPE_KIND_CHARACTERISTIC_KIND
   use mapl_NodeId_mod, only: NodeId, operator(==), operator(/=)
   use mapl_TransformBase_mod, only: TransformBase
   use gFTL2_StringVector, only: StringVector
   use mapl_StringSortUtilities_mod, only: sorted_names
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

   type, extends(ReferenceCharacteristic) :: VerticalGridCharacteristic
      private
      character(:), allocatable :: grid_id
      type(StringVector) :: dimensions
      class(VerticalGrid), allocatable :: vgrid
   contains
      procedure, nopass :: get_kind => vcoord_get_kind
      procedure :: needs_extension_for => vcoord_needs_extension_for
      procedure :: build_transform => vcoord_build_transform
      procedure :: covered_kinds => vcoord_covered_kinds
      procedure :: build_transform_for_item => vcoord_build_transform_for_item
      procedure :: get_signature => vcoord_get_signature
      procedure :: write_info => vcoord_write_info
      procedure :: is_structural => vcoord_is_structural
      procedure :: resolve_deferred => vcoord_resolve_deferred
      procedure :: drop_if_unsupplied => vcoord_drop_if_unsupplied
      procedure :: set_from_vgrid
      procedure :: get_vgrid
      procedure :: has_vgrid
      procedure :: get_grid_id
      procedure :: has_grid_id
      procedure :: get_dimensions
   end type VerticalGridCharacteristic

   interface VerticalGridCharacteristic
      module procedure new_VerticalGridCharacteristic
   end interface VerticalGridCharacteristic

contains

   ! referenced_node_id: the NodeId of the ordinary graph node this
   ! characteristic points at - set via the inherited
   ! ReferenceCharacteristic%set_referenced_node_id accessor, not a bare
   ! field assignment (that component is private to
   ! mapl_StateItemCharacteristic_mod). OPTIONAL: a GraphBuilder call site
   ! supplies grid_id/dimensions, a GraphStateItem-sharing call site supplies
   ! only referenced_node_id.
   ! A given vgrid supplies grid_id and dimensions unless they are also given.
   function new_VerticalGridCharacteristic(referenced_node_id, grid_id, dimensions, vgrid) result(characteristic)
      type(NodeId), optional, intent(in) :: referenced_node_id
      character(*), optional, intent(in) :: grid_id
      type(StringVector), optional, intent(in) :: dimensions
      class(VerticalGrid), optional, intent(in) :: vgrid
      type(VerticalGridCharacteristic) :: characteristic

      if (present(referenced_node_id)) call characteristic%set_referenced_node_id(referenced_node_id)
      if (present(vgrid)) call characteristic%set_from_vgrid(vgrid)
      if (present(grid_id)) characteristic%grid_id = grid_id
      if (present(dimensions)) characteristic%dimensions = dimensions
      if (present(referenced_node_id) .or. present(grid_id) .or. present(dimensions) .or. present(vgrid)) &
           call characteristic%set_status(CHARACTERISTIC_STATUS_SPECIFIED)
   end function new_VerticalGridCharacteristic

   subroutine set_from_vgrid(this, vgrid)
      class(VerticalGridCharacteristic), intent(inout) :: this
      class(VerticalGrid), intent(in) :: vgrid

      character(20) :: id_buffer

      if (allocated(this%vgrid)) deallocate(this%vgrid)
      allocate(this%vgrid, source=vgrid)
      write(id_buffer, '(I0)') vgrid%get_id()
      this%grid_id = trim(id_buffer)
      this%dimensions = vgrid%get_supported_physical_dimensions()
   end subroutine set_from_vgrid

   function vcoord_get_kind() result(kind)
      type(StateItemCharacteristicKind) :: kind

      kind = VERTICAL_GRID_CHARACTERISTIC_KIND
   end function vcoord_get_kind

   ! `goal` is guaranteed a VerticalGridCharacteristic for the real
   ! call chain this method is used in (same guarantee documented on
   ! GeometryCharacteristic/PhysicalUnitsCharacteristic: StateItemDescriptor%
   ! set_characteristic asserts kind match before inserting). The class
   ! default branch below should be checked by the calling procedure.
   !
   ! Real NodeId on both sides is authoritative; then two held grids (legacy
   ! rule); otherwise the identity token, else the dimension set, decides
   ! (module header).
   logical function vcoord_needs_extension_for(this, goal) result(needs_extension)
      class(VerticalGridCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal

      type(NodeId) :: this_node_id, goal_node_id

      select type (goal)
      class is (VerticalGridCharacteristic)
         this_node_id = this%get_referenced_node_id()
         goal_node_id = goal%get_referenced_node_id()
         if (this_node_id%is_valid() .and. goal_node_id%is_valid()) then
            needs_extension = (this_node_id /= goal_node_id)
         else if (allocated(this%vgrid) .and. allocated(goal%vgrid)) then
            needs_extension = .not. grids_match(this%vgrid, goal%vgrid)
         else if (allocated(this%grid_id) .and. allocated(goal%grid_id)) then
            needs_extension = (this%grid_id /= goal%grid_id)
         else
            needs_extension = .not. same_set(this%dimensions, goal%dimensions)
         end if
      class default
         error stop 'VerticalGridCharacteristic: needs_extension_for called with a non-VerticalGridCharacteristic goal - should be checked by calling procedure'
      end select
   end function vcoord_needs_extension_for

   ! Writes vgrid= (when held). Shapes with no Field or FieldBundle payload
   ! have nothing to write.
   subroutine vcoord_write_info(this, handle, phase, rc)
      class(VerticalGridCharacteristic), intent(in) :: this
      type(GraphStateItem), intent(inout) :: handle
      integer, intent(in) :: phase
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_StateItem_Flag) :: item_type
      type(ESMF_Field) :: field
      type(ESMF_FieldBundle) :: bundle

      _RETURN_UNLESS(phase == SYNC_PHASE_STRUCTURAL)
      _RETURN_UNLESS(this%get_status() == CHARACTERISTIC_STATUS_SPECIFIED)
      _RETURN_UNLESS(allocated(this%vgrid))

      item_type = handle%itemType(_RC)
      if (item_type == ESMF_STATEITEM_FIELD) then
         field = handle%get_field(_RC)
         call MAPL_FieldSet(field, vgrid=this%vgrid, _RC)
      else if (item_type == ESMF_STATEITEM_FIELDBUNDLE) then
         bundle = handle%get_field_bundle(_RC)
         call MAPL_FieldBundleSet(bundle, vgrid=this%vgrid, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine vcoord_write_info

   logical function vcoord_is_structural(this) result(structural)
      class(VerticalGridCharacteristic), intent(in) :: this

      structural = .true.
      _UNUSED_DUMMY(this)
   end function vcoord_is_structural

   ! A DEFERRED vertical coordinate becomes SPECIFIED with the component's
   ! vertical grid when it has one; otherwise it stays DEFERRED.
   subroutine vcoord_resolve_deferred(this, defaults, rc)
      class(VerticalGridCharacteristic), intent(inout) :: this
      type(CharacteristicDefaults), intent(in) :: defaults
      integer, optional, intent(out) :: rc

      class(VerticalGrid), allocatable :: vgrid

      _RETURN_UNLESS(this%get_status() == CHARACTERISTIC_STATUS_DEFERRED)
      _RETURN_UNLESS(defaults%has_vgrid())

      call defaults%get_vgrid(vgrid)
      call this%set_from_vgrid(vgrid)
      call this%set_status(CHARACTERISTIC_STATUS_SPECIFIED)

      _RETURN(_SUCCESS)
   end subroutine vcoord_resolve_deferred

   ! An item whose component has no vertical grid has no vertical dimension.
   logical function vcoord_drop_if_unsupplied(this, defaults) result(drop)
      class(VerticalGridCharacteristic), intent(in) :: this
      type(CharacteristicDefaults), intent(in) :: defaults

      drop = this%get_status() == CHARACTERISTIC_STATUS_DEFERRED .and. .not. defaults%has_vgrid()
   end function vcoord_drop_if_unsupplied

   function get_vgrid(this) result(vgrid)
      class(VerticalGridCharacteristic), intent(in) :: this
      class(VerticalGrid), allocatable :: vgrid

      allocate(vgrid, source=this%vgrid)
   end function get_vgrid

   logical function has_vgrid(this) result(has)
      class(VerticalGridCharacteristic), intent(in) :: this

      has = allocated(this%vgrid)
   end function has_vgrid

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

   ! Three-way dimension-overlap classification, computed from
   ! `dimensions` whichever comparison path detected the mismatch. A single
   ! overlap needs both grids held, each with level values in that
   ! dimension; two basic grids (no level values) cannot connect.
   subroutine vcoord_build_transform(this, goal, transformer, rc)
      class(VerticalGridCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal
      class(TransformBase), allocatable, intent(out) :: transformer
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: outcome
      character(:), allocatable :: overlap_name
      character(:), allocatable :: overlap_list
      character(:), allocatable :: failure_message

      select type (goal)
      class is (VerticalGridCharacteristic)
         outcome = classify_dimension_overlap(this%dimensions, goal%dimensions, overlap_name, overlap_list, _RC)

         select case (outcome)
         case (DIMENSION_OVERLAP_SINGLE)
            if (.not. (allocated(this%vgrid) .and. allocated(goal%vgrid))) then
               call report_build_transform_unsupported(rc)
            else if (is_basic(this%vgrid) .and. is_basic(goal%vgrid)) then
               call report_build_transform_incompatible(rc)
            else if (this%vgrid%has_column_coordinate(overlap_name) .and. &
                 goal%vgrid%has_column_coordinate(overlap_name)) then
               allocate(transformer, source=VerticalColumnRegridTransform())
               _RETURN(_SUCCESS)
            else
               ! A model grid's levels are a field (PLE, ZLE), which the
               ! graph cannot yet bind as a Transform input.
               call report_build_transform_unsupported(rc)
            end if
         case (DIMENSION_OVERLAP_INCOMPATIBLE)
            failure_message = 'VerticalGridCharacteristic: incompatible - ' // &
                 'no common physical dimension between export and ' // &
                 'import vertical grids'
            _FAIL(failure_message)
         case (DIMENSION_OVERLAP_AMBIGUOUS)
            failure_message = 'VerticalGridCharacteristic: ambiguous - ' // &
                 'more than one common physical dimension between ' // &
                 'export and import vertical grids (' // &
                 overlap_list // ')'
            _FAIL(failure_message)
         case default
            failure_message = 'VerticalGridCharacteristic: ' // &
                 'unrecognized dimension-overlap outcome'
            _FAIL(failure_message)
         end select
      class default
         _FAIL('VerticalGridCharacteristic: goal is not a VerticalGridCharacteristic')
      end select
   end subroutine vcoord_build_transform

   ! The regrid writes the import's orientation, so it also resolves an
   ! orientation difference. A stagger difference stays its own step, which
   ! reports unsupported: the regrid needs level centers on both sides.
   function vcoord_covered_kinds(this, tip, goal) result(kinds)
      class(VerticalGridCharacteristic), intent(in) :: this
      type(StateItemCharacteristicMap), target, intent(in) :: tip
      type(StateItemCharacteristicMap), target, intent(in) :: goal
      type(StateItemCharacteristicKind), allocatable :: kinds(:)

      kinds = [this%get_kind()]
      if (tip%count(VERTICAL_ORIENTATION_CHARACTERISTIC_KIND) > 0 .and. &
           goal%count(VERTICAL_ORIENTATION_CHARACTERISTIC_KIND) > 0) then
         kinds = [kinds, VERTICAL_ORIENTATION_CHARACTERISTIC_KIND]
      end if
   end function vcoord_covered_kinds

   ! Only a Field or a VECTOR has a vertical regrid, and only at level
   ! centers. A tip with no shape entry (component-level wiring), or a pair
   ! not holding both grids, is not restricted. The regrid is R4 only, so an
   ! R8 tip is unsupported at connect rather than failing at compute.
   subroutine vcoord_build_transform_for_item(this, goal, tip, transformer, rc)
      class(VerticalGridCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal
      type(StateItemCharacteristicMap), target, intent(in) :: tip
      class(TransformBase), allocatable, intent(out) :: transformer
      integer, optional, intent(out) :: rc

      integer :: status
      class(StateItemCharacteristic), pointer :: entry
      type(VerticalStaggerLoc) :: stagger

      if (tip%count(ITEM_SHAPE_CHARACTERISTIC_KIND) == 0 .or. &
           .not. holds_both(this, goal)) then
         call this%build_transform(goal, transformer, rc)
         return
      end if

      entry => tip%at(ITEM_SHAPE_CHARACTERISTIC_KIND, _RC)
      select type (entry)
      type is (ItemShapeCharacteristic)
         if (.not. (entry%get_item_class() == MAPL_STATEITEM_FIELD .or. &
              entry%get_item_class() == MAPL_STATEITEM_VECTOR)) then
            call report_build_transform_unsupported(rc)
            return
         end if
      class default
         _FAIL('VerticalGridCharacteristic: ITEM_SHAPE entry has wrong type')
      end select

      if (tip%count(VERTICAL_STAGGER_CHARACTERISTIC_KIND) > 0) then
         entry => tip%at(VERTICAL_STAGGER_CHARACTERISTIC_KIND, _RC)
         select type (entry)
         type is (VerticalStaggerCharacteristic)
            stagger = entry%get_vertical_stagger()
            if (stagger%to_string() /= VERTICAL_STAGGER_CENTER%to_string()) then
               call report_build_transform_unsupported(rc)
               return
            end if
         class default
            _FAIL('VerticalGridCharacteristic: bad VERTICAL_STAGGER entry')
         end select
      end if

      if (tip%count(TYPE_KIND_CHARACTERISTIC_KIND) > 0) then
         entry => tip%at(TYPE_KIND_CHARACTERISTIC_KIND, _RC)
         select type (entry)
         type is (TypeKindCharacteristic)
            if (entry%get_type_kind() == 'R8') then
               call report_build_transform_unsupported(rc)
               return
            end if
         end select
      end if

      call this%build_transform(goal, transformer, rc)
   end subroutine vcoord_build_transform_for_item

   ! Legacy VerticalGridAspect rule: the same grid, or the export grid
   ! matches the import grid (a basic grid matches on level count alone).
   logical function grids_match(export_grid, import_grid)
      class(VerticalGrid), intent(in) :: export_grid
      class(VerticalGrid), intent(in) :: import_grid

      grids_match = export_grid%get_id() == import_grid%get_id()
      if (.not. grids_match) grids_match = export_grid%matches(import_grid)
   end function grids_match

   ! Only a pair holding both grids can get a Transform; any other pair
   ! (e.g. VERTICALGRID composites, which hold dimension names) keeps the
   ! plain dimension-overlap outcome whatever the item shape.
   logical function holds_both(this, goal)
      class(VerticalGridCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal

      holds_both = .false.
      select type (goal)
      class is (VerticalGridCharacteristic)
         holds_both = allocated(this%vgrid) .and. allocated(goal%vgrid)
      end select
   end function holds_both

   logical function is_basic(vgrid)
      class(VerticalGrid), intent(in) :: vgrid

      select type (vgrid)
      type is (BasicVerticalGrid)
         is_basic = .true.
      class default
         is_basic = .false.
      end select
   end function is_basic

   ! Includes the identity token (when present) and the sorted dimension set
   ! (when non-empty), so characteristics differing in either get distinct keys.
   function vcoord_get_signature(this) result(signature)
      class(VerticalGridCharacteristic), intent(in) :: this
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

   ! A small, pure, independently-testable helper (no
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
   ! level").
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

end module mapl_VerticalGridCharacteristic_mod
