#include "MAPL.h"

!------------------------------------------------------------------------------
! GeometryCharacteristic: the "geometry" StateItemCharacteristic
! (docs/graph/spec/18-state-item-characteristics.md REQ-CHAR-001 table) -
! a ReferenceCharacteristic (REQ-CHAR-002a/002b: holds a NodeId
! referencing a shared graph node, not a value of its own).
!
! Named GeometryCharacteristic, NOT GeomCharacteristic (the spec table's
! own suggested name) - design.md D3: an exact, unavoidable collision
! with the existing, unrelated mapl_GeomCharacteristic_mod/
! GeomCharacteristic type already shipped for graph/extension-reuse
! (Fortran module/type names share one global namespace). Same
! collision-avoidance discipline already applied once in this codebase
! for UnitsConverterTransform vs. legacy ConvertUnitsTransform.
!
! Absorbs the retired mapl_GeomCharacteristic_mod's own opaque geom_id
! string-token identity and its (always-failing) build_transform/get_signature,
! in addition to this type's own pre-existing NodeId-sharing role - same HYBRID
! needs_extension_for pattern as VerticalGridCharacteristic.F90
! (see that module's own header comment for the full rationale): a real
! referenced_node_id set on both sides (the GraphStateItem-sharing use
! case, REQ-CHAR-012) is authoritative and unchanged from this type's
! own pre-merge behavior; otherwise (the GraphBuilder-production-
! call-site case, which supplies only an opaque geom_id string token via
! GeomId%get_value() rendered to text, never a NodeId) falls back to the
! retired mapl_GeomCharacteristic_mod's own identity-token comparison.
!
! An item-level instance may also hold the ESMF_Geom itself, plus the
! regridder param the item declares. Only the geometry identity is
! compared (like legacy GeomAspect%matches). The regridder param is written
! to Info and chooses the regrid method (select_regridder_param).
!
! Status: SPECIFIED when constructed with a value. DEFERRED means the
! owning component supplies the geometry later (resolve_deferred, GEOM_B).
! MIRRORED only when the factory sets it.
!
! An item with no horizontal dimensions (HorizontalDimsSpec NONE) is not
! compared, but field creation still needs a geom. Such an instance is
! built with compared=.false.: it is UNCHECKED instead of SPECIFIED or
! DEFERRED, and is written to Info only when it holds a geom.
!------------------------------------------------------------------------------
module mapl_GeometryCharacteristic_mod
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, ReferenceCharacteristic
   use mapl_StateItemCharacteristic_mod, only: SYNC_PHASE_STRUCTURAL, StateItemCharacteristicMap
   use mapl_StateItemCharacteristic_mod, only: report_build_transform_unsupported
   use mapl_CharacteristicStatus_mod, only: CharacteristicStatus, operator(==), CHARACTERISTIC_STATUS_SPECIFIED, CHARACTERISTIC_STATUS_DEFERRED, &
        CHARACTERISTIC_STATUS_MIRRORED, CHARACTERISTIC_STATUS_UNCHECKED
   use mapl_CharacteristicDefaults_mod, only: CharacteristicDefaults
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind, GEOMETRY_CHARACTERISTIC_KIND
   use mapl_NodeId_mod, only: NodeId, operator(==), operator(/=)
   use mapl_TransformBase_mod, only: TransformBase
   use mapl_EsmfRegridder_mod, only: EsmfRegridderParam
   use mapl_GeomId_mod, only: GeomId
   use mapl_geom_api, only: mapl_SameGeom
   use mapl_field_api, only: MAPL_FieldSet
   use mapl_field_bundle_api, only: MAPL_FieldBundleSet
   use ESMF, only: ESMF_Geom, ESMF_Field, ESMF_FieldBundle, ESMF_Info, ESMF_InfoDestroy
   use ESMF, only: ESMF_StateItem_Flag, ESMF_STATEITEM_FIELD, ESMF_STATEITEM_FIELDBUNDLE
   use ESMF, only: operator(==)
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: GeometryCharacteristic
   public :: insert_geom_id_characteristic

   type, extends(ReferenceCharacteristic) :: GeometryCharacteristic
      private
      ! Moved from the retired mapl_GeomCharacteristic_mod - the opaque
      ! identity token, carried alongside the inherited
      ! referenced_node_id (same deliberate mix as
      ! VerticalGridCharacteristic.F90, design.md Risks).
      character(:), allocatable :: geom_id
      type(ESMF_Geom), allocatable :: geom
      type(EsmfRegridderParam), allocatable :: regridder_param
      logical :: compared = .true.
   contains
      procedure, nopass :: get_kind => geometry_get_kind
      procedure :: needs_extension_for => geometry_needs_extension_for
      procedure :: build_transform => geometry_build_transform
      procedure :: get_signature => geometry_get_signature
      procedure :: write_info => geometry_write_info
      procedure :: is_structural => geometry_is_structural
      procedure :: resolve_deferred => geometry_resolve_deferred
      procedure :: is_complete => geometry_is_complete
      procedure :: get_geom_id
      procedure :: has_geom_id
      procedure :: get_geom
      procedure :: has_geom
      procedure :: get_regridder_param
      procedure :: has_regridder_param
      procedure :: select_regridder_param
   end type GeometryCharacteristic

   interface GeometryCharacteristic
      module procedure new_GeometryCharacteristic
   end interface GeometryCharacteristic

contains

   function new_GeometryCharacteristic(referenced_node_id, geom_id, geom, regridder_param, compared) &
        result(characteristic)
      type(NodeId), optional, intent(in) :: referenced_node_id
      character(*), optional, intent(in) :: geom_id
      type(ESMF_Geom), optional, intent(in) :: geom
      type(EsmfRegridderParam), optional, intent(in) :: regridder_param
      logical, optional, intent(in) :: compared
      type(GeometryCharacteristic) :: characteristic

      if (present(referenced_node_id)) call characteristic%set_referenced_node_id(referenced_node_id)
      if (present(geom_id)) characteristic%geom_id = geom_id
      if (present(geom)) allocate(characteristic%geom, source=geom)
      if (present(regridder_param)) allocate(characteristic%regridder_param, source=regridder_param)
      if (present(compared)) characteristic%compared = compared
      if (present(referenced_node_id) .or. present(geom_id) .or. present(geom)) then
         call characteristic%set_status(settled_status(characteristic%compared))
      end if
   end function new_GeometryCharacteristic

   ! Inserts a SPECIFIED geometry entry identified only by `geom_id`, the
   ! form the component-level geometry wiring compares.
   subroutine insert_geom_id_characteristic(characteristics, geom_id)
      type(StateItemCharacteristicMap), intent(inout) :: characteristics
      type(GeomId), intent(in) :: geom_id

      character(20) :: id_buffer

      write(id_buffer, '(I0)') geom_id%get_value()
      call characteristics%insert(GEOMETRY_CHARACTERISTIC_KIND, GeometryCharacteristic(geom_id=trim(id_buffer)))
   end subroutine insert_geom_id_characteristic

   function geometry_get_kind() result(kind)
      type(StateItemCharacteristicKind) :: kind

      kind = GEOMETRY_CHARACTERISTIC_KIND
   end function geometry_get_kind

   logical function geometry_needs_extension_for(this, goal) result(needs_extension)
      class(GeometryCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal

      type(NodeId) :: this_node_id, goal_node_id

      select type (goal)
      class is (GeometryCharacteristic)
         this_node_id = this%get_referenced_node_id()
         goal_node_id = goal%get_referenced_node_id()
         if (this_node_id%is_valid() .and. goal_node_id%is_valid()) then
            needs_extension = (this_node_id /= goal_node_id)
         else if (allocated(this%geom) .and. allocated(goal%geom)) then
            needs_extension = .not. mapl_SameGeom(this%geom, goal%geom)
         else if (allocated(this%geom_id) .and. allocated(goal%geom_id)) then
            needs_extension = (this%geom_id /= goal%geom_id)
         else
            needs_extension = .true.
         end if
      class default
         error stop 'GeometryCharacteristic: needs_extension_for called with a non-GeometryCharacteristic goal - should be checked by calling procedure'
      end select
   end function geometry_needs_extension_for

   subroutine geometry_build_transform(this, goal, transformer, rc)
      class(GeometryCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal
      class(TransformBase), allocatable, intent(out) :: transformer
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(goal)

      ! No horizontal-regrid Transform exists yet.
      call report_build_transform_unsupported(rc)
   end subroutine geometry_build_transform

   function geometry_get_signature(this) result(signature)
      class(GeometryCharacteristic), intent(in) :: this
      character(:), allocatable :: signature

      type(NodeId) :: this_node_id

      if (allocated(this%geom_id)) then
         signature = 'geom=' // this%geom_id
      else
         this_node_id = this%get_referenced_node_id()
         signature = 'geom_node=' // this_node_id%to_string()
      end if
   end function geometry_get_signature

   ! Writes geom= (when held) and the regridder param. UNCHECKED is written
   ! only when it holds a geom, since it is then an uncompared geometry (not
   ! a valueless wildcard). Shapes with no Field or FieldBundle payload
   ! (service, expression) have nothing to write.
   subroutine geometry_write_info(this, handle, phase, rc)
      class(GeometryCharacteristic), intent(in) :: this
      type(GraphStateItem), intent(inout) :: handle
      integer, intent(in) :: phase
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_StateItem_Flag) :: item_type
      type(ESMF_Field) :: field
      type(ESMF_FieldBundle) :: bundle
      type(ESMF_Info), allocatable :: regridder_param_info

      _RETURN_UNLESS(phase == SYNC_PHASE_STRUCTURAL)
      if (this%get_status() == CHARACTERISTIC_STATUS_UNCHECKED) then
         _RETURN_UNLESS(allocated(this%geom))
      else
         _RETURN_UNLESS(this%get_status() == CHARACTERISTIC_STATUS_SPECIFIED)
      end if

      item_type = handle%itemType(_RC)
      if (allocated(this%regridder_param)) then
         regridder_param_info = this%regridder_param%make_info(_RC)
      end if

      if (item_type == ESMF_STATEITEM_FIELD) then
         field = handle%get_field(_RC)
         if (allocated(this%geom)) then
            call MAPL_FieldSet(field, geom=this%geom, regridder_param_info=regridder_param_info, _RC)
         else if (allocated(regridder_param_info)) then
            call MAPL_FieldSet(field, regridder_param_info=regridder_param_info, _RC)
         end if
      else if (item_type == ESMF_STATEITEM_FIELDBUNDLE) then
         bundle = handle%get_field_bundle(_RC)
         if (allocated(this%geom)) then
            call MAPL_FieldBundleSet(bundle, geom=this%geom, regridder_param_info=regridder_param_info, _RC)
         else if (allocated(regridder_param_info)) then
            call MAPL_FieldBundleSet(bundle, regridder_param_info=regridder_param_info, _RC)
         end if
      end if

      if (allocated(regridder_param_info)) then
         call ESMF_InfoDestroy(regridder_param_info, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine geometry_write_info

   logical function geometry_is_structural(this) result(structural)
      class(GeometryCharacteristic), intent(in) :: this

      structural = .true.
      _UNUSED_DUMMY(this)
   end function geometry_is_structural

   ! A DEFERRED geometry becomes SPECIFIED with the offered geometry when
   ! there is one; an uncompared one with no geom yet takes it and stays
   ! UNCHECKED. Otherwise it stays DEFERRED, to be filled by the
   ! component's own phase (freeze reports it if never filled).
   subroutine geometry_resolve_deferred(this, defaults, rc)
      class(GeometryCharacteristic), intent(inout) :: this
      type(CharacteristicDefaults), intent(in) :: defaults
      integer, optional, intent(out) :: rc

      type(GeomId) :: id
      character(20) :: id_buffer

      if (this%get_status() == CHARACTERISTIC_STATUS_UNCHECKED) then
         _RETURN_IF(this%compared .or. allocated(this%geom))
      else
         _RETURN_UNLESS(this%get_status() == CHARACTERISTIC_STATUS_DEFERRED)
      end if
      _RETURN_UNLESS(defaults%has_geom())

      if (allocated(this%geom)) deallocate(this%geom)
      allocate(this%geom, source=defaults%get_geom())
      id = defaults%get_geom_id()
      if (id%is_assigned()) then
         write(id_buffer, '(I0)') id%get_value()
         this%geom_id = trim(id_buffer)
      end if
      call this%set_status(settled_status(this%compared))

      _RETURN(_SUCCESS)
   end subroutine geometry_resolve_deferred

   ! A settled item-level entry must hold the geom its field is created on.
   ! A MIRRORED entry is filled from its export, never created from.
   logical function geometry_is_complete(this) result(complete)
      class(GeometryCharacteristic), intent(in) :: this

      complete = .true.
      if (this%get_status() == CHARACTERISTIC_STATUS_MIRRORED) return
      complete = allocated(this%geom)
   end function geometry_is_complete

   function settled_status(compared) result(status)
      logical, intent(in) :: compared
      type(CharacteristicStatus) :: status

      if (compared) then
         status = CHARACTERISTIC_STATUS_SPECIFIED
      else
         status = CHARACTERISTIC_STATUS_UNCHECKED
      end if
   end function settled_status

   function get_geom_id(this) result(geom_id)
      class(GeometryCharacteristic), intent(in) :: this
      character(:), allocatable :: geom_id

      geom_id = this%geom_id
   end function get_geom_id

   logical function has_geom_id(this) result(has)
      class(GeometryCharacteristic), intent(in) :: this

      has = allocated(this%geom_id)
   end function has_geom_id

   function get_geom(this) result(geom)
      class(GeometryCharacteristic), intent(in) :: this
      type(ESMF_Geom) :: geom

      geom = this%geom
   end function get_geom

   logical function has_geom(this) result(has)
      class(GeometryCharacteristic), intent(in) :: this

      has = allocated(this%geom)
   end function has_geom

   ! A subroutine, not a function with rc: the result has allocatable
   ! components and would be undefined on failure.
   subroutine get_regridder_param(this, regridder_param, rc)
      class(GeometryCharacteristic), intent(in) :: this
      type(EsmfRegridderParam), intent(out) :: regridder_param
      integer, optional, intent(out) :: rc

      character(*), parameter :: MSG = &
           'GeometryCharacteristic: no regridder param declared'

      _ASSERT(allocated(this%regridder_param), MSG)
      regridder_param = this%regridder_param

      _RETURN(_SUCCESS)
   end subroutine get_regridder_param

   logical function has_regridder_param(this) result(has)
      class(GeometryCharacteristic), intent(in) :: this

      has = allocated(this%regridder_param)
   end function has_regridder_param

   ! Regridder param for regridding from `this` (source) to `goal`
   ! (destination): the one side that declares one, else the default.
   ! Declaring one on both sides is an error, even if they agree.
   subroutine select_regridder_param(this, goal, regridder_param, rc)
      class(GeometryCharacteristic), intent(in) :: this
      class(GeometryCharacteristic), intent(in) :: goal
      type(EsmfRegridderParam), intent(out) :: regridder_param
      integer, optional, intent(out) :: rc

      character(*), parameter :: BOTH_MSG = 'GeometryCharacteristic: ' // &
           'source and destination both specify a regridder param; only one may'

      if (allocated(this%regridder_param) .and. &
           allocated(goal%regridder_param)) then
         _FAIL(BOTH_MSG)
      else if (allocated(goal%regridder_param)) then
         regridder_param = goal%regridder_param
      else if (allocated(this%regridder_param)) then
         regridder_param = this%regridder_param
      else
         regridder_param = EsmfRegridderParam()
      end if

      _RETURN(_SUCCESS)
   end subroutine select_regridder_param

end module mapl_GeometryCharacteristic_mod
