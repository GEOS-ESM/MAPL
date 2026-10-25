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
! needs_extension_for pattern as VerticalCoordinateCharacteristic.F90
! (see that module's own header comment for the full rationale): a real
! referenced_node_id set on both sides (the GraphStateItem-sharing use
! case, REQ-CHAR-012) is authoritative and unchanged from this type's
! own pre-merge behavior; otherwise (the GraphBuilder-production-
! call-site case, which supplies only an opaque geom_id string token via
! GeomId%get_value() rendered to text, never a NodeId) falls back to the
! retired mapl_GeomCharacteristic_mod's own identity-token comparison.
!
! An item-level instance may also hold the ESMF_Geom itself, plus the
! horizontal_dims_spec and regridder param the item declares. Only the
! geometry identity is compared (like legacy GeomAspect%matches, plus
! horizontal_dims_spec); the regridder param is carried for write_info.
!
! Status: SPECIFIED when constructed with a value. DEFERRED means the
! owning component supplies the geometry later (resolve_deferred, GEOM_B).
! No geometry is ever UNCHECKED; MIRRORED only when the factory sets it.
!------------------------------------------------------------------------------
module mapl_GeometryCharacteristic_mod
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, ReferenceCharacteristic
   use mapl_StateItemCharacteristic_mod, only: SYNC_PHASE_STRUCTURAL, StateItemCharacteristicMap
   use mapl_StateItemCharacteristic_mod, only: report_build_transform_unsupported
   use mapl_CharacteristicStatus_mod, only: operator(==), CHARACTERISTIC_STATUS_SPECIFIED, CHARACTERISTIC_STATUS_DEFERRED, &
        CHARACTERISTIC_STATUS_MIRRORED
   use mapl_CharacteristicDefaults_mod, only: CharacteristicDefaults
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind, GEOMETRY_CHARACTERISTIC_KIND
   use mapl_NodeId_mod, only: NodeId, operator(==), operator(/=)
   use mapl_TransformBase_mod, only: TransformBase
   use mapl_HorizontalDimsSpec_mod, only: HorizontalDimsSpec, HORIZONTAL_DIMS_GEOM, operator(==), operator(/=)
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
      ! VerticalCoordinateCharacteristic.F90, design.md Risks).
      character(:), allocatable :: geom_id
      type(ESMF_Geom), allocatable :: geom
      type(HorizontalDimsSpec) :: horizontal_dims_spec = HORIZONTAL_DIMS_GEOM
      type(EsmfRegridderParam), allocatable :: regridder_param
   contains
      procedure, nopass :: get_kind => geometry_get_kind
      procedure :: needs_extension_for => geometry_needs_extension_for
      procedure :: build_transform => geometry_build_transform
      procedure :: get_signature => geometry_get_signature
      procedure :: write_info => geometry_write_info
      procedure :: is_structural => geometry_is_structural
      procedure :: resolve_deferred => geometry_resolve_deferred
      procedure :: get_geom_id
      procedure :: has_geom_id
      procedure :: get_geom
      procedure :: has_geom
      procedure :: get_horizontal_dims_spec
   end type GeometryCharacteristic

   interface GeometryCharacteristic
      module procedure new_GeometryCharacteristic
   end interface GeometryCharacteristic

contains

   function new_GeometryCharacteristic(referenced_node_id, geom_id, geom, horizontal_dims_spec, regridder_param) &
        result(characteristic)
      type(NodeId), optional, intent(in) :: referenced_node_id
      character(*), optional, intent(in) :: geom_id
      type(ESMF_Geom), optional, intent(in) :: geom
      type(HorizontalDimsSpec), optional, intent(in) :: horizontal_dims_spec
      type(EsmfRegridderParam), optional, intent(in) :: regridder_param
      type(GeometryCharacteristic) :: characteristic

      if (present(referenced_node_id)) call characteristic%set_referenced_node_id(referenced_node_id)
      if (present(geom_id)) characteristic%geom_id = geom_id
      if (present(geom)) allocate(characteristic%geom, source=geom)
      if (present(horizontal_dims_spec)) characteristic%horizontal_dims_spec = horizontal_dims_spec
      if (present(regridder_param)) allocate(characteristic%regridder_param, source=regridder_param)
      if (present(referenced_node_id) .or. present(geom_id) .or. present(geom)) then
         call characteristic%set_status(CHARACTERISTIC_STATUS_SPECIFIED)
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
         if (this%horizontal_dims_spec /= goal%horizontal_dims_spec) then
            needs_extension = .true.
            return
         end if
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

   ! Writes geom= (when held), horizontal_dims_spec= and the regridder
   ! param. Shapes with no Field or FieldBundle payload (service,
   ! expression) have nothing to write.
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
      _RETURN_UNLESS(this%get_status() == CHARACTERISTIC_STATUS_SPECIFIED)

      item_type = handle%itemType(_RC)
      if (allocated(this%regridder_param)) then
         regridder_param_info = this%regridder_param%make_info(_RC)
      end if

      if (item_type == ESMF_STATEITEM_FIELD) then
         field = handle%get_field(_RC)
         if (allocated(this%geom)) then
            call MAPL_FieldSet(field, geom=this%geom, horizontal_dims_spec=this%horizontal_dims_spec, &
                 regridder_param_info=regridder_param_info, _RC)
         else
            call MAPL_FieldSet(field, horizontal_dims_spec=this%horizontal_dims_spec, &
                 regridder_param_info=regridder_param_info, _RC)
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
   ! there is one. Otherwise it stays DEFERRED, to be filled by the
   ! component's own phase (freeze reports it if never filled).
   subroutine geometry_resolve_deferred(this, defaults, rc)
      class(GeometryCharacteristic), intent(inout) :: this
      type(CharacteristicDefaults), intent(in) :: defaults
      integer, optional, intent(out) :: rc

      type(GeomId) :: id
      character(20) :: id_buffer

      _RETURN_UNLESS(this%get_status() == CHARACTERISTIC_STATUS_DEFERRED)
      _RETURN_UNLESS(defaults%has_geom())

      if (allocated(this%geom)) deallocate(this%geom)
      allocate(this%geom, source=defaults%get_geom())
      id = defaults%get_geom_id()
      if (id%is_assigned()) then
         write(id_buffer, '(I0)') id%get_value()
         this%geom_id = trim(id_buffer)
      end if
      call this%set_status(CHARACTERISTIC_STATUS_SPECIFIED)

      _RETURN(_SUCCESS)
   end subroutine geometry_resolve_deferred

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

   function get_horizontal_dims_spec(this) result(horizontal_dims_spec)
      class(GeometryCharacteristic), intent(in) :: this
      type(HorizontalDimsSpec) :: horizontal_dims_spec

      horizontal_dims_spec = this%horizontal_dims_spec
   end function get_horizontal_dims_spec

end module mapl_GeometryCharacteristic_mod
