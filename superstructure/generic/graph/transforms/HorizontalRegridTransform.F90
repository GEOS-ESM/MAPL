#include "MAPL.h"

!------------------------------------------------------------------------------
! HorizontalRegridTransform: regrids a field from the input field's geom to
! the output field's geom, for one regridder param. Field only.
!
! The regridder is looked up in RegridderManager at the first compute, from
! the geoms and typekinds of the bound fields, and looked up again whenever
! either geom or typekind changes. The lookup stays inside this Transform
! so that a later change in how regridders are owned stays local.
!
! Precondition: both geoms come from the geom manager (carry a MAPL geom
! id), since geoms are compared by that id; compute fails otherwise.
!
! Independent of the legacy layer: must not use the legacy Aspects (specs/),
! registry/, or the legacy transforms/.
!------------------------------------------------------------------------------
module mapl_HorizontalRegridTransform_mod
   use mapl_Transform_mod, only: Transform
   use mapl_PortNameGraphStateItemMap_mod, only: PortNameGraphStateItemMap
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_EsmfRegridder_mod, only: EsmfRegridderParam
   use mapl_Regridder_mod, only: Regridder
   use mapl_RegridderSpec_mod, only: RegridderSpec
   use mapl_RegridderManager_mod, only: RegridderManager, get_regridder_manager
   use mapl_geom_api, only: mapl_SameGeom, mapl_GeomGetId
   use mapl_GeomId_mod, only: GeomId
   use ESMF, only: ESMF_Field, ESMF_FieldGet, ESMF_Geom, ESMF_TypeKind_Flag
   use ESMF, only: ESMF_StateItem_Flag, ESMF_STATEITEM_FIELD
   use ESMF, only: operator(==), operator(/=)
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: HorizontalRegridTransform

   type, extends(Transform) :: HorizontalRegridTransform
      private
      type(EsmfRegridderParam) :: regridder_param
      type(ESMF_Geom) :: src_geom
      type(ESMF_Geom) :: dst_geom
      type(ESMF_TypeKind_Flag) :: typekind_in
      type(ESMF_TypeKind_Flag) :: typekind_out
      class(Regridder), pointer :: regrdr => null()
   contains
      procedure :: compute => regrid_compute
      procedure :: get_regridder_param
      procedure, private :: update_regridder_for
   end type HorizontalRegridTransform

   interface HorizontalRegridTransform
      module procedure new_HorizontalRegridTransform
   end interface HorizontalRegridTransform

   character(*), parameter :: INPUT_PORT_NAME = 'source_field'
   character(*), parameter :: OUTPUT_PORT_NAME = 'destination_field'

contains

   function new_HorizontalRegridTransform(regridder_param) result(transform)
      type(EsmfRegridderParam), intent(in) :: regridder_param
      type(HorizontalRegridTransform) :: transform

      integer :: status

      transform%regridder_param = regridder_param

      call transform%declare_input_port(INPUT_PORT_NAME, rc=status)
      call transform%declare_output_port(OUTPUT_PORT_NAME, rc=status)
   end function new_HorizontalRegridTransform

   subroutine regrid_compute(this, inputs, outputs, rc)
      class(HorizontalRegridTransform), intent(inout) :: this
      type(PortNameGraphStateItemMap), intent(in) :: inputs
      type(PortNameGraphStateItemMap), intent(in) :: outputs
      integer, intent(out) :: rc

      integer :: status
      type(ESMF_Field) :: f_in, f_out

      call get_port_field(inputs, INPUT_PORT_NAME, f_in, _RC)
      call get_port_field(outputs, OUTPUT_PORT_NAME, f_out, _RC)

      call this%update_regridder_for(f_in, f_out, _RC)
      call this%regrdr%regrid(f_in, f_out, _RC)

      _RETURN(_SUCCESS)
   end subroutine regrid_compute

   ! Looks up the regridder on first use and again when a field's geom or
   ! typekind no longer matches the one the regridder was made for. Geoms
   ! are compared by MAPL geom id, as RegridderManager does: each field
   ! holds its own ESMF_Geom object, so an alias test always differs.
   subroutine update_regridder_for(this, f_in, f_out, rc)
      class(HorizontalRegridTransform), intent(inout) :: this
      type(ESMF_Field), intent(inout) :: f_in, f_out
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Geom) :: geom_in, geom_out
      type(ESMF_TypeKind_Flag) :: typekind_in, typekind_out
      type(RegridderSpec) :: spec
      type(RegridderManager), pointer :: regridder_manager
      class(Regridder), pointer :: new_regrdr
      logical :: changed

      call ESMF_FieldGet(f_in, geom=geom_in, typekind=typekind_in, _RC)
      call ESMF_FieldGet(f_out, geom=geom_out, typekind=typekind_out, _RC)

      changed = .not. associated(this%regrdr)
      if (.not. changed) then
         changed = .not. mapl_SameGeom(geom_in, this%src_geom) &
              .or. .not. mapl_SameGeom(geom_out, this%dst_geom) &
              .or. (typekind_in /= this%typekind_in) &
              .or. (typekind_out /= this%typekind_out)
      end if
      _RETURN_UNLESS(changed)

      ! Cache nothing until the lookup succeeds, so a failed lookup cannot
      ! leave the old regridder paired with the new geoms.
      regridder_manager => get_regridder_manager()
      spec = RegridderSpec(this%regridder_param, geom_in, geom_out, &
           typekind_in=typekind_in, typekind_out=typekind_out)
      new_regrdr => regridder_manager%get_regridder(spec, _RC)

      this%regrdr => new_regrdr
      this%src_geom = geom_in
      this%dst_geom = geom_out
      this%typekind_in = typekind_in
      this%typekind_out = typekind_out

      _RETURN(_SUCCESS)
   end subroutine update_regridder_for

   subroutine get_port_field(items, port_name, field, rc)
      type(PortNameGraphStateItemMap), intent(in) :: items
      character(*), intent(in) :: port_name
      type(ESMF_Field), intent(out) :: field
      integer, optional, intent(out) :: rc

      integer :: status
      type(GraphStateItem), pointer :: item
      type(ESMF_StateItem_Flag) :: item_type
      type(ESMF_Geom) :: geom
      type(GeomId) :: id
      logical :: has_id
      character(:), allocatable :: msg

      item => items%at(port_name)
      msg = 'HorizontalRegridTransform: port not bound: ' // port_name
      _ASSERT(associated(item), msg)
      item_type = item%itemType(_RC)
      if (.not. (item_type == ESMF_STATEITEM_FIELD)) then
         msg = 'HorizontalRegridTransform: port is not a field: ' // port_name
         _FAIL(msg)
      end if
      field = item%get_field(_RC)

      ! Without an id every compute would look as if the geom changed, and
      ! each lookup would add another regridder to RegridderManager.
      call ESMF_FieldGet(field, geom=geom, _RC)
      id = mapl_GeomGetId(geom, isPresent=has_id, _RC)
      msg = 'HorizontalRegridTransform: geom has no MAPL geom id ' // &
           '(not from the geom manager) on port: ' // port_name
      _ASSERT(has_id, msg)

      _RETURN(_SUCCESS)
   end subroutine get_port_field

   function get_regridder_param(this) result(regridder_param)
      class(HorizontalRegridTransform), intent(in) :: this
      type(EsmfRegridderParam) :: regridder_param

      regridder_param = this%regridder_param
   end function get_regridder_param

end module mapl_HorizontalRegridTransform_mod
