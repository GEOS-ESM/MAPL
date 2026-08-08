#include "MAPL.h"

module mapl_GeometryClassAspect_mod
   use mapl_ActualConnectionPt_mod
   use mapl_AspectId_mod
   use mapl_ClassAspect_mod
   use mapl_ExtensionTransform_mod
   use mapl_GeometrySpec_mod
   use mapl_MultiState_mod
   use mapl_NullTransform_mod
   use mapl_StateItemAspect_mod
   use mapl_StateItemSpec_mod
   use mapl_VerticalGrid_mod
   use mapl_VirtualConnectionPtVector_mod
   use mapl_enums_api
   use mapl_field_api
   use mapl_KeywordEnforcer_mod
   use mapl_ErrorHandling_mod
   use esmf
   implicit none(type, external)
   private

   public :: GeometryClassAspect
   public :: GeometryCarrierLayout
   public :: make_geometry_carrier_spec
   public :: make_geometry_carrier_layout
   public :: to_GeometryClassAspect

   type, extends(ClassAspect) :: GeometryClassAspect
      private
      type(ESMF_Field) :: payload
      logical :: owns_payload = .true.
      type(ESMF_Geom), allocatable :: geom
      class(VerticalGrid), allocatable :: vertical_grid
   contains
      procedure :: get_aspect_order
      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific
      procedure :: matches
      procedure :: make_transform
      procedure :: connect_to_export
      procedure :: create
      procedure :: activate
      procedure :: allocate => allocate_carrier
      procedure :: destroy
      procedure :: add_to_state
      procedure :: get_payload
      procedure :: get_geom
      procedure :: get_vertical_grid
   end type GeometryClassAspect

   type :: GeometryCarrierLayout
      logical :: has_import = .false.
      logical :: has_export = .false.
   end type GeometryCarrierLayout

   interface to_GeometryClassAspect
      procedure :: to_geometry_class_aspect
   end interface to_GeometryClassAspect

   interface GeometryClassAspect
      procedure :: new_geometry_class_aspect
   end interface GeometryClassAspect

contains

   function make_geometry_carrier_spec(state_intent, geom, vertical_grid) result(carrier)
      type(StateItemSpec) :: carrier
      type(ESMF_StateIntent_Flag), intent(in) :: state_intent
      type(ESMF_Geom), optional, intent(in) :: geom
      class(VerticalGrid), optional, intent(in) :: vertical_grid
      type(AspectMap) :: aspects
      type(VirtualConnectionPtVector) :: dependencies

      call aspects%insert(CLASS_ASPECT_ID, GeometryClassAspect(geom, vertical_grid))
      carrier = new_StateItemSpec(state_intent, aspects, dependencies)
   end function make_geometry_carrier_spec

   function new_geometry_class_aspect(geom, vertical_grid) result(aspect)
      type(GeometryClassAspect) :: aspect
      type(ESMF_Geom), optional, intent(in) :: geom
      class(VerticalGrid), optional, intent(in) :: vertical_grid

      if (present(geom)) aspect%geom = geom
      if (present(vertical_grid)) aspect%vertical_grid = vertical_grid
   end function new_geometry_class_aspect

   function make_geometry_carrier_layout(geometry_spec, has_children) result(layout)
      type(GeometryCarrierLayout) :: layout
      type(GeometrySpec), intent(in) :: geometry_spec
      logical, optional, intent(in) :: has_children
      logical :: is_parent

      is_parent = .false.
      if (present(has_children)) is_parent = has_children
      select case (geometry_spec%kind)
      case (GEOMETRY_PROVIDER)
         layout%has_export = .true.
      case (GEOMETRY_FROM_PARENT)
         layout%has_import = .true.
         layout%has_export = is_parent
      case (GEOMETRY_FROM_CHILD)
         layout%has_export = .true.
      case (GEOMETRY_NONE)
         continue
      end select
   end function make_geometry_carrier_layout

   function get_aspect_order(this, goal_aspects, rc) result(aspect_ids)
      type(AspectId), allocatable :: aspect_ids(:)
      class(GeometryClassAspect), intent(in) :: this
      type(AspectMap), intent(in) :: goal_aspects
      integer, optional, intent(out) :: rc

      aspect_ids = [CLASS_ASPECT_ID]
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(goal_aspects)
   end function get_aspect_order

   logical function supports_conversion_general(src)
      class(GeometryClassAspect), intent(in) :: src
      supports_conversion_general = .false.
      _UNUSED_DUMMY(src)
   end function supports_conversion_general

   logical function supports_conversion_specific(src, dst)
      class(GeometryClassAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst
      supports_conversion_specific = .false.
      _UNUSED_DUMMY(src)
      _UNUSED_DUMMY(dst)
   end function supports_conversion_specific

   logical function matches(src, dst)
      class(GeometryClassAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst
      select type (dst)
      type is (GeometryClassAspect)
         matches = .true.
      class default
         matches = .false.
      end select
      _UNUSED_DUMMY(src)
   end function matches

   function make_transform(src, dst, other_aspects, rc) result(transform)
      class(ExtensionTransform), allocatable :: transform
      class(GeometryClassAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst
      type(AspectMap), target, intent(in) :: other_aspects
      integer, optional, intent(out) :: rc

      transform = NullTransform()
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(src)
      _UNUSED_DUMMY(dst)
      _UNUSED_DUMMY(other_aspects)
   end function make_transform

   subroutine create(this, other_aspects, rc)
      class(GeometryClassAspect), intent(inout) :: this
      type(AspectMap), intent(in) :: other_aspects
      integer, optional, intent(out) :: rc
      integer :: status

      this%payload = ESMF_FieldEmptyCreate(_RC)
      this%owns_payload = .true.
      if (allocated(this%geom)) call mapl_FieldSet(this%payload, geom=this%geom, _RC)
      if (allocated(this%vertical_grid)) call mapl_FieldSet(this%payload, vgrid=this%vertical_grid, _RC)
      call mapl_FieldSet(this%payload, allocation_status=MAPL_STATEITEM_ALLOCATION_CREATED, _RC)
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(other_aspects)
   end subroutine create

   subroutine activate(this, rc)
      class(GeometryClassAspect), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: status
      call mapl_FieldSet(this%payload, allocation_status=MAPL_STATEITEM_ALLOCATION_ACTIVE, _RC)
      _RETURN(_SUCCESS)
   end subroutine activate

   subroutine allocate_carrier(this, other_aspects, rc)
      class(GeometryClassAspect), intent(inout) :: this
      type(AspectMap), intent(in) :: other_aspects
      integer, optional, intent(out) :: rc
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(other_aspects)
   end subroutine allocate_carrier

   subroutine destroy(this, rc)
      class(GeometryClassAspect), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: status
      if (this%owns_payload) then
         call ESMF_FieldDestroy(this%payload, noGarbage=.true., _RC)
         this%owns_payload = .false.
      end if
      _RETURN(_SUCCESS)
   end subroutine destroy

   subroutine connect_to_export(this, export, actual_pt, rc)
      class(GeometryClassAspect), intent(inout) :: this
      class(StateItemAspect), intent(in) :: export
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc
      type(GeometryClassAspect) :: export_aspect
      integer :: status

      export_aspect = to_GeometryClassAspect(export, _RC)
      call this%destroy(_RC)
      this%payload = export_aspect%payload
      this%owns_payload = .false.
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(actual_pt)
   end subroutine connect_to_export

   subroutine add_to_state(this, multi_state, actual_pt, rc)
      class(GeometryClassAspect), intent(in) :: this
      type(MultiState), intent(inout) :: multi_state
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(multi_state)
      _UNUSED_DUMMY(actual_pt)
   end subroutine add_to_state

   subroutine get_payload(this, unusable, field, bundle, state, rc)
      class(GeometryClassAspect), intent(in) :: this
      class(KeywordEnforcer), optional, intent(out) :: unusable
      type(ESMF_Field), optional, allocatable, intent(out) :: field
      type(ESMF_FieldBundle), optional, allocatable, intent(out) :: bundle
      type(ESMF_State), optional, allocatable, intent(out) :: state
      integer, optional, intent(out) :: rc
      field = this%payload
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(bundle)
      _UNUSED_DUMMY(state)
   end subroutine get_payload

   function get_geom(this, rc) result(geom)
      type(ESMF_Geom), allocatable :: geom
      class(GeometryClassAspect), intent(in) :: this
      integer, optional, intent(out) :: rc
      integer :: status
      call mapl_FieldGet(this%payload, geom=geom, _RC)
      _RETURN(_SUCCESS)
   end function get_geom

   function get_vertical_grid(this, rc) result(vertical_grid)
      class(VerticalGrid), pointer :: vertical_grid
      class(GeometryClassAspect), intent(in) :: this
      integer, optional, intent(out) :: rc
      integer :: status
      call mapl_FieldGet(this%payload, vgrid=vertical_grid, _RC)
      _RETURN(_SUCCESS)
   end function get_vertical_grid

   function to_geometry_class_aspect(aspect, rc) result(geometry_aspect)
      type(GeometryClassAspect) :: geometry_aspect
      class(StateItemAspect), intent(in) :: aspect
      integer, optional, intent(out) :: rc
      select type (aspect)
      type is (GeometryClassAspect)
         geometry_aspect = aspect
      class default
         _FAIL('aspect is not GeometryClassAspect')
      end select
      _RETURN(_SUCCESS)
   end function to_geometry_class_aspect

end module mapl_GeometryClassAspect_mod
