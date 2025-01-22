#include "MAPL_Generic.h"

module mapl3g_FieldClassAspect
   use mapl3g_ActualConnectionPt

   use mapl3g_AspectId
   use mapl3g_StateItemAspect
   use mapl3g_ClassAspect
   use mapl3g_GeomAspect
   use mapl3g_VerticalGridAspect
   use mapl3g_UnitsAspect
   use mapl3g_TypekindAspect
   use mapl3g_UngriddedDimsAspect

   use mapl3g_VerticalGrid
   use mapl3g_VerticalDimSpec
   use mapl3g_VerticalStaggerLoc
   use mapl3g_UngriddedDims

   use mapl3g_NullAction
   use mapl3g_ExtensionAction
   use mapl3g_MultiState
   use mapl3g_ESMF_Utilities, only: get_substate

   use mapl3g_FieldCreate
   use mapl_FieldUtilities

   use mapl_ErrorHandling
   use esmf
   implicit none(type,external)
   private

   public :: FieldClassAspect
   public :: to_FieldClassAspect

   interface to_FieldClassAspect
      procedure :: to_fieldclassaspect_from_poly
      procedure :: to_fieldclassaspect_from_map
   end interface to_FieldClassAspect
   
   type, extends(ClassAspect) :: FieldClassAspect
      private
      type(ESMF_Field) :: payload
      character(:), allocatable :: standard_name
      character(:), allocatable :: long_name
      real(kind=ESMF_KIND_R4), allocatable :: default_value
   contains
      procedure :: make_action
      procedure :: make_action2
      procedure :: matches
      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific
      procedure :: connect_to_export

      procedure :: create
      procedure :: allocate
      procedure :: destroy
      procedure :: add_to_state
      procedure :: add_to_bundle
      
   end type FieldClassAspect

   interface FieldClassAspect
      procedure :: new_FieldClassAspect
   end interface FieldClassAspect

contains

   function new_FieldClassAspect(standard_name, long_name, default_value) result(aspect)
      type(FieldClassAspect) :: aspect
      character(*), intent(in) :: standard_name
      character(*), intent(in) :: long_name
      real(kind=ESMF_KIND_R4), intent(in), optional :: default_value

      aspect%standard_name = standard_name
      aspect%long_name = long_name
      if (present(default_value)) then
         aspect%default_value = default_value
      end if
      
   end function new_FieldClassAspect

   subroutine create(this, rc)
      class(FieldClassAspect), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      this%payload = ESMF_FieldEmptyCreate(_RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine create

   ! Tile / Grid   X  or X, Y
   subroutine allocate(this, other_aspects, rc)
      class(FieldClassAspect), intent(inout) :: this
      type(AspectMap), intent(in) :: other_aspects
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_FieldStatus_Flag) :: fstatus


      type(GeomAspect) :: geom_aspect
      type(ESMF_Geom) :: geom

      type(VerticalGridAspect) :: vert_aspect
      class(VerticalGrid), allocatable :: vert_grid
      type(VerticalDimSpec) :: vertical_dim_spec
      type(VerticalStaggerLoc) :: vert_staggerloc
      integer, allocatable :: num_levels_grid
      integer, allocatable :: num_levels

      type(UngriddedDimsAspect) :: ungridded_dims_aspect
      type(UngriddedDims) :: ungridded_dims

      type(UnitsAspect) :: units_aspect
      character(:), allocatable :: units

      type(TypekindAspect) :: typekind_aspect
      type(ESMF_TypeKind_Flag) :: typekind

      call ESMF_FieldGet(this%payload, status=fstatus, _RC)
      _RETURN_IF(fstatus == ESMF_FIELDSTATUS_COMPLETE)

      geom_aspect = to_GeomAspect(other_aspects, _RC)
      geom = geom_aspect%get_geom(_RC)
      call ESMF_FieldEmptySet(this%payload, geom, _RC)

      vert_aspect = to_VerticalGridAspect(other_aspects, _RC)
      vert_grid = vert_aspect%get_vertical_grid(_RC)
      num_levels_grid = vert_grid%get_num_levels()
      vertical_dim_spec = vert_aspect%get_vertical_dim_spec()
      if (vertical_dim_spec == VERTICAL_DIM_NONE) then
         vert_staggerloc = VERTICAL_STAGGER_NONE
      else if (vertical_dim_spec == VERTICAL_DIM_EDGE) then
         vert_staggerloc = VERTICAL_STAGGER_EDGE
         num_levels = num_levels_grid + 1
      else if (vertical_dim_spec == VERTICAL_DIM_CENTER) then
         vert_staggerloc = VERTICAL_STAGGER_CENTER
         num_levels = num_levels_grid
      else
         _FAIL('unknown stagger')
      end if

      ungridded_dims_aspect = to_UngriddedDimsAspect(other_aspects, _RC)
      ungridded_dims = ungridded_dims_aspect%get_ungridded_dims()

      units_aspect = to_UnitsAspect(other_aspects, _RC)
      units = units_aspect%get_units(_RC)

      typekind_aspect = to_TypekindAspect(other_aspects, _RC)
      typekind = typekind_aspect%get_typekind()

      call MAPL_FieldEmptyComplete(this%payload, &
           typekind=typekind, &
           ungridded_dims=ungridded_dims, &
           num_levels=num_levels, &
           vert_staggerLoc=vert_staggerLoc, &
           units=units, &
           standard_name=this%standard_name, &
           long_name=this%long_name, &
           _RC)
      _VERIFY(status)
    
      call ESMF_FieldGet(this%payload, status=fstatus, _RC)
      _ASSERT(fstatus == ESMF_FIELDSTATUS_COMPLETE, 'ESMF field status problem.')

      if (allocated(this%default_value)) then
         call FieldSet(this%payload, this%default_value, _RC)
      end if

      _RETURN(ESMF_SUCCESS)
   end subroutine allocate


  subroutine destroy(this, rc)
      class(FieldClassAspect), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call ESMF_FieldDestroy(this%payload, nogarbage=.true., _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine destroy


   subroutine connect_to_export(this, export, actual_pt, rc)
      class(FieldClassAspect), intent(inout) :: this
      class(StateItemAspect), intent(in) :: export
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      type(FieldClassAspect) :: export_
      integer :: status

      export_ = to_FieldClassAspect(export, _RC)
      this%payload = export_%payload

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(actual_pt)
   end subroutine connect_to_export
   

   function to_fieldclassaspect_from_poly(aspect, rc) result(field_aspect)
      type(FieldClassAspect) :: field_aspect
      class(StateItemAspect), intent(in) :: aspect
      integer, optional, intent(out) :: rc

      integer :: status

      select type(aspect)
      class is (FieldClassAspect)
         field_aspect = aspect
      class default
         _FAIL('aspect is not FieldClassAspect')
      end select

      _RETURN(_SUCCESS)
   end function to_fieldclassaspect_from_poly

   function to_fieldclassaspect_from_map(map, rc) result(field_aspect)
      type(FieldClassAspect) :: field_aspect
      type(AspectMap), target, intent(in) :: map
      integer, optional, intent(out) :: rc

      integer :: status
      class(StateItemAspect), pointer :: poly

      poly => map%at(CLASS_ASPECT_ID, _RC)
      field_aspect = to_FieldClassAspect(poly, _RC)

      _RETURN(_SUCCESS)
   end function to_fieldclassaspect_from_map
   

   function make_action(src, dst, rc) result(action)
      class(ExtensionAction), allocatable :: action
      class(FieldClassAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst
      integer, optional, intent(out) :: rc
      
      action = NullAction()
      _RETURN(_SUCCESS)
   end function make_action

   function make_action2(src, dst, other_aspects, rc) result(action)
      class(ExtensionAction), allocatable :: action
      class(FieldClassAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst
      type(AspectMap), target, intent(in) :: other_aspects
      integer, optional, intent(out) :: rc
      
      action = NullAction()

      _RETURN(_SUCCESS)
   end function make_action2

   logical function matches(src, dst)
      class(FieldClassAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      matches = .false.
      select type(dst)
      class is (FieldClassAspect)
         matches = .true.
      end select

   end function matches

   logical function supports_conversion_general(src)
      class(FieldClassAspect), intent(in) :: src
      supports_conversion_general = .false.
   end function supports_conversion_general

   logical function supports_conversion_specific(src, dst)
      class(FieldClassAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      supports_conversion_specific = .false.

      _UNUSED_DUMMY(dst)
   end function supports_conversion_specific

   subroutine add_to_state(this, multi_state, actual_pt, rc)
      class(FieldClassAspect), intent(in) :: this
      type(MultiState), intent(inout) :: multi_state
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      type(ESMF_Field) :: alias
      integer :: status
      type(ESMF_State) :: state, substate
      character(:), allocatable :: full_name, inner_name
      integer :: idx

      call multi_state%get_state(state, actual_pt%get_state_intent(), _RC)

      full_name = actual_pt%get_full_name()
      idx = index(full_name, '/', back=.true.)
      call get_substate(state, full_name(:idx-1), substate=substate, _RC)
      inner_name = full_name(idx+1:)

      alias = ESMF_NamedAlias(this%payload, name=inner_name, _RC)
      call ESMF_StateAdd(substate, [alias], _RC)

      _RETURN(_SUCCESS)
   end subroutine add_to_state

   subroutine add_to_bundle(this, field_bundle, rc)
      class(FieldClassAspect), intent(in) :: this
      type(ESMF_FieldBundle), intent(inout) :: field_bundle
      integer, optional, intent(out) :: rc

      integer :: status

      call ESMF_FieldBundleAdd(field_bundle, [this%payload], multiflag=.true., _RC)

      _RETURN(_SUCCESS)
   end subroutine add_to_bundle

end module mapl3g_FieldClassAspect
