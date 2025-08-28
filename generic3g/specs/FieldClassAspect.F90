#include "MAPL.h"

module mapl3g_FieldClassAspect

   use mapl3g_ActualConnectionPt
   use mapl3g_AspectId
   use mapl3g_StateItemAspect
   use mapl3g_ClassAspect
   use mapl3g_GeomAspect
   use mapl3g_HorizontalDimsSpec
   use mapl3g_VerticalGridAspect
   use mapl3g_UnitsAspect
   use mapl3g_TypekindAspect
   use mapl3g_UngriddedDimsAspect

   use mapl3g_VerticalGrid
   use mapl3g_VerticalStaggerLoc
   use mapl3g_VerticalStaggerLoc
   use mapl3g_UngriddedDims

   use mapl3g_NullTransform
   use mapl3g_ExtensionTransform
   use mapl3g_MultiState
   use mapl3g_ESMF_Utilities, only: get_substate

   use mapl3g_Field_API
   use mapl3g_FieldInfo, only: FieldInfoSetInternal, FieldInfoSetPrivate
   use mapl3g_RestartModes, only: MAPL_RESTART_MODE

   use mapl_FieldUtilities
   use mapl_ErrorHandling
   use esmf
   use pflogger

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
      logical :: is_created = .false.
      type(ESMF_Field) :: payload
      character(:), allocatable :: standard_name
      character(:), allocatable :: long_name
      character(:), allocatable :: short_name
      character(:), allocatable :: gridcomp_name
      real(kind=ESMF_KIND_R4), allocatable :: default_value
      integer(kind=kind(MAPL_RESTART_MODE)), allocatable :: restart_mode
   contains
      procedure :: get_aspect_order
      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific
      procedure :: make_transform
      procedure :: matches => matches_a
      procedure :: connect_to_import
      procedure :: connect_to_export

      procedure :: create
      procedure :: activate
      procedure :: allocate
      procedure :: destroy
      procedure :: add_to_state
      procedure :: add_to_bundle

      procedure :: get_payload
      procedure, nopass :: get_aspect_id
   end type FieldClassAspect

   interface
      module function matches_a(src, dst) result(matches)
        logical matches
         class(FieldClassAspect), intent(in) :: src
         class(StateItemAspect), intent(in) :: dst
      end function matches_a
   end interface

   interface FieldClassAspect
      procedure :: new_FieldClassAspect
   end interface FieldClassAspect


contains

   function new_FieldClassAspect( &
        standard_name, &
        long_name, &
        short_name, &
        gridcomp_name, &
        default_value, &
        restart_mode) result(aspect)
      type(FieldClassAspect) :: aspect
      character(*), optional, intent(in) :: standard_name
      character(*), optional, intent(in) :: long_name
      character(*), optional, intent(in) :: short_name
      character(*), optional, intent(in) :: gridcomp_name
      real(kind=ESMF_KIND_R4), optional, intent(in) :: default_value
      integer(kind=kind(MAPL_RESTART_MODE)), optional, intent(in) :: restart_mode

      aspect%standard_name = 'unknown'
      if (present(standard_name)) then
         aspect%standard_name = standard_name
      end if

      aspect%long_name = 'unknown'
      if (present(long_name)) then
         aspect%long_name = long_name
      end if

      if (present(short_name)) then
         aspect%short_name = short_name
      end if

      if (present(gridcomp_name)) then
         aspect%gridcomp_name = gridcomp_name
      end if

      if (present(default_value)) then
         aspect%default_value = default_value
      end if

      if (present(restart_mode)) then
         aspect%restart_mode = restart_mode
      end if
   end function new_FieldClassAspect

   function get_aspect_order(this, goal_aspects, rc) result(aspect_ids)
      type(AspectId), allocatable :: aspect_ids(:)
      class(FieldClassAspect), intent(in) :: this
      type(AspectMap), intent(in) :: goal_aspects
      integer, optional, intent(out) :: rc

      aspect_ids = [ &
           CLASS_ASPECT_ID, &
           ATTRIBUTES_ASPECT_ID, &
           UNGRIDDED_DIMS_ASPECT_ID, &
           GEOM_ASPECT_ID, &
           VERTICAL_GRID_ASPECT_ID, &
           UNITS_ASPECT_ID, &
           TYPEKIND_ASPECT_ID &
           ]

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(goal_aspects)
   end function get_aspect_order


   subroutine create(this, handle, rc)
      class(FieldClassAspect), intent(inout) :: this
      integer, optional, intent(in) :: handle(:)
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info

      this%payload = ESMF_FieldEmptyCreate(_RC)
      _RETURN_UNLESS(present(handle))
      
      call ESMF_InfoGetFromHost(this%payload, info, _RC)
      call FieldInfoSetInternal(info, spec_handle=handle, _RC)
      call FieldInfoSetInternal(info, allocation_status=STATEITEM_ALLOCATION_CREATED, _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine create

   subroutine activate(this, rc)
      class(FieldClassAspect), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info

      call ESMF_InfoGetFromHost(this%payload, info, _RC)
      call FieldInfoSetInternal(info, allocation_status=STATEITEM_ALLOCATION_ACTIVE, _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine activate

   ! Tile / Grid   X  or X, Y
   subroutine allocate(this, other_aspects, rc)
      class(FieldClassAspect), intent(inout) :: this
      type(AspectMap), intent(in) :: other_aspects
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_FieldStatus_Flag) :: fstatus

      type(GeomAspect) :: geom_aspect
      type(ESMF_Geom) :: geom
      type(HorizontalDimsSpec) :: horizontal_dims_spec
      integer :: dim_count
      integer, allocatable :: grid_to_field_map(:)

      type(VerticalGridAspect) :: vertical_aspect
      class(VerticalGrid), allocatable :: vertical_grid
      type(VerticalStaggerLoc) :: vertical_stagger
      integer, allocatable :: num_vgrid_levels
      integer, allocatable :: num_field_levels

      type(UngriddedDimsAspect) :: ungridded_dims_aspect
      type(UngriddedDims) :: ungridded_dims

      type(UnitsAspect) :: units_aspect
      character(:), allocatable :: units

      type(TypekindAspect) :: typekind_aspect
      type(ESMF_TypeKind_Flag) :: typekind

      integer :: idim

      call ESMF_FieldGet(this%payload, status=fstatus, _RC)
      _RETURN_IF(fstatus == ESMF_FIELDSTATUS_COMPLETE)

      geom_aspect = to_GeomAspect(other_aspects, _RC)
      geom = geom_aspect%get_geom(_RC)
      call ESMF_FieldEmptySet(this%payload, geom, _RC)

      call ESMF_GeomGet(geom, dimCount=dim_count, _RC)
      allocate(grid_to_field_map(dim_count), source=0)
      horizontal_dims_spec = geom_aspect%get_horizontal_dims_spec(_RC)
      _ASSERT(horizontal_dims_spec /= HORIZONTAL_DIMS_UNKNOWN, "should be one of GEOM/NONE")
      if (horizontal_dims_spec == HORIZONTAL_DIMS_GEOM) then
         grid_to_field_map = [(idim, idim=1,dim_count)]
      end if

      vertical_aspect = to_VerticalGridAspect(other_aspects, _RC)
      vertical_stagger = vertical_aspect%get_vertical_stagger()
      if (vertical_stagger /= VERTICAL_STAGGER_NONE) then
         vertical_grid = vertical_aspect%get_vertical_grid(_RC)
         num_vgrid_levels = vertical_grid%get_num_levels()
         if (vertical_stagger == VERTICAL_STAGGER_EDGE) then
            num_field_levels = num_vgrid_levels + 1
         else if (vertical_stagger == VERTICAL_STAGGER_CENTER) then
            num_field_levels = num_vgrid_levels
         end if
      end if

      ungridded_dims_aspect = to_UngriddedDimsAspect(other_aspects, _RC)
      ungridded_dims = ungridded_dims_aspect%get_ungridded_dims()

      units_aspect = to_UnitsAspect(other_aspects, _RC)
      units = units_aspect%get_units(_RC)

      typekind_aspect = to_TypekindAspect(other_aspects, _RC)
      typekind = typekind_aspect%get_typekind()

      call MAPL_FieldEmptyComplete(this%payload, &
           typekind=typekind, &
           gridToFieldMap=grid_to_field_map, &
           ungridded_dims=ungridded_dims, &
           num_levels=num_field_levels, &
           vert_staggerLoc=vertical_stagger, &
           units=units, &
           standard_name=this%standard_name, &
           long_name=this%long_name, &
           _RC)
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


   subroutine connect_to_import(this, import, rc)
      class(FieldClassAspect), intent(inout) :: this
      class(StateItemAspect), intent(in) :: import
      integer, optional, intent(out) :: rc

      type(FieldClassAspect) :: import_
      integer :: status

      _RETURN_IF(allocated(this%default_value))

      import_ = to_FieldClassAspect(import, _RC)
      if (allocated(import_%default_value)) then ! import wins (for now)
         this%default_value = import_%default_value
      end if

      _RETURN(_SUCCESS)
   end subroutine connect_to_import

   subroutine connect_to_export(this, export, actual_pt, rc)
      class(FieldClassAspect), intent(inout) :: this
      class(StateItemAspect), intent(in) :: export
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      type(FieldClassAspect) :: export_
      integer :: status
      type(ESMF_Info) :: info

      export_ = to_FieldClassAspect(export, _RC)
      call this%destroy(_RC) ! import is replaced by export/extension
      this%payload = export_%payload

      call mirror(this%default_value, export_%default_value)

      call ESMF_InfoGetFromHost(this%payload, info, _RC)
      call FieldInfoSetInternal(info, restart_mode=this%restart_mode, _RC)
      call FieldInfoSetInternal(info, allocation_status=STATEITEM_ALLOCATION_CONNECTED, _RC)
      if (allocated(this%restart_mode)) then
         _ASSERT(allocated(this%gridcomp_name), "gridcomp name is not known")
         _ASSERT(allocated(this%short_name), "field's short name is not known")
         call ESMF_InfoGetFromHost(this%payload, info, _RC)
         call FieldInfoSetPrivate(info, this%gridcomp_name, this%short_name, restart_mode=this%restart_mode, _RC)
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(actual_pt)

   contains

     subroutine mirror(dst, src)
        real, allocatable, intent(inout) :: dst
        real, allocatable, intent(in) :: src

        character(100) :: buffer
        class(Logger), pointer :: lgr

        if (.not. allocated(src)) return

        if (.not. allocated(dst)) then
           dst = src
           return
        end if

        ! TODO: Problematic case: both allocated with different values.
        if (dst /= src) then
           lgr => logging%get_logger('mapl.generic')
           write(buffer,*) actual_pt
           call lgr%info('Mismatched default values for %a src = %g0~; dst = %g0 (src value wins)', trim(buffer), src, dst)
        end if

      end subroutine mirror
      
   end subroutine connect_to_export

   function to_fieldclassaspect_from_poly(aspect, rc) result(field_aspect)
      type(FieldClassAspect) :: field_aspect
      class(StateItemAspect), intent(in) :: aspect
      integer, optional, intent(out) :: rc

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

   function make_transform(src, dst, other_aspects, rc) result(transform)
      class(ExtensionTransform), allocatable :: transform
      class(FieldClassAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst
      type(AspectMap), target, intent(in) :: other_aspects
      integer, optional, intent(out) :: rc
      
      transform = NullTransform()

      _RETURN(_SUCCESS)
   end function make_transform

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

      type(ESMF_Field) :: alias, existing_field
      type(esmf_StateItem_Flag) :: itemType
      logical :: is_alias
      integer :: status
      type(ESMF_State) :: state, substate
      character(:), allocatable :: full_name, inner_name
      integer :: idx
      character(:), allocatable :: intent
      
      intent = actual_pt%get_state_intent()
      call multi_state%get_state(state, intent, _RC)

      full_name = actual_pt%get_full_name()
      idx = index(full_name, '/', back=.true.)
      call get_substate(state, full_name(:idx-1), substate=substate, _RC)
      inner_name = full_name(idx+1:)

      alias = ESMF_NamedAlias(this%payload, name=inner_name, _RC)

      call ESMF_StateGet(substate, itemName=inner_name, itemType=itemType, _RC)
      if (itemType /= ESMF_STATEITEM_NOTFOUND) then
         if (intent /= 'import') then
            call ESMF_StateGet(substate, itemName=inner_name, field=existing_field, _RC)
            is_alias = mapl_FieldsAreAliased(alias, existing_field, _RC)
            _ASSERT(is_alias, 'Different fields added under the same name in state.')
         end if
      end if
      call ESMF_StateAddReplace(substate, [alias], _RC)

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

   function get_payload(this) result(field)
      type(ESMF_Field) :: field
      class(FieldClassAspect), intent(in) :: this
      field = this%payload
   end function get_payload

   
   function get_aspect_id() result(aspect_id)
      type(AspectId) :: aspect_id
      aspect_id = CLASS_ASPECT_ID
   end function get_aspect_id

end module mapl3g_FieldClassAspect
