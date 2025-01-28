#include "MAPL_Generic.h"

#if defined _SET_FIELD
#  undef _SET_FIELD
#endif
#define _SET_FIELD(A, B, F) A%F = B%F

#if defined(_SET_ALLOCATED_FIELD)
#  undef _SET_ALLOCATED_FIELD
#endif
#define _SET_ALLOCATED_FIELD(A, B, F) if(allocated(B%F)) _SET_FIELD(A, B, F)

module mapl3g_FieldSpec
   use mapl3g_StateItemAspect
   use mapl3g_AspectCollection
   use mapl3g_GeomAspect
   use mapl3g_VerticalGridAspect
   use mapl3g_UnitsAspect
   use mapl3g_TypekindAspect
   use mapl3g_UngriddedDimsAspect
   use mapl3g_AttributesAspect
   use mapl3g_FrequencyAspect
   use mapl3g_HorizontalDimsSpec
   use mapl3g_VerticalStaggerLoc
   use mapl3g_StateItemSpec
   use mapl3g_UngriddedDims
   use mapl3g_ActualConnectionPt
   use mapl3g_ESMF_Utilities, only: get_substate
   use mapl3g_MultiState
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer
   use mapl3g_InfoUtilities
   use mapl3g_VerticalGrid
   use mapl3g_VerticalDimSpec
   use mapl3g_EsmfRegridder, only: EsmfRegridderParam
   use MAPL_FieldUtils
   use mapl3g_LU_Bound
   use mapl3g_FieldDictionary
   use mapl3g_VariableSpec, only: VariableSpec
   use mapl3g_VerticalRegridMethod
   use gftl2_StringVector
   use esmf
   use nuopc
   use mapl3g_Field_API

   implicit none
   private

   public :: FieldSpec
   public :: new_FieldSpec_geom

   ! Two FieldSpec's can be connected if:
   !   1) They only differ in the following components:
   !      - geom (couple with Regridder)
   !      - vertical_regrid (couple with VerticalRegridder)
   !      - typekind (Copy)
   !      - units (Convert)
   !      - frequency_spec (tbd)
   !      - halo width (tbd)
   !   2) They have the same values for
   !      - ungridded_dims
   !      - standard_name
   !      - long_name
   !      - regrid_param
   !      - default_value
   !   3) The attributes of destination spec are a subset of the
   !      attributes of the source spec.

   type, extends(StateItemSpec) :: FieldSpec

      type(StringVector) :: attributes
!#      type(EsmfRegridderParam) :: regrid_param

      ! Metadata
      character(:), allocatable :: standard_name
      character(:), allocatable :: long_name
      ! TBD
!#      type(FrequencySpec) :: freq_spec
!#      class(AbstractFrequencySpec), allocatable :: freq_spec
!#      integer :: halo_width = 0

      type(ESMF_Field) :: payload
      real, allocatable :: default_value
!#      type(VariableSpec) :: variable_spec

      logical :: is_created = .false.

   contains

      procedure :: create
      procedure :: destroy
      procedure :: allocate
      procedure :: get_payload

      procedure :: connect_to
      procedure :: can_connect_to
      procedure :: add_to_state
      procedure :: add_to_bundle

      procedure :: get_aspect_priorities

      procedure :: set_geometry

      procedure :: write_formatted
   end type FieldSpec

   interface FieldSpec
      module procedure new_FieldSpec_geom
      module procedure new_FieldSpec_varspec
   end interface FieldSpec


contains

   function new_FieldSpec_geom(unusable, geom, vertical_grid, vertical_dim_spec, typekind, ungridded_dims, &
        standard_name, long_name, units, &
        attributes, regrid_param, horizontal_dims_spec, default_value, accumulation_type, timestep) result(field_spec)
      type(FieldSpec), target :: field_spec

      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Geom), optional, intent(in) :: geom
      class(VerticalGrid), optional, intent(in) :: vertical_grid
      type(VerticalDimSpec), intent(in) :: vertical_dim_spec
      type(ESMF_Typekind_Flag), intent(in) :: typekind
      type(UngriddedDims), optional, intent(in) :: ungridded_dims
      character(*), optional, intent(in) :: standard_name
      character(*), optional, intent(in) :: units
      character(*), optional, intent(in) :: long_name
      type(StringVector), optional, intent(in) :: attributes
      type(EsmfRegridderParam), optional, intent(in) :: regrid_param
      type(HorizontalDimsSpec), optional, intent(in) :: horizontal_dims_spec

      ! optional args last
      real, optional, intent(in) :: default_value
      character(*), optional, intent(in) :: accumulation_type
      type(ESMF_TimeInterval), optional, intent(in) :: timestep

      type(AspectCollection), pointer :: aspects

      aspects => field_spec%get_aspects()

      call aspects%set_vertical_grid_aspect(VerticalGridAspect( &
           vertical_grid=vertical_grid, &
           vertical_dim_spec=vertical_dim_spec, &
           geom=geom))
      call aspects%set_geom_aspect(GeomAspect(geom, regrid_param, horizontal_dims_spec))
      call aspects%set_units_aspect(UnitsAspect(units))
      call aspects%set_ungridded_dims_aspect(UngriddedDimsAspect(ungridded_dims))
      call aspects%set_typekind_aspect(TypekindAspect(typekind))
      call aspects%set_frequency_aspect(FrequencyAspect(timestep, accumulation_type))
      call aspects%set_attributes_aspect(AttributesAspect(attributes))
      
      if (present(standard_name)) field_spec%standard_name = standard_name
      if (present(long_name)) field_spec%long_name = long_name
 
      ! regrid_param

      if (present(default_value)) field_spec%default_value = default_value

      _UNUSED_DUMMY(unusable)

   end function new_FieldSpec_geom

   function new_FieldSpec_varspec(variable_spec) result(field_spec)
      type(FieldSpec) :: field_spec
      class(VariableSpec), intent(in) :: variable_spec

      _SET_ALLOCATED_FIELD(field_spec, variable_spec, standard_name)

      ! Cannot do a simple copy as some setters have side-effects
      call field_spec%set_aspect(variable_spec%aspects%get_aspect('GEOM'))
      call field_spec%set_aspect(variable_spec%aspects%get_aspect('VERTICAL_GRID'))
      call field_spec%set_aspect(variable_spec%aspects%get_aspect('UNGRIDDED_DIMS'))
      call field_spec%set_aspect(variable_spec%aspects%get_aspect('ATTRIBUTES'))
      call field_spec%set_aspect(variable_spec%aspects%get_aspect('TYPEKIND'))
      call field_spec%set_aspect(variable_spec%aspects%get_aspect('UNITS'))
      call field_spec%set_aspect(variable_spec%aspects%get_aspect('FREQUENCY'))
      _SET_ALLOCATED_FIELD(field_spec, variable_spec, default_value)

      field_spec%long_name = 'unknown'

   end function new_FieldSpec_varspec

   subroutine set_geometry(this, geom, vertical_grid, rc)
      class(FieldSpec), intent(inout) :: this
      type(ESMF_Geom), optional, intent(in) :: geom
      class(VerticalGrid), optional, intent(in) :: vertical_grid
      integer, optional, intent(out) :: rc

      integer :: status

      call target_set_geom(this, geom, vertical_grid)

      _RETURN(_SUCCESS)

   contains

      ! Helper needed to add target attribute to "this"
      subroutine target_set_geom(this, geom, vertical_grid)
         class(FieldSpec), target, intent(inout) :: this
         type(ESMF_Geom), optional, intent(in) :: geom
         class(VerticalGrid), optional, intent(in) :: vertical_grid

         type(AspectCollection), pointer :: aspects
         type(GeomAspect), pointer :: geom_aspect
         type(VerticalGridAspect), pointer :: vertical_grid_aspect
         
         aspects => this%get_aspects()

         if (present(geom)) then
            geom_aspect => aspects%get_geom_aspect()
            if (associated(geom_aspect)) then
               call geom_aspect%set_geom(geom)
            else
               call aspects%set_aspect(GeomAspect(geom))
            end if
         end if

         if (present(vertical_grid)) then
            vertical_grid_aspect => aspects%get_vertical_grid_aspect()
            if (associated(vertical_grid_aspect)) then
               call vertical_grid_aspect%set_vertical_grid(vertical_grid)
               if (present(geom)) then
                  call vertical_grid_aspect%set_geom(geom)
               end if
            else
               call aspects%set_aspect(VerticalGridAspect(vertical_grid=vertical_grid, geom=geom))
            end if

         end if

      end subroutine target_set_geom

   end subroutine set_geometry

   subroutine create(this, rc)
      class(FieldSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      this%payload = ESMF_FieldEmptyCreate(_RC)
      this%is_created = .true.

      _RETURN(ESMF_SUCCESS)
   end subroutine create

  subroutine destroy(this, rc)
      class(FieldSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call ESMF_FieldDestroy(this%payload, nogarbage=.true., _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine destroy


   ! Tile / Grid   X  or X, Y
   subroutine allocate(this, rc)
      class(FieldSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_FieldStatus_Flag) :: fstatus

      integer, allocatable :: num_levels_grid
      integer, allocatable :: num_levels
      type(VerticalStaggerLoc) :: vertical_staggerloc
      class(VerticalGrid), allocatable :: vertical_grid
      type(VerticalDimSpec) :: vertical_dim_spec
      class(StateItemAspect), pointer :: aspect
      type(UngriddedDims) :: ungridded_dims
      type(ESMF_TypeKind_Flag) :: typekind
      character(:), allocatable :: units

      _RETURN_UNLESS(this%is_active())

      call ESMF_FieldGet(this%payload, status=fstatus, _RC)
      _RETURN_IF(fstatus == ESMF_FIELDSTATUS_COMPLETE)

      aspect => this%get_aspect('GEOM', _RC)
      select type (aspect)
      class is (GeomAspect)
         call ESMF_FieldEmptySet(this%payload, aspect%get_geom(), _RC)
      class default
         _FAIL('no geom aspect')
      end select

      aspect => this%get_aspect('VERTICAL_GRID', _RC)

      select type (aspect)
      class is (VerticalGridAspect)

         vertical_grid = aspect%get_vertical_grid(_RC)
         num_levels_grid = vertical_grid%get_num_levels()
         vertical_dim_spec = aspect%get_vertical_dim_spec(_RC)
         if (vertical_dim_spec == VERTICAL_DIM_NONE) then
            vertical_staggerloc = VERTICAL_STAGGER_NONE
         else if (vertical_dim_spec == VERTICAL_DIM_EDGE) then
            vertical_staggerloc = VERTICAL_STAGGER_EDGE
            num_levels = num_levels_grid + 1
         else if (vertical_dim_spec == VERTICAL_DIM_CENTER) then
            vertical_staggerloc = VERTICAL_STAGGER_CENTER
            num_levels = num_levels_grid
         else
            _FAIL('unknown stagger')
         end if
      class default
         _FAIL('no vertical grid aspect')
      end select

      aspect => this%get_aspect('UNGRIDDED_DIMS', _RC)
      if (associated(aspect)) then
         select type (aspect)
         class is (UngriddedDimsAspect)
            ungridded_dims = aspect%get_ungridded_dims(_RC)
         class default
            _FAIL('no ungridded_dims aspect')
         end select
      end if

      aspect => this%get_aspect('UNITS', _RC)
      select type(aspect)
      class is (UnitsAspect)
         units = aspect%get_units(_RC)
      class default
         _FAIL('no units aspect')
      end select

      aspect => this%get_aspect('TYPEKIND', _RC)
      select type(aspect)
      class is (TypekindAspect)
         typekind = aspect%typekind
      class default
         _FAIL('no units aspect')
      end select

      call MAPL_FieldEmptyComplete(this%payload, &
           typekind=typekind, &
           ungridded_dims=ungridded_dims, &
           num_levels=num_levels, &
           vert_staggerLoc=vertical_staggerLoc, &
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

   subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
      class(FieldSpec), intent(in) :: this
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      write(unit, "(a)", iostat=iostat, iomsg=iomsg) "FieldSpec("
      if (allocated(this%standard_name)) then
         write(unit, "(a, a, a)", iostat=iostat, iomsg=iomsg) new_line("a"), "standard name:", this%standard_name
      end if
      if (allocated(this%long_name)) then
         write(unit, "(a, a, a)", iostat=iostat, iomsg=iomsg) new_line("a"), "long name:", this%long_name
      end if
      write(unit, "(a)") ")"

      _UNUSED_DUMMY(iotype)
      _UNUSED_DUMMY(v_list)
   end subroutine write_formatted


   subroutine connect_to(this, src_spec, actual_pt, rc)

      class(FieldSpec), intent(inout) :: this
      class(StateItemSpec), intent(inout) :: src_spec
      type(ActualConnectionPt), intent(in) :: actual_pt ! unused
      integer, optional, intent(out) :: rc

      integer :: status
      class(StateItemAspect), pointer :: aspect

      interface mirror
         procedure :: mirror_real
      end interface mirror

      _ASSERT(this%can_connect_to(src_spec), 'illegal connection')
      
      select type (src_spec)
      class is (FieldSpec)
         ! Import fields are preemptively created just so that they
         ! can still be queried even when not satisfied.  It is
         ! possible that such is not really necessary.  But for now
         ! when an import is ultimately connected we must destroy the
         ! ESMF_Field object before copying the payload from the
         ! source spec.
         call this%destroy(_RC)
         this%payload = src_spec%payload

         aspect => src_spec%get_aspect('GEOM', _RC)
         call this%set_aspect(aspect, _RC)
         aspect => src_spec%get_aspect('VERTICAL_GRID', _RC)
         call this%set_aspect(aspect, _RC)
         aspect => src_spec%get_aspect('UNGRIDDED_DIMS', _RC)
         call this%set_aspect(aspect, _RC)
         aspect => src_spec%get_aspect('TYPEKIND', _RC)
         call this%set_aspect(aspect, _RC)
         aspect => src_spec%get_aspect('UNITS', _RC)
         call this%set_aspect(aspect, _RC)
         aspect => src_spec%get_aspect('FREQUENCY', _RC)
         call this%set_aspect(aspect, _RC)

         call mirror(dst=this%default_value, src=src_spec%default_value)
      class default
         _FAIL('Cannot connect field spec to non field spec.')
      end select

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(actual_pt)

   contains

      subroutine mirror_real(dst, src)
         real, allocatable, intent(inout) :: dst, src

         if (allocated(dst) .eqv. allocated(src)) return

         if (.not. allocated(dst)) then
            dst = src
         end if

         if (.not. allocated(src)) then
            src = dst
         end if
      end subroutine mirror_real

   end subroutine connect_to

   logical function can_connect_to(this, src_spec, rc)

      class(FieldSpec), intent(in) :: this
      class(StateItemSpec), intent(in) :: src_spec
      integer, optional, intent(out) :: rc

      logical :: can_convert_units
      class(StateItemAspect), pointer :: src_aspect, dst_aspect
      character(:), pointer :: aspecT_name
      type(StringVector), target :: aspect_list
      type(StringVectorIterator) :: aspect_iter


      select type(src_spec)
      class is (FieldSpec)
         aspect_list = src_spec%get_aspect_order(this)
         aspect_iter = aspect_list%ftn_begin()
         associate (e => aspect_list%ftn_end())
           do while (aspect_iter /= e)
              call aspect_iter%next()
              aspect_name => aspect_iter%of()
              src_aspect => src_spec%get_aspect(aspect_name)
              dst_aspect => this%get_aspect(aspect_name)
              can_connect_to = src_aspect%can_connect_to(dst_aspect)
              _RETURN_UNLESS(can_connect_to)
           end do
         end associate

      class default
         can_connect_to = .false.
      end select
      _RETURN(_SUCCESS)

   end function can_connect_to

   subroutine add_to_state(this, multi_state, actual_pt, rc)
      class(FieldSpec), intent(in) :: this
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

   subroutine add_to_bundle(this, bundle, rc)
      class(FieldSpec), intent(in) :: this
      type(ESMF_FieldBundle), intent(inout) :: bundle
      integer, optional, intent(out) :: rc

      integer :: status

      call ESMF_FieldBundleAdd(bundle, [this%payload], multiflag=.true., _RC)

      _RETURN(_SUCCESS)
   end subroutine add_to_bundle

  function get_payload(this) result(payload)
      type(ESMF_Field) :: payload
      class(FieldSpec), intent(in) :: this
      payload = this%payload
   end function get_payload


   function get_aspect_priorities(src_spec, dst_spec) result(order)
      character(:), allocatable :: order
      class(FieldSpec), intent(in) :: src_spec
      class(StateItemSpec), intent(in) :: dst_spec

      order = 'ATTRIBUTES::UNGRIDDED_DIMS::GEOM::VERTICAL_GRID::UNITS::TYPEKIND'

   end function get_aspect_priorities
   
end module mapl3g_FieldSpec

#undef _SET_FIELD
#undef _SET_ALLOCATED_FIELD
