#include "MAPL_Generic.h"
#if defined(IS_IN_SET)
#  undef IS_IN_SET
#endif
#define IS_IN_SET_(V, S) findloc(S, V) >= lbound(S)

module mapl3g_VariableSpec
   use mapl3g_StateItemSpec
   use mapl3g_StateItemAspect
   use mapl3g_GeomAspect

   use mapl3g_ClassAspect
   use mapl3g_FieldClassAspect
   use mapl3g_FieldBundleClassAspect
   use mapl3g_StateClassAspect
   use mapl3g_VectorClassAspect
   use mapl3g_BracketClassAspect
   use mapl3g_WildcardClassAspect
   use mapl3g_ServiceClassAspect
   use mapl3g_ExpressionClassAspect

   use mapl3g_UnitsAspect
   use mapl3g_AttributesAspect
   use mapl3g_UngriddedDimsAspect
   use mapl3g_VerticalGridAspect
   use mapl3g_VerticalRegridMethod
   use mapl3g_FrequencyAspect
   use mapl3g_TypekindAspect
   use mapl3g_UngriddedDims
   use mapl3g_VerticalStaggerLoc
   use mapl3g_HorizontalDimsSpec
   use mapl3g_VirtualConnectionPt
   use mapl3g_ActualConnectionPt
   use mapl3g_VerticalGrid
   use mapl3g_VirtualConnectionPtVector
   use mapl_ErrorHandling
   use mapl3g_StateRegistry
   use mapl3g_StateItem
   use mapl3g_AspectId
   use mapl3g_EsmfRegridder, only: EsmfRegridderParam
   use mapl3g_FieldDictionary
   use mapl_KeywordEnforcerMod
   use esmf
   use gFTL2_StringVector
   use nuopc

   implicit none
   private

   public :: VariableSpec
   public :: make_VariableSpec

   ! This type provides components that might be needed for _any_
   ! state item.  This is largely to support legacy interfaces, but it
   ! also allows us to defer interpretation until after user
   ! setservices() have run.
   type VariableSpec
      ! TODO: delete - move to StateItemSpec

      ! Mandatory values:
      type(ESMF_StateIntent_Flag) :: state_intent
      character(:), allocatable :: short_name
      type(ESMF_StateItem_Flag) :: itemType = MAPL_STATEITEM_FIELD

      !=====================
      ! class aspect
      !=====================
      !---------------------
      ! Field & Vector
      !---------------------
      character(:), allocatable :: standard_name
      character(:), allocatable :: long_name ! from FieldDictionary or override
      !---------------------
      ! Vector
      !---------------------
      type(StringVector) :: vector_component_names ! default empty
      real(kind=ESMF_KIND_R4), allocatable :: default_value
      ! Todo: implement these
      ! type(VectorOrientation_Flag), allocatable :: vectororientation
      ! type(ArakawaStagger_Flag), allocatable :: arakawa_stagger
      !---------------------
      ! Bracket
      !---------------------
      integer, allocatable :: bracket_size
      !---------------------
      ! Service
      !---------------------
      type(StringVector) :: service_items ! default empty
      !---------------------
      ! Expression
      !---------------------
      character(:), allocatable :: expression ! default empt

      
      !=====================
      ! typekind aspect
      !=====================
      type(ESMF_TypeKind_Flag) :: typekind = ESMF_TYPEKIND_R4 ! default

      !=====================
      ! geomaspect
      !=====================
      type(ESMF_Geom), allocatable :: geom
      type(HorizontalDimsSpec) :: horizontal_dims_spec = HORIZONTAL_DIMS_GEOM
      ! next two items are mutually exclusive
      type(EsmfRegridderParam), allocatable :: regrid_param
      type(ESMF_RegridMethod_Flag), allocatable :: regrid_method

      !=====================
      ! vertical aspect
      !=====================
      class(VerticalGrid), allocatable :: vertical_grid
      type(VerticalStaggerLoc), allocatable :: vertical_stagger

      !=====================
      ! units aspect
      !=====================
      character(:), allocatable :: units ! from FieldDictionary or override

      !=====================
      ! frequency aspect
      !=====================
      ! TODO: Should be an enum
      character(:), allocatable :: accumulation_type
      type(ESMF_TimeInterval), allocatable :: timeStep
      type(ESMF_TimeInterval), allocatable :: offset

      !=====================
      ! ungridded_dims aspect
      !=====================
      type(UngriddedDims) :: ungridded_dims ! default no ungridded
      !=====================
      ! attributes aspect
      !=====================
      type(StringVector) :: attributes ! default empty

      !=====================
      ! miscellaneous
      !=====================
      type(StringVector) :: dependencies ! default emuty

   contains
      procedure :: make_virtualPt
      procedure :: make_dependencies

      procedure :: make_StateItemSpec
      procedure :: make_aspects
      procedure :: make_UnitsAspect
      procedure :: make_TypekindAspect
      procedure :: make_GeomAspect
      procedure :: make_UngriddedDimsAspect
      procedure :: make_AttributesAspect
      procedure :: make_VerticalGridAspect
      procedure :: make_FrequencyAspect
      procedure :: make_ClassAspect
   end type VariableSpec

   interface is_in
      module procedure :: is_in_integer
      module procedure :: is_in_realR4
   end interface is_in

   interface
      logical function StringPredicate(string)
         character(len=*), intent(in) :: string
      end function StringPredicate
   end interface

contains

   function make_VariableSpec( &
        state_intent, short_name, unusable, &
        standard_name, &
        geom, &
        units, &
        itemtype, &
        typekind, &
        vertical_grid, &
        vertical_stagger, &
        ungridded_dims, &
        default_value, &
        service_items, &
        attributes, &
        bracket_size, &
        expression, &
        dependencies, &
        regrid_param, &
        horizontal_dims_spec, &
        accumulation_type, &
        timeStep, &
        offset, &
        vector_component_names, &
        rc) result(var_spec)

      type(VariableSpec) :: var_spec
      character(*), intent(in) :: short_name
      type(ESMF_StateIntent_Flag), intent(in) :: state_intent
      ! Optional args:
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: standard_name
      type(ESMF_Geom), optional, intent(in) :: geom
      character(*), optional, intent(in) :: units
      character(*), optional, intent(in) :: expression
      type(ESMF_StateItem_Flag), optional, intent(in) :: itemtype
      type(ESMF_TypeKind_Flag), optional, intent(in) :: typekind
      class(VerticalGrid), optional, intent(in) :: vertical_grid
      type(VerticalStaggerLoc), optional, intent(in) :: vertical_stagger
      type(UngriddedDims), optional, intent(in) :: ungridded_dims
      real, optional, intent(in) :: default_value
      type(StringVector), optional :: service_items
      type(StringVector), optional, intent(in) :: attributes
      integer, optional, intent(in) :: bracket_size
      type(StringVector), optional, intent(in) :: dependencies
      type(EsmfRegridderParam), optional, intent(in) :: regrid_param
      type(HorizontalDimsSpec), optional, intent(in) :: horizontal_dims_spec
      character(len=*), optional, intent(in) :: accumulation_type
      type(ESMF_TimeInterval), optional, intent(in) :: timeStep
      type(ESMF_TimeInterval), optional, intent(in) :: offset
      type(StringVector), optional, intent(in) :: vector_component_names
      integer, optional, intent(out) :: rc

!#      type(ESMF_RegridMethod_Flag), allocatable :: regrid_method
!#      type(EsmfRegridderParam) :: regrid_param_
      integer :: status

      var_spec%short_name = short_name
      var_spec%state_intent = state_intent

#if defined(_SET_OPTIONAL)
#  undef _SET_OPTIONAL
#endif
#define _SET_OPTIONAL(opt) if (present(opt)) var_spec%opt = opt
      _SET_OPTIONAL(standard_name)
      _SET_OPTIONAL(geom)
      _SET_OPTIONAL(units)
      _SET_OPTIONAL(expression)
      _SET_OPTIONAL(itemtype)
      _SET_OPTIONAL(typekind)
      _SET_OPTIONAL(vertical_grid)
      _SET_OPTIONAL(vertical_stagger)
      _SET_OPTIONAL(ungridded_dims)
      _SET_OPTIONAL(default_value)
      _SET_OPTIONAL(service_items)
      _SET_OPTIONAL(attributes)
      _SET_OPTIONAL(bracket_size)
      _SET_OPTIONAL(dependencies)
      _SET_OPTIONAL(regrid_param)
      _SET_OPTIONAL(horizontal_dims_spec)
      _SET_OPTIONAL(accumulation_type)
      _SET_OPTIONAL(timeStep)
      _SET_OPTIONAL(offset)
      _SET_OPTIONAL(vector_component_names)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function make_VariableSpec

   subroutine split_name(encoded_name, name_1, name_2, rc)
      character(*), intent(in) :: encoded_name
      character(:), allocatable, intent(out) :: name_1
      character(:), allocatable, intent(out) :: name_2
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: idx_open, idx_close, idx_comma

      idx_open = index(encoded_name, '(')
      idx_close = index(encoded_name, ')')
      idx_comma = index(encoded_name, ',')

      _ASSERT(idx_open > 0, 'VectorAspect requires standard name to have tuple for the names of the vector  components.')
      _ASSERT(idx_close > 0, 'VectorAspect requires standard name to have tuple for the names of the vector components.')
      _ASSERT(idx_comma > idx_open+1, 'VectorAspect requires standard name to have tuple for the names of the vector components.')
      _ASSERT(idx_comma < idx_close-1, 'VectorAspect requires standard name to have tuple for the names of the vector components.')

      name_1 = encoded_name(idx_open+1:idx_comma-1) // encoded_name(idx_close+1:)
      name_2 = encoded_name(idx_comma+1:idx_close-1) // encoded_name(idx_close+1:)

      _RETURN(_SUCCESS)
   end subroutine split_name
   

   function make_virtualPt(this) result(v_pt)
      type(VirtualConnectionPt) :: v_pt
      class(VariableSpec), intent(in) :: this
      v_pt = VirtualConnectionPt(this%state_intent, this%short_name)
   end function make_virtualPt

   function make_dependencies(this, rc) result(dependencies)
      type(VirtualConnectionPtVector) :: dependencies
      class(VariableSpec), intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: i
      type(VirtualConnectionPt) :: v_pt

      dependencies = VirtualConnectionPtVector()
      do i = 1, this%dependencies%size()
         v_pt = VirtualConnectionPt(ESMF_STATEINTENT_EXPORT, this%dependencies%of(i))
         call dependencies%push_back(v_pt)
      end do

      _RETURN(_SUCCESS)
   end function make_dependencies

   function get_regrid_param(requested_param, standard_name) result(regrid_param)
      type(EsmfRegridderParam) :: regrid_param
      type(EsmfRegridderParam), optional, intent(in) :: requested_param
      character(*), optional, intent(in) :: standard_name
      
      type(ESMF_RegridMethod_Flag) :: regrid_method
      integer :: status

      if (present(requested_param)) then
         regrid_param = requested_param
         return
      end if

      ! if (NUOPC_FieldDictionaryHasEntry(this%standard_name, rc=status)) then
      !    call NUOPC_FieldDictionaryGetEntry(this%standard_name, regrid_method, rc=status)
      !    if (status==ESMF_SUCCESS) then
      !       this%regrid_param = EsmfRegridderParam(regridmethod=regrid_method)
      !       return
      !    end if
      ! end if
      regrid_param = EsmfRegridderParam() ! last resort - use default regrid method

      regrid_method = get_regrid_method_from_field_dict_(standard_name, rc=status)
      if (status==ESMF_SUCCESS) then
         regrid_param = EsmfRegridderParam(regridmethod=regrid_method)
         return
      end if

   end function get_regrid_param

   function get_regrid_method_from_field_dict_(standard_name, rc) result(regrid_method)
      type(ESMF_RegridMethod_Flag) :: regrid_method
      character(*), optional, intent(in) :: standard_name
      integer, optional, intent(out) :: rc

      character(len=*), parameter :: field_dictionary_file = "field_dictionary.yml"
      type(FieldDictionary) :: field_dict
      logical :: file_exists
      integer :: status

      inquire(file=trim(field_dictionary_file), exist=file_exists)
      if (.not. file_exists) then
         rc = _FAILURE
         return
      end if

      field_dict = FieldDictionary(filename=field_dictionary_file, _RC)
      if (.not. present(standard_name)) then
         rc = _FAILURE
         return
      end if
      regrid_method = field_dict%get_regrid_method(standard_name, _RC)

      _RETURN(_SUCCESS)
   end function get_regrid_method_from_field_dict_


   subroutine add_item(aspects, aspect, rc)
      class(AspectMap), intent(inout) :: aspects
      class(StateItemAspect), intent(in) :: aspect
      integer, optional, intent(out) :: rc

      select type(aspect)
      class is (ClassAspect)
         call aspects%insert(CLASS_ASPECT_ID, aspect)
      type is (GeomAspect)
         call aspects%insert(GEOM_ASPECT_ID, aspect)
      type is (UnitsAspect)
         call aspects%insert(UNITS_ASPECT_ID, aspect)
      type is (AttributesAspect)
         call aspects%insert(ATTRIBUTES_ASPECT_ID, aspect)
      type is (UngriddedDimsAspect)
         call aspects%insert(UNGRIDDED_DIMS_ASPECT_ID, aspect)
      type is (VerticalGridAspect)
         call aspects%insert(VERTICAL_GRID_ASPECT_ID, aspect)
      type is (FrequencyAspect)
         call aspects%insert(FREQUENCY_ASPECT_ID, aspect)
      type is (TypekindAspect)
         call aspects%insert(TYPEKIND_ASPECT_ID, aspect)
      class default
         _FAIL('Unsupported type')
      end select
      _RETURN(_SUCCESS)

   end subroutine add_item


   function make_StateitemSpec(this, registry, component_geom, vertical_grid, unusable, timestep, offset, rc) result(spec)
      type(StateItemSpec) :: spec
      class(VariableSpec), intent(in) :: this
      type(StateRegistry), pointer, intent(in) :: registry
      type(ESMF_Geom), optional, intent(in) :: component_geom
      class(VerticalGrid), optional, intent(in) :: vertical_grid
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_TimeInterval), optional, intent(in) :: timestep
      type(ESMF_TimeInterval), optional, intent(in) :: offset
      integer, optional, intent(out) :: rc

      type(AspectMap) :: aspects
      type(VirtualConnectionPtVector) :: dependencies
      integer :: status
      
      aspects = this%make_aspects(registry, component_geom, vertical_grid, timestep=timestep, offset=offset, _RC)
      dependencies = this%make_dependencies(_RC)
      spec = new_StateItemSpec(aspects, dependencies=dependencies)
      

      _RETURN(_SUCCESS)
   end function make_StateitemSpec


   function make_aspects(this, registry, component_geom, vertical_grid, unusable, timestep, offset, rc) result(aspects)
      type(AspectMap) :: aspects
      class(VariableSpec), intent(in) :: this
      type(StateRegistry), pointer, intent(in) :: registry
      type(ESMF_Geom), optional, intent(in) :: component_geom
      class(VerticalGrid), optional, intent(in) :: vertical_grid
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_TimeInterval), optional, intent(in) :: timestep
      type(ESMF_TimeInterval), optional, intent(in) :: offset
      integer, optional, intent(out) :: rc

      integer :: status
      class(StateItemAspect), allocatable :: aspect

      aspect = this%make_UnitsAspect(RC)
      call aspects%insert(UNITS_ASPECT_ID, aspect)
      
      aspect = this%make_TypekindAspect(_RC)
      call aspects%insert(TYPEKIND_ASPECT_ID, aspect)

      aspect = this%make_GeomAspect(component_geom, _RC)
      call aspects%insert(GEOM_ASPECT_ID, aspect)

      aspect = this%make_UngriddedDimsAspect(_RC)
      call aspects%insert(UNGRIDDED_DIMS_ASPECT_ID, aspect)
      
      aspect = this%make_AttributesAspect(_RC)
      call aspects%insert(ATTRIBUTES_ASPECT_ID, aspect)
      
      aspect = this%make_VerticalGridAspect(vertical_grid, &
           component_geom=component_geom, _RC)
      call aspects%insert(VERTICAL_GRID_ASPECT_ID, aspect)

      aspect = this%make_FrequencyAspect(timestep, offset, _RC)
      call aspects%insert(FREQUENCY_ASPECT_ID, aspect)

      aspect = this%make_ClassAspect(registry, _RC)
      call aspects%insert(CLASS_ASPECT_ID, aspect)
      
      _RETURN(_SUCCESS)
   end function make_aspects

   function make_UnitsAspect(this, rc) result(aspect)
      type(UnitsAspect) :: aspect
      class(VariableSpec), intent(in) :: this
      integer, optional, intent(out) :: rc
      aspect = UnitsAspect(this%units)
      _RETURN(_SUCCESS)
   end function make_UnitsAspect

   function make_TypekindAspect(this, rc) result(aspect)
      type(TypekindAspect) :: aspect
      class(VariableSpec), intent(in) :: this
      integer, optional, intent(out) :: rc
      aspect = TypekindAspect(this%typekind)
      _RETURN(_SUCCESS)
   end function make_TypekindAspect

   function make_GeomAspect(this, component_geom, rc) result(aspect)
      type(GeomAspect) :: aspect
      class(VariableSpec), intent(in) :: this
      type(ESMF_Geom), optional, intent(in) :: component_geom
      integer, optional, intent(out) :: rc

      type(ESMF_Geom), allocatable :: geom_

      ! If geom is allocated in var spec then it is prioritized over the
      ! component-wide geom.
      ! If not specified either way, then it indicates that the geom is
      ! mirrored ind will be determined by a connection.
      if (allocated(this%geom)) then
         geom_ = this%geom
      elseif (present(component_geom)) then
         geom_ = component_geom
      end if
      aspect = GeomAspect(geom_, this%regrid_param, this%horizontal_dims_spec)

      _RETURN(_SUCCESS)
   end function make_GeomAspect

   function make_UngriddedDimsAspect(this, rc) result(aspect)
      type(UngriddedDimsAspect) :: aspect
      class(VariableSpec), intent(in) :: this
      integer, optional, intent(out) :: rc
      aspect = UngriddedDimsAspect(this%ungridded_dims)
      _RETURN(_SUCCESS)
   end function make_UngriddedDimsAspect
  
   function make_AttributesAspect(this, rc) result(aspect)
      type(AttributesAspect) :: aspect
      class(VariableSpec), intent(in) :: this
      integer, optional, intent(out) :: rc
      aspect = AttributesAspect(this%attributes)
      _RETURN(_SUCCESS)
   end function make_AttributesAspect
  
   function make_VerticalGridAspect(this, vertical_grid, component_geom, time_dependent, rc) result(aspect)
      type(VerticalGridAspect) :: aspect
      class(VariableSpec), intent(in) :: this
      class(VerticalGrid), optional, intent(in) :: vertical_grid
      type(ESMF_Geom), optional, intent(in) :: component_geom
      logical, optional, intent(in) :: time_dependent
      integer, optional, intent(out) :: rc

      type(ESMF_Geom) :: geom_
      class(VerticalGrid), allocatable :: vgrid

      ! If geom is allocated in var spec then it is prioritized over the
      ! component-wide geom.
      ! If not specified either way, then it indicates that the geom is
      ! mirrored ind will be determined by a connection.
      if (allocated(this%geom)) then
         geom_ = this%geom
      elseif (present(component_geom)) then
         geom_ = component_geom
      end if

      if (allocated(this%vertical_grid)) then
         vgrid = this%vertical_grid
      elseif (present(vertical_grid)) then
         vgrid = vertical_grid
      end if

      aspect = VerticalGridAspect(vertical_grid=vgrid, vertical_stagger=this%vertical_stagger, geom=geom_, &
           typekind=this%typekind)

      _RETURN(_SUCCESS)
   end function make_VerticalGridAspect

   function make_FrequencyAspect(this, timestep, offset, rc) result(aspect)
      type(FrequencyAspect) :: aspect
      class(VariableSpec), intent(in) :: this
      type(ESMF_TimeInterval), optional, intent(in) :: timestep
      type(ESMF_TimeInterval), optional, intent(in) :: offset
      integer, optional, intent(out) :: rc

      aspect = FrequencyAspect(timestep, offset, this%accumulation_type)
      _RETURN(_SUCCESS)
   end function make_FrequencyAspect
   
   function make_ClassAspect(this, registry, rc) result(aspect)
      class(ClassAspect), allocatable :: aspect
      class(VariableSpec), intent(in) :: this
      type(StateRegistry), pointer, optional, intent(in) :: registry
      integer, optional, intent(out) :: rc
      
      integer :: status
      character(:), allocatable :: std_name_1, std_name_2

      select case (this%itemType%ot)
      case (MAPL_STATEITEM_FIELD%ot)
         aspect = FieldClassAspect(standard_name=this%standard_name, default_value=this%default_value)
      case (MAPL_STATEITEM_FIELDBUNDLE%ot)
         aspect = FieldBundleClassAspect(standard_name=this%standard_name)
      case (MAPL_STATEITEM_STATE%ot)
         aspect = StateClassAspect(state_intent=this%state_intent, standard_name=this%standard_name)
      case (MAPL_STATEITEM_VECTOR%ot)
         call split_name(this%standard_name, std_name_1, std_name_2, _RC)
         aspect = VectorClassAspect(this%vector_component_names, &
              [ &
              FieldClassAspect(standard_name=std_name_1, default_value=this%default_value), &
              FieldClassAspect(standard_name=std_name_2, default_value=this%default_value) &
              ])
      case (MAPL_STATEITEM_BRACKET%ot)
         aspect = BracketClassAspect(this%bracket_size, this%standard_name)
      case (MAPL_STATEITEM_WILDCARD%ot)
         allocate(aspect,source=WildcardClassAspect())
      case (MAPL_STATEITEM_SERVICE%ot)
         _ASSERT(present(registry), 'must have registry for creating a Service')
         aspect = ServiceClassAspect(registry, this%service_items)
      case (MAPL_STATEITEM_EXPRESSION%ot)
         aspect = ExpressionClassAspect(registry=registry, expression=this%expression)
      case default
         aspect=FieldClassAspect('') ! must allocate something
         _FAIL('Unsupported itemType')
      end select
      
      _RETURN(_SUCCESS)
      
   end function make_ClassAspect
      
   subroutine validate_variable_spec(spec, rc)
      class(VariableSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc
      integer :: status
      type(StringVector) :: svector
      character, parameter :: UPPER = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
      character, parameter :: LOWER = 'abcdefghijklmnopqrstuvwxyz'
      character, parameter :: ALPHA = UPPER // LOWER
      character, parameter :: NUMERIC = '0123456789'
      character, parameter :: ALPHANUMERIC = ALPHA // NUMERIC
      character, parameter :: FORTRAN_IDENTIFIER = ALPHANUMERIC // '_'

      call validate_string(spec%short_name, is_valid_identifier, _RC)
      if(present(spec%standard)) then
         call validate_string(spec%standard, is_not_empty, _RC)
      else if(present(spec%long)) then
         call validate_string(spec%long, is_not_empty, _RC)
      else
         _FAIL('Neither standard_name nor long_name is present.')
      end if
      _ASSERT(valid_state_intent(spec%state_item), 'Invalid state intent')
      _ASSERT(valid_item_type(spec%itemType), 'Invalid item type')
      call validate_string_vector(spec%vector_component_names, svector, _RC)
      _ASSERT(valid_r4(spec%default_value), 'Invalid default_value')
      _ASSERT(valid_integer(spec%bracket_size), 'Invalid bracket size') 
      call validate_service_items(spec%service_items, _RC)
      call validate_expression(spec%expression, _RC)
      call validate_typekind(spec%typekind, _RC)
      call validate_geom(spec%geom, _RC)
      call validate_horizontal_dims_spec(spec%horizontal_dims_spec, _RC)
      call validate_regrid_param_regrid_method(spec%regrid_param, spec%regrid_method, _RC)
      call validate_timestep(spec%timestep, _RC)
      call validate_offset(spec%offset, _RC)
      call validate_ungridded_dims(spec%ungridded_dims, _RC)
      call validate_attributes(spec%attributes, _RC)
      call validate_dependencies(spec%dependencies, _RC)

   end subroutine validate_variable_spec
   
   function ascii_ranges(bounds) result(ranges)
      character(len=:), allocatable :: ranges
      character(len=*), intent(in) :: bounds
      integer :: i, j
      integer :: range_index(2)
      character(len=:), allocatable :: range

      ranges = ''
      do i=1, len(bounds)/2
         range_index = [iachar(ranges(2*i-1:2*i-1)), iachar(ranges(2*i:2*i))]
         range_index = [minval(range_index), maxval(range_index)]
         allocate(range(range_index(2) - range_index(1)+1))
         do j = range_index(1), range_index(2)
            range(j:j) = achar(j)
         end do
         ranges = ranges // range 
      end do

   end function ascii_ranges
      
   logical function is_valid_identifier(s)
      character(len=*), intent(in) :: s
      
      is_valid_identifier = is_all_alphanumeric_(s(1:1), alpha_only=.TRUE.) .and. is_all_alphanumeric_(s(2:))

   end function is_valid_identifier

   logical function is_all_alphanumeric_(s, alpha_only)
      character(len=*), intent(in) :: s
      logical, optional, intent(in) :: alpha_only
      character(len=*), parameter :: ALPHA = ascii_ranges('AZaz')
      character(len=*), parameter :: ALPHANUMERIC_ = ALPHA // ascii_ranges('09') // '_'
      logical :: alpha_only_

      if(.not. present(alpha_only)) return (verify(s, ALPHANUMERIC_) == 0)
      if(alpha_only) return verify(s, ALPHA) == 0
      return verify(s, ALPHANUMERIC_) == 0

   end function is_all_alphanumeric_

   logical function is_in_integer(n, bounds) result(lval)
      integer, intent(in) :: n
      integer, intent(in) :: bounds(:)
      integer :: i

      lval = .TRUE.
      if(.not. present(bounds)) return
      if(size(bounds) < 1) return

      if(size(bounds) == 1)
         lval = n == bounds(1)
         return
      end if

      lval = .FALSE.
      do i = 2, mod(size(bounds), 2), 2
         lval = .not. (n < minval(bounds(i-1) .or. n > maxval(bounds(i))
         if(lval) exit
      end do
      
   end function is_in_integer

   logical function is_in_realR4(t, bounds) result(lval)
      real(kind=ESMF_KIND_R4), intent(in) :: t
      real(kind=ESMF_KIND_R4), intent(in) :: bounds(:)
      integer :: i

      lval = .TRUE.
      if(.not. present(bounds)) return
      if(size(bounds) < 1) return

      lval = .FALSE.
      do i = 2, mod(size(bounds), 2), 2
         lval = .not. (n < minval(bounds(i-1) .or. n > maxval(bounds(i))
         if(lval) exit
      end do

   end function is_in_realR4

   subroutine validate_string(string, validator, rc)
      character(len=*), intent(in) :: string
      procedure(StringPredicate) :: validator

      _ASSERT(validator(string), '"' // trim(string) // '" is not a valid string.')
      _RETURN(_SUCCESS)

   end validate_string
      
   logical function is_not_empty(string)
      character(len=*), intent(in) :: string

      is_not_empty = len_trim(string) > 0

   end function is_not_empty

   subroutine validate_string_vector(strings, valid_strings, rc)
      class(StringVector), optional, intent(in) :: strings
      class(StringVector), optional, intent(in) :: valid_strings
      integer, optional, intent(out) :: rc
      integer :: status
      type(StringVectorIterator) :: iter, e, iiter, ie
      character(len=:), allocatable :: string
      logical :: found

      _RETURN_UNLESS(present(strings))

      iter = strings%begin()
      e = strings%end()
      do while(iter /= e)
         _ASSERT(is_valid_identifier(iter%of()), 'Invalid string')
         call iter%next()
      end do

      _RETURN_UNLESS(present(valid_strings))
      _RETURN_IF(valid_strings%empty())

      iter = strings%begin()
      e = strings%end()
      outer: do while(iter /= e)
         string = iter%of()
         iiter = valid_strings%begin()
         ie = valid_strings%end()
         inner: do while(iiter /= ie)
            found = string == iiter%of()
            if(found) exit inner
            call iiter%next()
         end do inner
         _ASSERT(found, 'Failed to find "' // trim(string) // '" in valid strings')
         call iter%next()
      end do outer
      _RETURN(_SUCCESS)

   end subroutine validate_string_vector

   logical function valid_state_intent(val)
      class(ESMF_StateIntent_Flag), intent(in) :: val
      type(ESMF_StateIntent_Flag), parameter :: VALID(*) = &
         & [ESMF_STATEINTENT_IMPORT, ESMF_STATEINTENT_EXPORT, ESMF_STATEINTENT_INTERNAL]

      valid_state_intent = IS_IN_SET_(val, VALID)

   end function valid_state_intent

   logical function valid_item_type(val)
      type(ESMF_StateItem_Flag), intent(in):: item_type
      type(ESMF_StateItem_Flag), parameter :: VALID(*) = &
         & [ESMF_STATEITEM_FIELD, ESMF_STATEITEM_FIELDBUNDLE]

      valid_item_type = IS_IN_SET_(val, VALID)

   end function valid_item_type

   logical function valid_r4(val, bounds, invert)
      real(kind=ESMF_KIND_R4), intent(in) :: val
      real(kind=ESMF_KIND_R4), optional, intent(in) :: bounds(:)
      logical, optional, intent(in) :: invert

      valid_r4 = .TRUE.
      if(.not. present(bounds)) return
      valid_r4 = is_in(val, bounds)
      if(present(invert)) valid_r4 = valid_r4 .eqv. .not. invert

   end function valid_r4

   logical function valid_integer(val, bounds, invert)
      integer, intent(in) :: val
      integer, optional, intent(in) :: bounds(:)
      logical, optional, intent(in) :: invert

      valid_integer = .TRUE.
      if(.not. present(bounds)) return
      valid_integer = is_in(val, bounds)
      if(present(invert)) valid_integer = valid_intger .eqv. .not. invert

   end function valid_integer

   subroutine validate_service_items(spec%service_items, rc)
      TYPE :: service_items
   end subroutine validate_service_items

   subroutine validate_expression(spec%expression, rc)
      TYPE :: expression
   end subroutine validate_expression

   subroutine validate_typekind(spec%typekind, rc)
      TYPE :: typekind
   end subroutine validate_typekind

   subroutine validate_geom(spec%geom, rc)
      TYPE :: geom
   end subroutine validate_geom

   subroutine validate_horizontal_dims_spec(spec%horizontal_dims_spec, rc)
      TYPE :: horizontal_dims_spec
   end subroutine validate_horizontal_dims_spec

      call validate_regrid_param_regrid_method(spec%regrid_param, spec%regrid_method, _RC)
   subroutine validate_timestep(spec%timestep, rc)
      TYPE :: timestep
   end subroutine validate_timestep

   subroutine validate_offset(spec%offset, rc)
      TYPE :: offset
   end subroutine validate_offset

   subroutine validate_ungridded_dims(spec%ungridded_dims, rc)
      TYPE :: ungridded_dims
   end subroutine validate_ungridded_dims

   subroutine validate_attributes(spec%attributes, rc)
      TYPE :: attributes
   end subroutine validate_attributes

   subroutine validate_dependencies(spec%dependencies, rc)
      TYPE :: dependencies
   end subroutine validate_dependencies

end module mapl3g_VariableSpec

