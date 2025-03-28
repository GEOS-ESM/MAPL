#include "MAPL_Generic.h"
module mapl3g_VariableSpec
   use mapl3g_StateItemAspect
   use mapl3g_GeomAspect
   use mapl3g_ClassAspect
   use mapl3g_FieldClassAspect
   use mapl3g_VectorClassAspect
   use mapl3g_BracketClassAspect
   use mapl3g_WildcardClassAspect
   use mapl3g_ServiceClassAspect
   use mapl3g_UnitsAspect
   use mapl3g_AttributesAspect
   use mapl3g_UngriddedDimsAspect
   use mapl3g_VerticalGridAspect
   use mapl3g_FrequencyAspect
   use mapl3g_TypekindAspect
   use mapl3g_UngriddedDims
   use mapl3g_VerticalStaggerLoc
   use mapl3g_HorizontalDimsSpec
   use mapl3g_VirtualConnectionPt
   use mapl3g_ActualConnectionPt
   use mapl3g_VerticalGrid
   use mapl_KeywordEnforcerMod
   use mapl3g_ActualPtVector
   use mapl_ErrorHandling
   use mapl3g_StateRegistry
   use mapl3g_StateItem
   use mapl3g_AspectId
   use mapl3g_EsmfRegridder, only: EsmfRegridderParam
   use mapl3g_FieldDictionary
   use esmf
   use gFTL2_StringVector
   use nuopc

   implicit none
   private

   public :: VariableSpec
   public :: make_VariableSpec
   public :: make_VariableSpecFromAspects

   ! This type provides components that might be needed for _any_
   ! state item.  This is largely to support legacy interfaces, but it
   ! also allows us to defer interpretation until after user
   ! setservices() have run.
   type VariableSpec
      type(AspectMap) :: aspects
      ! Mandatory values:
      type(ESMF_StateIntent_Flag) :: state_intent
      character(:), allocatable :: short_name

      ! Metadata
      type(ESMF_StateItem_Flag) :: itemtype = MAPL_STATEITEM_FIELD
      type(StringVector), allocatable :: service_items

      type(StringVector) :: dependencies
   contains
      procedure :: make_virtualPt
      procedure :: make_dependencies
   end type VariableSpec

contains

   function make_VariableSpec( &
        state_intent, short_name, unusable, &
        standard_name, &
        geom, &
        units, &
        itemtype, &
        typekind, &
        vertical_stagger, &
        ungridded_dims, &
        default_value, &
        service_items, &
        attributes, &
        bracket_size, &
        dependencies, &
        regrid_param, &
        horizontal_dims_spec, &
        accumulation_type, &
        timeStep, &
        offset, &
        vector_component_names, &
        rc) result(var_spec)

      type(VariableSpec) :: var_spec
      type(ESMF_StateIntent_Flag), intent(in) :: state_intent
      character(*), intent(in) :: short_name
      ! Optional args:
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: standard_name
      type(ESMF_Geom), optional, intent(in) :: geom
      type(ESMF_StateItem_Flag), optional, intent(in) :: itemtype
      type(StringVector), optional :: service_items
      character(*), optional, intent(in) :: units
      type(ESMF_TypeKind_Flag), optional, intent(in) :: typekind
      type(VerticalStaggerLoc), optional, intent(in) :: vertical_stagger
      type(UngriddedDims), optional, intent(in) :: ungridded_dims
      real, optional, intent(in) :: default_value
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

      type(ESMF_RegridMethod_Flag), allocatable :: regrid_method
      type(EsmfRegridderParam) :: regrid_param_
      integer :: status
      class(ClassAspect), allocatable :: class_aspect
      type(ESMF_StateItem_Flag) :: itemType_

      itemType_ = ESMF_STATEITEM_FIELD
      if (present(itemtype)) itemType_ = itemType

      regrid_param_ = get_regrid_param(regrid_param, standard_name)

      class_aspect = make_ClassAspect(itemType_, _RC)
         
      var_spec = make_VariableSpecFromAspects(state_intent, short_name, &
           class_aspect=class_aspect,  itemType=itemType_, &
           service_items=service_items, &
           dependencies=dependencies, &
           geom_aspect=GeomAspect(geom, regrid_param_, horizontal_dims_spec), &
           units_aspect=UnitsAspect(units), &
           attributes_aspect=AttributesAspect(attributes), &
           ungridded_aspect=UngriddedDimsAspect(ungridded_dims), &
           vertical_aspect=VerticalGridAspect(vertical_stagger=vertical_stagger, geom=geom), &
           frequency_aspect=FrequencyAspect(timeStep=timeStep, offset=offset, &
           accumulation_type=accumulation_type), &
           typekind_aspect=TypekindAspect(typekind), &
           _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

      contains

         function make_ClassAspect(itemType, rc) result(class_aspect)
            type(ESMF_StateItem_Flag), optional, intent(in) :: itemType
            class(ClassAspect), allocatable :: class_aspect
            integer, optional, intent(out) :: rc

            integer :: status
            character(:), allocatable :: std_name_1, std_name_2
            
            select case (itemType%ot)
            case (MAPL_STATEITEM_FIELD%ot)
               class_aspect = FieldClassAspect(standard_name=standard_name, default_value=default_value)
            case (MAPL_STATEITEM_VECTOR%ot)
               call split_name(standard_name, std_name_1, std_name_2, _RC)
               class_aspect = VectorClassAspect(vector_component_names, &
                    [ &
                    FieldClassAspect(standard_name=std_name_1, default_value=default_value), &
                    FieldClassAspect(standard_name=std_name_2, default_value=default_value) &
                    ])
            case (MAPL_STATEITEM_BRACKET%ot)
               class_aspect = BracketClassAspect(bracket_size, standard_name)
            case (MAPL_STATEITEM_WILDCARD%ot)
               allocate(class_aspect, source=WildcardClassAspect())
            case (MAPL_STATEITEM_SERVICE%ot)
               class_aspect = ServiceClassAspect() ! placeholder
            case default
               _FAIL('Unsupported itemType.')
            end select

            _RETURN(_SUCCESS)

         end function make_ClassAspect

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
      type(ActualPtVector) :: dependencies
      class(VariableSpec), intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: i
      type(ActualConnectionPt) :: a_pt

      dependencies = ActualPtVector()
      do i = 1, this%dependencies%size()
         a_pt = ActualConnectionPt(VirtualConnectionPt(ESMF_STATEINTENT_EXPORT, this%dependencies%of(i)))
         call dependencies%push_back(a_pt)
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

   function make_VariableSpecFromAspects(state_intent, short_name, class_aspect, unusable, &
        itemtype, service_items, &
        dependencies, geom_aspect, units_aspect, attributes_aspect, &
        ungridded_aspect, vertical_aspect, frequency_aspect, typekind_aspect, &
        rc) &
        result(var_spec)

      type(VariableSpec) :: var_spec
      type(ESMF_StateIntent_Flag), intent(in) :: state_intent
      character(*), intent(in) :: short_name
      class(ClassAspect), intent(in) :: class_aspect
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_StateItem_Flag), optional, intent(in) :: itemType
      type(StringVector), optional :: service_items
      type(StringVector), optional, intent(in) :: dependencies
      class(GeomAspect), optional, intent(in) :: geom_aspect
      class(UnitsAspect), optional, intent(in) :: units_aspect
      class(AttributesAspect), optional, intent(in) :: attributes_aspect
      class(UngriddedDimsAspect), optional, intent(in) :: ungridded_aspect
      class(VerticalGridAspect), optional, intent(in) :: vertical_aspect
      class(FrequencyAspect), optional, intent(in) :: frequency_aspect
      class(TypekindAspect), optional, intent(in) :: typekind_aspect
      integer, optional, intent(out) :: rc
      
      var_spec%state_intent = state_intent
      var_spec%short_name = short_name
#if defined(_SET_OPTIONAL)
#  undef _SET_OPTIONAL
#endif
#define _SET_OPTIONAL(attr) if (present(attr)) var_spec%attr = attr
      _SET_OPTIONAL(itemType)
      _SET_OPTIONAL(service_items)
      _SET_OPTIONAL(dependencies)
#undef _SET_OPTIONAL

#if defined(_SET_ASPECT)
#  undef _SET_ASPECT
#endif
#define _SET_ASPECT(A) call add_item(var_spec%aspects, A)

#if defined(_SET_ASPECT_IF)
#  undef _SET_ASPECT_IF
#endif
#define _SET_ASPECT_IF(A, D) if(present(A)) then; _SET_ASPECT(A); else; _SET_ASPECT(D); end if

      _SET_ASPECT(class_aspect)
      
      _SET_ASPECT_IF(geom_aspect, GeomAspect())
      _SET_ASPECT_IF(units_aspect, UnitsAspect())
      _SET_ASPECT_IF(attributes_aspect, AttributesAspect())
      _SET_ASPECT_IF(ungridded_aspect, UngriddedDimsAspect())
      _SET_ASPECT_IF(vertical_aspect, VerticalGridAspect())
      _SET_ASPECT_IF(frequency_aspect, FrequencyAspect())
      _SET_ASPECT_IF(typekind_aspect, TypekindAspect())

#undef _SET_ASPECT_IF         
#undef _SET_ASPECT

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function make_VariableSpecFromAspects

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

end module mapl3g_VariableSpec
