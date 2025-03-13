#include "MAPL_Generic.h"
module mapl3g_VariableSpec
   use mapl3g_StateItemAspect
   use mapl3g_GeomAspect
   use mapl3g_UnitsAspect
   use mapl3g_AttributesAspect
   use mapl3g_UngriddedDimsAspect
   use mapl3g_VerticalGridAspect
   use mapl3g_FrequencyAspect
   use mapl3g_TypekindAspect
   use mapl3g_UngriddedDims
   use mapl3g_VerticalDimSpec
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
      character(:), allocatable :: standard_name
      type(ESMF_StateItem_Flag) :: itemtype = MAPL_STATEITEM_FIELD
      type(StringVector), allocatable :: service_items
      real, allocatable :: default_value
      type(StringVector) :: attributes
      integer, allocatable :: bracket_size

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
        vertical_dim_spec, &
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
      type(VerticalDimSpec), optional, intent(in) :: vertical_dim_spec
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
      integer, optional, intent(out) :: rc

      type(ESMF_RegridMethod_Flag), allocatable :: regrid_method
      type(EsmfRegridderParam) :: regrid_param_
      integer :: status

      var_spec = make_VariableSpecFromAspects(state_intent, short_name, &
         & standard_name=standard_name, itemType=itemType, service_items=service_items, &
         & default_value=default_value, bracket_size=bracket_size, dependencies=dependencies, &
         & geom_aspect=GeomAspect(geom, regrid_param_, horizontal_dims_spec), &
         & units_aspect=UnitsAspect(units), &
         & attributes_aspect=AttributesAspect(attributes), &
         & ungridded_aspect=UngriddedDimsAspect(ungridded_dims), &
         & vertical_aspect=VerticalGridAspect(vertical_dim_spec=vertical_dim_spec, geom=geom), &
         & frequency_aspect=FrequencyAspect(timeStep=timeStep, offset=offset, &
         &   accumulation_type=accumulation_type), &
         & typekind_aspect=TypekindAspect(typekind), _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function make_VariableSpec

   function make_virtualPt(this) result(v_pt)
      type(VirtualConnectionPt) :: v_pt
      class(VariableSpec), intent(in) :: this
      v_pt = VirtualConnectionPt(this%state_intent, this%short_name)
   end function make_virtualPt

   function make_dependencies(this, rc) result(dependencies)
      type(ActualPtVector) :: dependencies
      class(VariableSpec), intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status
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

   function make_VariableSpecFromAspects(state_intent, short_name, unusable, &
         & standard_name, itemtype, service_items, default_value, bracket_size, &
         & dependencies, geom_aspect, units_aspect, attributes_aspect, &
         & ungridded_aspect, vertical_aspect, frequency_aspect, typekind_aspect, rc) &
         & result(var_spec)
      type(VariableSpec) :: var_spec
      type(ESMF_StateIntent_Flag), intent(in) :: state_intent
      character(*), intent(in) :: short_name
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: standard_name
      type(ESMF_StateItem_Flag), optional, intent(in) :: itemtype
      type(StringVector), optional :: service_items
      real, optional, intent(in) :: default_value
      integer, optional, intent(in) :: bracket_size
      type(StringVector), optional, intent(in) :: dependencies
      class(GeomAspect), optional, intent(in) :: geom_aspect
      class(UnitsAspect), optional, intent(in) :: units_aspect
      class(AttributesAspect), optional, intent(in) :: attributes_aspect
      class(UngriddedDimsAspect), optional, intent(in) :: ungridded_aspect
      class(VerticalGridAspect), optional, intent(in) :: vertical_aspect
      class(FrequencyAspect), optional, intent(in) :: frequency_aspect
      class(TypekindAspect), optional, intent(in) :: typekind_aspect
      integer, optional, intent(out) :: rc
      integer :: status
      
      var_spec%state_intent = state_intent
      var_spec%short_name = short_name
#if defined(_SET_OPTIONAL)
#  undef _SET_OPTIONAL
#endif
#define _SET_OPTIONAL(attr) if (present(attr)) var_spec%attr = attr
      _SET_OPTIONAL(standard_name)
      _SET_OPTIONAL(itemtype)
      _SET_OPTIONAL(service_items)
      _SET_OPTIONAL(default_value)
      _SET_OPTIONAL(bracket_size)
      _SET_OPTIONAL(dependencies)
#undef _SET_OPTIONAL

#if defined(_SET_ASPECT)
#  undef _SET_ASPECT
#endif
#define _SET_ASPECT(A) if(present(A)) call add_item(var_spec%aspects, A)
         _SET_ASPECT(geom_aspect)
         _SET_ASPECT(units_aspect)
         _SET_ASPECT(attributes_aspect)
         _SET_ASPECT(ungridded_aspect)
         _SET_ASPECT(vertical_aspect)
         _SET_ASPECT(frequency_aspect)
         _SET_ASPECT(typekind_aspect)
#undef _SET_ASPECT

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end function make_VariableSpecFromAspects

   subroutine add_item(aspects, aspect, rc)
      class(AspectMap), intent(inout) :: aspects
      class(StateItemAspect), intent(in) :: aspect
      integer, optional, intent(out) :: rc
      integer :: status

      select type(aspect)
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
