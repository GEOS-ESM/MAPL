#include "MAPL_Generic.h"

module mapl3g_make_itemSpec
   use mapl3g_StateItemSpec
   use mapl3g_ActualPtVector, only: ActualPtVector
   use mapl3g_VirtualConnectionPt
   use mapl3g_StateItemExtension
   use mapl3g_StateItem
   use mapl3g_StateItemAspect
   use mapl3g_AspectId
   use mapl3g_ClassAspect
   use mapl3g_FieldClassAspect
   use mapl3g_VectorClassAspect
   use mapl3g_WildcardClassAspect
   use mapl3g_ServiceClassAspect
   use mapl3g_BracketClassAspect
   use mapl3g_GeomAspect
   use mapl3g_FrequencyAspect
   use mapl3g_UnitsAspect
   use mapl3g_UngriddedDimsAspect
   use mapl3g_TypekindAspect
   use mapl3g_AttributesAspect
   use mapl3g_VerticalGridAspect
   use mapl3g_StateItem
   use mapl3g_VariableSpec, only: VariableSpec
   use mapl3g_StateRegistry, only: StateRegistry
   use mapl_KeywordEnforcer
   use gftl2_StringVector
   use mapl_ErrorHandling
   use esmf
   implicit none
   private
   public :: make_ItemSpec

contains

   function make_itemSpec(variable_spec, registry, rc) result(item_spec)
      type(StateItemSpec), target :: item_spec
      type(VariableSpec), intent(in) :: variable_spec
      type(StateRegistry), pointer, intent(in) :: registry
      integer, optional, intent(out) :: rc

      integer :: status
      type(ActualPtVector) :: dependencies
      integer :: i, n
      type(StateItemSpecPtr), allocatable :: spec_ptrs(:)
      type(VirtualConnectionPt) :: v_pt
      type(StateItemExtension), pointer :: primary

      integer :: idx
      character(:), allocatable :: std_name_1
      character(:), allocatable :: std_name_2
      class(ClassAspect), allocatable :: class_aspect
      type(AspectMap), target :: aspects

      aspects = variable_spec%aspects

!#      select case (variable_spec%itemtype%ot)
!#      case (MAPL_STATEITEM_SERVICE%ot)
!#         class_aspect = ServiceClassAspect(registry, variable_spec%service_items)
!#         call aspects%insert(CLASS_ASPECT_ID, class_aspect)
!#      end select
!#

      
      class_aspect = make_ClassAspect(variable_spec, registry, _RC)
      call aspects%insert(CLASS_ASPECT_ID, class_aspect)

 
!#      item_spec = StateItemSpec(aspects)

      dependencies = variable_spec%make_dependencies(_RC)
      call item_spec%set_dependencies(dependencies)
      call item_spec%set_raw_dependencies(variable_spec%dependencies)

      _RETURN(_SUCCESS)

   end function make_itemSpec


!#   function make_aspects(variable_spec, registry, rc) result(aspects)
!#      type(AspectMap) :: aspects
!#      type(VariableSpec), intent(in) :: variable_spec
!#      type(StateRegistry), intent(in) :: registry
!#      integer, optional, intent(out) :: rc
!#
!#      
!#      type(ESMF_StateIntent_Flag), intent(in) :: state_intent
!#      character(*), intent(in) :: short_name
!#      class(ClassAspect), intent(in) :: class_aspect
!#      class(KeywordEnforcer), optional, intent(in) :: unusable
!#      type(ESMF_StateItem_Flag), optional, intent(in) :: itemType
!#      type(StringVector), optional :: service_items
!#      type(StringVector), optional, intent(in) :: dependencies
!#      class(GeomAspect), optional, intent(in) :: geom_aspect
!#      class(UnitsAspect), optional, intent(in) :: units_aspect
!#      class(AttributesAspect), optional, intent(in) :: attributes_aspect
!#      class(UngriddedDimsAspect), optional, intent(in) :: ungridded_aspect
!#      class(VerticalGridAspect), optional, intent(in) :: vertical_aspect
!#      class(FrequencyAspect), optional, intent(in) :: frequency_aspect
!#      class(TypekindAspect), optional, intent(in) :: typekind_aspect
!#      integer, optional, intent(out) :: rc
!#
!#      integer :: status
!#      
!#      var_spec%state_intent = state_intent
!#      var_spec%short_name = short_name
!##if defined(_SET_OPTIONAL)
!##  undef _SET_OPTIONAL
!##endif
!##define _SET_OPTIONAL(attr) if (present(attr)) var_spec%attr = attr
!#      _SET_OPTIONAL(itemType)
!#      _SET_OPTIONAL(service_items)
!#      _SET_OPTIONAL(dependencies)
!##undef _SET_OPTIONAL
!#
!##if defined(_SET_ASPECT)
!##  undef _SET_ASPECT
!##endif
!##define _SET_ASPECT(A) call add_item(var_spec%aspects, A)
!#
!##if defined(_SET_ASPECT_IF)
!##  undef _SET_ASPECT_IF
!##endif
!##define _SET_ASPECT_IF(A, D) if(present(A)) then; _SET_ASPECT(A); else; _SET_ASPECT(D); end if
!#
!#      _SET_ASPECT(class_aspect)
!#      
!#      _SET_ASPECT_IF(geom_aspect, GeomAspect())
!#      _SET_ASPECT_IF(units_aspect, UnitsAspect())
!#      _SET_ASPECT_IF(attributes_aspect, AttributesAspect())
!#      _SET_ASPECT_IF(ungridded_aspect, UngriddedDimsAspect())
!#      _SET_ASPECT_IF(vertical_aspect, VerticalGridAspect())
!#      _SET_ASPECT_IF(frequency_aspect, FrequencyAspect())
!#      _SET_ASPECT_IF(typekind_aspect, TypekindAspect())
!#
!##undef _SET_ASPECT_IF         
!##undef _SET_ASPECT
!#
!#      _RETURN(_SUCCESS)
!#      _UNUSED_DUMMY(unusable)
!#   end function make_VariableSpecFromAspects

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


   function make_ClassAspect(variable_spec, registry, rc) result(class_aspect)
      class(ClassAspect), allocatable :: class_aspect
      type(VariableSpec), intent(in) :: variable_spec
      type(StateRegistry), pointer, intent(in) :: registry
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: std_name_1, std_name_2

      associate ( v => variable_spec)
        select case (v%itemType%ot)
        case (MAPL_STATEITEM_FIELD%ot)
           class_aspect = FieldClassAspect(standard_name=v%standard_name, default_value=v%default_value)
        case (MAPL_STATEITEM_VECTOR%ot)
            call split_name(v%standard_name, std_name_1, std_name_2, _RC)
            class_aspect = VectorClassAspect(v%vector_component_names, &
                [ &
                FieldClassAspect(standard_name=std_name_1, default_value=v%default_value), &
                FieldClassAspect(standard_name=std_name_2, default_value=v%default_value) &
                ])
        case (MAPL_STATEITEM_BRACKET%ot)
           class_aspect = BracketClassAspect(v%bracket_size, v%standard_name)
        case (MAPL_STATEITEM_WILDCARD%ot)
           allocate(class_aspect, source=WildcardClassAspect())
        case (MAPL_STATEITEM_SERVICE%ot)
           class_aspect = ServiceClassAspect(registry, v%service_items)
        case default
           _FAIL('Unsupported itemType.')
        end select
      end associate

      _RETURN(_SUCCESS)

   end function make_ClassAspect

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
 
end module mapl3g_make_itemSpec
