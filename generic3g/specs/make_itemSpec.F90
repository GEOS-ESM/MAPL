#include "MAPL_Generic.h"

module mapl3g_make_itemSpec
   use mapl3g_StateItemSpec
   use mapl3g_StateItem
   use mapl3g_StateItemAspect
   use mapl3g_AspectId
   use mapl3g_ClassAspect
   use mapl3g_FieldClassAspect
   use mapl3g_VectorClassAspect
   use mapl3g_WildcardClassAspect
   use mapl3g_ServiceClassAspect
   use mapl3g_BracketClassAspect
   use mapl3g_StateRegistry, only: StateRegistry
   use mapl_ErrorHandling
   use esmf, only: ESMF_STATEINTENT_INTERNAL, operator(==)
   implicit none
   private
   public :: make_ItemSpec

contains

   function make_itemSpec(variable_spec, registry, rc) result(item_spec)
      use mapl3g_VariableSpec, only: VariableSpec
      use mapl3g_ActualPtVector, only: ActualPtVector
      use mapl3g_VirtualConnectionPt
      use mapl3g_StateItemExtension
      type(StateItemSpec), target :: item_spec
      class(VariableSpec), intent(in) :: variable_spec
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

      select case (variable_spec%itemtype%ot)
      case (MAPL_STATEITEM_FIELD%ot)
         class_aspect = FieldClassAspect(standard_name=variable_spec%standard_name, default_value=variable_spec%default_value)
      case (MAPL_STATEITEM_VECTOR%ot)
         call split_name(variable_spec%standard_name, std_name_1, std_name_2, _RC)
         class_aspect = VectorClassAspect([ &
              FieldClassAspect(standard_name=std_name_1, default_value=variable_spec%default_value), &
              FieldClassAspect(standard_name=std_name_2, default_value=variable_spec%default_value) ])
      case (MAPL_STATEITEM_SERVICE%ot)
         class_aspect = ServiceClassAspect(registry, variable_spec%service_items)
      case (MAPL_STATEITEM_WILDCARD%ot)
         allocate(class_aspect, source=WildcardClassAspect())
      case (MAPL_STATEITEM_BRACKET%ot)
         class_aspect = BracketClassAspect(variable_spec%bracket_size, variable_spec%standard_name)
         item_spec = StateItemSpec(aspects)
      case default
         _FAIL('Unsupported type.')
      end select

      aspects = variable_spec%aspects
      call aspects%insert(CLASS_ASPECT_ID, class_aspect)
      item_spec = StateItemSpec(aspects)

      dependencies = variable_spec%make_dependencies(_RC)
      call item_spec%set_dependencies(dependencies)
      call item_spec%set_raw_dependencies(variable_spec%dependencies)

      _RETURN(_SUCCESS)

   end function make_itemSpec


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

      _ASSERT(idx_open > 0, 'VectorAspect requires standard name to have tuple for the names of the vector  components')
      _ASSERT(idx_close > 0, 'VectorAspect requires standard name to have tuple for east west components')
      _ASSERT(idx_comma > idx_open+1, 'VectorAspect requires standard name to have tuple for east west components')
      _ASSERT(idx_comma < idx_close-1, 'VectorAspect requires standard name to have tuple for east west components')

      name_1 = encoded_name(idx_open+1:idx_comma-1) // encoded_name(idx_close+1:)
      name_2 = encoded_name(idx_comma+1:idx_close-1) // encoded_name(idx_close+1:)

      _RETURN(_SUCCESS)
   end subroutine split_name
   
end module mapl3g_make_itemSpec
