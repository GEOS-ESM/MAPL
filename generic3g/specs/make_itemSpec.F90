#include "MAPL_Generic.h"

module mapl3g_make_itemSpec
   use mapl3g_StateItemSpec
   use mapl3g_StateItem
   use mapl3g_FieldSpec, only: FieldSpec
   use mapl3g_ServiceSpec, only: ServiceSpec
   use mapl3g_WildcardSpec, only: WildcardSpec
   use mapl3g_BracketSpec, only: BracketSpec
   use mapl3g_StateSpec, only: StateSpec
   use mapl3g_InvalidSpec, only: InvalidSpec
   use mapl3g_StateRegistry, only: StateRegistry
   use mapl_ErrorHandling
   implicit none
   private
   public :: make_ItemSpec

contains

   function make_itemSpec(variable_spec, registry, rc) result(item_spec)
      use mapl3g_VariableSpec, only: VariableSpec
      class(StateItemSpec), allocatable :: item_spec
      class(VariableSpec), intent(in) :: variable_spec
      type(StateRegistry), pointer, intent(in) :: registry
      integer, optional, intent(out) :: rc

      integer :: status
      type(FieldSpec) :: field_spec

      select case (variable_spec%itemtype%ot)
      case (MAPL_STATEITEM_FIELD%ot)
         allocate(FieldSpec :: item_spec)
         item_spec = FieldSpec(variable_spec)
      case (MAPL_STATEITEM_SERVICE%ot)
         allocate(ServiceSpec :: item_spec)
         item_spec = ServiceSpec(variable_spec, registry)
      case (MAPL_STATEITEM_WILDCARD%ot)
         allocate(WildcardSpec :: item_spec)
         field_spec = FieldSpec(variable_spec)
         item_spec = WildcardSpec(field_spec)
      case (MAPL_STATEITEM_BRACKET%ot)
         allocate(BracketSpec :: item_spec)
         field_spec = FieldSpec(variable_spec)
         item_spec = BracketSpec(field_spec, variable_spec%bracket_size)
!#      case (MAPL_STATEITEM_STATE%ot)
!#         allocate(StateSpec :: item_spec)
      case default
         allocate(InvalidSpec :: item_spec)
         _FAIL('Unsupported type.')
      end select

      _RETURN(_SUCCESS)

   end function make_itemSpec

end module mapl3g_make_itemSpec
