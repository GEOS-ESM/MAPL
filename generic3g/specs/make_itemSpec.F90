#include "MAPL_Generic.h"

module mapl3g_make_itemSpec
   use mapl3g_StateItemSpec
   use mapl3g_StateItem
   use mapl3g_StateItemAspect
   use mapl3g_AspectId
   use mapl3g_ClassAspect
   use mapl3g_FieldClassAspect
   use mapl3g_WildcardClassAspect
   use mapl3g_ServiceClassAspect
   use mapl3g_BracketClassAspect
   
!#   use mapl3g_FieldSpec, only: FieldSpec
!#   use mapl3g_ServiceSpec, only: ServiceSpec
!#   use mapl3g_WildcardSpec, only: WildcardSpec
!#   use mapl3g_BracketSpec, only: BracketSpec
!#   use mapl3g_StateSpec, only: StateSpec
!#   use mapl3g_InvalidSpec, only: InvalidSpec
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

      class(ClassAspect), allocatable :: class_aspect, ref_class_aspect
      type(StateItemSpec), target:: ref_spec
      type(AspectMap), target :: ref_aspects, aspects

      select case (variable_spec%itemtype%ot)
      case (MAPL_STATEITEM_FIELD%ot)
         class_aspect = FieldClassAspect(standard_name=variable_spec%standard_name, default_value=variable_spec%default_value)
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

      if (variable_spec%state_intent == ESMF_STATEINTENT_INTERNAL) then
         call item_spec%set_active()
      end if

      dependencies = variable_spec%make_dependencies(_RC)
      call item_spec%set_dependencies(dependencies)
      call item_spec%set_raw_dependencies(variable_spec%dependencies)

      _RETURN(_SUCCESS)

   end function make_itemSpec

end module mapl3g_make_itemSpec
