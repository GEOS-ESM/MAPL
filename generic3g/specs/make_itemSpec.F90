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

      aspects = variable_spec%aspects

      select case (variable_spec%itemtype%ot)
      case (MAPL_STATEITEM_SERVICE%ot)
         class_aspect = ServiceClassAspect(registry, variable_spec%service_items)
         call aspects%insert(CLASS_ASPECT_ID, class_aspect)
      end select

      item_spec = StateItemSpec(aspects)

      dependencies = variable_spec%make_dependencies(_RC)
      call item_spec%set_dependencies(dependencies)
      call item_spec%set_raw_dependencies(variable_spec%dependencies)

      _RETURN(_SUCCESS)

   end function make_itemSpec


end module mapl3g_make_itemSpec
